{ ***************************************************************************

  Copyright (c) 2016-2018 Kike P�rez

  Unit        : Quick.SMTP
  Description : Send Emails
  Author      : Kike P�rez
  Version     : 1.4
  Created     : 12/10/2017
  Modified    : 22/06/2018

  This file is part of QuickLib: https://github.com/exilon/QuickLib

 ***************************************************************************

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

 *************************************************************************** }

unit Quick.SMTP;

interface

{$i QuickLib.inc}

uses
  Classes,
  SysUtils,
  IdGlobal,
  IdSMTP,
  IdMessage,
  IdReplySMTP,
  IdSSLOpenSSL,
  IdText,
  IdAttachment,
  IdAttachmentFile,
  IdExplicitTLSClientServerBase,
  IdHTTP;

type

  TMailMessage = class
  private
    fSenderName : string;
    fFrom : string;
    fRecipient : string;
    fSubject : string;
    fBody : string;
    fCC : string;
    fBCC : string;
    fBodyFromFile : Boolean;
    fAttachments : TStringList;
    procedure SetBody(aValue : string);
  public
    constructor Create;
    destructor Destroy; override;
    property SenderName : string read fSenderName write fSenderName;
    property From : string read fFrom write fFrom;
    property Recipient : string read fRecipient write fRecipient;
    property Subject : string read fSubject write fSubject;
    property Body : string read fBody write SetBody;
    property CC : string read fCC write fCC;
    property BCC : string read fBCC write fBCC;
    property Attachments : TStringList read fAttachments write fAttachments;
    procedure AddBodyFromFile(const cFileName : string);
  end;

  TSMTP = class(TIdSMTP)
  private
    fServerAuth : Boolean;
    fUseSSL : Boolean;
    fMail : TMailMessage;
  public
    constructor Create; overload;
    constructor Create(const cHost : string; cPort : Integer; cUseSSL : Boolean = False); overload;
    destructor Destroy; override;
    property ServerAuth : Boolean read fServerAuth write fServerAuth;
    property UseSSL: Boolean read fUseSSL write fUseSSL;
    property Mail : TMailMessage read fMail write fMail;
    function SendMail: Boolean; overload;
    function SendMail(aMail : TMailMessage) : Boolean; overload;
    function SendEmail(const aFromName,aSubject,aTo,aCC,aBC,aBody : string) : Boolean; overload;
  end;

implementation


{ TMailMessage }

constructor TMailMessage.Create;
begin
  fCC := '';
  fBCC := '';
  fBody := '';
  fAttachments := TStringList.Create;
end;

destructor TMailMessage.Destroy;
begin
  if Assigned(fAttachments) then fAttachments.Free;
  inherited;
end;

procedure TMailMessage.AddBodyFromFile(const cFileName: string);
begin
  fBodyFromFile := True;
  fBody := cFileName;
end;

procedure TMailMessage.SetBody(aValue: string);
begin
  fBodyFromFile := False;
  fBody := aValue;
end;

{ TSMTP }

constructor TSMTP.Create;
begin
  inherited Create;
  fMail := TMailMessage.Create;
  Port := 25;
  UseTLS := TIdUseTLS.utNoTLSSupport;
  fUseSSL := False;
end;

constructor TSMTP.Create(const cHost : string; cPort : Integer; cUseSSL : Boolean = False);
begin
  inherited Create;
  Host := cHost;
  Port := cPort;
  fUseSSL := cUseSSL;
end;

destructor TSMTP.Destroy;
begin
  if Assigned(fMail) then fMail.Free;
  inherited;
end;

function TSMTP.SendEmail(const aFromName,aSubject,aTo,aCC,aBC,aBody : string) : Boolean;
var
  mail : TMailMessage;
begin
  mail := TMailMessage.Create;
  try
    Mail.From := fMail.fFrom;
    Mail.SenderName := aFromName;
    Mail.Subject := aSubject;
    Mail.Body := aBody;
    Mail.Recipient := aTo;
    Mail.CC := aCC;
    Mail.BCC := aBC;
    Result := Self.SendMail(mail);
  finally
    mail.Free;
  end;
end;

function TSMTP.SendMail: Boolean;
begin
  Result := SendMail(fMail);
end;

function TSMTP.SendMail(aMail : TMailMessage) : Boolean;
var
  msg : TIdMessage;
  SSLHandler : TIdSSLIOHandlerSocketOpenSSL;
  email : string;
  filename : string;
  mBody : TIdText;
  idattach : TIdAttachmentFile;
begin
  Result := False;
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    msg := TIdMessage.Create(nil);
    try
      //create mail msg
      idattach := nil;
      mBody := nil;
      msg.From.Address := aMail.From;
      if aMail.SenderName <> '' then msg.From.Name := aMail.SenderName;
      msg.Subject := aMail.Subject;
      for email in aMail.Recipient.Split([',',';']) do msg.Recipients.Add.Address := email;
      for email in aMail.CC.Split([',',';']) do msg.CCList.Add.Address := email;
      for email in aMail.BCC.Split([',',';']) do msg.BCCList.Add.Address := email;
      if aMail.fBodyFromFile then
      begin
        msg.Body.LoadFromFile(aMail.Body);
      end
      else
      begin
        mBody := TIdText.Create(msg.MessageParts);
        mBody.ContentType := 'text/html';
        mBody.Body.Text := aMail.Body;
      end;
      //add attachements if exists
      if aMail.Attachments.Count > 0 then
      begin
        for filename in aMail.Attachments do
        begin
          idattach := TIdAttachmentFile.Create(msg.MessageParts,filename);
        end;
      end;

      //configure smtp SSL
      try
        if fUseSSL then
        begin
          Self.IOHandler := SSLHandler;
          SSLHandler.MaxLineAction := maException;
          SSLHandler.SSLOptions.Method := sslvTLSv1_2;
          SSLHandler.SSLOptions.Mode := sslmUnassigned;
          SSLHandler.SSLOptions.VerifyMode := [];
          SSLHandler.SSLOptions.VerifyDepth := 0;
          //Self.UseTLS := utUseExplicitTLS;
        end;
        //server auth
        if ServerAuth then Self.AuthType := TIdSMTPAuthenticationType.satDefault;
        Self.Port := fPort;
      except
        on E : Exception do raise Exception.Create(Format('[%s] : %s',[Self.ClassName,e.Message]));
      end;

      //send email
      try
        Self.Connect;
        if Self.Connected then
        begin
          Self.Send(msg);
          Self.Disconnect(False);
          Result := True;
        end;
      except
        on E : Exception do raise Exception.Create(Format('[%s] : %s',[Self.ClassName,e.Message]));
      end;
    finally
      if Assigned(idattach) then idattach.Free;
      if Assigned(mBody) then mBody.Free;
      msg.Free;
    end;
  finally
    SSLHandler.Free;
  end;
end;


end.
