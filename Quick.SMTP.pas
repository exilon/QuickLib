{ ***************************************************************************

  Copyright (c) 2016-2021 Kike P�rez

  Unit        : Quick.SMTP
  Description : Send Emails
  Author      : Kike P�rez
  Version     : 1.5
  Created     : 12/10/2017
  Modified    : 08/09/2021

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
  Generics.Collections,
  SysUtils,
  IdGlobal,
  IdSMTP,
  IdMessage,
  IdReplySMTP,
  IdSSLOpenSSL,
  IdText,
  IdAttachment,
  IdAttachmentFile,
  IdAttachmentMemory,
  IdExplicitTLSClientServerBase,
  IdHTTP;

type
  TAttachment = class
  private
    fFilename : string;
    fContent : TStream;
  public
    constructor Create(const aFilename : string; const aContent : TStream);
    destructor Destroy; override;
    property Filename : string read fFilename;
    property Content : TStream read fContent;
  end;

  TMailMessage = class
  private
    fSenderName : string;
    fFrom : string;
    fRecipient : string;
    fSubject : string;
    fBody : string;
    fCC : string;
    fBCC : string;
    fReplyTo : string;
    fBodyFromFile : Boolean;
    fAttachments : TStringList;
    fAttachmentFiles : TObjectList<TAttachment>;
    procedure SetBody(aValue : string);
    procedure SetAttachments(const Value: TStringList);
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
    property ReplyTo : string read fReplyTo write fReplyTo;
    property Attachments : TStringList read fAttachments write SetAttachments;
    property AttachmentFiles : TObjectList<TAttachment> read fAttachmentFiles;
    procedure AddAttachment(const aFilename : string; aStream : TStream); overload;
    procedure AddAttachment(const aFilename, aFilePath : string); overload;
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
    function SendEmail(const aFromEmail,aFromName,aSubject,aTo,aCC,aBC,aReplyTo,aBody : string) : Boolean; overload;
    function SendEmail(const aFromName,aSubject,aTo,aCC,aBC,aReplyTo,aBody : string) : Boolean; overload;
    function SendEmail(const aFromName,aSubject,aTo,aCC,aBC,aReplyTo,aBody : string; const aAttachments : TStringList) : Boolean; overload;
  end;

implementation


{ TMailMessage }

constructor TMailMessage.Create;
begin
  fCC := '';
  fBCC := '';
  fBody := '';
  fAttachments := TStringList.Create;
  fAttachmentFiles := TObjectList<TAttachment>.Create(True);
end;

destructor TMailMessage.Destroy;
begin
  if Assigned(fAttachments) then fAttachments.Free;
  if Assigned(fAttachmentFiles) then fAttachmentFiles.Free;
  inherited;
end;

procedure TMailMessage.AddAttachment(const aFilename, aFilePath : string);
var
  fs : TFileStream;
begin
  if not FileExists(aFilePath) then raise Exception.CreateFmt('MailMessage: file "%s" not found!',[aFilename]);
  fs := TFileStream.Create(aFilePath,fmOpenRead);
  fAttachmentFiles.Add(TAttachment.Create(aFilename,fs));
end;

procedure TMailMessage.AddAttachment(const aFilename : string; aStream : TStream);
begin
  fAttachmentFiles.Add(TAttachment.Create(aFilename,aStream));
end;

procedure TMailMessage.AddBodyFromFile(const cFileName: string);
begin
  fBodyFromFile := True;
  fBody := cFileName;
end;

procedure TMailMessage.SetAttachments(const Value: TStringList);
begin
  if Assigned(fAttachments) then fAttachments.Free;

  fAttachments := Value;
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
  Create;
  Host := cHost;
  Port := cPort;
  fUseSSL := cUseSSL;
end;

destructor TSMTP.Destroy;
begin
  if Assigned(fMail) then fMail.Free;
  inherited;
end;

function TSMTP.SendEmail(const aFromEmail, aFromName, aSubject, aTo, aCC, aBC, aReplyTo, aBody: string): Boolean;
begin
  fMail.From := aFromEmail;
  Result := SendEmail(aFromName,aSubject,aTo,aCC,aBC,aReplyTo,aBody);
end;

function TSMTP.SendEmail(const aFromName,aSubject,aTo,aCC,aBC,aReplyTo,aBody : string) : Boolean;
begin
  Result := SendEmail(aFromName,aSubject,aTo,aCC,aBC,aReplyTo,aBody,nil);
end;

function TSMTP.SendEmail(const aFromName,aSubject,aTo,aCC,aBC,aReplyTo,aBody : string; const aAttachments : TStringList) : Boolean;
var
  mail : TMailMessage;
begin
  if fMail.From.IsEmpty then raise Exception.Create('Email sender not specified!');
  mail := TMailMessage.Create;
  try
    Mail.From := fMail.From;
    if aFromName.IsEmpty then Mail.SenderName := fMail.From
      else Mail.SenderName := aFromName;
    Mail.Subject := aSubject;
    Mail.Body := aBody;
    Mail.Recipient := aTo;
    Mail.CC := aCC;
    Mail.BCC := aBC;
    Mail.ReplyTo := aReplyTo;
    if aAttachments <> nil then Mail.Attachments := aAttachments;
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
  idattach : TIdAttachment;
  attach : TAttachment;
begin
  Result := False;
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    idattach := nil;
    mBody := nil;
    msg := TIdMessage.Create(nil);
    try
      //create mail msg
      msg.From.Address := aMail.From;
      if aMail.SenderName <> '' then msg.From.Name := aMail.SenderName;
      msg.Subject := aMail.Subject;
      for email in aMail.Recipient.Split([',',';']) do msg.Recipients.Add.Address := email;
      for email in aMail.CC.Split([',',';']) do msg.CCList.Add.Address := email;
      for email in aMail.BCC.Split([',',';']) do msg.BCCList.Add.Address := email;
      for email in aMail.ReplyTo.Split([',',';']) do msg.ReplyTo.Add.Address := email;
      if aMail.fBodyFromFile then
      begin
        msg.Body.LoadFromFile(aMail.Body);
      end
      else
      begin
        mBody := TIdText.Create(msg.MessageParts);
        mBody.ContentType := 'text/html';
        mBody.CharSet:= 'utf-8';
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
      //add stream attachments if exists
      if aMail.AttachmentFiles.Count > 0 then
      begin
        //mBody.ContentType := 'multipart/mixed';
        //msg.ContentType := 'multipart/mixed';
        for attach in aMail.AttachmentFiles do
        begin
          idattach := TIdAttachmentMemory.Create(msg.MessageParts,attach.Content);
          idattach.Filename := attach.Filename;
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
          if fPort = 465 then Self.UseTLS := utUseImplicitTLS
            else Self.UseTLS := utUseExplicitTLS;
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


{ TAttachment }

constructor TAttachment.Create(const aFilename: string; const aContent: TStream);
begin
  fFilename := aFilename;
  fContent := aContent;
end;

destructor TAttachment.Destroy;
begin
  if Assigned(fContent) then fContent.Free;
  inherited;
end;

end.
