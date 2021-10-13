{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.HttpServer
  Description : Http Server
  Author      : Kike Pérez
  Version     : 1.8
  Created     : 30/08/2019
  Modified    : 12/06/2020

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

unit Quick.HttpServer;

{$i QuickLib.inc}

interface

uses
  {$IFDEF DEBUG_HTTPSERVER}
  Quick.Debug.Utils,
  {$ENDIF}
  SysUtils,
  Classes,
  IdHTTPServer,
  IdCustomHTTPServer,
  IdSSLOpenSSL,
  IdContext,
  Quick.Commons,
  Quick.Value,
  Quick.Logger.Intf,
  Quick.HttpServer.Types,
  Quick.HttpServer.Request,
  Quick.HttpServer.Response;

type
  EHttpProtocolError = class(Exception);

  TRequestEvent = procedure(aRequest : IHttpRequest; aResponse : IHttpResponse) of object;
  TOnConnectEvent = procedure of object;
  TOnDisconnectEvent = procedure of object;

  TCustomErrorPages = class
  private
    fPath : string;
    fDynamicErrorPage : Boolean;
    fEnabled : Boolean;
  public
    property Path : string read fPath write fPath;
    property DynamicErrorPage : Boolean read fDynamicErrorPage write fDynamicErrorPage;
    property Enabled : Boolean read fEnabled write fEnabled;
  end;

  IHttpServer = interface
  ['{3B48198A-49F7-40A5-BBFD-39C78B6FA1EA}']
    procedure SetOnRequest(aRequestEvent : TRequestEvent);
    function GetOnRequest : TRequestEvent;
    function GetCustomErrorPages: TCustomErrorPages;
    procedure SetCustomErrorPages(const Value: TCustomErrorPages);
    function GetLogger : ILogger;
    procedure SetLogger(const aLogger : ILogger);
    function GetHost: string;
    function GetPort: Integer;
    property OnNewRequest : TRequestEvent read GetOnRequest write SetOnRequest;
    property CustomErrorPages : TCustomErrorPages read GetCustomErrorPages write SetCustomErrorPages;
    property Host : string read GetHost;
    property Port : Integer read GetPort;
    property Logger : ILogger read GetLogger write SetLogger;
    procedure Start;
    procedure Stop;
  end;

  TCustomHttpServer = class(TInterfacedObject,IHttpServer)
  private
    fLogger : ILogger;
    fOnConnect : TOnConnectEvent;
    fOnDisconnect : TOnDisconnectEvent;
    fCustomErrorPages : TCustomErrorPages;
    procedure SetOnRequest(aRequestEvent : TRequestEvent);
    function GetOnRequest : TRequestEvent;
    function GetCustomErrorPages: TCustomErrorPages;
    procedure SetCustomErrorPages(const Value: TCustomErrorPages);
    function GetLogger : ILogger;
    procedure SetLogger(const aLogger : ILogger);
    function GetHost: string;
    function GetPort: Integer;
  protected
    fOnRequest : TRequestEvent;
    fHost : string;
    fPort : Integer;
    fSSLSecured : Boolean;
    procedure GetErrorPage(const aURL : string; aResponse : IHttpResponse); virtual;
  public
    constructor Create(const aHost : string; aPort : Integer; aSSLEnabled : Boolean; aLogger : ILogger = nil); virtual;
    destructor Destroy; override;
    property Host : string read GetHost;
    property Port : Integer read GetPort;
    property CustomErrorPages : TCustomErrorPages read GetCustomErrorPages write SetCustomErrorPages;
    property OnNewRequest : TRequestEvent read GetOnRequest write SetOnRequest;
    property OnConnect : TOnConnectEvent read fOnConnect write fOnConnect;
    property OnDisconnect : TOnDisconnectEvent read fOnDisconnect write fOnDisconnect;
    property Logger : ILogger read GetLogger write SetLogger;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
  end;

  THttpServer = class(TCustomHttpServer)
  private
    fHTTPServer : TidHTTPServer;
    procedure OnGetRequest(aContext: TIdContext; aRequestInfo: TIdHTTPRequestInfo; aResponseInfo: TIdHTTPResponseInfo);
    function GetSSLIOHandler : TIdServerIOHandlerSSLOpenSSL;
    function OnVerifyPeer(aCertificate: TIdX509; aOk: Boolean; aDepth, aError: Integer): Boolean;
    function GetRequestInfo(aRequestInfo : TIdHTTPRequestInfo) : THttpRequest;
    procedure SetResponseInfo(aResponseInfo : TIdHTTPResponseInfo; aResponse : IHttpResponse);
    procedure DoOnQuerySSLPort(aPort: Word; var vUseSSL: Boolean);
    procedure DoConnect(aContext: TIdContext);
    procedure DoDisconnect(aContext: TIdContext);
  protected
    procedure ProcessRequest(aRequest: IHttpRequest; aResponse: IHttpResponse); virtual;
  public
    constructor Create(const aHost : string; aPort : Integer; aSSLEnabled : Boolean; aLogger : ILogger = nil); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
  end;

implementation

{ TCustomHttpServer }

constructor TCustomHttpServer.Create(const aHost : string; aPort : Integer; aSSLEnabled : Boolean; aLogger : ILogger = nil);
begin
  fCustomErrorPages := TCustomErrorPages.Create;
  fCustomErrorPages.Path := '.';
  fCustomErrorPages.DynamicErrorPage := False;
  fCustomErrorPages.Enabled := False;
  if aHost.IsEmpty then fHost := '127.0.0.1'
    else fHost := aHost;
  {$IFDEF DELPHILINUX}
  if fHost = '127.0.0.1' then fHost := '0.0.0.0';
  {$ENDIF}
  fPort := aPort;
  if aLogger = nil then
  begin
    fLogger := TNullLogger.Create;
  end
  else fLogger := aLogger;
  fSSLSecured := aSSLEnabled;
end;

destructor TCustomHttpServer.Destroy;
begin
  fCustomErrorPages.Free;
  inherited;
end;

function TCustomHttpServer.GetCustomErrorPages: TCustomErrorPages;
begin
  Result := fCustomErrorPages;
end;

procedure TCustomHttpServer.GetErrorPage(const aURL : string; aResponse : IHttpResponse);
var
  filestream : TFileStream;
  pagestream : TStringStream;
  pagefilename : string;
  found : Boolean;
  content : string;
begin
  content := '';
  found := False;
  if (fCustomErrorPages.Enabled) then
  begin
    pagestream := TStringStream.Create;
    try
      //get specific error filename
      pagefilename := Format('%s\%d.html',[fCustomErrorPages.Path,aResponse.StatusCode]);
      found := FileExists(pagefilename);
      //get generic error type filanema
      if not found then
      begin
        pagefilename := Format('%s\%sxx.html',[fCustomErrorPages.Path,(aResponse.StatusCode).ToString[Low(string)]]);
        found := FileExists(pagefilename);
      end;
      //get generic error filename
      if not found then
      begin
        pagefilename := Format('%s\error.html',[fCustomErrorPages.Path]);
        found := FileExists(pagefilename);
      end;

      if found then
      begin
        filestream := TFileStream.Create(pagefilename,fmShareDenyNone);
        try
          pagestream.CopyFrom(filestream,filestream.Size);
        finally
          filestream.Free;
        end;
        content := pagestream.DataString;
        if fCustomErrorPages.DynamicErrorPage then
        begin
          content := StringReplace(content,'{{URL}}',aURL,[rfReplaceAll,rfIgnoreCase]);
          content := StringReplace(content,'{{STATUSCODE}}',aResponse.StatusCode.ToString,[rfReplaceAll,rfIgnoreCase]);
          content := StringReplace(content,'{{STATUSTEXT}}',aResponse.StatusText,[rfReplaceAll,rfIgnoreCase]);
          content := StringReplace(content,'{{CONTENT}}',aResponse.ContentText,[rfReplaceAll,rfIgnoreCase]);
        end;
      end;
    finally
      pagestream.Free;
    end;
  end;

  if not found then
  begin
    aResponse.ContentText := Format('<h2>%d Error: %s</h2>',[aResponse.StatusCode,aResponse.StatusText])
          + Format('<h4>Message: %s</h4>',[aResponse.ContentText]);
  end
  else aResponse.ContentText := content;
end;

function TCustomHttpServer.GetHost: string;
begin
  Result := fHost;
end;

function TCustomHttpServer.GetLogger: ILogger;
begin
  Result := fLogger;
end;

function TCustomHttpServer.GetOnRequest: TRequestEvent;
begin
  Result := fOnRequest;
end;

function TCustomHttpServer.GetPort: Integer;
begin
  Result := fPort;
end;

procedure TCustomHttpServer.SetCustomErrorPages(const Value: TCustomErrorPages);
begin
  fCustomErrorPages := Value;
end;

procedure TCustomHttpServer.SetLogger(const aLogger: ILogger);
begin
  fLogger := aLogger;
end;

procedure TCustomHttpServer.SetOnRequest(aRequestEvent: TRequestEvent);
begin
  fOnRequest := aRequestEvent;
end;

{ THTTPServer }

constructor THTTPServer.Create(const aHost : string; aPort : Integer; aSSLEnabled : Boolean; aLogger : ILogger = nil);
begin
  inherited Create(aHost, aPort, aSSLEnabled, aLogger);
  Logger.Info('HTTPServer: Indy');
  fHTTPServer := TIdHTTPServer.Create(nil);
  fHTTPServer.Bindings.Clear; //make sure there's no other bindings
  with fHTTPServer.Bindings.Add do
  begin
    IP := fHost;
    Port := fPort;
  end;
  if fSSLSecured then fHTTPServer.IOHandler := GetSSLIOHandler;
  fHTTPServer.OnCommandGet := OnGetRequest;
  fHTTPServer.OnCommandOther := OnGetRequest;
  fHTTPServer.OnConnect := DoConnect;
  fHTTPServer.OnDisconnect := DoDisconnect;
  //fHTTPServer.OnExecute := DoConnect;
  fHTTPServer.OnQuerySSLPort := DoOnQuerySSLPort;
  fHTTPServer.ServerSoftware := 'Quick.HttpServer';
  fHTTPServer.MaxConnections := 0;
  fHTTPServer.AutoStartSession := False;
  fHTTPServer.KeepAlive := True;
  fHTTPServer.SessionState := False;
  fHTTPServer.ParseParams := False;
end;

destructor THTTPServer.Destroy;
begin
  if Assigned(fHTTPServer) then
  begin
    if Assigned(fHTTPServer.IOHandler) then fHTTPServer.IOHandler.Free;
    fHTTPServer.Free;
  end;
  inherited;
end;

function THTTPServer.GetSSLIOHandler : TIdServerIOHandlerSSLOpenSSL;
begin
  Result := TIdServerIOHandlerSSLOpenSSL.Create(nil);
  //Result.SSLOptions.RootCertFile := '.\ca.cert.pem';
  Result.SSLOptions.CertFile := '.\server.cert.pem';
  Result.SSLOptions.KeyFile := '.\server.key.pem';
  Result.SSLOptions.Method := sslvSSLv23;
  Result.SSLOptions.Mode := sslmServer;
  Result.OnVerifyPeer := OnVerifyPeer;
end;

function THTTPServer.OnVerifyPeer(aCertificate: TIdX509; aOk: Boolean; aDepth, aError: Integer): Boolean;
begin
  Result := aOk;
end;

function THttpServer.GetRequestInfo(aRequestInfo: TIdHTTPRequestInfo): THttpRequest;
var
  i : Integer;
  uhost : TArray<string>;
begin
  Result := THttpRequest.Create;
  if aRequestInfo.Host.Contains(':') then
  begin
    uhost := aRequestInfo.Host.Split([':']);
    Result.Host := uhost[0];
    Result.Port := StrToIntDef(uhost[1],80);
  end
  else Result.Host := aRequestInfo.Host;
  Result.URL := aRequestInfo.URI;
  Result.ClientIP := aRequestInfo.RemoteIP;
  Result.UnParsedParams := aRequestInfo.QueryParams;
  Result.SetMethodFromString(aRequestInfo.Command);
  Result.UserAgent := aRequestInfo.UserAgent;
  Result.CacheControl := aRequestInfo.CacheControl;
  Result.Referer := aRequestInfo.Referer;
  Result.Content := aRequestInfo.PostStream;
  Result.ContentType := aRequestInfo.ContentType;
  Result.ContentEncoding := aRequestInfo.ContentEncoding;
  Result.ContentLength := aRequestInfo.ContentLength;
  {$IFDEF DEBUG_HTTPSERVER}
  TDebugger.Trace(Self,'Request: Headers (%s)',[aRequestInfo.RawHeaders.Text]);
  {$ENDIF}
  for i := 0 to aRequestInfo.RawHeaders.Count -1 do
  begin
    if not StrInArray(aRequestInfo.RawHeaders.Names[i],['Host','Accept-Encoding','Accept','User-Agent','Connection','Cache-Control']) then
    begin
      Result.Headers.Add(aRequestInfo.RawHeaders.Names[i],aRequestInfo.RawHeaders.Values[aRequestInfo.RawHeaders.Names[i]]);
    end;
  end;
end;

procedure THttpServer.SetResponseInfo(aResponseInfo: TIdHTTPResponseInfo; aResponse: IHttpResponse);
var
  pair : TPairItem;
begin
  for pair in aResponse.Headers do
  begin
    aResponseInfo.CustomHeaders.AddValue(pair.Name,pair.Value);
  end;
  aResponseInfo.ResponseNo := aResponse.StatusCode;
  aResponseInfo.ResponseText := aResponse.StatusText;
  aResponseInfo.ContentStream := aResponse.Content;
  aResponseInfo.ContentText := aResponse.ContentText;
  aResponseInfo.ContentType := aResponse.ContentType;
  //delegate stream to responseinfo
  aResponse.Content := nil;
end;

procedure THttpServer.ProcessRequest(aRequest: IHttpRequest; aResponse: IHttpResponse);
begin
  if Assigned(fOnRequest) then fOnRequest(aRequest,aResponse);
end;

procedure THttpServer.DoConnect(aContext: TIdContext);
begin
  {$IFDEF DEBUG_HTTPSERVER}
  TDebugger.Enter(Self,'DoConnect').TimeIt;
  {$ENDIF}
  Logger.Debug('Client connected');
  if Assigned(fOnConnect) then fOnConnect;
end;

procedure THttpServer.DoDisconnect(aContext: TIdContext);
begin
  {$IFDEF DEBUG_HTTPSERVER}
  TDebugger.Enter(Self,'DoDisconnect').TimeIt;
  {$ENDIF}
  Logger.Debug('Client disconnected!');
  if Assigned(fOnDisconnect) then fOnDisconnect;
end;

procedure THTTPServer.DoOnQuerySSLPort(aPort: Word; var vUseSSL: Boolean);
begin
  vUseSSL := (aPort <> 443);
end;

procedure THTTPServer.OnGetRequest(aContext: TIdContext; aRequestInfo: TIdHTTPRequestInfo; aResponseInfo: TIdHTTPResponseInfo);
var
  request : IHttpRequest;
  response : IHttpResponse;
begin
  {$IFDEF DEBUG_HTTPSERVER}
  TDebugger.Enter(Self,Format('OnGetRequest (%s %s)',[aRequestInfo.Command,aRequestInfo.URI])).TimeIt;
  {$ENDIF}
  Logger.Debug('Request: %s',[aRequestInfo.RawHTTPCommand]);
  request := GetRequestInfo(aRequestInfo);
  response := THttpResponse.Create;
  //process incoming Request
  try
    ProcessRequest(request,response);
  except
    on E : Exception do
    begin
      //get unexpected exception
      if E.InheritsFrom(EControlledException) then
      begin
        Logger.Error('Request: %s %s [%d %s] %s',[request.GetMethodAsString,request.URL, response.StatusCode, response.StatusText,e.Message]);
        response.ContentText := response.ContentText + '<BR>' + e.Message;
      end
      else
      begin
        if response.StatusCode = 200 then
        begin
          response.StatusCode := 500;
          response.StatusText := 'Internal server error';
        end;
        response.ContentText := e.Message;
        //log error
        if response.StatusCode = 404 then Logger.Warn('Request: %s %s [%d %s] %s',[request.GetMethodAsString,request.URL, response.StatusCode, response.StatusText,e.Message])
          else Logger.Error('Request: %s %s [%d %s] %s',[request.GetMethodAsString,request.URL, response.StatusCode, response.StatusText,e.Message]);
      end;
    end;
  end;
  //check if need return error page
  if response.StatusCode > 399 then GetErrorPage(aRequestInfo.URI,response);
  //return response to client
  {$IFDEF DEBUG_HTTPSERVER}
  TDebugger.TimeIt(Self,Format('OnGetRequest (%s)',[aRequestInfo.URI]),'SendResponse');
  {$ENDIF}
  SetResponseInfo(aResponseInfo,response);
  aResponseInfo.WriteContent;
end;

procedure THttpServer.Start;
begin
  fHTTPServer.Active := True;
end;

procedure THttpServer.Stop;
begin
  fHTTPServer.Active := False;
end;

end.
