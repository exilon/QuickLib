{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.HttpServer
  Description : Http Server
  Author      : Kike Pérez
  Version     : 1.8
  Created     : 30/08/2019
  Modified    : 14/02/2020

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
  SysUtils,
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


  IHttpServer = interface
  ['{3B48198A-49F7-40A5-BBFD-39C78B6FA1EA}']
    procedure SetOnRequest(aRequestEvent : TRequestEvent);
    function GetOnRequest : TRequestEvent;
    property OnNewRequest : TRequestEvent read GetOnRequest write SetOnRequest;
    function Logger : ILogger;
    procedure Start;
    procedure Stop;
  end;

  TCustomHttpServer = class(TInterfacedObject,IHttpServer)
  private
    fLogger : ILogger;
    fOnConnect : TOnConnectEvent;
    fOnDisconnect : TOnDisconnectEvent;
    procedure SetOnRequest(aRequestEvent : TRequestEvent);
    function GetOnRequest : TRequestEvent;
  protected
    fOnRequest : TRequestEvent;
    fHost : string;
    fPort : Integer;
    fSSLSecured : Boolean;
  public
    constructor Create(const aHost : string; aPort : Integer; aSSLEnabled : Boolean; aLogger : ILogger = nil); virtual;
    property Host : string read fHost;
    property Port : Integer read fPort;
    property OnNewRequest : TRequestEvent read GetOnRequest write SetOnRequest;
    property OnConnect : TOnConnectEvent read fOnConnect write fOnConnect;
    property OnDisconnect : TOnDisconnectEvent read fOnDisconnect write fOnDisconnect;
    function Logger : ILogger;
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
    procedure DoDisconnect;
    procedure OnConnect(aContext: TIdContext);
    procedure OnDisconnect;
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

function TCustomHttpServer.GetOnRequest: TRequestEvent;
begin
  Result := fOnRequest;
end;

procedure TCustomHttpServer.SetOnRequest(aRequestEvent: TRequestEvent);
begin
  fOnRequest := aRequestEvent;
end;

function TCustomHttpServer.Logger: ILogger;
begin
  Result := fLogger;
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
  //fHTTPServer.OnExecute := DoConnect;
  fHTTPServer.OnQuerySSLPort := DoOnQuerySSLPort;
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
  if Assigned(fOnConnect) then fOnConnect;
end;

procedure THttpServer.DoDisconnect;
begin
  if Assigned(fOnDisconnect) then fOnDisconnect;
end;

procedure THTTPServer.DoOnQuerySSLPort(aPort: Word; var vUseSSL: Boolean);
begin
  vUseSSL := (aPort <> 443);
end;

procedure THTTPServer.OnConnect(aContext: TIdContext);
begin
  Logger.Debug('Client connected');
end;

procedure THTTPServer.OnDisconnect;
begin
  Logger.Debug('Client disconnected!');
end;

procedure THTTPServer.OnGetRequest(aContext: TIdContext; aRequestInfo: TIdHTTPRequestInfo; aResponseInfo: TIdHTTPResponseInfo);
var
  request : IHttpRequest;
  response : IHttpResponse;
begin
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
      if E.ClassType <> EControlledException then
      begin
        if response.StatusCode = 200 then
        begin
          response.StatusCode := 500;
          response.StatusText := 'Internal server error';
        end;
        response.ContentText := e.Message;
      end
      else response.ContentText := response.ContentText + '<BR>' + e.Message;
    end;
  end;
  //check if need return error page
  if response.StatusCode > 399 then
  begin
    response.ContentText := Format('<h2>%d Error: %s</h2>',[response.StatusCode,response.StatusText])
          + Format('<h4>Message: %s</h4>',[response.ContentText]);
  end;
  //return response to client
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
