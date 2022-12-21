{ ***************************************************************************

  Copyright (c) 2016-2021 Kike Pérez

  Unit        : Quick.HttpClient
  Description : Json Http Client
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 22/05/2018
  Modified    : 02/08/2021

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

unit Quick.HttpClient;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  Quick.Commons,
  {$IFDEF DELPHIXE8_UP}
  System.Net.HttpClient,
  System.Net.URLClient,
  System.NetConsts,
  System.JSON;
  {$ELSE}
    {$IFDEF DELPHIXE7_UP}
    System.JSON,
    {$ENDIF}
    IdHTTP,
    IdException,
    {$IFDEF FPC}
    fpjson;
    {$ELSE}
    Data.DBXJSON;
    {$ENDIF}
  {$ENDIF}

type

  IHttpRequestResponse = interface
  ['{64DC58F7-B551-4619-85E9-D13E781529CD}']
    function StatusCode : Integer;
    function StatusText : string;
    function Response : TJSONObject;
  end;
  THttpRequestResponse = class(TInterfacedObject,IHttpRequestResponse)
  private
    fStatusCode : Integer;
    fStatusText : string;
    fResponse : TJSONObject;
  public
    {$IFDEF DELPHIXE8_UP}
    constructor Create(aResponse : IHTTPResponse; const aContent : string);
    {$ELSE}
    constructor Create(aResponse : TIdHTTPResponse; const aContent : string);
    {$ENDIF}
    destructor Destroy; override;
    function StatusCode : Integer;
    function StatusText : string;
    function Response : TJSONObject;
  end;

  TJsonHttpClient = class
  private
    {$IFDEF DELPHIXE8_UP}
    fHTTPClient : System.Net.HttpClient.THTTPClient;
    {$ELSE}
    fHTTPClient : TIdHTTP;
    {$ENDIF}
    fUserAgent : string;
    fContentType : string;
    fResponseTimeout : Integer;
    fConnectionTimeout : Integer;
    fHandleRedirects : Boolean;
    procedure SetContentType(const aValue: string);
    procedure SetUserAgent(const aValue: string);
    procedure SetResponseTimeout(const aValue: Integer);
    procedure SetConnectionTimeout(const aValue: Integer);
    procedure SetHandleRedirects(const aValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property UserAgent : string read fUserAgent write SetUserAgent;
    property ContentType : string read fContentType write SetContentType;
    property ResponseTimeout : Integer read fResponseTimeout write SetResponseTimeout;
    property ConnectionTimeout : Integer read fConnectionTimeout write SetConnectionTimeout;
    property HandleRedirects : Boolean read fHandleRedirects write SetHandleRedirects;
    function Get(const aURL : string) : IHttpRequestResponse;
    function Post(const aURL, aInContent : string; aHeaders : TPairList = nil) : IHttpRequestResponse; overload;
    function Post(const aURL : string; aInContent : TStream) : IHttpRequestResponse; overload;
    function Post(const aURL : string; aJsonContent : TJsonObject) : IHttpRequestResponse; overload;
    function Put(const aURL, aInContent : string) : IHttpRequestResponse;
  end;

implementation

const
  DEF_USER_AGENT = 'XLHttpClient';


constructor TJsonHttpClient.Create;
begin
  {$IFDEF DELPHIXE8_UP}
  fHTTPClient := THTTPClient.Create;
  fHTTPClient.ContentType := 'application/json';
  fHTTPClient.UserAgent := DEF_USER_AGENT;
  {$ELSE}
  fHTTPClient := TIdHTTP.Create(nil);
  fHTTPClient.Request.ContentType := 'application/json';
  fHTTPClient.Request.UserAgent := DEF_USER_AGENT;
  {$ENDIF}
end;

destructor TJsonHttpClient.Destroy;
begin
  fHTTPClient.Free;
  inherited;
end;

function TJsonHttpClient.Get(const aURL : string) : IHttpRequestResponse;
var
  {$IFDEF DELPHIXE8_UP}
  resp : IHTTPResponse;
  {$ELSE}
  resp : TIdHTTPResponse;
  {$ENDIF}
  bodycontent : TStringStream;
  responsecontent : TStringStream;
begin
  bodycontent := TStringStream.Create;
  try
    responsecontent := TStringStream.Create;
    try
      {$IFDEF DELPHIXE8_UP}
      resp := fHTTPClient.Get(aURL,responsecontent,nil);
      {$ELSE}
        {$If Defined(FPC) OR Not Defined(DELPHIXE8_UP)}
        fHTTPClient.Get(aURL,responsecontent);
        {$ELSE}
        fHTTPClient.Get(aURL,responsecontent,nil);
        {$ENDIF}
      resp := fHTTPClient.Response;
      {$ENDIF}
      Result := THttpRequestResponse.Create(resp,responsecontent.DataString);
    finally
      responsecontent.Free;
    end;
  finally
    bodycontent.Free;
  end;
end;

function TJsonHttpClient.Post(const aURL, aInContent : string; aHeaders : TPairList = nil) : IHttpRequestResponse;
var
  pair : TPairItem;
  {$IFDEF DELPHIXE8_UP}
  resp : IHTTPResponse;
  headers : TArray<TNameValuePair>;
  {$ELSE}
  resp : TIdHTTPResponse;
  {$ENDIF}
  responsecontent : TStringStream;
  postcontent : TStringStream;
begin
  postcontent := TStringStream.Create(Utf8Encode(aInContent));
  try
    //postcontent.WriteString(aInContent);
    responsecontent := TStringStream.Create;
    try
      {$IFDEF DELPHIXE8_UP}
      if aHeaders <> nil then
      begin
        for pair in aHeaders do
        begin
          headers := headers + [TNameValuePair.Create(pair.Name,pair.Value)];
        end;
      end;
      resp := fHTTPClient.Post(aURL,postcontent,responsecontent,headers);
      {$ELSE}
        if aHeaders <> nil then
        begin
          for pair in aHeaders do
          begin
            fHttpClient.Request.CustomHeaders.Values[pair.Name] := pair.Value;
          end;
        end;
        {$IFDEF FPC}
        try
           fHTTPClient.Post(aURL,postcontent,responsecontent);
           fHTTPClient.Disconnect(False);
        except
          on E : Exception do
          begin
            if e.ClassType <> EIdConnClosedGracefully then raise;
          end;
        end;
        {$ELSE}
        fHTTPClient.Post(aURL,postcontent,responsecontent);
        {$ENDIF}
      resp := fHTTPClient.Response;
      {$ENDIF}
      Result := THttpRequestResponse.Create(resp,responsecontent.DataString);
    finally
      responsecontent.Free;
    end;
  finally
    postcontent.Free;
  end;
end;

function TJsonHttpClient.Post(const aURL : string; aInContent : TStream) : IHttpRequestResponse;
var
  {$IFDEF DELPHIXE8_UP}
  resp : IHTTPResponse;
  {$ELSE}
  resp : TIdHTTPResponse;
  {$ENDIF}
  responsecontent : TStringStream;
begin
  //postcontent.WriteString(aInContent);
  responsecontent := TStringStream.Create;
  try
    {$IFDEF DELPHIXE8_UP}
    resp := fHTTPClient.Post(aURL,aInContent,responsecontent);
    {$ELSE}
      {$IFDEF FPC}
      try
         fHTTPClient.Post(aURL,aInContent,responsecontent);
         fHTTPClient.Disconnect(False);
      except
        on E : Exception do
        begin
          if e.ClassType <> EIdConnClosedGracefully then raise;
        end;
      end;
      {$ELSE}
      fHTTPClient.Post(aURL,aInContent,responsecontent);
      {$ENDIF}
    resp := fHTTPClient.Response;
    {$ENDIF}
    Result := THttpRequestResponse.Create(resp,responsecontent.DataString);
  finally
    responsecontent.Free;
  end;
end;

function TJsonHttpClient.Post(const aURL : string; aJsonContent : TJsonObject) : IHttpRequestResponse;
begin
  {$IFDEF DELPHIXE8_UP}
   Result := Self.Post(aURL,aJsonContent.ToJSON);
  {$ELSE}
    {$IFDEF FPC}
     Result := Self.Post(aURL,aJsonContent.AsJson);
    {$ELSE}
     Result := Self.Post(aURL,aJsonContent.ToString);
    {$ENDIF}
  {$ENDIF}
end;

function TJsonHttpClient.Put(const aURL, aInContent : string) : IHttpRequestResponse;
var
  {$IFDEF DELPHIXE8_UP}
  resp : IHTTPResponse;
  {$ELSE}
  resp : TIdHTTPResponse;
  {$ENDIF}
  responsecontent : TStringStream;
  postcontent : TStringStream;
begin
  postcontent := TStringStream.Create(Utf8Encode(aInContent));
  try
    //postcontent.WriteString(aInContent);
    responsecontent := TStringStream.Create;
    try
      {$IFDEF DELPHIXE8_UP}
      resp := fHTTPClient.Put(aURL,postcontent,responsecontent);
      {$ELSE}
        {$IFDEF FPC}
        try
           fHTTPClient.Put(aURL,postcontent,responsecontent);
           fHTTPClient.Disconnect(False);
        except
          on E : Exception do
          begin
            if e.ClassType <> EIdConnClosedGracefully then raise;
          end;
        end;
        {$ELSE}
        fHTTPClient.Post(aURL,postcontent,responsecontent);
        {$ENDIF}
      resp := fHTTPClient.Response;
      {$ENDIF}
      Result := THttpRequestResponse.Create(resp,responsecontent.DataString);
    finally
      responsecontent.Free;
    end;
  finally
    postcontent.Free;
  end;
end;

procedure TJsonHttpClient.SetConnectionTimeout(const aValue: Integer);
begin
  fConnectionTimeout := aValue;
  {$IFDEF DELPHIXE8_UP}
    {$IFDEF DELPHIRX102_UP} //in previous versions don't exists ConnectionTimeout property
    fHTTPClient.ConnectionTimeout := aValue;
    {$ENDIF}
  {$ELSE}
  fHTTPClient.ConnectTimeout := aValue;
  {$ENDIF}
end;

procedure TJsonHttpClient.SetContentType(const aValue: string);
begin
  fContentType := aValue;
  {$IFDEF DELPHIXE8_UP}
  fHTTPClient.ContentType := aValue;
  {$ELSE}
  fHTTPClient.Request.ContentType := aValue;
  {$ENDIF}
end;

procedure TJsonHttpClient.SetHandleRedirects(const aValue: Boolean);
begin
  fHandleRedirects := aValue;
  {$IFDEF DELPHIXE8_UP}
  fHTTPClient.HandleRedirects := aValue;
  {$ELSE}
  fHTTPClient.HandleRedirects := aValue;
  {$ENDIF}
end;

procedure TJsonHttpClient.SetResponseTimeout(const aValue: Integer);
begin
  fResponseTimeout := aValue;
  {$IFDEF DELPHIXE8_UP}
    {$IFDEF DELPHIRX102_UP} //in previous versions don't exist ResponseTimeout property
    fHTTPClient.ResponseTimeout := aValue;
    {$ENDIF}
  {$ELSE}
  fHTTPClient.ReadTimeout := aValue;
  {$ENDIF}
end;

procedure TJsonHttpClient.SetUserAgent(const aValue: string);
begin
  fUserAgent := aValue;
  {$IFDEF DELPHIXE8_UP}
  fHTTPClient.UserAgent := aValue;
  {$ELSE}
  fHTTPClient.Request.UserAgent := aValue;
  {$ENDIF}
end;

{ THttpRequestResponse }

{$IFDEF DELPHIXE8_UP}
constructor THttpRequestResponse.Create(aResponse: IHTTPResponse; const aContent : string);
begin
  fStatusCode := aResponse.StatusCode;
  fStatusText := aResponse.StatusText;
  if aContent <> '' then fResponse := TJSONObject.ParseJSONValue(aContent) as TJSONObject;
  //if response is not json, get as json result
  if fResponse = nil then
  begin
    fResponse := TJSONObject.Create;
    fResponse.AddPair('Result',aContent);
  end;
end;
{$ELSE}
constructor THttpRequestResponse.Create(aResponse : TIdHTTPResponse; const aContent : string);
begin
  fStatusCode := aResponse.ResponseCode;
  fStatusText := aResponse.ResponseText;
  {$If Defined(FPC) OR Defined(DELPHIXE8_UP)}
  if (aContent.Contains('{')) and (aContent.Contains('}')) then fResponse := GetJSON(aContent) as TJsonObject;
  {$ELSE}
  if (aContent.Contains('{')) and (aContent.Contains('}')) then fResponse:= TJsonObject.ParseJSONValue(aContent) as TJsonObject;
  {$ENDIF}
  //if response is not json, get as json result
  if fResponse = nil then
  begin
    fResponse := TJSONObject.Create;
    {$IFDEF DELPHIXE4_UP}
    fResponse.AddPair('Result',aContent);
    {$ELSE}
    fResponse.Add('Result',aContent);
    {$ENDIF}
  end;
end;
{$ENDIF}

destructor THttpRequestResponse.Destroy;
begin
  if Assigned(fResponse) then fResponse.Free;
  inherited;
end;

function THttpRequestResponse.Response: TJSONObject;
begin
  Result := fResponse;
end;

function THttpRequestResponse.StatusCode: Integer;
begin
  Result := fStatusCode;
end;

function THttpRequestResponse.StatusText: string;
begin
  Result := fStatusText;
end;

end.
