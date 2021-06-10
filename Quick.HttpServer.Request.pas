{ ***************************************************************************

  Copyright (c) 2016-2021 Kike Pérez

  Unit        : Quick.HttpServer.Request
  Description : Http Server Request
  Author      : Kike Pérez
  Version     : 1.8
  Created     : 30/08/2019
  Modified    : 07/02/2021

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

unit Quick.HttpServer.Request;

{$i QuickLib.inc}

interface

uses
  {$IFDEF DEBUG_HTTPSERVER}
  Quick.Debug.Utils,
  {$ENDIF}
  Classes,
  SysUtils,
  Quick.Commons,
  Quick.Arrays,
  Quick.Value,
  Quick.HttpServer.Types;

type
  EHttpRequestError = class(Exception);

type
  IHttpRequest = interface
  ['{D6B236A5-9D04-4380-8A89-5BD4CC60A1A6}']
    function GetPathSegment(aIndex : Integer) : string;
    function GetQuery(const aName : string) : TFlexValue;
    function GetURL: string;
    function GetMethod: TMethodVerb;
    function GetCacheControl: string;
    function GetClientIP: string;
    function GetContent: TStream;
    function GetHeaders: TPairList;
    function GetHost: string;
    function GetPort: Integer;
    function GetReferer: string;
    function GetUnparsedParams: string;
    function GetUserAgent: string;
    property URL : string read GetURL;
    property Method : TMethodVerb read GetMethod;
    property Host : string read GetHost;
    property Port : Integer read GetPort;
    property Referer : string read GetReferer;
    property UserAgent : string read GetUserAgent;
    property CacheControl : string read GetCacheControl;
    property PathSegment[aIndex : Integer] : string read GetPathSegment;
    property UnparsedParams : string read GetUnparsedParams;
    property Query[const aName : string] : TFlexValue read GetQuery;
    property ClientIP : string read GetClientIP;
    property Headers : TPairList read GetHeaders;
    property Content : TStream read GetContent;
    function ContentAsString : string;
    function GetMethodAsString: string;
  end;

  THttpRequest = class(TInterfacedObject,IHttpRequest)
  private
    fURL : string;
    fMethod : TMethodVerb;
    fHost : string;
    fPort : Integer;
    fReferer : string;
    fUserAgent : string;
    fCacheControl : string;
    fUnparsedParams : string;
    fParsedQuery : TFlexPairArray;
    fClientIP : string;
    fHeaders : TPairList;
    fContent : TStream;
    fContentType : string;
    fContentEncoding : string;
    fContentLength : Int64;
    function GetPathSegment(aIndex : Integer) : string;
    function GetQuery(const aName : string) : TFlexValue;
    procedure ParseQuery;
    function GetURL: string;
    function GetMethod: TMethodVerb;
    function GetCacheControl: string;
    function GetClientIP: string;
    function GetContent: TStream;
    function GetHeaders: TPairList;
    function GetHost: string;
    function GetPort: Integer;
    function GetReferer: string;
    function GetUnparsedParams: string;
    function GetUserAgent: string;
    procedure SetURL(const Value: string);
    function GetContentEncoding: string;
    function GetContentLength: Int64;
    function GetContentType: string;
    procedure SetContentEncoding(const Value: string);
    procedure SetContentLength(const Value: Int64);
    procedure SetContentType(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property URL : string read GetURL write SetURL;
    property Method : TMethodVerb read GetMethod write fMethod;
    property Host : string read GetHost write fHost;
    property Port : Integer read GetPort write fPort;
    property Referer : string read GetReferer write fReferer;
    property UserAgent : string read GetUserAgent write fUserAgent;
    property CacheControl : string read GetCacheControl write fCacheControl;
    property PathSegment[aIndex : Integer] : string read GetPathSegment;
    property UnparsedParams : string read GetUnparsedParams write fUnparsedParams;
    property Query[const aName : string] : TFlexValue read GetQuery;
    property ClientIP : string read GetClientIP write fClientIP;
    property Headers : TPairList read GetHeaders write fHeaders;
    property Content : TStream read GetContent write fContent;
    property ContentType : string read GetContentType write SetContentType;
    property ContentEncoding : string read GetContentEncoding write SetContentEncoding;
    property ContentLength : Int64 read GetContentLength write SetContentLength;
    procedure SetMethodFromString(const aVerbMethod : string);
    function GetMethodAsString: string;
    function ContentAsString : string;
  end;

implementation

function THttpRequest.ContentAsString: string;
begin
  {$IFDEF DEBUG_HTTPSERVER}
  TDebugger.Trace(Self,'ContentAsString Encode: %s',[ContentEncoding]);
  {$ENDIF}
  if fContent <> nil then Result := StreamToString(fContent,TEncoding.UTF8);
end;

constructor THttpRequest.Create;
begin
  fHeaders := TPairList.Create;
end;

destructor THttpRequest.Destroy;
begin
  fHeaders.Free;
  inherited;
end;

function THttpRequest.GetCacheControl: string;
begin
  Result := fCacheControl;
end;

function THttpRequest.GetClientIP: string;
begin
  Result := fClientIP;
end;

function THttpRequest.GetContent: TStream;
begin
  Result := fContent;
end;

function THttpRequest.GetContentEncoding: string;
begin
  Result := fContentEncoding;
end;

function THttpRequest.GetContentLength: Int64;
begin
  Result := fContentLength;
end;

function THttpRequest.GetContentType: string;
begin
  Result := fContentType;
end;

function THttpRequest.GetHeaders: TPairList;
begin
  Result := fHeaders;
end;

function THttpRequest.GetHost: string;
begin
  Result := fHost;
end;

function THttpRequest.GetMethod: TMethodVerb;
begin
  Result := fMethod;
end;

function THttpRequest.GetMethodAsString: string;
begin
  Result := MethodVerbStr[Integer(fMethod)];
end;

function THttpRequest.GetPathSegment(aIndex: Integer): string;
var
  upath : string;
  segment : TArray<string>;
begin
  try
    if fURL.StartsWith('/') then upath := furl.Substring(1)
      else upath := fURL;
    segment := upath.Split(['/']);
    if (High(segment) < aIndex) or (aIndex < 0) then raise EHttpRequestError.CreateFmt('param out of bounds (%d)',[aIndex]);
    Result := segment[aIndex];
  except
    on E : Exception do raise EHttpRequestError.CreateFmt('Error getting url path param : %s',[e.message]);
  end;
end;

function THttpRequest.GetPort: Integer;
begin
  Result := fPort;
end;

function THttpRequest.GetQuery(const aName: string): TFlexValue;
begin
  if fParsedQuery.Count = 0 then ParseQuery;
  Result := fParsedQuery.GetValue(aName);
end;

function THttpRequest.GetReferer: string;
begin
  Result := fReferer;
end;

function THttpRequest.GetUnparsedParams: string;
begin
  Result := fUnparsedParams;
end;

function THttpRequest.GetURL: string;
begin
  Result := fURL;
end;

function THttpRequest.GetUserAgent: string;
begin
  Result := fUserAgent;
end;

procedure THttpRequest.ParseQuery;
var
  param : string;
  pair : TFlexPair;
  posi : Integer;
begin
  for param in fUnparsedParams.Split(['&']) do
  begin
    posi := Pos('=',param);
    pair.Name := Copy(param,1,posi - 1);
    pair.Value := param.Substring(posi);
    fParsedQuery.Add(pair);
  end;
end;

procedure THttpRequest.SetContentEncoding(const Value: string);
begin
  fContentEncoding := Value;
end;

procedure THttpRequest.SetContentLength(const Value: Int64);
begin
  fContentLength := Value;
end;

procedure THttpRequest.SetContentType(const Value: string);
begin
  fContentType := Value;
end;

procedure THttpRequest.SetMethodFromString(const aVerbMethod: string);
var
  i : Integer;
begin
  fMethod := TMethodVerb.mUNKNOWN;
  for i := 0 to Ord(High(TMethodVerb)) do
  begin
    if CompareText(aVerbMethod,MethodVerbStr[i]) = 0 then
    begin
      fMethod := TMethodVerb(i);
      Exit;
    end;
  end;
end;

procedure THttpRequest.SetURL(const Value: string);
begin
  //remove first slash
  if Value.StartsWith('/') then fURL := Value.Substring(1)
    else fURL := Value;
  //remove last slash
  if fURL.EndsWith('/') then fURL := Copy(fURL,0,fURL.Length -1);
end;

end.
