{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.HttpServer.Response
  Description : Http Server Response
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 30/08/2019
  Modified    : 06/10/2019

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

unit Quick.HttpServer.Response;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  Quick.Value,
  Quick.Commons;

type
  IHttpResponse = interface
  ['{3E90F34D-5F4D-41E5-89C5-CA9832C7405E}']
    procedure SetStatusCode(const Value: Cardinal);
    procedure SetStatusText(const Value: string);
    function GetStatusCode: Cardinal;
    function GetStatusText: string;
    function GetHeaders: TPairList;
    procedure SetHeaders(const Value: TPairList);
    function GetContentStream: TStream;
    procedure SetContentStream(const Value: TStream);
    function GetContentText: string;
    procedure SetContentText(const Value: string);
    function GetContentType: string;
    procedure SetContentType(const Value: string);
    property Headers : TPairList read GetHeaders write SetHeaders;
    property StatusCode : Cardinal read GetStatusCode write SetStatusCode;
    property StatusText : string read GetStatusText write SetStatusText;
    property Content : TStream read GetContentStream write SetContentStream;
    property ContentText : string read GetContentText write SetContentText;
    property ContentType : string read GetContentType write SetContentType;
  end;

  {$M+}
  THttpResponse = class(TInterfacedObject,IHttpResponse)
  private
    fHeaders : TPairList;
    fStatusText: string;
    fStatusCode: Cardinal;
    fContentText : string;
    fContent : TStream;
    fContentType : string;
    procedure SetStatusCode(const Value: Cardinal);
    procedure SetStatusText(const Value: string);
    function GetStatusCode: Cardinal;
    function GetStatusText: string;
    function GetContentText: string;
    function GetContentStream: TStream;
    procedure SetContentText(const Value: string);
    procedure SetContentStream(const Value: TStream);
    function GetHeaders: TPairList;
    procedure SetHeaders(const Value: TPairList);
    function GetContentType: string;
    procedure SetContentType(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ContentFromStream(const Value: TStream);
  published
    property Headers : TPairList read GetHeaders write SetHeaders;
    property StatusCode : Cardinal read GetStatusCode write SetStatusCode;
    property StatusText : string read GetStatusText write SetStatusText;
    property Content : TStream read GetContentStream write SetContentStream;
    property ContentText : string read GetContentText write SetContentText;
    property ContentType : string read GetContentType write SetContentType;
  end;
  {$M-}

implementation

{ THttpResponse }

constructor THttpResponse.Create;
begin
  fContentText := '';
  fContent := nil;
  fStatusCode := 200;
  fStatusText := '';
  //add custom header to response
  fHeaders := TPairList.Create;
  fHeaders.Add('Server','QuickHttpServer');
end;

destructor THttpResponse.Destroy;
begin
  fHeaders.Free;
  if Assigned(fContent) and (fContent <> nil) then fContent.Free;
  inherited;
end;

function THttpResponse.GetContentStream: TStream;
begin
  Result := fContent;
end;

function THttpResponse.GetContentText: string;
begin
  Result := fContentText;
end;

function THttpResponse.GetContentType: string;
begin
  Result := fContentType;
end;

function THttpResponse.GetHeaders: TPairList;
begin
  Result := fHeaders;
end;

function THttpResponse.GetStatusCode: Cardinal;
begin
  Result := fStatusCode;
end;

function THttpResponse.GetStatusText: string;
begin
  Result := fStatusText;
end;

procedure THttpResponse.SetStatusCode(const Value: Cardinal);
begin
  fStatusCode := Value;
end;

procedure THttpResponse.SetStatusText(const Value: string);
begin
  fStatusText := Value;
end;

procedure THttpResponse.SetContentStream(const Value: TStream);
begin
  fContent := Value;
end;

procedure THttpResponse.SetContentText(const Value: string);
begin
  fContentText := Value;
end;

procedure THttpResponse.SetContentType(const Value: string);
begin
  fContentType := Value;
end;

procedure THttpResponse.SetHeaders(const Value: TPairList);
begin
  fHeaders := Value;
end;

procedure THttpResponse.ContentFromStream(const Value: TStream);
begin
  if Value <> nil then
  begin
    if fContent = nil then fContent := TMemoryStream.Create;
    fContent.CopyFrom(Value,Value.Size);
  end
  else fContent := nil;
end;

end.
