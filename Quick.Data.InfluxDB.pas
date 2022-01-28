{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.Data.InfluxDB
  Description : InfluxDB data provider
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 05/04/2019
  Modified    : 21/04/2020

  This file is part of QuickLogger: https://github.com/exilon/QuickLogger

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

unit Quick.Data.InfluxDB;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  DateUtils,
  Quick.Collections,
  Quick.HttpClient,
  Quick.Commons,
  Quick.Value,
  Quick.Arrays,
  Quick.Data.Custom;

type

  TInfluxDBData = class(TDataProvider)
  private
    fHTTPClient : TJsonHTTPClient;
    fURL : string;
    fFullURL : string;
    fDataBase : string;
    fUserName : string;
    fPassword : string;
    fUserAgent : string;
    fTags : TPairArray;
    fCreateDataBaseIfNotExists : Boolean;
    procedure CreateDataBase;
    function GenerateWriteQuery(const aMeasurement : string; aTagPairs : IList<TPair>; aFieldPairs : IList<TFlexPair>; aTime : TDateTime): string;
    procedure EscapeData(var aTags : string);
    procedure SetWriteURL;
    procedure SetPassword(const Value: string);
    procedure SetUserName(const Value: string);
    procedure Write(const aLine: string); overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    property URL : string read fURL write fURL;
    property DataBase : string read fDataBase write fDataBase;
    property UserName : string read fUserName write SetUserName;
    property Password : string read fPassword write SetPassword;
    property CreateDataBaseIfNotExists : Boolean read fCreateDataBaseIfNotExists write fCreateDataBaseIfNotExists;
    property UserAgent : string read fUserAgent write fUserAgent;
    property Tags : TPairArray read fTags write fTags;
    procedure Init; override;
    procedure Restart; override;
    procedure Stop; override;
    procedure Write(const aMeasurement : string; aFieldPairs : IList<TFlexPair>; aTime : TDateTime = 0); overload;
    procedure Write(const aMeasurement: string; aTagPairs : IList<TPair>; aFieldPairs: IList<TFlexPair>; aTime: TDateTime); overload;
    procedure Write(const aMeasurement: string; const aFieldKey : string; aFieldValue : TFlexValue; aTime: TDateTime); overload;
  end;

  EInfluxDBData = class(Exception);

implementation

constructor TInfluxDBData.Create;
begin
  inherited;
  fURL := 'http://localhost:8086';
  fDataBase := 'db';
  fUserName := '';
  fPassword := '';
  fCreateDataBaseIfNotExists := True;
  OutputOptions.UseUTCTime := True;
  fUserAgent := DEF_USER_AGENT;
end;

destructor TInfluxDBData.Destroy;
begin
  if Assigned(fHTTPClient) then fHTTPClient.Free;
  inherited;
end;

procedure TInfluxDBData.Init;
begin
  if fInitiated then Stop;
  SetWriteURL;
  fHTTPClient := TJsonHTTPClient.Create;
  fHTTPClient.ContentType := 'application/json';
  fHTTPClient.UserAgent := fUserAgent;
  fHTTPClient.HandleRedirects := True;
  if fCreateDataBaseIfNotExists then CreateDataBase;
  inherited;
end;

procedure TInfluxDBData.Restart;
begin
  Stop;
  if Assigned(fHTTPClient) then FreeAndNil(fHTTPClient);
  Init;
end;

procedure TInfluxDBData.SetPassword(const Value: string);
begin
  if fPassword <> Value then
  begin
    fPassword := Value;
    SetWriteURL;
  end;
end;

procedure TInfluxDBData.SetWriteURL;
begin
  if fUserName+fPassword <> '' then fFullURL := Format('%s/write?db=%s&u=%s&p=%s&precision=ms',[fURL,fDataBase,fUserName,fPassword])
    else fFullURL := Format('%s/write?db=%s&precision=ms',[fURL,fDataBase]);
end;

procedure TInfluxDBData.Stop;
begin
  inherited;
  if Assigned(fHTTPClient) then FreeAndNil(fHTTPClient);
end;

procedure TInfluxDBData.Write(const aMeasurement: string; const aFieldKey : string; aFieldValue : TFlexValue; aTime: TDateTime);
var
  fields : IList<TFlexPair>;
begin
  fields := TxList<TFlexPair>.Create;
  fields.Add(TFlexPair.Create(aFieldKey,aFieldValue));
  if atime <> 0 then Write(GenerateWriteQuery(aMeasurement,nil,fields,aTime))
    else Write(GenerateWriteQuery(aMeasurement,nil,fields,Now()));
end;

procedure TInfluxDBData.Write(const aMeasurement: string; aTagPairs : IList<TPair>; aFieldPairs: IList<TFlexPair>; aTime: TDateTime);
begin
  if atime <> 0 then Write(GenerateWriteQuery(aMeasurement,aTagPairs,aFieldPairs,aTime))
    else Write(GenerateWriteQuery(aMeasurement,aTagPairs,aFieldPairs,Now()));
end;

procedure TInfluxDBData.Write(const aMeasurement: string; aFieldPairs: IList<TFlexPair>; aTime: TDateTime);
begin
  if atime <> 0 then Write(GenerateWriteQuery(aMeasurement,nil,aFieldPairs,aTime))
    else Write(GenerateWriteQuery(aMeasurement,nil,aFieldPairs,Now()));
end;

procedure TInfluxDBData.SetUserName(const Value: string);
begin
  if fUserName <> Value then
  begin
    fUserName := Value;
    SetWriteURL;
  end;
end;

procedure TInfluxDBData.CreateDataBase;
var
  resp : IHttpRequestResponse;
begin
  try
    resp := fHTTPClient.Post(Format('%s/query?q=CREATE DATABASE %s',[fURL,fDatabase]),'');
  except
    on E : Exception do raise EInfluxDBData.CreateFmt('[TInfluxDBData] Creating DB: %s',[e.Message]);
  end;

  if not (resp.StatusCode in [200,204]) then
    raise EInfluxDBData.Create(Format('[TInfluxDBData] : Response %d : %s trying to create database',[resp.StatusCode,resp.StatusText]));
end;

procedure TInfluxDBData.EscapeData(var aTags : string);
begin
  aTags := StringReplace(aTags,' ','\ ',[rfReplaceAll]);
end;

function TInfluxDBData.GenerateWriteQuery(const aMeasurement : string; aTagPairs : IList<TPair>; aFieldPairs : IList<TFlexPair>; aTime : TDateTime): string;
var
  incinfo : TStringList;
  tags : string;
  fields : string;
  tagpair : TPair;
  flexpair : TFlexPair;
begin
  incinfo := TStringList.Create;
  try
    //add global tags
    for tagpair in fTags do
    begin
      if not tagpair.Value.IsEmpty then incinfo.Add(Format('%s=%s',[tagpair.Name,tagpair.Value]));
    end;
    //add current query tags
    if aTagPairs <> nil then
    begin
      for tagpair in aTagPairs do
      begin
        if not tagpair.Value.IsEmpty then incinfo.Add(Format('%s=%s',[tagpair.Name,tagpair.Value]));
      end;
    end;
    tags := CommaText(incinfo);
    EscapeData(tags);

    incinfo.Clear;

    for flexpair in aFieldPairs do
    begin
      if flexpair.Value.IsInteger then incinfo.Add(Format('%s=%d',[flexpair.Name,flexpair.Value.AsInt64]))
        else if flexpair.Value.IsFloating then incinfo.Add(Format('%s=%f',[flexpair.Name,flexpair.Value.AsExtended]))
        else incinfo.Add(Format('%s="%s"',[flexpair.Name,flexpair.Value.AsString]));
    end;
    fields := CommaText(incinfo);

    Result := Format('%s,%s %s %d',[aMeasurement,tags,fields,DateTimeToUnix(LocalTimeToUTC(aTime){$IFNDEF FPC},True{$ENDIF})*1000]);
  finally
    incinfo.Free;
  end;
end;

procedure TInfluxDBData.Write(const aLine : string);
var
  resp : IHttpRequestResponse;
  stream : TStringStream;
begin
  if not fInitiated then Init;

  stream := TStringStream.Create(aLine,TEncoding.UTF8);
  var a := aline;
  try
    try
      resp := fHTTPClient.Post(fFullURL,stream);
    except
      on E : Exception do raise EInfluxDBData.CreateFmt('[TInfluxDBData] Write Error: %s',[e.Message]);
    end;
  finally
    stream.Free;
  end;

  if not (resp.StatusCode in [200,204]) then
    raise EInfluxDBData.Create(Format('[TInfluxDBData] : Response %d : %s trying to post event',[resp.StatusCode,resp.StatusText]));
end;

end.
