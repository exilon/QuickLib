{ ***************************************************************************

  Copyright (c) 2015-2020 Kike Pérez

  Unit        : Quick.Options.Serializer.Json
  Description : Configuration groups Json Serializer
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 18/10/2019
  Modified    : 15/04/2020

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

unit Quick.Options.Serializer.Json;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  System.Json,
  Quick.Commons,
  Quick.JSON.Utils,
  Quick.Json.Serializer,
  Quick.Options;

type

  TJsonOptionsSerializer = class(TOptionsSerializer)
  private
    fSerializer : TRTTIJson;
    function ParseFile(const aFilename : string; out aJsonObj : TJsonObject) : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Load(const aFilename : string; aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean; override;
    function LoadSection(const aFilename : string; aSections : TSectionList; aOptions: TOptions) : Boolean; override;
    procedure Save(const aFilename : string; aSections : TSectionList); override;
    function GetFileSectionNames(const aFilename : string; out oSections : TArray<string>) : Boolean; override;
  end;

implementation

{ TJsonOptionsSerializer }

constructor TJsonOptionsSerializer.Create;
begin
  fSerializer := TRTTIJson.Create(TSerializeLevel.slPublishedProperty,True);
end;

destructor TJsonOptionsSerializer.Destroy;
begin
  fSerializer.Free;
  inherited;
end;

function TJsonOptionsSerializer.GetFileSectionNames(const aFilename: string; out oSections: TArray<string>): Boolean;
var
  json : TJsonObject;
  i : Integer;
begin
  Result := False;
  json := nil;
  if ParseFile(aFilename,json) then
  begin
    try
      for i := 0 to json.Count - 1 do
      begin
        oSections := oSections + [json.Pairs[i].JsonString.Value];
      end;
      Result := True;
    finally
      json.Free;
    end;
  end;
end;

function TJsonOptionsSerializer.ParseFile(const aFilename : string; out aJsonObj : TJsonObject) : Boolean;
var
  fileoptions : string;
begin
  aJsonObj := nil;
  if FileExists(aFilename) then
  begin
    fileoptions := TFile.ReadAllText(aFilename,TEncoding.UTF8);
    aJsonObj := TJsonObject.ParseJSONValue(fileoptions) as TJsonObject;
    if aJsonObj = nil then raise EOptionLoadError.CreateFmt('Config file "%s" is damaged or not well-formed Json format!',[ExtractFileName(aFilename)]);
  end;
  Result := aJsonObj <> nil;
end;

function TJsonOptionsSerializer.Load(const aFilename : string; aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean;
var
  option : TOptions;
  json : TJsonObject;
  jpair : TJSONPair;
  found : Integer;
begin
  Result := False;
  if ParseFile(aFilename,json) then
  begin
    found := 0;
    try
      for option in aSections do
      begin
        jpair := fSerializer.GetJsonPairByName(json,option.Name);
        if jpair = nil then
        begin
          if aFailOnSectionNotExists then raise Exception.CreateFmt('Config section "%s" not found',[option.Name])
          else
          begin
            //count as found if hidden
            if option.HideOptions then Inc(found);
            Continue;
          end;
        end;
        if jpair.JsonValue <> nil then
        begin
          //deserialize option
          fSerializer.DeserializeObject(option,jpair.JsonValue as TJSONObject);
          //validate loaded configuration
          option.ValidateOptions;
          Inc(found);
        end;
      end;
    finally
      json.Free;
    end;
    //returns true if all sections located into file
    Result := found = aSections.Count;
  end;
end;

function TJsonOptionsSerializer.LoadSection(const aFilename : string; aSections : TSectionList; aOptions: TOptions) : Boolean;
var
  json : TJsonObject;
  jpair : TJSONPair;
begin
  Result := False;
  //read option file
  if ParseFile(aFilename,json) then
  begin
    try
      jpair := fSerializer.GetJsonPairByName(json,aOptions.Name);
      if (jpair <> nil) and (jpair.JsonValue <> nil) then
      begin
        //deserialize option
        fSerializer.DeserializeObject(aOptions,jpair.JsonValue as TJSONObject);
        //validate loaded configuration
        aOptions.ValidateOptions;
        Result := True;
      end;
    finally
      json.Free;
    end;
  end;
end;

procedure TJsonOptionsSerializer.Save(const aFilename : string; aSections : TSectionList);
var
  option : TOptions;
  fileoptions : string;
  json : TJSONObject;
  jpair : TJSONPair;
begin
  json := TJSONObject.Create;
  try
    for option in aSections do
    begin
      if not option.HideOptions then
      begin
        //validate configuration before save
        option.ValidateOptions;
        //serialize option
        jpair := TJSONPair.Create(option.Name,fSerializer.SerializeObject(option));
        json.AddPair(jpair);
      end;
    end;
    fileoptions := TJsonUtils.JsonFormat(json.ToJSON);
    if not fileoptions.IsEmpty then TFile.WriteAllText(aFilename,fileoptions);
  finally
    json.Free;
  end;
end;

end.
