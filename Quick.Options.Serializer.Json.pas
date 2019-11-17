{ ***************************************************************************

  Copyright (c) 2015-2019 Kike Pérez

  Unit        : Quick.Options.Serializer.Json
  Description : Configuration groups Json Serializer
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 18/10/2019
  Modified    : 22/10/2019

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
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const aFilename : string; aSections : TSectionList); override;
    procedure Save(const aFilename : string; aSections : TSectionList); override;
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

procedure TJsonOptionsSerializer.Load(const aFilename : string; aSections : TSectionList);
var
  option : TOptions;
  fileoptions : string;
  json : TJsonObject;
  jpair : TJSONPair;
begin
  if FileExists(aFilename) then
  begin
    //read option file
    fileoptions := TFile.ReadAllText(aFilename,TEncoding.UTF8);
    json := TJSONObject.ParseJSONValue(fileoptions) as TJSONObject;
    for option in aSections do
    begin
      jpair := fSerializer.GetJsonPairByName(json,option.Name);
      if jpair = nil then raise Exception.CreateFmt('Config section "%s" not found',[option.Name]);
      if jpair.JsonValue <> nil then
      begin
        //deserialize option
        fSerializer.DeserializeObject(option,jpair.JsonValue as TJSONObject);
        //validate loaded configuration
        option.ValidateOptions;
      end;
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
      //validate configuration before save
      option.ValidateOptions;
      //serialize option
      jpair := fSerializer.Serialize(option.Name,option);
      json.AddPair(jpair);
    end;
    fileoptions := TJsonUtils.JsonFormat(json.ToJSON);
    TFile.WriteAllText(aFilename,fileoptions);
  finally
    json.Free;
  end;
end;

end.
