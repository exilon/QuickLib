{ ***************************************************************************

  Copyright (c) 2015-2017 Kike Pérez

  Unit        : Quick.Config
  Description : Load/Save config from/to JSON file
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 26/01/2017
  Modified    : 29/09/2017

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

unit Quick.Config;

interface

uses
  System.Classes,
  System.SysUtils,
  {$IF CompilerVersion >= 32.0}
  System.JSON.Types,
  System.JSON.Serializers;
  {$ELSE}
  Rest.Json.Types,
  Rest.Json;
  {$ENDIF}

type

  [JsonSerialize(TJsonMemberSerialization.&Public)]
  TAppConfig = class
  private
    [JSONMarshalledAttribute(False)]
    fConfigFile : string;
    [JSONMarshalledAttribute(False)]
    fConfigEncrypted : Boolean;
    [JSONMarshalledAttribute(False)]
    fConfigPassword : string;
  public
    constructor Create(const ConfigFileName : string);
    [JsonIgnoreAttribute]
    property ConfigFile : string read fConfigFile;
    [JsonIgnoreAttribute]
    property ConfigEncrypted : Boolean read fConfigEncrypted write fConfigEncrypted;
    [JsonIgnoreAttribute]
    property ConfigPassword : string read fConfigPassword write fConfigPassword;
    function Load(CreateIfNotExists : Boolean = False) : Boolean;
    function Save : Boolean;
    function AsJsonString : string;
  end;

  {Usage: create a descend class from TAppConfig and add public properties to be loaded/saved

  TMyConfig = class(TAppConfig)
  private
    fName : string;
    fSurname : string;
    fStatus : Integer;
  public
    property Name : string read fName write fName;
    property SurName : string read fSurname write fSurname;
    property Status : Integer read fStatus write fStatus;
  end;
  }

implementation


{ TAppConfig }

constructor TAppConfig.Create(const ConfigFileName : string);
begin
  fConfigFile := ConfigFileName;
end;

function TAppConfig.Load(CreateIfNotExists : Boolean = False) : Boolean;
var
  json : TStrings;
  {$IF CompilerVersion >= 32.0}
    Serializer : TJsonSerializer;
  {$ENDIF}
begin
  if (CreateIfNotExists) and (not FileExists(fConfigFile)) then
  begin
    Self.Save;
    Result := False;
  end;

  try
    json := TStringList.Create;
    try
      json.LoadFromFile(fConfigFile);
      if fConfigEncrypted then
      begin
        //decrypt json
      end;
      {$IF CompilerVersion >= 32.0}
        Serializer := TJsonSerializer.Create;
        try
          Self := Serializer.Deserialize<TAppConfig>(json.Text);
        finally
          Serializer.Free;
        end;
      {$ELSE}
        Self := TJson.JsonToObject<TAppConfig>(json.Text)
      {$ENDIF}
    finally
      json.Free;
    end;
  except
    on e : Exception do raise e;
  end;
end;

function TAppConfig.Save : Boolean;
var
  json : TStrings;
  {$IF CompilerVersion >= 32.0}
    Serializer : TJsonSerializer;
  {$ENDIF}
begin
  try
    json := TStringList.Create;
    try
      if fConfigEncrypted then
      begin
        //encrypt json
      end;
      {$IF CompilerVersion >= 32.0}
        Serializer := TJsonSerializer.Create;
        try
          Serializer.Formatting := TJsonFormatting.Indented;
          json.Text := Serializer.Serialize(Self);
        finally
          Serializer.Free;
        end;
      {$ELSE}
        json.Text := TJson.ObjectToJsonString(Self);
      {$ENDIF}
      json.SaveToFile(fConfigFile);
    finally
      json.Free;
    end;
  except
    on e : Exception do raise e;
  end;
end;

function TAppConfig.AsJsonString : string;
{$IF CompilerVersion >= 32.0}
  var
    Serializer: TJsonSerializer;
{$ENDIF}
begin
  Result := '';
  {$IF CompilerVersion >= 32.0}
    Serializer := TJsonSerializer.Create;
    try
      Serializer.Formatting := TJsonFormatting.Indented;
      Result := Serializer.Serialize(Self);
    finally
      Serializer.Free;
    end;
  {$ELSE}
    Result := TJson.ObjectToJsonString(Self);
  {$ENDIF}
end;



end.
