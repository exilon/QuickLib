{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.Config
  Description : Load/Save config from/to JSON file
  Author      : Kike Pérez
  Version     : 1.4
  Created     : 26/01/2017
  Modified    : 07/04/2018

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

{$i QuickLib.inc}

uses
  Classes,
  SysUtils,
  Rtti,
  {$IFDEF DELPHIRX102_UP}
    DBXJSON,
    JSON.Types,
    JSON.Serializers;
  {$ELSE}
    {$IFDEF FPC}
    fpjson,
    jsonparser,
    fpjsonrtti,
    Quick.Json.Serializer;
    {$ELSE}
    Quick.Json.Serializer,
    DBXJSON,
    System.JSON,
    Rest.Json.Types,
    Rest.Json;
    {$ENDIF}
  {$ENDIF}

type

  TDateTimeZone = (tzLocal, tzUTC);

  IAppConfigProvider<T> = interface
  ['{D55B1EBF-47F6-478B-8F70-9444575CB825}']
    procedure Load(var cConfig : T);
    procedure Save(var cConfig : T);
  end;

  TSerializeProperty = (spPublic, spPublished);

  TAppConfigProviderBase<T : class> = class(TInterfacedObject,IAppConfigProvider<T>)
  private
    fCreateIfNotExists : Boolean;
    fSerializeLevel : TSerializeProperty;
  public
    constructor Create(var cConfig : T); virtual;
    property CreateIfNotExists : Boolean read fCreateIfNotExists write fCreateIfNotExists;
    property SerializeLevel : TSerializeProperty read fSerializeLevel write fSerializeLevel;
    function InitObject : T;
    procedure Load(var cConfig : T); virtual; abstract;
    procedure Save(var cConfig : T); virtual; abstract;
  end;

  TApplyConfigEvent = procedure of object;

  {$IFDEF DELPHIXE2_UP}[JsonSerialize(TJsonMemberSerialization.&Public)]{$ENDIF}
  TAppConfig = class
  private
    {$IFDEF FPC}
    fOnApplyConfig : TApplyConfigEvent;
    fDateTimeZone: TDateTimeZone;
    fJsonIndent: Boolean;
    fLastSaved : TDateTime;
    {$ELSE}
    {$IF CompilerVersion < 32.0}[JSONMarshalledAttribute(False)]{$ENDIF}
    fOnApplyConfig : TApplyConfigEvent;
    {$IF CompilerVersion < 32.0}[JSONMarshalledAttribute(False)]{$ENDIF}
    fDateTimeZone: TDateTimeZone;
    {$IF CompilerVersion < 32.0}[JSONMarshalledAttribute(False)]{$ENDIF}
    fJsonIndent: Boolean;
    {$IF CompilerVersion < 32.0}[JSONMarshalledAttribute(False)]{$ENDIF}
    fLastSaved : TDateTime;
    {$ENDIF}
  public
    constructor Create; virtual;
    {$IFDEF DELPHIRX102_UP}[JsonIgnoreAttribute]{$ENDIF}
    property OnApplyConfig : TApplyConfigEvent read fOnApplyConfig write fOnApplyConfig;
    {$IFDEF DELPHIRX102_UP}[JsonIgnoreAttribute]{$ENDIF}
    property DateTimeZone : TDateTimeZone read fDateTimeZone write fDateTimeZone;
    {$IFDEF DELPHIRX102_UP}[JsonIgnoreAttribute]{$ENDIF}
    property JsonIndent : Boolean read fJsonIndent write fJsonIndent;
    {$IFDEF DELPHIRX102_UP}[JsonIgnoreAttribute]{$ENDIF}
    property LastSaved : TDateTime read fLastSaved write fLastSaved;
    procedure Apply;
    procedure DefaultValues; virtual;
    function ToJSON : string;
    procedure FromJSON(const json : string);
  end;

  {Usage: create a descend class from TAppConfig and add published properties to be loaded/saved

  TMyConfig = class(TAppConfig)
  private
    fName : string;
    fSurname : string;
    fStatus : Integer;
  published
    property Name : string read fName write fName;
    property SurName : string read fSurname write fSurname;
    property Status : Integer read fStatus write fStatus;
  end;

  AppConfigProvider := TAppConfigJsonProvider<TMyConfig>.Create(MyConfig);
  MyConfig.Name := 'John';
  }

implementation


{ TAppConfigProviderBase }

constructor TAppConfigProviderBase<T>.Create(var cConfig : T);
begin
  fCreateIfNotExists := True;
  fSerializeLevel := spPublished;
  //create object with rtti
  if Assigned(cConfig) then cConfig.Free;
  cConfig := InitObject;
end;

function TAppConfigProviderBase<T>.InitObject : T;
var
  AValue: TValue;
  ctx: TRttiContext;
  rType: TRttiType;
  AMethCreate: TRttiMethod;
begin
  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(TypeInfo(T));
    for AMethCreate in rType.GetMethods do
    begin
      if (AMethCreate.IsConstructor) and (Length(AMethCreate.GetParameters) = 0) then
      begin
        {$IFDEF FPC}
        Result := T(GetClass(T.ClassName).Create);
        {$ELSE}
        AValue := AMethCreate.Invoke(rType.AsInstance.AsInstance.MetaclassType,[]);
        Result := AValue.AsType<T>;
        {$ENDIF}
        Break;
      end;
    end;
  finally
    ctx.Free;
  end;
end;

{ TAppConfig }

constructor TAppConfig.Create;
begin
  fDateTimeZone := TDateTimeZone.tzLocal;
  fJsonIndent := True;
  fLastSaved := 0;
end;

procedure TAppConfig.Apply;
begin
  if Assigned(fOnApplyConfig) then fOnApplyConfig;
end;

procedure TAppConfig.DefaultValues;
begin
  //inherit to set default values if no config exists before
end;


function TAppConfig.ToJSON : string;
var
  Serializer : TJsonSerializer;
begin
  Result := '';
  try
    {$IFDEF DELPHIRX102_UP}
      Serializer := TJsonSerializer.Create;
      try
        Serializer.Formatting := TJsonFormatting.Indented;
        if JsonIndent then Serializer.Formatting := TJsonFormatting.Indented;
        if DateTimeZone = TDateTimeZone.tzLocal then
        begin
          Serializer.DateTimeZoneHandling := TJsonDateTimeZoneHandling.Local;
          Serializer.DateFormatHandling := TJsonDateFormatHandling.FormatSettings;
        end
        else Serializer.DateTimeZoneHandling := TJsonDateTimeZoneHandling.Utc;
        Result := Serializer.Serialize<TObject>(Self);
      finally
        Serializer.Free;
      end;
    {$ELSE}
      serializer := TJsonSerializer.Create(slPublishedProperty);
      try
        //Streamer.Options := Streamer.Options + [jsoDateTimeAsString ,jsoUseFormatString];
        //Streamer.DateTimeFormat := 'yyyy-mm-dd"T"hh:mm:ss.zz';
        Result := serializer.ObjectToJSON(Self);
      finally
        serializer.Free;
      end;
    {$ENDIF}
  except
    on e : Exception do raise Exception.Create(e.Message);
  end;
end;

procedure TAppConfig.FromJSON(const json : string);
var
  Serializer : TJsonSerializer;
begin
  try
    {$IFDEF DELPHIRX102_UP}
      Serializer := TJsonSerializer.Create;
      try
        Serializer.Formatting := TJsonFormatting.Indented;
        if JsonIndent then Serializer.Formatting := TJsonFormatting.Indented;
        if DateTimeZone = TDateTimeZone.tzLocal then
        begin
          Serializer.DateTimeZoneHandling := TJsonDateTimeZoneHandling.Local;
          Serializer.DateFormatHandling := TJsonDateFormatHandling.FormatSettings;
        end
        else Serializer.DateTimeZoneHandling := TJsonDateTimeZoneHandling.Utc;
        Self := Serializer.Deserialize<TAppConfig>(json);
      finally
        Serializer.Free;
      end;
    {$ELSE}
      serializer := TJsonSerializer.Create(slPublishedProperty);
      try
        //Streamer.Options := Streamer. .Options + [jsoDateTimeAsString ,jsoUseFormatString];
        //Streamer.DateTimeFormat := 'yyyy-mm-dd"T"hh:mm:ss.zz';
        Self := TAppConfig(serializer.JsonToObject(Self,json));
      finally
        serializer.Free;
      end;
    {$ENDIF}
  except
    on e : Exception do raise Exception.Create(e.Message);
  end;
end;

end.
