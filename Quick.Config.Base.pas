{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.Config
  Description : Load/Save config from/to JSON file
  Author      : Kike Pérez
  Version     : 1.5
  Created     : 26/01/2017
  Modified    : 10/12/2018

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

unit Quick.Config.Base;

interface

{$i QuickLib.inc}

uses
  Classes,
  SysUtils,
  Rtti,
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

type

  TDateTimeZone = (tzLocal, tzUTC);

  TAppConfig = class;

  IAppConfigProvider = interface
  ['{D55B1EBF-47F6-478B-8F70-9444575CB825}']
    procedure Load(cConfig : TAppConfig);
    procedure Save(cConfig : TAppConfig);
  end;

  TSerializeProperty = (spPublic, spPublished);

  TAppConfigProviderBase = class(TInterfacedObject,IAppConfigProvider)
  private
    fCreateIfNotExists : Boolean;
    fSerializeLevel : TSerializeProperty;
  protected
    procedure Load(cConfig : TAppConfig); virtual; abstract;
    procedure Save(cConfig : TAppConfig); virtual; abstract;
  public
    constructor Create; virtual;
    property CreateIfNotExists : Boolean read fCreateIfNotExists write fCreateIfNotExists;
    property SerializeLevel : TSerializeProperty read fSerializeLevel write fSerializeLevel;
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
  protected
    function GetProvider : IAppConfigProvider; virtual; abstract;
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
    procedure Load;
    procedure Save;
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

constructor TAppConfigProviderBase.Create;
begin
  fCreateIfNotExists := True;
  fSerializeLevel := spPublished;
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
    serializer := TJsonSerializer.Create(slPublishedProperty);
    try
      Result := serializer.ObjectToJSON(Self,True);
    finally
      serializer.Free;
    end;
  except
    on e : Exception do raise Exception.Create(e.Message);
  end;
end;

procedure TAppConfig.FromJSON(const json : string);
var
  Serializer : TJsonSerializer;
begin
  try
    serializer := TJsonSerializer.Create(slPublishedProperty);
    try
      {$IF NOT DEFINED(FPC) AND DEFINED(ANDROID)}
      serializer.JsonToObject(Self,json);
      {$ELSE}
      Self := TAppConfig(serializer.JsonToObject(Self,json));
      {$ENDIF}
    finally
      serializer.Free;
    end;
  except
    on e : Exception do raise Exception.Create(e.Message);
  end;
end;

procedure TAppConfig.Load;
begin
  GetProvider.Load(Self);
end;

procedure TAppConfig.Save;
begin
  GetProvider.Save(Self);
end;

end.
