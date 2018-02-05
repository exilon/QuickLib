{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.Config
  Description : Load/Save config from/to JSON file
  Author      : Kike Pérez
  Version     : 1.2
  Created     : 26/01/2017
  Modified    : 02/02/2018

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
  System.Rtti,
  {$IF CompilerVersion >= 32.0}
    JSON.Types,
    JSON.Serializers;
  {$ELSE}
    Rest.Json.Types,
    Rest.Json;
  {$ENDIF}

type

  TDateTimeZone = (tzLocal, tzUTC);

  IAppConfigProvider<T> = interface
  ['{D55B1EBF-47F6-478B-8F70-9444575CB825}']
    procedure Load(var cConfig : T);
    procedure Save(var cConfig : T);
  end;

  TAppConfigProviderBase<T : class> = class(TInterfacedObject,IAppConfigProvider<T>)
  private
    fCreateIfNotExists : Boolean;
  public
    constructor Create(var cConfig : T); virtual;
    property CreateIfNotExists : Boolean read fCreateIfNotExists write fCreateIfNotExists;
    function InitObject : T;
    procedure Load(var cConfig : T); virtual; abstract;
    procedure Save(var cConfig : T); virtual; abstract;
  end;

  TApplyConfigEvent = procedure of object;

  [JsonSerialize(TJsonMemberSerialization.&Public)]
  TAppConfig = class
  private
    {$IF CompilerVersion < 32.0}[JSONMarshalledAttribute(False)]{$ENDIF}
    fOnApplyConfig : TApplyConfigEvent;
    {$IF CompilerVersion < 32.0}[JSONMarshalledAttribute(False)]{$ENDIF}
    fDateTimeZone: TDateTimeZone;
    {$IF CompilerVersion < 32.0}[JSONMarshalledAttribute(False)]{$ENDIF}
    fJsonIndent: Boolean;
    {$IF CompilerVersion < 32.0}[JSONMarshalledAttribute(False)]{$ENDIF}
    fLastSaved : TDateTime;
  public
    constructor Create; virtual;
    //constructor Create(ADefaultValues : Boolean); overload; virtual; abstract;
    {$IF CompilerVersion >= 32.0}[JsonIgnoreAttribute]{$ENDIF}
    property OnApplyConfig : TApplyConfigEvent read fOnApplyConfig write fOnApplyConfig;
    {$IF CompilerVersion >= 32.0}[JsonIgnoreAttribute]{$ENDIF}
    property DateTimeZone : TDateTimeZone read fDateTimeZone write fDateTimeZone;
    {$IF CompilerVersion >= 32.0}[JsonIgnoreAttribute]{$ENDIF}
    property JsonIndent : Boolean read fJsonIndent write fJsonIndent;
    {$IF CompilerVersion >= 32.0}[JsonIgnoreAttribute]{$ENDIF}
    property LastSaved : TDateTime read fLastSaved write fLastSaved;
    procedure Apply;
    function ToJSON : string;
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

  AppConfigProvider := TAppConfigJsonProvider<TMyConfig>.Create(MyConfig);
  MyConfig.Name := 'John';
  }

implementation


{ TAppConfigProviderBase }

constructor TAppConfigProviderBase<T>.Create(var cConfig : T);
begin
  fCreateIfNotExists := True;
  //create object with rtti
  if Assigned(cConfig) then cConfig.Free;
  cConfig := InitObject;
end;

function TAppConfigProviderBase<T>.InitObject : T;
var
  AValue: TValue;
  ctx: TRttiContext;
  f : TRttiField;
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
        AValue := AMethCreate.Invoke(rType.AsInstance.AsInstance.MetaclassType,[]);
        Result := AValue.AsType<T>;
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


function TAppConfig.ToJSON : string;
{$IF CompilerVersion >= 32.0}
  var
    Serializer : TJsonSerializer;
  {$ENDIF}
begin
  Result := '';
  try
    {$IF CompilerVersion >= 32.0}
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
      Result := TJson.ObjectToJsonString(Self);
    {$ENDIF}
  except
    on e : Exception do raise Exception.Create(e.Message);
  end;
end;

end.
