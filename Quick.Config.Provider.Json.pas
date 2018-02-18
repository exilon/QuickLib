{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.Config.Provider.Json
  Description : Save config to JSON file
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 21/10/2017
  Modified    : 12/02/2018

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
unit Quick.Config.Provider.Json;

interface

uses
  Classes,
  System.SysUtils,
  System.IOUtils,
  System.Rtti,
  {$IF CompilerVersion >= 32.0}
    System.JSON.Types,
    System.JSON.Serializers,
  {$ELSE}
    Rest.Json.Types,
    System.JSON,
    Rest.Json,
  {$ENDIF}
  Quick.Config;

type

  TAppConfigJsonProvider<T: class> = class(TAppConfigProviderBase<T>)
  private
    fFilename : string;
  public
    constructor Create(var cConfig : T); override;
    property Filename : string read fFilename write fFilename;
    procedure Load(var cConfig : T); override;
    procedure Save(var cConfig : T); override;
  end;


implementation


constructor TAppConfigJsonProvider<T>.Create(var cConfig : T);
begin
  inherited Create(cConfig);
  fFilename := TPath.ChangeExtension(ParamStr(0),'json');
end;

procedure TAppConfigJsonProvider<T>.Load(var cConfig : T);
var
  json : TStrings;
  {$IF CompilerVersion >= 32.0}
    Serializer : TJsonSerializer;
  {$ENDIF}
  NewObj : T;
begin
  //create object with rtti if nil
  //if not Assigned(Config) then Config := InitObject;

  if (not FileExists(fFilename)) and (CreateIfNotExists) then
  begin
    TAppConfig(cConfig).DefaultValues;
    Self.Save(cConfig);
  end;

  try
    json := TStringList.Create;
    try
      json.LoadFromFile(fFilename);
      {$IF CompilerVersion >= 32.0}
        Serializer := TJsonSerializer.Create;
        try
          if TAppConfig(cConfig).DateTimeZone = TDateTimeZone.tzLocal then
          begin
            Serializer.DateTimeZoneHandling := TJsonDateTimeZoneHandling.Local;
            Serializer.DateFormatHandling := TJsonDateFormatHandling.FormatSettings;
          end
          else Serializer.DateTimeZoneHandling := TJsonDateTimeZoneHandling.Utc;
          NewObj := Serializer.Deserialize<T>(json.Text);
        finally
          Serializer.Free;
        end;
      {$ELSE}
        TJson.JsonToObject(cConfig,TJSONObject(TJSONObject.ParseJSONValue(json.Text)));
      {$ENDIF}
      if Assigned(cConfig) then cConfig.Free;
      cConfig := NewObj;
    finally
      json.Free;
    end;
  except
    on e : Exception do raise e;
  end;
end;

procedure TAppConfigJsonProvider<T>.Save(var cConfig : T);
var
  json : TStrings;
  {$IF CompilerVersion >= 32.0}
    Serializer : TJsonSerializer;
  {$ENDIF}
  ctx : TRttiContext;
  rprop : TRttiProperty;
begin
  //create object with rtti if nil
  if not Assigned(cConfig) then cConfig := InitObject;

  try
    json := TStringList.Create;
    try
      {$IF CompilerVersion >= 32.0}
        Serializer := TJsonSerializer.Create;
        try
          if TAppConfig(cConfig).JsonIndent then Serializer.Formatting := TJsonFormatting.Indented;
          if TAppConfig(cConfig).DateTimeZone = TDateTimeZone.tzLocal then
          begin
            Serializer.DateTimeZoneHandling := TJsonDateTimeZoneHandling.Local;
            Serializer.DateFormatHandling := TJsonDateFormatHandling.FormatSettings;
          end
          else Serializer.DateTimeZoneHandling := TJsonDateTimeZoneHandling.Utc;
          json.Text := Serializer.Serialize<T>(cConfig);
        finally
          Serializer.Free;
        end;
      {$ELSE}
        json.Text := TJson.ObjectToJsonString(cConfig);
      {$ENDIF}
      json.SaveToFile(fFilename);
      ctx := TRttiContext.Create;
      try
        rprop := ctx.GetType(TypeInfo(T)).GetProperty('LastSaved');
        rprop.SetValue(TObject(cConfig),TValue.FromVariant(Now()));
      finally
        ctx.Free;
      end;
    finally
      json.Free;
    end;
  except
    on e : Exception do raise e;
  end;
end;


end.
