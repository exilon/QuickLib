{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.Config.Provider.Json
  Description : Save config to JSON file
  Author      : Kike Pérez
  Version     : 1.4
  Created     : 21/10/2017
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
unit Quick.Config.Provider.Json;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF DELPHIXE_UP}
  IOUtils,
  {$ELSE}
  Quick.Files,
  {$ENDIF}
  Rtti,
  Quick.Json.Serializer,
  {$IFDEF FPC}
  fpjson,
  fpjsonrtti,
  {$ELSE}
  Rest.Json.Types,
  System.JSON,
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
  Serializer : TJsonSerializer;
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
      serializer := TJsonSerializer.Create(slPublishedProperty);
      try
        //Streamer.Options := Streamer.Options + [jsoDateTimeAsString ,jsoUseFormatString];
        //Streamer.DateTimeFormat := 'yyyy-mm-dd"T"hh:mm:ss.zz';
        serializer.JsonToObject(cConfig,json.Text);
        Exit;
      finally
        serializer.Free;
      end;
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
  Serializer : TJsonSerializer;
  ctx : TRttiContext;
  rprop : TRttiProperty;
begin
  //create object with rtti if nil
  if not Assigned(cConfig) then cConfig := InitObject;

  try
    json := TStringList.Create;
    try
      serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
      try
        //Streamer.Options := Streamer.Options + [jsoDateTimeAsString ,jsoUseFormatString];
        //Streamer.DateTimeFormat := 'yyyy-mm-dd"T"hh:mm:ss.zz';
        json.Text := serializer.ObjectToJson(cConfig,True);
      finally
        serializer.Free;
      end;
      json.SaveToFile(fFilename);
      {$IFDEF FPC}
      //TAppConfig(cConfig).LastSaved := Now;
      {$ELSE}
      ctx := TRttiContext.Create;
      try
        rprop := ctx.GetType(TypeInfo(T)).GetProperty('LastSaved');
        rprop.SetValue(TObject(cConfig),TValue.FromVariant(Now()));
      finally
        ctx.Free;
      end;
      {$ENDIF}
    finally
      json.Free;
    end;
  except
    on e : Exception do raise e;
  end;
end;


end.
