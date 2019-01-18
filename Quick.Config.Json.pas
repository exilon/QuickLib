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
unit Quick.Config.Json;

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
  Quick.Config.Base;

type

  TAppConfigJsonProvider = class(TAppConfigProviderBase)
  private
    fFilename : string;
    procedure Load(cConfig : TAppConfig); override;
    procedure Save(cConfig : TAppConfig); override;
  public
    constructor Create; override;
    property Filename : string read fFilename write fFilename;
  end;

  TAppConfigJson = class(TAppConfig)
  private
    fProvider : TAppConfigJsonProvider;
    function GetProvider : IAppConfigProvider; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Provider : TAppConfigJsonProvider read fProvider write fProvider;
  end;


implementation

constructor TAppConfigJsonProvider.Create;
begin
  inherited Create;
  fFilename := TPath.ChangeExtension(ParamStr(0),'json');
end;

procedure TAppConfigJsonProvider.Load(cConfig : TAppConfig);
var
  json : TStrings;
  Serializer : TJsonSerializer;
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
      finally
        serializer.Free;
      end;
    finally
      json.Free;
    end;
  except
    on e : Exception do raise e;
  end;
end;

procedure TAppConfigJsonProvider.Save(cConfig : TAppConfig);
var
  json : TStrings;
  Serializer : TJsonSerializer;
  ctx : TRttiContext;
  rprop : TRttiProperty;
begin
  //create object with rtti if nil
  if not Assigned(cConfig) then cConfig := TAppConfigJson.Create;

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
      cConfig.LastSaved := Now;
    finally
      json.Free;
    end;
  except
    on e : Exception do raise e;
  end;
end;


{ TAppConfigJson }

constructor TAppConfigJson.Create;
begin
  inherited;
  fProvider := TAppConfigJsonProvider.Create;
end;


destructor TAppConfigJson.Destroy;
begin
  if Assigned(fProvider) then fProvider.Free;
  inherited;
end;

function TAppConfigJson.GetProvider: IAppConfigProvider;
begin
  Result := fProvider;
end;

end.
