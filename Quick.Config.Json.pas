{ ***************************************************************************

  Copyright (c) 2015-2021 Kike Pérez

  Unit        : Quick.Config.Json
  Description : Save config to JSON file
  Author      : Kike Pérez
  Version     : 1.5
  Created     : 21/10/2017
  Modified    : 10/03/2021

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
  Quick.FileMonitor,
  Quick.Config.Base;

type

  TFileModifiedEvent = procedure of object;
  TLoadConfigEvent = procedure of object;

  {$IFNDEF FPC}
  TNotSerializableProperty = Quick.Json.Serializer.TNotSerializableProperty;
  TCommentProperty = Quick.Json.Serializer.TCommentProperty;
  TCustomNameProperty = Quick.Json.Serializer.TCustomNameProperty;
  {$ENDIF}

  TAppConfigJsonProvider = class(TAppConfigProvider)
  private
    fFilename : string;
    fFileMonitor : TFileMonitor;
    fOnFileModified : TFileModifiedEvent;
    fLoaded : Boolean;
    fReloadIfFileChanged : Boolean;
    fOnConfigLoaded : TLoadConfigEvent;
    fOnConfigReloaded : TLoadConfigEvent;
    fNotifyReload : TLoadConfigEvent;
    procedure CreateFileMonitor;
    procedure FileModifiedNotify(MonitorNotify : TMonitorNotify);
    procedure SetFileName(const Value: string);
    procedure SetReloadIfFileChanged(const Value: Boolean);
    procedure SetReloadNotify(aNotifyReload : TLoadConfigEvent);
    procedure DoNofifyReload;
  protected
    procedure Load(cConfig : TAppConfig); override;
    procedure Save(cConfig : TAppConfig); override;
  public
    constructor Create(const aFilename : string; aReloadIfFileChanged : Boolean = False); virtual;
    destructor Destroy; override;
    property Filename : string read fFilename write SetFileName;
    property ReloadIfFileChanged : Boolean read fReloadIfFileChanged write SetReloadIfFileChanged;
    property IsLoaded : Boolean read fLoaded;
    property OnFileModified : TFileModifiedEvent read fOnFileModified write fOnFileModified;
    property OnConfigLoaded : TLoadConfigEvent read fOnConfigLoaded write fOnConfigLoaded;
    property OnConfigReloaded : TLoadConfigEvent read fOnConfigReloaded write fOnConfigReloaded;
  end;

  TAppConfigJson = class(TAppConfig)
  private
    function GetProvider : TAppConfigJsonProvider;
    procedure ReloadNotify;
  public
    constructor Create(const aFilename : string; aReloadIfFileChanged : Boolean = False); virtual;
    destructor Destroy; override;
    property Provider : TAppConfigJsonProvider read GetProvider;
  end;

  {Usage: create a descend class from TAppConfigJson and add published properties to be loaded/saved

  TMyConfig = class(TAppConfigJson)
  private
    fName : string;
    fSurname : string;
    fStatus : Integer;
  published
    property Name : string read fName write fName;
    property SurName : string read fSurname write fSurname;
    property Status : Integer read fStatus write fStatus;
  end;

  MyConfig := TMyConfig.Create;
  MyConfig.Provider.FileName := '.\MyAppName.json';
  MyConfig.Name := 'John';
  MyConfig.Save;
  }


implementation

constructor TAppConfigJsonProvider.Create(const aFilename : string; aReloadIfFileChanged : Boolean = False);
begin
  inherited Create;
  if aFilename = '' then fFilename := TPath.ChangeExtension(ParamStr(0),'json')
    else fFilename := aFilename;
  fLoaded := False;
  fReloadIfFileChanged := aReloadIfFileChanged;
  if aReloadIfFileChanged then CreateFileMonitor;
end;

procedure TAppConfigJsonProvider.CreateFileMonitor;
begin
  fFileMonitor := TQuickFileMonitor.Create;
  fFileMonitor.FileName := fFilename;
  fFileMonitor.Interval := 2000;
  fFileMonitor.Notifies := [TMonitorNotify.mnFileModified];
  fFileMonitor.OnFileChange := FileModifiedNotify;
  fFileMonitor.Enabled := True;
end;

destructor TAppConfigJsonProvider.Destroy;
begin
  if Assigned(fFileMonitor) then fFileMonitor.Free;
  inherited;
end;

procedure TAppConfigJsonProvider.DoNofifyReload;
begin
  if Assigned(fNotifyReload) then fNotifyReload
    else raise EAppConfig.Create('Not config assigned to reload!');
end;

procedure TAppConfigJsonProvider.FileModifiedNotify(MonitorNotify : TMonitorNotify);
begin
  if MonitorNotify = TMonitorNotify.mnFileModified then
  begin
    if Assigned(fOnFileModified) then fOnFileModified;
    if fReloadIfFileChanged then DoNofifyReload;
  end;
end;

procedure TAppConfigJsonProvider.Load(cConfig : TAppConfig);
var
  json : TStrings;
  serializer : TJsonSerializer;
begin
  if (not FileExists(fFilename)) and (CreateIfNotExists) then
  begin
    TAppConfig(cConfig).DefaultValues;
    Self.Save(cConfig);
  end;

  try
    json := TStringList.Create;
    try
      json.LoadFromFile(fFilename);
      serializer := TJsonSerializer.Create(slPublishedProperty,UseEnumNames);
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
    if not fLoaded then
    begin
      fLoaded := True;
      if Assigned(fOnConfigLoaded) then fOnConfigLoaded;
    end
    else if Assigned(fOnConfigReloaded) then fOnConfigReloaded;
  except
    on e : Exception do raise EAppConfig.Create(e.Message);
  end;
end;

procedure TAppConfigJsonProvider.Save(cConfig : TAppConfig);
var
  json : TStrings;
  Serializer : TJsonSerializer;
begin
  if not Assigned(cConfig) then cConfig := TAppConfigJson.Create(fFilename,fReloadIfFileChanged);

  try
    json := TStringList.Create;
    try
      serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty,UseEnumNames);
      try
        //Streamer.Options := Streamer.Options + [jsoDateTimeAsString ,jsoUseFormatString];
        //Streamer.DateTimeFormat := 'yyyy-mm-dd"T"hh:mm:ss.zz';
        json.Text := serializer.ObjectToJson(cConfig,cConfig.JsonIndent);
      finally
        serializer.Free;
      end;
      json.SaveToFile(fFilename);
    finally
      json.Free;
    end;
  except
    on e : Exception do raise EAppConfig.Create(e.Message);
  end;
end;


procedure TAppConfigJsonProvider.SetFileName(const Value: string);
begin
  fFilename := Value;
  if Assigned(fFileMonitor) then fFileMonitor.Free;
  if fReloadIfFileChanged then CreateFileMonitor;
end;

procedure TAppConfigJsonProvider.SetReloadIfFileChanged(const Value: Boolean);
begin
  if Value = fReloadIfFileChanged then Exit;
  fReloadIfFileChanged := Value;
  if Assigned(fFileMonitor) then fFileMonitor.Free;
  if fReloadIfFileChanged then CreateFileMonitor;
end;

procedure TAppConfigJsonProvider.SetReloadNotify(aNotifyReload: TLoadConfigEvent);
begin
  fNotifyReload := aNotifyReload;
end;

{ TAppConfigJson }

constructor TAppConfigJson.Create(const aFilename : string; aReloadIfFileChanged : Boolean = False);
begin
  inherited Create(TAppConfigJsonProvider.Create(aFileName,aReloadIfFileChanged));
  TAppConfigJsonProvider(fProvider).SetReloadNotify(ReloadNotify);
end;

destructor TAppConfigJson.Destroy;
begin
  inherited;
end;

function TAppConfigJson.GetProvider: TAppConfigJsonProvider;
begin
  if not Assigned(fProvider) then raise EAppConfig.Create('No provider assigned!');
  Result := TAppConfigJsonProvider(fProvider);
end;

procedure TAppConfigJson.ReloadNotify;
begin
  Self.Load;
end;

end.
