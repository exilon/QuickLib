{ ***************************************************************************

  Copyright (c) 2015-2019 Kike Pérez

  Unit        : Quick.Config.YAML
  Description : Save config to YAML file
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 12/04/2019
  Modified    : 27/04/2019

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

unit Quick.Config.YAML;

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
  Quick.YAML.Serializer,
  Quick.FileMonitor,
  Quick.Config.Base;

type

  TFileModifiedEvent = procedure of object;
  TLoadConfigEvent = procedure of object;

  {$IFNDEF FPC}
  TNotSerializableProperty = Quick.YAML.Serializer.TNotSerializableProperty;
  TCommentProperty = Quick.YAML.Serializer.TCommentProperty;
  TCustomNameProperty = Quick.YAML.Serializer.TCustomNameProperty;
  {$ENDIF}

  TAppConfigYMALProvider = class(TAppConfigProvider)
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
    constructor Create(const aFilename : string; aReloadIfFileChanged : Boolean = False); overload;
    destructor Destroy; override;
    property Filename : string read fFilename write SetFileName;
    property ReloadIfFileChanged : Boolean read fReloadIfFileChanged write SetReloadIfFileChanged;
    property IsLoaded : Boolean read fLoaded;
    property OnFileModified : TFileModifiedEvent read fOnFileModified write fOnFileModified;
    property OnConfigLoaded : TLoadConfigEvent read fOnConfigLoaded write fOnConfigLoaded;
    property OnConfigReloaded : TLoadConfigEvent read fOnConfigReloaded write fOnConfigReloaded;
  end;

  TAppConfigYAML = class(TAppConfig)
  private
    function GetProvider : TAppConfigYMALProvider;
    procedure ReloadNotify;
  public
    constructor Create(const aFilename : string; aReloadIfFileChanged : Boolean = False);
    destructor Destroy; override;
    property Provider : TAppConfigYMALProvider read GetProvider;
    function ToYAML : string;
    procedure FromYAML(const yaml : string);
  end;

  {Usage: create a descend class from TAppConfigYAML and add published properties to be loaded/saved

  TMyConfig = class(TAppConfigYAML)
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
  MyConfig.Provider.FileName := '.\MyAppName.yml';
  MyConfig.Name := 'John';
  MyConfig.Save;
  }


implementation

constructor TAppConfigYMALProvider.Create(const aFilename : string; aReloadIfFileChanged : Boolean = False);
begin
  inherited Create;
  if aFilename = '' then fFilename := TPath.ChangeExtension(ParamStr(0),'yml')
    else fFilename := aFilename;
  fLoaded := False;
  fReloadIfFileChanged := aReloadIfFileChanged;
  if aReloadIfFileChanged then CreateFileMonitor;
end;

procedure TAppConfigYMALProvider.CreateFileMonitor;
begin
  fFileMonitor := TQuickFileMonitor.Create;
  fFileMonitor.FileName := fFilename;
  fFileMonitor.Interval := 2000;
  fFileMonitor.Notifies := [TMonitorNotify.mnFileModified];
  fFileMonitor.OnFileChange := FileModifiedNotify;
  fFileMonitor.Enabled := True;
end;

destructor TAppConfigYMALProvider.Destroy;
begin
  if Assigned(fFileMonitor) then fFileMonitor.Free;
  inherited;
end;

procedure TAppConfigYMALProvider.DoNofifyReload;
begin
  if Assigned(fNotifyReload) then fNotifyReload
    else raise EAppConfig.Create('Not config assigned to reload!');
end;

procedure TAppConfigYMALProvider.FileModifiedNotify(MonitorNotify : TMonitorNotify);
begin
  if MonitorNotify = TMonitorNotify.mnFileModified then
  begin
    if Assigned(fOnFileModified) then fOnFileModified;
    if fReloadIfFileChanged then DoNofifyReload;
  end;
end;

procedure TAppConfigYMALProvider.Load(cConfig : TAppConfig);
var
  yaml : TStrings;
  serializer : TYamlSerializer;
begin
  if (not FileExists(fFilename)) and (CreateIfNotExists) then
  begin
    TAppConfig(cConfig).DefaultValues;
    Self.Save(cConfig);
  end;

  try
    yaml := TStringList.Create;
    try
      yaml.LoadFromFile(fFilename);
      if yaml.Count > 0 then
      begin
        serializer := TYamlSerializer.Create(slPublishedProperty,UseEnumNames);
        try
          //Streamer.Options := Streamer.Options + [jsoDateTimeAsString ,jsoUseFormatString];
          //Streamer.DateTimeFormat := 'yyyy-mm-dd"T"hh:mm:ss.zz';
          serializer.YamlToObject(cConfig,yaml.Text);
        finally
          serializer.Free;
        end;
      end else
        TAppConfig(cConfig).DefaultValues;
    finally
      yaml.Free;
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

procedure TAppConfigYMALProvider.Save(cConfig : TAppConfig);
var
  yaml : TStrings;
  Serializer : TYamlSerializer;
begin
  if not Assigned(cConfig) then cConfig := TAppConfigYAML.Create(fFilename,fReloadIfFileChanged);

  try
    yaml := TStringList.Create;
    try
      serializer := TYamlSerializer.Create(TSerializeLevel.slPublishedProperty,UseEnumNames);
      try
        //Streamer.Options := Streamer.Options + [jsoDateTimeAsString ,jsoUseFormatString];
        //Streamer.DateTimeFormat := 'yyyy-mm-dd"T"hh:mm:ss.zz';
        yaml.Text := serializer.ObjectToYaml(cConfig);
      finally
        serializer.Free;
      end;
      yaml.SaveToFile(fFilename);
    finally
      yaml.Free;
    end;
  except
    on e : Exception do raise EAppConfig.Create(e.Message);
  end;
end;


procedure TAppConfigYMALProvider.SetFileName(const Value: string);
begin
  fFilename := Value;
  if Assigned(fFileMonitor) then fFileMonitor.Free;
  if fReloadIfFileChanged then CreateFileMonitor;
end;

procedure TAppConfigYMALProvider.SetReloadIfFileChanged(const Value: Boolean);
begin
  if Value = fReloadIfFileChanged then Exit;
  fReloadIfFileChanged := Value;
  if Assigned(fFileMonitor) then fFileMonitor.Free;
  if fReloadIfFileChanged then CreateFileMonitor;
end;

procedure TAppConfigYMALProvider.SetReloadNotify(aNotifyReload: TLoadConfigEvent);
begin
  fNotifyReload := aNotifyReload;
end;

{ TAppConfigYAML }

constructor TAppConfigYAML.Create(const aFilename : string; aReloadIfFileChanged : Boolean = False);
begin
  inherited Create(TAppConfigYMALProvider.Create(aFileName,aReloadIfFileChanged));
  TAppConfigYMALProvider(fProvider).SetReloadNotify(ReloadNotify);
end;

destructor TAppConfigYAML.Destroy;
begin
  inherited;
end;

function TAppConfigYAML.GetProvider: TAppConfigYMALProvider;
begin
  if not Assigned(fProvider) then raise EAppConfig.Create('No provider assigned!');
  Result := TAppConfigYMALProvider(fProvider);
end;

procedure TAppConfigYAML.ReloadNotify;
begin
  Self.Load;
end;

function TAppConfigYAML.ToYAML: string;
var
  serializer : TYamlSerializer;
begin
  serializer := TYamlSerializer.Create(slPublishedProperty,fProvider.UseEnumNames);
  try
    Result := serializer.ObjectToYaml(Self);
  finally
    serializer.Free;
  end;
end;

procedure TAppConfigYAML.FromYAML(const yaml: string);
var
  serializer : TYamlSerializer;
begin
  serializer := TYamlSerializer.Create(slPublishedProperty,fProvider.UseEnumNames);
  try
    serializer.YamlToObject(Self,yaml);
  finally
    serializer.Free;
  end;
end;

end.
