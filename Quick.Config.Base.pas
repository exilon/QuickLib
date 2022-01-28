{ ***************************************************************************

  Copyright (c) 2015-2019 Kike Pérez

  Unit        : Quick.Config.Base
  Description : Quick Config Base classes
  Author      : Kike Pérez
  Version     : 1.5
  Created     : 26/01/2017
  Modified    : 12/02/2019

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
  Quick.Json.Serializer;

type

  TAppConfig = class;

  IAppConfigProvider = interface
  ['{D55B1EBF-47F6-478B-8F70-9444575CB825}']
    procedure Load(cConfig : TAppConfig);
    procedure Save(cConfig : TAppConfig);
  end;

  TSerializeProperty = (spPublic, spPublished);

  TAppConfigProvider = class(TInterfacedObject,IAppConfigProvider)
  private
    fCreateIfNotExists : Boolean;
    fSerializeLevel : TSerializeProperty;
    fUseEnumNames : Boolean;
  protected
    procedure Load(cConfig : TAppConfig); virtual; abstract;
    procedure Save(cConfig : TAppConfig); virtual; abstract;
  public
    constructor Create;
    property CreateIfNotExists : Boolean read fCreateIfNotExists write fCreateIfNotExists;
    property SerializeLevel : TSerializeProperty read fSerializeLevel write fSerializeLevel;
    property UseEnumNames : Boolean read fUseEnumNames write fUseEnumNames;
  end;

  TApplyConfigEvent = procedure of object;

  TAppConfig = class
  private
    fOnApplyConfig : TApplyConfigEvent;
    fJsonIndent: Boolean;
    fLastSaved : TDateTime;
  protected
    fProvider : TAppConfigProvider;
  public
    constructor Create(aConfigProvider : TAppConfigProvider);
    destructor Destroy; override;
    {$IFNDEF FPC}[TNotSerializableProperty]{$ENDIF}
    property OnApplyConfig : TApplyConfigEvent read fOnApplyConfig write fOnApplyConfig;
    {$IFNDEF FPC}[TNotSerializableProperty]{$ENDIF}
    property JsonIndent : Boolean read fJsonIndent write fJsonIndent;
    {$IFNDEF FPC}[TNotSerializableProperty]{$ENDIF}
    property LastSaved : TDateTime read fLastSaved write fLastSaved;
    procedure Apply;
    //override this method to provide your class initialization
    procedure Init; virtual;
    procedure DefaultValues; virtual;
    procedure Load; virtual;
    procedure Save; virtual;
    function ToJSON : string;
    procedure FromJSON(const json : string);
  end;

  EAppConfig = class(Exception);

implementation


{ TAppConfigProviderBase }

constructor TAppConfigProvider.Create;
begin
  fCreateIfNotExists := True;
  fSerializeLevel := spPublished;
  fUseEnumNames := True;
end;

{ TAppConfig }

constructor TAppConfig.Create(aConfigProvider : TAppConfigProvider);
begin
  fProvider := aConfigProvider;
  fJsonIndent := True;
  fLastSaved := 0;
  Init;
end;

procedure TAppConfig.Apply;
begin
  if Assigned(fOnApplyConfig) then fOnApplyConfig;
end;

procedure TAppConfig.DefaultValues;
begin
  //inherit to set default values if no config exists before
end;


destructor TAppConfig.Destroy;
begin
  if Assigned(fProvider) then fProvider.Free;
  inherited;
end;

function TAppConfig.ToJSON : string;
var
  Serializer : TJsonSerializer;
begin
  Result := '';
  try
    serializer := TJsonSerializer.Create(slPublishedProperty,fProvider.UseEnumNames);
    try
      Result := serializer.ObjectToJSON(Self,fJsonIndent);
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
    serializer := TJsonSerializer.Create(slPublishedProperty,fProvider.UseEnumNames);
    try
      serializer.JsonToObject(Self,json);
    finally
      serializer.Free;
    end;
  except
    on e : Exception do raise Exception.Create(e.Message);
  end;
end;

procedure TAppConfig.Init;
begin
  //override to create private classes
end;

procedure TAppConfig.Load;
begin
  if not Assigned(fProvider) then raise EAppConfig.Create('No provider assigned!');
  fProvider.Load(Self);
end;

procedure TAppConfig.Save;
begin
  if not Assigned(fProvider) then raise EAppConfig.Create('No provider assigned!');
  fProvider.Save(Self);
  fLastSaved := Now();
end;

end.
