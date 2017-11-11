{ ***************************************************************************

  Copyright (c) 2015-2017 Kike Pérez

  Unit        : Quick.Config.Provider.Registry
  Description : Save config to Windows Registry
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 21/10/2017
  Modified    : 11/11/2017

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
unit Quick.Config.Provider.Registry;

interface

uses
  Classes,
  Windows,
  System.SysUtils,
  System.Win.Registry,
  {$IF CompilerVersion >= 32.0}
    System.Json,
    System.JSON.Types,
    System.JSON.Serializers,
  {$ELSE}
    Rest.Json.Types,
    System.JSON,
    Rest.Json,
  {$ENDIF}
  Quick.Commons,
  Quick.Config;

type

  TAppConfigRegistryProvider<T : class> = class(TAppConfigProviderBase<T>)
  private
    fRootKey : HKEY;
    fMainKey : string;
    fRegConfig : TRegistry;
    function JsonToRegistry(const StrJson : string) : Boolean;
    function RegistryToJson(out StrJson : string) : Boolean;
    class function IsSimpleJsonValue(v: TJSONValue): Boolean;
    function IsRegKeyObject(const cCurrentKey : string = '') : Boolean;
    function IsRegKeyArray(const cCurrentKey : string = '') : Boolean;
    function ProcessPairRead(const cCurrentKey, cRegKey : string; aIndex : Integer) : TJSONValue;
    function ProcessElementRead(const cCurrentKey, cRegKey : string; aIndex : Integer) : TJSONValue;
    procedure ProcessPairWrite(const cCurrentKey: string; obj: TJSONObject; aIndex: integer);
    procedure ProcessElementWrite(const cCurrentKey: string; arr: TJSONArray; aIndex, aMax : integer);
    function AddRegKey(const cCurrentKey, NewKey : string) : Boolean;
    function ReadRegValue(const cCurrentKey, cName : string) : TJSONValue;
    procedure AddRegValue(const cCurrentKey, cName : string; cValue : TJSONValue);
  public
    constructor Create(var cConfig : T); override;
    destructor Destroy; override;
    property HRoot : HKEY read fRootKey write fRootKey;
    property MainKey : string read fMainKey write fMainKey;
    procedure Load(var cConfig : T); override;
    procedure Save(var cConfig : T); override;
  end;

  EAppConfig = Exception;


implementation


{ TAppConfigRegistryProvider }

constructor TAppConfigRegistryProvider<T>.Create(var cConfig : T);
begin
  inherited Create(cConfig);
  fRootKey := HKEY_CURRENT_USER;
  fMainKey := '_AppConfig';
  fRegConfig := TRegistry.Create(KEY_READ or KEY_WRITE);
end;

destructor TAppConfigRegistryProvider<T>.Destroy;
begin
  if Assigned(fRegConfig) then fRegConfig.Free;
  inherited;
end;

procedure TAppConfigRegistryProvider<T>.Load(var cConfig : T);
var
  {$IF CompilerVersion >= 32.0}
    Serializer: TJsonSerializer;
  {$ENDIF}
  json : string;
  newObj : T;
begin
  fRegConfig.Access := KEY_READ;
  fRegConfig.RootKey := fRootKey;
  if (not fRegConfig.KeyExists('\Software\' + fMainKey))
      and (CreateIfNotExists) then
  begin
    Save(cConfig);
  end;
  RegistryToJson(json);
  {$IF CompilerVersion >= 32.0}
    Serializer := TJsonSerializer.Create;
    try
      Serializer.Formatting := TJsonFormatting.Indented;
      if TAppConfig(cConfig).DateTimeZone = TDateTimeZone.tzLocal then
      begin
        Serializer.DateTimeZoneHandling := TJsonDateTimeZoneHandling.Local;
        Serializer.DateFormatHandling := TJsonDateFormatHandling.FormatSettings;
      end
      else Serializer.DateTimeZoneHandling := TJsonDateTimeZoneHandling.Utc;
      newObj := Serializer.Deserialize<T>(json);
    finally
      Serializer.Free;
    end;
  {$ELSE}
    TJson.JsonToObject(cConfig,TJSONObject(TJSONObject.ParseJSONValue(json)));
  {$ENDIF}
  if Assigned(cConfig) then cConfig.Free;
  cConfig := newObj;
end;

procedure TAppConfigRegistryProvider<T>.Save(var cConfig : T);
begin
  //create object with rtti if nil
  if not Assigned(cConfig) then cConfig := InitObject;
  JsonToRegistry(TAppConfig(cConfig).ToJSON);
end;

function TAppConfigRegistryProvider<T>.JsonToRegistry(const StrJson : string) : Boolean;
var
  jValue  : TJSONValue;
  aCount : Integer;
  i : Integer;
  aCurrentKey : string;
begin
  Result := False;
  if fMainKey = '' then raise EAppConfig.Create('MainKey not defined!');

  fRegConfig.Access := KEY_READ or KEY_WRITE;
  fRegConfig.RootKey := fRootKey;
  aCurrentKey := '\Software\' + fMainKey;

  if fRegConfig.KeyExists(aCurrentKey) then
  begin
    try
     if fRegConfig.KeyExists(aCurrentKey + '_bak') then fRegConfig.DeleteKey(aCurrentKey + '_bak');
     fRegConfig.MoveKey(aCurrentKey,aCurrentKey + '_bak',True);
    except
      raise EAppConfig.Create('Can''t write Config Registry');
    end;
  end;
  try
    if not AddRegKey('\Software',fMainKey) then
    begin
      raise EAppConfig.Create('Can''t create key');
    end;

    jValue := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StrJson),0) as TJSONValue;
    try
      if IsSimpleJsonValue(jValue) then
      begin
        AddRegValue(aCurrentKey,TJSONPair(jValue).JsonString.ToString.DeQuotedString('"'),TJSONPair(jValue).JsonValue);
      end
      else if jValue is TJSONObject then
      begin
        aCount := TJSONObject(jValue).Count;
        for i := 0 to aCount - 1 do
          ProcessPairWrite(aCurrentKey,TJSONObject(jValue),i);
      end
      else if jValue is TJSONArray then
      begin
        aCount := TJSONArray(jValue).Count;
        for i := 0 to aCount - 1 do
          ProcessElementWrite(aCurrentKey,TJSONArray(jValue),i,aCount);
      end
      else
        raise EAppConfig.Create('Error Saving config to Registry');
      Result := True;
    finally
       jValue.Free;
    end;
    if fRegConfig.KeyExists(aCurrentKey + '_bak') then fRegConfig.DeleteKey(aCurrentKey + '_bak');
  except
    fRegConfig.DeleteKey(aCurrentKey);
    fRegConfig.MoveKey(aCurrentKey+'_bak',aCurrentKey,True);
  end;
end;

function TAppConfigRegistryProvider<T>.RegistryToJson(out StrJson : string) : Boolean;
var
  jValue  : TJSONValue;
  jPair : TJSONPair;
  jArray : TJSONArray;
  a, b : string;
  aCount : Integer;
  i : Integer;
  aName : string;
  aValue : TJSONValue;
  aCurrentKey : string;
  newObj : TJSONObject;
  RegKeyList : TStringList;
  RegValueList : TStringList;
  RegKey : string;
  RegValue : string;
  RegKeyInfo : TRegKeyInfo;
begin
  Result := False;
  //check if exists root key
  fRegConfig.Access := KEY_READ;
  fRegConfig.RootKey := fRootKey;
  if fRegConfig.KeyExists('\Software\' + fMainKey) then
  begin
    fRegConfig.OpenKeyReadOnly('\Software\' + fMainKey);
    aCurrentKey := '\Software\' + fMainKey;
  end
  else raise EAppConfig.Create('Can''t read key');

  newObj := TJSONObject.Create;
  try
    //read root values
    RegValueList := TStringList.Create;
    try
      fRegConfig.GetValueNames(RegValueList);
      for RegValue in RegValueList do
      begin
        newObj.AddPair(RegValue,ReadRegValue(aCurrentKey,RegValue));
      end;
    finally
      RegValueList.Free;
    end;
    //read root keys
    RegKeyList := TStringList.Create;
    try
      fRegConfig.GetKeyNames(RegKeyList);
      for RegKey in RegKeyList do
      begin
        fRegConfig.OpenKeyReadOnly(aCurrentKey + '\' + RegKey);
        if IsRegKeyObject then
        begin
          jValue := ProcessPairRead(aCurrentKey + '\' + RegKey,Regkey,i);
          newObj.AddPair(RegKey,jValue);
        end
        else if IsRegKeyArray then
        begin
          jValue := ProcessElementRead(aCurrentKey + '\' + RegKey,Regkey,i);
          newObj.AddPair(RegKey,jValue);
        end
        else raise EAppConfig.Create('Unknow value reading Config Registry');
      end;
    finally
      RegKeyList.Free;
    end;
    StrJson := newObj.ToJSON;
  finally
    newObj.Free;
  end;
end;

function TAppConfigRegistryProvider<T>.IsRegKeyObject(const cCurrentKey : string = '') : Boolean;
begin
  Result := not IsRegKeyArray(cCurrentKey);
end;

function TAppConfigRegistryProvider<T>.IsRegKeyArray(const cCurrentKey : string = '') : Boolean;
var
  RegValue : string;
  RegValueList : TStrings;
  RegKey : string;
  RegKeyList : TStrings;
  n : Integer;
begin
  Result := False;
  if cCurrentKey <> '' then fRegConfig.OpenKeyReadOnly(cCurrentKey);
  //check if exists RegKey numeric (indicates is a Array)
  RegKeyList := TStringList.Create;
  try
    fRegConfig.GetKeyNames(RegKeyList);
    for RegKey in RegKeyList do
      if TryStrToInt(RegKey,n) then
      begin
        Result := True;
        Break;
      end;
  finally
    RegKeyList.Free;
  end;
  //check if exists RegValue numeric (indicates is a Array)
  RegValueList := TStringList.Create;
  try
    fRegConfig.GetValueNames(RegValueList);
    for RegValue in RegValueList do
      if TryStrToInt(RegValue,n) then
      begin
        Result := True;
        Break;
      end;
  finally
    RegValueList.Free;
  end;
end;

class function TAppConfigRegistryProvider<T>.IsSimpleJsonValue(v: TJSONValue): Boolean;
begin
  Result := (v is TJSONNumber)
    or (v is TJSONString)
    or (v is TJSONTrue)
    or (v is TJSONFalse)
    or (v is TJSONNull);
end;

function TAppConfigRegistryProvider<T>.ReadRegValue(const cCurrentKey, cName : string) : TJSONValue;
var
  aValue : string;
  RegInfo : TRegDataInfo;
begin
  if fRegConfig.OpenKeyReadOnly(cCurrentKey) then
  begin
    if fRegConfig.GetDataInfo(cName,RegInfo) then
    case RegInfo.RegData of
      rdInteger : Result := TJSONNumber.Create(fRegConfig.ReadInteger(cName));
      rdString :
        begin
          aValue := fRegConfig.ReadString(cName);
          if aValue.ToLower = 'true' then Result := TJSONBool.Create(True)
           else if aValue.ToLower = 'false' then Result := TJSONBool.Create(False)
            else Result := TJSONString.Create(aValue);
        end;
      else Result := TJSONNull.Create;
    end;
  end;
end;

function TAppConfigRegistryProvider<T>.AddRegKey(const cCurrentKey, NewKey : string) : Boolean;
begin
  Result := fRegConfig.CreateKey(Format('%s\%s',[cCurrentKey,NewKey]));
end;

procedure TAppConfigRegistryProvider<T>.AddRegValue(const cCurrentKey, cName : string; cValue : TJSONValue);
var
  aName : string;
  aValue : string;
begin
  aName := cName.DeQuotedString('"');
  aValue := cValue.ToString.DeQuotedString('"');
  fRegConfig.OpenKey(cCurrentKey,True);
  if cValue is TJSONNumber then  fRegConfig.WriteInteger(aName,StrToInt64(aValue))
   else if cValue is TJSONString then fRegConfig.WriteString(aName,aValue)
    else if cValue is TJSONBool then fRegConfig.WriteString(aName,aValue);
      //else if cValue is TJSONNull then fRegConfig.WriteString(aName,'');
end;

function TAppConfigRegistryProvider<T>.ProcessPairRead(const cCurrentKey, cRegKey : string; aIndex : Integer) : TJSONValue;
var
  i : Integer;
  jValue : TJSONValue;
  RegValue : string;
  RegValueList : TStrings;
  RegKey : string;
  RegKeyList : TStrings;
  newObj : TJSONObject;
begin
  newObj := TJSONObject.Create;
  //read root values
  RegValueList := TStringList.Create;
  try
    fRegConfig.GetValueNames(RegValueList);
    for RegValue in RegValueList do
    begin
      newObj.AddPair(RegValue,ReadRegValue(cCurrentKey,RegValue));
    end;
  finally
    RegValueList.Free;
  end;
  //read root keys
  RegKeyList := TStringList.Create;
  try
    fRegConfig.GetKeyNames(RegKeyList);
    for RegKey in RegKeyList do
    begin
      fRegConfig.OpenKeyReadOnly(cCurrentKey + '\' + RegKey);
      if IsRegKeyObject then
      begin
        jValue := ProcessPairRead(cCurrentKey + '\' + RegKey,Regkey,i);
        newObj.AddPair(RegKey,jValue);
      end
      else if IsRegKeyArray then
      begin
        jValue := ProcessElementRead(cCurrentKey + '\' + RegKey,Regkey,i);
        newObj.AddPair(RegKey,jValue);
      end
      else raise EAppConfig.Create('Unknow value reading Config Registry');
    end;
  finally
    RegKeyList.Free;
  end;
  Result := TJsonValue(newObj);
end;

function TAppConfigRegistryProvider<T>.ProcessElementRead(const cCurrentKey, cRegKey : string; aIndex : Integer) : TJSONValue;
var
  i : Integer;
  jValue : TJSONValue;
  RegValue : string;
  RegValueList : TStrings;
  RegKey : string;
  RegKeyList : TStrings;
  newObj : TJSONArray;
begin
  newObj := TJSONArray.Create;
  //read root values
  RegValueList := TStringList.Create;
  try
    fRegConfig.GetValueNames(RegValueList);
    for RegValue in RegValueList do
    begin
      newObj.AddElement(ReadRegValue(cCurrentKey,RegValue));
    end;
  finally
    RegValueList.Free;
  end;
  //read root keys
  RegKeyList := TStringList.Create;
  try
    fRegConfig.GetKeyNames(RegKeyList);
    for RegKey in RegKeyList do
    begin
      fRegConfig.OpenKeyReadOnly(cCurrentKey + '\' + RegKey);
      if IsRegKeyObject then
      begin
        jValue := ProcessPairRead(cCurrentKey + '\' + RegKey,Regkey,i);
        newObj.AddElement(jValue);
      end
      else if IsRegKeyArray then
      begin
        jValue := ProcessElementRead(cCurrentKey + '\' + RegKey,Regkey,i);
        newObj.AddElement(jValue);
      end
      else raise EAppConfig.Create('Unknow value reading Config Registry');
    end;
  finally
    RegKeyList.Free;
  end;
  Result := TJsonValue(newObj);
end;

procedure TAppConfigRegistryProvider<T>.ProcessPairWrite(const cCurrentKey: string; obj: TJSONObject; aIndex: integer);
var
  jPair: TJSONPair;
  i : Integer;
  aCount: integer;
begin
  jPair := obj.Pairs[aIndex];

  if IsSimpleJsonValue(jPair.JsonValue) then
  begin
    AddRegValue(cCurrentKey,jPair.JsonString.ToString,jPair.JsonValue);
    Exit;
  end;

  if jPair.JsonValue is TJSONObject then
  begin
    aCount := TJSONObject(jPair.JsonValue).Count;
    for i := 0 to aCount - 1 do
      ProcessPairWrite(cCurrentKey + '\' + jPair.JsonString.ToString.DeQuotedString('"'), TJSONObject(jPair.JsonValue),i);
  end
  else if jPair.JsonValue is TJSONArray then
  begin
    aCount := TJSONArray(jPair.JsonValue).Count;
    for i := 0 to aCount - 1 do
      ProcessElementWrite(cCurrentKey + '\' + jPair.JsonString.ToString.DeQuotedString('"'), TJSONArray(jPair.JsonValue),i,aCount);
  end
  else raise EAppConfig.Create('Error Saving config to Registry');
end;

procedure TAppConfigRegistryProvider<T>.ProcessElementWrite(const cCurrentKey: string; arr: TJSONArray; aIndex, aMax: integer);
var
  jValue: TJSONValue;
  i : Integer;
  aCount: integer;
  dig : Integer;
begin
  jValue := arr.Items[aIndex];
  dig := CountDigits(aMax);

  if IsSimpleJsonValue(jValue) then
  begin
    AddRegValue(cCurrentKey,Zeroes(aIndex,dig),jValue);
    Exit;
  end;

  if jValue is TJSONObject then
  begin
    aCount := TJSONObject(jValue).Count;
    for i := 0 to aCount - 1 do
      ProcessPairWrite(cCurrentKey + '\' + Zeroes(aIndex,dig),TJSONObject(jValue),i);
  end
  else if jValue is TJSONArray then
  begin
    aCount := TJSONArray(jValue).Count;
    for i := 0 to aCount - 1 do
      ProcessElementWrite(cCurrentKey + '\' + Zeroes(i,dig),TJSONArray(jValue),i,aCount);
  end
  else raise EAppConfig.Create('Error Saving config to Registry');
end;

end.

