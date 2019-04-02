{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.RTTI.Utils
  Description : Files functions
  Author      : Kike Pérez
  Version     : 1.5
  Created     : 09/03/2018
  Modified    : 20/02/2019

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

unit Quick.RTTI.Utils;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  Rtti;

type

  TRTTI = class
  private class var
    fCtx : TRttiContext;
  public
    //class function GetProperties();
    class function GetField(aInstance : TObject; const aFieldName : string) : TRttiField; overload;
    class function GetField(aTypeInfo : Pointer; const aFieldName : string) : TRttiField; overload;
    class function FieldExists(aTypeInfo : Pointer; const aFieldName : string) : Boolean;
    class function GetFieldValue(aInstance : TObject; const aFieldName : string) : TValue; overload;
    class function GetFieldValue(aTypeInfo : Pointer; const aFieldName: string) : TValue; overload;
    class function GetProperty(aInstance : TObject; const aPropertyName : string) : TRttiProperty; overload;
    class function GetProperty(aTypeInfo : Pointer; const aPropertyName : string) : TRttiProperty; overload;
    class function PropertyExists(aTypeInfo : Pointer; const aPropertyName : string) : Boolean;
    class function GetPropertyValue(aInstance : TObject; const aPropertyName : string) : TValue; overload;
    class function GetPropertyValue(aTypeInfo : Pointer; const aPropertyName : string) : TValue; overload;
    class function FindClass(const aClassName: string): TClass;
  end;

  ERTTIError = class(Exception);

implementation

{ TRTTIUtils }

class function TRTTI.FieldExists(aTypeInfo: Pointer; const aFieldName: string): Boolean;
var
  rtype : TRttiType;
begin
  rtype := fCtx.GetType(aTypeInfo);
  Result := rtype.GetField(aFieldName) <> nil;
end;

class function TRTTI.GetField(aInstance: TObject; const aFieldName: string): TRttiField;
var
  rtype : TRttiType;
begin
  rtype := fCtx.GetType(aInstance.ClassInfo);
  if rtype <> nil then
  begin
    Result := rtype.GetField(aFieldName);
  end;
end;

class function TRTTI.GetField(aTypeInfo: Pointer; const aFieldName: string): TRttiField;
var
  rtype : TRttiType;
begin
  rtype := fCtx.GetType(aTypeInfo);
  if rtype <> nil then
  begin
    Result := rtype.GetField(aFieldName);
  end;
end;

class function TRTTI.GetFieldValue(aInstance : TObject; const aFieldName: string): TValue;
var
  rfield: TRttiField;
begin
  rfield := GetField(aInstance,aFieldName);
  if rfield <> nil then Result := rfield.GetValue(aInstance);
end;

class function TRTTI.GetFieldValue(aTypeInfo : Pointer; const aFieldName: string): TValue;
var
  rfield: TRttiField;
begin
  rfield := GetField(aTypeInfo,aFieldName);
  if rfield <> nil then rfield.GetValue(aTypeInfo);
end;

class function TRTTI.GetProperty(aInstance: TObject; const aPropertyName: string): TRttiProperty;
var
  rtype : TRttiType;
begin
  rtype := fCtx.GetType(aInstance.ClassInfo);
  if rtype <> nil then Result := rtype.GetProperty(aPropertyName);
end;

class function TRTTI.GetProperty(aTypeInfo: Pointer; const aPropertyName: string): TRttiProperty;
var
  rtype : TRttiType;
begin
  rtype := fCtx.GetType(aTypeInfo);
  if rtype <> nil then  Result := rtype.GetProperty(aPropertyName);
end;

class function TRTTI.GetPropertyValue(aInstance: TObject; const aPropertyName: string): TValue;
var
  rprop : TRttiProperty;
begin
  rprop := GetProperty(aInstance,aPropertyName);
  if rprop <> nil then Result := rprop.GetValue(aInstance);
end;

class function TRTTI.GetPropertyValue(aTypeInfo: Pointer; const aPropertyName: string): TValue;
var
  rprop : TRttiProperty;
begin
  rprop := GetProperty(aTypeInfo,aPropertyName);
  if rprop <> nil then Result := rprop.GetValue(aTypeInfo);
end;

class function TRTTI.PropertyExists(aTypeInfo: Pointer; const aPropertyName: string): Boolean;
begin
  Result := fCtx.GetType(aTypeInfo).GetProperty(aPropertyName) <> nil;
end;

class function TRTTI.FindClass(const aClassName: string): TClass;
var
  rType : TRttiType;
  rList : TArray<TRttiType>;
begin
  Result := nil;
  rList := fCtx.GetTypes;
  for rType in rList do
  begin
    if (rType.IsInstance) and (aClassName.EndsWith(rType.Name)) then
      begin
        Result := rType.AsInstance.MetaClassType;
        Break;
      end;
  end;
end;


end.
