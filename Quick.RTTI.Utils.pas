{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.RTTI.Utils
  Description : Files functions
  Author      : Kike Pérez
  Version     : 1.4
  Created     : 09/03/2018
  Modified    : 03/04/2020

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
  Quick.Commons,
  TypInfo,
  Rtti;

type

  TRttiPropertyOrder = (roFirstBase, roFirstInherited);

  TRTTI = class
  private class var
    fCtx : TRttiContext;
  public
    {$IFNDEF FPC}
    class constructor Create;
    class destructor Destroy;
    class function GetField(aInstance : TObject; const aFieldName : string) : TRttiField; overload;
    class function GetField(aTypeInfo : Pointer; const aFieldName : string) : TRttiField; overload;
    class function FieldExists(aTypeInfo : Pointer; const aFieldName : string) : Boolean;
    class function GetFieldValue(aInstance : TObject; const aFieldName : string) : TValue; overload;
    class function GetFieldValue(aTypeInfo : Pointer; const aFieldName: string) : TValue; overload;
    {$ENDIF}
    class function GetProperties(aType : TRttiType; aOrder : TRttiPropertyOrder = roFirstBase) : TArray<TRttiProperty>;
    class function GetType(aTypeInfo : Pointer) : TRttiType;
    class function GetProperty(aInstance : TObject; const aPropertyName : string) : TRttiProperty; overload;
    class function GetProperty(aTypeInfo : Pointer; const aPropertyName : string) : TRttiProperty; overload;
    class function GetPropertyPath(aInstance : TObject; const aPropertyPath : string) : TRttiProperty;
    {$IFNDEF FPC}
    class function GetMemberPath(aInstance: TObject; const aPropertyPath: string): TRttiMember;
    {$ENDIF}
    class function PathExists(aInstance: TObject; const aPropertyPath: string) : Boolean;
    class function GetPathValue(aInstance : TObject; const aPropertyPath : string) : TValue;
    class procedure SetPathValue(aInstance: TObject; const aPropertyPath: string; aValue : TValue);
    class procedure SetPropertyValue(aInstance : TObject; const aPropertyName : string; aValue : TValue);
    class function PropertyExists(aTypeInfo : Pointer; const aPropertyName : string) : Boolean;
    class function GetPropertyValue(aInstance : TObject; const aPropertyName : string) : TValue; overload;
    class function GetPropertyValue(aTypeInfo : Pointer; const aPropertyName : string) : TValue; overload;
    class function GetPropertyValueEx(aInstance: TObject; const aPropertyName: string): TValue;
    {$IFNDEF FPC}
    class function FindClass(const aClassName: string): TClass;
    class function CreateInstance<T>: T;
    {$ENDIF}
  end;

  ERTTIError = class(Exception);

  TArrayHelper<T> = class
  public
    class function Concat(const Args: array of TArray<T>): TArray<T>; static;
  end;

implementation

{ TRTTIUtils }

{$IFNDEF FPC}
class constructor TRTTI.Create;
begin
  fCtx := TRttiContext.Create;
end;

class function TRTTI.CreateInstance<T>: T;
var
  value: TValue;
  rtype: TRttiType;
  rmethod: TRttiMethod;
  rinstype: TRttiInstanceType;
begin
  rtype := fCtx.GetType(TypeInfo(T));
  for rmethod in rtype.GetMethods do
  begin
    if (rmethod.IsConstructor) and (Length(rmethod.GetParameters) = 0) then
    begin
      rinstype := rtype.AsInstance;
      value := rmethod.Invoke(rinstype.MetaclassType,[]);
      Result := value.AsType<T>;
      Exit;
    end;
  end;
end;

class destructor TRTTI.Destroy;
begin
  fCtx.Free;
end;

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
  Result := nil;
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
  Result := nil;
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
{$ENDIF}

class function TRTTI.GetProperty(aInstance: TObject; const aPropertyName: string): TRttiProperty;
var
  rtype : TRttiType;
begin
  Result := nil;
  rtype := fCtx.GetType(aInstance.ClassInfo);
  if rtype <> nil then Result := rtype.GetProperty(aPropertyName);
end;

class function TArrayHelper<T>.Concat(const Args: array of TArray<T>): TArray<T>;
var
  i, j, out, len: Integer;
begin
  len := 0;
  for i := 0 to High(Args) do
    len := len + Length(Args[i]);
  SetLength(Result, len);
  out := 0;
  for i := 0 to High(Args) do
    for j := 0 to High(Args[i]) do
    begin
      Result[out] := Args[i][j];
      Inc(out);
    end;
end;

class function TRTTI.GetProperties(aType: TRttiType; aOrder: TRttiPropertyOrder = roFirstBase): TArray<TRttiProperty>;
var
  flat: TArray<TArray<TRttiProperty>>;
  t: TRttiType;
  depth: Integer;
begin
  if aOrder = TRttiPropertyOrder.roFirstBase then
  begin
    t := aType;
    depth := 0;
    while t <> nil do
    begin
      Inc(depth);
      t := t.BaseType;
    end;

    SetLength(flat, depth);
    t := aType;
    while t <> nil do
    begin
      Dec(depth);
      {$IFNDEF FPC}
      flat[depth] := t.GetDeclaredProperties;
      {$ELSE}
      flat[depth] := t.GetProperties;
      {$ENDIF}
      t := t.BaseType;
    end;
  end
  else
  begin
    t := aType;
    depth := 0;
    while t <> nil do
    begin
      Inc(depth);
      t := t.BaseType;
    end;

    SetLength(flat, depth);
    t := aType;
    depth := 0;
    while t <> nil do
    begin
      {$IFNDEF FPC}
      flat[depth] := t.GetDeclaredProperties;
      {$ELSE}
      flat[depth] := t.GetProperties;
      {$ENDIF}
      Inc(depth);
      t := t.BaseType;
    end;
  end;

  Result := TArrayHelper<TRttiProperty>.Concat(flat);
end;

class function TRTTI.GetProperty(aTypeInfo: Pointer; const aPropertyName: string): TRttiProperty;
var
  rtype : TRttiType;
begin
  Result := nil;
  rtype := fCtx.GetType(aTypeInfo);
  if rtype <> nil then  Result := rtype.GetProperty(aPropertyName);
end;

class function TRTTI.GetPropertyPath(aInstance: TObject; const aPropertyPath: string): TRttiProperty;
var
  prop : TRttiProperty;
  proppath : string;
  propname : string;
  i : Integer;
  value : TValue;
  rtype : TRttiType;
  {$IFNDEF FPC}
  rfield : TRttiField;
  {$ENDIF}
  lastsegment : Boolean;
begin
  Result := nil;
  proppath := aPropertyPath;
  lastsegment := False;
  rtype := fCtx.GetType(aInstance.ClassType);
  repeat
    i := proppath.IndexOf('.');
    if i > -1 then
    begin
      propname := Copy(proppath,1,i);
      Delete(proppath,1,i+1);
    end
    else
    begin
      propname := proppath;
      lastsegment := True;
    end;
    if rtype.TypeKind = TTypeKind.tkRecord then
    begin
      {$IFNDEF FPC}
      rfield := rtype.GetField(propname);
      if rfield <> nil then value := rfield.GetValue(aInstance);
      {$ELSE}
      raise ERTTIError.Create('FPC not supports record fields in RTTI');
      {$ENDIF}
    end
    else
    begin
      prop := rtype.GetProperty(propname);
      if prop = nil then Exit;
      if lastsegment then Exit(prop)
        else value := prop.GetValue(aInstance);
    end;
    if not lastsegment then
    begin
      if value.Kind = TTypeKind.tkClass then rType := fCtx.GetType(value.AsObject.ClassType)
        else if value.Kind = TTypeKind.tkRecord then rtype := fCtx.GetType(value.TypeInfo);
    end;
  until lastsegment;
  Result := nil;
end;

{$IFNDEF FPC}
class function TRTTI.GetMemberPath(aInstance: TObject; const aPropertyPath: string): TRttiMember;
var
  prop : TRttiProperty;
  proppath : string;
  propname : string;
  i : Integer;
  value : TValue;
  rtype : TRttiType;
  {$IFNDEF FPC}
  rfield : TRttiField;
  {$ENDIF}
  lastsegment : Boolean;
begin
  Result := nil;
  proppath := aPropertyPath;
  lastsegment := False;
  rtype := fCtx.GetType(aInstance.ClassType);
  repeat
    i := proppath.IndexOf('.');
    if i > -1 then
    begin
      propname := Copy(proppath,1,i);
      Delete(proppath,1,i+1);
    end
    else
    begin
      propname := proppath;
      lastsegment := True;
    end;
    if rtype.TypeKind = TTypeKind.tkRecord then
    begin
      {$IFNDEF FPC}
      rfield := rtype.GetField(propname);
      if rfield <> nil then
      begin
        if lastsegment then Exit(rfield)
          else value := rfield.GetValue(value.GetReferenceToRawData);
      end;
      {$ELSE}
      raise ERTTIError.Create('FPC not supports record fields in RTTI');
      {$ENDIF}
    end
    else
    begin
      prop := rtype.GetProperty(propname);
      if prop = nil then Exit;
      if lastsegment then Exit(prop)
        else value := prop.GetValue(aInstance);
    end;
    if not lastsegment then
    begin
      if value.Kind = TTypeKind.tkClass then rType := fCtx.GetType(value.AsObject.ClassType)
        else if value.Kind = TTypeKind.tkRecord then rtype := fCtx.GetType(value.TypeInfo);
    end;
  until lastsegment;
end;
{$ENDIF}

class function TRTTI.PathExists(aInstance: TObject; const aPropertyPath: string) : Boolean;
var
  proppath : string;
  propname : string;
  i : Integer;
  value : TValue;
  rtype : TRttiType;
  rprop : TRttiProperty;
  {$IFNDEF FPC}
  rfield : TRttiField;
  {$ENDIF}
  lastsegment : Boolean;
begin
  if not Assigned(aInstance) then Exit(False);
  lastsegment := False;
  proppath := aPropertyPath;
  rtype := fCtx.GetType(aInstance.ClassType);
  repeat
    Result := False;
    i := proppath.IndexOf('.');
    if i > -1 then
    begin
      propname := Copy(proppath,1,i);
      Delete(proppath,1,i+1);
    end
    else
    begin
      propname := proppath;
      lastsegment := True;
    end;
    if rtype.TypeKind = TTypeKind.tkRecord then
    begin
      {$IFNDEF FPC}
      rfield := rtype.GetField(propname);
      if rfield = nil then Exit
      else
      begin
        value := rfield.GetValue(value.GetReferenceToRawData);
        Result := True;
      end;
      {$ELSE}
      raise ERTTIError.Create('FPC not supports record fields in RTTI');
      {$ENDIF}
    end
    else
    begin
      rprop := rtype.GetProperty(propname);
      if rprop = nil then Exit
      else
      begin
        value := rprop.GetValue(aInstance);
        Result := True;
      end;
    end;
    if not lastsegment then
    begin
      if value.Kind = TTypeKind.tkClass then rType := fCtx.GetType(value.AsObject.ClassType)
        else if value.Kind = TTypeKind.tkRecord then rtype := fCtx.GetType(value.TypeInfo);
    end;
  until lastsegment;
end;

class function TRTTI.GetPathValue(aInstance: TObject; const aPropertyPath: string): TValue;
var
  proppath : string;
  propname : string;
  i : Integer;
  value : TValue;
  rtype : TRttiType;
  rprop : TRttiProperty;
  {$IFNDEF FPC}
  rfield : TRttiField;
  {$ENDIF}
  lastsegment : Boolean;
begin
  Result := nil;
  if not Assigned(aInstance) then Exit;

  lastsegment := False;
  proppath := aPropertyPath;
  rtype := fCtx.GetType(aInstance.ClassType);
  {$IFDEF FPC}
  value := aInstance;
  {$ENDIF}
  repeat
    i := proppath.IndexOf('.');
    if i > -1 then
    begin
      propname := Copy(proppath,1,i);
      Delete(proppath,1,i+1);
    end
    else
    begin
      propname := proppath;
      lastsegment := True;
    end;
    if rtype.TypeKind = TTypeKind.tkRecord then
    begin
      {$IFNDEF FPC}
      rfield := rtype.GetField(propname);
      if rfield = nil then raise ERTTIError.CreateFmt('Field "%s" not found in record',[propname])
        else value := rfield.GetValue(value.GetReferenceToRawData);
      {$ELSE}
      raise ERTTIError.Create('FPC not supports record fields in RTTI');
      {$ENDIF}
    end
    else
    begin
      rprop := rtype.GetProperty(propname);
      if rprop = nil then raise ERTTIError.CreateFmt('Property "%s" not found in object',[propname])
      {$IFNDEF FPC}
      else value := rprop.GetValue(aInstance);
      {$ELSE}
      else
      begin
        if rprop.PropertyType.IsInstance then value := GetObjectProp(value.AsObject,propname)
           else value := rprop.GetValue(value.AsObject);
      end;
      {$ENDIF}
    end;
    if not lastsegment then
    begin
      if value.Kind = TTypeKind.tkClass then rType := fCtx.GetType(value.AsObject.ClassType)
        else if value.Kind = TTypeKind.tkRecord then rtype := fCtx.GetType(value.TypeInfo);
    end;
  until lastsegment;
  Result := value;
end;

class procedure TRTTI.SetPathValue(aInstance: TObject; const aPropertyPath: string; aValue : TValue);
var
  proppath : string;
  propname : string;
  i : Integer;
  value : TValue;
  rtype : TRttiType;
  rprop : TRttiProperty;
  {$IFNDEF FPC}
  rfield : TRttiField;
  {$ENDIF}
  lastsegment : Boolean;
begin
  if not Assigned(aInstance) then Exit;
  lastsegment := False;
  proppath := aPropertyPath;
  rtype := fCtx.GetType(aInstance.ClassType);
  repeat
    i := proppath.IndexOf('.');
    if i > -1 then
    begin
      propname := Copy(proppath,1,i);
      Delete(proppath,1,i+1);
    end
    else
    begin
      propname := proppath;
      lastsegment := True;
    end;
    if rtype.TypeKind = TTypeKind.tkRecord then
    begin
      {$IFNDEF FPC}
      rfield := rtype.GetField(propname);
      if rfield = nil then raise ERTTIError.CreateFmt('Field "%s" not found in record',[propname])
      else
      begin
        if lastsegment then rfield.SetValue(value.GetReferenceToRawData,aValue)
          else value := rfield.GetValue(value.GetReferenceToRawData);
      end;
      {$ELSE}
      raise ERTTIError.Create('FPC not supports record fields in RTTI');
      {$ENDIF}
    end
    else
    begin
      rprop := rtype.GetProperty(propname);
      if rprop = nil then raise ERTTIError.CreateFmt('Property "%s" not found in object',[propname])
      else
      begin
        if lastsegment then rprop.SetValue(aInstance,aValue)
          else value := rprop.GetValue(aInstance);
      end;
    end;
    if not lastsegment then
    begin
      if value.Kind = TTypeKind.tkClass then rType := fCtx.GetType(value.AsObject.ClassType)
        else if value.Kind = TTypeKind.tkRecord then rtype := fCtx.GetType(value.TypeInfo);
    end;
  until lastsegment;
end;

class function TRTTI.GetPropertyValue(aInstance: TObject; const aPropertyName: string): TValue;
var
  rprop : TRttiProperty;
begin
  rprop := GetProperty(aInstance,aPropertyName);
  if rprop <> nil then
  begin
    {$IFNDEF FPC}
    Result := rprop.GetValue(aInstance);
    {$ELSE}
    if rprop.PropertyType.IsInstance then Result := GetObjectProp(aInstance,aPropertyName)
      else Result := rprop.GetValue(aInstance);
    {$ENDIF}
  end;
end;

class function TRTTI.GetPropertyValue(aTypeInfo: Pointer; const aPropertyName: string): TValue;
var
  rprop : TRttiProperty;
begin
  rprop := GetProperty(aTypeInfo,aPropertyName);
  if rprop <> nil then
  begin
    {$IFNDEF FPC}
    Result := rprop.GetValue(aTypeInfo);
    {$ELSE}
    if rprop.PropertyType.IsInstance then Result := GetObjectProp(aTypeInfo,aPropertyName)
      else Result := rprop.GetValue(aTypeInfo);
    {$ENDIF}
  end;
end;

class function TRTTI.GetPropertyValueEx(aInstance: TObject; const aPropertyName: string): TValue;
var
  pinfo : PPropInfo;
begin
  Result := nil;
  pinfo := GetPropInfo(aInstance,aPropertyName);
  if pinfo = nil then
  begin
    //if not found can be a public property
    Result := GetPropertyValue(aInstance,aPropertyName);
    Exit;
  end;
  case pinfo.PropType^.Kind of
    tkInteger : Result := GetOrdProp(aInstance,pinfo);
    tkInt64 : Result := GetInt64Prop(aInstance,aPropertyName);
    tkFloat : Result := GetFloatProp(aInstance,aPropertyName);
    tkChar : Result := Char(GetOrdProp(aInstance,aPropertyName));
    {$IFDEF FPC}
    tkWString : Result := GetWideStrProp(aInstance,aPropertyName);
    tkSString,
    tkAString,
    {$ELSE}
    tkUString,
    tkWString,
    {$ENDIF}
    tkLString : Result := GetStrProp(aInstance,pinfo);
    {$IFDEF FPC}
    tkEnumeration :Result  := GetOrdProp(aInstance,aPropertyName);
    {$ELSE}
    tkEnumeration : Result := GetOrdProp(aInstance,aPropertyName);
    {$ENDIF}
    tkSet : Result := GetSetProp(aInstance,pinfo,True);
    {$IFNDEF FPC}
    tkClass :
    {$ELSE}
    tkBool : Result := Boolean(GetOrdProp(aInstance,pinfo));
    tkObject :
    {$ENDIF} Result := GetObjectProp(aInstance,pinfo);
    tkDynArray : Result := GetDynArrayProp(aInstance,pinfo);
  end;
end;


class function TRTTI.GetType(aTypeInfo: Pointer): TRttiType;
begin
  Result := fCtx.GetType(aTypeInfo);
end;

class function TRTTI.PropertyExists(aTypeInfo: Pointer; const aPropertyName: string) : Boolean;
var
  rtype : TRttiType;
begin
  Result := False;
  rtype := fCtx.GetType(aTypeInfo);
  if rtype <> nil then Result := rtype.GetProperty(aPropertyName) <> nil;
end;

class procedure TRTTI.SetPropertyValue(aInstance: TObject; const aPropertyName: string; aValue: TValue);
var
  rprop : TRttiProperty;
begin
  rprop := GetProperty(aInstance,aPropertyName);
  if rprop <> nil then rprop.SetValue(aInstance,aValue);
end;

{$IFNDEF FPC}
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
{$ENDIF}


end.
