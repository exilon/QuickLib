{ ***************************************************************************

  Copyright (c) 2015-2021 Kike Pérez

  Unit        : Quick.AutoMapper
  Description : Auto Mapper object properties
  Author      : Kike Pérez
  Version     : 1.5
  Created     : 25/08/2018
  Modified    : 27/02/2026

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

unit Quick.AutoMapper;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  Generics.Collections,
  typinfo,
  Quick.Value,
  {$IFDEF FPC}
  Variants,
  {$ENDIF}
  RTTI,
  Quick.RTTI.Utils;

  //enable use of property paths (like namespaces) in custom mapping
  {$DEFINE PROPERTYPATH_MODE}

type

  {$IFNDEF FPC}
  TFlexValue = TValue;
  {$ELSE}
  TFlexValue = variant;
  {$ENDIF}

  {$IFNDEF FPC}
  TMappingProc<TClass1> = reference to procedure(const aSrcObj : TClass1; const aTargetName : string; out Value : TFlexValue);
  TAfterMappingProc<TClass1,TClass2> = reference to procedure(const aSrcObj : TClass1; aTgtObj : TClass2);
  {$ELSE}
  TMappingProc<TObject> = procedure(const aSrcObj : TObject; const aTargetName : string; out Value : TFlexValue) of object;
  TAfterMappingProc<TClass1,TClass2> = procedure(const aSrcObj : TClass1; aTgtObj : TClass2) of object;
  {$ENDIF}

  TCustomMapping = class
  private
    fMapDictionary : TDictionary<string,string>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMap(const aName, aMapName : string);
    function GetMap(const aName : string; out vMapName : string) : Boolean;
    function Count : Integer;
  end;

  TObjMapper = class
  public
    class procedure Map(aSrcObj : TObject; aTgtObj : TObject; aCustomMapping: TCustomMapping = nil); overload;
    {$IFNDEF FPC}
    class procedure Map<Tm>(aSrcObj : TObject; aTgtObj : TObject; aDoMappingProc : TMappingProc<Tm>; aCustomMapping: TCustomMapping = nil); overload;
    {$ELSE}
    class procedure Map(aSrcObj : TObject; aTgtObj : TObject; aDoMappingProc : TMappingProc<TObject>; aCustomMapping: TCustomMapping = nil); overload;
    {$ENDIF}
  end;

  TListMapper = class
  public
    class procedure Map(aSrcList, aTgtList: TObject; aCustomMapping: TCustomMapping);
  end;

  TObjListMapper = class
  public
    class procedure Map(aSrcObjList : TObject; aTgtObjList : TObject; aCustomMapping : TCustomMapping = nil); overload;
    {$IFNDEF FPC}
    class procedure Map<Tm>(aSrcObjList : TObject; aTgtObjList : TObject; aDoMappingProc : TMappingProc<Tm>; aCustomMapping : TCustomMapping = nil); overload;
    {$ELSE}
    class procedure Map(aSrcObjList : TObject; aTgtObjList : TObject; aDoMappingProc : TMappingProc<TObject>; aCustomMapping : TCustomMapping = nil); overload;
    {$ENDIF}
  end;

  {$IFNDEF FPC}
  TMapper = class
  public
    class function Map<T : class, constructor>(aSrcObj : TObject) : T;
  end;
  {$ENDIF}

  TMapper<T : class, constructor> = class
  public
    class function Map(aSrcObj : TObject; aCustomMapping: TCustomMapping = nil): T; overload;
    class procedure Map(aSrcObj : TObject; aTgtObj : T; aCustomMapping : TCustomMapping = nil); overload;
    {$IFNDEF FPC}
    class function Map<Tm>(aSrcObj : TObject; aDoMappingProc : TMappingProc<Tm>; aCustomMapping: TCustomMapping = nil): T; overload;
    class procedure Map<Tm>(aSrcObj : TObject; aTgtObj : T; aDoMappingProc : TMappingProc<Tm>; aCustomMapping: TCustomMapping = nil); overload;
    {$ELSE}
    class function Map(aSrcObj : TObject; aDoMappingProc : TMappingProc<TObject>; aCustomMapping: TCustomMapping = nil): T; overload;
    class procedure Map(aSrcObj : TObject; aTgtObj : T; aDoMappingProc : TMappingProc<TObject>; aCustomMapping: TCustomMapping);
    {$ENDIF}
  end;

  IAutoMapper<TClass1, TClass2 : class, constructor> = interface
  ['{9F7B2DEA-76D8-4DD1-95D0-22C22AEB5DD0}']
    function Map(aSrcObj : TClass1) : TClass2; overload;
    {$IFNDEF FPC}
    function Map(aSrcObj : TClass2) : TClass1; overload;
    procedure SetOnDoMapping(CustomProc : TMappingProc<TClass1>);
    procedure SetOnAfterMapping(CustomProc : TAfterMappingProc<TClass1,TClass2>);
    {$ELSE}
    //freepascal detects overload with generic types as duplicated function, added dummy field to avoid this
    function Map(aSrcObj : TClass2; dummy : Boolean = True) : TClass1; overload;
    {$ENDIF}
  end;

  TAutoMapper<TClass1, TClass2 : class, constructor> = class(TInterfacedObject, IAutoMapper<TClass1, TClass2>)
  private
    fCustomMapping : TCustomMapping;
    {$IFNDEF FPC}
    fOnDoMapping : TMappingProc<TClass1>;
    {$ELSE}
    fOnDoMapping : TMappingProc<TObject>;
    {$ENDIF}
    fOnAfterMapping : TAfterMappingProc<TClass1,TClass2>;
  public
    constructor Create;
    destructor Destroy; override;
    property CustomMapping : TCustomMapping read fCustomMapping write fCustomMapping;
    {$IFNDEF FPC}
    property OnDoMapping : TMappingProc<TClass1> read fOnDoMapping write fOnDoMapping;
    {$ELSE}
    property OnDoMapping : TMappingProc<TObject> read fOnDoMapping write fOnDoMapping;
    {$ENDIF}
    property OnAfterMapping : TAfterMappingProc<TClass1,TClass2> read fOnAfterMapping write fOnAfterMapping;
    function Map(aSrcObj : TClass1) : TClass2; overload;
    {$IFNDEF FPC}
    function Map(aSrcObj : TClass2) : TClass1; overload;
    procedure SetOnDoMapping(CustomProc : TMappingProc<TClass1>);
    procedure SetOnAfterMapping(CustomProc : TAfterMappingProc<TClass1,TClass2>);
    {$ELSE}
    //freepascal detects overload with generic types as duplicated function, added dummy field to avoid this
    function Map(aSrcObj : TClass2; dummy : Boolean = True) : TClass1; overload;
    {$ENDIF}
  end;

  EAutoMapperError = class(Exception);

implementation

{ TObjMapper }

class procedure TObjMapper.Map(aSrcObj : TObject; aTgtObj : TObject; aCustomMapping: TCustomMapping = nil);
begin
  Map{$IFNDEF FPC}<TObject>{$ENDIF}(aSrcObj,aTgtObj,nil,aCustomMapping);
end;

{$IFNDEF FPC}
class procedure TObjMapper.Map<Tm>(aSrcObj : TObject; aTgtObj : TObject; aDoMappingProc : TMappingProc<Tm>; aCustomMapping: TCustomMapping = nil);
{$ELSE}
class procedure TObjMapper.Map(aSrcObj : TObject; aTgtObj : TObject; aDoMappingProc : TMappingProc<TObject>; aCustomMapping: TCustomMapping = nil);
{$ENDIF}
var
  ctx : TRttiContext;
  rType : TRttiType;
  tgtprop : TRttiProperty;
  tgtfield : TRttiField;
  mapname : string;
  obj : TObject;
  manualmapping : Boolean;
  value : TFlexValue;
  {$IFNDEF FPC}
  clname : string;
  objvalue : TValue;
  {$ENDIF}
begin
  //if aTgtObj = nil then aTgtObj := GetTypeData(aTgtObj.ClassInfo).classType.Create;
  if aTgtObj = nil then raise EAutoMapperError.Create('TObjMapper: Target Object passed must be created before');

  {$IFNDEF FPC}
  objvalue := TValue.From(aSrcObj);
  {$ENDIF}
  rType := ctx.GetType(aSrcObj.ClassInfo);
  for tgtprop in ctx.GetType(aTgtObj.ClassInfo).GetProperties do
  begin
    if tgtprop.IsWritable then
    begin
      if not tgtprop.PropertyType.IsInstance then
      begin
        if Assigned(aCustomMapping) and (not Assigned(aDoMappingProc)) then
        begin
          if aCustomMapping.GetMap(tgtprop.Name,mapname) then
          begin
            {$IFNDEF PROPERTYPATH_MODE}
              if rType.GetProperty(mapname) = nil then raise EAutoMapperError.CreateFmt('No valid custom mapping (Source: %s - Target: %s)',[mapname,tgtprop.Name]);
              begin
                try
                  {$IFNDEF FPC}
                  tgtprop.SetValue(aTgtObj,rType.GetProperty(mapname).GetValue(aSrcObj));
                  {$ELSE}
                  SetPropValue(aTgtObj,tgtprop.Name,GetPropValue(aSrcObj,mapname));
                  {$ENDIF}
                except
                  on E : Exception do raise EAutoMapperError.CreateFmt('Error mapping property "%s" : %s',[tgtprop.Name,e.message]);
                end;
              end;
            {$ELSE}
              if not TRTTI.PathExists(aSrcObj,mapname) then raise EAutoMapperError.CreateFmt('No valid custom mapping (Source: %s - Target: %s)',[mapname,tgtprop.Name]);
              TRTTI.SetPathValue(aTgtObj,tgtprop.Name,TRTTI.GetPathValue(aSrcObj,mapname));
            {$ENDIF}
          end
          else
          begin
            if rType.GetProperty(tgtprop.Name) <> nil then
            try
              {$IFNDEF FPC}
              tgtprop.SetValue(aTgtObj,rType.GetProperty(tgtprop.Name).GetValue(aSrcObj));
              {$ELSE}
              SetPropValue(aTgtObj,tgtprop.Name,GetPropValue(aSrcObj,tgtprop.Name));
              {$ENDIF}
            except
              on E : Exception do raise EAutoMapperError.CreateFmt('Error mapping property "%s" : %s',[tgtprop.Name,e.message]);
            end;
          end;
        end
        else
        begin
          try
            if Assigned(aDoMappingProc) then
            begin
              {$IFNDEF FPC}
              aDoMappingProc(objvalue.AsType<Tm>,tgtprop.Name,value);
              manualmapping := not value.IsEmpty;
              {$ELSE}
              aDoMappingProc(aSrcObj,tgtprop.Name,value);
              manualmapping := not varType(value) = varEmpty;
              {$ENDIF}
            end
            else manualmapping := False;

            if manualmapping then
            begin
              {$IFNDEF FPC}
              tgtprop.SetValue(aTgtObj,value);
              {$ELSE}
              SetPropValue(aTgtObj,tgtprop.Name,value);
              {$ENDIF}
            end
            else
            begin
              {$IFNDEF FPC}
              if rType.GetProperty(tgtprop.Name) <> nil then tgtprop.SetValue(aTgtObj,rType.GetProperty(tgtprop.Name).GetValue(aSrcObj));
              {$ELSE}
              if rType.GetProperty(tgtprop.Name) <> nil then SetPropValue(aTgtObj,tgtprop.Name,GetPropValue(aSrcObj,tgtprop.Name));
              {$ENDIF}
            end;
          except
            on E : Exception do raise EAUtoMapperError.CreateFmt('Error mapping property "%s" : %s',[tgtprop.Name,e.message]);
          end;
        end;
      end
      else
      begin
        obj := tgtprop.GetValue(aTgtObj).AsObject;
        {$IFNDEF FPC}
        if obj = nil then obj := GetObjectProp(aSrcObj,tgtprop.Name).ClassType.Create;// TObject.Create;
        {$ELSE}
        if obj = nil then obj := GetObjectProp(aSrcObj,tgtprop.Name).ClassType.Create;
        {$ENDIF}

        if obj <> nil then
        begin
          {$IFNDEF FPC}
          try
            if (rType.GetProperty(tgtprop.Name) <> nil)
              and (not rType.GetProperty(tgtprop.Name).GetValue(aSrcObj).IsEmpty) then clname := rType.GetProperty(tgtprop.Name).GetValue(aSrcObj).AsObject.ClassName
            else Continue;
          except
            on E : Exception do raise EAUtoMapperError.CreateFmt('Error mapping property "%s" : %s',[tgtprop.Name,e.message]);
          end;
          if clname.StartsWith('TList') then TListMapper.Map(rType.GetProperty(tgtprop.Name).GetValue(aSrcObj).AsObject,obj,aCustomMapping)
          else if clname.StartsWith('TObjectList') then TObjListMapper.Map(rType.GetProperty(tgtprop.Name).GetValue(aSrcObj).AsObject,obj,aCustomMapping)
            else TObjMapper.Map(rType.GetProperty(tgtprop.Name).GetValue(aSrcObj).AsObject,obj,aCustomMapping)
          {$ELSE}
          TObjMapper.Map(GetObjectProp(aSrcObj,tgtprop.Name),obj,aCustomMapping);
          SetObjectProp(aTgtObj,tgtprop.Name,obj);
          {$ENDIF}
        end
        else raise EAutoMapperError.CreateFmt('Target object "%s" not autocreated by class',[tgtprop.Name]);
      end;
    end;
  end;

  for tgtfield in ctx.GetType(aTgtObj.ClassInfo).GetFields do
  begin
    if tgtfield.Visibility in [mvPublic, mvPublished] then
    begin
      if not tgtfield.FieldType.IsInstance then
      begin
        if Assigned(aCustomMapping) and (not Assigned(aDoMappingProc)) then
        begin
          if aCustomMapping.GetMap(tgtfield.Name,mapname) then
          begin
            {$IFNDEF PROPERTYPATH_MODE}
              if rType.GetField(mapname) <> nil then
              begin
                try
                  tgtfield.SetValue(aTgtObj,rType.GetField(mapname).GetValue(aSrcObj));
                except
                  on E : Exception do raise EAutoMapperError.CreateFmt('Error mapping field "%s" : %s',[tgtfield.Name,e.message]);
                end;
              end
              else if rType.GetProperty(mapname) <> nil then
              begin
                try
                  tgtfield.SetValue(aTgtObj,rType.GetProperty(mapname).GetValue(aSrcObj));
                except
                  on E : Exception do raise EAutoMapperError.CreateFmt('Error mapping field "%s" : %s',[tgtfield.Name,e.message]);
                end;
              end
              else raise EAutoMapperError.CreateFmt('No valid custom mapping (Source: %s - Target: %s)',[mapname,tgtfield.Name]);
            {$ELSE}
              if not TRTTI.PathExists(aSrcObj,mapname) then raise EAutoMapperError.CreateFmt('No valid custom mapping (Source: %s - Target: %s)',[mapname,tgtfield.Name]);
              TRTTI.SetPathValue(aTgtObj,tgtfield.Name,TRTTI.GetPathValue(aSrcObj,mapname));
            {$ENDIF}
          end
          else
          begin
            if rType.GetField(tgtfield.Name) <> nil then
            begin
              try
                tgtfield.SetValue(aTgtObj,rType.GetField(tgtfield.Name).GetValue(aSrcObj));
              except
                on E : Exception do raise EAutoMapperError.CreateFmt('Error mapping field "%s" : %s',[tgtfield.Name,e.message]);
              end;
            end
            else if rType.GetProperty(tgtfield.Name) <> nil then
            begin
              try
                tgtfield.SetValue(aTgtObj,rType.GetProperty(tgtfield.Name).GetValue(aSrcObj));
              except
                on E : Exception do raise EAutoMapperError.CreateFmt('Error mapping field "%s" : %s',[tgtfield.Name,e.message]);
              end;
            end;
          end;
        end
        else
        begin
          try
            if Assigned(aDoMappingProc) then
            begin
              {$IFNDEF FPC}
              aDoMappingProc(objvalue.AsType<Tm>,tgtfield.Name,value);
              manualmapping := not value.IsEmpty;
              {$ELSE}
              aDoMappingProc(aSrcObj,tgtfield.Name,value);
              manualmapping := not varType(value) = varEmpty;
              {$ENDIF}
            end
            else manualmapping := False;

            if manualmapping then
            begin
              tgtfield.SetValue(aTgtObj,value);
            end
            else
            begin
              if rType.GetField(tgtfield.Name) <> nil then tgtfield.SetValue(aTgtObj,rType.GetField(tgtfield.Name).GetValue(aSrcObj))
              else if rType.GetProperty(tgtfield.Name) <> nil then tgtfield.SetValue(aTgtObj,rType.GetProperty(tgtfield.Name).GetValue(aSrcObj));
            end;
          except
            on E : Exception do raise EAUtoMapperError.CreateFmt('Error mapping field "%s" : %s',[tgtfield.Name,e.message]);
          end;
        end;
      end
      else
      begin
        obj := tgtfield.GetValue(aTgtObj).AsObject;
        if obj = nil then
        begin
           if tgtfield.FieldType.IsInstance then
           begin
             obj := tgtfield.FieldType.AsInstance.MetaclassType.Create;
           end;
        end;

        if obj <> nil then
        begin
          try
             if (rType.GetField(tgtfield.Name) <> nil) and (not rType.GetField(tgtfield.Name).GetValue(aSrcObj).IsEmpty) then
               clname := rType.GetField(tgtfield.Name).GetValue(aSrcObj).AsObject.ClassName
             else if (rType.GetProperty(tgtfield.Name) <> nil) and (not rType.GetProperty(tgtfield.Name).GetValue(aSrcObj).IsEmpty) then
               clname := rType.GetProperty(tgtfield.Name).GetValue(aSrcObj).AsObject.ClassName
             else Continue;
          except
            on E : Exception do raise EAUtoMapperError.CreateFmt('Error mapping field "%s" : %s',[tgtfield.Name,e.message]);
          end;

          if clname.StartsWith('TList') then
          begin
             if rType.GetField(tgtfield.Name) <> nil then TListMapper.Map(rType.GetField(tgtfield.Name).GetValue(aSrcObj).AsObject,obj,aCustomMapping)
             else TListMapper.Map(rType.GetProperty(tgtfield.Name).GetValue(aSrcObj).AsObject,obj,aCustomMapping);
          end
          else if clname.StartsWith('TObjectList') then
          begin
             if rType.GetField(tgtfield.Name) <> nil then TObjListMapper.Map(rType.GetField(tgtfield.Name).GetValue(aSrcObj).AsObject,obj,aCustomMapping)
             else TObjListMapper.Map(rType.GetProperty(tgtfield.Name).GetValue(aSrcObj).AsObject,obj,aCustomMapping);
          end
          else
          begin
             if rType.GetField(tgtfield.Name) <> nil then TObjMapper.Map(rType.GetField(tgtfield.Name).GetValue(aSrcObj).AsObject,obj,aCustomMapping)
             else TObjMapper.Map(rType.GetProperty(tgtfield.Name).GetValue(aSrcObj).AsObject,obj,aCustomMapping);
          end;
          tgtfield.SetValue(aTgtObj, obj);
        end
        else raise EAutoMapperError.CreateFmt('Target object "%s" not autocreated by class',[tgtfield.Name]);
      end;
    end;
  end;
end;

class function TMapper<T>.Map(aSrcObj : TObject; aCustomMapping: TCustomMapping = nil) : T;
begin
  Result := Map{$IFNDEF FPC}<TObject>{$ENDIF}(aSrcObj,nil,aCustomMapping);
end;

{$IFNDEF FPC}
class function TMapper<T>.Map<Tm>(aSrcObj : TObject; aDoMappingProc : TMappingProc<Tm>; aCustomMapping: TCustomMapping = nil): T;
{$ELSE}
class function TMapper<T>.Map(aSrcObj : TObject; aDoMappingProc : TMappingProc<TObject>; aCustomMapping: TCustomMapping = nil): T;
{$ENDIF}
var
  obj : T;
begin
  obj := T.Create;
  {$IFNDEF FPC}
  TObjMapper.Map<Tm>(aSrcObj,obj,aDoMappingProc,aCustomMapping);
  {$ELSE}
  TObjMapper.Map(aSrcObj,obj,aDoMappingProc,aCustomMapping);
  {$ENDIF}
  Result := obj;
end;

class procedure TMapper<T>.Map(aSrcObj : TObject; aTgtObj : T; aCustomMapping : TCustomMapping = nil);
begin
  {$IFNDEF FPC}
  Map<T>(aSrcObj,aTgtObj,nil,aCustomMapping);
  {$ELSE}
  Map(aSrcObj,aTgtObj,nil,aCustomMapping);
  {$ENDIF}
end;

{$IFNDEF FPC}
class procedure TMapper<T>.Map<Tm>(aSrcObj : TObject; aTgtObj : T; aDoMappingProc : TMappingProc<Tm>; aCustomMapping : TCustomMapping = nil);
{$ELSE}
class procedure TMapper<T>.Map(aSrcObj : TObject; aTgtObj : T; aDoMappingProc : TMappingProc<TObject>; aCustomMapping : TCustomMapping);
{$ENDIF}
begin
  {$IFNDEF FPC}
  TObjMapper.Map<Tm>(aSrcObj, aTgtObj, aDoMappingProc, aCustomMapping);
  {$ELSE}
  TObjMapper.Map(aSrcObj, aTgtObj, aDoMappingProc, aCustomMapping);
  {$ENDIF}
end;


{ TAutoMapper<TClass1, TClass2> }

constructor TAutoMapper<TClass1, TClass2>.Create;
begin
  fCustomMapping := TCustomMapping.Create;
  fOnDoMapping := nil;
  fOnAfterMapping := nil;
end;

destructor TAutoMapper<TClass1, TClass2>.Destroy;
begin
  if Assigned(fCustomMapping) then fCustomMapping.Free;
  fOnDoMapping := nil;
  fOnAfterMapping := nil;
  inherited;
end;

function TAutoMapper<TClass1, TClass2>.Map(aSrcObj: TClass1): TClass2;
var
  obj : TObject;
begin
  obj := aSrcObj as TObject;
  //objvalue := TValue.From(aSrcObj).AsObject;
  {$IFNDEF FPC}
  Result := TMapper<TClass2>.Map<TClass1>(obj,fOnDoMapping,fCustomMapping);
  {$ELSE}
  Result := TMapper<TClass2>.Map(obj,fOnDoMapping,fCustomMapping);
  {$ENDIF}
  if Assigned(fOnAfterMapping) then fOnAfterMapping(aSrcObj,Result);
end;

{$IFNDEF FPC}
function TAutoMapper<TClass1, TClass2>.Map(aSrcObj: TClass2): TClass1;
begin
  Result := TMapper<TClass1>.Map<TClass1>(aSrcObj,fOnDoMapping,fCustomMapping);
end;
procedure TAutoMapper<TClass1, TClass2>.SetOnAfterMapping(CustomProc: TAfterMappingProc<TClass1, TClass2>);
begin
  fOnAfterMapping := CustomProc;
end;

procedure TAutoMapper<TClass1, TClass2>.SetOnDoMapping(CustomProc: TMappingProc<TClass1>);
begin
  fOnDoMapping := CustomProc;
end;

{$ELSE}
function TAutoMapper<TClass1, TClass2>.Map(aSrcObj: TClass2; dummy : Boolean = True): TClass1;
begin
  Result := TMapper<TClass1>.Map(aSrcObj,fOnDoMapping,fCustomMapping);
end;
{$ENDIF}

{ TCustomMapping }

procedure TCustomMapping.AddMap(const aName, aMapName: string);
begin
  //add map fields
  fMapDictionary.Add(aName,aMapName);
  //add reverse lookup if not same name
  if aName <> aMapName then fMapDictionary.Add(aMapName,aName);
end;

function TCustomMapping.Count: Integer;
begin
  Result := fMapDictionary.Count;
end;

constructor TCustomMapping.Create;
begin
  fMapDictionary := TDictionary<string,string>.Create;
end;

destructor TCustomMapping.Destroy;
begin
  fMapDictionary.Free;
  inherited;
end;

function TCustomMapping.GetMap(const aName: string; out vMapName: string): Boolean;
begin
  Result := fMapDictionary.TryGetValue(aName,vMapName);
end;

{ TListMapper }

class procedure TListMapper.Map(aSrcList, aTgtList: TObject; aCustomMapping: TCustomMapping);
{$IFNDEF FPC}
var
  rtype    : TRttiType;
  propCount: TRttiProperty;
  propItems: TRttiIndexedProperty;
  typinfo  : PTypeInfo;
  obj      : TObject;
  i, cnt   : Integer;
  idxVal   : TValue;
  elemVal  : TValue;
  ctx      : TRttiContext;
begin
  // Use Count property + Items[] indexed property — works for any TList<T>
  // regardless of whether ToArray is exposed via RTTI.
  rtype     := ctx.GetType(aSrcList.ClassInfo);
  propCount := rtype.GetProperty('Count');
  propItems := rtype.GetIndexedProperty('Items');
  if (propCount = nil) or (propItems = nil) then Exit;

  cnt      := propCount.GetValue(aSrcList).AsInteger;
  typinfo  := propItems.PropertyType.Handle;

  case typinfo.Kind of
    tkChar, tkString, tkWChar, tkWString, tkUString : TList<string>(aTgtList).Capacity   := cnt;
    tkInteger, tkInt64                              : TList<Integer>(aTgtList).Capacity   := cnt;
    tkFloat                                         : TList<Double>(aTgtList).Capacity   := cnt;
    tkEnumeration                                   : TList<Boolean>(aTgtList).Capacity   := cnt;
    tkClass                                         : TList<TObject>(aTgtList).Capacity   := cnt;
    tkRecord :
      begin
        // fall back to object-level mapping for record-element lists
        TObjMapper.Map(aSrcList, aTgtList, aCustomMapping);
        Exit;
      end;
  end;

  for i := 0 to cnt - 1 do
  begin
    idxVal  := TValue.From<Integer>(i);
    elemVal := propItems.GetValue(aSrcList, [idxVal]);

    if typinfo.Kind = tkClass then
    begin
      obj := typinfo.TypeData.ClassType.Create;
      TObjMapper.Map(elemVal.AsObject, obj, aCustomMapping);
      TList<TObject>(aTgtList).Add(obj);
    end
    else
    begin
      case typinfo.Kind of
        tkChar, tkString, tkWChar, tkWString, tkUString : TList<string>(aTgtList).Add(elemVal.AsString);
        tkInteger, tkInt64                              : TList<Integer>(aTgtList).Add(elemVal.AsInt64);
        tkFloat                                         : TList<Double>(aTgtList).Add(elemVal.AsExtended);
        tkEnumeration                                   : TList<Boolean>(aTgtList).Add(elemVal.AsBoolean);
      end;
    end;
  end;
end;
{$ELSE}
begin

end;
{$ENDIF}


{ TObjListMapper }

class procedure TObjListMapper.Map(aSrcObjList, aTgtObjList: TObject; aCustomMapping: TCustomMapping);
begin
  {$IFNDEF FPC}
  Map<TObject>(aSrcObjList,aTgtObjList,nil,aCustomMapping);
  {$ELSE}
  Map(aSrcObjList,aTgtObjList,nil,aCustomMapping);
  {$ENDIF}
end;

{$IFNDEF FPC}
class procedure TObjListMapper.Map<Tm>(aSrcObjList : TObject; aTgtObjList : TObject; aDoMappingProc : TMappingProc<Tm>; aCustomMapping : TCustomMapping = nil);
var
  rtype    : TRttiType;
  typinfo  : PTypeInfo;
  propCount: TRttiProperty;
  propItems: TRttiIndexedProperty;
  obj      : TObject;
  i, cnt   : Integer;
  idxVal   : TValue;
  ctx      : TRttiContext;
begin
  rtype     := ctx.GetType(aSrcObjList.ClassInfo);
  propCount := rtype.GetProperty('Count');
  propItems := rtype.GetIndexedProperty('Items');
  if (propCount = nil) or (propItems = nil) then Exit;

  cnt     := propCount.GetValue(aSrcObjList).AsInteger;
  typinfo := propItems.PropertyType.Handle;

  TObjectList<TObject>(aTgtObjList).Capacity := cnt;

  for i := 0 to cnt - 1 do
  begin
    idxVal := TValue.From<Integer>(i);
    obj    := typinfo.TypeData.ClassType.Create;
    TObjMapper.Map<Tm>(propItems.GetValue(aSrcObjList, [idxVal]).AsObject, obj, aDoMappingProc, aCustomMapping);
    TObjectList<TObject>(aTgtObjList).Add(obj);
  end;
end;
{$ELSE}
class procedure TObjListMapper.Map(aSrcObjList : TObject; aTgtObjList : TObject; aDoMappingProc : TMappingProc<TObject>; aCustomMapping : TCustomMapping = nil);
begin

end;

{$ENDIF}

{ TMapper }

{$IFNDEF FPC}
class function TMapper.Map<T>(aSrcObj: TObject): T;
begin
  Result := T.Create;
  TObjMapper.Map(aSrcObj,Result,nil);
end;
{$ENDIF}

end.
