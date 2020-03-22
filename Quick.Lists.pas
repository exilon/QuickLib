{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.Lists
  Description : Generic Lists functions
  Author      : Kike Pérez
  Version     : 1.2
  Created     : 04/11/2018
  Modified    : 12/03/2020

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

unit Quick.Lists;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  RTTI,
  TypInfo,
  Generics.Collections,
  Generics.Defaults,
  Quick.RTTI.Utils,
  Quick.Arrays,
  Quick.Value;

  //enable use of property paths (like namespaces) in search
  {$DEFINE PROPERTYPATH_MODE}

type

  TClassField = (cfField, cfProperty);

  TSearchDictionary<TKey,TValue> = class(TObjectDictionary<TKey,TValue>)
  private
    fIndexName : string;
    fFieldName : string;
    fClassField : TClassField;
  public
    property IndexName : string read fIndexName write fIndexName;
    property FieldName : string read fFieldName write fFieldName;
    property ClassField : TClassField read fClassField write fClassField;
  end;

  TIndexList<T> = class
  private
    fList : TList<TSearchDictionary<Variant,T>>;
    fDictionaryIndex : TObjectDictionary<string,TSearchDictionary<Variant,T>>;
  public
    constructor Create;
    destructor Destroy; override;
    property List : TList<TSearchDictionary<Variant,T>> read fList;
    function Get(const aIndexName : string) : TSearchDictionary<Variant,T>;
    procedure Add(const aIndexName, aFieldName : string; aClassField : TClassField = cfProperty);
    procedure Remove(const aIndexName : string);
  end;

  TIndexedObjectList<T: class> = class(TList<T>)
  private
    fOwnsObjects: Boolean;
    fIndexes : TIndexList<T>;
  protected
    procedure Notify(const Value: T; Action: TCollectionNotification); override;
  public
    constructor Create(aOwnsObjects: Boolean = True); overload;
    constructor Create(const aComparer: IComparer<T>; aOwnsObjects: Boolean = True); overload;
    constructor Create(const aCollection: TEnumerable<T>; aOwnsObjects: Boolean = True); overload;
    destructor Destroy; override;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Indexes : TIndexList<T> read fIndexes;
    function Get(const aIndexName : string; aValue : Variant) : T;
  end;

  TSearchObjectList<T: class> = class(TObjectList<T>)
  public
    function Get(const aFieldName: string; const aValue: string; aClassField : TClassField = cfProperty) : T; overload;
    function Get(const aFieldName : string; aValue : Int64; aClassField : TClassField = cfProperty) : T; overload;
    function Get(const aFieldName : string; aValue : Double; aClassField : TClassField = cfProperty) : T; overload;
    function Get(const aFieldName : string; aValue : TDateTime; aClassField : TClassField = cfProperty) : T; overload;
  end;

implementation



{ TIndexedObjectList<T> }

constructor TIndexedObjectList<T>.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := aOwnsObjects;
  fIndexes := TIndexList<T>.Create;
end;

constructor TIndexedObjectList<T>.Create(const aComparer: IComparer<T>; aOwnsObjects: Boolean);
begin
  inherited Create(aComparer);
  FOwnsObjects := aOwnsObjects;
  fIndexes := TIndexList<T>.Create;
end;

constructor TIndexedObjectList<T>.Create(const aCollection: TEnumerable<T>; aOwnsObjects: Boolean);
begin
  inherited Create(aCollection);
  FOwnsObjects := aOwnsObjects;
  fIndexes := TIndexList<T>.Create;
end;

procedure TIndexedObjectList<T>.Notify(const Value: T; Action: TCollectionNotification);
var
  sindex : TSearchDictionary<Variant,T>;
  propvalue : TValue;
begin
  inherited;
  if Action = cnAdded then
  begin
    for sindex in fIndexes.List do
    begin
      try
        if sindex.ClassField = TClassField.cfField then propvalue := TRTTI.GetFieldValue(TObject(Value),sindex.FieldName)
        else
        begin
          {$IFNDEF PROPERTYPATH_MODE}
          propvalue := TRTTI.GetPropertyValue(TObject(Value),sindex.FieldName);
          {$ELSE}
          propvalue := TRTTI.GetPathValue(TObject(Value),sindex.FieldName);
          {$ENDIF}
        end;
      except
        raise Exception.CreateFmt('Cannot add value to "%s" search dictionary!',[sindex.IndexName]);
      end;
      sindex.Add(propvalue.AsVariant,Value);
    end;
  end;
  //remove object if owned
  if OwnsObjects and ((Action = cnRemoved) or (Action = cnExtracted)) then
  begin
    for sindex in fIndexes.List do
    begin
      try
        if sindex.ClassField = TClassField.cfField then propvalue := TRTTI.GetFieldValue(TObject(Value),sindex.FieldName)
        else
        begin
          {$IFNDEF PROPERTYPATH_MODE}
          propvalue := TRTTI.GetPropertyValue(TObject(Value),sindex.FieldName);
          {$ELSE}
          propvalue := TRTTI.GetPathValue(TObject(Value),sindex.FieldName);
          {$ENDIF}
        end;
      except
        raise Exception.CreateFmt('Cannot remove value to "%s" search dictionary!',[sindex.IndexName]);
      end;
      sindex.Remove(propvalue.AsVariant);
    end;
    Value.DisposeOf;
  end;
end;

destructor TIndexedObjectList<T>.Destroy;
begin
  inherited;
  fIndexes.Free;
end;

function TIndexedObjectList<T>.Get(const aIndexName: string; aValue : Variant): T;
var
  sindex : TSearchDictionary<Variant,T>;
begin
  Result := nil;
  sindex := fIndexes.Get(aIndexName.ToLower);
  if sindex <> nil then sindex.TryGetValue(aValue,Result)
    else raise Exception.CreateFmt('Index "%s" not found!',[aIndexName]);
end;

{ TIndexList<T> }

procedure TIndexList<T>.Add(const aIndexName, aFieldName : string; aClassField : TClassField = cfProperty);
var
  sdict : TSearchDictionary<Variant,T>;
begin
  if aClassField = TClassField.cfField then
  begin
    if not TRTTI.FieldExists(TypeInfo(T),aFieldName) then raise Exception.CreateFmt('Not found field "%s" to create a search dictionary!',[aFieldName]);
  end
  else
  begin
    if not TRTTI.PropertyExists(TypeInfo(T),aFieldName) then raise Exception.CreateFmt('Not found property "%s" to create a search dictionary!',[aFieldName]);
  end;
  sdict := TSearchDictionary<Variant,T>.Create;
  sdict.IndexName := aIndexName;
  sdict.FieldName := aFieldName;
  sdict.ClassField := aClassField;
  fList.Add(sdict);
  fDictionaryIndex.Add(aIndexName.ToLower,sdict);
end;

procedure TIndexList<T>.Remove(const aIndexName: string);
var
  sdict : TSearchDictionary<Variant,T>;
begin
  if not fDictionaryIndex.ContainsKey(aIndexName) then raise Exception.CreateFmt('Cannot remove an inexistent "%s" search dictionary!',[aIndexName]);
  fList.Remove(sdict);
  fDictionaryIndex.Remove(aIndexName.ToLower);
  sdict.Free;
end;

constructor TIndexList<T>.Create;
begin
  fList := TList<TSearchDictionary<Variant,T>>.Create;
  fDictionaryIndex := TObjectDictionary<string,TSearchDictionary<Variant,T>>.Create;
end;

destructor TIndexList<T>.Destroy;
var
  sindex : TSearchDictionary<Variant,T>;
begin
  for sindex in fList do sindex.Free;
  fList.Free;
  fDictionaryIndex.Free;
  inherited;
end;

function TIndexList<T>.Get(const aIndexName: string): TSearchDictionary<Variant, T>;
begin
  Result := nil;
  fDictionaryIndex.TryGetValue(aIndexName,Result);
end;

{ TSearchObjectList<T> }

function TSearchObjectList<T>.Get(const aFieldName: string; const aValue: string; aClassField : TClassField = cfProperty): T;
var
  val : T;
begin
  Result := nil;
  for val in List do
  begin
    if aClassField = TClassField.cfField then
    begin
      if (val <> nil) and (TRTTI.GetFieldValue(TObject(val),aFieldName).AsString = aValue) then Exit(val);
    end
    else
    begin
      if (val <> nil) and (GetStrProp(TObject(val),aFieldName) = aValue) then Exit(val);
    end;
  end;
end;

function TSearchObjectList<T>.Get(const aFieldName: string; aValue: Int64; aClassField : TClassField = cfProperty): T;
var
  val : T;
begin
  Result := nil;
  for val in List do
  begin
    if aClassField = TClassField.cfField then
    begin
      if TRTTI.GetFieldValue(TObject(val),aFieldName).AsInt64 = aValue then Exit(val);
    end
    else
    begin
      if GetInt64Prop(TObject(val),aFieldName) = aValue then Exit(val);
    end;
  end;
end;

function TSearchObjectList<T>.Get(const aFieldName: string; aValue: Double; aClassField : TClassField = cfProperty): T;
var
  val : T;
begin
  Result := nil;
  for val in List do
  begin
    if aClassField = TClassField.cfField then
    begin
      if TRTTI.GetFieldValue(TObject(val),aFieldName).AsExtended = aValue then Exit(val);
    end
    else
    begin
      if GetFloatProp(TObject(val),aFieldName) = aValue then Exit(val);
    end;
  end;
end;

function TSearchObjectList<T>.Get(const aFieldName: string; aValue: TDateTime; aClassField : TClassField = cfProperty): T;
var
  val : T;
begin
  Result := nil;
  for val in List do
  begin
    if aClassField = TClassField.cfField then
    begin
      if TRTTI.GetFieldValue(TObject(val),aFieldName).AsExtended = aValue then Exit(val);
    end
    else
    begin
      if GetFloatProp(TObject(val),aFieldName) = aValue then Exit(val);
    end;
  end;
end;

end.
