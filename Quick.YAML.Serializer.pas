{ ***************************************************************************

  Copyright (c) 2015-2020 Kike Pérez

  Unit        : Quick.YAML.Serializer
  Description : YAML Serializer
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 12/04/2019
  Modified    : 07/04/20120

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

unit Quick.YAML.Serializer;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  TypInfo,
  {$IFDEF FPC}
   rttiutils,
   strUtils,
   Generics.Collections,
  {$ELSE}
    {$IFDEF DELPHIRX103_UP}
    System.Generics.Collections,
    {$ENDIF}
  {$ENDIF}
  DateUtils,
  Quick.Commons,
  Quick.RTTI.Utils,
  Quick.YAML,
  Quick.Value,
  Quick.Arrays;

type

  EYamlSerializeError = class(Exception);
  EYamlDeserializeError = class(Exception);

  {$IFNDEF FPC}
  TNotSerializableProperty = class(TCustomAttribute);

  TCommentProperty = class(TCustomAttribute)
  private
    fComment : string;
  public
    constructor Create(const aComment: string);
    property Comment : string read fComment;
  end;

  TCustomNameProperty = class(TCustomAttribute)
  private
    fName : string;
  public
    constructor Create(const aName: string);
    property Name : string read fName;
  end;
  {$ENDIF}

  IYamlSerializer = interface
  ['{CA26F7AE-F1FE-41BE-9C23-723A687F60D1}']
    function YamlToObject(aType: TClass; const aYaml: string): TObject; overload;
    function YamlToObject(aObject: TObject; const aYaml: string): TObject; overload;
    function ObjectToYaml(aObject : TObject): string;
  end;

  TSerializeLevel = (slPublicProperty, slPublishedProperty);

  TRTTIYaml = class
  private
    fSerializeLevel : TSerializeLevel;
    fUseEnumNames : Boolean;
    fUseYamlCaseSense : Boolean;
    function GetValue(aAddr: Pointer; aType: TRTTIType): TValue; overload;
    function GetValue(aAddr: Pointer; aTypeInfo: PTypeInfo): TValue; overload;
    function IsAllowedProperty(aObject : TObject; const aPropertyName : string) : Boolean;
    function IsGenericList(aObject : TObject) : Boolean;
    function IsGenericXArray(const aClassName : string) : Boolean;
    function GetPropertyValue(Instance : TObject; const PropertyName : string) : TValue;
    function GetPropertyValueFromObject(Instance : TObject; const PropertyName : string) : TValue;
    {$IFNDEF FPC}
    function GetFieldValueFromRecord(aValue : TValue; const FieldName : string) : TValue;
    {$ENDIF}
    procedure SetPropertyValue(Instance : TObject; aPropInfo : PPropInfo; aValue : TValue); overload;
    procedure SetPropertyValue(Instance : TObject; const PropertyName : string; aValue : TValue); overload;
    {$IFDEF FPC}
    function FloatProperty(aObject : TObject; aPropInfo: PPropInfo): string;
    function GetPropType(aPropInfo: PPropInfo): PTypeInfo;
    procedure LoadSetProperty(aInstance : TObject; aPropInfo: PPropInfo; const aValue: string);
    {$ENDIF}
  public
    constructor Create(aSerializeLevel : TSerializeLevel; aUseEnumNames : Boolean = True);
    property UseEnumNames : Boolean read fUseEnumNames write fUseEnumNames;
    property UseYamlCaseSense : Boolean read fUseYamlCaseSense write fUseYamlCaseSense;
    {$IFNDEF FPC}
    function DeserializeDynArray(aTypeInfo : PTypeInfo; aObject : TObject; const aYamlArray: TYamlArray) : TValue;
    function DeserializeRecord(aRecord : TValue; aObject : TObject; const aYaml : TYamlObject) : TValue;
    {$ELSE}
    procedure DeserializeDynArray(aTypeInfo: PTypeInfo; const aPropertyName : string; aObject: TObject; const aYamlArray: TYamlArray);
    {$ENDIF}
    function DeserializeClass(aType : TClass; const aYaml : TYamlObject) : TObject;
    function DeserializeObject(aObject : TObject; const aYaml : TYamlObject) : TObject; overload;
    {$IFNDEF FPC}
    function DeserializeList(aObject: TObject; const aName : string; const aYaml: TYamlObject) : TObject;
    procedure DeserializeXArray(Instance : TObject; aRecord : TValue; aProperty : TRttiProperty; const aPropertyName : string; aYaml : TYamlObject);
    {$ENDIF}
    function DeserializeProperty(aObject : TObject; const aName : string; aProperty : TRttiProperty; const aYaml : TYamlObject) : TObject; overload;
    {$IFNDEF FPC}
    function DeserializeType(aObject : TObject; aType : TTypeKind; aTypeInfo : PTypeInfo; const aValue: string) : TValue;
    {$ELSE}
    function DeserializeType(aObject : TObject; aType : TTypeKind; const aPropertyName, aValue: string) : TValue;
    {$ENDIF}
    {$IFNDEF FPC}
    function Serialize(const aName : string; aValue : TValue) : TYamlPair; overload;
    {$ELSE}
    function Serialize(aObject : TObject; aType : TTypeKind; const aPropertyName : string) : TYamlPair;
    function Serialize(const aName : string; aValue : TValue) : TYamlPair;
    {$ENDIF}
    function Serialize(aObject : TObject) : TYamlObject; overload;
    function GetYamlPairByName(aYaml : TYamlObject; const aName : string) : TYamlPair;
  end;

  TYamlSerializer = class(TInterfacedObject,IYamlSerializer)
  strict private
    fSerializeLevel : TSerializeLevel;
    fUseEnumNames : Boolean;
    fUseYamlCaseSense : Boolean;
    fRTTIYaml : TRTTIYaml;
  private
    procedure SetUseEnumNames(const Value: Boolean);
    procedure SetUseYamlCaseSense(const Value: Boolean);
  public
    constructor Create(aSerializeLevel: TSerializeLevel; aUseEnumNames : Boolean = True);
    destructor Destroy; override;
    property SerializeLevel : TSerializeLevel read fSerializeLevel;
    property UseEnumNames : Boolean read fUseEnumNames write SetUseEnumNames;
    property UseYamlCaseSense : Boolean read fUseYamlCaseSense write SetUseYamlCaseSense;
    function YamlToObject(aType : TClass; const aYaml: string) : TObject; overload;
    function YamlToObject(aObject : TObject; const aYaml: string) : TObject; overload;
    function ObjectToYaml(aObject : TObject): string;
  end;

  PPByte = ^PByte;

resourcestring
  cNotSupportedDataType = 'Not supported "%s" data type "%s"';
  cNotSerializable = 'Object is not serializable';

implementation

{ TRTTIYaml }

{$IFNDEF FPC}
function TRTTIYaml.DeserializeDynArray(aTypeInfo: PTypeInfo; aObject: TObject; const aYamlArray: TYamlArray) : TValue;
var
  rType: PTypeInfo;
  len: NativeInt;
  pArr: Pointer;
  rItemValue: TValue;
  i: Integer;
  objClass: TClass;
  ctx : TRttiContext;
  Yaml : TYamlObject;
  rDynArray : TRttiDynamicArrayType;
  propObj : TObject;
begin
  if GetTypeData(aTypeInfo).DynArrElType = nil then Exit;
  if not assigned(aYamlArray) then Exit;

  len := aYamlArray.Count;
  rType := GetTypeData(aTypeInfo).DynArrElType^;
  pArr := nil;
  DynArraySetLength(pArr,aTypeInfo, 1, @len);
  try
    TValue.Make(@pArr,aTypeInfo, Result);
    rDynArray := ctx.GetType(Result.TypeInfo) as TRTTIDynamicArrayType;

    for i := 0 to aYamlArray.Count - 1 do
    begin
      rItemValue := nil;
      case rType.Kind of
        tkClass :
          begin
            if TYamlPair(aYamlArray.Items[i]).Value is TYamlObject then
            begin
              Yaml := TYamlObject(TYamlPair(aYamlArray.Items[i]).value);
              propObj := GetValue(PPByte(Result.GetReferenceToRawData)^ +rDynArray.ElementType.TypeSize * i, rDynArray.ElementType).AsObject;
              if propObj = nil then
              begin
                objClass := rType.TypeData.ClassType;
                rItemValue := DeserializeClass(objClass,yaml);
              end
              else
              begin
                DeserializeObject(propObj,yaml);
              end;
            end;
          end;
        tkRecord :
          begin
            Yaml := TYamlObject(TYamlPair(aYamlArray.Items[i]).value);
            rItemValue := DeserializeRecord(GetValue(PPByte(Result.GetReferenceToRawData)^ +rDynArray.ElementType.TypeSize * i,
                                            rDynArray.ElementType),aObject,Yaml);
          end;
        tkMethod, tkPointer, tkClassRef ,tkInterface, tkProcedure :
          begin
            //skip these properties
          end
      else
        begin
          rItemValue := DeserializeType(aObject,rType.Kind,aTypeInfo,aYamlArray.Items[i].Value);
        end;
      end;
      if not rItemValue.IsEmpty then Result.SetArrayElement(i,rItemValue);
    end;
    //aProperty.SetValue(aObject,rValue);
  finally
    DynArrayClear(pArr,aTypeInfo);
  end;
end;
{$ELSE}
procedure TRTTIYaml.DeserializeDynArray(aTypeInfo: PTypeInfo; const aPropertyName : string; aObject: TObject; const aYamlArray: TYamlArray);
var
  rType: PTypeInfo;
  len: NativeInt;
  pArr: Pointer;
  rItemValue: TValue;
  i: Integer;
  objClass: TClass;
  propObj : TObject;
  rValue : TValue;
  yaml : TYamlObject;
begin
  if GetTypeData(aTypeInfo).ElType2 = nil then Exit;
  len := aYamlArray.Count;
  rType := GetTypeData(aTypeInfo).ElType2;
  pArr := nil;
  DynArraySetLength(pArr,aTypeInfo, 1, @len);
  try
    TValue.Make(@pArr,aTypeInfo, rValue);
    for i := 0 to aYamlArray.Count - 1 do
    begin
      rItemValue := nil;
      case rType.Kind of
        tkClass :
          begin
            if TYamlPair(aYamlArray.Items[i]).Value is TYamlObject then
            begin
              Yaml := TYamlObject(TYamlPair(aYamlArray.Items[i]).value);
              propObj := GetValue(PPByte(rValue.GetReferenceToRawData)^ +GetTypeData(aTypeInfo).elSize * i, GetTypeData(aTypeInfo).ElType2).AsObject;
              if propObj = nil then
              begin
                //objClass := GetTypeData(aTypeInfo).ClassType;
                objClass := GetTypeData(GetTypeData(aTypeInfo).ElType2).ClassType;
                rItemValue := DeserializeClass(objClass,yaml);
              end
              else
              begin
                DeserializeObject(propObj,yaml);
              end;
            end;
          end;
        tkRecord :
          begin
            {Yaml := TYamlObject(aYamlArray.Items[i]);
            rItemValue := DeserializeRecord(GetValue(PPByte(Result.GetReferenceToRawData)^ +rDynArray.ElementType.TypeSize * i,
                                            rDynArray.ElementType),aObject,Yaml);  }
          end;
        tkMethod, tkPointer, tkClassRef ,tkInterface, tkProcedure :
          begin
            //skip these properties
          end
      else
        begin
          rItemValue := DeserializeType(aObject,GetTypeData(aTypeInfo).ElType2.Kind,aPropertyName,aYamlArray.Items[i].Value);
        end;
      end;
      if not rItemValue.IsEmpty then rValue.SetArrayElement(i,rItemValue);
    end;
    //aProperty.SetValue(aObject,rValue);
    SetDynArrayProp(aObject,GetPropInfo(aObject,aPropertyName),pArr);
  finally
    DynArrayClear(pArr,aTypeInfo);
  end;
end;
{$ENDIF}

{$IFNDEF FPC}
function TRTTIYaml.DeserializeRecord(aRecord : TValue; aObject : TObject; const aYaml : TYamlObject) : TValue;
var
  ctx : TRttiContext;
  rRec : TRttiRecordType;
  rField : TRttiField;
  rValue : TValue;
  member : TYamlPair;
  yArray : TYamlArray;
  Yaml : TYamlObject;
  objClass : TClass;
  propobj : TObject;
begin
  rRec := ctx.GetType(aRecord.TypeInfo).AsRecord;
  try
    for rField in rRec.GetFields do
    begin
      rValue := nil;
      //member := TYamlPair(aYaml.GetValue(rField.Name));
      member := GetYamlPairByName(aYaml,rField.Name);
      if member <> nil then
      case rField.FieldType.TypeKind of
        tkDynArray :
          begin
            yArray := TYamlObject.ParseYamlValue(member.ToYaml) as TYamlArray;
            try
              rValue := DeserializeDynArray(rField.FieldType.Handle,aObject,yArray);
            finally
              yArray.Free;
            end;
          end;
        tkClass :
          begin
            //if (member.YamlValue is TYamlObject) then
            begin
              propobj := rField.GetValue(@aRecord).AsObject;
              Yaml := TYamlObject.ParseYamlValue(member.ToYaml) as TYamlObject;
              try
                if propobj = nil then
                begin
                  objClass := rField.FieldType.Handle^.TypeData.ClassType;// aProperty.PropertyType.Handle^.TypeData.ClassType;
                  rValue := DeserializeClass(objClass,Yaml);
                end
                else
                begin
                  DeserializeObject(propobj,Yaml);
                end;
              finally
                Yaml.Free;
              end;
            end
          end;
        tkRecord :
          begin
            Yaml := TYamlObject.ParseYamlValue(member.ToYaml) as TYamlObject;
            try
              rValue := DeserializeRecord(rField.GetValue(aRecord.GetReferenceToRawData),aObject,Yaml);
            finally
              Yaml.Free;
            end;
          end
      else
        begin
          //rValue := DeserializeType(aObject,rField.FieldType.TypeKind,rField.FieldType.Handle,member.ToYaml);
          {$IFNDEF FPC}
          //avoid return unicode escaped chars if string
          if rField.FieldType.TypeKind in [tkString, tkLString, tkWString, tkUString] then
            {$IFDEF DELPHIRX103_UP}
            rValue := DeserializeType(aObject,rField.FieldType.TypeKind,rField.FieldType.Handle,member.Value.AsString)
            {$ELSE}
            rValue := DeserializeType(aObject,rField.FieldType.TypeKind,rField.FieldType.Handle,member.YamlString.ToString)
            {$ENDIF}
            else rValue := DeserializeType(aObject,rField.FieldType.TypeKind,rField.FieldType.Handle,member.Value.AsString);
          {$ELSE}
          rValue := DeserializeType(aObject,rField.FieldType.TypeKind,aName,member.Value.AsString);
          {$ENDIF}
        end;
      end;
      if not rValue.IsEmpty then rField.SetValue(aRecord.GetReferenceToRawData,rValue);
    end;
    Result := aRecord;
  finally
    ctx.Free;
  end;
end;
{$ENDIF}

constructor TRTTIYaml.Create(aSerializeLevel : TSerializeLevel; aUseEnumNames : Boolean = True);
begin
  fSerializeLevel := aSerializeLevel;
  fUseEnumNames := aUseEnumNames;
  fUseYamlCaseSense := False;
end;

function TRTTIYaml.DeserializeClass(aType: TClass; const aYaml: TYamlObject): TObject;
begin
  Result := nil;
  if (aYaml = nil) or ((aYaml as TYamlValue) is TYamlNull) or (aYaml.Count = 0) then Exit;

  try
    Result := aType.Create;
    Result := DeserializeObject(Result,aYaml);
  except
    on E : Exception do
    begin
      Result.Free;
      raise EYamlDeserializeError.CreateFmt('Deserialize error class "%s" : %s',[aType.ClassName,e.Message]);
    end;
  end;
end;

function TRTTIYaml.DeserializeObject(aObject: TObject; const aYaml: TYamlObject): TObject;
var
  ctx: TRttiContext;
  rType: TRttiType;
  rProp: TRttiProperty;
  {$IFNDEF FPC}
  attr: TCustomAttribute;
  {$ENDIF}
  propertyname : string;
begin
  Result := aObject;

  if (aYaml = nil) or ((aYaml as TYamlValue) is TYamlNull) or (aYaml.Count = 0) or (Result = nil) then Exit;

  try
    rType := ctx.GetType(aObject.ClassInfo);
    try
      for rProp in rType.GetProperties do
      begin
        {$IFNDEF FPC}
        if ((fSerializeLevel = slPublicProperty) and (rProp.PropertyType.IsPublicType))
            or ((fSerializeLevel = slPublishedProperty) and ((IsPublishedProp(aObject,rProp.Name)) or (rProp.Name = 'List'))) then
        {$ENDIF}
        begin
          if ((rProp.IsWritable) or (rProp.Name = 'List')) and (IsAllowedProperty(aObject,rProp.Name)) then
          begin
            propertyname := rProp.Name;
            {$IFNDEF FPC}
            for attr in rProp.GetAttributes do if attr is TCustomNameProperty then propertyname := TCustomNameProperty(attr).Name;
            if rProp.Name = 'List' then
            begin
              Result := DeserializeList(Result,propertyname,aYaml);
            end
            else if (rProp.GetValue(aObject).IsObject) and (IsGenericList(rProp.GetValue(aObject).AsObject)) then
            begin
              DeserializeList(rProp.GetValue(aObject).AsObject,'List',TYamlObject(aYaml.GetValue(propertyname)));
            end
            else if (not rProp.GetValue(aObject).IsObject) and (IsGenericXArray(rProp.GetValue(aObject){$IFNDEF NEXTGEN}.TypeInfo.Name{$ELSE}.TypeInfo.NameFld.ToString{$ENDIF})) then
            begin
              DeserializeXArray(Result,rProp.GetValue(aObject),rProp,propertyname,aYaml);
            end
            else
            {$ENDIF}
            Result := DeserializeProperty(Result,propertyname,rProp,aYaml);
          end;
        end;
      end;
    finally
      ctx.Free;
    end;
  except
    on E : Exception do
    begin
      Result.Free;
      raise EYamlDeserializeError.CreateFmt('Deserialize error for object "%s" : %s',[aObject.ClassName,e.Message]);
    end;
  end;
end;

{$IFNDEF FPC}
function TRTTIYaml.DeserializeList(aObject: TObject; const aName : string; const aYaml: TYamlObject) : TObject;
var
  ctx : TRttiContext;
  rType : TRttiType;
  yArray : TYamlArray;
  member : TYamlPair;
  rvalue : TValue;
  i : Integer;
  rProp : TRttiProperty;
  {$IFNDEF DELPHIRX103_UP}
  rfield : TRttiField;
  {$ENDIF}
begin
  Result := aObject;

  rType := ctx.GetType(aObject.ClassInfo);
  try
    rProp := rType.GetProperty('List');
    if rProp = nil then Exit;
  finally
    ctx.Free;
  end;
  member := GetYamlPairByName(aYaml,aName);
  //var a := aYaml.ToYaml;
  if member = nil then yArray := TYamlArray(aYaml) //TYamlObject.ParseYamlValue(aYaml.ToYaml) as TYamlArray
    else yArray := TYamlObject.ParseYamlValue(member.ToYaml) as TYamlArray;

  rvalue := DeserializeDynArray(rProp.PropertyType.Handle,Result,yArray);

  if not rValue.IsEmpty then
  begin
    {$IFDEF DELPHIRX103_UP}
    if (TObjectList<TObject>(aObject) <> nil) and (rvalue.IsArray) then
    begin
      TObjectList<TObject>(aObject).Clear;
      TObjectList<TObject>(aObject).Capacity := rvalue.GetArrayLength;
      for i := 0 to rvalue.GetArrayLength - 1 do
      begin
        TObjectList<TObject>(aObject).Add(rvalue.GetArrayElement(i).AsObject);
      end;
    end;
    {$ELSE}
    for rfield in rType.GetFields do
    begin
      if rfield.Name = 'FOwnsObjects' then rfield.SetValue(aObject,True);
      //if rfield.Name = 'FCount' then rfield.SetValue(aObject,i);
      if rfield.Name = 'FItems' then
      begin
        //if TList(aObject) <> nil then TList(aObject).Clear;
        //rfield.GetValue(aObject).AsObject.Free;// aValue.GetReferenceToRawData)
        rfield.SetValue(aObject,rValue);// .SetDynArrayProp(aObject,'fItems',Result);
        Break;
      end;
    end;
    rProp := rType.GetProperty('Count');
    rProp.SetValue(aObject,i);
    {$ENDIF}
  end;
end;
{$ENDIF}

{$IFNDEF FPC}
procedure TRTTIYaml.DeserializeXArray(Instance : TObject; aRecord : TValue; aProperty : TRttiProperty; const aPropertyName : string; aYaml : TYamlObject);
var
  ctx : TRttiContext;
  rRec : TRttiRecordType;
  rfield : TRttiField;
  rValue : TValue;
  member : TYamlPair;
  yArray : TYamlArray;
begin
  rRec := ctx.GetType(aRecord.TypeInfo).AsRecord;
  try
    rfield := rRec.GetField('fArray');
    if rfield <> nil then
    begin
      rValue := nil;
      //member := TYamlPair(aYaml.GetValue(rField.Name));
      member := GetYamlPairByName(aYaml,aPropertyName);
      if (member <> nil) and (rField.FieldType.TypeKind = tkDynArray) then
      begin
        yArray := TYamlObject.ParseYamlValue(member.ToYaml) as TYamlArray;
        try
          rValue := DeserializeDynArray(rField.FieldType.Handle,nil,yArray);
        finally
          yArray.Free;
        end;
      end;
    end;
    if not rValue.IsEmpty then rField.SetValue(aRecord.GetReferenceToRawData,rValue);
    aProperty.SetValue(Instance,aRecord);
  finally
    ctx.Free;
  end;
end;
{$ENDIF}

function TRTTIYaml.DeserializeProperty(aObject : TObject; const aName : string; aProperty : TRttiProperty; const aYaml : TYamlObject) : TObject;
var
  rValue : TValue;
  member : TYamlPair;
  objClass: TClass;
  yArray : TYamlArray;
  Yaml : TYamlObject;
begin
    Result := aObject;
    rValue := nil;
    //member := TYamlPair(aYaml.GetValue(aName));
    member := GetYamlPairByName(aYaml,aName);
    if member <> nil then
    begin
      case aProperty.PropertyType.TypeKind of
        tkDynArray :
          begin
            yArray := member.Value as TYamlArray;
            {$IFNDEF FPC}
            aProperty.SetValue(aObject,DeserializeDynArray(aProperty.PropertyType.Handle,Result,yArray));
            {$ELSE}
            DeserializeDynArray(aProperty.PropertyType.Handle,aName,Result,yArray);
            {$ENDIF}
            Exit;
          end;
        tkClass :
          begin
            //if (member.YamlValue is TYamlObject) then
            begin
              Yaml := TYamlObject(TYamlObject.ParseYamlValue(member.ToYaml));
              try
                if aProperty.GetValue(aObject).AsObject = nil then
                begin
                  {$IFNDEF FPC}
                  objClass := aProperty.PropertyType.Handle^.TypeData.ClassType;
                  rValue := DeserializeClass(objClass,Yaml);
                  {$ELSE}
                  objClass := GetObjectPropClass(aObject,aName);
                  //objClass := GetTypeData(aProperty.PropertyType.Handle)^.ClassType;
                  rValue := DeserializeClass(objClass,Yaml);
                  SetObjectProp(aObject,aName,rValue.AsObject);
                  Exit;
                  {$ENDIF}
                end
                else
                begin
                  rValue := DeserializeObject(aProperty.GetValue(aObject).AsObject,Yaml);
                  Exit;
                end;
              finally
                Yaml.Free;
              end;
            end
          end;
        {$IFNDEF FPC}
        tkRecord :
          begin
            Yaml := TYamlObject.ParseYamlValue(member.ToYaml) as TYamlObject;
            try
              rValue := DeserializeRecord(aProperty.GetValue(aObject),aObject,Yaml);
            finally
              Yaml.Free;
            end;
          end;
        tkSet :
          begin
            rValue := DeserializeType(aObject,aProperty.PropertyType.TypeKind,aProperty.GetValue(aObject).TypeInfo,member.ToYaml)
          end
        {$ENDIF}
      else
        begin
          {$IFNDEF FPC}
          //avoid return unicode escaped chars if string
          if aProperty.PropertyType.TypeKind in [tkString, tkLString, tkWString, tkUString] then
            {$IFDEF DELPHIRX103_UP}
            rValue := DeserializeType(aObject,aProperty.PropertyType.TypeKind,aProperty.GetValue(aObject).TypeInfo,member.Value.AsString)
            {$ELSE}
            rValue := DeserializeType(aObject,aProperty.PropertyType.TypeKind,aProperty.GetValue(aObject).TypeInfo,member.YamlString.ToString)
            {$ENDIF}
          else rValue := DeserializeType(aObject,aProperty.PropertyType.TypeKind,aProperty.GetValue(aObject).TypeInfo,member.Value.AsString);
          {$ELSE}
          rValue := DeserializeType(aObject,aProperty.PropertyType.TypeKind,aName,member.Value.AsString);
          if not rValue.IsEmpty then SetPropertyValue(aObject,aName,rValue);
          {$ENDIF}
        end;
      end;
      {$IFNDEF FPC}
      if not rValue.IsEmpty then aProperty.SetValue(Result,rValue);
      {$ENDIF}
    end;
end;

{$IFNDEF FPC}
function TRTTIYaml.DeserializeType(aObject : TObject; aType : TTypeKind; aTypeInfo : PTypeInfo; const aValue: string) : TValue;
var
  i : Integer;
  value : string;
  fsettings : TFormatSettings;
begin
  try
    value := AnsiDequotedStr(aValue,'"');
    case aType of
      tkString, tkLString, tkWString, tkUString :
        begin
          Result := value;
        end;
      tkChar, tkWChar :
        begin
          Result := value;
        end;
      tkInteger :
        begin
          Result := StrToInt(value);
        end;
      tkInt64 :
        begin
          Result := StrToInt64(value);
        end;
      tkFloat :
        begin
          if aTypeInfo = TypeInfo(TDateTime) then
          begin
            if CompareText(value,'null') <> 0 then Result := JsonDateToDateTime(value);
          end
          else if aTypeInfo = TypeInfo(TDate) then
          begin
            if CompareText(value,'null') <> 0 then Result := StrToDate(value);
          end
          else if aTypeInfo = TypeInfo(TTime) then
          begin
            Result := StrToTime(value);
          end
          else
          begin
            fsettings := TFormatSettings.Create;
            Result := StrToFloat(StringReplace(value,'.',fsettings.DecimalSeparator,[]));
          end;
        end;
      tkEnumeration :
        begin
          if aTypeInfo = System.TypeInfo(Boolean) then
          begin
            Result := StrToBool(value);
          end
          else
          begin
            //if fUseEnumNames then TValue.Make(GetEnumValue(aTypeInfo,value),aTypeInfo, Result)
            //  else TValue.Make(StrToInt(value),aTypeInfo, Result);
            if not TryStrToInt(value,i) then TValue.Make(GetEnumValue(aTypeInfo,value),aTypeInfo, Result)
              else TValue.Make(StrToInt(value),aTypeInfo, Result);
          end;
        end;
      tkSet :
        begin
          i := StringToSet(aTypeInfo,value);
          TValue.Make(@i,aTypeInfo,Result);
        end;
    else
        begin
          //raise EclYamlSerializerError.Create('Not supported data type!');
        end;
    end;
  except
    on E : Exception do
    begin
      raise EYamlDeserializeError.CreateFmt('Deserialize error type "%s.%s" : %s',[aObject.ClassName,GetTypeName(aTypeInfo),e.Message]);
    end;
  end;
end;
{$ELSE}
function TRTTIYaml.DeserializeType(aObject : TObject; aType : TTypeKind; const aPropertyName, aValue: string) : TValue;
var
  value : string;
  propinfo : PPropInfo;
  fsettings : TFormatSettings;
begin
  try
    value := AnsiDequotedStr(aValue,'"');

    if value = '' then
    begin
      Result := nil;
      Exit;
    end;
    propinfo := GetPropInfo(aObject,aPropertyName);
    //case propinfo.PropType.Kind of
    case aType of
      tkString, tkLString, tkWString, tkUString, tkAString :
        begin
          Result := value;
          //SetStrProp(aObject,propinfo,value);
        end;
      tkChar, tkWChar :
        begin
          Result := value;
        end;
      tkInteger :
        begin
          Result := StrToInt(value);
        end;
      tkInt64 :
        begin
          Result := StrToInt64(value);
        end;
      tkFloat :
        begin
          if propinfo.PropType = TypeInfo(TDateTime) then
          begin
            if CompareText(value,'null') <> 0  then Result := JsonDateToDateTime(value);
          end
          else if propinfo.PropType = TypeInfo(TDate) then
          begin
            if CompareText(value,'null') <> 0 then Result := StrToDate(value);
          end
          else if propinfo.PropType = TypeInfo(TTime) then
          begin
            Result := StrToTime(value);
          end
          else
          begin
            fsettings := DefaultFormatSettings;
            Result := StrToFloat(StringReplace(value,'.',fsettings.DecimalSeparator,[]));
          end;
        end;
      tkEnumeration:
        begin
          Result := value;
        end;
      tkBool :
          begin
            Result := StrToBool(value);
          end;
      tkSet :
        begin
          Result := value;
        end;
    else
        begin
          //raise EclYamlSerializerError.Create('Not supported data type!');
        end;
    end;
    //if not Result.IsEmpty then SetPropertyValue(aObject,propinfo,Result);
  except
    on E : Exception do
    begin
      raise EYamlDeserializeError.CreateFmt('Deserialize error type "%s" : %s',[aObject.ClassName,e.Message]);
    end;
  end;
end;
{$ENDIF}

function TRTTIYaml.IsAllowedProperty(aObject : TObject; const aPropertyName : string) : Boolean;
var
  propname : string;
begin
  Result := True;
  propname := aPropertyName.ToLower;
  if IsGenericList(aObject) then
  begin
    if (propname = 'capacity') or (propname = 'count') or (propname = 'ownsobjects') then Result := False;
  end
  else if (propname = 'refcount') then Result := False;
end;

function TRTTIYaml.IsGenericList(aObject : TObject) : Boolean;
var
  cname : string;
begin
  if aObject = nil then Exit(False);

  cname := aObject.ClassName;
  Result := (cname.StartsWith('TObjectList')) or (cname.StartsWith('TList'));
end;

function TRTTIYaml.IsGenericXArray(const aClassName : string) : Boolean;
begin
  Result := aClassName.StartsWith('TXArray');
end;

function TRTTIYaml.GetYamlPairByName(aYaml: TYamlObject; const aName: string): TYamlPair;
var
  candidate : TYamlPair;
  yvalue : TYamlValue;
  i : Integer;
begin
  if fUseYamlCaseSense then
  begin
    yvalue := aYaml.GetValue(aName);
    if yvalue <> nil then Result := TYamlPair(yvalue);
    Exit;
  end
  else
  begin
    if aYaml <> nil then
    for i := 0 to aYaml.Count - 1 do
    begin
      candidate := aYaml.Pairs[I];
      if (candidate = nil) or (candidate.Value = nil) then Exit(nil);
      if CompareText(candidate.Name,aName) = 0 then
        Exit(candidate);
    end;
  end;
  Result := nil;
end;

function TRTTIYaml.GetPropertyValue(Instance : TObject; const PropertyName : string) : TValue;
var
  pinfo : PPropInfo;
begin
  Result := nil;
  pinfo := GetPropInfo(Instance,PropertyName);
  case pinfo.PropType^.Kind of
    tkInteger : Result := GetOrdProp(Instance,pinfo);
    tkInt64 : Result := GetInt64Prop(Instance,PropertyName);
    tkFloat : Result := GetFloatProp(Instance,PropertyName);
    tkChar : Result := Char(GetOrdProp(Instance,PropertyName));
    {$IFDEF FPC}
    tkWString : Result := GetWideStrProp(Instance,PropertyName);
    tkSString,
    tkAString,
    {$ELSE}
    tkWString,
    {$ENDIF}
    tkLString : Result := GetStrProp(Instance,pinfo);
    {$IFDEF FPC}
    tkEnumeration :
      begin
        if fUseEnumNames then Result := GetEnumName(pinfo.PropType,GetOrdProp(Instance,PropertyName))
          else Result := GetOrdProp(Instance,PropertyName);
      end;
    {$ELSE}
    tkEnumeration :
      begin
        if fUseEnumNames then Result := GetEnumName(@pinfo.PropType,GetOrdProp(Instance,PropertyName))
          else Result := GetOrdProp(Instance,PropertyName);
      end;
    {$ENDIF}
    tkSet : Result := GetSetProp(Instance,pinfo,True);
    {$IFNDEF FPC}
    tkClass :
    {$ELSE}
    tkBool : Result := Boolean(GetOrdProp(Instance,pinfo));
    tkObject :
    {$ENDIF} Result := GetObjectProp(Instance,pinfo);
    tkDynArray : Result := GetDynArrayProp(Instance,pinfo);
  end;
end;

function TRTTIYaml.GetPropertyValueFromObject(Instance : TObject; const PropertyName : string) : TValue;
var
  ctx : TRttiContext;
  rprop : TRttiProperty;
begin
  rprop := ctx.GetType(Instance.ClassInfo).GetProperty(PropertyName);
  Result := rprop.GetValue(Instance);
end;

{$IFNDEF FPC}
function TRTTIYaml.GetFieldValueFromRecord(aValue : TValue; const FieldName : string) : TValue;
var
  ctx : TRttiContext;
  rec : TRttiRecordType;
  rfield : TRttiField;
begin
  rec := ctx.GetType(aValue.TypeInfo).AsRecord;
  rfield := rec.GetField(FieldName);
  if rfield <> nil then Result := rField.GetValue(aValue.GetReferenceToRawData)
    else Result := nil;
end;
{$ENDIF}

procedure TRTTIYaml.SetPropertyValue(Instance : TObject; const PropertyName : string; aValue : TValue);
var
  pinfo : PPropInfo;
begin
  pinfo := GetPropInfo(Instance,PropertyName);
  SetPropertyValue(Instance,pinfo,aValue);
end;

procedure TRTTIYaml.SetPropertyValue(Instance : TObject; aPropInfo : PPropInfo; aValue : TValue);
begin
  case aPropInfo.PropType^.Kind of
    tkInteger : SetOrdProp(Instance,aPropInfo,aValue.AsInteger);
    tkInt64 : SetInt64Prop(Instance,aPropInfo,aValue.AsInt64);
    tkFloat : SetFloatProp(Instance,aPropInfo,aValue.AsExtended);
    tkChar : SetOrdProp(Instance,aPropInfo,aValue.AsOrdinal);
    {$IFDEF FPC}
    tkWString : SetWideStrProp(Instance,aPropInfo,aValue.AsString);
    tkSString,
    tkAString,
    {$ELSE}
    tkWString,
    {$ENDIF}
    tkLString : SetStrProp(Instance,aPropInfo,aValue.AsString);
    {$IFDEF FPC}
    tkBool : SetOrdProp(Instance,aPropInfo,aValue.AsOrdinal);
    tkSet : LoadSetProperty(Instance,aPropInfo,aValue.AsString);
    {$ENDIF}
    tkEnumeration : SetEnumProp(Instance,aPropInfo,aValue.AsString);
    {$IFNDEF FPC}
    tkClass :
    {$ELSE}
    tkObject :
    {$ENDIF} SetObjectProp(Instance,aPropInfo,aValue.AsObject);
  end;
end;

{$IFDEF FPC}
procedure TRTTIYaml.LoadSetProperty(aInstance : TObject; aPropInfo: PPropInfo; const aValue: string);
type
  TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;
const
  Delims = [' ', ',', '[', ']'];
var
  TypeInfo: PTypeInfo;
  W: Cardinal;
  I, N: Integer;
  Count: Integer;
  EnumName: string;
begin
  W := 0;
  TypeInfo := GetTypeData(GetPropType(aPropInfo))^.CompType;
  Count := WordCount(aValue, Delims);
  for N := 1 to Count do
  begin
    EnumName := ExtractWord(N, aValue, Delims);
    try
      I := GetEnumValue(TypeInfo, EnumName);
      if I >= 0 then Include(TCardinalSet(W),I);
    except
    end;
  end;
  SetOrdProp(aInstance,aPropInfo,W);
end;
{$ENDIF}

function TRTTIYaml.Serialize(aObject: TObject): TYamlObject;
var
  ctx: TRttiContext;
  {$IFNDEF FPC}
  attr : TCustomAttribute;
  comment : string;
  {$ENDIF}
  rType: TRttiType;
  rProp: TRttiProperty;
  ypair : TYamlPair;
  ExcludeSerialize : Boolean;
  propertyname : string;
begin
  if (aObject = nil) then
  begin
    Result := nil;
    Exit;
  end;

  Result := TYamlObject.Create;
  try
    rType := ctx.GetType(aObject.ClassInfo);
    try
      //s := rType.ToString;
      for rProp in TRTTI.GetProperties(rType,roFirstBase) do
      begin
        ExcludeSerialize := False;
        propertyname := rProp.Name;
        {$IFNDEF FPC}
        comment := '';
        for attr in rProp.GetAttributes do
        begin
          if attr is TNotSerializableProperty then ExcludeSerialize := True
          else if attr is TCommentProperty then comment := TCommentProperty(attr).Comment
          else if  attr is TCustomNameProperty then propertyname := TCustomNameProperty(attr).Name;
        end;
        if ((fSerializeLevel = slPublicProperty) and (rProp.PropertyType.IsPublicType))
            or ((fSerializeLevel = slPublishedProperty) and ((IsPublishedProp(aObject,rProp.Name)) or (rProp.Name = 'List'))) then
        {$ENDIF}
        begin
          if (IsAllowedProperty(aObject,propertyname)) and (not ExcludeSerialize) then
          begin
            //add comment as pair
            {$IFNDEF FPC}
            if comment <> '' then Result.AddPair(TYamlPair.Create('#',TYamlComment.Create(Comment)));
            {$ENDIF}
            begin
              if (rProp.GetValue(aObject).IsObject) and (IsGenericList(rProp.GetValue(aObject).AsObject)) then
              begin
                ypair := Serialize(propertyname,GetPropertyValueFromObject(rProp.GetValue(aObject).AsObject,'List'));
              end
              {$IFNDEF FPC}
              else if (not rProp.GetValue(aObject).IsObject) and (IsGenericXArray(rProp.GetValue(aObject){$IFNDEF NEXTGEN}.TypeInfo.Name{$ELSE}.TypeInfo.NameFld.ToString{$ENDIF})) then
              begin
                ypair := Serialize(propertyname,GetFieldValueFromRecord(rProp.GetValue(aObject),'fArray'));
              end
              {$ENDIF}
              else
              begin
                {$IFNDEF FPC}
                ypair := Serialize(propertyname,rProp.GetValue(aObject));
                {$ELSE}
                ypair := Serialize(aObject,rProp.PropertyType.TypeKind,propertyname);
                {$ENDIF}
              end;
              //s := jpair.YamlValue.ToString;
              if ypair <> nil then
              begin
                Result.AddPair(ypair);
              end
              else ypair.Free;
            end;
            //Result.AddPair(Serialize(rProp.Name,rProp.GetValue(aObject)));
            //s := Result.ToYaml;
          end;
        end;
      end;
    finally
      ctx.Free;
    end;
  except
    on E : Exception do
    begin
      Result.Free;
      raise EYamlSerializeError.CreateFmt('Serialize error object "%s" : %s',[aObject.ClassName,e.Message]);
    end;
  end;
end;

function TRTTIYaml.GetValue(aAddr: Pointer; aType: TRTTIType): TValue;
begin
  TValue.Make(aAddr,aType.Handle,Result);
end;

function TRTTIYaml.GetValue(aAddr: Pointer; aTypeInfo: PTypeInfo): TValue;
begin
  TValue.Make(aAddr,aTypeInfo,Result);
end;

{$IFNDEF FPC}
function TRTTIYaml.Serialize(const aName : string; aValue : TValue) : TYamlPair;
var
  ctx: TRttiContext;
  rRec : TRttiRecordType;
  rField : TRttiField;
  rDynArray : TRTTIDynamicArrayType;
  Yaml : TYamlObject;
  yArray : TYamlArray;
  ypair : TYamlPair;
  yvalue : TYamlValue;
  i : Integer;
begin
  Result := TYamlPair.Create(aName,nil);
  //Result.YamlString := TYamlString(aName);
  try
    case avalue.Kind of
      tkDynArray :
        begin
          yArray := TYamlArray.Create;
          rDynArray := ctx.GetType(aValue.TypeInfo) as TRTTIDynamicArrayType;
          try
            for i := 0 to aValue.GetArrayLength - 1 do
            begin
              if not GetValue(PPByte(aValue.GetReferenceToRawData)^ + rDynArray.ElementType.TypeSize * i, rDynArray.ElementType).IsEmpty then
              begin
                yvalue := nil;
                ypair := Serialize(aName,GetValue(PPByte(aValue.GetReferenceToRawData)^ + rDynArray.ElementType.TypeSize * i, rDynArray.ElementType));
                try
                  //jValue := TYamlValue(jPair.YamlValue.Clone);
                  yvalue := ypair.Value;
                  if yvalue <> nil then
                  begin
                    yArray.AddElement(yvalue);
                    ypair.Value.Owned := False;
                  end;
                finally
                  ypair.Free;
                  if yvalue <> nil then yvalue.Owned := True;
                end;
              end;
            end;
            Result.Value := yArray;
          finally
            ctx.Free;
          end;
        end;
      tkClass :
        begin
           Result.Value := TYamlValue(Serialize(aValue.AsObject));
        end;
      tkString, tkLString, tkWString, tkUString :
        begin
          Result.Value := TYamlString.Create(aValue.AsString);
        end;
      tkChar, tkWChar :
        begin
          Result.Value := TYamlString.Create(aValue.AsString);
        end;
      tkInteger :
        begin
          Result.Value := TYamlInteger.Create(aValue.AsInteger);
        end;
      tkInt64 :
        begin
          Result.Value := TYamlInteger.Create(aValue.AsInt64);
        end;
      tkFloat :
        begin
          if aValue.TypeInfo = TypeInfo(TDateTime) then
          begin
            if aValue.AsExtended <> 0.0 then Result.Value := TYamlString.Create(DateTimeToJsonDate(aValue.AsExtended));
          end
          else if aValue.TypeInfo = TypeInfo(TDate) then
          begin
            if aValue.AsExtended <> 0.0 then Result.Value := TYamlString.Create(DateToStr(aValue.AsExtended));
          end
          else if aValue.TypeInfo = TypeInfo(TTime) then
          begin
            Result.Value := TYamlString.Create(TimeToStr(aValue.AsExtended));
          end
          else
          begin
            Result.Value := TYamlFloat.Create(aValue.AsExtended);
          end;
        end;
      tkEnumeration :
        begin
          if (aValue.TypeInfo = System.TypeInfo(Boolean)) then
          begin
            Result.Value := TYamlBoolean.Create(aValue.AsBoolean);
          end
          else
          begin
            //Result.YamlValue := TYamlString.Create(GetEnumName(aValue.TypeInfo,aValue.AsOrdinal));
            if fUseEnumNames then Result.Value := TYamlString.Create(aValue.ToString)
              else Result.Value := TYamlInteger.Create(GetEnumValue(aValue.TypeInfo,aValue.ToString));
          end;
        end;
      tkSet :
        begin
          Result.Value := TYamlString.Create(aValue.ToString);
        end;
      tkRecord :
        begin
          rRec := ctx.GetType(aValue.TypeInfo).AsRecord;
          try
            Yaml := TYamlObject.Create;
            for rField in rRec.GetFields do
            begin
              Yaml.AddPair(Serialize(rField.name,rField.GetValue(aValue.GetReferenceToRawData)));
            end;
            Result.Value := Yaml;
          finally
            ctx.Free;
          end;
        end;
      tkMethod, tkPointer, tkClassRef ,tkInterface, tkProcedure :
        begin
          //skip these properties
          //FreeAndNil(Result);
        end
    else
      begin
        raise EYamlSerializeError.CreateFmt(cNotSupportedDataType,[aName,GetTypeName(aValue.TypeInfo)]);
      end;
    end;
    if Result.Value = nil then Result.Value := TYamlNull.Create;
  except
    on E : Exception do
    begin
      Result.Free;
      raise EYamlSerializeError.CreateFmt('Serialize error class "%s.%s" : %s',[aName,aValue.ToString,e.Message]);
    end;
  end;
end;
{$ELSE}
function TRTTIYaml.GetPropType(aPropInfo: PPropInfo): PTypeInfo;
begin
  Result := aPropInfo^.PropType;
end;

function TRTTIYaml.FloatProperty(aObject : TObject; aPropInfo: PPropInfo): string;
const
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 19);
var
  fsettings : TFormatSettings;
begin
  fsettings := FormatSettings;
  Result := StringReplace(FloatToStrF(GetFloatProp(aObject, aPropInfo), ffGeneral,
    Precisions[GetTypeData(GetPropType(aPropInfo))^.FloatType],0),
    '.',fsettings.DecimalSeparator,[rfReplaceAll]);
end;

function TRTTIYaml.Serialize(const aName : string; aValue : TValue) : TYamlPair;
begin
  Result := TYamlPair.Create(aName,nil);
  //Result.YamlString := TYamlString(aName);
  try
    case avalue.Kind of
      tkClass :
        begin
           Result.Value := TYamlValue(Serialize(aValue.AsObject));
        end;
      tkString, tkLString, tkWString, tkUString :
        begin
          Result.Value := TYamlString.Create(aValue.AsString);
        end;
      tkChar, tkWChar :
        begin
          Result.Value := TYamlString.Create(aValue.AsString);
        end;
      tkInteger :
        begin
          Result.Value := TYamlInteger.Create(aValue.AsInteger);
        end;
      tkInt64 :
        begin
          Result.Value := TYamlInteger.Create(aValue.AsInt64);
        end;
      tkFloat :
        begin
          if aValue.TypeInfo = TypeInfo(TDateTime) then
          begin
            if aValue.AsExtended <> 0.0 then Result.Value := TYamlString.Create(DateTimeToJsonDate(aValue.AsExtended));
          end
          else if aValue.TypeInfo = TypeInfo(TDate) then
          begin
            if aValue.AsExtended <> 0.0 then Result.Value := TYamlString.Create(DateToStr(aValue.AsExtended));
          end
          else if aValue.TypeInfo = TypeInfo(TTime) then
          begin
            Result.Value := TYamlString.Create(TimeToStr(aValue.AsExtended));
          end
          else
          begin
            Result.Value := TYamlFloat.Create(aValue.AsExtended);
          end;
        end;
      tkEnumeration :
        begin
          if (aValue.TypeInfo = System.TypeInfo(Boolean)) then
          begin
            Result.Value := TYamlBoolean.Create(aValue.AsBoolean);
          end
          else
          begin
            //Result.YamlValue := TYamlString.Create(GetEnumName(aValue.TypeInfo,aValue.AsOrdinal));
            if fUseEnumNames then Result.Value := TYamlString.Create(aValue.ToString)
              else Result.Value := TYamlInteger.Create(GetEnumValue(aValue.TypeInfo,aValue.ToString));
          end;
        end;
      tkSet :
        begin
          Result.Value := TYamlString.Create(aValue.ToString);
        end;
    else
      begin
        //raise EYamlDeserializeError.CreateFmt('Not supported type "%s":%d',[aName,Integer(aValue.Kind)]);
      end;
    end;
    if Result.Value = nil then Result.Value := TYamlNull.Create;
  except
    Result.Free;
  end;
end;

function TRTTIYaml.Serialize(aObject : TObject; aType : TTypeKind; const aPropertyName : string) : TYamlPair;
var
  propinfo : PPropInfo;
  yArray : TYamlArray;
  ypair : TYamlPair;
  yvalue : TYamlValue;
  i : Integer;
  pArr : Pointer;
  rValue : TValue;
  rItemValue : TValue;
  len : Integer;
begin
  try
    Result := TYamlPair.Create(aPropertyName,nil);

    propinfo := GetPropInfo(aObject,aPropertyName);
    //case propinfo.PropType.Kind of
    case aType of
      tkDynArray :
        begin
          len := 0;
          yArray := TYamlArray.Create;
          try
            pArr := GetDynArrayProp(aObject,aPropertyName);
            TValue.Make(@pArr,propinfo.PropType, rValue);
            if rValue.IsArray then
            begin
              len := rValue.GetArrayLength;
              for i := 0 to len - 1 do
              begin
                rItemValue := rValue.GetArrayElement(i);
                ypair := Serialize(aPropertyName,rItemValue);
                try
                  //jValue := TYamlValue(jPair.YamlValue.Clone);
                  yvalue := ypair.Value;
                  yArray.AddElement(yvalue);
                  //jPair.YamlValue.Owned := False;
                finally
                  ypair.Free;
                  //jValue.Owned := True;
                end;
              end;
            end;
            Result.Value := yArray;
          finally
            //DynArrayClear(pArr,propinfo.PropType);
            pArr := nil;
          end;
        end;
      tkClass :
        begin
          Result.Value := TYamlValue(Serialize(GetObjectProp(aObject,aPropertyName)));
        end;
      tkString, tkLString, tkWString, tkUString, tkAString :
        begin
          Result.Value := TYamlString.Create(GetStrProp(aObject,aPropertyName));
        end;
      tkChar, tkWChar :
        begin
          Result.Value := TYamlString.Create(Char(GetOrdProp(aObject,aPropertyName)));
        end;
      tkInteger :
        begin
          Result.Value := TYamlInteger.Create(GetOrdProp(aObject,aPropertyName));
        end;
      tkInt64 :
        begin
          Result.Value := TYamlInteger.Create(GetOrdProp(aObject,aPropertyName));
        end;
      tkFloat :
        begin
          if propinfo.PropType = TypeInfo(TDateTime) then
          begin
            if aValue.AsExtended <> 0.0 then Result.Value := TYamlString.Create(DateTimeToJsonDate(GetFloatProp(aObject,aPropertyName)));
          end
          else if propinfo.PropType = TypeInfo(TDate) then
          begin
            if aValue.AsExtended <> 0.0 then Result.Value := TYamlString.Create(DateToStr(GetFloatProp(aObject,aPropertyName)));
          end
          else if propinfo.PropType = TypeInfo(TTime) then
          begin
            Result.Value := TYamlString.Create(TimeToStr(GetFloatProp(aObject,aPropertyName)));
          end
          else
          begin
            //Result.YamlValue := TYamlFloatNumber.Create(GetFloatProp(aObject,aPropertyName));
            Result.Value := TYamlFloat.Create(StrToFloat(FloatProperty(aObject,propinfo)));
          end;
        end;
      tkEnumeration,tkBool :
        begin
          if (propinfo.PropType = System.TypeInfo(Boolean)) then
          begin
            Result.Value := TYamlBoolean.Create(Boolean(GetOrdProp(aObject,aPropertyName)));
          end
          else
          begin
            if fUseEnumNames then Result.Value := TYamlString.Create(GetEnumName(propinfo.PropType,GetOrdProp(aObject,aPropertyName)))
              else Result.Value := TYamlInteger.Create(GetOrdProp(aObject,aPropertyName));
            //Result.YamlValue := TYamlString.Create(aValue.ToString);
          end;
        end;
      tkSet :
        begin
          Result.Value := TYamlString.Create(GetSetProp(aObject,aPropertyName));
        end;
      {$IFNDEF FPC}
      tkRecord :
        begin
          rRec := ctx.GetType(aValue.TypeInfo).AsRecord;
          try
            Yaml := TYamlObject.Create;
            for rField in rRec.GetFields do
            begin
              Yaml.AddPair(Serialize(rField.name,rField.GetValue(aValue.GetReferenceToRawData)));
            end;
            Result.YamlValue := Yaml;
          finally
            ctx.Free;
          end;
        end;
      {$ENDIF}
      tkMethod, tkPointer, tkClassRef ,tkInterface, tkProcedure :
        begin
          //skip these properties
          //FreeAndNil(Result);
        end
    else
      begin

        //raise EYamlDeserializeError.CreateFmt('Not supported type "%s":%d',[aName,Integer(aValue.Kind)]);
      end;
    end;
    if Result.Value = nil then Result.Value := TYamlNull.Create;
  except
    on E : Exception do
    begin
      Result.Free;
      {$IFNDEF FPC}
      raise EYamlSerializeError.CreateFmt('Serialize error class "%s.%s" : %s',[aName,aValue.ToString,e.Message]);
      {$ENDIF}
    end;
  end;
end;
{$ENDIF}


{ TYamlSerializer}

constructor TYamlSerializer.Create(aSerializeLevel: TSerializeLevel; aUseEnumNames : Boolean = True);
begin
  {$IFDEF FPC}
  if aSerializeLevel = TSerializeLevel.slPublicProperty then raise EYamlSerializeError.Create('FreePascal RTTI only supports published properties');
  {$ENDIF}
  fSerializeLevel := aSerializeLevel;
  fUseEnumNames := aUseEnumNames;
  fUseYamlCaseSense := False;
  fRTTIYaml := TRTTIYaml.Create(aSerializeLevel,aUseEnumNames);
  fRTTIYaml.UseYamlCaseSense := fUseYamlCaseSense;
end;

function TYamlSerializer.YamlToObject(aType: TClass; const aYaml: string): TObject;
var
  Yaml: TYamlObject;
begin
  Result := nil;
  Yaml := TYamlObject.ParseYamlValue(aYaml) as TYamlObject;
  try
    Result := fRTTIYaml.DeserializeClass(aType,Yaml);
  finally
    Yaml.Free;
  end;
end;

destructor TYamlSerializer.Destroy;
begin
  fRTTIYaml.Free;
  inherited;
end;

function TYamlSerializer.YamlToObject(aObject: TObject; const aYaml: string): TObject;
var
  Yaml: TYamlObject;
begin
  Result := aObject;
  Yaml := TYamlObject(TYamlObject.ParseYamlValue(aYaml));
  try
    fRTTIYaml.DeserializeObject(aObject,Yaml);
  finally
    Yaml.Free;
  end;
end;

function TYamlSerializer.ObjectToYaml(aObject : TObject): string;
var
  Yaml: TYamlObject;
begin
  Yaml := fRTTIYaml.Serialize(aObject);
  try
    Result := Yaml.ToYaml;
  finally
    Yaml.Free;
  end;
end;

procedure TYamlSerializer.SetUseEnumNames(const Value: Boolean);
begin
  fUseEnumNames := Value;
  if Assigned(fRTTIYaml) then fRTTIYaml.UseEnumNames := Value;
end;

procedure TYamlSerializer.SetUseYamlCaseSense(const Value: Boolean);
begin
  fUseYamlCaseSense := Value;
  if Assigned(fRTTIYaml) then fRTTIYaml.UseYamlCaseSense := Value;
end;

{$IFNDEF FPC}
{ TCommentProperty }

constructor TCommentProperty.Create(const aComment: string);
begin
  fComment := aComment;
end;

{ TCustomNameProperty }

constructor TCustomNameProperty.Create(const aName: string);
begin
  fName := aName;
end;
{$ENDIF}


end.




