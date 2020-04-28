{ ***************************************************************************

  Copyright (c) 2015-2020 Kike Pérez

  Unit        : Quick.JSON.Serializer
  Description : Json Serializer
  Author      : Kike Pérez
  Version     : 1.11
  Created     : 21/05/2018
  Modified    : 27/04/2020

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

unit Quick.Json.Serializer;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  TypInfo,
  {$IFDEF FPC}
   rttiutils,
   fpjson,
   jsonparser,
   strUtils,
   //jsonreader,
   //fpjsonrtti,
   Quick.Json.fpc.Compatibility,  
  {$ELSE}
    {$IFDEF DELPHIXE7_UP}
    System.Json,
    {$ELSE}
    Data.DBXJSON,
    {$ENDIF}
    {$IFDEF DELPHIRX10_UP}
   
    {$ENDIF}
    Variants,
  {$ENDIF}
  Generics.Collections,
  Quick.RTTI.Utils,
  DateUtils,
  Quick.Commons,
  Quick.JSON.Utils;

type

  EJsonSerializeError = class(Exception);
  EJsonDeserializeError = class(Exception);

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
    {$IFNDEF DELPHIXE7_UP}
     TJSONArrayHelper = class helper for Data.DBXJson.TJSONArray
      private
        function GetItem(aValue : Integer) : TJSONValue;
      public
        function Count : Integer;
        property Items[index : Integer] : TJSONValue read GetItem;
        procedure SetElements(aElements : TList<TJSONValue>);
      end;

      TJSONValueHelper = class helper for Data.DBXJson.TJSONValue
      public
        function ToJson : string;
      end;

      TJSONObjectHelper = class helper for Data.DBXJson.TJSONObject
      private
        function GetPair(aValue : Integer) : TJSONPair;
      public
        function Count : Integer;
        function GetValue(const aName : string) : TJSONValue;
        property Pairs[index : Integer] : TJSONPair read GetPair;
      end;
    {$ENDIF}
  {$ENDIF}

  IJsonSerializer = interface
  ['{CA26F7AE-F1FE-41BE-9C23-723A687F60D1}']
    function JsonToObject(aType: TClass; const aJson: string): TObject; overload;
    function JsonToObject(aObject: TObject; const aJson: string): TObject; overload;
    function ObjectToJson(aObject : TObject; aIndent : Boolean = False): string;
    function ValueToJson(const aValue : TValue; aIndent : Boolean = False) : string;
  end;

  TSerializeLevel = (slPublicProperty, slPublishedProperty);

  TRTTIJson = class
  private
    fSerializeLevel : TSerializeLevel;
    fUseEnumNames : Boolean;
    fUseJsonCaseSense : Boolean;
    function GetValue(aAddr: Pointer; aType: TRTTIType): TValue; overload;
    function GetValue(aAddr: Pointer; aTypeInfo: PTypeInfo): TValue; overload;
    function IsAllowedProperty(aObject : TObject; const aPropertyName : string) : Boolean;
    function IsGenericList(aObject : TObject) : Boolean;
    function IsGenericXArray(const aClassName : string) : Boolean;
    function GetPropertyValue(Instance : TObject; const PropertyName : string) : TValue;
    function GetPropertyValueFromObject(Instance : TObject; const PropertyName : string) : TValue;
    {$IFNDEF FPC}
    function GetFieldValueFromRecord(const aValue : TValue; const FieldName : string) : TValue;
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
    property UseJsonCaseSense : Boolean read fUseJsonCaseSense write fUseJsonCaseSense;
    function GetJsonPairValueByName(aJson : TJSONObject; const aName : string) : TJsonValue;
    function GetJsonPairByName(aJson : TJSONObject; const aName : string) : TJSONPair;
    //serialize methods
    function SerializeValue(const aValue : TValue) : TJSONValue;
    function SerializeObject(aObject : TObject) : TJSONObject; overload;
    {$IFNDEF FPC}
    function SerializeDynArray(const aValue: TValue) : TJsonArray;
    function SerializeRecord(const aValue : TValue) : TJSONValue;
    {$ELSE}
    function SerializeObject(aObject : TObject; aType : TTypeKind; const aPropertyName : string) : TJSONPair;
    {$ENDIF}
    //deserialize methods
    function DeserializeClass(aType : TClass; const aJson : TJSONObject) : TObject;
    function DeserializeObject(aObject : TObject; const aJson : TJSONObject) : TObject; overload;
    function DeserializeProperty(aObject : TObject; const aName : string; aProperty : TRttiProperty; const aJson : TJSONObject) : TObject; overload;
    {$IFNDEF FPC}
    function DeserializeType(aObject : TObject; aType : TTypeKind; aTypeInfo : PTypeInfo; const aValue: string) : TValue;
    function DeserializeDynArray(aTypeInfo : PTypeInfo; aObject : TObject; const aJsonArray: TJSONArray) : TValue;
    function DeserializeRecord(const aRecord : TValue; aObject : TObject; const aJson : TJSONObject) : TValue;
    function DeserializeList(aObject: TObject; const aName : string; const aJson: TJSONObject) : TObject;
    procedure DeserializeXArray(Instance : TObject; aRecord : TValue; aProperty : TRttiProperty; const aPropertyName : string; aJson : TJsonObject);
    {$ELSE}
    function DeserializeType(aObject : TObject; aType : TTypeKind; const aPropertyName, aValue: string) : TValue;
    procedure DeserializeDynArray(aTypeInfo: PTypeInfo; const aPropertyName : string; aObject: TObject; const aJsonArray: TJSONArray);
    {$ENDIF}
  end;

  TJsonSerializer = class(TInterfacedObject,IJsonSerializer)
  strict private
    fSerializeLevel : TSerializeLevel;
    fUseEnumNames : Boolean;
    fUseJsonCaseSense : Boolean;
    fRTTIJson : TRTTIJson;
  private
    procedure SetUseEnumNames(const Value: Boolean);
    procedure SetUseJsonCaseSense(const Value: Boolean);
  public
    constructor Create(aSerializeLevel: TSerializeLevel; aUseEnumNames : Boolean = True);
    destructor Destroy; override;
    property SerializeLevel : TSerializeLevel read fSerializeLevel;
    property UseEnumNames : Boolean read fUseEnumNames write SetUseEnumNames;
    property UseJsonCaseSense : Boolean read fUseJsonCaseSense write SetUseJsonCaseSense;
    function JsonToObject(aType : TClass; const aJson: string) : TObject; overload;
    function JsonToObject(aObject : TObject; const aJson: string) : TObject; overload;
    function ObjectToJson(aObject : TObject; aIndent : Boolean = False): string;
    function ObjectToJsonString(aObject : TObject; aIndent : Boolean = False): string;
    function ValueToJson(const aValue : TValue; aIndent : Boolean = False) : string;
    function ValueToJsonString(const aValue : TValue; aIndent : Boolean = False) : string;
    function ArrayToJson<T>(aArray : TArray<T>; aIndent : Boolean = False) : string;
    function ArrayToJsonString<T>(aArray : TArray<T>; aIndent : Boolean = False) : string;
    {$IFNDEF FPC}
    function JsonToArray<T>(const aJson : string) : TArray<T>;
    function JsonToValue(const aJson: string): TValue;
    {$ENDIF}
  end;

  EJsonSerializerError = class(Exception);

  PPByte = ^PByte;

resourcestring
  cNotSupportedDataType = 'Not supported data type "%s"';
  cSerializeObjectError = 'Serialize object "%s" error: %s';
  cSerializePropertyError = 'Property "%s" ("%s")';
  cNotSerializable = 'Object is not serializable';
  cNotValidJson = 'Not a valid Json';

implementation

{ TRTTIJson }

{$IFNDEF FPC}
function TRTTIJson.DeserializeDynArray(aTypeInfo: PTypeInfo; aObject: TObject; const aJsonArray: TJSONArray) : TValue;
var
  rType: PTypeInfo;
  len: NativeInt;
  pArr: Pointer;
  rItemValue: TValue;
  i: Integer;
  objClass: TClass;
  ctx : TRttiContext;
  json : TJSONObject;
  rDynArray : TRttiDynamicArrayType;
  propObj : TObject;
begin
  if GetTypeData(aTypeInfo).DynArrElType = nil then Exit;
  if not assigned(aJsonArray) then Exit;

  len := aJsonArray.Count;
  rType := GetTypeData(aTypeInfo).DynArrElType^;
  pArr := nil;
  DynArraySetLength(pArr,aTypeInfo, 1, @len);
  try
    TValue.Make(@pArr,aTypeInfo, Result);
    rDynArray := ctx.GetType(Result.TypeInfo) as TRTTIDynamicArrayType;

    for i := 0 to aJsonArray.Count - 1 do
    begin
      rItemValue := nil;
      case rType.Kind of
        tkClass :
          begin
            if aJsonArray.Items[i] is TJSONObject then
            begin
              propObj := GetValue(PPByte(Result.GetReferenceToRawData)^ +rDynArray.ElementType.TypeSize * i, rDynArray.ElementType).AsObject;
              if propObj = nil then
              begin
                objClass := rType.TypeData.ClassType;
                rItemValue := DeserializeClass(objClass, TJSONObject(aJsonArray.Items[i]));
              end
              else
              begin
                DeserializeObject(propObj,TJSONObject(aJsonArray.Items[i]));
              end;
            end;
          end;
        tkRecord :
          begin
            json := TJSONObject(aJsonArray.Items[i]);
            rItemValue := DeserializeRecord(GetValue(PPByte(Result.GetReferenceToRawData)^ +rDynArray.ElementType.TypeSize * i,
                                            rDynArray.ElementType),aObject,json);
          end;
        tkMethod, tkPointer, tkClassRef ,tkInterface, tkProcedure :
          begin
            //skip these properties
          end
      else
        begin
          rItemValue := DeserializeType(aObject,rType.Kind,aTypeInfo,aJsonArray.Items[i].Value);
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
procedure TRTTIJson.DeserializeDynArray(aTypeInfo: PTypeInfo; const aPropertyName : string; aObject: TObject; const aJsonArray: TJSONArray);
var
  rType: PTypeInfo;
  len: NativeInt;
  pArr: Pointer;
  rItemValue: TValue;
  i: Integer;
  objClass: TClass;
  propObj : TObject;
  rValue : TValue;
begin
  if GetTypeData(aTypeInfo).ElType2 = nil then Exit;
  len := aJsonArray.Count;
  rType := GetTypeData(aTypeInfo).ElType2;
  pArr := nil;
  DynArraySetLength(pArr,aTypeInfo, 1, @len);
  try
    TValue.Make(@pArr,aTypeInfo, rValue);
    for i := 0 to aJsonArray.Count - 1 do
    begin
      rItemValue := nil;
      case rType.Kind of
        tkClass :
          begin
            if aJsonArray.Items[i] is TJSONObject then
            begin
              propObj := GetValue(PPByte(rValue.GetReferenceToRawData)^ +GetTypeData(aTypeInfo).elSize * i, GetTypeData(aTypeInfo).ElType2).AsObject;
              if propObj = nil then
              begin
                objClass := GetTypeData(aTypeInfo).ClassType;
                rItemValue := DeserializeClass(objClass, TJSONObject(aJsonArray.Items[i]));
              end
              else
              begin
                DeserializeObject(propObj,TJSONObject(aJsonArray.Items[i]));
              end;
            end;
          end;
        tkRecord :
          begin
            {json := TJSONObject(aJsonArray.Items[i]);
            rItemValue := DeserializeRecord(GetValue(PPByte(Result.GetReferenceToRawData)^ +rDynArray.ElementType.TypeSize * i,
                                            rDynArray.ElementType),aObject,json);  }
          end;
        tkMethod, tkPointer, tkClassRef ,tkInterface, tkProcedure :
          begin
            //skip these properties
          end
      else
        begin
          rItemValue := DeserializeType(aObject,GetTypeData(aTypeInfo).ElType2.Kind,aPropertyName,aJsonArray.Items[i].Value);
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
function TRTTIJson.DeserializeRecord(const aRecord : TValue; aObject : TObject; const aJson : TJSONObject) : TValue;
var
  ctx : TRttiContext;
  rRec : TRttiRecordType;
  rField : TRttiField;
  rValue : TValue;
  member : TJsonValue;
  jArray : TJSONArray;
  json : TJSONObject;
  objClass : TClass;
  propobj : TObject;
begin
  rRec := ctx.GetType(aRecord.TypeInfo).AsRecord;
  for rField in rRec.GetFields do
  begin
    rValue := nil;
    //member := TJSONPair(aJson.GetValue(rField.Name));
    member := GetJsonPairValueByName(aJson,rField.Name);
    if member <> nil then
    case rField.FieldType.TypeKind of
      tkDynArray :
        begin
          jArray := TJSONObject.ParseJSONValue(member.ToJSON) as TJSONArray;
          try
            rValue := DeserializeDynArray(rField.FieldType.Handle,aObject,jArray);
          finally
            jArray.Free;
          end;
        end;
      tkClass :
        begin
          //if (member.JsonValue is TJSONObject) then
          begin
            propobj := rField.GetValue(@aRecord).AsObject;
            json := TJSONObject.ParseJSONValue(member.ToJson) as TJSONObject;
            try
              if propobj = nil then
              begin
                objClass := rField.FieldType.Handle^.TypeData.ClassType;// aProperty.PropertyType.Handle^.TypeData.ClassType;
                rValue := DeserializeClass(objClass,json);
              end
              else
              begin
                DeserializeObject(propobj,json);
              end;
            finally
              json.Free;
            end;
          end
        end;
      tkRecord :
        begin
          json := TJSONObject.ParseJSONValue(member.ToJson) as TJSONObject;
          try
            rValue := DeserializeRecord(rField.GetValue(aRecord.GetReferenceToRawData),aObject,json);
          finally
            json.Free;
          end;
        end
    else
      begin
        //rValue := DeserializeType(aObject,rField.FieldType.TypeKind,rField.FieldType.Handle,member.ToJson);
        //avoid return unicode escaped chars if string
        if rField.FieldType.TypeKind in [tkString, tkLString, tkWString, tkUString] then
          {$IFDEF DELPHIRX10_UP}
          rValue := DeserializeType(aObject,rField.FieldType.TypeKind,rField.FieldType.Handle,TJsonValue(member).value)
          {$ELSE}
          rValue := DeserializeType(aObject,rField.FieldType.TypeKind,rField.FieldType.Handle,member.Value)
          {$ENDIF}
          else rValue := DeserializeType(aObject,rField.FieldType.TypeKind,rField.FieldType.Handle,member.ToJSON);
      end;
    end;
    if not rValue.IsEmpty then rField.SetValue(aRecord.GetReferenceToRawData,rValue);
  end;
  Result := aRecord;
end;
{$ENDIF}

constructor TRTTIJson.Create(aSerializeLevel : TSerializeLevel; aUseEnumNames : Boolean = True);
begin
  fSerializeLevel := aSerializeLevel;
  fUseEnumNames := aUseEnumNames;
  fUseJsonCaseSense := False;
end;

function TRTTIJson.DeserializeClass(aType: TClass; const aJson: TJSONObject): TObject;
begin
  Result := nil;
  if (aJson = nil) or ((aJson as TJSONValue) is TJSONNull) or (aJson.Count = 0) then Exit;

  Result := aType.Create;
  try
    Result := DeserializeObject(Result,aJson);
  except
    on E : Exception do
    begin
      Result.Free;
      raise EJsonDeserializeError.CreateFmt('Deserialize error class "%s" : %s',[aType.ClassName,e.Message]);
    end;
  end;
end;

function TRTTIJson.DeserializeObject(aObject: TObject; const aJson: TJSONObject): TObject;
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

  if (aJson = nil) or ((aJson as TJSONValue) is TJSONNull) or (aJson.Count = 0) or (Result = nil) then Exit;

  try
    rType := ctx.GetType(aObject.ClassInfo);
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
            Result := DeserializeList(Result,propertyname,aJson);
          end
          else if (rProp.GetValue(aObject).IsObject) and (IsGenericList(rProp.GetValue(aObject).AsObject)) then
          begin
            DeserializeList(rProp.GetValue(aObject).AsObject,'List',TJSONObject(aJson.GetValue(propertyname)));
          end
          else if (not rProp.GetValue(aObject).IsObject) and (IsGenericXArray(rProp.GetValue(aObject){$IFNDEF NEXTGEN}.TypeInfo.Name{$ELSE}.TypeInfo.NameFld.ToString{$ENDIF})) then
          begin
            DeserializeXArray(Result,rProp.GetValue(aObject),rProp,propertyname,aJson);
          end
          else
          {$ENDIF}
          Result := DeserializeProperty(Result,propertyname,rProp,aJson);
        end;
      end;
    end;
  except
    on E : Exception do
    begin
      Result.Free;
      raise EJsonDeserializeError.CreateFmt('Deserialize error for object "%s" : %s',[aObject.ClassName,e.Message]);
    end;
  end;
end;

{$IFNDEF FPC}
function TRTTIJson.DeserializeList(aObject: TObject; const aName : string; const aJson: TJSONObject) : TObject;
var
  ctx : TRttiContext;
  rType : TRttiType;
  jarray : TJSONArray;
  member : TJsonValue;
  rvalue : TValue;
  i : Integer;
  n : Integer;
  rProp : TRttiProperty;
  {$IFNDEF DELPHIRX10_UP}
  rfield : TRttiField;
  {$ENDIF}
begin
  Result := aObject;

  rType := ctx.GetType(aObject.ClassInfo);
  rProp := rType.GetProperty('List');
  if rProp = nil then Exit;

  //check if exists List (denotes delphi json serialized) or not (normal json serialized)
  member := GetJsonPairValueByName(aJson,aName);
  if member = nil then jArray := TJSONObject.ParseJSONValue(aJson.ToJSON) as TJSONArray
    else jArray := TJSONObject.ParseJSONValue(member.ToJSON) as TJSONArray;
  try
    rvalue := DeserializeDynArray(rProp.PropertyType.Handle,Result,jArray);
    //i := jarray.Count;
  finally
    jArray.Free;
  end;

  if not rValue.IsEmpty then
  begin
    {$IFDEF DELPHIRX10_UP}
    if (TObjectList<TObject>(aObject) <> nil) and (rvalue.IsArray) then
    begin
      TObjectList<TObject>(aObject).Clear;
      n := rvalue.GetArrayLength - 1;
      for i := 0 to n do
      begin
        TObjectList<TObject>(aObject).Add(rvalue.GetArrayElement(i).AsObject);
      end;
    end;
    {$ELSE}
    n := 0;
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
    rProp.SetValue(aObject,n);
    {$ENDIF}
  end;
end;
{$ENDIF}

{$IFNDEF FPC}
procedure TRTTIJson.DeserializeXArray(Instance : TObject; aRecord : TValue; aProperty : TRttiProperty; const aPropertyName : string; aJson : TJsonObject);
var
  ctx : TRttiContext;
  rRec : TRttiRecordType;
  rfield : TRttiField;
  rValue : TValue;
  member : TJsonValue;
  jArray : TJSONArray;
begin
  rRec := ctx.GetType(aRecord.TypeInfo).AsRecord;
  rfield := rRec.GetField('fArray');
  if rfield <> nil then
  begin
    rValue := nil;
    //member := TJSONPair(aJson.GetValue(rField.Name));
    member := GetJsonPairValueByName(aJson,aPropertyName);
    if (member <> nil) and (rField.FieldType.TypeKind = tkDynArray) then
    begin
      jArray := TJSONObject.ParseJSONValue(member.ToJSON) as TJSONArray;
      try
        rValue := DeserializeDynArray(rField.FieldType.Handle,nil,jArray);
      finally
        jArray.Free;
      end;
    end;
  end;
  if not rValue.IsEmpty then rField.SetValue(aRecord.GetReferenceToRawData,rValue);
  aProperty.SetValue(Instance,aRecord);
end;
{$ENDIF}

function TRTTIJson.DeserializeProperty(aObject : TObject; const aName : string; aProperty : TRttiProperty; const aJson : TJSONObject) : TObject;
var
  rValue : TValue;
  {$IFNDEF FPC}
  member : TJsonValue;
  {$ELSE}
  member : TJsonObject;
  {$ENDIF}
  objClass: TClass;
  jArray : TJSONArray;
  json : TJSONObject;
begin
    Result := aObject;
    rValue := nil;
    {$IFNDEF FPC}
     //member := TJSONPair(aJson.GetValue(aName));
     member := GetJsonPairValueByName(aJson,aName);
    {$ELSE}
    member := TJsonObject(aJson.Find(aName));
    {$ENDIF}
    if member <> nil then
    begin
      case aProperty.PropertyType.TypeKind of
        tkDynArray :
          begin
            {$IFNDEF FPC}
            jArray := TJSONObject.ParseJSONValue(member.ToJSON) as TJSONArray;
            {$ELSE}
            jArray := TJSONArray(TJSONObject.ParseJSONValue(member.ToJSON));
            {$ENDIF}
            try
              {$IFNDEF FPC}
              aProperty.SetValue(aObject,DeserializeDynArray(aProperty.PropertyType.Handle,Result,jArray));
              {$ELSE}
              DeserializeDynArray(aProperty.PropertyType.Handle,aName,Result,jArray);
              {$ENDIF}
              Exit;
            finally
              jArray.Free;
            end;
          end;
        tkClass :
          begin
            //if (member.JsonValue is TJSONObject) then
            begin
              json := TJsonObject(TJSONObject.ParseJSONValue(member.ToJson));
              try
                if aProperty.GetValue(aObject).AsObject = nil then
                begin
                  {$IFNDEF FPC}
                  objClass := aProperty.PropertyType.Handle^.TypeData.ClassType;
                  rValue := DeserializeClass(objClass,json);
                  {$ELSE}
                  objClass := GetObjectPropClass(aObject,aName);
                  //objClass := GetTypeData(aProperty.PropertyType.Handle)^.ClassType;
                  rValue := DeserializeClass(objClass,json);
                  SetObjectProp(aObject,aName,rValue.AsObject);
                  Exit;
                  {$ENDIF}
                end
                else
                begin              
                  rValue := DeserializeObject(aProperty.GetValue(aObject).AsObject,json);
                  Exit;
                end;
              finally
                json.Free;
              end;
            end
          end;
        {$IFNDEF FPC}
        tkRecord :
          begin
            if aProperty.GetValue(aObject).TypeInfo = System.TypeInfo(TGUID) then
            begin
              rValue:=TValue.From<TGUID>(StringToGUID(member.ToJSON.DeQuotedString('"')));
            end
            else
            begin
              json := TJSONObject.ParseJSONValue(member.ToJson) as TJSONObject;
              try
                rValue := DeserializeRecord(aProperty.GetValue(aObject),aObject,json);
              finally
                json.Free;
              end;
            end;
          end;
        {$ENDIF}
      else
        begin
          {$IFNDEF FPC}
          //avoid return unicode escaped chars if string
          if aProperty.PropertyType.TypeKind in [tkString, tkLString, tkWString, tkUString] then
            {$IFDEF DELPHIRX10_UP}
            rValue := DeserializeType(aObject,aProperty.PropertyType.TypeKind,aProperty.GetValue(aObject).TypeInfo,TJsonValue(member).value)
            {$ELSE}
            rValue := DeserializeType(aObject,aProperty.PropertyType.TypeKind,aProperty.GetValue(aObject).TypeInfo,member.Value)
            {$ENDIF}
          else rValue := DeserializeType(aObject,aProperty.PropertyType.TypeKind,aProperty.GetValue(aObject).TypeInfo,member.ToJSON);
          {$ELSE}
          rValue := DeserializeType(aObject,aProperty.PropertyType.TypeKind,aName,member.ToJSON);
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
function TRTTIJson.DeserializeType(aObject : TObject; aType : TTypeKind; aTypeInfo : PTypeInfo; const aValue: string) : TValue;
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
          //raise EclJsonSerializerError.Create('Not supported data type!');
        end;
    end;
  except
    on E : Exception do
    begin
      raise EJsonDeserializeError.CreateFmt('Deserialize error type "%s.%s" : %s',[aObject.ClassName,GetTypeName(aTypeInfo),e.Message]);
    end;
  end;
end;
{$ELSE}
function TRTTIJson.DeserializeType(aObject : TObject; aType : TTypeKind; const aPropertyName, aValue: string) : TValue;
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
            if CompareText(value,'null') <> 0 then Result := JsonDateToDateTime(value);
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
          //raise EclJsonSerializerError.Create('Not supported data type!');
        end;
    end;
    //if not Result.IsEmpty then SetPropertyValue(aObject,propinfo,Result);
  except
    on E : Exception do
    begin
      raise EJsonDeserializeError.CreateFmt('Deserialize error type "%s" : %s',[aObject.ClassName,e.Message]);
    end;
  end;
end;
{$ENDIF}

function TRTTIJson.IsAllowedProperty(aObject : TObject; const aPropertyName : string) : Boolean;
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

function TRTTIJson.IsGenericList(aObject : TObject) : Boolean;
var
  cname : string;
begin
  if aObject = nil then Exit(False);

  cname := aObject.ClassName;
  Result := (cname.StartsWith('TObjectList')) or (cname.StartsWith('TList'));
end;

function TRTTIJson.IsGenericXArray(const aClassName : string) : Boolean;
begin
  Result := aClassName.StartsWith('TXArray');
end;

function TRTTIJson.GetJsonPairValueByName(aJson: TJSONObject; const aName: string): TJsonValue;
var
  candidate : TJSONPair;
  i : Integer;
begin
  if fUseJsonCaseSense then
  begin
    Result := aJson.GetValue(aName);
    Exit;
  end
  else
  begin
    for i := 0 to aJson.Count - 1 do
    begin
      candidate := aJson.Pairs[I];
      if candidate.JsonValue = nil then continue;
      if CompareText(candidate.JsonString{$IFNDEF FPC}.Value{$ENDIF},aName) = 0 then Exit(candidate.JsonValue);
    end;
  end;
  Result := nil;
end;

function TRTTIJson.GetJsonPairByName(aJson: TJSONObject; const aName: string): TJSONPair;
var
  i : Integer;
begin
  if fUseJsonCaseSense then
  begin
    Result := TJSONPair(aJson.GetValue(aName));
    Exit;
  end
  else
  begin
    if aJson <> nil then
    begin
      for i := 0 to aJson.Count - 1 do
      begin
        Result := aJson.Pairs[I];
        if Result.JsonValue = nil then continue;
        if CompareText(Result.JsonString{$IFNDEF FPC}.Value{$ENDIF},aName) = 0 then Exit;
      end;
    end;
  end;
  Result := nil;
end;

function TRTTIJson.GetPropertyValue(Instance : TObject; const PropertyName : string) : TValue;
var
  pinfo : PPropInfo;
begin
  Result := nil;
  pinfo := GetPropInfo(Instance,PropertyName);
  if pinfo = nil then raise EJsonSerializeError.CreateFmt('Property "%s" not found!',[PropertyName]);
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

function TRTTIJson.GetPropertyValueFromObject(Instance : TObject; const PropertyName : string) : TValue;
var
  ctx : TRttiContext;
  rprop : TRttiProperty;
begin
  rprop := ctx.GetType(Instance.ClassInfo).GetProperty(PropertyName);
  Result := rprop.GetValue(Instance);
end;

{$IFNDEF FPC}
function TRTTIJson.GetFieldValueFromRecord(const aValue : TValue; const FieldName : string) : TValue;
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

procedure TRTTIJson.SetPropertyValue(Instance : TObject; const PropertyName : string; aValue : TValue);
var
  pinfo : PPropInfo;
begin
  pinfo := GetPropInfo(Instance,PropertyName);
  SetPropertyValue(Instance,pinfo,aValue);
end;

procedure TRTTIJson.SetPropertyValue(Instance : TObject; aPropInfo : PPropInfo; aValue : TValue);
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
procedure TRTTIJson.LoadSetProperty(aInstance : TObject; aPropInfo: PPropInfo; const aValue: string);
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

function TRTTIJson.SerializeObject(aObject: TObject): TJSONObject;
var
  ctx: TRttiContext;
  {$IFNDEF FPC}
  attr : TCustomAttribute;
  comment : string;
  {$ENDIF}
  rType: TRttiType;
  rProp: TRttiProperty;
  jpair : TJSONPair;
  ExcludeSerialize : Boolean;
  propertyname : string;
  propvalue : TValue;
begin
  if (aObject = nil) then
  begin
    Result := nil;
    Exit;
  end;

  Result := TJSONObject.Create;
  try
    propertyname := '';
    rType := ctx.GetType(aObject.ClassInfo);
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
          if comment <> '' then Result.AddPair(TJSONPair.Create('#Comment#->'+propertyname,Comment));
          {$ENDIF}
          begin
            propvalue := rProp.GetValue(aObject);
            jpair := TJSONPair.Create(propertyName,nil);
            if (propvalue.IsObject) and (IsGenericList(propvalue.AsObject)) then
            begin
              jpair.JsonValue := SerializeValue(GetPropertyValueFromObject(propvalue.AsObject,'List'));
            end
            {$IFNDEF FPC}
            else if (not propvalue.IsObject) and (IsGenericXArray(propvalue{$IFNDEF NEXTGEN}.TypeInfo.Name{$ELSE}.TypeInfo.NameFld.ToString{$ENDIF})) then
            begin
              jpair.JsonValue := SerializeValue(GetFieldValueFromRecord(propvalue,'fArray'));
            end
            {$ENDIF}
            else
            begin
              {$IFNDEF FPC}
              jpair.JsonValue := SerializeValue(propvalue);
              {$ELSE}
              jpair.JsonValue := SerializeValue(propvalue);// SerializeObject(aObject,rProp.PropertyType.TypeKind,propertyname);
              {$ENDIF}
            end;
            //s := jpair.JsonValue.ToString;
            if jpair.JsonValue <> nil then
            begin
              Result.AddPair(jpair);
            end
            else jpair.Free;
          end;
        end;
      end;
    end;
  except
    on E : Exception do
    begin
      Result.Free;
      if not propertyname.IsEmpty then raise EJsonSerializeError.CreateFmt('Serialize Error -> Object property: "%s" (%s)',[propertyname,e.Message])
       else raise EJsonSerializeError.CreateFmt('Serialize Error -> Object (%s)',[e.Message]);
    end;
  end;
end;

function TRTTIJson.GetValue(aAddr: Pointer; aType: TRTTIType): TValue;
begin
  TValue.Make(aAddr,aType.Handle,Result);
end;

function TRTTIJson.GetValue(aAddr: Pointer; aTypeInfo: PTypeInfo): TValue;
begin
  TValue.Make(aAddr,aTypeInfo,Result);
end;

function TRTTIJson.SerializeValue(const aValue : TValue) : TJSONValue;
begin
  Result := nil;
  case avalue.Kind of
    tkDynArray :
      begin
        {$IFNDEF FPC}
        Result := SerializeDynArray(aValue);
        {$ENDIF}
      end;
    tkClass :
      begin
         Result := TJSONValue(SerializeObject(aValue.AsObject));
      end;
    tkString, tkLString, tkWString, tkUString :
      begin
        Result := TJSONString.Create(aValue.AsString);
      end;
    tkChar, tkWChar :
      begin
        Result := TJSONString.Create(aValue.AsString);
      end;
    tkInteger :
      begin
        Result := TJSONNumber.Create(aValue.AsInteger);
      end;
    tkInt64 :
      begin
        Result := TJSONNumber.Create(aValue.AsInt64);
      end;
    tkFloat :
      begin
        if aValue.TypeInfo = TypeInfo(TDateTime) then
        begin
          if aValue.AsExtended <> 0.0 then Result := TJSONString.Create(DateTimeToJsonDate(aValue.AsExtended));
        end
        else if aValue.TypeInfo = TypeInfo(TDate) then
        begin
          if aValue.AsExtended <> 0.0 then Result := TJSONString.Create(DateToStr(aValue.AsExtended));
        end
        else if aValue.TypeInfo = TypeInfo(TTime) then
        begin
          Result := TJSONString.Create(TimeToStr(aValue.AsExtended));
        end
        else
        begin
          Result := TJSONNumber.Create(aValue.AsExtended);
        end;
      end;
    tkEnumeration :
      begin
        if (aValue.TypeInfo = System.TypeInfo(Boolean)) then
        begin
          {$IF Defined(DELPHIRX10_UP) OR Defined(FPC)}
          Result := TJSONBool.Create(aValue.AsBoolean);
          {$ELSE}
          if aValue.AsBoolean then Result := TJsonTrue.Create
            else Result := TJsonFalse.Create;
          {$ENDIF}
        end
        else
        begin
          //Result.JsonValue := TJSONString.Create(GetEnumName(aValue.TypeInfo,aValue.AsOrdinal));
          if fUseEnumNames then Result := TJSONString.Create(aValue.ToString)
            else Result := TJSONNumber.Create(GetEnumValue(aValue.TypeInfo,aValue.ToString));
        end;
      end;
    {$IFDEF FPC}
    tkBool :
      begin
        Result := TJSONBool.Create(aValue.AsBoolean);
      end;
    {$ENDIF}
    tkSet :
      begin
        Result := TJSONString.Create(aValue.ToString);
      end;
    tkRecord :
      begin
        {$IFNDEF FPC}
        Result := SerializeRecord(aValue);
        {$ENDIF}
      end;
    tkVariant :
      begin
        {$IFNDEF FPC}
        case VarType(aValue.AsVariant) and VarTypeMask of
          varInteger, varInt64 : Result := TJSONNumber.Create(aValue.AsInteger);
          varString, varUString, varEmpty : Result := TJSONString.Create(aValue.AsString);
          varDouble : Result := TJSONNumber.Create(aValue.AsExtended);
        end;
        {$ENDIF}
      end;
    tkMethod, tkPointer, tkClassRef ,tkInterface, tkProcedure, tkUnknown :
      begin
        //skip these properties
      end
  else
    begin
      {$IFNDEF FPC}
      raise EJsonSerializeError.CreateFmt(cNotSupportedDataType,[GetTypeName(aValue.TypeInfo)]);
      {$ELSE}
      raise EJsonSerializeError.Create('Not supported Data Type');
      {$ENDIF}
    end;
  end;
  if Result = nil then Result := TJSONNull.Create;
end;

{$IFNDEF FPC}
function TRTTIJson.SerializeDynArray(const aValue: TValue) : TJsonArray;
var
  ctx : TRttiContext;
  rDynArray : TRTTIDynamicArrayType;
  i : Integer;
  jValue : TJSONValue;
  element : Integer;
  list : TList<TJSONValue>;
begin
  element := -1;
  Result := TJSONArray.Create;
  try
    rDynArray := ctx.GetType(aValue.TypeInfo) as TRTTIDynamicArrayType;
    list := TList<TJSONValue>.Create;
    list.Capacity := aValue.GetArrayLength;
    for i := 0 to aValue.GetArrayLength - 1 do
    begin
      if not GetValue(PPByte(aValue.GetReferenceToRawData)^ + rDynArray.ElementType.TypeSize * i, rDynArray.ElementType).IsEmpty then
      begin
        element := i;
        jValue := SerializeValue(GetValue(PPByte(aValue.GetReferenceToRawData)^ + rDynArray.ElementType.TypeSize * i, rDynArray.ElementType));
        if jValue = nil then jValue := TJSONNull.Create;
        list.Add(jValue);
      end;
    end;
    Result.SetElements(list);
  except
    on E : Exception do
    begin
      if element > -1 then raise EJsonSerializeError.CreateFmt('Serialize Error -> Array[%d] (%s)',[element,e.Message])
       else raise EJsonSerializeError.CreateFmt('Serialize Error -> Array (%s)',[e.Message]);
    end;
  end;
end;

function TRTTIJson.SerializeRecord(const aValue : TValue) : TJSONValue;
var
  ctx : TRttiContext;
  json : TJSONObject;
  rRec : TRttiRecordType;
  rField : TRttiField;
begin
  rField := nil;
  try
    rRec := ctx.GetType(aValue.TypeInfo).AsRecord;
    if aValue.TypeInfo = System.TypeInfo(TGUID) then
    begin
      Result := TJSONString.Create(GUIDToString(aValue.AsType<TGUID>));
    end
    else
    begin
      json := TJSONObject.Create;
      for rField in rRec.GetFields do
      begin
        json.AddPair(rField.Name,SerializeValue(rField.GetValue(aValue.GetReferenceToRawData)));
      end;
      Result := json;
    end;
  except
    on E : Exception do
    begin
      if rField <> nil then raise EJsonSerializeError.CreateFmt('Serialize Error -> Record property "%s" (%s)',[rField.Name,e.Message])
       else raise EJsonSerializeError.CreateFmt('Serialize Error -> Record (%s)',[e.Message]);
    end;
  end;
end;

{$ELSE}
function TRTTIJson.GetPropType(aPropInfo: PPropInfo): PTypeInfo;
begin
  Result := aPropInfo^.PropType;
end;

function TRTTIJson.FloatProperty(aObject : TObject; aPropInfo: PPropInfo): string;
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

function TRTTIJson.SerializeObject(aObject : TObject; aType : TTypeKind; const aPropertyName : string) : TJSONPair;
var
  propinfo : PPropInfo;
  jArray : TJsonArray;
  jPair : TJsonPair;
  jValue : TJsonValue;
  i : Integer;
  pArr : Pointer;
  rValue : TValue;
  rItemValue : TValue;
  len : Integer;
begin
  try
    Result := TJSONPair.Create(aPropertyName,nil);

    propinfo := GetPropInfo(aObject,aPropertyName);
    //case propinfo.PropType.Kind of
    case aType of
      tkDynArray :
        begin
          len := 0;
          jArray := TJSONArray.Create;
          try
            pArr := GetDynArrayProp(aObject,aPropertyName);
            TValue.Make(@pArr,propinfo.PropType, rValue);
            if rValue.IsArray then
            begin
              len := rValue.GetArrayLength;
              for i := 0 to len - 1 do
              begin
                rItemValue := rValue.GetArrayElement(i);
                jValue := SerializeValue(rItemValue);
                jArray.Add(jValue);
              end;
            end;
            Result.JsonValue := jArray;
          finally
            //DynArrayClear(pArr,propinfo.PropType);
            pArr := nil;
          end;
        end;
      tkClass :
        begin
          Result.JsonValue := TJSONValue(SerializeObject(GetObjectProp(aObject,aPropertyName)));
        end;
      tkString, tkLString, tkWString, tkUString, tkAString :
        begin
          Result.JsonValue := TJSONString.Create(GetStrProp(aObject,aPropertyName));
        end;
      tkChar, tkWChar :
        begin
          Result.JsonValue := TJSONString.Create(Char(GetOrdProp(aObject,aPropertyName)));
        end;
      tkInteger :
        begin
          Result.JsonValue := TJSONNumber.Create(GetOrdProp(aObject,aPropertyName));
        end;
      tkInt64 :
        begin
          Result.JsonValue := TJSONNumber.Create(GetOrdProp(aObject,aPropertyName));
        end;
      tkFloat :
        begin
          if propinfo.PropType = TypeInfo(TDateTime) then
          begin
            Result.JsonValue := TJSONString.Create(DateTimeToJsonDate(GetFloatProp(aObject,aPropertyName)));
          end
          else if propinfo.PropType = TypeInfo(TDate) then
          begin
            Result.JsonValue := TJSONString.Create(DateToStr(GetFloatProp(aObject,aPropertyName)));
          end
          else if propinfo.PropType = TypeInfo(TTime) then
          begin
            Result.JsonValue := TJSONString.Create(TimeToStr(GetFloatProp(aObject,aPropertyName)));
          end
          else
          begin
            //Result.JsonValue := TJsonFloatNumber.Create(GetFloatProp(aObject,aPropertyName));
            Result.JsonValue := TJsonFloatNumber.Create(StrToFloat(FloatProperty(aObject,propinfo)));
          end;
        end;
      tkEnumeration,tkBool :
        begin
          if (propinfo.PropType = System.TypeInfo(Boolean)) then
          begin
            Result.JsonValue := TJSONBool.Create(Boolean(GetOrdProp(aObject,aPropertyName)));
          end
          else
          begin
            if fUseEnumNames then Result.JsonValue := TJSONString.Create(GetEnumName(propinfo.PropType,GetOrdProp(aObject,aPropertyName)))
              else Result.JsonValue := TJSONNumber.Create(GetOrdProp(aObject,aPropertyName));
            //Result.JsonValue := TJSONString.Create(aValue.ToString);
          end;
        end;
      tkSet :
        begin
          Result.JsonValue := TJSONString.Create(GetSetProp(aObject,aPropertyName));
        end;
      {$IFNDEF FPC}
      tkRecord :
        begin
          Result.JsonValue := SerializeRecord(aValue);
        end;
      {$ENDIF}
      tkMethod, tkPointer, tkClassRef ,tkInterface, tkProcedure :
        begin
          //skip these properties
          //FreeAndNil(Result);
        end
    else
      begin

        //raise EJsonDeserializeError.CreateFmt('Not supported type "%s":%d',[aName,Integer(aValue.Kind)]);
      end;
    end;
    if Result.JsonValue = nil then Result.JsonValue := TJSONNull.Create;
  except
    on E : Exception do
    begin
      Result.Free;
      {$IFNDEF FPC}
      raise EJsonSerializeError.CreateFmt('Serialize error class "%s.%s" : %s',[aName,aValue.ToString,e.Message]);
      {$ENDIF}
    end;
  end;
end;
{$ENDIF}


{ TJsonSerializer}

constructor TJsonSerializer.Create(aSerializeLevel: TSerializeLevel; aUseEnumNames : Boolean = True);
begin
  {$IFDEF FPC}
  if aSerializeLevel = TSerializeLevel.slPublicProperty then raise EJsonSerializeError.Create('FreePascal RTTI only supports published properties');
  {$ENDIF}
  fSerializeLevel := aSerializeLevel;
  fUseEnumNames := aUseEnumNames;
  fUseJsonCaseSense := False;
  fRTTIJson := TRTTIJson.Create(aSerializeLevel,aUseEnumNames);
  fRTTIJson.UseJsonCaseSense := fUseJsonCaseSense;
end;

destructor TJsonSerializer.Destroy;
begin
  fRTTIJson.Free;
  inherited;
end;

function TJsonSerializer.JsonToObject(aType: TClass; const aJson: string): TObject;
var
  json: TJSONObject;
begin
  try
    {$IFDEF DELPHIRX10_UP}
    json := TJSONObject.ParseJSONValue(aJson,True) as TJSONObject;
    {$ELSE}
     {$IFDEF FPC}
     json := TJSONObject(TJSONObject.ParseJSONValue(aJson,True));
     {$ELSE}
     json := TJsonObject.ParseJSONValue(TEncoding.UTF8.GetBytes(aJson),0,True) as TJSONObject;
     {$ENDIF}
    {$ENDIF}
  except
    raise EJsonDeserializeError.Create(cNotValidJson);
  end;
  try
    Result := fRTTIJson.DeserializeClass(aType,json);
  finally
    json.Free;
  end;
end;

function TJsonSerializer.JsonToObject(aObject: TObject; const aJson: string): TObject;
var
  json: TJSONObject;
begin;
  try
    {$IFDEF DELPHIRX10_UP}
    json := TJSONObject.ParseJSONValue(aJson,True) as TJSONObject;
    {$ELSE}
     {$IFDEF FPC}
     json := TJSONObject(TJSONObject.ParseJSONValue(aJson,True));
     {$ELSE}
     json := TJsonObject.ParseJSONValue(TEncoding.UTF8.GetBytes(aJson),0,True) as TJSONObject;
     {$ENDIF}
    {$ENDIF}
  except
    raise EJsonDeserializeError.Create(cNotValidJson);
  end;
  try
    Result := fRTTIJson.DeserializeObject(aObject,json);
  finally
    json.Free;
  end;
end;

function TJsonSerializer.ObjectToJson(aObject : TObject; aIndent : Boolean = False): string;
var
  json: TJSONObject;
begin
  json := fRTTIJson.SerializeObject(aObject);
  try
    if aIndent then Result := TJsonUtils.JsonFormat(json.ToJSON)
      else Result := json.ToJSON;
  finally
    json.Free;
  end;
end;

function TJsonSerializer.ObjectToJsonString(aObject : TObject; aIndent : Boolean = False): string;
var
  json: TJSONObject;
begin
  json := fRTTIJson.SerializeObject(aObject);
  try
    if aIndent then Result := TJsonUtils.JsonFormat(json.ToString)
      else  Result := json.ToString;
  finally
    json.Free;
  end;
end;

function TJsonSerializer.ValueToJson(const aValue: TValue; aIndent: Boolean): string;
var
  json: TJSONValue;
begin
  json:= fRTTIJson.SerializeValue(aValue);
  if json = nil then raise EJsonSerializerError.Create('Error serializing TValue');
  try
    if aIndent then Result := TJsonUtils.JsonFormat(json{$IFNDEF FPC}.ToJSON{$ELSE}.AsJson{$ENDIF})
      else Result := json{$IFNDEF FPC}.ToJSON{$ELSE}.AsJson{$ENDIF};
  finally
    json.Free;
  end;
end;

function TJsonSerializer.ValueToJsonString(const aValue: TValue; aIndent: Boolean): string;
var
  json: TJSONValue;
begin
  json:= fRTTIJson.SerializeValue(aValue);
  if json = nil then raise EJsonSerializerError.Create('Error serializing TValue');
  try
    if aIndent then Result := TJsonUtils.JsonFormat(json.ToString)
      else Result := json.ToString;
  finally
    json.Free;
  end;
end;

function TJsonSerializer.ArrayToJson<T>(aArray: TArray<T>; aIndent: Boolean): string;
var
  json: TJSONValue;
begin
  json:= fRTTIJson.SerializeValue(TValue.From<TArray<T>>(aArray));
  if json = nil then raise EJsonSerializerError.Create('Error serializing Array');
  try
    if aIndent then Result := TJsonUtils.JsonFormat(json{$IFNDEF FPC}.ToJSON{$ELSE}.AsJson{$ENDIF})
      else Result := json{$IFNDEF FPC}.ToJSON{$ELSE}.AsJson{$ENDIF};
  finally
    json.Free;
  end;
end;

function TJsonSerializer.ArrayToJsonString<T>(aArray: TArray<T>; aIndent: Boolean): string;
var
  json: TJSONValue;
begin
  json:= fRTTIJson.SerializeValue(TValue.From<TArray<T>>(aArray));
  if json = nil then raise EJsonSerializerError.Create('Error serializing Array');
  try
    if aIndent then Result := TJsonUtils.JsonFormat(json.ToString)
      else Result := json.ToString;
  finally
    json.Free;
  end;
end;

{$IFNDEF FPC}
function TJsonSerializer.JsonToArray<T>(const aJson: string): TArray<T>;
var
  jarray: TJSONArray;
  value : TValue;
begin;
  try
    {$If Defined(FPC) OR Defined(DELPHIRX10_UP)}
    jarray := TJSONObject.ParseJSONValue(aJson,True) as TJSONArray;
    {$ELSE}
    jarray := TJsonObject.ParseJSONValue(TEncoding.UTF8.GetBytes(aJson),0,True) as TJSONArray;
  {$ENDIF}
  except
    raise EJsonDeserializeError.Create(cNotValidJson);
  end;
  try
    value := fRTTIJson.DeserializeDynArray(PTypeInfo(TypeInfo(TArray<T>)),nil,jarray);
    Result := value.AsType<TArray<T>>;
  finally
    jarray.Free;
  end;
end;

function TJsonSerializer.JsonToValue(const aJson: string): TValue;
var
  json: TJSONObject;
  value : TValue;
begin;
  try
    {$If Defined(FPC) OR Defined(DELPHIRX10_UP)}
    json := TJSONObject.ParseJSONValue(aJson,True) as TJSONObject;
    {$ELSE}
    json := TJsonObject.ParseJSONValue(TEncoding.UTF8.GetBytes(aJson),0,True) as TJSONObject;
    {$ENDIF}
  except
    raise EJsonDeserializeError.Create(cNotValidJson);
  end;
  try
    value := fRTTIJson.DeserializeRecord(value,nil,json);
    Result := value; // value.AsType<TArray<T>>;
  finally
    json.Free;
  end;
end;
{$ENDIF}

procedure TJsonSerializer.SetUseEnumNames(const Value: Boolean);
begin
  fUseEnumNames := Value;
  if Assigned(fRTTIJson) then fRTTIJson.UseEnumNames := Value;
end;

procedure TJsonSerializer.SetUseJsonCaseSense(const Value: Boolean);
begin
  fUseJsonCaseSense := Value;
  if Assigned(fRTTIJson) then fRTTIJson.UseJsonCaseSense := Value;
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

{$IF NOT DEFINED(DELPHIXE7_UP) AND NOT DEFINED(FPC)}
{ TJSONArrayHelper }

function TJSONArrayHelper.Count: Integer;
begin
  Result := Self.Size;
end;

function TJSONArrayHelper.GetItem(aValue: Integer): TJSONValue;
begin
  Result := Self.Get(aValue);
end;

procedure TJSONArrayHelper.SetElements(aElements: TList<TJSONValue>);
var
  jvalue : TJSONValue;
begin
  for jvalue in aElements do Self.AddElement(jvalue);
  aElements.Free;
end;

{ TJSONValueHelper }

function TJSONValueHelper.ToJson: string;
begin
  Result := Self.ToString;
end;


{ TJSONObjectHelper }

function TJSONObjectHelper.Count: Integer;
begin
  Result := Self.Size;
end;

function TJSONObjectHelper.GetValue(const aName: string): TJSONValue;
var
  jPair : TJSONPair;
begin
  Result := nil;
  for jPair in Self do
  begin
    if jPair.JsonString.ToString = aName then Exit(jPair.JsonValue);
  end;
end;

function TJSONObjectHelper.GetPair(aValue: Integer) : TJSONPair;
begin
  Result := Self.Get(aValue);
end;

{$ENDIF}

end.


