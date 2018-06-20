{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.JSON.Serializer
  Description : Json Serializer
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 21/05/2018
  Modified    : 20/06/2018

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

interface

uses
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Json,
  System.DateUtils,
  Quick.Commons;

type

  EJsonSerializeError = class(Exception);
  EJsonDeserializeError = class(Exception);

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

  IJsonSerializer = interface
  ['{CA26F7AE-F1FE-41BE-9C23-723A687F60D1}']
    function JsonToObject(aType: TClass; const aJson: string): TObject; overload;
    function JsonToObject(aObject: TObject; const aJson: string): TObject; overload;
    function ObjectToJson(aObject: TObject): string;
  end;


  TJsonSerializer = class(TInterfacedObject,IJsonSerializer)
  strict private
    function GetValue(aAddr: Pointer; aType: TRTTIType): TValue;
    function IsAllowedProperty(aObject : TObject; const aPropertyName : string) : Boolean;
    procedure DeserializeDynArray(aProperty : TRttiProperty; aObject : TObject; const aJsonArray: TJSONArray);
    function DeserializeRecord(aRecord : TValue; aObject : TObject; const aJson : TJSONObject) : TValue;
    function DeserializeClass(aType : TClass; const aJson : TJSONObject) : TObject;
    function DeserializeObject(aObject : TObject; const aJson : TJSONObject) : TObject; overload;
    function DeserializeProperty(aObject : TObject; const aName : string; aProperty : TRttiProperty; const aJson : TJSONObject) : TObject; overload;
    function DeserializeType(aObject : TObject; aType : TTypeKind; aTypeInfo : PTypeInfo; const aValue: string) : TValue;
    function Serialize(const aName : string; aValue : TValue) : TJSONPair; overload;
    function Serialize(aObject : TObject) : TJSONObject; overload;
  public
    function JsonToObject(aType : TClass; const aJson: string) : TObject; overload;
    function JsonToObject(aObject : TObject; const aJson: string) : TObject; overload;
    function ObjectToJson(aObject : TObject): string;
  end;

  PPByte = ^PByte;

resourcestring
  cNotSupportedDataType = 'Not supported "%s" data type "%s"';
  cNotSerializable = 'Object is not serializable';

implementation

{ TqlJsonSerializer }

procedure TJsonSerializer.DeserializeDynArray(aProperty: TRttiProperty; aObject: TObject; const aJsonArray: TJSONArray);
var
  rType: PTypeInfo;
  len: NativeInt;
  pArr: Pointer;
  rValue : TValue;
  rItemValue: TValue;
  i: Integer;
  objClass: TClass;
  ctx : TRttiContext;
  json : TJSONObject;
  rDynArray : TRttiDynamicArrayType;
  propObj : TObject;
begin
  if GetTypeData(aProperty.PropertyType.Handle).DynArrElType = nil then Exit;
  len := aJsonArray.Count;
  rType := GetTypeData(aProperty.PropertyType.Handle).DynArrElType^;
  pArr := nil;
  DynArraySetLength(pArr, aProperty.PropertyType.Handle, 1, @len);
  try
    TValue.Make(@pArr, aProperty.PropertyType.Handle, rValue);
    rDynArray := ctx.GetType(rValue.TypeInfo) as TRTTIDynamicArrayType;

    for i := 0 to aJsonArray.Count - 1 do
    begin
      rItemValue := nil;
      case rType.Kind of
        tkClass :
          begin
            if aJsonArray.Items[i] is TJSONObject then
            begin
              propObj := GetValue(PPByte(rValue.GetReferenceToRawData)^ +rDynArray.ElementType.TypeSize * i, rDynArray.ElementType).AsObject;
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
            rItemValue := DeserializeRecord(GetValue(PPByte(rValue.GetReferenceToRawData)^ +rDynArray.ElementType.TypeSize * i,
                                            rDynArray.ElementType),aObject,json);
          end;
        tkMethod, tkPointer, tkClassRef ,tkInterface, tkProcedure :
          begin
            //skip these properties
          end
      else
        begin
          //raise EJsonSerializeError.Create(Format(cNotSupportedDataType,[aProperty.Name,GetTypeName(rType)]));
          rItemValue := DeserializeType(aObject,rType.Kind,aProperty.GetValue(aObject).TypeInfo,aJsonArray.Items[i].Value);
        end;
      end;
      if not rItemValue.IsEmpty then rValue.SetArrayElement(i,rItemValue);
    end;
    aProperty.SetValue(aObject,rValue);
  finally
    DynArrayClear(pArr, aProperty.PropertyType.Handle);
  end;
end;

function TJsonSerializer.DeserializeRecord(aRecord : TValue; aObject : TObject; const aJson : TJSONObject) : TValue;
var
  ctx : TRttiContext;
  rRec : TRttiRecordType;
  rField : TRttiField;
  rValue : TValue;
  member : TJSONPair;
  json : TJSONObject;
  objClass : TClass;
  propobj : TObject;
begin
  rRec := ctx.GetType(aRecord.TypeInfo).AsRecord;
  try
    for rField in rRec.GetFields do
    begin
      rValue := nil;
      member := TJSONPair(aJson.GetValue(rField.Name));
      if member <> nil then
      case rField.FieldType.TypeKind of
        tkDynArray :
          begin
            {jArray := TJSONObject.ParseJSONValue(member.ToJSON) as TJSONArray;
            try
              DeserializeDynArray(aProp,Result,jArray);
            finally
              jArray.Free;
            end;}
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
      else
        begin
          rValue := DeserializeType(aObject,rField.FieldType.TypeKind,rField.FieldType.Handle,member.JsonString.ToString);
        end;
      end;
      if not rValue.IsEmpty then rField.SetValue(aRecord.GetReferenceToRawData,rValue);
    end;
    Result := aRecord;
  finally
    ctx.Free;
  end;
end;

function TJsonSerializer.JsonToObject(aObject: TObject; const aJson: string): TObject;
var
  json: TJSONObject;
begin
  json := TJSONObject.ParseJSONValue(aJson,True) as TJSONObject;
  try
    Result := DeserializeObject(aObject,json);
  finally
    json.Free;
  end;
end;

function TJsonSerializer.JsonToObject(aType: TClass; const aJson: string): TObject;
var
  json: TJSONObject;
begin
  json := TJSONObject.ParseJSONValue(aJson) as TJSONObject;
  try
    Result := DeserializeClass(aType,json);
  finally
    json.Free;
  end;
end;

function TJsonSerializer.ObjectToJson(aObject: TObject): string;
var
  json: TJSONObject;
begin
  json := Serialize(aObject);
  try
    Result := json.ToJSON;
  finally
    json.Free;
  end;
end;

function TJsonSerializer.DeserializeClass(aType: TClass; const aJson: TJSONObject): TObject;
begin
  Result := nil;
  if (aJson = nil) or (aJson.Count = 0) then Exit;

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

function TJsonSerializer.DeserializeObject(aObject: TObject; const aJson: TJSONObject): TObject;
var
  ctx: TRttiContext;
  rType: TRttiType;
  rProp: TRttiProperty;
  attr: TCustomAttribute;
  propertyname : string;
begin
  Result := aObject;

  if (aJson = nil) or (aJson.Count = 0) or (Result = nil) then Exit;
  try
    rType := ctx.GetType(aObject.ClassInfo);
    try
      for rProp in rType.GetProperties do
      begin
        if (rProp.PropertyType.IsPublicType) and (rProp.IsWritable) and (IsAllowedProperty(aObject,rProp.Name)) then
        begin
          propertyname := rProp.Name;
          for attr in rProp.GetAttributes do if attr is TCustomNameProperty then propertyname := TCustomNameProperty(attr).Name;
          Result := DeserializeProperty(Result, propertyname, rProp, aJson);
        end;
      end;
    finally
      ctx.Free;
    end;
  except
    on E : Exception do
    begin
      Result.Free;
      raise EJsonDeserializeError.CreateFmt('Deserialize error for object "%s" : %s',[aObject.ClassName,e.Message]);
    end;
  end;
end;


function TJsonSerializer.DeserializeProperty(aObject : TObject; const aName : string; aProperty : TRttiProperty; const aJson : TJSONObject) : TObject;
var
  rValue : TValue;
  member : TJSONPair;
  objClass: TClass;
  jArray : TJSONArray;
  json : TJSONObject;
begin
    Result := aObject;
    member := TJSONPair(aJson.GetValue(aName));
    if member <> nil then
    begin
      case aProperty.PropertyType.TypeKind of
        tkDynArray :
          begin
            jArray := TJSONObject.ParseJSONValue(member.ToJSON) as TJSONArray;
            try
              DeserializeDynArray(aProperty,Result,jArray);
            finally
              jArray.Free;
            end;
          end;
        tkClass :
          begin
            //if (member.JsonValue is TJSONObject) then
            begin
              json := TJSONObject.ParseJSONValue(member.ToJson) as TJSONObject;
              try
                if aProperty.GetValue(aObject).AsObject = nil then
                begin
                  objClass := aProperty.PropertyType.Handle^.TypeData.ClassType;
                  rValue := DeserializeClass(objClass,json)
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
        tkRecord :
          begin
            json := TJSONObject.ParseJSONValue(member.ToJson) as TJSONObject;
            try
              rValue := DeserializeRecord(aProperty.GetValue(aObject),aObject,json);
            finally
              json.Free;
            end;
          end;
      else
        begin
          rValue := DeserializeType(Result,aProperty.PropertyType.TypeKind,aProperty.GetValue(Result).TypeInfo,member.ToJSON);
        end;
      end;
      if not rValue.IsEmpty then aProperty.SetValue(Result,rValue);
    end;
end;

function TJsonSerializer.DeserializeType(aObject : TObject; aType : TTypeKind; aTypeInfo : PTypeInfo; const aValue: string) : TValue;
var
  i : Integer;
  value : string;
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
            Result := JsonDateToDateTime(value);
          end
          else if aTypeInfo = TypeInfo(TDate) then
          begin
            Result := StrToDate(value);
          end
          else if aTypeInfo = TypeInfo(TTime) then
          begin
            Result := StrToTime(value);
          end
          else
          begin
            Result := StrToFloat(value);
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
            TValue.Make(GetEnumValue(aTypeInfo,value),aTypeInfo, Result);
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

function TJsonSerializer.IsAllowedProperty(aObject : TObject; const aPropertyName : string) : Boolean;
var
  propname : string;
begin
  Result := True;
  propname := aPropertyName.ToLower;

  if (aObject.ClassName.StartsWith('TObjectList')) then
  begin
    if (propname = 'capacity') or (propname = 'count') or (propname = 'ownsobjects') then Result := False;
  end
  else if (propname = 'refcount') then Result := False;
end;

function TJsonSerializer.Serialize(aObject: TObject): TJSONObject;
var
  ctx: TRttiContext;
  attr : TCustomAttribute;
  rType: TRttiType;
  rProp: TRttiProperty;
  jpair : TJSONPair;
  ExcludeSerialize : Boolean;
  comment : string;
  propertyname : string;
begin
  if (aObject = nil) then
  begin
    Result := nil;
    Exit;
  end;

  Result := TJSONObject.Create;
  try
    rType := ctx.GetType(aObject.ClassInfo);
    try
      //s := rType.ToString;
      for rProp in rType.GetProperties do
      begin
        ExcludeSerialize := False;
        comment := '';
        propertyname := rProp.Name;
        for attr in rProp.GetAttributes do
        begin
          if attr is TNotSerializableProperty then ExcludeSerialize := True
          else if attr is TCommentProperty then comment := TCommentProperty(attr).Comment
          else if  attr is TCustomNameProperty then propertyname := TCustomNameProperty(attr).Name;
        end;
        if (rProp.PropertyType.IsPublicType) and (IsAllowedProperty(aObject,propertyname)) and (not ExcludeSerialize) then
        begin
          //add comment as pair
          if comment <> '' then Result.AddPair(TJSONPair.Create('#Comment#->'+propertyname,Comment));
          //s := rProp.Name;
          jpair := Serialize(propertyname,rProp.GetValue(aObject));
          //s := jpair.JsonValue.ToString;
          if jpair <> nil then Result.AddPair(jpair)
            else jpair.Free;
          //Result.AddPair(Serialize(rProp.Name,rProp.GetValue(aObject)));
          //s := Result.ToJSON;
        end;
      end;
    finally
      ctx.Free;
    end;
  except
    on E : Exception do
    begin
      Result.Free;
      raise EJsonSerializeError.CreateFmt('Serialize error object "%s" : %s',[aObject.ClassName,e.Message]);
    end;
  end;
end;

function TJsonSerializer.GetValue(aAddr: Pointer; aType: TRTTIType): TValue;
begin
  TValue.Make(aAddr,aType.Handle,Result);
end;

function TJsonSerializer.Serialize(const aName : string; aValue : TValue): TJSONPair;
var
  ctx: TRttiContext;
  rRec : TRttiRecordType;
  rField : TRttiField;
  rDynArray : TRTTIDynamicArrayType;
  json : TJSONObject;
  jArray : TJSONArray;
  jPair : TJSONPair;
  jValue : TJSONValue;
  i : Integer;
begin
  Result := TJSONPair.Create(aName,nil);
  //Result.JsonString := TJSONString(aName);
  try
    case aValue.Kind of
      tkDynArray :
        begin
          jArray := TJSONArray.Create;
          rDynArray := ctx.GetType(aValue.TypeInfo) as TRTTIDynamicArrayType;
          try
            for i := 0 to aValue.GetArrayLength - 1 do
            begin
              jPair := Serialize(aName,GetValue(PPByte(aValue.GetReferenceToRawData)^ + rDynArray.ElementType.TypeSize * i, rDynArray.ElementType));
              try
                //jValue := TJsonValue(jPair.JsonValue.Clone);
                jValue := jPair.JsonValue;
                jArray.AddElement(jValue);
                jPair.JsonValue.Owned := False;
              finally
                jPair.Free;
                jValue.Owned := True;
              end;
            end;
            Result.JsonValue := jArray;
          finally
            ctx.Free;
          end;
        end;
      tkClass :
        begin
           Result.JsonValue := TJSONValue(Serialize(aValue.AsObject));
        end;
      tkString, tkLString, tkWString, tkUString :
        begin
          Result.JsonValue := TJSONString.Create(aValue.AsString);
        end;
      tkChar, tkWChar :
        begin
          Result.JsonValue := TJSONString.Create(aValue.AsString);
        end;
      tkInteger :
        begin
          Result.JsonValue := TJSONNumber.Create(aValue.AsInteger);
        end;
      tkInt64 :
        begin
          Result.JsonValue := TJSONNumber.Create(aValue.AsInt64);
        end;
      tkFloat :
        begin
          if aValue.TypeInfo = TypeInfo(TDateTime) then
          begin
            Result.JsonValue := TJSONString.Create(DateTimeToJsonDate(aValue.AsExtended));
          end
          else if aValue.TypeInfo = TypeInfo(TDate) then
          begin
            Result.JsonValue := TJSONString.Create(DateToStr(aValue.AsExtended));
          end
          else if aValue.TypeInfo = TypeInfo(TTime) then
          begin
            Result.JsonValue := TJSONString.Create(TimeToStr(aValue.AsExtended));
          end
          else Result.JsonValue := TJSONNumber.Create(aValue.AsExtended);
        end;
      tkEnumeration :
        begin
          if (aValue.TypeInfo = System.TypeInfo(Boolean)) then
          begin
            Result.JsonValue := TJSONBool.Create(aValue.AsBoolean);
          end
          else
          begin
            Result.JsonValue := TJSONString.Create(GetEnumName(aValue.TypeInfo,aValue.AsOrdinal));
            //Result.JsonValue := TJSONString.Create(aValue.ToString);
          end;
        end;
      tkSet :
        begin
          Result.JsonValue := TJSONString.Create(aValue.ToString);
        end;
      tkRecord :
        begin
          rRec := ctx.GetType(aValue.TypeInfo).AsRecord;
          try
            json := TJSONObject.Create;
            for rField in rRec.GetFields do
            begin
              json.AddPair(Serialize(rField.name,rField.GetValue(aValue.GetReferenceToRawData)));
            end;
            Result.JsonValue := json;
          finally
            ctx.Free;
          end;
        end;
      tkMethod, tkPointer, tkClassRef ,tkInterface, tkProcedure :
        begin
          //skip these properties
          FreeAndNil(Result);
        end
    else
      begin
        raise EJsonSerializeError.Create(Format(cNotSupportedDataType,[aName,GetTypeName(aValue.TypeInfo)]));
      end;
    end;
  except
    on E : Exception do
    begin
      Result.Free;
      raise EJsonSerializeError.CreateFmt('Serialize error class "%s.%s" : %s',[aName,aValue.ToString,e.Message]);
    end;
  end;
end;


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

end.


