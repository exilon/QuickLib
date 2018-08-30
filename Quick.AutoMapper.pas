{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.AutoMapper
  Description : Auto Mapper object properties
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 25/08/2018
  Modified    : 30/08/2018

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
  {$IFDEF FPC}
  typinfo,
  {$ENDIF}
  RTTI;

type

  TCustomMapping = class
  private
    fMapDictionary : TDictionary<string,string>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMap(const aName, aMapName : string);
    function GetMap(const aName : string; out vMapName : string) : Boolean;
  end;

  TObjMapper = class
  public
    class procedure Map(aSrcObj : TObject; aTgtObj : TObject; aCustomMapping: TCustomMapping = nil);
  end;

  TMapper<T : class, constructor> = class
  public
    class function Map(aSrcObj : TObject; aCustomMapping: TCustomMapping = nil): T; overload;
    class procedure Map(aSrcObj : TObject; aTgtObj : T; aCustomMapping : TCustomMapping = nil); overload;
  end;

  TAutoMapper<TClass1, TClass2 : class, constructor> = class
  private
    fCustomMapping : TCustomMapping;
  public
    constructor Create;
    destructor Destroy; override;
    property CustomMapping : TCustomMapping read fCustomMapping write fCustomMapping;
    function Map(aSrcObj : TClass1) : TClass2; overload;
    {$IFNDEF FPC}
    function Map(aSrcObj : TClass2) : TClass1; overload;
    {$ELSE}
    //freepascal detects overload with generic types as duplicated function, added dummy field to avoid this
    function Map(aSrcObj : TClass2; dummy : Boolean = True) : TClass1; overload;
    {$ENDIF}
  end;

  EAutoMapperError = class(Exception);

implementation

{ TObjMapper }

class procedure TObjMapper.Map(aSrcObj : TObject; aTgtObj : TObject; aCustomMapping: TCustomMapping = nil);
var
  ctx : TRttiContext;
  rType : TRttiType;
  tgtprop : TRttiProperty;
  mapname : string;
  obj : TObject;
begin
  if aTgtObj = nil then aTgtObj := aTgtObj.ClassType.Create;

  for tgtprop in ctx.GetType(aTgtObj.ClassInfo).GetProperties do
  begin
    if tgtprop.IsWritable then
    begin
      if not tgtprop.PropertyType.IsInstance then
      begin
        rType := ctx.GetType(aSrcObj.ClassInfo);
        if Assigned(aCustomMapping) then
        begin
          if aCustomMapping.GetMap(tgtprop.Name,mapname) then
          begin
            if rType.GetProperty(mapname) = nil then raise EAutoMapperError.CreateFmt('No valid custom mapping (Source: %s - Target: %s)',[mapname,tgtprop.Name]);
            {$IFNDEF FPC}
            tgtprop.SetValue(aTgtObj,rType.GetProperty(mapname).GetValue(aSrcObj))
            {$ELSE}
            SetPropValue(aTgtObj,tgtprop.Name,GetPropValue(aSrcObj,mapname));
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
              on E : Exception do raise EAUtoMapperError.CreateFmt('Error mapping property "%s" : %s',[tgtprop.Name,e.message]);
            end;
          end;
        end
        else
        begin
          try
            {$IFNDEF FPC}
            if rType.GetProperty(tgtprop.Name) <> nil then tgtprop.SetValue(aTgtObj,rType.GetProperty(tgtprop.Name).GetValue(aSrcObj));
            {$ELSE}
            if rType.GetProperty(tgtprop.Name) <> nil then SetPropValue(aTgtObj,tgtprop.Name,GetPropValue(aSrcObj,tgtprop.Name));
            {$ENDIF}
          except
            on E : Exception do raise EAUtoMapperError.CreateFmt('Error mapping property "%s" : %s',[tgtprop.Name,e.message]);
          end;
        end;
      end
      else
      begin
        obj := tgtprop.GetValue(aTgtObj).AsObject;
        {$IFNDEF FPC}
        if obj = nil then obj := TObject.Create;
        {$ELSE}
        if obj = nil then obj := GetObjectProp(aSrcObj,tgtprop.Name).ClassType.Create;
        {$ENDIF}

        if obj <> nil then
        begin
          {$IFNDEF FPC}
          TObjMapper.Map(rType.GetProperty(tgtprop.Name).GetValue(aSrcObj).AsObject,obj,aCustomMapping);
          {$ELSE}
          TObjMapper.Map(GetObjectProp(aSrcObj,tgtprop.Name),obj,aCustomMapping);
          SetObjectProp(aTgtObj,tgtprop.Name,obj);
          {$ENDIF}
        end
        else raise EAutoMapperError.CreateFmt('Target object "%s" not autocreated by class',[tgtprop.Name]);
      end;
    end;
  end;
end;

class function TMapper<T>.Map(aSrcObj : TObject; aCustomMapping: TCustomMapping = nil) : T;
var
  obj : T;
begin
  obj := T.Create;
  TObjMapper.Map(aSrcObj,obj,aCustomMapping);
  Result := obj;
end;

class procedure TMapper<T>.Map(aSrcObj : TObject; aTgtObj : T; aCustomMapping : TCustomMapping = nil);
begin
  TObjMapper.Map(aSrcObj, aTgtObj, aCustomMapping);
end;

{ TAutoMapper<TClass1, TClass2> }

constructor TAutoMapper<TClass1, TClass2>.Create;
begin
  fCustomMapping := TCustomMapping.Create;
end;

destructor TAutoMapper<TClass1, TClass2>.Destroy;
begin
  if Assigned(fCustomMapping) then fCustomMapping.Free;
  inherited;
end;

{}
function TAutoMapper<TClass1, TClass2>.Map(aSrcObj: TClass1): TClass2;
begin
  Result := TMapper<TClass2>.Map(aSrcObj,fCustomMapping);
end;

{$IFNDEF FPC}
function TAutoMapper<TClass1, TClass2>.Map(aSrcObj: TClass2): TClass1;
{$ELSE}
function TAutoMapper<TClass1, TClass2>.Map(aSrcObj: TClass2; dummy : Boolean = True): TClass1;
{$ENDIF}
begin
  Result := TMapper<TClass1>.Map(aSrcObj,fCustomMapping);
end;

{ TCustomMappingFields }

procedure TCustomMapping.AddMap(const aName, aMapName: string);
begin
  //add map fields
  fMapDictionary.Add(aName,aMapName);
  //add reverse lookup
  fMapDictionary.Add(aMapName,aName);
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

end.
