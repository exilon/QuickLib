{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.MemoryCache.Serializer.Json
  Description : Cache Json serializer
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 14/07/2019
  Modified    : 02/11/2019

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

unit Quick.MemoryCache.Serializer.Json;

{$i QuickLib.inc}

interface

uses
  RTTI,
  //System.JSON.Serializers,
  Quick.Json.Serializer,
  Quick.MemoryCache.Types;

type

  TCacheJsonSerializer = class(TInterfacedObject,ICacheSerializer)
  private
    fJsonSerializer : TJsonSerializer;
  public
    constructor Create;
    destructor Destroy; override;
    function Serialize(aObject : TObject) : string; overload;
    function Serialize(aArray : TArray<string>) : string; overload;
    function Serialize(aArray: TArray<TObject>): string; overload;
    procedure Deserialize(const aValue : string; aObject : TObject); overload;
    procedure Deserialize(const aValue : string; var aArray : TArray<string>); overload;
    procedure Deserialize(const aValue : string; var aArray: TArray<TObject>); overload;
  end;


implementation

{ TCacheJsonSerializer }

constructor TCacheJsonSerializer.Create;
begin
  fJsonSerializer := TJsonSerializer.Create(TSerializeLevel.slPublicProperty,False);
  //fJsonSerializer := TJsonSerializer.Create;
end;

destructor TCacheJsonSerializer.Destroy;
begin
  fJsonSerializer.Free;
  inherited;
end;

function TCacheJsonSerializer.Serialize(aObject: TObject): string;
begin
  Result := fJsonSerializer.ObjectToJson(aObject,False);
  //Result := fJsonSerializer.Serialize<TObject>(aObject);
end;

function TCacheJsonSerializer.Serialize(aArray: TArray<string>): string;
begin
  Result := fJsonSerializer.ArrayToJson<string>(aArray);
end;

function TCacheJsonSerializer.Serialize(aArray: TArray<TObject>): string;
begin
  Result := fJsonSerializer.ArrayToJson<TObject>(aArray);
end;

procedure TCacheJsonSerializer.Deserialize(const aValue: string; aObject: TObject);
begin
  fJsonSerializer.JsonToObject(aObject,aValue);
  //aObject := fJsonSerializer.Deserialize<TObject>(aValue);
end;

procedure TCacheJsonSerializer.Deserialize(const aValue: string; var aArray: TArray<string>);
begin
  aArray := fJsonSerializer.JsonToArray<string>(aValue);
end;

procedure TCacheJsonSerializer.Deserialize(const aValue: string; var aArray: TArray<TObject>);
begin
  aArray := fJsonSerializer.JsonToArray<TObject>(aValue);
end;

end.
