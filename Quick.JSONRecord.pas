{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.JSONRecord
  Description : Serializable class
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 05/05/2018
  Modified    : 28/08/2018

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
unit Quick.JSONRecord;

{$i QuickLib.inc}

interface

uses
  Quick.Json.Serializer,
  Quick.AutoMapper;

type

  IJsonable = interface
  ['{AF71F59C-89A5-4BFB-8227-0CC3068B7671}']
    procedure FromJson(const aJson : string);
    function ToJson : string;
    procedure MapTo(aTgtObj : TObject);
    procedure MapFrom(aSrcObj : TObject);
  end;

  TJsonRecord = class(TInterfacedObject,IJsonable)
  public
    constructor CreateFromJson(const aJson : string);
    procedure FromJson(const aJson : string);
    function ToJson : string;
    function Map<T : class, constructor> : T;
    procedure MapTo(aTgtObj : TObject);
    procedure MapFrom(aSrcObj : TObject);
    function Clone : TObject; virtual;
  end;

implementation

{ TJsonRecord }

constructor TJsonRecord.CreateFromJson(const aJson: string);
var
  serializer : TJsonSerializer;
begin
  //inherited Create;
  serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
  try
    serializer.JsonToObject(Self,aJson);
  finally
    serializer.Free;
  end;
end;

procedure TJsonRecord.FromJson(const aJson: string);
var
  serializer : TJsonSerializer;
begin
  serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
  try
    serializer.JsonToObject(Self,aJson);
  finally
    serializer.Free;
  end;
end;

function TJsonRecord.Map<T> : T;
begin
  Result := TMapper<T>.Map(Self);
end;

procedure TJsonRecord.MapFrom(aSrcObj: TObject);
begin
  TObjMapper.Map(aSrcObj,Self);
end;

procedure TJsonRecord.MapTo(aTgtObj: TObject);
begin
  TObjMapper.Map(Self,aTgtObj);
end;

function TJsonRecord.ToJson: string;
var
  serializer : TJsonSerializer;
begin
  serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
  try
    Result := serializer.ObjectToJson(Self);
  finally
    serializer.Free;
  end;
end;

function TJsonRecord.Clone : TObject;
begin
  Result := Self.ClassType.Create;
  TObjMapper.Map(Self,Result);
end;

end.
