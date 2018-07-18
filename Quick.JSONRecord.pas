{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.JSONRecord
  Description : Serializable class
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 05/05/2018
  Modified    : 08/07/2018

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
  Rest.Json.Types;

type

  IJsonable = interface
  ['{AF71F59C-89A5-4BFB-8227-0CC3068B7671}']
    procedure FromJson(const aJson : string);
    function ToJson : string;
  end;

  TJsonRecord = class(TInterfacedObject,IJsonable)
    constructor CreateFromJson(const aJson : string);
    procedure FromJson(const aJson : string);
    function ToJson : string;
  end;

implementation

{ TJsonRecord }

constructor TJsonRecord.CreateFromJson(const aJson: string);
var
  serializer : TJsonSerializer;
begin
  {$IFNDEF FPC}
  serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
  {$ELSE}
  serializer := TJsonSerializer.Create;
  {$ENDIF}
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
  {$IFNDEF FPC}
  serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
  {$ELSE}
  serializer := TJsonSerializer.Create;
  {$ENDIF}
  try
    serializer.JsonToObject(Self,aJson);
  finally
    serializer.Free;
  end;
end;

function TJsonRecord.ToJson: string;
var
  serializer : TJsonSerializer;
begin
  {$IFNDEF FPC}
  serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
  {$ELSE}
  serializer := TJsonSerializer.Create;
  {$ENDIF}
  try
    Result := serializer.ObjectToJson(Self);
  finally
    serializer.Free;
  end;
end;

end.
