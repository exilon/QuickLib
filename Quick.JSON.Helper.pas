{ ***************************************************************************

  Copyright (c) 2015-2020 Kike Pérez

  Unit        : Quick.JSON.Helper
  Description : Utils for working with JSON
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 27/01/2017
  Modified    : 16/01/2020

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

unit Quick.JSON.Helper;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  Quick.Json.Serializer;

type
  TObjectHelper = class helper for TObject
  public
    function ToJson : string;
    procedure FromJson(const json : string);
    procedure Clone(cObj : TObject);
  end;

  //only public properties will be cloned or gotten

var
  GlobalJsonIdenter : Boolean = True;


implementation

function TObjectHelper.ToJSON : string;
var
  Serializer : TJsonSerializer;
begin
  Result := '';
  try
    Serializer := TJsonSerializer.Create(TSerializeLevel.{$IFDEF FPC}slPublishedProperty{$ELSE}slPublicProperty{$ENDIF},True);
    try
      Result := Serializer.ObjectToJson(Self,GlobalJsonIdenter);
    finally
      Serializer.Free;
    end;
  except
    on e : Exception do raise Exception.Create(e.Message);
  end;
end;

procedure TObjectHelper.FromJson(const json :string);
var
  Serializer : TJsonSerializer;
begin
  try
    Serializer := TJsonSerializer.Create(TSerializeLevel.{$IFDEF FPC}slPublishedProperty{$ELSE}slPublicProperty{$ENDIF},True);
    try
      Serializer.JsonToObject(Self,json);
    finally
      Serializer.Free;
    end;
  except
    on e : Exception do raise Exception.Create(e.Message);
  end;
end;

procedure TObjectHelper.Clone(cObj : TObject);
begin
  cObj.FromJson(Self.ToJson);
end;


end.
