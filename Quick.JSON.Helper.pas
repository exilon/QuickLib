{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.JSON.Helper
  Description : Utils for working with JSON
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 27/01/2017
  Modified    : 09/05/2018

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
  System.SysUtils,
  {$IFDEF DELPHIRX102_UP}
    JSON.Types,
    REST.Json,
    System.JSON,
    JSON.Serializers,
  {$ELSE}
    System.JSON,
    REST.JSON,
    Rest.Json.Types,
  {$ENDIF}
  Quick.Json.Utils;

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
{$IFDEF DELPHIRX102_UP}
var
    Serializer : TJsonSerializer;
  {$ENDIF}
begin
  Result := '';
  try
    {$IFDEF DELPHIRX102_UP}
      Serializer := TJsonSerializer.Create;
      try
        if GlobalJsonIdenter then Serializer.Formatting := TJsonFormatting.Indented;
        Result := Serializer.Serialize<TObject>(Self);
      finally
        Serializer.Free;
      end;
    {$ELSE}
      Result := TJson.ObjectToJsonString(Self);
    {$ENDIF}
  except
    on e : Exception do raise Exception.Create(e.Message);
  end;
end;

procedure TObjectHelper.FromJson(const json :string);
var
  jObj : TJSONObject;
begin
  try
    jObj := TJSonObject.ParseJSONValue(json,true) as TJSONObject;
    try
      TJson.JsonToObject(self,jObj);
    finally
      jObj.Free;
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
