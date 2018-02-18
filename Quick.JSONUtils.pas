{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.JSONUtils
  Description : Utils for working with JSON
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 27/01/2017
  Modified    : 13/02/2018

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

unit Quick.JSONUtils;

interface

uses
  Classes,
  System.SysUtils,
  {$IF CompilerVersion >= 32.0}
    JSON.Types,
    REST.Json,
    System.JSON,
    JSON.Serializers;
  {$ELSE}
    System.JSON,
    REST.JSON,
    Rest.Json.Types;
  {$ENDIF}

type
  TObjectHelper = class helper for TObject
  public
    function ToJson : string;
    procedure FromJson(const json : string);
    procedure Clone(cObj : TObject);
  end;

  //only public properties will be cloned or gotten

  function IncludeJsonBraces(const json : string) : string;
  function RemoveJsonBraces(const json : string) : string;


implementation

function TObjectHelper.ToJSON : string;
{$IF CompilerVersion >= 32.0}
var
    Serializer : TJsonSerializer;
  {$ENDIF}
begin
  Result := '';
  try
    {$IF CompilerVersion >= 32.0}
      Serializer := TJsonSerializer.Create;
      try
        Serializer.Formatting := TJsonFormatting.Indented;
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

function IncludeJsonBraces(const json : string) : string;
begin
  if (not json.StartsWith('{')) and (not json.EndsWith('}')) then Result := '{' + json + '}'
    else Result := json;
end;

function RemoveJsonBraces(const json : string) : string;
begin
  if (json.StartsWith('{')) and (json.EndsWith('}')) then Result := Copy(json,2,Json.Length - 2)
    else Result := json;
end;


end.
