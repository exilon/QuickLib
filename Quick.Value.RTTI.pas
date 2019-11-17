{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.Value.RTTI
  Description : FlexValue Helper for RTTI
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 06/05/2019
  Modified    : 30/08/2019

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
unit Quick.Value.RTTI;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  Rtti,
  Quick.Value;

type
  TRTTIFlexValue = record helper for TFlexValue
  private
    function CastToTValue: TValue;
    procedure SetAsTValue(const Value: TValue);
  public
    property AsTValue : TValue read CastToTValue write SetAsTValue;
    function AsType<T : class> : T;
  end;

implementation

{ TRTTIFlexValue }

function TRTTIFlexValue.AsType<T>: T;
begin
  Result := T(AsObject);
end;

function TRTTIFlexValue.CastToTValue: TValue;
begin
  try
    case DataType of
      dtNull : Result := TValueExtended;
      dtBoolean : Result := AsBoolean;
      dtString : Result := AsString;
      {$IFDEF MSWINDOWS}
      dtAnsiString : Result := AsAnsiString;
      dtWideString : Result := AsWideString;
      {$ENDIF}
      dtInteger,
      dtInt64 : Result := AsInt64;
      {$IFNDEF FPC}
      dtVariant : Result := TValue.FromVariant(AsVariant);
      {$ENDIF}
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to TValue error: %s',[e.message]);
  end;
end;

procedure TRTTIFlexValue.SetAsTValue(const Value: TValue);
begin
  Clear;
  case Value.Kind of
    tkInteger,
    tkInt64 : AsInt64 := Value.AsInt64;
    tkFloat : AsExtended := Value.AsExtended;
    tkChar,
    {$IFNDEF FPC}
    tkString,
    tkUstring,
    {$ELSE}
    tkAstring,
    {$ENDIF}
    tkWideString,
    tkWideChar : AsString := Value.AsString;
    tkEnumeration,
    tkSet : AsInteger := Value.AsInteger;
    {$IFNDEF FPC}
    else AsVariant := Value.AsVariant;
    {$ENDIF}
  end;
end;

end.
