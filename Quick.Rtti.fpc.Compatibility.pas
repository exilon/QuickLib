{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.RTTI.fpc.Compatibility (only freepascal)
  Description : Delphi compatibility RTTI functions
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 20/08/2018
  Modified    : 25/08/2019

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

unit Quick.Rtti.fpc.Compatibility;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  Rtti;

type

TValueHelper = record helper for TValue
  function AsVariant : Variant;
end;

implementation

{ TValueHelper }

function TValueHelper.AsVariant: Variant;
begin
  case Kind of
    tkShortString, tkWideString, tkAnsiString, tkUString : Result := AsString;
    tkInteger : result := IntToStr(AsInteger);
    tkInt64 : Result := IntToStr(AsInt64);
    tkBool : Result := BoolToStr(AsBoolean, True);
    tkFloat : Result := FloatToStr(AsExtended);
  else
    Result := '';
  end;
end;

end.
