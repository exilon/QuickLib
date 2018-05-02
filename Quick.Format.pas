{ ***************************************************************************

  Copyright (c) 2016-2018 Kike Pérez

  Unit        : Quick.Format
  Description : String Format functions
  Author      : Kike Pérez
  Version     : 1.4
  Created     : 14/07/2017
  Modified    : 07/04/2018

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

unit Quick.Format;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  Math;

  //converts a number to thousand delimeter string
  function NumberToStr(const Number : Int64) : string;
  //convert bytes to KB, MB, TB...
  function FormatBytes(const aBytes : Int64; Spaced : Boolean = False) : string;

implementation


function NumberToStr(const Number : Int64) : string;
begin
  try
    Result := FormatFloat('0,',Number);
  except
    Result := '#Error';
  end;
end;

function FormatBytes(const aBytes : Int64; Spaced : Boolean = False) : string;
const
  mesure : array [0..8] of string = ('Byte(s)', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB');
var
  i : Integer;
  bfmt : string;
begin
  i := 0;
  while aBytes > Power(1024, i + 1) do Inc(i);
  if Spaced then bfmt := '%.2f %s'
    else bfmt := '%.2f%s';
  Result := Format(bfmt,[aBytes / IntPower(1024, i),mesure[i]]);
end;



end.
