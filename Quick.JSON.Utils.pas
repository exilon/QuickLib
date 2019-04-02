{ ***************************************************************************

  Copyright (c) 2015-2018 Kike P�rez

  Unit        : Quick.JSON.Utils
  Description : Json utils
  Author      : Kike P�rez
  Version     : 1.4
  Created     : 21/09/2018
  Modified    : 24/09/2018

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

unit Quick.JSON.Utils;

{$i QuickLib.inc}

interface

uses
  SysUtils;

type

  TJsonUtils = class
  public
    class function IncludeJsonBraces(const json : string) : string;
    class function RemoveJsonBraces(const json : string) : string;
    class function JsonFormat(const json : string): string;
  end;

implementation

class function TJsonUtils.IncludeJsonBraces(const json : string) : string;
begin
  if (not json.StartsWith('{')) and (not json.EndsWith('}')) then Result := '{' + json + '}'
    else Result := json;
end;

class function TJsonUtils.RemoveJsonBraces(const json : string) : string;
begin
  if (json.StartsWith('{')) and (json.EndsWith('}')) then Result := Copy(json,2,Json.Length - 2)
    else Result := json;
end;


class function TJsonUtils.JsonFormat(const json : string) : string;
const
  EOL = #13#10;
  INDENT = '    ';
  SPACE = ' ';
var
  c : char;
  LIndent : string;
  isEOL : Boolean;
  isIntoString : Boolean;
  isEscape : Boolean;
  i : Integer;
begin
  Result := '';
  isEOL := True;
  isIntoString := False;
  isEscape := False;
  {$IFNDEF NEXTGEN}
  for i := 1 to json.Length do
  {$ELSE}
  for i := 0 to json.Length - 1 do
  {$ENDIF}
  begin
    c := json[i];
    if isIntoString then
    begin
      isEOL := False;
      Result := Result + c;
    end
    else
    begin
      case c of
        ':' : Result := Result + c + SPACE;
        '{','[' :
          begin
            LIndent := LIndent + INDENT;
            if json[i+1] in ['}',']'] then Result := Result + c
              else Result := Result + c + EOL + LIndent;
            isEOL := True;
          end;
        ',' :
          begin
            isEOL := False;
            Result := Result + c + EOL + LIndent;
          end;
        '}',']' :
          begin
            Delete(LIndent, 1, Length(INDENT));
            if not isEOL then Result := Result + EOL;
            if json[i+1] = ',' then Result := Result + LIndent + c
              else if json[i-1] in ['{','['] then Result := Result + c + EOL
                else Result := Result + LIndent + c + EOL;
            isEOL := True;
          end;
        else
          begin
            isEOL := False;
            Result := Result + c;
          end;
      end;
    end;
    isEscape := (c = '\') and not isEscape;
    if not isEscape and (c = '"') then isIntoString := not isIntoString;
  end;
end;

end.
