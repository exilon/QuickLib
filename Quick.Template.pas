{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.Template
  Description : String Replace Templates
  Author      : Kike Pérez
  Version     : 2.0
  Created     : 01/04/2020
  Modified    : 30/06/2020

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
 
unit Quick.Template;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Quick.Commons;

type
  {$IFNDEF FPC}
  TTokenFunc = reference to function(const aToken : string) : string;
  {$ELSE}
  TTokenFunc = function(const aToken : string) : string of object;
  {$ENDIF}

  TStringTemplate = class
  private
    fVariables : TDictionary<string,string>;
    fReplaceFunc : TTokenFunc;
    fQuoteBegin : string;
    fQuoteEnd : string;
    fBeginOffSet : Integer;
    fEndOffSet : Integer;
  protected
    constructor Create; overload;
  public
    constructor Create(const aQuoteBegin, aQuoteEnd : string; aVariables : TDictionary<string,string>); overload;
    constructor Create(const aQuoteBegin, aQuoteEnd : string; aReplaceFunc : TTokenFunc); overload;
    function Replace(const aTemplate : string) : string; virtual;
  end;

  EStringTemplateError = class(Exception);

implementation

uses
  System.Classes, System.Types;

{ TStringTemplate }

constructor TStringTemplate.Create;
begin
  raise Exception.Create('Do not use this Constructor');
end;

constructor TStringTemplate.Create(const aQuoteBegin, aQuoteEnd: string; aVariables: TDictionary<string, string>);
begin
  inherited Create;
  if aQuoteBegin.IsEmpty or aQuoteEnd.IsEmpty then raise EStringTemplateError.Create('QuoteBegin and QuoteEnd cannot be null!');
  if aVariables = nil then raise EStringTemplateError.Create('Dictionary cannot be null!');
  fQuoteBegin := aQuoteBegin;
  fQuoteEnd := aQuoteEnd;
  fBeginOffSet := aQuoteBegin.Length;
  fEndOffSet := aQuoteEnd.Length;
  fVariables := aVariables;
end;

constructor TStringTemplate.Create(const aQuoteBegin, aQuoteEnd: string; aReplaceFunc: TTokenFunc);
begin
  inherited Create;
  if aQuoteBegin.IsEmpty or aQuoteEnd.IsEmpty then raise EStringTemplateError.Create('QuoteBegin and QuoteEnd cannot be null!');
  if not Assigned(aReplaceFunc) then raise EStringTemplateError.Create('ReplaceFunc cannot be null!');
  fQuoteBegin := aQuoteBegin;
  fQuoteEnd := aQuoteEnd;
  fBeginOffSet := aQuoteBegin.Length;
  fEndOffSet := aQuoteEnd.Length;
  fReplaceFunc := aReplaceFunc;
end;

function TStringTemplate.Replace(const aTemplate : string) : string;
var
  idx: Integer;
  st: Integer;
  et: Integer;
  token: string;
  tokrep: string;
  tokenArray: array of array [0..1] of integer;
  arrLen: integer;
begin
  result:='';
  st:=0;
  repeat
    st:=aTemplate.IndexOf(fQuoteBegin, st);
    if st > -1 then
    begin
      SetLength(tokenArray, Length(tokenArray) + 1);
      tokenArray[Length(tokenArray) - 1][0]:=st + fBeginOffSet;
      st:=st + fBeginOffSet + 1;
      et:=aTemplate.IndexOf(fQuoteEnd, st);
      if et = -1 then
        Break;
      tokenArray[Length(tokenArray) - 1][1]:=et - 1;
    end;
  until st = -1;

  arrLen:=Length(tokenArray);
  st:=0;
  for idx := 0 to arrLen - 1 do
  begin
    result:=result + aTemplate.Substring(st, tokenArray[idx][0] - fBeginOffSet - st);
    st:=tokenArray[idx][1] + fEndOffSet + 1;
    token:=aTemplate.Substring(tokenArray[idx][0], tokenArray[idx][1] - tokenArray[idx][0] + 1);
    //replace token
    tokrep := '';
    if fVariables <> nil then
    begin
      fVariables.TryGetValue(token,tokrep)
    end
    else
    begin
      if Assigned(fReplaceFunc) then
        tokrep := fReplaceFunc(token);
    end;
    if tokrep.IsEmpty then
      tokrep := fQuoteBegin + token + '?' + fQuoteEnd;
    Result := Result + tokrep;
  end;
  if arrLen > 0 then
    result:=result + aTemplate.SubString(tokenArray[arrLen - 1][1] + fEndOffSet + 1);
end;


end.
