{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.Expression
  Description : Expression parser & validator
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 04/05/2019
  Modified    : 31/05/2019

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

unit Quick.Expression;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  StrUtils,
  RTTI,
  Quick.Commons,
  Quick.RTTI.Utils,
  Quick.Value,
  Quick.Value.RTTI;

type
  TOperator = (opNone, opEqual, opNotEqual, opGreater, opEqualOrGreater, opLower, opEqualOrLower, opLike, opLikeR, opLikeL);

  TCombine = (coNone, coAND, coOR, coXOR);

  TExpression = class
  private
    fCombine : TCombine;
  public
    property Combine : TCombine read fCombine write fCombine;
    function Validate(aValue : TObject) : Boolean; virtual; abstract;
    function IsNull : Boolean; virtual; abstract;
  end;

  TSingleExpression = class(TExpression)
  private
    fValue1 : string;
    fOperator : TOperator;
    fValue2 : string;
  public
    property Value1 : string read fValue1 write fValue1;
    property &Operator : TOperator read fOperator write fOperator;
    property Value2 : string read fValue2 write fValue2;
    function Validate(aValue : TObject) : Boolean; override;
    function IsNull : Boolean; override;
  end;

  TExpressionArray = array of TExpression;

  TMultiExpression = class(TExpression)
  private
    fArray : TExpressionArray;
  public
    destructor Destroy; override;
    property Items : TExpressionArray read fArray write fArray;
    function Validate(aValue : TObject) : Boolean; override;
    function IsNull : Boolean; override;
    procedure Add(aExpression : TExpression);
  end;

  TExpressionParser = class
  private
    class function IsSingleExpression(const aExpression : string) : Boolean;
    class function GetSingleExpression(const aExpression : string) : TSingleExpression;
    class function GetMultiExpression(const aExpression : string) : TMultiExpression;
    class function GetOperator(const aOperator : string) : TOperator;
    class function GetCombine(const aValue : string) : TCombine;
  public
    class function Parse(const aExpression : string) : TExpression;
    class function Validate(const obj : TObject; const aExpression : string) : Boolean;
  end;

  ENotValidExpression = class(Exception);
  EExpressionValidateError = class(Exception);

implementation

const
  OperatorStr : array[Low(TOperator)..TOperator.opLike] of string = ('none','=','<>','>','>=','<','<=','LIKE');
  {$IFDEF NEXTGEN}
  LOWSTR = 0;
  {$ELSE}
  LOWSTR = 1;
  {$ENDIF}

{ TExpressionParser }

//a > 10
//(a > 10) AND (b < 1)
//((a > 10) AND (b < 1)) OR (c = 10)

class function TExpressionParser.GetCombine(const aValue: string): TCombine;
begin
  if CompareText(aValue,'AND') = 0 then Result := TCombine.coAND
    else if CompareText(aValue,'OR') = 0 then Result := TCombine.coOR
    else if CompareText(aValue,'XOR') = 0 then Result := TCombine.coXOR;
end;

class function TExpressionParser.GetMultiExpression(const aExpression : string) : TMultiExpression;
var
  count : Integer;
  i : Integer;
  idx : Integer;
  exp : string;
  combine : string;
  rexp : TExpression;
  str : string;
begin
  i := LOWSTR;
  idx := 0;
  count := 0;
  Result := TMultiExpression.Create;
  exp := aExpression.TrimLeft;
  while not exp.IsEmpty do
  begin
    if exp[i] = '(' then
    begin
      Inc(count);
      if count = 1 then idx := i;
    end
    else if exp[i] = ')' then Dec(count);
    if (count = 0) and (idx > 0) then
    begin
      str := ExtractStr(exp,idx,i - idx +1);
      exp := exp.TrimLeft;
      if IsSingleExpression(str) then rexp := GetSingleExpression(str)
      else
      begin
        //remove outer parentesis
        if str.StartsWith('(') then str := Copy(str,LOWSTR + 1,str.Length - 2);
        rexp := GetMultiExpression(str);
      end;
      //get combine
      combine := ExtractStr(exp,LOWSTR,exp.IndexOf(' '));
      exp := exp.TrimLeft;
      rexp.Combine := GetCombine(combine);
      if (rexp.Combine = TCombine.coNone) and not (exp.IsEmpty) then raise ENotValidExpression.Create('Not valid expression defined!');
      //add to multiexpression
      Result.Add(rexp);
      idx := 0;
      i := -1;
    end;
    Inc(i);
  end;
end;

class function TExpressionParser.GetOperator(const aOperator: string): TOperator;
var
  op : TOperator;
begin
  for op := Low(TOperator) to High(TOperator) do
  begin
    if CompareText(OperatorStr[op],aOperator) = 0 then Exit(op);
  end;
  raise ENotValidExpression.Create('Not valid operator defined!');
end;

class function TExpressionParser.GetSingleExpression(const aExpression: string) : TSingleExpression;
var
  exp : string;
begin
  if aExpression.StartsWith('(') then exp := GetSubString(aExpression,'(',')')
    else exp := aExpression;
  Result := TSingleExpression.Create;
  Result.Value1 := ExtractStr(exp,LOWSTR,exp.IndexOf(' '));
  exp := exp.TrimLeft;
  Result.&Operator := GetOperator(ExtractStr(exp,LOWSTR,exp.IndexOf(' ')));
  Result.Value2 := UnDbQuotedStr(exp);
  //determine like
  if Result.&Operator = opLike then
  begin
    if Result.Value2.CountChar('%') = 2 then Result.Value2 := Copy(Result.Value2, 2, Result.Value2.Length - 2)
    else if Result.Value2.StartsWith('%') then
    begin
      Result.&Operator := TOperator.opLikeR;
      Result.Value2 := Copy(Result.Value2, 2, Result.Value2.Length);
    end
    else if Result.Value2.EndsWith('%') then
    begin
      Result.&Operator := TOperator.opLikeL;
      Result.Value2 := Copy(Result.Value2,LOWSTR,Result.Value2.Length - 1);
    end
    else raise ENotValidExpression.Create('Not valid Like specified!');
  end;
end;

class function TExpressionParser.IsSingleExpression(const aExpression: string): Boolean;
begin
  Result := (aExpression.CountChar('(') < 2) and (aExpression.CountChar(')') < 2);
end;

class function TExpressionParser.Parse(const aExpression : string) : TExpression;
var
  exp : string;
begin
  if aExpression.IsEmpty then raise ENotValidExpression.Create('Expression is empty');
  exp := aExpression.TrimLeft;
  //single expression or multiexpression
  if IsSingleExpression(exp) then Exit(GetSingleExpression(exp))
    else Result := GetMultiExpression(exp);
end;

class function TExpressionParser.Validate(const obj: TObject; const aExpression: string): Boolean;
var
  exp : TExpression;
begin
  exp := TExpressionParser.Parse(aExpression);
  try
    Result := exp.Validate(obj);
  finally
    exp.Free;
  end;
end;

{ TSingleExpression }

function TSingleExpression.IsNull: Boolean;
begin
  Result := (fValue1.IsEmpty) or (fValue2.IsEmpty);
end;

function TSingleExpression.Validate(aValue : TObject) : Boolean;
var
  value1 : TFlexValue;
  value2 : TFlexValue;
begin
  if aValue = nil then Exit;
  value1.AsTValue := TRTTI.GetPathValue(aValue,fValue1);
  case fOperator of
    TOperator.opEqual :
      begin
        if value1.IsString then Result := CompareText(value1,fValue2) = 0
          else Result := value1{$IFDEF FPC}.AsAnsiString{$ENDIF} = fValue2;
      end;
    TOperator.opNotEqual : Result := value1{$IFDEF FPC}.AsAnsiString{$ENDIF}  <> fValue2;
    TOperator.opGreater : Result := value1{$IFDEF FPC}.AsAnsiString{$ENDIF}  > fValue2;
    TOperator.opEqualOrGreater : Result := value1{$IFDEF FPC}.AsAnsiString{$ENDIF}  >= fValue2;
    TOperator.opLower : Result := value1{$IFDEF FPC}.AsAnsiString{$ENDIF}  < fValue2;
    TOperator.opEqualOrLower : Result := value1{$IFDEF FPC}.AsAnsiString{$ENDIF}  <= fValue2;
    TOperator.opLike : Result := {$IFNDEF FPC}ContainsText(value1,fValue2);{$ELSE}AnsiContainsText(value1.AsAnsiString,fValue2);{$ENDIF}
    TOperator.opLikeR : Result := EndsText(fValue2,value1);
    TOperator.opLikeL : Result := StartsText(fValue2,value1);
    else raise ENotValidExpression.Create('Operator not defined');
  end;
end;

{ TMultiExpression }

procedure TMultiExpression.Add(aExpression: TExpression);
begin
  fArray := fArray + [aExpression];
end;

destructor TMultiExpression.Destroy;
var
  exp : TExpression;
begin
  for exp in fArray do exp.Free;
  inherited;
end;

function TMultiExpression.IsNull: Boolean;
begin
  Result := High(fArray) < 0;
end;

function TMultiExpression.Validate(aValue : TObject) : Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := Low(fArray) to High(fArray) do
  begin
    if i = Low(fArray) then Result := fArray[i].Validate(aValue)
    else
    begin
      case fArray[i-1].Combine of
        TCombine.coAND : Result := Result and fArray[i].Validate(aValue);
        TCombine.coOR : Result := Result or fArray[i].Validate(aValue);
        TCombine.coXOR : Result := Result xor fArray[i].Validate(aValue);
        else Exit;
      end;
    end;
  end;
end;

end.
