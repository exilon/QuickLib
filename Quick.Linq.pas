{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.Linq
  Description : Arrays and Generic Lists Linq functions
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 04/04/2019
  Modified    : 01/12/2019

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

unit Quick.Linq;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  Generics.Collections,
  Generics.Defaults,
  RTTI,
  Quick.RTTI.Utils,
  Quick.Expression,
  Quick.Commons,
  Quick.Value,
  {$IFDEF FPC}
  Quick.Value.RTTI,
  {$ENDIF}
  Quick.Arrays;

type

  TOrderDirection = (odAscending, odDescending);

  ILinqQuery<T> = interface
  ['{16B68C0B-EA38-488A-99D9-BAD1E8560E8E}']
    function Where(const aWhereClause : string; aWhereValues : array of const) : ILinqQuery<T>; overload;
    function Where(const aWhereClause: string): ILinqQuery<T>; overload;
    function OrderBy(const aFieldNames : string) : ILinqQuery<T>;
    function OrderByDescending(const aFieldNames : string) : ILinqQuery<T>;
    function SelectFirst : T;
    function SelectLast : T;
    function SelectTop(aLimit : Integer) : TxArray<T>;
    function Select : TxArray<T>; overload;
    function Select(const aPropertyName : string) : TFlexArray; overload;
    function Count : Integer;
    function Update(const aFields : array of string; aValues : array of const) : Integer;
    function Delete : Integer;
  end;

  TLinqQuery<T : class> = class(TInterfacedObject,ILinqQuery<T>)
  private type
    arrayOfT = array of T;
  private
    fWhereClause : TExpression;
    fOrderBy : TArray<string>;
    fOrderDirection : TOrderDirection;
    //fPList : Pointer;
    fList : arrayOfT;
    function FormatParams(const aWhereClause : string; aWhereParams : array of const) : string;
    procedure DoOrderBy(vArray : ArrayOfT);
    function Compare(const aPropertyName : string; L, R : T) : Integer;
    procedure Clear;
  public
    {$IFNDEF FPC}
    constructor Create(aObjectList : TObjectList<T>); overload;
    {$ENDIF}
    constructor Create(aList : TList<T>); overload;
    constructor Create(aXArray : TxArray<T>); overload;
    constructor Create(aArray : TArray<T>); overload;
    destructor Destroy; override;
    function Where(const aWhereClause : string; aWhereParams : array of const) : ILinqQuery<T>; overload;
    function Where(const aWhereClause: string): ILinqQuery<T>; overload;
    function OrderBy(const aFieldNames : string) : ILinqQuery<T>;
    function OrderByDescending(const aFieldNames : string) : ILinqQuery<T>;
    function SelectFirst : T;
    function SelectLast : T;
    function SelectTop(aLimit : Integer) : TxArray<T>;
    function Select : TxArray<T>; overload;
    function Select(const aPropertyName : string) : TFlexArray; overload;
    function Count : Integer;
    function Update(const aFields : array of string; aValues : array of const) : Integer;
    function Delete : Integer;
  end;

  TLinq<T : class> = class
  public
    {$IFNDEF FPC}
    class function From(aObjectList : TObjectList<T>) : ILinqQuery<T>; overload;
    {$ENDIF}
    class function From(aArray : TArray<T>) : ILinqQuery<T>; overload;
    class function From(aXArray : TXArray<T>) : ILinqQuery<T>; overload;
  end;

  ELinqNotValidExpression = class(Exception);
  ELinqError = class(Exception);


implementation

{ TLinqQuery<T> }

procedure TLinqQuery<T>.Clear;
begin
  SetLength(fOrderBy,0);
end;

constructor TLinqQuery<T>.Create(aArray: TArray<T>);
begin
  Clear;
  fList := aArray;
end;

{$IFNDEF FPC}
constructor TLinqQuery<T>.Create(aObjectList: TObjectList<T>);
begin
  Clear;
  //Create(aObjectList.List);
  //fPList := Pointer(aObjectList.List);
  //fList := arrayOfT(fPList);
  fList := aObjectList.List;
end;
{$ENDIF}

constructor TLinqQuery<T>.Create(aXArray: TxArray<T>);
begin
  Clear;
  fList := aXArray;
end;

constructor TLinqQuery<T>.Create(aList: TList<T>);
begin
  Clear;
  fList := aList.ToArray;
end;

function TLinqQuery<T>.Compare(const aPropertyName: string; L, R: T): Integer;
var
  valueL : TValue;
  valueR : TValue;
begin
  Result := 0;
  valueL := TRTTI.GetPathValue(L,aPropertyName);
  valueR := TRTTI.GetPathValue(R,aPropertyName);
  if (valueL.IsEmpty) and (not valueR.IsEmpty) then Exit(1)
    else if (not valueL.IsEmpty) and (valueR.IsEmpty) then Exit(-1);

  case valueL.Kind of
    {$IFNDEF FPC}
    tkString,
    {$ENDIF}
    tkChar, tkWString, tkUString : Result := CompareText(valueL.AsString, valueR.AsString);
    tkInteger, tkInt64 :
      begin
        if valueL.AsInteger > valueR.AsInteger then Result := 1
          else if valueL.AsInteger < valueR.AsInteger then Result := -1;
      end;
    tkFloat :
      begin
        if valueL.AsExtended > valueR.AsExtended then Result := 1
          else if valueL.AsExtended < valueR.AsExtended then Result := -1;
      end;
    end;
end;

function TLinqQuery<T>.Count: Integer;
var
  i : Integer;
begin
  Result := 0;
  if fWhereClause = nil then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for i := High(fList) downto Low(fList) do
  begin
    if fWhereClause.Validate(fList[i]) then
    begin
      Inc(Result);
    end;
  end;
end;

function TLinqQuery<T>.Delete: Integer;
var
  i : Integer;
begin
  Result := 0;
  if fWhereClause = nil then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for i := High(fList) downto Low(fList) do
  begin
    if fWhereClause.Validate(fList[i]) then
    begin
      TObject(fList[i]).Free;
      //System.Delete(fList,i,1);
      Inc(Result);
    end;
  end;
end;

destructor TLinqQuery<T>.Destroy;
begin
  if Assigned(fWhereClause) then fWhereClause.Free;
  inherited;
end;

procedure TLinqQuery<T>.DoOrderBy(vArray : ArrayOfT);
begin
  if High(fOrderBy) < 0 then Exit;
  {$IFNDEF FPC}
  TArray.Sort<T>(vArray, TComparer<T>.Construct(
        function (const A, B: T): Integer
        var
          field : string;
        begin
          for field in fOrderBy do
          begin
            Result := Compare(field,A,B);
            if Result <> 0 then Break;
          end;
          if fOrderDirection = TOrderDirection.odDescending then Result := Result * -1;
        end)
      );
  {$ENDIF}
end;

function TLinqQuery<T>.OrderBy(const aFieldNames: string): ILinqQuery<T>;
begin
  Result := Self;
  if aFieldNames = '' then raise ELinqError.Create('No order fields specified!');
  fOrderBy := aFieldNames.Split([',']);
  fOrderDirection := TOrderDirection.odAscending;
end;

function TLinqQuery<T>.OrderByDescending(const aFieldNames: string): ILinqQuery<T>;
begin
  Result := Self;
  if aFieldNames = '' then raise ELinqError.Create('No order fields specified!');
  fOrderBy := aFieldNames.Split([',']);
  fOrderDirection := TOrderDirection.odDescending;
end;

function TLinqQuery<T>.Select(const aPropertyName: string): TFlexArray;
var
  obj : T;
  value : TFlexValue;
begin
  if fWhereClause = nil then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for obj in fList do
  begin
    if fWhereClause.Validate(obj) then
    begin
      //value := TRTTI.GetProperty(obj,aPropertyName);
      {$IFNDEF FPC}
      value := TRTTI.GetPathValue(obj,aPropertyName).AsVariant;
      {$ELSE}
      value.AsTValue := TRTTI.GetPathValue(obj,aPropertyName);
      {$ENDIF}
      Result.Add(value);
    end;
  end;
end;

function TLinqQuery<T>.Select: TxArray<T>;
var
  obj : T;
begin
  if fWhereClause = nil then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for obj in fList do
  begin
    if fWhereClause.Validate(obj) then Result.Add(obj);
  end;
  DoOrderBy(Result);
end;

function TLinqQuery<T>.SelectFirst: T;
var
  obj : T;
begin
  {$IFNDEF FPC}
  Result := nil;
  {$ENDIF}
  DoOrderBy(fList);
  if fWhereClause = nil then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for obj in fList do
  begin
    if fWhereClause.Validate(obj) then Exit(obj);
  end;
end;

function TLinqQuery<T>.SelectLast: T;
var
  obj : T;
begin
  {$IFNDEF FPC}
  Result := nil;
  {$ENDIF}
  DoOrderBy(fList);
  if fWhereClause = nil then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for obj in fList do
  begin
    if fWhereClause.Validate(obj) then Result := obj;
  end;
end;

function TLinqQuery<T>.SelectTop(aLimit: Integer): TxArray<T>;
var
  obj : T;
  i : Integer;
begin
  DoOrderBy(fList);
  if fWhereClause = nil then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  i := 0;
  for obj in fList do
  begin
    if fWhereClause.Validate(obj) then
    begin
      Result.Add(obj);
      Inc(i);
      if i > aLimit then Exit;
    end;
  end;
end;

function TLinqQuery<T>.Update(const aFields: array of string; aValues: array of const): Integer;
var
  obj : T;
  i : Integer;
  {$IFDEF FPC}
  value : TValue;
  {$ENDIF}
begin
  Result := 0;
  if fWhereClause = nil then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for obj in fList do
  begin
    {$IFNDEF FPC}
    if obj = nil then continue;
    {$ELSE}
    if Pointer(obj) = nil then continue;
    {$ENDIF}
    if fWhereClause.Validate(obj) then
    begin
      for i := Low(aFields) to High(aFields)  do
      begin
        if not TRTTI.PropertyExists(TypeInfo(T),aFields[i]) then raise ELinqError.CreateFmt('Linq update field "%s" not found in obj',[aFields[i]]);
        try
          {$IFNDEF FPC}
          TRTTI.SetPropertyValue(obj,aFields[i],aValues[i]);
          {$ELSE}
          case aValues[i].VType of
            vtString : value := string(aValues[i].VString^);
            vtChar : value := string(aValues[i].VChar);
            {$IFDEF MSWINDOWS}
            vtAnsiString : value := AnsiString(aValues[i].VAnsiString);
            vtWideString : value := WideString(aValues[i].VWideString);
            {$ENDIF}
            {$IFDEF UNICODE}
            vtUnicodeString: AsString := string(aValues[i].VUnicodeString);
            {$ENDIF UNICODE}
            vtInteger : value := aValues[i].VInteger;
            vtInt64 : value := aValues[i].VInt64^;
            vtExtended : value := aValues[i].VExtended^;
            vtBoolean : value := aValues[i].VBoolean;
           else raise Exception.Create('DataType not supported by Linq update');
          end;
          TRTTI.SetPropertyValue(obj,aFields[i],value);
          {$ENDIF}
        except
          on E : Exception do raise ELinqError.CreateFmt('Linq update error: %s',[e.Message]);
        end;
      end;
      Inc(Result);
    end;
  end;
end;

function TLinqQuery<T>.FormatParams(const aWhereClause : string; aWhereParams : array of const) : string;
var
  i : Integer;
begin
  Result := aWhereClause;
  if aWhereClause = '' then
  begin
    Result := '1 = 1';
    Exit;
  end;
  for i := 0 to aWhereClause.CountChar('?') - 1 do
  begin
    case aWhereParams[i].VType of
      vtInteger : Result := StringReplace(Result,'?',IntToStr(aWhereParams[i].VInteger),[]);
      vtInt64 : Result := StringReplace(Result,'?',IntToStr(aWhereParams[i].VInt64^),[]);
      vtExtended : Result := StringReplace(Result,'?',FloatToStr(aWhereParams[i].VExtended^),[]);
      vtBoolean : Result := StringReplace(Result,'?',BoolToStr(aWhereParams[i].VBoolean),[]);
      vtAnsiString : Result := StringReplace(Result,'?',string(aWhereParams[i].VAnsiString),[]);
      vtWideString : Result := StringReplace(Result,'?',string(aWhereParams[i].VWideString^),[]);
      {$IFNDEF NEXTGEN}
      vtString : Result := StringReplace(Result,'?',aWhereParams[i].VString^,[]);
      {$ENDIF}
      vtChar : Result := StringReplace(Result,'?',aWhereParams[i].VChar,[]);
      vtPChar : Result := StringReplace(Result,'?',string(aWhereParams[i].VPChar),[]);
    else Result := StringReplace(Result,'?', DbQuotedStr(string(aWhereParams[i].VUnicodeString)),[]);
    end;
  end;
end;

function TLinqQuery<T>.Where(const aWhereClause: string; aWhereParams: array of const): ILinqQuery<T>;
begin
  Result := Where(FormatParams(aWhereClause,aWhereParams));
end;

function TLinqQuery<T>.Where(const aWhereClause: string): ILinqQuery<T>;
begin
  Result := Self;
  try
    fWhereClause := TExpressionParser.Parse(aWhereClause);
  except
    on E : Exception do raise ELinqNotValidExpression.Create(e.Message);
  end;
end;

{ TLinq }

{$IFNDEF FPC}
class function TLinq<T>.From(aObjectList: TObjectList<T>): ILinqQuery<T>;
begin
  Result := TLinqQuery<T>.Create(aObjectList);
end;
{$ENDIF}

class function TLinq<T>.From(aArray: TArray<T>): ILinqQuery<T>;
begin
  Result := TLinqQuery<T>.Create(aArray);
end;

class function TLinq<T>.From(aXArray : TXArray<T>) : ILinqQuery<T>;
begin
  Result := TLinqQuery<T>.Create(aXArray);
end;


end.
