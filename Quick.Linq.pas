{ ***************************************************************************

  Copyright (c) 2016-2022 Kike Pérez

  Unit        : Quick.Linq
  Description : Arrays and Generic Lists Linq functions
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 04/04/2019
  Modified    : 27/01/2022

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
  {$ELSE}
  System.RegularExpressions,
  {$ENDIF}
  Quick.Arrays;

type

  TOrderDirection = (odAscending, odDescending);

  {$IFNDEF FPC}
  TLinqExpression<T : class> = class(TExpression)
  private
    fPredicate : TPredicate<T>;
  public
    constructor Create(aPredicate : TPredicate<T>);
    function Validate(const aValue : TValue) : Boolean; override;
    function IsNull : Boolean; override;
  end;
  {$ENDIF}

  ILinqArray<T> = interface
  ['{3133DCAB-06C5-434B-B169-B32DC8C6308B}']
    function Any : Boolean; overload;
    function Any(const aMatchString : string; aUseRegEx : Boolean) : Boolean; overload;
    function Where(const aMatchString : string; aUseRegEx : Boolean) : ILinqArray<T>; overload;
    function OrderAscending : ILinqArray<T>;
    function OrderDescending : ILinqArray<T>;
    function SelectFirst : T;
    function SelectLast : T;
    function SelectTop(aLimit : Integer) : TArray<T>;
    function Select : TArray<T>; overload;
    function Count : Integer;
    function Update(const aNewValue : T) : Integer;
    function Delete : Integer;
  end;

  {$IFNDEF FPC}
  TLinqArray<T> = class(TInterfacedObject,ILinqArray<T>)
  type
    TLinqComparer = class(TInterfacedObject,IComparer<T>)
    private
      fSortAscending : Boolean;
    public
      constructor Create(aSortAscending : Boolean);
      property SortAscending : Boolean read fSortAscending;
      function Compare(const L, R: T): Integer;
    end;
  private
    fArray : TArray<T>;
    fMatchString : string;
    fUseRegEx : Boolean;
    function Validate(aValue : T) : Boolean;
  public
    constructor Create(aArray : TArray<T>);
    function Any : Boolean; overload;
    function Any(const aMatchString : string; aUseRegEx : Boolean) : Boolean; overload;
    function Where(const aMatchString : string; aUseRegEx : Boolean) : ILinqArray<T>; overload;
    function OrderAscending : ILinqArray<T>;
    function OrderDescending : ILinqArray<T>;
    function SelectFirst : T;
    function SelectLast : T;
    function SelectTop(aLimit : Integer) : TArray<T>;
    function Select : TArray<T>; overload;
    function Count : Integer;
    function Update(const aNewValue : T) : Integer;
    function Delete : Integer;
  end;

  TLinqArrayHelper = record helper for TArray<string>
    function Add(const aValue : string) : Integer;
    function AddIfNotExists(const aValue : string) : Integer;
    function Remove(const aValue : string) : Boolean;
    function Any : Boolean; overload;
    function Any(const aValue : string) : Boolean; overload;
    function Any(const aMatchString : string; aUseRegEx : Boolean) : Boolean; overload;
    function Where(const aMatchString : string; aUseRegEx : Boolean) : ILinqArray<string>; overload;
  end;
  {$ENDIF}

  ILinqQuery<T> = interface
  ['{16B68C0B-EA38-488A-99D9-BAD1E8560E8E}']
    function Where(const aWhereClause : string; aWhereValues : array of const) : ILinqQuery<T>; overload;
    function Where(const aWhereClause: string): ILinqQuery<T>; overload;
    {$IFNDEF FPC}
    function Where(aPredicate : TPredicate<T>) : ILinqQuery<T>; overload;
    {$ENDIF}
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
    TArrType = (atArray, atXArray, atList, atObjectList);
  private
    fWhereClause : TExpression;
    fOrderBy : TArray<string>;
    fOrderDirection : TOrderDirection;
    fPList : Pointer;
    fList : arrayOfT;
    fArrType : TArrType;
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
    {$IFNDEF FPC}
    function Where(aPredicate : TPredicate<T>) : ILinqQuery<T>; overload;
    {$ENDIF}
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
  fPList := Pointer(aArray);
  fList := aArray;
  fArrType := TArrType.atArray;
end;

{$IFNDEF FPC}
constructor TLinqQuery<T>.Create(aObjectList: TObjectList<T>);
begin
  Clear;
  fPList := Pointer(aObjectList);
  {$IFDEF DELPHIRX104_UP}
  fList := aObjectList.PList^;
  {$ELSE}
  fList := aObjectList.List;
  {$ENDIF}
  fArrType := TArrType.atObjectList;
end;
{$ENDIF}

constructor TLinqQuery<T>.Create(aXArray: TxArray<T>);
begin
  Clear;
  fPList := Pointer(aXArray);
  fList := aXArray.PArray^;
  fArrType := TArrType.atXArray;
end;

constructor TLinqQuery<T>.Create(aList: TList<T>);
begin
  Clear;
  fPList := Pointer(aList);
  {$IFDEF DELPHIRX104_UP}
  fList := aList.PList^;
  {$ELSE}
  fList := aList.ToArray;
  {$ENDIF}
  fArrType := TArrType.atList;
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
      case fArrType of
        TArrType.atArray, TArrType.atXArray :
          begin
            TObject(fList[i]).Free;
            System.Delete(fList,i,1);
            //fPList := Pointer(fList);
          end;
        TArrType.atList :
          begin
            TList<T>(fPList).Delete(i);
          end;
        TArrType.atObjectList :
          begin
            TObjectList<T>(fPList).Delete(i);
          end;
      end;
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
    {$IFNDEF FPC}
    if obj = nil then continue;
    {$ELSE}
    if Pointer(obj) = nil then continue;
    {$ENDIF}
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
  {$If Defined(FPC) OR Defined(DELPHIRX102_UP)}
  Result := [];
  {$ELSE}
  Result := nil;
  {$ENDIF}
  if fWhereClause = nil then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for obj in fList do
  begin
    {$IFNDEF FPC}
    if obj = nil then continue;
    {$ELSE}
    if Pointer(obj) = nil then continue;
    {$ENDIF}
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
    {$IFNDEF FPC}
    if obj = nil then continue;
    {$ELSE}
    if Pointer(obj) = nil then continue;
    {$ENDIF}
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
    {$IFNDEF FPC}
    if obj = nil then continue;
    {$ELSE}
    if Pointer(obj) = nil then continue;
    {$ENDIF}
    if fWhereClause.Validate(obj) then Result := obj;
  end;
end;

function TLinqQuery<T>.SelectTop(aLimit: Integer): TxArray<T>;
var
  obj : T;
  i : Integer;
begin
  {$If Defined(FPC) OR Defined(DELPHIRX102_UP)}
  Result := [];
  {$ELSE}
  Result := nil;
  {$ENDIF}
  DoOrderBy(fList);
  if fWhereClause = nil then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  i := 0;
  for obj in fList do
  begin
    {$IFNDEF FPC}
    if obj = nil then continue;
    {$ELSE}
    if Pointer(obj) = nil then continue;
    {$ENDIF}
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
      vtString : Result := StringReplace(Result,'?',string(aWhereParams[i].VString^),[]);
      {$ENDIF}
      vtChar : Result := StringReplace(Result,'?',string(aWhereParams[i].VChar),[]);
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

{$IFNDEF FPC}
function TLinqQuery<T>.Where(aPredicate: TPredicate<T>): ILinqQuery<T>;
begin
  Result := Self;
  fWhereClause := TLinqExpression<T>.Create(aPredicate);
end;
{$ENDIF}

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

{ TLinqExpression<T> }

{$IFNDEF FPC}
constructor TLinqExpression<T>.Create(aPredicate: TPredicate<T>);
begin
  fPredicate := aPredicate;
end;

function TLinqExpression<T>.Validate(const aValue : TValue) : Boolean;
begin
  Result := fPredicate(aValue.AsType<T>);
end;

function TLinqExpression<T>.IsNull : Boolean;
begin
  Result := not Assigned(fPredicate);
end;

{ TLinqArray<T> }

function TLinqArray<T>.Any : Boolean;
begin
  Result := High(fArray) >= 0;
end;

function TLinqArray<T>.Any(const aMatchString: string; aUseRegEx: Boolean): Boolean;
begin
  fMatchString := aMatchString;
  fUseRegEx := aUseRegEx;
end;

function TLinqArray<T>.Count: Integer;
begin
  Result := High(fArray) + 1;
end;

constructor TLinqArray<T>.Create(aArray: TArray<T>);
begin
  {$IFDEF DELPHIRX104_UP}
  Pointer(fArray) := aArray;
  {$ELSE}
  fArray := aArray;
  {$ENDIF}
end;

function TLinqArray<T>.Delete: Integer;
var
  i : Integer;
  {$IFNDEF DELPHIXE7_UP}
  n : Integer;
  len : Integer;
  {$ENDIF}
begin
  Result := 0;
  if fMatchString.IsEmpty then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for i := High(fArray) downto Low(fArray) do
  begin
    if Validate(fArray[i]) then
    begin
      //TObject(fArray[i]).Free;
      {$IFDEF DELPHIXE7_UP}
      System.Delete(fArray,i,1);
      {$ELSE}
      len := Length(fArray);
      if (len > 0) and (i < len) then
      begin
        for n := i + 1 to len - 1 do fArray[n - 1] := fArray[n];
        SetLength(fArray, len - 1);
      end;
      {$ENDIF}
      Inc(Result);
    end;
  end;
end;

function TLinqArray<T>.OrderAscending: ILinqArray<T>;
var
  comparer : IComparer<T>;
begin
  comparer := TLinqComparer.Create(True);
  TArray.Sort<T>(fArray,comparer);
end;

function TLinqArray<T>.OrderDescending: ILinqArray<T>;
var
  comparer : IComparer<T>;
begin
  comparer := TLinqComparer.Create(False);
  TArray.Sort<T>(fArray,comparer);
end;

function TLinqArray<T>.Select: TArray<T>;
var
  value : T;
begin
  Result := [];
  //DoOrderBy(fList);
  if fMatchString.IsEmpty then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for value in fArray do
  begin
    if Validate(value) then Result := Result + [value];
  end;
end;

function TLinqArray<T>.SelectFirst: T;
var
  value : T;
begin
  //DoOrderBy(fList);
  if fMatchString.IsEmpty then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for value in fArray do
  begin
    if Validate(value) then Exit(value);
  end;
end;

function TLinqArray<T>.SelectLast: T;
var
  value : T;
  found : T;
begin
  //DoOrderBy(fList);
  if fMatchString.IsEmpty then raise ELinqNotValidExpression.Create('Not valid expression defined!');
  for value in fArray do
  begin
    if Validate(value) then found := value;
  end;
  Result := found;
end;

function TLinqArray<T>.SelectTop(aLimit: Integer): TArray<T>;
var
  i : Integer;
  limit : Integer;
begin
  Result := [];
  if aLimit > High(fArray) then limit := High(fArray)
    else limit := aLimit;
  SetLength(Result,limit);
  for i := Low(fArray) to limit do
  begin
    Result[i] := fArray[i];
  end;
end;

function TLinqArray<T>.Update(const aNewValue: T): Integer;
var
  i : Integer;
begin
  for i := Low(fArray) to High(fArray) do
  begin
    if Validate(fArray[i]) then fArray[i] := aNewValue;
  end;
end;

function TLinqArray<T>.Validate(aValue: T): Boolean;
var
  regEx : TRegEx;
  value : TValue;
begin
  value := TValue.From<T>(aValue);
  if fUseRegEx then
  begin
    regEx := TRegEx.Create(fMatchString,[roIgnoreCase,roMultiline]);
    try
      Result := regEx.IsMatch(value.AsString);
    except
      raise Exception.Create('TLinqArray not valid RegEx!');
    end;
  end
  else
  begin
    Result := CompareText(fMatchString,value.AsString) = 0;
  end;
end;

function TLinqArray<T>.Where(const aMatchString: string; aUseRegEx: Boolean): ILinqArray<T>;
begin
  Result := Self;
  fMatchString := aMatchString;
  fUseRegEx := aUseRegEx;
end;

{ TLinqArray<T>.TLinqComparer }

constructor TLinqArray<T>.TLinqComparer.Create(aSortAscending : Boolean);
begin
  fSortAscending := aSortAscending;
end;

function TLinqArray<T>.TLinqComparer.Compare(const L, R: T): Integer;
var
  valueL : TValue;
  valueR : TValue;
  hr : Integer;
  lr : Integer;
begin
  Result := 0;
  if fSortAscending then
  begin
    hr := 1;
    lr := -1;
  end
  else
  begin
    hr := -1;
    lr := 1;
  end;
  valueL := TValue.From<T>(L);
  valueR := TValue.From<T>(R);
  if (valueL.IsEmpty) and (not valueR.IsEmpty) then Exit(hr)
    else if (not valueL.IsEmpty) and (valueR.IsEmpty) then Exit(lr);

  case valueL.Kind of
    {$IFNDEF FPC}
    tkString,
    {$ENDIF}
    tkChar, tkWString, tkUString : Result := CompareText(valueL.AsString, valueR.AsString);
    tkInteger, tkInt64 :
      begin
        if valueL.AsInteger > valueR.AsInteger then Result := hr
          else if valueL.AsInteger < valueR.AsInteger then Result := lr;
      end;
    tkFloat :
      begin
        if valueL.AsExtended > valueR.AsExtended then Result := hr
          else if valueL.AsExtended < valueR.AsExtended then Result := lr;
      end;
    end;
end;

{ TLinqArrayHelper }

function TLinqArrayHelper.Add(const aValue : string) : Integer;
begin
  SetLength(Self,Length(Self)+1);
  Self[High(Self)] := aValue;
  Result := High(Self);
end;

function TLinqArrayHelper.AddIfNotExists(const aValue : string) : Integer;
var
  i : Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i],aValue) = 0 then Exit(i);
  end;
  //if not exists add it
  Result := Self.Add(aValue);
end;

function TLinqArrayHelper.Remove(const aValue : string) : Boolean;
var
  i : Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i],aValue) = 0 then
    begin
      System.Delete(Self,i,1);
      Exit(True);
    end;
  end;
  Result := False;
end;

function TLinqArrayHelper.Any : Boolean;
begin
  Result := High(Self) >= 0;
end;

function TLinqArrayHelper.Any(const aValue : string) : Boolean;
var
  value : string;
begin
  Result := False;
  for value in Self do
  begin
    if CompareText(value,aValue) = 0 then Exit(True)
  end;
end;

function TLinqArrayHelper.Any(const aMatchString : string; aUseRegEx : Boolean) : Boolean;
begin
  Result := TLinqArray<string>.Create(Self).Any(aMatchString,aUseRegEx);
end;

function TLinqArrayHelper.Where(const aMatchString : string; aUseRegEx : Boolean) : ILinqArray<string>;
begin
  Result := TLinqArray<string>.Create(Self).Where(aMatchString,aUseRegEx);
end;
{$ENDIF}

end.
