{ ***************************************************************************

  Copyright (c) 2016-2021 Kike Pérez

  Unit        : Quick.Conditions
  Description : Conditions validator
  Author      : Kike Pérez
  Version     : 2.0
  Created     : 05/05/2021
  Modified    : 11/05/2021

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

unit Quick.Conditions;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  System.StrUtils,
  Quick.Commons;

type
  ICondition = interface
  ['{54F1E937-CE14-426A-9FC5-C6C7944915A2}']
  end;

  TCondition = class(TInterfacedObject,ICondition)
  protected
    fName : string;
    fExceptionClass : ExceptClass;
    fPostCondition : Boolean;
  public
    constructor Create;
    procedure ThrowException(const aMsg : string); overload;
    procedure ThrowException(const aMsg : string; aValues : array of const); overload;
    procedure ThrowException(aExceptionClass : ExceptClass; const aMsg : string); overload;
  end;

  IStringCondition = interface(ICondition)
  ['{B9591175-22E0-4624-94E2-B183DEE1F793}']
    function WithExceptionOnFailure(aExceptionClass : ExceptClass) : IStringCondition;
    function IsEmpty : IStringCondition; overload;
    function IsEmpty(const aCustomMessage : string) : IStringCondition; overload;
    function IsNotEmpty : IStringCondition; overload;
    function IsNotEmpty(const aCustomMessage : string) : IStringCondition; overload;
    function StartsWith(const aText : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function StartsWith(const aText, aCustomMessage : string; aIgnoreCase : Boolean = False): IStringCondition; overload;
    function DoesNotStartsWith(const aText : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function DoesNotStartsWith(const aText, aCustomMessage : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function EndsWith(const aText : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function EndsWith(const aText, aCustomMessage : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function DoesNotEndsWith(const aText : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function DoesNotEndsWith(const aText,aCustomMessage : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function Contains(const aText : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function Contains(const aText, aCustomMessage : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function DoesNotContains(const aText: string; aIgnoreCase: Boolean = False): IStringCondition; overload;
    function DoesNotContains(const aText, aCustomMessage: string; aIgnoreCase: Boolean = False): IStringCondition; overload;
    function HasLength(aLen : Integer) : IStringCondition; overload;
    function HasLength(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function DoesNotHasLength(aLen : Integer) : IStringCondition; overload;
    function DoesNotHasLength(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function IsLongerThan(aLen : Integer) : IStringCondition; overload;
    function IsLongerThan(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function IsLongerOrEqual(aLen : Integer) : IStringCondition; overload;
    function IsLongerOrEqual(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function IsShorterThan(aLen : Integer) : IStringCondition; overload;
    function IsShorterThan(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function IsShorterOrEqual(aLen : Integer) : IStringCondition; overload;
    function IsShorterOrEqual(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function HasLengthRange(aMin, aMax : Integer) : IStringCondition; overload;
    function HasLengthRange(aMin, aMax : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function IsUpperCase : IStringCondition; overload;
    function IsUpperCase(const aCustomMessage : string) : IStringCondition; overload;
    function IsLowerCase : IStringCondition; overload;
    function IsLowerCase(const aCustomMessage : string) : IStringCondition; overload;
    function IsNotUpperCase : IStringCondition; overload;
    function IsNotUpperCase(const aCustomMessage : string) : IStringCondition; overload;
    function IsNotLowerCase : IStringCondition; overload;
    function IsNotLowerCase(const aCustomMessage : string) : IStringCondition; overload;
    function Evaluate(aExpression : Boolean) : IStringCondition; overload;
    function Evaluate(aExpression : Boolean; const aCustomMessage : string) : IStringCondition; overload;
  end;

  IIntegerCondition = interface(ICondition)
  ['{A34856DD-175B-40BB-BC64-CF131CB448C7}']
    function WithExceptionOnFailure(aExceptionClass : ExceptClass) : IIntegerCondition;
    function IsInRange(aMin, aMax : Int64) : IIntegerCondition; overload;
    function IsInRange(aMin, aMax : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotInRange(aMin, aMax : Int64) : IIntegerCondition; overload;
    function IsNotInRange(aMin, aMax : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsEqualTo(aValue : Int64) : IIntegerCondition; overload;
    function IsEqualTo(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotEqualTo(aValue : Int64) : IIntegerCondition; overload;
    function IsNotEqualTo(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsGreaterThan(aValue : Int64) : IIntegerCondition; overload;
    function IsGreaterThan(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotGreaterThan(aValue : Int64) : IIntegerCondition; overload;
    function IsNotGreaterThan(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsGreaterOrEqual(aValue : Int64) : IIntegerCondition; overload;
    function IsGreaterOrEqual(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotGreaterOrEqual(aValue : Int64) : IIntegerCondition; overload;
    function IsNotGreaterOrEqual(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsLessThan(aValue : Int64) : IIntegerCondition; overload;
    function IsLessThan(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotLessThan(aValue : Int64) : IIntegerCondition; overload;
    function IsNotLessThan(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsLessOrEqual(aValue : Int64) : IIntegerCondition; overload;
    function IsLessOrEqual(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotLessOrEqual(aValue : Int64) : IIntegerCondition; overload;
    function IsNotLessOrEqual(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function Evaluate(aExpression : Boolean) : IIntegerCondition; overload;
    function Evaluate(aExpression : Boolean; const aCustomMessage : string) : IIntegerCondition; overload;
  end;

  IFloatCondition = interface(ICondition)
  ['{D0237A24-A00F-4B96-BA7B-0FF9BE7363E5}']
    function WithExceptionOnFailure(aExceptionClass : ExceptClass) : IFloatCondition;
    function IsInRange(aMin, aMax : Extended) : IFloatCondition; overload;
    function IsInRange(aMin, aMax : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotInRange(aMin, aMax : Extended) : IFloatCondition; overload;
    function IsNotInRange(aMin, aMax : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsEqualTo(aValue : Extended) : IFloatCondition; overload;
    function IsEqualTo(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotEqualTo(aValue : Extended) : IFloatCondition; overload;
    function IsNotEqualTo(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsGreaterThan(aValue : Extended) : IFloatCondition; overload;
    function IsGreaterThan(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotGreaterThan(aValue : Extended) : IFloatCondition; overload;
    function IsNotGreaterThan(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsGreaterOrEqual(aValue : Extended) : IFloatCondition; overload;
    function IsGreaterOrEqual(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotGreaterOrEqual(aValue : Extended) : IFloatCondition; overload;
    function IsNotGreaterOrEqual(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsLessThan(aValue : Extended) : IFloatCondition; overload;
    function IsLessThan(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotLessThan(aValue : Extended) : IFloatCondition; overload;
    function IsNotLessThan(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsLessOrEqual(aValue : Extended) : IFloatCondition; overload;
    function IsLessOrEqual(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotLessOrEqual(aValue : Extended) : IFloatCondition; overload;
    function IsNotLessOrEqual(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function Evaluate(aExpression : Boolean) : IFloatCondition; overload;
    function Evaluate(aExpression : Boolean; const aCustomMessage : string) : IFloatCondition; overload;
  end;

  IObjectCondition = interface(ICondition)
  ['{497E21D2-7780-4C3B-B51E-921847491FC1}']
    function WithExceptionOnFailure(aExceptionClass : ExceptClass) : IObjectCondition;
    function IsNull : IObjectCondition; overload;
    function IsNull(const aCustomMessage : string): IObjectCondition; overload;
    function IsNotNull : IObjectCondition; overload;
    function IsNotNull(const aCustomMessage : string) : IObjectCondition; overload;
    function IsOfType(aClass : TClass) : IObjectCondition; overload;
    function IsOfType(aClass : TClass; const aCustomMessage : string) : IObjectCondition; overload;
    function DoesNotOfType(aClass : TClass) : IObjectCondition; overload;
    function DoesNotOfType(aClass : TClass; const aCustomMessage : string) : IObjectCondition; overload;
    function Evaluate(aExpression : Boolean) : IObjectCondition; overload;
    function Evaluate(aExpression : Boolean; const aCustomMessage : string) : IObjectCondition; overload;
  end;

  TStringCondition = class(TCondition,IStringCondition)
  private
    fValue : string;
  public
    constructor Create(const aValue : string; const aName : string; aPostCondition : Boolean);
    function WithExceptionOnFailure(aExceptionClass : ExceptClass) : IStringCondition;
    function IsEmpty : IStringCondition; overload;
    function IsEmpty(const aCustomMessage : string) : IStringCondition; overload;
    function IsNotEmpty : IStringCondition; overload;
    function IsNotEmpty(const aCustomMessage : string) : IStringCondition; overload;
    function StartsWith(const aText : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function StartsWith(const aText, aCustomMessage : string; aIgnoreCase : Boolean = False): IStringCondition; overload;
    function DoesNotStartsWith(const aText : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function DoesNotStartsWith(const aText, aCustomMessage : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function EndsWith(const aText : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function EndsWith(const aText, aCustomMessage : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function DoesNotEndsWith(const aText : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function DoesNotEndsWith(const aText,aCustomMessage : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function Contains(const aText : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function Contains(const aText, aCustomMessage : string; aIgnoreCase : Boolean = False) : IStringCondition; overload;
    function DoesNotContains(const aText: string; aIgnoreCase: Boolean = False): IStringCondition; overload;
    function DoesNotContains(const aText, aCustomMessage: string; aIgnoreCase: Boolean = False): IStringCondition; overload;
    function HasLength(aLen : Integer) : IStringCondition; overload;
    function HasLength(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function DoesNotHasLength(aLen : Integer) : IStringCondition; overload;
    function DoesNotHasLength(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function IsLongerThan(aLen : Integer) : IStringCondition; overload;
    function IsLongerThan(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function IsLongerOrEqual(aLen : Integer) : IStringCondition; overload;
    function IsLongerOrEqual(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function IsShorterThan(aLen : Integer) : IStringCondition; overload;
    function IsShorterThan(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function IsShorterOrEqual(aLen : Integer) : IStringCondition; overload;
    function IsShorterOrEqual(aLen : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function HasLengthRange(aMin, aMax : Integer) : IStringCondition; overload;
    function HasLengthRange(aMin, aMax : Integer; const aCustomMessage : string) : IStringCondition; overload;
    function IsUpperCase : IStringCondition; overload;
    function IsUpperCase(const aCustomMessage : string) : IStringCondition; overload;
    function IsLowerCase : IStringCondition; overload;
    function IsLowerCase(const aCustomMessage : string) : IStringCondition; overload;
    function IsNotUpperCase : IStringCondition; overload;
    function IsNotUpperCase(const aCustomMessage : string) : IStringCondition; overload;
    function IsNotLowerCase : IStringCondition; overload;
    function IsNotLowerCase(const aCustomMessage : string) : IStringCondition; overload;
    function Evaluate(aExpression : Boolean) : IStringCondition; overload;
    function Evaluate(aExpression : Boolean; const aCustomMessage : string) : IStringCondition; overload;
  end;

  TIntegerCondition = class(TCondition,IIntegerCondition)
  private
    fValue : Int64;
  public
    constructor Create(const aValue : Int64; const aName : string; aPostCondition : Boolean);
    function WithExceptionOnFailure(aExceptionClass : ExceptClass) : IIntegerCondition;
    function IsInRange(aMin, aMax : Int64) : IIntegerCondition; overload;
    function IsInRange(aMin, aMax : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotInRange(aMin, aMax : Int64) : IIntegerCondition; overload;
    function IsNotInRange(aMin, aMax : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsEqualTo(aValue : Int64) : IIntegerCondition; overload;
    function IsEqualTo(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotEqualTo(aValue : Int64) : IIntegerCondition; overload;
    function IsNotEqualTo(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsGreaterThan(aValue : Int64) : IIntegerCondition; overload;
    function IsGreaterThan(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotGreaterThan(aValue : Int64) : IIntegerCondition; overload;
    function IsNotGreaterThan(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsGreaterOrEqual(aValue : Int64) : IIntegerCondition; overload;
    function IsGreaterOrEqual(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotGreaterOrEqual(aValue : Int64) : IIntegerCondition; overload;
    function IsNotGreaterOrEqual(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsLessThan(aValue : Int64) : IIntegerCondition; overload;
    function IsLessThan(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotLessThan(aValue : Int64) : IIntegerCondition; overload;
    function IsNotLessThan(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsLessOrEqual(aValue : Int64) : IIntegerCondition; overload;
    function IsLessOrEqual(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function IsNotLessOrEqual(aValue : Int64) : IIntegerCondition; overload;
    function IsNotLessOrEqual(aValue : Int64; const aCustomMessage : string) : IIntegerCondition; overload;
    function Evaluate(aExpression : Boolean) : IIntegerCondition; overload;
    function Evaluate(aExpression : Boolean; const aCustomMessage : string) : IIntegerCondition; overload;
  end;

  TFloatCondition = class(TCondition,IFloatCondition)
  private
    fValue : Extended;
  public
    constructor Create(const aValue : Extended; const aName : string; aPostCondition : Boolean);
    function WithExceptionOnFailure(aExceptionClass : ExceptClass) : IFloatCondition;
    function IsInRange(aMin, aMax : Extended) : IFloatCondition; overload;
    function IsInRange(aMin, aMax : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotInRange(aMin, aMax : Extended) : IFloatCondition; overload;
    function IsNotInRange(aMin, aMax : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsEqualTo(aValue : Extended) : IFloatCondition; overload;
    function IsEqualTo(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotEqualTo(aValue : Extended) : IFloatCondition; overload;
    function IsNotEqualTo(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsGreaterThan(aValue : Extended) : IFloatCondition; overload;
    function IsGreaterThan(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotGreaterThan(aValue : Extended) : IFloatCondition; overload;
    function IsNotGreaterThan(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsGreaterOrEqual(aValue : Extended) : IFloatCondition; overload;
    function IsGreaterOrEqual(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotGreaterOrEqual(aValue : Extended) : IFloatCondition; overload;
    function IsNotGreaterOrEqual(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsLessThan(aValue : Extended) : IFloatCondition; overload;
    function IsLessThan(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotLessThan(aValue : Extended) : IFloatCondition; overload;
    function IsNotLessThan(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsLessOrEqual(aValue : Extended) : IFloatCondition; overload;
    function IsLessOrEqual(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function IsNotLessOrEqual(aValue : Extended) : IFloatCondition; overload;
    function IsNotLessOrEqual(aValue : Extended; const aCustomMessage : string) : IFloatCondition; overload;
    function Evaluate(aExpression : Boolean) : IFloatCondition; overload;
    function Evaluate(aExpression : Boolean; const aCustomMessage : string) : IFloatCondition; overload;
  end;

  TObjectCondition = class(TCondition,IObjectCondition)
  private
    fValue : TObject;
  public
    constructor Create(const aValue : TObject; const aName : string; aPostCondition : Boolean);
    function WithExceptionOnFailure(aExceptionClass : ExceptClass) : IObjectCondition;
    function IsNull : IObjectCondition; overload;
    function IsNull(const aCustomMessage : string): IObjectCondition; overload;
    function IsNotNull : IObjectCondition; overload;
    function IsNotNull(const aCustomMessage : string) : IObjectCondition; overload;
    function IsOfType(aClass : TClass) : IObjectCondition; overload;
    function IsOfType(aClass : TClass; const aCustomMessage : string) : IObjectCondition; overload;
    function DoesNotOfType(aClass : TClass) : IObjectCondition; overload;
    function DoesNotOfType(aClass : TClass; const aCustomMessage : string) : IObjectCondition; overload;
    function Evaluate(aExpression : Boolean) : IObjectCondition; overload;
    function Evaluate(aExpression : Boolean; const aCustomMessage : string) : IObjectCondition; overload;
  end;

  IConditionValidator = interface
  ['{F707606E-7603-4690-BE76-2443B0A36D5F}']
    function Requires(const aValue : string; const aName : string = '') : IStringCondition; overload;
    function Requires(const aValue : Int64; const aName : string = '') : IIntegerCondition; overload;
    function Requires(const aValue : Extended; const aName : string = '') : IFloatCondition; overload;
    function Requires(const aValue : TObject; const aName : string = '') : IObjectCondition; overload;
    function Ensures(const aValue : string; const aName : string = '') : IStringCondition; overload;
    function Ensures(const aValue : Int64; const aName : string = '') : IIntegerCondition; overload;
    function Ensures(const aValue : Extended; const aName : string = '') : IFloatCondition; overload;
    function Ensures(const aValue : TObject; const aName : string = '') : IObjectCondition; overload;
  end;

  TConditionValidator = class(TInterfacedObject,IConditionValidator)
  public
    function Requires(const aValue : string; const aName : string = '') : IStringCondition; overload;
    function Requires(const aValue : Int64; const aName : string = '') : IIntegerCondition; overload;
    function Requires(const aValue : Extended; const aName : string = '') : IFloatCondition; overload;
    function Requires(const aValue : TObject; const aName : string = '') : IObjectCondition; overload;
    function Ensures(const aValue : string; const aName : string = '') : IStringCondition; overload;
    function Ensures(const aValue : Int64; const aName : string = '') : IIntegerCondition; overload;
    function Ensures(const aValue : Extended; const aName : string = '') : IFloatCondition; overload;
    function Ensures(const aValue : TObject; const aName : string = '') : IObjectCondition; overload;
  end;

  EPreConditionError = class(Exception);
  EPostConditionError = class(Exception);

  function Condition : IConditionValidator;

implementation

function Condition : IConditionValidator;
begin
  Result := TConditionValidator.Create;
end;

{ TEvaluator }

function TConditionValidator.Requires(const aValue: string; const aName : string = ''): IStringCondition;
begin
  Result := TStringCondition.Create(aValue,aName,False);
end;

function TConditionValidator.Requires(const aValue: Int64; const aName : string = ''): IIntegerCondition;
begin
  Result := TIntegerCondition.Create(aValue,aName,False);
end;

function TConditionValidator.Requires(const aValue: Extended; const aName : string = ''): IFloatCondition;
begin
  Result := TFloatCondition.Create(aValue,aName,False);
end;

function TConditionValidator.Requires(const aValue: TObject; const aName : string = ''): IObjectCondition;
begin
  Result := TObjectCondition.Create(aValue,aName,False);
end;

function TConditionValidator.Ensures(const aValue, aName: string): IStringCondition;
begin
  Result := TStringCondition.Create(aValue,aName,True);
end;

function TConditionValidator.Ensures(const aValue: Int64; const aName: string): IIntegerCondition;
begin
  Result := TIntegerCondition.Create(aValue,aName,True);
end;

function TConditionValidator.Ensures(const aValue: Extended; const aName: string): IFloatCondition;
begin
  Result := TFloatCondition.Create(aValue,aName,True);
end;

function TConditionValidator.Ensures(const aValue: TObject; const aName: string): IObjectCondition;
begin
  Result := TObjectCondition.Create(aValue,aName,True);
end;

{ TStringCondition }

constructor TStringCondition.Create(const aValue: string; const aName : string; aPostCondition : Boolean);
begin
  fName := aName;
  fValue := aValue;
  fPostCondition := aPostCondition;
end;

function TStringCondition.WithExceptionOnFailure(aExceptionClass: ExceptClass): IStringCondition;
begin
  fExceptionClass := aExceptionClass;
  Result := Self;
end;

function TStringCondition.IsEmpty: IStringCondition;
begin
  Result := Self.IsEmpty('');
end;

function TStringCondition.IsEmpty(const aCustomMessage : string): IStringCondition;
begin
  if not fValue.IsEmpty then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentNilException,aCustomMessage)
      else ThrowException('must be empty');
  end;
  Result := Self;
end;

function TStringCondition.IsNotEmpty: IStringCondition;
begin
  Result := Self.IsNotEmpty('');
end;

function TStringCondition.IsNotEmpty(const aCustomMessage : string): IStringCondition;
begin
  if fValue.IsEmpty then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentNilException,aCustomMessage)
      else ThrowException('should not be empty');
  end;
  Result := Self;
end;

function TStringCondition.StartsWith(const aText: string; aIgnoreCase : Boolean = False): IStringCondition;
begin
  Result := Self.StartsWith(aText,'',aIgnoreCase);
end;

function TStringCondition.StartsWith(const aText, aCustomMessage : string; aIgnoreCase : Boolean = False): IStringCondition;
begin
  if not fValue.StartsWith(aText,aIgnoreCase) then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must start with "%s"',[aText])
  end;
  Result := Self;
end;

function TStringCondition.DoesNotStartsWith(const aText: string; aIgnoreCase: Boolean): IStringCondition;
begin
  Result := Self.DoesNotStartsWith(aText,'',aIgnoreCase);
end;

function TStringCondition.DoesNotStartsWith(const aText, aCustomMessage: string; aIgnoreCase: Boolean): IStringCondition;
begin
  if fValue.StartsWith(aText,aIgnoreCase) then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not start with "%s"',[aText]);
  end;
  Result := Self;
end;

function TStringCondition.EndsWith(const aText: string; aIgnoreCase : Boolean = False): IStringCondition;
begin
  Result := Self.EndsWith(aText,'',aIgnoreCase);
end;

function TStringCondition.EndsWith(const aText, aCustomMessage: string; aIgnoreCase : Boolean = False): IStringCondition;
begin
  if not fValue.EndsWith(aText,aIgnoreCase) then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must end with "%s"',[aText]);
  end;
  Result := Self;
end;

function TStringCondition.DoesNotEndsWith(const aText: string; aIgnoreCase: Boolean): IStringCondition;
begin
  Result := Self.DoesNotEndsWith(aText,'',aIgnoreCase);
end;

function TStringCondition.DoesNotEndsWith(const aText, aCustomMessage: string; aIgnoreCase: Boolean): IStringCondition;
begin
  if fValue.EndsWith(aText,aIgnoreCase) then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be end with "%s"',[aText]);
  end;
  Result := Self;
end;

function TStringCondition.Contains(const aText: string; aIgnoreCase: Boolean): IStringCondition;
begin
  Result := Self.Contains(aText,'',aIgnoreCase);
end;

function TStringCondition.Contains(const aText, aCustomMessage: string; aIgnoreCase: Boolean): IStringCondition;
begin
  if aIgnoreCase then
  begin
    if not ContainsText(fValue,aText) then
    begin
      if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
        else ThrowException('must contain "%s"',[aText]);
    end;
  end
  else
  begin
    if not fValue.Contains(aText) then
    begin
      if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
        else ThrowException('must contain "%s"',[aText]);
    end;
  end;
  Result := Self;
end;

function TStringCondition.DoesNotContains(const aText: string; aIgnoreCase: Boolean = False): IStringCondition;
begin
  Result := Self.DoesNotContains(aText,'',aIgnoreCase);
end;

function TStringCondition.DoesNotContains(const aText, aCustomMessage: string; aIgnoreCase: Boolean = False): IStringCondition;
begin
  if aIgnoreCase then
  begin
    if ContainsText(fValue,aText) then
    begin
      if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
        else ThrowException('should not contain "%s"',[aText]);
    end;
  end
  else
  begin
    if fValue.Contains(aText) then
    begin
      if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
        else ThrowException('should not contain "%s"',[aText]);
    end;
  end;
  Result := Self;
end;

function TStringCondition.HasLength(aLen: Integer): IStringCondition;
begin
  Result := Self.HasLength(aLen,'');
end;

function TStringCondition.HasLength(aLen: Integer; const aCustomMessage : string): IStringCondition;
begin
  if fValue.Length <> aLen then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be %d length',[aLen]);
  end;
  Result := Self;
end;

function TStringCondition.DoesNotHasLength(aLen: Integer): IStringCondition;
begin
  Result := Self.DoesNotHasLength(aLen,'');
end;

function TStringCondition.DoesNotHasLength(aLen: Integer; const aCustomMessage : string): IStringCondition;
begin
  if fValue.Length = aLen then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be %d length',[aLen]);
  end;
  Result := Self;
end;

function TStringCondition.IsShorterThan(aLen: Integer): IStringCondition;
begin
  Result := Self.IsShorterThan(aLen,'');
end;

function TStringCondition.IsShorterThan(aLen: Integer; const aCustomMessage : string): IStringCondition;
begin
  if fValue.Length >= aLen then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be shorten than %d',[aLen]);
  end;
  Result := Self;
end;

function TStringCondition.IsShorterOrEqual(aLen: Integer): IStringCondition;
begin
  Result := Self.IsShorterOrEqual(aLen,'');
end;

function TStringCondition.IsShorterOrEqual(aLen: Integer; const aCustomMessage : string): IStringCondition;
begin
  if fValue.Length > aLen then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be shorter or equal to %d',[aLen]);
  end;
  Result := Self;
end;

function TStringCondition.IsLongerOrEqual(aLen: Integer): IStringCondition;
begin
  Result := Self.IsLongerOrEqual(aLen,'');
end;

function TStringCondition.IsLongerOrEqual(aLen: Integer; const aCustomMessage : string): IStringCondition;
begin
  if fValue.Length < aLen then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be longer or equal to %d',[aLen]);
  end;
  Result := Self;
end;

function TStringCondition.IsLongerThan(aLen: Integer): IStringCondition;
begin
  Result := Self.IsLongerThan(aLen,'');
end;

function TStringCondition.IsLongerThan(aLen: Integer; const aCustomMessage : string): IStringCondition;
begin
  if fValue.Length <= aLen then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be longer than %d',[aLen]);
  end;
  Result := Self;
end;

function TStringCondition.HasLengthRange(aMin, aMax: Integer): IStringCondition;
begin
  Result := Self.HasLengthRange(aMin,aMax,'');
end;

function TStringCondition.HasLengthRange(aMin, aMax: Integer; const aCustomMessage : string): IStringCondition;
begin
  if (fValue.Length < aMin) or (fValue.Length > aMax) then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentOutOfRangeException,aCustomMessage)
      else ThrowException('must be in %d-%d length range',[aMin,aMax]);
  end;
  Result := Self;
end;

function TStringCondition.IsUpperCase: IStringCondition;
begin
  Result := Self.IsUpperCase('');
end;

function TStringCondition.IsUpperCase(const aCustomMessage : string): IStringCondition;
begin
  if fValue.ToUpper <> fValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be upper case');
  end;
  Result := Self;
end;

function TStringCondition.IsLowerCase: IStringCondition;
begin
  Result := Self.IsLowerCase('');
end;

function TStringCondition.IsLowerCase(const aCustomMessage : string): IStringCondition;
begin
  if fValue.ToLower <> fValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be lower case');
  end;
  Result := Self;
end;

function TStringCondition.IsNotUpperCase: IStringCondition;
begin
  Result := Self.IsNotUpperCase('');
end;

function TStringCondition.IsNotUpperCase(const aCustomMessage : string): IStringCondition;
begin
  if fValue.ToUpper = fValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be upper case');
  end;
  Result := Self;
end;

function TStringCondition.IsNotLowerCase: IStringCondition;
begin
  Result := Self.IsNotLowerCase('');
end;

function TStringCondition.IsNotLowerCase(const aCustomMessage : string): IStringCondition;
begin
  if fValue.ToLower = fValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be lower case');
  end;
  Result := Self;
end;

function TStringCondition.Evaluate(aExpression: Boolean): IStringCondition;
begin
  Result := Self.Evaluate(aExpression,'');
end;

function TStringCondition.Evaluate(aExpression: Boolean; const aCustomMessage : string): IStringCondition;
begin
  if not aExpression then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else  ThrowException('must meet condition');
  end;
end;

{ TIntegerCondition }

constructor TIntegerCondition.Create(const aValue: Int64; const aName : string; aPostCondition : Boolean);
begin
  fName := aName;
  fValue := aValue;
  fPostCondition := aPostCondition;
end;

function TIntegerCondition.WithExceptionOnFailure(aExceptionClass: ExceptClass): IIntegerCondition;
begin
  fExceptionClass := aExceptionClass;
  Result := Self;
end;

function TIntegerCondition.IsEqualTo(aValue: Int64): IIntegerCondition;
begin
  Result := Self.IsEqualTo(aValue,'');
end;

function TIntegerCondition.IsEqualTo(aValue: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if fValue <> aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TIntegerCondition.IsGreaterOrEqual(aValue: Int64): IIntegerCondition;
begin
  Result := Self.IsGreaterOrEqual(aValue,'');
end;

function TIntegerCondition.IsGreaterOrEqual(aValue: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if fValue < aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be greather or equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TIntegerCondition.IsGreaterThan(aValue: Int64): IIntegerCondition;
begin
  Result := Self.IsGreaterThan(aValue,'');
end;

function TIntegerCondition.IsGreaterThan(aValue: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if fValue <= aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be greather than %d',[aValue]);
  end;
  Result := Self;
end;

function TIntegerCondition.IsInRange(aMin, aMax: Int64): IIntegerCondition;
begin
  Result := Self.IsInRange(aMin,aMax,'');
end;

function TIntegerCondition.IsInRange(aMin, aMax: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if (fValue < aMin) or (fValue > aMax) then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentOutOfRangeException,aCustomMessage)
      else ThrowException('must be in %d-%d range',[aMin,aMax]);
  end;
  Result := Self;
end;

function TIntegerCondition.IsLessOrEqual(aValue: Int64): IIntegerCondition;
begin
  Result := Self.IsLessOrEqual(aValue,'');
end;

function TIntegerCondition.IsLessOrEqual(aValue: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if fValue > aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be less or equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TIntegerCondition.IsLessThan(aValue: Int64): IIntegerCondition;
begin
  Result := Self.IsLessThan(aValue,'');
end;

function TIntegerCondition.IsLessThan(aValue: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if fValue >= aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be less than %d',[aValue]);
  end;
  Result := Self;
end;

function TIntegerCondition.IsNotEqualTo(aValue: Int64): IIntegerCondition;
begin
  Result := Self.IsNotEqualTo(aValue,'');
end;

function TIntegerCondition.IsNotEqualTo(aValue: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if fValue = aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TIntegerCondition.IsNotGreaterOrEqual(aValue: Int64): IIntegerCondition;
begin
  Result := Self.IsNotGreaterOrEqual(aValue,'');
end;

function TIntegerCondition.IsNotGreaterOrEqual(aValue: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if fValue >= aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be greater or equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TIntegerCondition.IsNotGreaterThan(aValue: Int64): IIntegerCondition;
begin
  Result := Self.IsNotGreaterThan(aValue,'');
end;

function TIntegerCondition.IsNotGreaterThan(aValue: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if fValue > aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be greater than %d',[aValue]);
  end;
  Result := Self;
end;

function TIntegerCondition.IsNotInRange(aMin, aMax: Int64): IIntegerCondition;
begin
  Result := Self.IsNotInRange(aMin,aMax,'');
end;

function TIntegerCondition.IsNotInRange(aMin, aMax: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if (fValue >= aMin) and (fValue <= aMax) then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentOutOfRangeException,aCustomMessage)
      else ThrowException('should not be in range %d-%d',[aMin,aMax]);
  end;
  Result := Self;
end;

function TIntegerCondition.IsNotLessOrEqual(aValue: Int64): IIntegerCondition;
begin
  Result := Self.IsNotLessOrEqual(aValue,'');
end;

function TIntegerCondition.IsNotLessOrEqual(aValue: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if fValue <= aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be less or equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TIntegerCondition.IsNotLessThan(aValue: Int64): IIntegerCondition;
begin
  Result := Self.IsNotLessThan(aValue,'');
end;

function TIntegerCondition.IsNotLessThan(aValue: Int64; const aCustomMessage : string): IIntegerCondition;
begin
  if fValue < aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be less than %d',[aValue]);
  end;
  Result := Self;
end;

function TIntegerCondition.Evaluate(aExpression: Boolean): IIntegerCondition;
begin
  Result := Self.Evaluate(aExpression,'');
end;

function TIntegerCondition.Evaluate(aExpression: Boolean; const aCustomMessage : string): IIntegerCondition;
begin
  if not aExpression then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must meet condition');
  end;
end;

{ TFloatCondition }

constructor TFloatCondition.Create(const aValue: Extended; const aName : string; aPostCondition : Boolean);
begin
  fName := aName;
  fValue := aValue;
  fPostCondition := aPostCondition;
end;

function TFloatCondition.WithExceptionOnFailure(aExceptionClass: ExceptClass): IFloatCondition;
begin
  fExceptionClass := aExceptionClass;
  Result := Self;
end;

function TFloatCondition.IsEqualTo(aValue: Extended): IFloatCondition;
begin
  Result := Self.IsEqualTo(aValue,'');
end;

function TFloatCondition.IsEqualTo(aValue: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if fValue <> aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TFloatCondition.IsGreaterOrEqual(aValue: Extended): IFloatCondition;
begin
  Result := Self.IsGreaterOrEqual(aValue,'');
end;

function TFloatCondition.IsGreaterOrEqual(aValue: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if fValue < aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be greather or equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TFloatCondition.IsGreaterThan(aValue: Extended): IFloatCondition;
begin
  Result := Self.IsGreaterThan(aValue,'');
end;

function TFloatCondition.IsGreaterThan(aValue: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if fValue <= aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be greather than %d',[aValue]);
  end;
  Result := Self;
end;

function TFloatCondition.IsInRange(aMin, aMax: Extended): IFloatCondition;
begin
  Result := Self.IsInRange(aMin,aMax,'');
end;

function TFloatCondition.IsInRange(aMin, aMax: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if (fValue < aMin) or (fValue > aMax) then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentOutOfRangeException,aCustomMessage)
      else ThrowException('must be in %d-%d range',[aMin,aMax]);
  end;
  Result := Self;
end;

function TFloatCondition.IsLessOrEqual(aValue: Extended): IFloatCondition;
begin
  Result := Self.IsLessOrEqual(aValue,'');
end;

function TFloatCondition.IsLessOrEqual(aValue: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if fValue > aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be less or equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TFloatCondition.IsLessThan(aValue: Extended): IFloatCondition;
begin
  Result := Self.IsLessThan(aValue,'');
end;

function TFloatCondition.IsLessThan(aValue: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if fValue >= aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be less than %d',[aValue]);
  end;
  Result := Self;
end;

function TFloatCondition.IsNotEqualTo(aValue: Extended): IFloatCondition;
begin
  Result := Self.IsNotEqualTo(aValue,'');
end;

function TFloatCondition.IsNotEqualTo(aValue: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if fValue = aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TFloatCondition.IsNotGreaterOrEqual(aValue: Extended): IFloatCondition;
begin
  Result := Self.IsNotGreaterOrEqual(aValue,'');
end;

function TFloatCondition.IsNotGreaterOrEqual(aValue: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if fValue >= aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be greater or equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TFloatCondition.IsNotGreaterThan(aValue: Extended): IFloatCondition;
begin
  Result := Self.IsNotGreaterThan(aValue,'');
end;

function TFloatCondition.IsNotGreaterThan(aValue: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if fValue > aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be greater than %d',[aValue]);
  end;
  Result := Self;
end;

function TFloatCondition.IsNotInRange(aMin, aMax: Extended): IFloatCondition;
begin
  Result := Self.IsNotInRange(aMin,aMax,'');
end;

function TFloatCondition.IsNotInRange(aMin, aMax: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if (fValue >= aMin) and (fValue <= aMax) then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentOutOfRangeException,aCustomMessage)
      else ThrowException('should not be in range %d-%d',[aMin,aMax]);
  end;
  Result := Self;
end;

function TFloatCondition.IsNotLessOrEqual(aValue: Extended): IFloatCondition;
begin
  Result := Self.IsNotLessOrEqual(aValue,'');
end;

function TFloatCondition.IsNotLessOrEqual(aValue: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if fValue <= aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be less or equal to %d',[aValue]);
  end;
  Result := Self;
end;

function TFloatCondition.IsNotLessThan(aValue: Extended): IFloatCondition;
begin
  Result := Self.IsNotLessThan(aValue,'');
end;

function TFloatCondition.IsNotLessThan(aValue: Extended; const aCustomMessage : string): IFloatCondition;
begin
  if fValue < aValue then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be less than %d',[aValue]);
  end;
  Result := Self;
end;

function TFloatCondition.Evaluate(aExpression: Boolean): IFloatCondition;
begin
  Result := Self.Evaluate(aExpression,'');
end;

function TFloatCondition.Evaluate(aExpression: Boolean; const aCustomMessage : string): IFloatCondition;
begin
  if not aExpression then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must meet condition');
  end;
end;

{ TObjectCondition }

constructor TObjectCondition.Create(const aValue: TObject; const aName: string; aPostCondition : Boolean);
begin
  fName := aName;
  fValue := aValue;
  fPostCondition := aPostCondition;
end;

function TObjectCondition.WithExceptionOnFailure(aExceptionClass: ExceptClass): IObjectCondition;
begin
  fExceptionClass := aExceptionClass;
  Result := Self;
end;

function TObjectCondition.IsNull: IObjectCondition;
begin
  Result := Self.IsNull('');
end;

function TObjectCondition.IsNull(const aCustomMessage: string): IObjectCondition;
begin
  if fValue <> nil then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentNilException,aCustomMessage)
      else ThrowException('must be null');
  end;
  Result := Self;
end;

function TObjectCondition.IsNotNull: IObjectCondition;
begin
  Result := Self.IsNotNull('');
end;

function TObjectCondition.IsNotNull(const aCustomMessage: string): IObjectCondition;
begin
  if fValue = nil then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentNilException,aCustomMessage)
      else ThrowException('should not be null');
  end;
  Result := Self;
end;

function TObjectCondition.IsOfType(aClass: TClass): IObjectCondition;
begin
  Result := Self.IsOfType(aClass,'');
end;

function TObjectCondition.IsOfType(aClass: TClass; const aCustomMessage: string): IObjectCondition;
begin
  if not(fValue is aClass) then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must be of type "%s"',[aClass.ClassName]);
  end;
  Result := Self;
end;

function TObjectCondition.DoesNotOfType(aClass: TClass): IObjectCondition;
begin
  Result := Self.DoesNotOfType(aClass,'');
end;

function TObjectCondition.DoesNotOfType(aClass: TClass; const aCustomMessage: string): IObjectCondition;
begin
  if fValue is aClass then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('should not be of type "%s"',[aClass.ClassName]);
  end;
  Result := Self;
end;

function TObjectCondition.Evaluate(aExpression: Boolean): IObjectCondition;
begin
  Result := Self.Evaluate(aExpression,'');
end;

function TObjectCondition.Evaluate(aExpression: Boolean; const aCustomMessage: string): IObjectCondition;
begin
  if not aExpression then
  begin
    if not aCustomMessage.IsEmpty then ThrowException(EArgumentException,aCustomMessage)
      else ThrowException('must meet condition');
  end;
  Result := Self;
end;

{ TCondition }

constructor TCondition.Create;
begin
  fName := '';
  fExceptionClass := nil;
  fPostCondition := False;
end;

procedure TCondition.ThrowException(const aMsg: string);
var
  rexception : ExceptClass;
begin
  if fExceptionClass <> nil then raise fExceptionClass.Create(aMsg)
  else
  begin
    if fPostCondition then rexception := EPostConditionError
      else rexception := EPreConditionError;
    if fName.IsEmpty then raise rexception.Create(aMsg)
      else raise rexception.CreateFmt('[%s] %s',[fName,aMsg]);
  end;
end;

procedure TCondition.ThrowException(const aMsg: string; aValues: array of const);
begin
  if fExceptionClass <> nil then raise fExceptionClass.Create(aMsg)
    else ThrowException(Format(aMsg,aValues));
end;

procedure TCondition.ThrowException(aExceptionClass : ExceptClass; const aMsg : string);
begin
  if fExceptionClass <> nil then raise fExceptionClass.Create(aMsg)
    else raise aExceptionClass.Create(aMsg);
end;

end.
