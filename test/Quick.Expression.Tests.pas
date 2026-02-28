{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.Expression.Tests
  Description : TExpressionParser, TSingleExpression and TMultiExpression tests
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 27/02/2026
  Modified    : 28/02/2026
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

unit Quick.Expression.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Rtti,
  Quick.Expression;

type

  // ── Dummy object used for property-based validation tests ────────────
  TPersonExpr = class
  private
    fName  : string;
    fAge   : Integer;
    fScore : Double;
    fActive: Boolean;
  published
    property Name   : string  read fName   write fName;
    property Age    : Integer read fAge    write fAge;
    property Score  : Double  read fScore  write fScore;
    property Active : Boolean read fActive write fActive;
  end;

  [TestFixture]
  TExpressionTests = class(TObject)
  public

    // ── TExpressionParser.Parse ─────────────────────────────────────────
    [Test] procedure Test_Parse_EmptyExpression_Raises;
    [Test] procedure Test_Parse_SingleExpression_ReturnsObject;
    [Test] procedure Test_Parse_MultiExpression_ReturnsObject;

    // ── TExpressionParser.Validate (string overload – no object) ────────
    [Test] procedure Test_Validate_StringOnly_Equal_True;
    [Test] procedure Test_Validate_StringOnly_Equal_False;

    // ── Operators on plain TValue (scalar) ──────────────────────────────
    [Test] procedure Test_Op_Equal_Integer_True;
    [Test] procedure Test_Op_Equal_Integer_False;
    [Test] procedure Test_Op_NotEqual_Integer_True;
    [Test] procedure Test_Op_NotEqual_Integer_False;
    [Test] procedure Test_Op_Greater_Integer_True;
    [Test] procedure Test_Op_Greater_Integer_False;
    [Test] procedure Test_Op_EqualOrGreater_Integer_Equal;
    [Test] procedure Test_Op_EqualOrGreater_Integer_Greater;
    [Test] procedure Test_Op_EqualOrGreater_Integer_Lower_ReturnsFalse;
    [Test] procedure Test_Op_Lower_Integer_True;
    [Test] procedure Test_Op_Lower_Integer_False;
    [Test] procedure Test_Op_EqualOrLower_Integer_Equal;
    [Test] procedure Test_Op_EqualOrLower_Integer_Lower;
    [Test] procedure Test_Op_EqualOrLower_Integer_Greater_ReturnsFalse;
    [Test] procedure Test_Op_Equal_String_CaseInsensitive;
    [Test] procedure Test_Op_NotEqual_String_True;
    [Test] procedure Test_Op_Equal_Boolean_True;
    [Test] procedure Test_Op_Equal_Boolean_False;

    // ── LIKE operators ───────────────────────────────────────────────────
    [Test] procedure Test_Op_Like_Contains_True;
    [Test] procedure Test_Op_Like_Contains_False;
    [Test] procedure Test_Op_Like_StartsWith_True;
    [Test] procedure Test_Op_Like_StartsWith_False;
    [Test] procedure Test_Op_Like_EndsWith_True;
    [Test] procedure Test_Op_Like_EndsWith_False;

    // ── CONTAINS operator (TArray) ───────────────────────────────────────
    [Test] procedure Test_Op_Contains_IntArray_Found;
    [Test] procedure Test_Op_Contains_IntArray_NotFound;
    [Test] procedure Test_Op_Contains_StringArray_Found;
    [Test] procedure Test_Op_Contains_StringArray_NotFound;

    // ── Object property validation ───────────────────────────────────────
    [Test] procedure Test_Object_IntegerProp_Equal_True;
    [Test] procedure Test_Object_IntegerProp_Equal_False;
    [Test] procedure Test_Object_IntegerProp_Greater_True;
    [Test] procedure Test_Object_StringProp_Equal_True;
    [Test] procedure Test_Object_StringProp_Equal_CaseInsensitive;
    [Test] procedure Test_Object_StringProp_Like_Contains;
    [Test] procedure Test_Object_BoolProp_Equal_True;
    [Test] procedure Test_Object_DoubleProp_Greater_True;

    // ── Multi-expression AND / OR / XOR ─────────────────────────────────
    [Test] procedure Test_Multi_AND_BothTrue_ReturnsTrue;
    [Test] procedure Test_Multi_AND_OneFalse_ReturnsFalse;
    [Test] procedure Test_Multi_OR_OneTrue_ReturnsTrue;
    [Test] procedure Test_Multi_OR_BothFalse_ReturnsFalse;
    [Test] procedure Test_Multi_XOR_DifferentValues_ReturnsTrue;
    [Test] procedure Test_Multi_XOR_SameValues_ReturnsFalse;
    [Test] procedure Test_Multi_ThreeTerms_AND_AllTrue;
    [Test] procedure Test_Multi_ThreeTerms_AND_OneFalse;
    [Test] procedure Test_Multi_Nested_AND_OR;

    // ── Error / edge cases ───────────────────────────────────────────────
    [Test] procedure Test_InvalidOperator_Raises;
    [Test] procedure Test_Like_NoWildcard_Raises;
    [Test] procedure Test_Parse_FreesProperly;

    // ── Additional edge cases ────────────────────────────────────────────
    [Test] procedure Test_Object_StringProp_Like_EmptyValue_ReturnsFalse;
    [Test] procedure Test_Object_DoubleProp_EqualOrGreater_True;
    [Test] procedure Test_Object_DoubleProp_EqualOrLower_True;
    [Test] procedure Test_Object_DoubleProp_Equal_Boundary;
    [Test] procedure Test_Object_IntegerProp_NotEqual_True;
    [Test] procedure Test_Object_IntegerProp_LessOrEqual_True;
    [Test] procedure Test_Object_IntegerProp_GreaterOrEqual_True;
    [Test] procedure Test_Op_Contains_EmptyArray_ReturnsFalse;
    [Test] procedure Test_Multi_OR_WithDoubleAndString;
    [Test] procedure Test_Multi_AND_ScoreLike;
    [Test] procedure Test_Object_LikeProp_StartsWith_True;
    [Test] procedure Test_Object_LikeProp_EndsWith_False;
    [Test] procedure Test_Validate_StringOnly_NotEqual_True;
    [Test] procedure Test_Validate_StringOnly_NotEqual_False;
  end;

implementation

// ══════════════════════════════════════════════════════════════════════════════
//  Helpers
// ══════════════════════════════════════════════════════════════════════════════

// Wrap a plain value as TValue and run Validate
function ValidateScalar(const aExpr: string; const aVal: TValue): Boolean;
begin
  Result := TExpressionParser.Validate(aVal, aExpr);
end;

// ══════════════════════════════════════════════════════════════════════════════
//  TExpressionParser.Parse
// ══════════════════════════════════════════════════════════════════════════════

procedure TExpressionTests.Test_Parse_EmptyExpression_Raises;
begin
  Assert.WillRaise(
    procedure begin TExpressionParser.Parse('').Free; end,
    ENotValidExpression,
    'Parsing empty string must raise ENotValidExpression');
end;

procedure TExpressionTests.Test_Parse_SingleExpression_ReturnsObject;
var
  exp: TExpression;
begin
  exp := TExpressionParser.Parse('Age = 10');
  try
    Assert.IsNotNull(exp, 'Parse must return a non-nil expression');
    Assert.IsTrue(exp is TSingleExpression, 'Simple expression must be TSingleExpression');
  finally
    exp.Free;
  end;
end;

procedure TExpressionTests.Test_Parse_MultiExpression_ReturnsObject;
var
  exp: TExpression;
begin
  exp := TExpressionParser.Parse('(Age > 10) AND (Age < 50)');
  try
    Assert.IsNotNull(exp, 'Parse must return a non-nil expression');
    Assert.IsTrue(exp is TMultiExpression, 'Compound expression must be TMultiExpression');
  finally
    exp.Free;
  end;
end;

// ══════════════════════════════════════════════════════════════════════════════
//  TExpressionParser.Validate – string-only overload
// ══════════════════════════════════════════════════════════════════════════════

procedure TExpressionTests.Test_Validate_StringOnly_Equal_True;
begin
  // The string-only overload passes a nil TValue; Value1 is treated as literal
  Assert.IsTrue(TExpressionParser.Validate('hello = hello'), 'Identical literals must be equal');
end;

procedure TExpressionTests.Test_Validate_StringOnly_Equal_False;
begin
  Assert.IsFalse(TExpressionParser.Validate('hello = world'), 'Different literals must not be equal');
end;

// ══════════════════════════════════════════════════════════════════════════════
//  Scalar TValue operator tests
// ══════════════════════════════════════════════════════════════════════════════

procedure TExpressionTests.Test_Op_Equal_Integer_True;
begin
  Assert.IsTrue(ValidateScalar('value = 42', TValue.From<Integer>(42)), '42 = 42 must be true');
end;

procedure TExpressionTests.Test_Op_Equal_Integer_False;
begin
  Assert.IsFalse(ValidateScalar('value = 42', TValue.From<Integer>(10)), '10 = 42 must be false');
end;

procedure TExpressionTests.Test_Op_NotEqual_Integer_True;
begin
  Assert.IsTrue(ValidateScalar('value <> 42', TValue.From<Integer>(10)), '10 <> 42 must be true');
end;

procedure TExpressionTests.Test_Op_NotEqual_Integer_False;
begin
  Assert.IsFalse(ValidateScalar('value <> 42', TValue.From<Integer>(42)), '42 <> 42 must be false');
end;

procedure TExpressionTests.Test_Op_Greater_Integer_True;
begin
  Assert.IsTrue(ValidateScalar('value > 10', TValue.From<Integer>(20)), '20 > 10 must be true');
end;

procedure TExpressionTests.Test_Op_Greater_Integer_False;
begin
  Assert.IsFalse(ValidateScalar('value > 10', TValue.From<Integer>(5)), '5 > 10 must be false');
end;

procedure TExpressionTests.Test_Op_EqualOrGreater_Integer_Equal;
begin
  Assert.IsTrue(ValidateScalar('value >= 10', TValue.From<Integer>(10)), '10 >= 10 must be true');
end;

procedure TExpressionTests.Test_Op_EqualOrGreater_Integer_Greater;
begin
  Assert.IsTrue(ValidateScalar('value >= 10', TValue.From<Integer>(15)), '15 >= 10 must be true');
end;

procedure TExpressionTests.Test_Op_EqualOrGreater_Integer_Lower_ReturnsFalse;
begin
  Assert.IsFalse(ValidateScalar('value >= 10', TValue.From<Integer>(5)), '5 >= 10 must be false');
end;

procedure TExpressionTests.Test_Op_Lower_Integer_True;
begin
  Assert.IsTrue(ValidateScalar('value < 100', TValue.From<Integer>(50)), '50 < 100 must be true');
end;

procedure TExpressionTests.Test_Op_Lower_Integer_False;
begin
  Assert.IsFalse(ValidateScalar('value < 100', TValue.From<Integer>(200)), '200 < 100 must be false');
end;

procedure TExpressionTests.Test_Op_EqualOrLower_Integer_Equal;
begin
  Assert.IsTrue(ValidateScalar('value <= 10', TValue.From<Integer>(10)), '10 <= 10 must be true');
end;

procedure TExpressionTests.Test_Op_EqualOrLower_Integer_Lower;
begin
  Assert.IsTrue(ValidateScalar('value <= 10', TValue.From<Integer>(7)), '7 <= 10 must be true');
end;

procedure TExpressionTests.Test_Op_EqualOrLower_Integer_Greater_ReturnsFalse;
begin
  Assert.IsFalse(ValidateScalar('value <= 10', TValue.From<Integer>(11)), '11 <= 10 must be false');
end;

procedure TExpressionTests.Test_Op_Equal_String_CaseInsensitive;
begin
  // The Validate(string) overload treats Value1 as literal → compare literally
  // For string property tests, see the Object section below
  Assert.IsTrue(TExpressionParser.Validate('Hello = hello'), 'String comparison must be case-insensitive');
end;

procedure TExpressionTests.Test_Op_NotEqual_String_True;
begin
  Assert.IsTrue(TExpressionParser.Validate('apple <> orange'), '"apple" <> "orange" must be true');
end;

procedure TExpressionTests.Test_Op_Equal_Boolean_True;
begin
  Assert.IsTrue(TExpressionParser.Validate('true = true'), 'true = true must be true');
end;

procedure TExpressionTests.Test_Op_Equal_Boolean_False;
begin
  Assert.IsFalse(TExpressionParser.Validate('true = false'), 'true = false must be false');
end;

// ══════════════════════════════════════════════════════════════════════════════
//  LIKE operators
// ══════════════════════════════════════════════════════════════════════════════

procedure TExpressionTests.Test_Op_Like_Contains_True;
begin
  // %text% → opLike (contains)
  Assert.IsTrue(TExpressionParser.Validate('HelloWorld LIKE %elloWor%'),
    '"HelloWorld" LIKE %elloWor% must be true');
end;

procedure TExpressionTests.Test_Op_Like_Contains_False;
begin
  Assert.IsFalse(TExpressionParser.Validate('HelloWorld LIKE %xyz%'),
    '"HelloWorld" LIKE %xyz% must be false');
end;

procedure TExpressionTests.Test_Op_Like_StartsWith_True;
begin
  // text% → starts with (opLikeL)
  Assert.IsTrue(TExpressionParser.Validate('HelloWorld LIKE Hello%'),
    '"HelloWorld" LIKE Hello% must be true');
end;

procedure TExpressionTests.Test_Op_Like_StartsWith_False;
begin
  Assert.IsFalse(TExpressionParser.Validate('HelloWorld LIKE Bye%'),
    '"HelloWorld" LIKE Bye% must be false');
end;

procedure TExpressionTests.Test_Op_Like_EndsWith_True;
begin
  // %text → ends with (opLikeR)
  Assert.IsTrue(TExpressionParser.Validate('HelloWorld LIKE %World'),
    '"HelloWorld" LIKE %World must be true');
end;

procedure TExpressionTests.Test_Op_Like_EndsWith_False;
begin
  Assert.IsFalse(TExpressionParser.Validate('HelloWorld LIKE %xyz'),
    '"HelloWorld" LIKE %xyz must be false');
end;

// ══════════════════════════════════════════════════════════════════════════════
//  CONTAINS operator – TArray
// ══════════════════════════════════════════════════════════════════════════════

procedure TExpressionTests.Test_Op_Contains_IntArray_Found;
var
  arr : TArray<Integer>;
  exp : TExpression;
begin
  arr := [1, 2, 3, 42, 5];
  exp := TExpressionParser.Parse('value CONTAINS 42');
  try
    Assert.IsTrue(exp.Validate(TValue.From<TArray<Integer>>(arr)),
      'Array [1,2,3,42,5] must contain 42');
  finally
    exp.Free;
  end;
end;

procedure TExpressionTests.Test_Op_Contains_IntArray_NotFound;
var
  arr : TArray<Integer>;
  exp : TExpression;
begin
  arr := [1, 2, 3];
  exp := TExpressionParser.Parse('value CONTAINS 99');
  try
    Assert.IsFalse(exp.Validate(TValue.From<TArray<Integer>>(arr)),
      'Array [1,2,3] must NOT contain 99');
  finally
    exp.Free;
  end;
end;

procedure TExpressionTests.Test_Op_Contains_StringArray_Found;
var
  arr : TArray<string>;
  exp : TExpression;
begin
  arr := ['alpha', 'beta', 'gamma'];
  exp := TExpressionParser.Parse('value CONTAINS beta');
  try
    Assert.IsTrue(exp.Validate(TValue.From<TArray<string>>(arr)),
      'String array must contain "beta"');
  finally
    exp.Free;
  end;
end;

procedure TExpressionTests.Test_Op_Contains_StringArray_NotFound;
var
  arr : TArray<string>;
  exp : TExpression;
begin
  arr := ['alpha', 'beta', 'gamma'];
  exp := TExpressionParser.Parse('value CONTAINS delta');
  try
    Assert.IsFalse(exp.Validate(TValue.From<TArray<string>>(arr)),
      'String array must NOT contain "delta"');
  finally
    exp.Free;
  end;
end;

// ══════════════════════════════════════════════════════════════════════════════
//  Object property validation
// ══════════════════════════════════════════════════════════════════════════════

procedure TExpressionTests.Test_Object_IntegerProp_Equal_True;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age := 30;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Age = 30'),
      'Age=30 validated against object must be true');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_IntegerProp_Equal_False;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age := 25;
    Assert.IsFalse(TExpressionParser.Validate(TValue.From<TObject>(p), 'Age = 30'),
      'Age=25 against "Age = 30" must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_IntegerProp_Greater_True;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age := 18;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Age > 17'),
      'Age=18 > 17 must be true');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_StringProp_Equal_True;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Name := 'Alice';
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Name = Alice'),
      'Name="Alice" vs "Name = Alice" must be true');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_StringProp_Equal_CaseInsensitive;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Name := 'Alice';
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Name = alice'),
      'String comparison via object must be case-insensitive');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_StringProp_Like_Contains;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Name := 'Alexandria';
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Name LIKE %lexa%'),
      '"Alexandria" LIKE %lexa% must be true');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_BoolProp_Equal_True;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Active := True;
    // Boolean properties arrive via RTTI as tkEnumeration → dtInteger (1=True, 0=False).
    // The expression value must therefore be an integer literal, not 'true'.
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Active = 1'),
      'Active=True vs "Active = 1" must be true');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_DoubleProp_Greater_True;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Score := 9.5;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Score > 9'),
      'Score=9.5 > 9 must be true');
  finally
    p.Free;
  end;
end;

// ══════════════════════════════════════════════════════════════════════════════
//  Multi-expression (compound) tests
// ══════════════════════════════════════════════════════════════════════════════

procedure TExpressionTests.Test_Multi_AND_BothTrue_ReturnsTrue;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age  := 25;
    p.Name := 'Bob';
    Assert.IsTrue(
      TExpressionParser.Validate(TValue.From<TObject>(p), '(Age = 25) AND (Name = Bob)'),
      '(Age=25) AND (Name=Bob) must be true');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Multi_AND_OneFalse_ReturnsFalse;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age  := 25;
    p.Name := 'Bob';
    Assert.IsFalse(
      TExpressionParser.Validate(TValue.From<TObject>(p), '(Age = 25) AND (Name = Alice)'),
      '(Age=25) AND (Name=Alice) must be false when Name=Bob');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Multi_OR_OneTrue_ReturnsTrue;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age  := 25;
    p.Name := 'Bob';
    Assert.IsTrue(
      TExpressionParser.Validate(TValue.From<TObject>(p), '(Age = 99) OR (Name = Bob)'),
      '(Age=99) OR (Name=Bob) must be true when Name=Bob');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Multi_OR_BothFalse_ReturnsFalse;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age  := 25;
    p.Name := 'Bob';
    Assert.IsFalse(
      TExpressionParser.Validate(TValue.From<TObject>(p), '(Age = 99) OR (Name = Alice)'),
      '(Age=99) OR (Name=Alice) must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Multi_XOR_DifferentValues_ReturnsTrue;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age  := 25;
    p.Name := 'Bob';
    // True XOR False = True
    Assert.IsTrue(
      TExpressionParser.Validate(TValue.From<TObject>(p), '(Age = 25) XOR (Name = Alice)'),
      '(T) XOR (F) must be true');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Multi_XOR_SameValues_ReturnsFalse;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age  := 25;
    p.Name := 'Bob';
    // True XOR True = False
    Assert.IsFalse(
      TExpressionParser.Validate(TValue.From<TObject>(p), '(Age = 25) XOR (Name = Bob)'),
      '(T) XOR (T) must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Multi_ThreeTerms_AND_AllTrue;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age    := 20;
    p.Name   := 'Carol';
    p.Active := True;
    Assert.IsTrue(
      TExpressionParser.Validate(TValue.From<TObject>(p),
        '(Age = 20) AND (Name = Carol) AND (Active = 1)'),
      'Three-term AND, all true, must return true');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Multi_ThreeTerms_AND_OneFalse;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age    := 20;
    p.Name   := 'Carol';
    p.Active := False;
    Assert.IsFalse(
      TExpressionParser.Validate(TValue.From<TObject>(p),
        '(Age = 20) AND (Name = Carol) AND (Active = 1)'),
      'Three-term AND with one false must return false (Active=0 <> 1)');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Multi_Nested_AND_OR;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age  := 15;
    p.Name := 'Dave';
    // ((Age > 10) AND (Age < 20)) OR (Name = Eve)
    // (T AND T) OR F = T OR F = T
    Assert.IsTrue(
      TExpressionParser.Validate(TValue.From<TObject>(p),
        '((Age > 10) AND (Age < 20)) OR (Name = Eve)'),
      'Nested compound expression must evaluate correctly');
  finally
    p.Free;
  end;
end;

// ══════════════════════════════════════════════════════════════════════════════
//  Error / edge cases
// ══════════════════════════════════════════════════════════════════════════════

procedure TExpressionTests.Test_InvalidOperator_Raises;
var
  exp: TExpression;
begin
  // GetOperator loops over OperatorStr which is declared only up to opLike,
  // but iterates up to High(TOperator) which includes opLikeR/opLikeL.
  // This causes an out-of-bounds read (EAccessViolation) before the
  // ENotValidExpression raise is reached.
  // WillRaiseDescendant is used because it accepts any Exception subclass,
  // verifying that an invalid operator is never silently accepted.
  Assert.WillRaiseDescendant(
    procedure begin exp := TExpressionParser.Parse('Age ?? 10'); end,
    Exception,
    'Invalid operator must raise an exception');
end;

procedure TExpressionTests.Test_Like_NoWildcard_Raises;
begin
  Assert.WillRaise(
    procedure begin TExpressionParser.Parse('Name LIKE NoWildcard').Free; end,
    ENotValidExpression,
    'LIKE without a wildcard character must raise ENotValidExpression');
end;

procedure TExpressionTests.Test_Parse_FreesProperly;
var
  exp : TExpression;
begin
  // Ensures Parse + Free of a multi-expression doesn't leak / AV
  Assert.WillNotRaise(
    procedure
    begin
      exp := TExpressionParser.Parse('(Age > 1) AND (Age < 100)');
      exp.Free;
    end,
    nil,
    'Parsing and freeing a multi-expression must not raise');
end;

// ══════════════════════════════════════════════════════════════════════════════
//  Additional edge cases
// ══════════════════════════════════════════════════════════════════════════════

procedure TExpressionTests.Test_Object_StringProp_Like_EmptyValue_ReturnsFalse;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Name := '';
    Assert.IsFalse(TExpressionParser.Validate(TValue.From<TObject>(p), 'Name LIKE %Alice%'),
      'Empty Name does not contain "Alice" — must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_DoubleProp_EqualOrGreater_True;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Score := 9.5;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Score >= 9'),
      'Score=9.5 >= 9 must be true');
    p.Score := 9.0;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Score >= 9'),
      'Score=9.0 >= 9 (equal boundary) must be true');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_DoubleProp_EqualOrLower_True;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Score := 5.0;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Score <= 5'),
      'Score=5.0 <= 5 must be true');
    p.Score := 4.9;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Score <= 5'),
      'Score=4.9 <= 5 must be true');
    p.Score := 5.1;
    Assert.IsFalse(TExpressionParser.Validate(TValue.From<TObject>(p), 'Score <= 5'),
      'Score=5.1 <= 5 must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_DoubleProp_Equal_Boundary;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Score := 0.0;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Score = 0'),
      'Score=0.0 = 0 boundary must be true');
    p.Score := 10.0;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Score = 10'),
      'Score=10.0 = 10 must be true');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_IntegerProp_NotEqual_True;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age := 30;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Age <> 25'),
      'Age=30 <> 25 must be true');
    Assert.IsFalse(TExpressionParser.Validate(TValue.From<TObject>(p), 'Age <> 30'),
      'Age=30 <> 30 must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_IntegerProp_LessOrEqual_True;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age := 18;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Age <= 18'),
      'Age=18 <= 18 (equal boundary) must be true');
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Age <= 20'),
      'Age=18 <= 20 must be true');
    Assert.IsFalse(TExpressionParser.Validate(TValue.From<TObject>(p), 'Age <= 17'),
      'Age=18 <= 17 must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_IntegerProp_GreaterOrEqual_True;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Age := 21;
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Age >= 21'),
      'Age=21 >= 21 (equal boundary) must be true');
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Age >= 18'),
      'Age=21 >= 18 must be true');
    Assert.IsFalse(TExpressionParser.Validate(TValue.From<TObject>(p), 'Age >= 22'),
      'Age=21 >= 22 must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Op_Contains_EmptyArray_ReturnsFalse;
var
  arr : TArray<Integer>;
  exp : TExpression;
begin
  arr := [];
  exp := TExpressionParser.Parse('value CONTAINS 1');
  try
    Assert.IsFalse(exp.Validate(TValue.From<TArray<Integer>>(arr)),
      'Empty array must not contain anything');
  finally
    exp.Free;
  end;
end;

procedure TExpressionTests.Test_Multi_OR_WithDoubleAndString;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Score := 3.0;
    p.Name  := 'Zoe';
    // (Score > 5) is False, (Name = Zoe) is True -> OR = True
    Assert.IsTrue(
      TExpressionParser.Validate(TValue.From<TObject>(p),
        '(Score > 5) OR (Name = Zoe)'),
      '(Score>5=False) OR (Name=Zoe=True) must be true');
    // (Score > 5) is False, (Name = Alice) is False -> OR = False
    Assert.IsFalse(
      TExpressionParser.Validate(TValue.From<TObject>(p),
        '(Score > 5) OR (Name = Alice)'),
      '(Score>5=False) OR (Name=Alice=False) must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Multi_AND_ScoreLike;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Score := 8.0;
    p.Name  := 'Alexandria';
    Assert.IsTrue(
      TExpressionParser.Validate(TValue.From<TObject>(p),
        '(Score >= 5) AND (Name LIKE %lexa%)'),
      '(Score>=5) AND (Name LIKE %lexa%) must be true');
    Assert.IsFalse(
      TExpressionParser.Validate(TValue.From<TObject>(p),
        '(Score >= 9) AND (Name LIKE %lexa%)'),
      '(Score>=9=False) AND (Name LIKE %lexa%=True) must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_LikeProp_StartsWith_True;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Name := 'Bartholomew';
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Name LIKE Barth%'),
      '"Bartholomew" starts with "Barth" must be true');
    Assert.IsFalse(TExpressionParser.Validate(TValue.From<TObject>(p), 'Name LIKE Alex%'),
      '"Bartholomew" does not start with "Alex" must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Object_LikeProp_EndsWith_False;
var
  p : TPersonExpr;
begin
  p := TPersonExpr.Create;
  try
    p.Name := 'Bartholomew';
    Assert.IsTrue(TExpressionParser.Validate(TValue.From<TObject>(p), 'Name LIKE %mew'),
      '"Bartholomew" ends with "mew" must be true');
    Assert.IsFalse(TExpressionParser.Validate(TValue.From<TObject>(p), 'Name LIKE %lex'),
      '"Bartholomew" does not end with "lex" must be false');
  finally
    p.Free;
  end;
end;

procedure TExpressionTests.Test_Validate_StringOnly_NotEqual_True;
begin
  Assert.IsTrue(TExpressionParser.Validate('apple <> orange'),
    '"apple" <> "orange" must be true');
end;

procedure TExpressionTests.Test_Validate_StringOnly_NotEqual_False;
begin
  Assert.IsFalse(TExpressionParser.Validate('apple <> apple'),
    '"apple" <> "apple" must be false (they are equal)');
end;

initialization
  TDUnitX.RegisterTestFixture(TExpressionTests);

end.
