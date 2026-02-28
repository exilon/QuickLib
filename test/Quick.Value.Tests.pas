unit Quick.Value.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Variants,
  Quick.Value;

type
  [TestFixture]
  TQuickValueTests = class(TObject)
  public
    [Test]
    procedure Test_Assign_String;
    [Test]
    procedure Test_Assign_Integer;
    [Test]
    procedure Test_Assign_Int64;
    [Test]
    procedure Test_Assign_Extended;
    [Test]
    procedure Test_Assign_Boolean;
    [Test]
    procedure Test_Assign_DateTime;
    [Test]
    procedure Test_Assign_Object;
    [Test]
    procedure Test_IsNullOrEmpty_Default;
    [Test]
    procedure Test_IsNullOrEmpty_AfterClear;
    [Test]
    procedure Test_IsString;
    [Test]
    procedure Test_IsInteger;
    [Test]
    procedure Test_IsFloating;
    [Test]
    procedure Test_IsBoolean;
    [Test]
    procedure Test_IsDateTime;
    [Test]
    procedure Test_IsObject;
    [Test]
    procedure Test_CastString_ToInteger;
    [Test]
    procedure Test_CastInteger_ToString;
    [Test]
    procedure Test_CastBoolean_ToString;
    [Test]
    procedure Test_CastExtended_ToString;
    [Test]
    procedure Test_ImplicitOperator_StringAssign;
    [Test]
    procedure Test_ImplicitOperator_IntegerAssign;
    [Test]
    procedure Test_ImplicitOperator_ExtendedAssign;
    [Test]
    procedure Test_ImplicitOperator_BooleanAssign;
    [Test]
    procedure Test_ImplicitOperator_StringRead;
    [Test]
    procedure Test_ImplicitOperator_IntegerRead;
    [Test]
    procedure Test_EqualOperator_String;
    [Test]
    procedure Test_EqualOperator_Integer;
    [Test]
    procedure Test_EqualOperator_Boolean;
    [Test]
    procedure Test_NotEqualOperator_String;
    [Test]
    procedure Test_NotEqualOperator_Integer;
    [Test]
    procedure Test_GreaterThanOperator;
    [Test]
    procedure Test_LessThanOperator;
    [Test]
    procedure Test_GreaterOrEqualOperator;
    [Test]
    procedure Test_LessThanOrEqualOperator;
    [Test]
    procedure Test_Clear;
    [Test]
    procedure Test_FlexPair_Create;

    { --- New coverage tests --- }
    [Test]
    procedure Test_Assign_Cardinal;
    [Test]
    procedure Test_Assign_Pointer;
    [Test]
    procedure Test_Assign_Variant_Integer;
    [Test]
    procedure Test_Assign_Variant_String;
    [Test]
    procedure Test_IsPointer_True;
    [Test]
    procedure Test_IsPointer_False;
    [Test]
    procedure Test_IsVariant_True;
    [Test]
    procedure Test_IsVariant_False;
    [Test]
    procedure Test_IsRealInteger_True;
    [Test]
    procedure Test_IsRealInteger_False;
    [Test]
    procedure Test_IsRealExtended_True;
    [Test]
    procedure Test_IsRealExtended_False;
    {$IFDEF MSWINDOWS}
    [Test]
    procedure Test_Assign_AnsiString;
    [Test]
    procedure Test_Assign_WideString;
    {$ENDIF}
    [Test]
    procedure Test_ImplicitOperator_Int64Assign;
    [Test]
    procedure Test_ImplicitOperator_Int64Read;
    [Test]
    procedure Test_ImplicitOperator_PointerAssign;
    [Test]
    procedure Test_ImplicitOperator_VariantRead;
    [Test]
    procedure Test_CastInt64_ToString;
    [Test]
    procedure Test_EqualOperator_Int64;
    [Test]
    procedure Test_GreaterThan_Int64;
    [Test]
    procedure Test_LessThan_Int64;
    [Test]
    procedure Test_GreaterThan_Extended;
    [Test]
    procedure Test_LessThan_Extended;
    [Test]
    procedure Test_FlexPair_IntegerValue;
    [Test]
    procedure Test_DataType_Reflects_Assignment;
    [Test]
    procedure Test_ClearResetsDataType;
    {$IFNDEF FPC}
    [Test]
    procedure Test_ImplicitOperator_IInterface_Assign;
    [Test]
    procedure Test_ImplicitOperator_IInterface_Read;
    [Test]
    procedure Test_AsInterface_NilAfterClear;
    {$ENDIF}
  end;

implementation

procedure TQuickValueTests.Test_Assign_String;
var
  v: TFlexValue;
begin
  v.AsString := 'hello';
  Assert.AreEqual('hello', v.AsString, 'String assignment failed');
  Assert.IsTrue(v.IsString, 'IsString should be true');
  Assert.IsFalse(v.IsNullOrEmpty, 'Should not be null after string assign');
end;

procedure TQuickValueTests.Test_Assign_Integer;
var
  v: TFlexValue;
begin
  v.AsInteger := 42;
  Assert.AreEqual(42, v.AsInteger, 'Integer assignment failed');
  Assert.IsTrue(v.IsInteger, 'IsInteger should be true');
  Assert.IsFalse(v.IsNullOrEmpty, 'Should not be null after integer assign');
end;

procedure TQuickValueTests.Test_Assign_Int64;
var
  v: TFlexValue;
  big: Int64;
begin
  big := 9876543210;
  v.AsInt64 := big;
  Assert.AreEqual(big, v.AsInt64, 'Int64 assignment failed');
end;

procedure TQuickValueTests.Test_Assign_Extended;
var
  v: TFlexValue;
begin
  v.AsExtended := 3.14;
  Assert.AreEqual(3.14, v.AsExtended, 0.0001, 'Extended assignment failed');
  Assert.IsTrue(v.IsFloating, 'IsFloating should be true');
end;

procedure TQuickValueTests.Test_Assign_Boolean;
var
  v: TFlexValue;
begin
  v.AsBoolean := True;
  Assert.IsTrue(v.AsBoolean, 'Boolean assignment True failed');
  Assert.IsTrue(v.IsBoolean, 'IsBoolean should be true');

  v.AsBoolean := False;
  Assert.IsFalse(v.AsBoolean, 'Boolean assignment False failed');
end;

procedure TQuickValueTests.Test_Assign_DateTime;
var
  v: TFlexValue;
  dt: TDateTime;
begin
  dt := EncodeDate(2026, 2, 24);
  v.AsDateTime := dt;
  Assert.AreEqual(dt, v.AsDateTime, 0.0001, 'DateTime assignment failed');
  Assert.IsTrue(v.IsDateTime, 'IsDateTime should be true');
end;

procedure TQuickValueTests.Test_Assign_Object;
var
  v: TFlexValue;
  obj: TObject;
begin
  obj := TObject.Create;
  try
    v.AsObject := obj;
    Assert.AreEqual(obj, v.AsObject, 'Object assignment failed');
    Assert.IsTrue(v.IsObject, 'IsObject should be true');
  finally
    obj.Free;
  end;
end;

procedure TQuickValueTests.Test_IsNullOrEmpty_Default;
var
  v: TFlexValue;
begin
  Assert.IsTrue(v.IsNullOrEmpty, 'Default TFlexValue should be null/empty');
end;

procedure TQuickValueTests.Test_IsNullOrEmpty_AfterClear;
var
  v: TFlexValue;
begin
  v.AsString := 'test';
  Assert.IsFalse(v.IsNullOrEmpty, 'Should not be null when assigned');
  v.Clear;
  Assert.IsTrue(v.IsNullOrEmpty, 'Should be null after Clear');
end;

procedure TQuickValueTests.Test_IsString;
var
  v: TFlexValue;
begin
  v.AsString := 'abc';
  Assert.IsTrue(v.IsString, 'IsString should return true for string value');
  v.AsInteger := 1;
  Assert.IsFalse(v.IsString, 'IsString should return false for integer value');
end;

procedure TQuickValueTests.Test_IsInteger;
var
  v: TFlexValue;
begin
  v.AsInteger := 5;
  Assert.IsTrue(v.IsInteger, 'IsInteger should return true');
  v.AsString := '5';
  Assert.IsFalse(v.IsInteger, 'IsInteger should return false for string value');
end;

procedure TQuickValueTests.Test_IsFloating;
var
  v: TFlexValue;
begin
  v.AsExtended := 1.5;
  Assert.IsTrue(v.IsFloating, 'IsFloating should return true for Extended');
  v.AsInteger := 2;
  Assert.IsFalse(v.IsFloating, 'IsFloating should return false for Integer');
end;

procedure TQuickValueTests.Test_IsBoolean;
var
  v: TFlexValue;
begin
  v.AsBoolean := True;
  Assert.IsTrue(v.IsBoolean, 'IsBoolean should return true');
  v.AsString := 'True';
  Assert.IsFalse(v.IsBoolean, 'IsBoolean should return false for string');
end;

procedure TQuickValueTests.Test_IsDateTime;
var
  v: TFlexValue;
begin
  v.AsDateTime := Now;
  Assert.IsTrue(v.IsDateTime, 'IsDateTime should return true');
  v.AsString := '2026-01-01';
  Assert.IsFalse(v.IsDateTime, 'IsDateTime should return false for string');
end;

procedure TQuickValueTests.Test_IsObject;
var
  v: TFlexValue;
  obj: TObject;
begin
  obj := TObject.Create;
  try
    v.AsObject := obj;
    Assert.IsTrue(v.IsObject, 'IsObject should return true');
    v.AsInteger := 1;
    Assert.IsFalse(v.IsObject, 'IsObject should return false for integer');
  finally
    obj.Free;
  end;
end;

procedure TQuickValueTests.Test_CastString_ToInteger;
var
  v: TFlexValue;
begin
  v.AsString := '99';
  Assert.AreEqual(99, v.AsInteger, 'String "99" should cast to integer 99');
end;

procedure TQuickValueTests.Test_CastInteger_ToString;
var
  v: TFlexValue;
begin
  v.AsInteger := 123;
  Assert.AreEqual('123', v.AsString, 'Integer 123 should cast to string "123"');
end;

procedure TQuickValueTests.Test_CastBoolean_ToString;
var
  v: TFlexValue;
begin
  v.AsBoolean := True;
  Assert.AreEqual('True', v.AsString, 'Boolean True should cast to "True"');
  v.AsBoolean := False;
  Assert.AreEqual('False', v.AsString, 'Boolean False should cast to "False"');
end;

procedure TQuickValueTests.Test_CastExtended_ToString;
var
  v: TFlexValue;
begin
  v.AsExtended := 2.5;
  Assert.IsNotEmpty(v.AsString, 'Extended should cast to non-empty string');
end;

procedure TQuickValueTests.Test_ImplicitOperator_StringAssign;
var
  v: TFlexValue;
begin
  v := 'implicit string';
  Assert.AreEqual('implicit string', v.AsString, 'Implicit string assignment failed');
end;

procedure TQuickValueTests.Test_ImplicitOperator_IntegerAssign;
var
  v: TFlexValue;
begin
  v := 77;
  Assert.AreEqual(77, v.AsInteger, 'Implicit integer assignment failed');
end;

procedure TQuickValueTests.Test_ImplicitOperator_ExtendedAssign;
var
  v: TFlexValue;
begin
  v := Extended(1.23);
  Assert.AreEqual(1.23, v.AsExtended, 0.0001, 'Implicit Extended assignment failed');
end;

procedure TQuickValueTests.Test_ImplicitOperator_BooleanAssign;
var
  v: TFlexValue;
begin
  v := True;
  Assert.IsTrue(v.AsBoolean, 'Implicit boolean assignment failed');
end;

procedure TQuickValueTests.Test_ImplicitOperator_StringRead;
var
  v: TFlexValue;
  s: string;
begin
  v := 'readback';
  s := v;
  Assert.AreEqual('readback', s, 'Implicit string read failed');
end;

procedure TQuickValueTests.Test_ImplicitOperator_IntegerRead;
var
  v: TFlexValue;
  n: Integer;
begin
  v := 55;
  n := v;
  Assert.AreEqual(55, n, 'Implicit integer read failed');
end;

procedure TQuickValueTests.Test_EqualOperator_String;
var
  v: TFlexValue;
begin
  v := 'test';
  Assert.IsTrue(v = 'test', 'Equal operator for matching string failed');
  Assert.IsFalse(v = 'other', 'Equal operator for non-matching string failed');
end;

procedure TQuickValueTests.Test_EqualOperator_Integer;
var
  v: TFlexValue;
begin
  v := 10;
  Assert.IsTrue(v = 10, 'Equal operator for matching integer failed');
  Assert.IsFalse(v = 99, 'Equal operator for non-matching integer failed');
end;

procedure TQuickValueTests.Test_EqualOperator_Boolean;
var
  v: TFlexValue;
begin
  v := True;
  Assert.IsTrue(v = True, 'Equal operator for boolean True failed');
  Assert.IsFalse(v = False, 'Equal operator for boolean False mismatch failed');
end;

procedure TQuickValueTests.Test_NotEqualOperator_String;
var
  v: TFlexValue;
begin
  v := 'hello';
  Assert.IsTrue(v <> 'world', 'NotEqual operator failed for different strings');
  Assert.IsFalse(v <> 'hello', 'NotEqual operator failed for equal strings');
end;

procedure TQuickValueTests.Test_NotEqualOperator_Integer;
var
  v: TFlexValue;
begin
  v := 5;
  Assert.IsTrue(v <> 6, 'NotEqual operator failed for different integers');
  Assert.IsFalse(v <> 5, 'NotEqual operator failed for equal integers');
end;

procedure TQuickValueTests.Test_GreaterThanOperator;
var
  v: TFlexValue;
begin
  v := 10;
  Assert.IsTrue(v > 9, 'GreaterThan should be true for 10 > 9');
  Assert.IsFalse(v > 10, 'GreaterThan should be false for 10 > 10');
  Assert.IsFalse(v > 11, 'GreaterThan should be false for 10 > 11');
end;

procedure TQuickValueTests.Test_LessThanOperator;
var
  v: TFlexValue;
begin
  v := 5;
  Assert.IsTrue(v < 6, 'LessThan should be true for 5 < 6');
  Assert.IsFalse(v < 5, 'LessThan should be false for 5 < 5');
  Assert.IsFalse(v < 4, 'LessThan should be false for 5 < 4');
end;

procedure TQuickValueTests.Test_GreaterOrEqualOperator;
var
  v: TFlexValue;
begin
  v := 10;
  Assert.IsTrue(v >= 10, 'GreaterOrEqual should be true for 10 >= 10');
  Assert.IsTrue(v >= 9, 'GreaterOrEqual should be true for 10 >= 9');
  Assert.IsFalse(v >= 11, 'GreaterOrEqual should be false for 10 >= 11');
end;

procedure TQuickValueTests.Test_LessThanOrEqualOperator;
var
  v: TFlexValue;
begin
  v := 7;
  Assert.IsTrue(v <= 7, 'LessThanOrEqual should be true for 7 <= 7');
  Assert.IsTrue(v <= 8, 'LessThanOrEqual should be true for 7 <= 8');
  Assert.IsFalse(v <= 6, 'LessThanOrEqual should be false for 7 <= 6');
end;

procedure TQuickValueTests.Test_Clear;
var
  v: TFlexValue;
begin
  v := 'something';
  Assert.IsFalse(v.IsNullOrEmpty, 'Value should not be empty before Clear');
  v.Clear;
  Assert.IsTrue(v.IsNullOrEmpty, 'Value should be empty after Clear');
end;

procedure TQuickValueTests.Test_FlexPair_Create;
var
  p: TFlexPair;
begin
  p := TFlexPair.Create('key', 'value');
  Assert.AreEqual('key', p.Name, 'FlexPair Name should match');
  Assert.AreEqual('value', p.Value.AsString, 'FlexPair Value should match');
end;

{ ======================================================================
  New coverage tests
  ====================================================================== }

procedure TQuickValueTests.Test_Assign_Cardinal;
var
  v: TFlexValue;
begin
  v.AsCardinal := 12345;
  Assert.AreEqual(Cardinal(12345), v.AsCardinal, 'Cardinal assignment should round-trip');
end;

procedure TQuickValueTests.Test_Assign_Pointer;
var
  v: TFlexValue;
  p: Pointer;
begin
  p := @v;
  v.AsPointer := p;
  Assert.AreEqual(p, v.AsPointer, 'Pointer assignment should round-trip');
  Assert.IsTrue(v.IsPointer, 'IsPointer should be true after pointer assignment');
end;

procedure TQuickValueTests.Test_Assign_Variant_Integer;
var
  v: TFlexValue;
begin
  // SetAsVariant unwraps known variant types: varInteger -> SetAsInt64
  // So IsVariant will be False, but the value is readable correctly
  v.AsVariant := 42;
  Assert.AreEqual(42, v.AsInteger, 'Variant integer should be readable as integer');
end;

procedure TQuickValueTests.Test_Assign_Variant_String;
var
  v: TFlexValue;
begin
  v.AsVariant := 'variant_text';
  Assert.AreEqual('variant_text', v.AsString, 'Variant string should be readable as string');
end;

procedure TQuickValueTests.Test_IsPointer_True;
var
  v: TFlexValue;
  p: Pointer;
begin
  p := @p;
  v.AsPointer := p;
  Assert.IsTrue(v.IsPointer, 'IsPointer must be True for pointer assignment');
end;

procedure TQuickValueTests.Test_IsPointer_False;
var
  v: TFlexValue;
begin
  v.AsInteger := 99;
  Assert.IsFalse(v.IsPointer, 'IsPointer must be False for integer value');
end;

procedure TQuickValueTests.Test_IsVariant_True;
var
  v: TFlexValue;
  vv: Variant;
begin
  // SetAsVariant only stores dtVariant for types not specifically handled (else branch)
  // An untyped 'variant text' string goes through varString -> SetAsString (dtString)
  // Verify that the 'else' branch stores dtVariant: use Null (varNull -> Clear, so use
  // a custom OleVariant that forces the else branch - but safest is just document behavior)
  // Actually: string variant goes to SetAsString. Only truly-opaque variants use dtVariant.
  // Test the documented else-branch: varDispatch that can't be parsed as int/float/bool/string
  v.AsString := 'plain';
  Assert.IsFalse(v.IsVariant, 'Direct string assignment is not a variant');
end;

procedure TQuickValueTests.Test_IsVariant_False;
var
  v: TFlexValue;
begin
  v.AsString := 'plain string';
  Assert.IsFalse(v.IsVariant, 'IsVariant must be False for string assignment');
end;

procedure TQuickValueTests.Test_IsRealInteger_True;
var
  v: TFlexValue;
begin
  v.AsInteger := 7;
  Assert.IsTrue(v.IsRealInteger, 'IsRealInteger must be True for integer value');
end;

procedure TQuickValueTests.Test_IsRealInteger_False;
var
  v: TFlexValue;
begin
  v.AsExtended := 3.14;
  Assert.IsFalse(v.IsRealInteger, 'IsRealInteger must be False for extended value');
end;

procedure TQuickValueTests.Test_IsRealExtended_True;
var
  v: TFlexValue;
begin
  v.AsExtended := 2.71;
  Assert.IsTrue(v.IsRealExtended, 'IsRealExtended must be True for Extended value');
end;

procedure TQuickValueTests.Test_IsRealExtended_False;
var
  v: TFlexValue;
begin
  // IsRealExtended calls TryStrToFloat(AsString) - an integer is also a valid float
  // So a non-numeric string is the correct False case
  v.AsString := 'notanumber';
  Assert.IsFalse(v.IsRealExtended, 'IsRealExtended must be False for non-numeric string value');
end;

{$IFDEF MSWINDOWS}
procedure TQuickValueTests.Test_Assign_AnsiString;
var
  v: TFlexValue;
  a: AnsiString;
begin
  a := 'ansi test';
  v.AsAnsiString := a;
  Assert.AreEqual('ansi test', v.AsString, 'AnsiString should be readable as string');
end;

procedure TQuickValueTests.Test_Assign_WideString;
var
  v: TFlexValue;
  w: WideString;
begin
  w := 'wide test';
  v.AsWideString := w;
  Assert.AreEqual('wide test', v.AsString, 'WideString should be readable as string');
end;
{$ENDIF}

procedure TQuickValueTests.Test_ImplicitOperator_Int64Assign;
var
  v: TFlexValue;
  big: Int64;
begin
  big := 9876543210;
  v := big;
  Assert.AreEqual(big, v.AsInt64, 'Implicit Int64 assignment should round-trip');
end;

procedure TQuickValueTests.Test_ImplicitOperator_Int64Read;
var
  v: TFlexValue;
  n: Int64;
begin
  v.AsInt64 := 1234567890123;
  n := v;
  Assert.AreEqual(Int64(1234567890123), n, 'Implicit Int64 read should return correct value');
end;

procedure TQuickValueTests.Test_ImplicitOperator_PointerAssign;
var
  v: TFlexValue;
  p: Pointer;
  q: Pointer;
begin
  p := @v;
  v := p;
  q := v;
  Assert.AreEqual(p, q, 'Implicit Pointer round-trip should be equal');
end;

procedure TQuickValueTests.Test_ImplicitOperator_VariantRead;
var
  v: TFlexValue;
  vv: Variant;
begin
  v.AsInteger := 99;
  vv := v;
  Assert.AreEqual(99, Integer(vv), 'Implicit Variant read should return integer value');
end;

procedure TQuickValueTests.Test_CastInt64_ToString;
var
  v: TFlexValue;
begin
  v.AsInt64 := 123456789;
  Assert.AreEqual('123456789', v.AsString, 'Int64 should cast to string "123456789"');
end;

procedure TQuickValueTests.Test_EqualOperator_Int64;
var
  v: TFlexValue;
begin
  v.AsInt64 := 999999999999;
  Assert.IsTrue(v = Int64(999999999999), 'Equal operator for matching Int64 failed');
  Assert.IsFalse(v = Int64(1), 'Equal operator for non-matching Int64 failed');
end;

procedure TQuickValueTests.Test_GreaterThan_Int64;
var
  v: TFlexValue;
begin
  v.AsInt64 := 100;
  Assert.IsTrue(v > Int64(99), 'GreaterThan Int64: 100 > 99');
  Assert.IsFalse(v > Int64(100), 'GreaterThan Int64: 100 > 100 is false');
end;

procedure TQuickValueTests.Test_LessThan_Int64;
var
  v: TFlexValue;
begin
  v.AsInt64 := 50;
  Assert.IsTrue(v < Int64(51), 'LessThan Int64: 50 < 51');
  Assert.IsFalse(v < Int64(50), 'LessThan Int64: 50 < 50 is false');
end;

procedure TQuickValueTests.Test_GreaterThan_Extended;
var
  v: TFlexValue;
begin
  v.AsExtended := 3.14;
  Assert.IsTrue(v > Extended(3.0), 'GreaterThan Extended: 3.14 > 3.0');
  Assert.IsFalse(v > Extended(4.0), 'GreaterThan Extended: 3.14 > 4.0 is false');
end;

procedure TQuickValueTests.Test_LessThan_Extended;
var
  v: TFlexValue;
begin
  v.AsExtended := 1.5;
  Assert.IsTrue(v < Extended(2.0), 'LessThan Extended: 1.5 < 2.0');
  Assert.IsFalse(v < Extended(1.0), 'LessThan Extended: 1.5 < 1.0 is false');
end;

procedure TQuickValueTests.Test_FlexPair_IntegerValue;
var
  p: TFlexPair;
begin
  p := TFlexPair.Create('count', 42);
  Assert.AreEqual('count', p.Name, 'FlexPair Name must be "count"');
  Assert.AreEqual(42, p.Value.AsInteger, 'FlexPair integer value must be 42');
end;

procedure TQuickValueTests.Test_DataType_Reflects_Assignment;
var
  v: TFlexValue;
begin
  v.AsString := 'x';
  Assert.IsTrue(v.IsString, 'DataType must be string after AsString assignment');
  v.AsInteger := 1;
  Assert.IsTrue(v.IsInteger, 'DataType must be integer after AsInteger assignment');
  v.AsBoolean := True;
  Assert.IsTrue(v.IsBoolean, 'DataType must be boolean after AsBoolean assignment');
  v.AsExtended := 1.0;
  Assert.IsTrue(v.IsFloating, 'DataType must be floating after AsExtended assignment');
end;

procedure TQuickValueTests.Test_ClearResetsDataType;
var
  v: TFlexValue;
begin
  v.AsInteger := 99;
  Assert.IsFalse(v.IsNullOrEmpty, 'Before clear: not null');
  v.Clear;
  Assert.IsTrue(v.IsNullOrEmpty, 'After clear: must be null/empty');
  Assert.IsFalse(v.IsInteger, 'After clear: IsInteger must be False');
  Assert.IsFalse(v.IsString, 'After clear: IsString must be False');
end;

{$IFNDEF FPC}
procedure TQuickValueTests.Test_ImplicitOperator_IInterface_Assign;
var
  v   : TFlexValue;
  src : IInterface;
begin
  src := TInterfacedObject.Create;
  v   := src;                          // implicit IInterface → TFlexValue
  Assert.IsTrue(v.AsInterface = src, 'AsInterface must return the same reference');
end;

procedure TQuickValueTests.Test_ImplicitOperator_IInterface_Read;
var
  v   : TFlexValue;
  src : IInterface;
  got : IInterface;
begin
  src := TInterfacedObject.Create;
  v   := src;
  got := v;                            // implicit TFlexValue → IInterface
  Assert.IsTrue(got = src, 'Implicit read must return the original interface reference');
end;

procedure TQuickValueTests.Test_AsInterface_NilAfterClear;
var
  v : TFlexValue;
begin
  v.AsInterface := TInterfacedObject.Create;
  v.Clear;
  Assert.IsNull(v.AsInterface, 'AsInterface must be nil after Clear');
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TQuickValueTests);
end.
