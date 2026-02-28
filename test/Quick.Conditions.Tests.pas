unit Quick.Conditions.Tests;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  Quick.Conditions;

type
  ECustomTestException = class(Exception);

  [TestFixture]
  TQuickConditionsTests = class(TObject)
  public
    // String conditions
    [Test] procedure Test_String_IsNotEmpty_Passes;
    [Test] procedure Test_String_IsNotEmpty_Fails;
    [Test] procedure Test_String_IsEmpty_Passes;
    [Test] procedure Test_String_IsEmpty_Fails;
    [Test] procedure Test_String_StartsWith_Passes;
    [Test] procedure Test_String_StartsWith_Fails;
    [Test] procedure Test_String_EndsWith_Passes;
    [Test] procedure Test_String_EndsWith_Fails;
    [Test] procedure Test_String_Contains_Passes;
    [Test] procedure Test_String_Contains_Fails;
    [Test] procedure Test_String_DoesNotContains_Passes;
    [Test] procedure Test_String_DoesNotContains_Fails;
    [Test] procedure Test_String_IsLongerThan_Passes;
    [Test] procedure Test_String_IsLongerThan_Fails;
    [Test] procedure Test_String_IsShorterThan_Passes;
    [Test] procedure Test_String_IsShorterThan_Fails;
    [Test] procedure Test_String_HasLength_Passes;
    [Test] procedure Test_String_HasLength_Fails;
    [Test] procedure Test_String_HasLengthRange_Passes;
    [Test] procedure Test_String_HasLengthRange_Fails;
    [Test] procedure Test_String_IsUpperCase_Passes;
    [Test] procedure Test_String_IsUpperCase_Fails;
    [Test] procedure Test_String_IsLowerCase_Passes;
    [Test] procedure Test_String_IsLowerCase_Fails;
    [Test] procedure Test_String_Evaluate_Passes;
    [Test] procedure Test_String_Evaluate_Fails;
    [Test] procedure Test_String_WithExceptionOnFailure;
    [Test] procedure Test_String_CustomMessage;
    // Integer conditions
    [Test] procedure Test_Integer_IsInRange_Passes;
    [Test] procedure Test_Integer_IsInRange_Fails;
    [Test] procedure Test_Integer_IsEqualTo_Passes;
    [Test] procedure Test_Integer_IsEqualTo_Fails;
    [Test] procedure Test_Integer_IsGreaterThan_Passes;
    [Test] procedure Test_Integer_IsGreaterThan_Fails;
    [Test] procedure Test_Integer_IsLessThan_Passes;
    [Test] procedure Test_Integer_IsLessThan_Fails;
    [Test] procedure Test_Integer_IsNotEqualTo_Passes;
    [Test] procedure Test_Integer_IsGreaterOrEqual_Passes;
    [Test] procedure Test_Integer_IsLessOrEqual_Passes;
    [Test] procedure Test_Integer_Evaluate_Passes;
    [Test] procedure Test_Integer_Evaluate_Fails;
    // Float conditions
    [Test] procedure Test_Float_IsInRange_Passes;
    [Test] procedure Test_Float_IsInRange_Fails;
    [Test] procedure Test_Float_IsGreaterThan_Passes;
    [Test] procedure Test_Float_IsLessThan_Passes;
    // Object conditions
    [Test] procedure Test_Object_IsNotNull_Passes;
    [Test] procedure Test_Object_IsNotNull_Fails;
    [Test] procedure Test_Object_IsNull_Passes;
    [Test] procedure Test_Object_IsNull_Fails;
    [Test] procedure Test_Object_IsOfType_Passes;
    [Test] procedure Test_Object_IsOfType_Fails;
    [Test] procedure Test_Object_Evaluate_Passes;
    // Ensures (post-condition)
    [Test] procedure Test_Ensures_String_IsNotEmpty_Passes;
    [Test] procedure Test_Ensures_String_IsNotEmpty_Fails;
    [Test] procedure Test_Ensures_Integer_IsInRange_Passes;
  end;

implementation

{ String conditions }

procedure TQuickConditionsTests.Test_String_IsNotEmpty_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('hello', 'str').IsNotEmpty; end,
    Exception,
    'IsNotEmpty should not raise for non-empty string'
  );
end;

procedure TQuickConditionsTests.Test_String_IsNotEmpty_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('', 'str').IsNotEmpty; end,
    EPreConditionError,
    'IsNotEmpty should raise EPreConditionError for empty string'
  );
end;

procedure TQuickConditionsTests.Test_String_IsEmpty_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('', 'str').IsEmpty; end,
    Exception,
    'IsEmpty should not raise for empty string'
  );
end;

procedure TQuickConditionsTests.Test_String_IsEmpty_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('not empty', 'str').IsEmpty; end,
    EPreConditionError,
    'IsEmpty should raise for non-empty string'
  );
end;

procedure TQuickConditionsTests.Test_String_StartsWith_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('HelloWorld', 'str').StartsWith('Hello'); end,
    Exception,
    'StartsWith should not raise when string starts with prefix'
  );
end;

procedure TQuickConditionsTests.Test_String_StartsWith_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('WorldHello', 'str').StartsWith('Hello'); end,
    EPreConditionError,
    'StartsWith should raise when string does not start with prefix'
  );
end;

procedure TQuickConditionsTests.Test_String_EndsWith_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('HelloWorld', 'str').EndsWith('World'); end,
    Exception,
    'EndsWith should not raise when string ends with suffix'
  );
end;

procedure TQuickConditionsTests.Test_String_EndsWith_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('HelloWorld', 'str').EndsWith('Hello'); end,
    EPreConditionError,
    'EndsWith should raise when string does not end with suffix'
  );
end;

procedure TQuickConditionsTests.Test_String_Contains_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('Hello Delphi World', 'str').Contains('Delphi'); end,
    Exception,
    'Contains should not raise when substring is present'
  );
end;

procedure TQuickConditionsTests.Test_String_Contains_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('Hello World', 'str').Contains('Delphi'); end,
    EPreConditionError,
    'Contains should raise when substring is absent'
  );
end;

procedure TQuickConditionsTests.Test_String_DoesNotContains_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('Hello World', 'str').DoesNotContains('Delphi'); end,
    Exception,
    'DoesNotContains should not raise when substring is absent'
  );
end;

procedure TQuickConditionsTests.Test_String_DoesNotContains_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('Hello Delphi', 'str').DoesNotContains('Delphi'); end,
    EPreConditionError,
    'DoesNotContains should raise when substring is present'
  );
end;

procedure TQuickConditionsTests.Test_String_IsLongerThan_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('Hello', 'str').IsLongerThan(3); end,
    Exception,
    'IsLongerThan should not raise when string is longer'
  );
end;

procedure TQuickConditionsTests.Test_String_IsLongerThan_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('Hi', 'str').IsLongerThan(5); end,
    EPreConditionError,
    'IsLongerThan should raise when string is not longer'
  );
end;

procedure TQuickConditionsTests.Test_String_IsShorterThan_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('Hi', 'str').IsShorterThan(10); end,
    Exception,
    'IsShorterThan should not raise when string is shorter'
  );
end;

procedure TQuickConditionsTests.Test_String_IsShorterThan_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('Hello World', 'str').IsShorterThan(5); end,
    EPreConditionError,
    'IsShorterThan should raise when string is not shorter'
  );
end;

procedure TQuickConditionsTests.Test_String_HasLength_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('Hello', 'str').HasLength(5); end,
    Exception,
    'HasLength should not raise when length matches'
  );
end;

procedure TQuickConditionsTests.Test_String_HasLength_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('Hello', 'str').HasLength(3); end,
    EPreConditionError,
    'HasLength should raise when length does not match'
  );
end;

procedure TQuickConditionsTests.Test_String_HasLengthRange_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('Hello', 'str').HasLengthRange(3, 10); end,
    Exception,
    'HasLengthRange should not raise when length is within range'
  );
end;

procedure TQuickConditionsTests.Test_String_HasLengthRange_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('Hi', 'str').HasLengthRange(5, 10); end,
    EPreConditionError,
    'HasLengthRange should raise when length is out of range'
  );
end;

procedure TQuickConditionsTests.Test_String_IsUpperCase_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('HELLO', 'str').IsUpperCase; end,
    Exception,
    'IsUpperCase should not raise for uppercase string'
  );
end;

procedure TQuickConditionsTests.Test_String_IsUpperCase_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('Hello', 'str').IsUpperCase; end,
    EPreConditionError,
    'IsUpperCase should raise for mixed-case string'
  );
end;

procedure TQuickConditionsTests.Test_String_IsLowerCase_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('hello', 'str').IsLowerCase; end,
    Exception,
    'IsLowerCase should not raise for lowercase string'
  );
end;

procedure TQuickConditionsTests.Test_String_IsLowerCase_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('Hello', 'str').IsLowerCase; end,
    EPreConditionError,
    'IsLowerCase should raise for mixed-case string'
  );
end;

procedure TQuickConditionsTests.Test_String_Evaluate_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires('test', 'str').Evaluate(True); end,
    Exception,
    'Evaluate(True) should not raise'
  );
end;

procedure TQuickConditionsTests.Test_String_Evaluate_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires('test', 'str').Evaluate(False); end,
    EPreConditionError,
    'Evaluate(False) should raise'
  );
end;

procedure TQuickConditionsTests.Test_String_WithExceptionOnFailure;
begin
  Assert.WillRaise(
    procedure
    begin
      Condition.Requires('', 'str')
        .WithExceptionOnFailure(ECustomTestException)
        .IsNotEmpty;
    end,
    ECustomTestException,
    'WithExceptionOnFailure should raise the specified exception type'
  );
end;

procedure TQuickConditionsTests.Test_String_CustomMessage;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      Condition.Requires('', 'str').IsNotEmpty('Custom error message');
    end,
    EPreConditionError,
    'Custom error message',
    'Custom error message should appear in exception'
  );
end;

{ Integer conditions }

procedure TQuickConditionsTests.Test_Integer_IsInRange_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(Int64(5), 'num').IsInRange(1, 10); end,
    Exception,
    'IsInRange should not raise when value is in range'
  );
end;

procedure TQuickConditionsTests.Test_Integer_IsInRange_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires(Int64(15), 'num').IsInRange(1, 10); end,
    EPreConditionError,
    'IsInRange should raise when value is out of range'
  );
end;

procedure TQuickConditionsTests.Test_Integer_IsEqualTo_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(Int64(42), 'num').IsEqualTo(42); end,
    Exception,
    'IsEqualTo should not raise when values are equal'
  );
end;

procedure TQuickConditionsTests.Test_Integer_IsEqualTo_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires(Int64(42), 'num').IsEqualTo(100); end,
    EPreConditionError,
    'IsEqualTo should raise when values differ'
  );
end;

procedure TQuickConditionsTests.Test_Integer_IsGreaterThan_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(Int64(10), 'num').IsGreaterThan(5); end,
    Exception,
    'IsGreaterThan should not raise when value is greater'
  );
end;

procedure TQuickConditionsTests.Test_Integer_IsGreaterThan_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires(Int64(3), 'num').IsGreaterThan(5); end,
    EPreConditionError,
    'IsGreaterThan should raise when value is not greater'
  );
end;

procedure TQuickConditionsTests.Test_Integer_IsLessThan_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(Int64(3), 'num').IsLessThan(10); end,
    Exception,
    'IsLessThan should not raise when value is less'
  );
end;

procedure TQuickConditionsTests.Test_Integer_IsLessThan_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires(Int64(15), 'num').IsLessThan(10); end,
    EPreConditionError,
    'IsLessThan should raise when value is not less'
  );
end;

procedure TQuickConditionsTests.Test_Integer_IsNotEqualTo_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(Int64(5), 'num').IsNotEqualTo(10); end,
    Exception,
    'IsNotEqualTo should not raise when values differ'
  );
end;

procedure TQuickConditionsTests.Test_Integer_IsGreaterOrEqual_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(Int64(10), 'num').IsGreaterOrEqual(10); end,
    Exception,
    'IsGreaterOrEqual should not raise when value equals boundary'
  );
end;

procedure TQuickConditionsTests.Test_Integer_IsLessOrEqual_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(Int64(5), 'num').IsLessOrEqual(5); end,
    Exception,
    'IsLessOrEqual should not raise when value equals boundary'
  );
end;

procedure TQuickConditionsTests.Test_Integer_Evaluate_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(Int64(10), 'num').Evaluate(10 > 5); end,
    Exception,
    'Evaluate(True) should not raise for integer condition'
  );
end;

procedure TQuickConditionsTests.Test_Integer_Evaluate_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires(Int64(10), 'num').Evaluate(10 < 5); end,
    EPreConditionError,
    'Evaluate(False) should raise for integer condition'
  );
end;

{ Float conditions }

procedure TQuickConditionsTests.Test_Float_IsInRange_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(Extended(3.5), 'num').IsInRange(1.0, 5.0); end,
    Exception,
    'Float IsInRange should not raise when in range'
  );
end;

procedure TQuickConditionsTests.Test_Float_IsInRange_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires(Extended(10.0), 'num').IsInRange(1.0, 5.0); end,
    EPreConditionError,
    'Float IsInRange should raise when out of range'
  );
end;

procedure TQuickConditionsTests.Test_Float_IsGreaterThan_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(Extended(5.5), 'num').IsGreaterThan(5.0); end,
    Exception,
    'Float IsGreaterThan should not raise when value is greater'
  );
end;

procedure TQuickConditionsTests.Test_Float_IsLessThan_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(Extended(1.5), 'num').IsLessThan(2.0); end,
    Exception,
    'Float IsLessThan should not raise when value is less'
  );
end;

{ Object conditions }

procedure TQuickConditionsTests.Test_Object_IsNotNull_Passes;
var
  obj: TObject;
begin
  obj := TObject.Create;
  try
    Assert.WillNotRaise(
      procedure begin Condition.Requires(obj, 'obj').IsNotNull; end,
      Exception,
      'IsNotNull should not raise for non-nil object'
    );
  finally
    obj.Free;
  end;
end;

procedure TQuickConditionsTests.Test_Object_IsNotNull_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Requires(TObject(nil), 'obj').IsNotNull; end,
    EPreConditionError,
    'IsNotNull should raise for nil object'
  );
end;

procedure TQuickConditionsTests.Test_Object_IsNull_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Requires(TObject(nil), 'obj').IsNull; end,
    Exception,
    'IsNull should not raise for nil object'
  );
end;

procedure TQuickConditionsTests.Test_Object_IsNull_Fails;
var
  obj: TObject;
begin
  obj := TObject.Create;
  try
    Assert.WillRaise(
      procedure begin Condition.Requires(obj, 'obj').IsNull; end,
      EPreConditionError,
      'IsNull should raise for non-nil object'
    );
  finally
    obj.Free;
  end;
end;

procedure TQuickConditionsTests.Test_Object_IsOfType_Passes;
var
  obj: TStringList;
begin
  obj := TStringList.Create;
  try
    Assert.WillNotRaise(
      procedure begin Condition.Requires(TObject(obj), 'obj').IsOfType(TStringList); end,
      Exception,
      'IsOfType should not raise when object is of expected type'
    );
  finally
    obj.Free;
  end;
end;

procedure TQuickConditionsTests.Test_Object_IsOfType_Fails;
var
  obj: TObject;
begin
  obj := TObject.Create;
  try
    Assert.WillRaise(
      procedure begin Condition.Requires(obj, 'obj').IsOfType(TStringList); end,
      EPreConditionError,
      'IsOfType should raise when object is not of expected type'
    );
  finally
    obj.Free;
  end;
end;

procedure TQuickConditionsTests.Test_Object_Evaluate_Passes;
var
  obj: TObject;
begin
  obj := TObject.Create;
  try
    Assert.WillNotRaise(
      procedure begin Condition.Requires(obj, 'obj').Evaluate(obj <> nil); end,
      Exception,
      'Evaluate(True) should not raise for object condition'
    );
  finally
    obj.Free;
  end;
end;

{ Ensures (post-condition) }

procedure TQuickConditionsTests.Test_Ensures_String_IsNotEmpty_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Ensures('result', 'output').IsNotEmpty; end,
    Exception,
    'Ensures IsNotEmpty should not raise for non-empty result'
  );
end;

procedure TQuickConditionsTests.Test_Ensures_String_IsNotEmpty_Fails;
begin
  Assert.WillRaise(
    procedure begin Condition.Ensures('', 'output').IsNotEmpty; end,
    EPostConditionError,
    'Ensures IsNotEmpty should raise EPostConditionError for empty result'
  );
end;

procedure TQuickConditionsTests.Test_Ensures_Integer_IsInRange_Passes;
begin
  Assert.WillNotRaise(
    procedure begin Condition.Ensures(Int64(7), 'result').IsInRange(1, 10); end,
    Exception,
    'Ensures IsInRange should not raise when result is in range'
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickConditionsTests);
end.
