unit Quick.YAML.Tests;

interface

uses
  DUnitX.TestFramework,
  Quick.YAML,
  Quick.Value,
  System.SysUtils,
  System.Classes;

type
  [TestFixture]
  TYAMLTest = class
  private
    fYamlObj: TYamlObject;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Scalar value types
    [Test] procedure TestScalarValues;
    [Test] procedure TestScalarString;
    [Test] procedure TestScalarInteger;
    [Test] procedure TestScalarFloat;
    [Test] procedure TestScalarBoolean;

    // Object structure
    [Test] procedure TestNestedObjects;
    [Test] procedure TestDeepNestedObjects;
    [Test] procedure TestAddPairFluent;
    [Test] procedure TestGetPairByName;
    [Test] procedure TestGetPairByIndex;
    [Test] procedure TestRemovePair;
    [Test] procedure TestCount;
    [Test] procedure TestEnumerator;

    // Arrays
    [Test] procedure TestArrays;
    [Test] procedure TestArrayScalarItems;
    [Test] procedure TestArrayMixedItems;
    [Test] procedure TestInlineArray;

    // Null & comments
    [Test] procedure TestNullValues;
    [Test] procedure TestComments;

    // Parsing
    [Test] procedure TestParseYaml;
    [Test] procedure TestParseYaml_MultipleScalars;
    [Test] procedure TestParseYaml_NestedObject;
    [Test] procedure TestParseYaml_ScalarArray;
    [Test] procedure TestParseYaml_InlineArray;
    [Test] procedure TestParseYaml_IntegerValue;
    [Test] procedure TestParseYaml_FloatValue;
    [Test] procedure TestParseYaml_BooleanValue;

    // Serialization
    [Test] procedure TestToYaml;
    [Test] procedure TestToYaml_NestedObject;
    [Test] procedure TestToYaml_Array;
    [Test] procedure TestToYaml_RoundTrip;

    // Edge cases
    [Test] procedure TestParseYaml_EmptyString;
    [Test] procedure TestParseYaml_Comments;
    [Test] procedure TestGetValue_NonExisting_ReturnsNil;
    [Test] procedure TestParseYamlValue_Object;
    [Test] procedure TestParseYamlValue_Array;
  end;

implementation

procedure TYAMLTest.Setup;
begin
  fYamlObj := TYamlObject.Create;
end;

procedure TYAMLTest.TearDown;
begin
  fYamlObj.Free;
end;

{ ---- Scalar types ---- }

procedure TYAMLTest.TestScalarValues;
begin
  fYamlObj.AddPair('string', 'test');
  fYamlObj.AddPair('integer', 123);
  fYamlObj.AddPair('float', 123.45);
  fYamlObj.AddPair('boolean', TYamlBoolean.Create(True));

  Assert.AreEqual('test', fYamlObj.Values['string'].AsString);
  Assert.AreEqual(123, fYamlObj.Values['integer'].Value.AsInteger);
  Assert.AreEqual(Double(123.45), Double(fYamlObj.Values['float'].Value.AsExtended));
  Assert.IsTrue(fYamlObj.Values['boolean'].Value.AsBoolean);
end;

procedure TYAMLTest.TestScalarString;
var
  v: TYamlString;
begin
  v := TYamlString.Create('hello');
  Assert.AreEqual('hello', v.AsString);
  Assert.IsTrue(v.IsScalar);
  Assert.IsFalse(v.Null);
  v.Free;

  v := TYamlString.Create;
  Assert.IsTrue(v.Null, 'Default TYamlString should be null');
  v.Free;
end;

procedure TYAMLTest.TestScalarInteger;
var
  v: TYamlInteger;
begin
  v := TYamlInteger.Create(42);
  Assert.AreEqual(42, v.Value.AsInteger);
  Assert.IsTrue(v.IsScalar);
  Assert.IsFalse(v.Null);
  Assert.AreEqual('42', v.AsString);
  v.Free;
end;

procedure TYAMLTest.TestScalarFloat;
var
  v: TYamlFloat;
begin
  v := TYamlFloat.Create(3.14);
  Assert.AreEqual(Double(3.14), Double(v.Value.AsExtended));
  Assert.IsTrue(v.IsScalar);
  Assert.IsFalse(v.Null);
  v.Free;
end;

procedure TYAMLTest.TestScalarBoolean;
var
  vTrue, vFalse: TYamlBoolean;
begin
  vTrue := TYamlBoolean.Create(True);
  vFalse := TYamlBoolean.Create(False);
  Assert.IsTrue(vTrue.Value.AsBoolean);
  Assert.IsFalse(vFalse.Value.AsBoolean);
  Assert.IsTrue(vTrue.IsScalar);
  vTrue.Free;
  vFalse.Free;
end;

{ ---- Object structure ---- }

procedure TYAMLTest.TestNestedObjects;
var
  nested: TYamlObject;
begin
  nested := TYamlObject.Create;
  nested.AddPair('child', 'value');
  fYamlObj.AddPair('parent', nested);

  Assert.AreEqual('value', TYamlObject(fYamlObj.Values['parent']).Values['child'].AsString);
end;

procedure TYAMLTest.TestDeepNestedObjects;
var
  level1, level2: TYamlObject;
begin
  level2 := TYamlObject.Create;
  level2.AddPair('deep', 'found');

  level1 := TYamlObject.Create;
  level1.AddPair('level2', level2);

  fYamlObj.AddPair('level1', level1);

  Assert.AreEqual('found',
    TYamlObject(TYamlObject(fYamlObj.Values['level1']).Values['level2']).Values['deep'].AsString,
    'Deep nested value should be accessible');
end;

procedure TYAMLTest.TestAddPairFluent;
begin
  // AddPair returns Self so calls can be chained
  fYamlObj
    .AddPair('a', 'alpha')
    .AddPair('b', 2)
    .AddPair('c', 3.0);

  Assert.AreEqual(3, fYamlObj.Count, 'Fluent AddPair should add 3 pairs');
  Assert.AreEqual('alpha', fYamlObj.Values['a'].AsString);
  Assert.AreEqual(2, fYamlObj.Values['b'].Value.AsInteger);
end;

procedure TYAMLTest.TestGetPairByName;
var
  pair: TYamlPair;
begin
  fYamlObj.AddPair('key', 'value');
  pair := fYamlObj.GetPairByName('key');
  Assert.IsNotNull(pair, 'Should find pair by name');
  Assert.AreEqual('key', pair.Name);
  Assert.AreEqual('value', pair.Value.AsString);

  Assert.IsNull(fYamlObj.GetPairByName('nonexistent'), 'Should return nil for missing pair');
end;

procedure TYAMLTest.TestGetPairByIndex;
var
  pair: TYamlPair;
begin
  fYamlObj.AddPair('first', 'one');
  fYamlObj.AddPair('second', 'two');
  pair := fYamlObj.GetPair(0);
  Assert.AreEqual('first', pair.Name);
  pair := fYamlObj.GetPair(1);
  Assert.AreEqual('second', pair.Name);
end;

procedure TYAMLTest.TestRemovePair;
var
  removed: TYamlPair;
begin
  fYamlObj.AddPair('keep', 'yes');
  fYamlObj.AddPair('remove', 'no');
  Assert.AreEqual(2, fYamlObj.Count);

  removed := fYamlObj.RemovePair('remove');
  Assert.IsNotNull(removed, 'RemovePair should return the removed pair');
  Assert.AreEqual(1, fYamlObj.Count, 'Count should decrease after removal');
  Assert.IsNull(fYamlObj.GetPairByName('remove'), 'Removed pair should no longer be found');
  removed.Free;
end;

procedure TYAMLTest.TestCount;
begin
  Assert.AreEqual(0, fYamlObj.Count, 'Empty object should have count 0');
  fYamlObj.AddPair('a', 'x');
  Assert.AreEqual(1, fYamlObj.Count);
  fYamlObj.AddPair('b', 'y');
  Assert.AreEqual(2, fYamlObj.Count);
end;

procedure TYAMLTest.TestEnumerator;
var
  pair: TYamlPair;
  count: Integer;
begin
  fYamlObj.AddPair('x', '1');
  fYamlObj.AddPair('y', '2');
  fYamlObj.AddPair('z', '3');
  count := 0;
  for pair in fYamlObj do
    Inc(count);
  Assert.AreEqual(3, count, 'Enumerator should iterate all pairs');
end;

{ ---- Arrays ---- }

procedure TYAMLTest.TestArrays;
var
  arr: TYamlArray;
begin
  arr := TYamlArray.Create;
  arr.AddElement(TYamlString.Create('item1'));
  arr.AddElement(TYamlString.Create('item2'));
  arr.AddElement(TYamlInteger.Create(123));
  fYamlObj.AddPair('array', arr);

  Assert.AreEqual(3, TYamlArray(fYamlObj.Values['array']).Count);
  Assert.AreEqual('item1', TYamlArray(fYamlObj.Values['array']).Items[0].AsString);
  Assert.AreEqual(123, TYamlArray(fYamlObj.Values['array']).Items[2].Value.AsInteger);
end;

procedure TYAMLTest.TestArrayScalarItems;
var
  arr: TYamlArray;
  item: TYamlValue;
  count: Integer;
begin
  arr := TYamlArray.Create;
  arr.AddElement(TYamlString.Create('a'));
  arr.AddElement(TYamlString.Create('b'));
  arr.AddElement(TYamlString.Create('c'));
  fYamlObj.AddPair('list', arr);

  count := 0;
  for item in TYamlArray(fYamlObj.Values['list']) do
    Inc(count);
  Assert.AreEqual(3, count, 'Array enumerator should visit all items');
end;

procedure TYAMLTest.TestArrayMixedItems;
var
  arr: TYamlArray;
begin
  arr := TYamlArray.Create;
  arr.AddElement(TYamlString.Create('text'));
  arr.AddElement(TYamlInteger.Create(7));
  arr.AddElement(TYamlFloat.Create(1.5));
  arr.AddElement(TYamlBoolean.Create(True));
  arr.AddElement(TYamlNull.Create);
  fYamlObj.AddPair('mixed', arr);

  var a := TYamlArray(fYamlObj.Values['mixed']);
  Assert.AreEqual(5, a.Count);
  Assert.AreEqual('text', a.Items[0].AsString);
  Assert.AreEqual(7, a.Items[1].Value.AsInteger);
  Assert.IsTrue(a.Items[3].Value.AsBoolean);
  Assert.IsTrue(a.Items[4].Null);
end;

procedure TYAMLTest.TestInlineArray;
var
  arr: TYamlArray;
begin
  arr := TYamlArray.Create(TYamlString.Create('only'));
  Assert.AreEqual(1, arr.Count, 'Single-element constructor should set Count=1');
  Assert.AreEqual('only', arr.Items[0].AsString);
  arr.Free;
end;

{ ---- Null & comments ---- }

procedure TYAMLTest.TestNullValues;
begin
  fYamlObj.AddPair('nullvalue', TYamlNull.Create);
  Assert.IsTrue(fYamlObj.Values['nullvalue'].Null);
  Assert.AreEqual('null', fYamlObj.Values['nullvalue'].AsString);
end;

procedure TYAMLTest.TestComments;
begin
  fYamlObj.AddPair('key', TYamlString.Create('value'));
  fYamlObj.AddPair('#comment', TYamlComment.Create('This is a comment'));

  Assert.IsTrue(fYamlObj.GetPairByName('#comment').Value is TYamlComment);
  Assert.AreEqual('This is a comment', fYamlObj.GetPairByName('#comment').Value.AsString);
end;

{ ---- Parsing ---- }

procedure TYAMLTest.TestParseYaml;
const
  TEST_YAML =
    'name: John'#13#10 +
    'age: 30'#13#10 +
    'address:'#13#10 +
    '  street: Main St'#13#10 +
    '  number: 123'#13#10 +
    'hobbies:'#13#10 +
    '  - reading'#13#10 +
    '  - gaming'#13#10;
begin
  fYamlObj.ParseYaml(TEST_YAML);

  Assert.AreEqual('John', fYamlObj.Values['name'].AsString);
  Assert.AreEqual(30, fYamlObj.Values['age'].Value.AsInteger);
  Assert.AreEqual('Main St', TYamlObject(fYamlObj.Values['address']).Values['street'].AsString);
  Assert.AreEqual(2, TYamlArray(fYamlObj.Values['hobbies']).Count);
  Assert.AreEqual('reading', TYamlArray(fYamlObj.Values['hobbies']).Items[0].AsString);
  Assert.AreEqual('gaming', TYamlArray(fYamlObj.Values['hobbies']).Items[1].AsString);
end;

procedure TYAMLTest.TestParseYaml_MultipleScalars;
const
  YAML =
    'first: alpha'#13#10 +
    'second: beta'#13#10 +
    'third: gamma'#13#10;
begin
  fYamlObj.ParseYaml(YAML);
  Assert.AreEqual(3, fYamlObj.Count, 'Should parse 3 scalar pairs');
  Assert.AreEqual('alpha', fYamlObj.Values['first'].AsString);
  Assert.AreEqual('beta', fYamlObj.Values['second'].AsString);
  Assert.AreEqual('gamma', fYamlObj.Values['third'].AsString);
end;

procedure TYAMLTest.TestParseYaml_NestedObject;
const
  YAML =
    'server:'#13#10 +
    '  host: localhost'#13#10 +
    '  port: 8080'#13#10;
begin
  fYamlObj.ParseYaml(YAML);
  var server := TYamlObject(fYamlObj.Values['server']);
  Assert.IsNotNull(server, 'server should be a nested object');
  Assert.AreEqual('localhost', server.Values['host'].AsString);
  Assert.AreEqual(8080, server.Values['port'].Value.AsInteger);
end;

procedure TYAMLTest.TestParseYaml_ScalarArray;
const
  YAML =
    'colors:'#13#10 +
    '  - red'#13#10 +
    '  - green'#13#10 +
    '  - blue'#13#10;
begin
  fYamlObj.ParseYaml(YAML);
  var arr := TYamlArray(fYamlObj.Values['colors']);
  Assert.IsNotNull(arr, 'colors should be an array');
  Assert.AreEqual(3, arr.Count, 'Should have 3 color items');
  Assert.AreEqual('red',   arr.Items[0].AsString);
  Assert.AreEqual('green', arr.Items[1].AsString);
  Assert.AreEqual('blue',  arr.Items[2].AsString);
end;

procedure TYAMLTest.TestParseYaml_InlineArray;
const
  YAML = 'tags: [one, two, three]'#13#10;
begin
  fYamlObj.ParseYaml(YAML);
  var arr := TYamlArray(fYamlObj.Values['tags']);
  Assert.IsNotNull(arr, 'tags should be parsed as inline array');
  Assert.AreEqual(3, arr.Count, 'Inline array should have 3 items');
  Assert.AreEqual('one',   arr.Items[0].AsString);
  Assert.AreEqual('two',   arr.Items[1].AsString);
  Assert.AreEqual('three', arr.Items[2].AsString);
end;

procedure TYAMLTest.TestParseYaml_IntegerValue;
const
  YAML = 'count: 42'#13#10;
begin
  fYamlObj.ParseYaml(YAML);
  Assert.AreEqual(42, fYamlObj.Values['count'].Value.AsInteger);
end;

procedure TYAMLTest.TestParseYaml_FloatValue;
const
  YAML = 'ratio: 0.75'#13#10;
var
  fs: TFormatSettings;
  expected: string;
begin
  fYamlObj.ParseYaml(YAML);
  // The parser may store the value as a string using the locale decimal separator.
  // Compare as string to be locale-independent.
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  expected := FloatToStr(0.75, fs);
  Assert.AreEqual(expected, fYamlObj.Values['ratio'].AsString, 'Float value should be parsed correctly');
end;

procedure TYAMLTest.TestParseYaml_BooleanValue;
const
  YAML =
    'enabled: true'#13#10 +
    'disabled: false'#13#10;
begin
  fYamlObj.ParseYaml(YAML);
  // booleans parsed as strings by the scalar path - check AsString
  Assert.AreEqual('true',  fYamlObj.Values['enabled'].AsString);
  Assert.AreEqual('false', fYamlObj.Values['disabled'].AsString);
end;

{ ---- Serialization ---- }

procedure TYAMLTest.TestToYaml;
var
  yaml: string;
begin
  fYamlObj.AddPair('name', 'John');
  fYamlObj.AddPair('age', 30);
  yaml := fYamlObj.ToYaml;
  Assert.Contains(yaml, 'name: John', 'ToYaml should contain name pair');
  Assert.Contains(yaml, 'age: 30',    'ToYaml should contain age pair');
end;

procedure TYAMLTest.TestToYaml_NestedObject;
var
  nested: TYamlObject;
  yaml: string;
begin
  nested := TYamlObject.Create;
  nested.AddPair('city', 'Madrid');
  fYamlObj.AddPair('address', nested);
  yaml := fYamlObj.ToYaml;
  Assert.Contains(yaml, 'address:', 'ToYaml should contain nested key');
  Assert.Contains(yaml, 'city: Madrid', 'ToYaml should contain nested value');
end;

procedure TYAMLTest.TestToYaml_Array;
var
  arr: TYamlArray;
  yaml: string;
begin
  arr := TYamlArray.Create;
  arr.AddElement(TYamlString.Create('alpha'));
  arr.AddElement(TYamlString.Create('beta'));
  fYamlObj.AddPair('items', arr);
  yaml := fYamlObj.ToYaml;
  Assert.Contains(yaml, 'items:', 'ToYaml should contain array key');
  Assert.Contains(yaml, 'alpha',  'ToYaml should contain first array item');
  Assert.Contains(yaml, 'beta',   'ToYaml should contain second array item');
end;

procedure TYAMLTest.TestToYaml_RoundTrip;
const
  YAMLTEXT =
    'name: Alice'#13#10 +
    'score: 99'#13#10;
var
  yaml: string;
  parsed: TYamlObject;
begin
  fYamlObj.ParseYaml(YAMLTEXT);
  yaml := fYamlObj.ToYaml;

  parsed := TYamlObject.Create;
  try
    parsed.ParseYaml(yaml);
    Assert.AreEqual('Alice', parsed.Values['name'].AsString, 'Round-trip name should match');
    Assert.AreEqual(99, parsed.Values['score'].Value.AsInteger, 'Round-trip score should match');
  finally
    parsed.Free;
  end;
end;

{ ---- Edge cases ---- }

procedure TYAMLTest.TestParseYaml_EmptyString;
begin
  Assert.WillNotRaise(
    procedure begin fYamlObj.ParseYaml(''); end,
    nil, 'Parsing empty string should not raise');
  Assert.AreEqual(0, fYamlObj.Count, 'Parsing empty string should produce empty object');
end;

procedure TYAMLTest.TestParseYaml_Comments;
const
  YAML =
    '# this is a top comment'#13#10 +
    'key: value'#13#10;
begin
  fYamlObj.ParseYaml(YAML);
  Assert.AreEqual('value', fYamlObj.Values['key'].AsString, 'Key after comment should be parsed');
end;

procedure TYAMLTest.TestGetValue_NonExisting_ReturnsNil;
begin
  Assert.IsNull(fYamlObj.Values['doesnotexist'], 'Values[] for unknown key should return nil');
end;

procedure TYAMLTest.TestParseYamlValue_Object;
const
  YAML = 'key: val'#13#10;
var
  result: TYamlAncestor;
begin
  result := TYamlObject.ParseYamlValue(YAML);
  try
    Assert.IsNotNull(result, 'ParseYamlValue should return a non-nil result');
    Assert.IsTrue(result is TYamlObject, 'ParseYamlValue should return a TYamlObject for key:value input');
  finally
    result.Free;
  end;
end;

procedure TYAMLTest.TestParseYamlValue_Array;
const
  YAML =
    '- first'#13#10 +
    '- second'#13#10;
var
  result: TYamlAncestor;
begin
  result := TYamlObject.ParseYamlValue(YAML);
  try
    Assert.IsNotNull(result, 'ParseYamlValue should return a non-nil result');
    Assert.IsTrue(result is TYamlArray, 'ParseYamlValue should return a TYamlArray for array input');
    Assert.AreEqual(2, TYamlArray(result).Count, 'Array should have 2 items');
  finally
    result.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TYAMLTest);
end.