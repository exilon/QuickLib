unit Quick.Arrays.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Quick.Arrays.Helper;

type
  [TestFixture]
  TQuickArraysTests = class(TObject)
  private
    fIntArray: TArray<Integer>;
    fStrArray: TArray<string>;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Test_Add_Integer;
    [Test]
    procedure Test_Remove_Integer;
    [Test]
    procedure Test_Clear_Integer;
    [Test]
    procedure Test_Contains_Integer;
    [Test]
    procedure Test_IndexOf_Integer;
    [Test]
    procedure Test_Add_String;
    [Test]
    procedure Test_Remove_String;
    [Test]
    procedure Test_Clear_String;
    [Test]
    procedure Test_Contains_String;
    [Test]
    procedure Test_IndexOf_String;
    [Test]
    procedure Test_Sort_Integer;
    [Test]
    procedure Test_Sort_String;
  end;

implementation

procedure TQuickArraysTests.SetUp;
begin
  SetLength(fIntArray, 0);
  SetLength(fStrArray, 0);
end;

procedure TQuickArraysTests.TearDown;
begin
  SetLength(fIntArray, 0);
  SetLength(fStrArray, 0);
end;

procedure TQuickArraysTests.Test_Add_Integer;
begin
  fIntArray.Add(10);
  fIntArray.Add(20);
  fIntArray.Add(30);
  Assert.AreEqual(3, fIntArray.Count, 'Array should contain 3 elements');
  Assert.AreEqual(10, fIntArray[0], 'First element should be 10');
  Assert.AreEqual(20, fIntArray[1], 'Second element should be 20');
  Assert.AreEqual(30, fIntArray[2], 'Third element should be 30');
end;

procedure TQuickArraysTests.Test_Remove_Integer;
begin
  fIntArray.Add(10);
  fIntArray.Add(20);
  fIntArray.Add(30);
  fIntArray.Remove(2);
  Assert.AreEqual(2, fIntArray.Count, 'Array should contain 2 elements after removal');
  Assert.AreEqual(10, fIntArray[0], 'First element should be 10');
  Assert.AreEqual(20, fIntArray[1], 'Second element should be 20 after removal of index 2');
end;

procedure TQuickArraysTests.Test_Clear_Integer;
begin
  fIntArray.Add(10);
  fIntArray.Add(20);
  fIntArray.Clear;
  Assert.AreEqual(0, fIntArray.Count, 'Array should be empty after Clear');
end;

procedure TQuickArraysTests.Test_Contains_Integer;
begin
  fIntArray.Add(10);
  fIntArray.Add(20);
  Assert.IsTrue(fIntArray.Contains(10), 'Array should contain value 10');
  Assert.IsTrue(fIntArray.Contains(20), 'Array should contain value 20');
  Assert.IsFalse(fIntArray.Contains(30), 'Array should not contain value 30');
end;

procedure TQuickArraysTests.Test_IndexOf_Integer;
begin
  fIntArray.Add(10);
  fIntArray.Add(20);
  fIntArray.Add(30);
  Assert.AreEqual(0, fIntArray.IndexOf(10), 'IndexOf should return 0 for value 10');
  Assert.AreEqual(1, fIntArray.IndexOf(20), 'IndexOf should return 1 for value 20');
  Assert.AreEqual(-1, fIntArray.IndexOf(40), 'IndexOf should return -1 for non-existent values');
end;

procedure TQuickArraysTests.Test_Add_String;
begin
  fStrArray.Add('one');
  fStrArray.Add('two');
  fStrArray.Add('three');
  Assert.AreEqual(3, fStrArray.Count, 'Array should contain 3 elements');
  Assert.AreEqual('one', fStrArray[0], 'First element should be "one"');
  Assert.AreEqual('two', fStrArray[1], 'Second element should be "two"');
  Assert.AreEqual('three', fStrArray[2], 'Third element should be "three"');
end;

procedure TQuickArraysTests.Test_Remove_String;
begin
  fStrArray.Add('one');
  fStrArray.Add('two');
  fStrArray.Add('three');
  fStrArray.Remove(2);
  Assert.AreEqual(2, fStrArray.Count, 'Array should contain 2 elements after removal');
  Assert.AreEqual('one', fStrArray[0], 'First element should be "one"');
  Assert.AreEqual('two', fStrArray[1], 'Second element should be "two" after removal of index 2');
end;

procedure TQuickArraysTests.Test_Clear_String;
begin
  fStrArray.Add('one');
  fStrArray.Add('two');
  fStrArray.Clear;
  Assert.AreEqual(0, fStrArray.Count, 'Array should be empty after Clear');
end;

procedure TQuickArraysTests.Test_Contains_String;
begin
  fStrArray.Add('one');
  fStrArray.Add('two');
  Assert.IsTrue(fStrArray.Contains('one'), 'Array should contain value "one"');
  Assert.IsTrue(fStrArray.Contains('two'), 'Array should contain value "two"');
  Assert.IsFalse(fStrArray.Contains('three'), 'Array should not contain value "three"');
end;

procedure TQuickArraysTests.Test_IndexOf_String;
begin
  fStrArray.Add('one');
  fStrArray.Add('two');
  fStrArray.Add('three');
  Assert.AreEqual(0, fStrArray.IndexOf('one'), 'IndexOf should return 0 for value "one"');
  Assert.AreEqual(1, fStrArray.IndexOf('two'), 'IndexOf should return 1 for value "two"');
  Assert.AreEqual(-1, fStrArray.IndexOf('four'), 'IndexOf should return -1 for non-existent values');
end;

procedure TQuickArraysTests.Test_Sort_Integer;
begin
  fIntArray.Add(30);
  fIntArray.Add(10);
  fIntArray.Add(20);
  fIntArray.Sort;
  Assert.AreEqual(10, fIntArray[0], 'First element after sort should be 10');
  Assert.AreEqual(20, fIntArray[1], 'Second element after sort should be 20');
  Assert.AreEqual(30, fIntArray[2], 'Third element after sort should be 30');
end;

procedure TQuickArraysTests.Test_Sort_String;
begin
  fStrArray.Add('charlie');
  fStrArray.Add('alpha');
  fStrArray.Add('bravo');
  fStrArray.Sort;
  Assert.AreEqual('alpha', fStrArray[0], 'First element after sort should be "alpha"');
  Assert.AreEqual('bravo', fStrArray[1], 'Second element after sort should be "bravo"');
  Assert.AreEqual('charlie', fStrArray[2], 'Third element after sort should be "charlie"');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickArraysTests);
end.