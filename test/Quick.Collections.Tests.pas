unit Quick.Collections.Tests;

{ ***************************************************************************
  Modified : 05/07/2025
 *************************************************************************** }

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  Quick.Arrays,
  Quick.Value,
  Quick.Collections;

type
  { Simple class for object-list and Where/predicate tests }
  TCollPerson = class
  private
    fName: string;
    fAge: Integer;
  public
    constructor Create(const aName: string; aAge: Integer);
  published
    property Name: string read fName write fName;
    property Age: Integer read fAge write fAge;
  end;

  [TestFixture]
  TQuickCollectionsTests = class(TObject)
  public
    [Test] procedure Test_Add_Clear_Count;
    [Test] procedure Test_AddRange;
    [Test] procedure Test_Insert_InsertRange;
    [Test] procedure Test_Remove_Delete_Extract;
    [Test] procedure Test_Exchange_Move;
    [Test] procedure Test_First_Last;
    [Test] procedure Test_Contains_IndexOf_LastIndexOf;
    [Test] procedure Test_Reverse_Sort;
    [Test] procedure Test_BinarySearch;
    [Test] procedure Test_TrimExcess_Capacity;
    [Test] procedure Test_ToArray_FromArray_ToList_FromList;
    [Test] procedure Test_Any_Where;
    [Test] procedure Test_ObjectList_OwnsObjects;
    { Additional coverage }
    [Test] procedure Test_DeleteRange;
    [Test] procedure Test_ObjectList_Where_WhereClause;
    [Test] procedure Test_ObjectList_Any_WhereClause;
    [Test] procedure Test_ObjectList_Where_Predicate;
    [Test] procedure Test_List_String_Type;
    [Test] procedure Test_First_Last_EmptyList_ReturnsDefault;
    [Test] procedure Test_ObjectList_Count_AfterOperations;
    [Test] procedure Test_ObjectList_Where_OrderBy;
    [Test] procedure Test_ObjectList_Where_Select_Property;
  end;

implementation

{ TCollPerson }

constructor TCollPerson.Create(const aName: string; aAge: Integer);
begin
  inherited Create;
  fName := aName;
  fAge := aAge;
end;

procedure TQuickCollectionsTests.Test_Add_Clear_Count;
var l: TxList<Integer>;
begin
  l := TxList<Integer>.Create;
  try
    Assert.AreEqual(0, l.Count);
    l.Add(1);
    l.Add(2);
    Assert.AreEqual(2, l.Count);
    l.Clear;
    Assert.AreEqual(0, l.Count);
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_AddRange;
var l: TxList<Integer>;
    arr: TArray<Integer>;
begin
  l := TxList<Integer>.Create;
  try
    arr := TArray<Integer>.Create(1,2,3);
    l.AddRange(arr);
    Assert.AreEqual(3, l.Count);
    l.AddRange([4,5]);
    Assert.AreEqual(5, l.Count);
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_Insert_InsertRange;
var l: TxList<Integer>;
begin
  l := TxList<Integer>.Create;
  try
    l.Add(1); l.Add(3);
    l.Insert(1,2);
    Assert.AreEqual(2, l[1]);
    l.InsertRange(1, [7,8]);
    Assert.AreEqual(7, l[1]);
    Assert.AreEqual(8, l[2]);
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_Remove_Delete_Extract;
var l: TxList<Integer>;
    v: Integer;
begin
  l := TxList<Integer>.Create;
  try
    l.AddRange([1,2,3,4]);
    l.Remove(2);
    Assert.AreEqual(3, l.Count);
    l.Delete(0);
    Assert.AreEqual(2, l.Count);
    v := l.Extract(3);
    Assert.AreEqual(3, v);
    Assert.AreEqual(1, l.Count);
    l.Add(5);
    v := l.ExtractAt(1);
    Assert.AreEqual(5, v);
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_Exchange_Move;
var l: TxList<Integer>;
begin
  l := TxList<Integer>.Create;
  try
    l.AddRange([1,2,3]);
    l.Exchange(0,2);
    Assert.AreEqual(3, l[0]);
    l.Move(0,2);
    Assert.AreEqual(3, l[2]);
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_First_Last;
var l: TxList<Integer>;
begin
  l := TxList<Integer>.Create;
  try
    l.AddRange([10,20,30]);
    Assert.AreEqual(10, l.First);
    Assert.AreEqual(30, l.Last);
    l.Clear;
    Assert.AreEqual(0, l.First);
    Assert.AreEqual(0, l.Last);
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_Contains_IndexOf_LastIndexOf;
var l: TxList<Integer>;
begin
  l := TxList<Integer>.Create;
  try
    l.AddRange([1,2,3,2]);
    Assert.IsTrue(l.Contains(2));
    Assert.AreEqual(1, l.IndexOf(2));
    Assert.AreEqual(3, l.LastIndexOf(2));
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_Reverse_Sort;
var l: TxList<Integer>;
begin
  l := TxList<Integer>.Create;
  try
    l.AddRange([3,1,2]);
    l.Sort;
    Assert.AreEqual(1, l[0]);
    l.Reverse;
    Assert.AreEqual(3, l[0]);
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_BinarySearch;
var l: TxList<Integer>;
    idx: Integer;
    found: Boolean;
begin
  l := TxList<Integer>.Create;
  try
    l.AddRange([1,2,3,4,5]);
    l.Sort;
    found := l.BinarySearch(3, idx);
    Assert.IsTrue(found);
    Assert.AreEqual(2, idx);
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_TrimExcess_Capacity;
var l: TxList<Integer>;
begin
  l := TxList<Integer>.Create;
  try
    l.Capacity := 100;
    l.Add(1);
    l.TrimExcess;
    Assert.IsTrue(l.Capacity <= 100);
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_ToArray_FromArray_ToList_FromList;
var l: TxList<Integer>;
    arr: TArray<Integer>;
    l2: TxList<Integer>;
    l3: TList<Integer>;
begin
  l := TxList<Integer>.Create;
  try
    l.AddRange([1,2,3]);
    arr := l.ToArray;
    Assert.AreEqual<Integer>(3, Length(arr));
    l2 := TxList<Integer>.Create;
    try
      l2.FromArray(arr);
      Assert.AreEqual(3, l2.Count);
    finally
      l2.Free;
    end;
    l3 := l.ToList;
    l2 := TxList<Integer>.Create;
    try
      l2.FromList(l3);
      Assert.AreEqual(3, l2.Count);
    finally
      l2.Free;
    end;
    try
      Assert.AreEqual<Integer>(3, l3.Count);
    finally
      l3.Free;
    end;
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_Any_Where;
var l: TxList<Integer>;
    any: Boolean;
begin
  l := TxList<Integer>.Create;
  try
    l.AddRange([1,2,3]);
    any := l.Any;
    Assert.IsTrue(any);
    Assert.IsTrue(l.Where('=2', False).Any);
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_ObjectList_OwnsObjects;
var l: TxObjectList<TObject>;
    sl: TObject;
begin
  l := TxObjectList<TObject>.Create(True);
  try
    sl := TObject.Create;
    l.Add(sl);
    Assert.AreEqual(1, l.Count);
    l.Remove(sl);
    Assert.AreEqual(0, l.Count);
  finally
    l.Free;
  end;
end;

{ --- Additional coverage --- }

procedure TQuickCollectionsTests.Test_DeleteRange;
var l: TxList<Integer>;
begin
  l := TxList<Integer>.Create;
  try
    l.AddRange([1, 2, 3, 4, 5]);
    l.DeleteRange(1, 2); // remove items at index 1 and 2 (values 2,3)
    Assert.AreEqual(3, l.Count, 'DeleteRange(1,2) must leave 3 items');
    Assert.AreEqual(1, l[0], 'First item must remain 1');
    Assert.AreEqual(4, l[1], 'Second item must be 4');
    Assert.AreEqual(5, l[2], 'Third item must be 5');
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_ObjectList_Where_WhereClause;
var
  l: TxObjectList<TCollPerson>;
  results: TxArray<TCollPerson>;
begin
  l := TxObjectList<TCollPerson>.Create(True);
  try
    l.Add(TCollPerson.Create('Alice', 30));
    l.Add(TCollPerson.Create('Bob',   25));
    l.Add(TCollPerson.Create('Charlie', 35));
    results := l.Where('Age > ?', [28]).Select;
    Assert.AreEqual(2, results.Count, 'Must find Alice(30) and Charlie(35)');
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_ObjectList_Any_WhereClause;
var
  l: TxObjectList<TCollPerson>;
begin
  l := TxObjectList<TCollPerson>.Create(True);
  try
    l.Add(TCollPerson.Create('Alice', 30));
    l.Add(TCollPerson.Create('Bob',   25));
    Assert.IsTrue(l.Any('Age > ?', [28]), 'Any must return True when Alice(30) matches');
    Assert.IsFalse(l.Any('Name = ?', ['Zara']),
      'Any must return False when no item named Zara exists');
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_ObjectList_Where_Predicate;
var
  l: TxObjectList<TCollPerson>;
  results: TxArray<TCollPerson>;
begin
  l := TxObjectList<TCollPerson>.Create(True);
  try
    l.Add(TCollPerson.Create('Alice', 30));
    l.Add(TCollPerson.Create('Bob',   25));
    l.Add(TCollPerson.Create('Charlie', 35));
    results := l.Where(function(p: TCollPerson): Boolean begin Result := p.Age < 30; end).Select;
    Assert.AreEqual(1, results.Count, 'Predicate must return only Bob (25)');
    Assert.AreEqual('Bob', results[0].Name, 'Result must be Bob');
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_List_String_Type;
var
  l: TxList<string>;
begin
  l := TxList<string>.Create;
  try
    l.Add('Hello');
    l.Add('World');
    Assert.AreEqual(2, l.Count);
    Assert.AreEqual('Hello', l[0]);
    Assert.AreEqual('World', l.Last);
    Assert.IsTrue(l.Contains('World'));
    l.Remove('Hello');
    Assert.AreEqual(1, l.Count);
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_First_Last_EmptyList_ReturnsDefault;
var
  l: TxList<string>;
begin
  l := TxList<string>.Create;
  try
    // First/Last on empty list must return Default(T) = '' for string (not raise)
    Assert.WillNotRaise(
      procedure begin
        Assert.AreEqual('', l.First, 'First on empty list must return empty string');
        Assert.AreEqual('', l.Last, 'Last on empty list must return empty string');
      end,
      nil,
      'First/Last on empty list must not raise');
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_ObjectList_Count_AfterOperations;
var
  l: TxObjectList<TCollPerson>;
begin
  l := TxObjectList<TCollPerson>.Create(True);
  try
    Assert.AreEqual(0, l.Count, 'Empty list must have count 0');
    l.Add(TCollPerson.Create('Alice', 30));
    l.Add(TCollPerson.Create('Bob',   25));
    Assert.AreEqual(2, l.Count, 'After 2 adds count must be 2');
    l.Delete(0);
    Assert.AreEqual(1, l.Count, 'After delete count must be 1');
    l.Clear;
    Assert.AreEqual(0, l.Count, 'After clear count must be 0');
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_ObjectList_Where_OrderBy;
var
  l: TxObjectList<TCollPerson>;
  results: TxArray<TCollPerson>;
begin
  l := TxObjectList<TCollPerson>.Create(True);
  try
    l.Add(TCollPerson.Create('Charlie', 35));
    l.Add(TCollPerson.Create('Alice',   30));
    l.Add(TCollPerson.Create('Bob',     25));
    results := l.Where('Age > ?', [0]).OrderBy('Age').Select;
    Assert.AreEqual(3, results.Count, 'All 3 must be returned');
    Assert.AreEqual(25, results[0].Age, 'First item must have lowest Age=25');
    Assert.AreEqual(35, results[results.Count - 1].Age, 'Last item must have highest Age=35');
  finally
    l.Free;
  end;
end;

procedure TQuickCollectionsTests.Test_ObjectList_Where_Select_Property;
var
  l: TxObjectList<TCollPerson>;
  names: TFlexArray;
  i: Integer;
begin
  l := TxObjectList<TCollPerson>.Create(True);
  try
    l.Add(TCollPerson.Create('Alice', 30));
    l.Add(TCollPerson.Create('Bob',   25));
    l.Add(TCollPerson.Create('Carol', 40));
    names := l.Where('Age >= ?', [30]).Select('Name');
    Assert.AreEqual(2, Integer(names.Count), 'Must select 2 names (Alice+Carol)');
    for i := 0 to names.Count - 1 do
      Assert.IsTrue((names[i].AsString = 'Alice') or (names[i].AsString = 'Carol'),
        'Each selected name must be Alice or Carol');
  finally
    l.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickCollectionsTests);
end.
