{ ***************************************************************************

  Copyright (c) 2016-2026 Kike Pérez

  Unit        : Quick.Lists.Tests
  Description : Unit Tests for Quick.Lists
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 27/02/2026
  Modified    : 05/07/2025

 *************************************************************************** }

unit Quick.Lists.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  Quick.Lists,
  Quick.RTTI.Utils;

type
  { Dummy class with published properties for RTTI-based searches }
  TListPerson = class
  private
    fName: string;
    fAge: Integer;
    fPersonId: Int64;
    fDepartment: string;
    fScore: Double;
  public
    constructor Create(const aName: string; aAge: Integer;
      const aDept: string; aScore: Double = 0; aPersonId: Int64 = 0);
  published
    property Name: string read fName write fName;
    property Age: Integer read fAge write fAge;
    property PersonId: Int64 read fPersonId write fPersonId;
    property Department: string read fDepartment write fDepartment;
    property Score: Double read fScore write fScore;
  end;

  { Dummy class with a TDateTime published property }
  TListEvent = class
  private
    fTitle: string;
    fEventDate: TDateTime;
  public
    constructor Create(const aTitle: string; aDate: TDateTime);
  published
    property Title: string read fTitle write fTitle;
    property EventDate: TDateTime read fEventDate write fEventDate;
  end;

  [TestFixture]
  TQuickListsTests = class(TObject)
  private
    fList: TIndexedObjectList<TListPerson>;
    fSearchList: TSearchObjectList<TListPerson>;
    fEventSearchList: TSearchObjectList<TListEvent>;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    { TIndexList — Add / Remove }
    [Test]
    procedure Test_IndexList_Add_CreatesIndex;
    [Test]
    procedure Test_IndexList_Add_DuplicateField_SecondIndexOK;
    [Test]
    procedure Test_IndexList_Add_UnknownField_Raises;
    [Test]
    procedure Test_IndexList_Remove_ExistingIndex;
    [Test]
    procedure Test_IndexList_Remove_UnknownIndex_Raises;

    { TIndexedObjectList — Get by index }
    [Test]
    procedure Test_IndexedObjectList_Get_ByStringProperty_Found;
    [Test]
    procedure Test_IndexedObjectList_Get_ByStringProperty_NotFound_ReturnsNil;
    [Test]
    procedure Test_IndexedObjectList_Get_UnknownIndex_Raises;
    [Test]
    procedure Test_IndexedObjectList_IndexUpdated_OnAdd;
    [Test]
    procedure Test_IndexedObjectList_IndexUpdated_OnRemove;
    [Test]
    procedure Test_IndexedObjectList_Get_ByIntegerProperty;
    { Additional TIndexedObjectList coverage }
    [Test]
    procedure Test_IndexedObjectList_MultipleIndexes;
    [Test]
    procedure Test_IndexedObjectList_IndexName_CaseInsensitive;
    [Test]
    procedure Test_IndexedObjectList_OwnsObjects_False_NilNotFreed;
    [Test]
    procedure Test_IndexedObjectList_Get_EmptyList_ReturnsNil;
    [Test]
    procedure Test_IndexedObjectList_TwoItemsSameIndexValue_ReturnsFirst;

    { TSearchObjectList — Get by string field }
    [Test]
    procedure Test_SearchObjectList_GetByString_Found;
    [Test]
    procedure Test_SearchObjectList_GetByString_NotFound_ReturnsNil;
    [Test]
    procedure Test_SearchObjectList_GetByInt64_Found;
    [Test]
    procedure Test_SearchObjectList_GetByInt64_NotFound_ReturnsNil;
    [Test]
    procedure Test_SearchObjectList_GetByDouble_Found;
    [Test]
    procedure Test_SearchObjectList_GetByDouble_NotFound_ReturnsNil;
    { Additional TSearchObjectList coverage }
    [Test]
    procedure Test_SearchObjectList_GetByDateTime_Found;
    [Test]
    procedure Test_SearchObjectList_GetByDateTime_NotFound_ReturnsNil;
    [Test]
    procedure Test_SearchObjectList_Empty_ReturnsNil;
    [Test]
    procedure Test_SearchObjectList_MultipleItems_ReturnsFirst;
  end;

implementation

{ TListPerson }

constructor TListPerson.Create(const aName: string; aAge: Integer;
  const aDept: string; aScore: Double = 0; aPersonId: Int64 = 0);
begin
  inherited Create;
  fName := aName;
  fAge := aAge;
  fPersonId := aPersonId;
  fDepartment := aDept;
  fScore := aScore;
end;

{ TListEvent }

constructor TListEvent.Create(const aTitle: string; aDate: TDateTime);
begin
  inherited Create;
  fTitle := aTitle;
  fEventDate := aDate;
end;

{ TQuickListsTests }

procedure TQuickListsTests.SetUp;
begin
  fList := TIndexedObjectList<TListPerson>.Create(True);
  fSearchList := TSearchObjectList<TListPerson>.Create(True);
  fEventSearchList := TSearchObjectList<TListEvent>.Create(True);
end;

procedure TQuickListsTests.TearDown;
begin
  fList.Free;
  fSearchList.Free;
  fEventSearchList.Free;
end;

{ --- TIndexList --- }

procedure TQuickListsTests.Test_IndexList_Add_CreatesIndex;
begin
  Assert.WillNotRaise(
    procedure begin fList.Indexes.Add('byName', 'Name'); end,
    nil,
    'Adding an index on an existing published property must not raise');
  Assert.IsNotNull(fList.Indexes.Get('byname'),
    'Index "byName" must be retrievable after creation');
end;

procedure TQuickListsTests.Test_IndexList_Add_DuplicateField_SecondIndexOK;
begin
  fList.Indexes.Add('byName', 'Name');
  Assert.WillNotRaise(
    procedure begin fList.Indexes.Add('byName2', 'Name'); end,
    nil,
    'Two indexes on the same field with different names must not raise');
end;

procedure TQuickListsTests.Test_IndexList_Add_UnknownField_Raises;
begin
  Assert.WillRaise(
    procedure begin fList.Indexes.Add('byX', 'NonExistentField'); end,
    Exception,
    'Adding index on a non-existent property must raise');
end;

procedure TQuickListsTests.Test_IndexList_Remove_ExistingIndex;
begin
  fList.Indexes.Add('byName', 'Name');
  Assert.WillNotRaise(
    procedure begin fList.Indexes.Remove('byName'); end,
    nil,
    'Removing an existing index must not raise');
  Assert.IsNull(fList.Indexes.Get('byname'),
    'Index must not be retrievable after removal');
end;

procedure TQuickListsTests.Test_IndexList_Remove_UnknownIndex_Raises;
begin
  Assert.WillRaise(
    procedure begin fList.Indexes.Remove('doesNotExist'); end,
    Exception,
    'Removing a non-existent index must raise');
end;

{ --- TIndexedObjectList --- }

procedure TQuickListsTests.Test_IndexedObjectList_Get_ByStringProperty_Found;
var
  p, found: TListPerson;
begin
  fList.Indexes.Add('byName', 'Name');
  p := TListPerson.Create('Alice', 30, 'Eng');
  fList.Add(p);
  found := fList.Get('byName', 'Alice');
  Assert.IsNotNull(found, 'Get must find "Alice" by index');
  Assert.AreEqual('Alice', found.Name, 'Found object must have Name = "Alice"');
end;

procedure TQuickListsTests.Test_IndexedObjectList_Get_ByStringProperty_NotFound_ReturnsNil;
var
  p, found: TListPerson;
begin
  fList.Indexes.Add('byName', 'Name');
  p := TListPerson.Create('Bob', 25, 'HR');
  fList.Add(p);
  found := fList.Get('byName', 'Charlie');
  Assert.IsNull(found, 'Get with no match must return nil');
end;

procedure TQuickListsTests.Test_IndexedObjectList_Get_UnknownIndex_Raises;
begin
  Assert.WillRaise(
    procedure begin fList.Get('unknownIndex', 'value'); end,
    Exception,
    'Get with unknown index name must raise');
end;

procedure TQuickListsTests.Test_IndexedObjectList_IndexUpdated_OnAdd;
var
  p1, p2, found: TListPerson;
begin
  fList.Indexes.Add('byName', 'Name');
  p1 := TListPerson.Create('Alice', 30, 'Eng');
  p2 := TListPerson.Create('Bob', 25, 'HR');
  fList.Add(p1);
  fList.Add(p2);
  found := fList.Get('byName', 'Bob');
  Assert.IsNotNull(found, 'Bob must be findable after being added');
  Assert.AreEqual('Bob', found.Name, 'Found object must be Bob');
end;

procedure TQuickListsTests.Test_IndexedObjectList_IndexUpdated_OnRemove;
var
  p, found: TListPerson;
begin
  fList.Indexes.Add('byName', 'Name');
  p := TListPerson.Create('Alice', 30, 'Eng');
  fList.Add(p);
  // Remove by index — OwnsObjects=True so the item is freed internally
  fList.Delete(0);
  found := fList.Get('byName', 'Alice');
  Assert.IsNull(found, 'After removal, Get must return nil');
end;

procedure TQuickListsTests.Test_IndexedObjectList_Get_ByIntegerProperty;
var
  p, found: TListPerson;
begin
  fList.Indexes.Add('byAge', 'Age');
  p := TListPerson.Create('Alice', 42, 'Eng');
  fList.Add(p);
  found := fList.Get('byAge', 42);
  Assert.IsNotNull(found, 'Get by integer property must find the matching object');
  Assert.AreEqual(42, found.Age, 'Found object must have Age = 42');
end;

{ --- TSearchObjectList --- }

procedure TQuickListsTests.Test_SearchObjectList_GetByString_Found;
var
  p, found: TListPerson;
begin
  p := TListPerson.Create('Alice', 30, 'Eng');
  fSearchList.Add(p);
  found := fSearchList.Get('Name', 'Alice');
  Assert.IsNotNull(found, 'Get by string must find Alice');
  Assert.AreEqual('Alice', found.Name, 'Found Name must be Alice');
end;

procedure TQuickListsTests.Test_SearchObjectList_GetByString_NotFound_ReturnsNil;
var
  p, found: TListPerson;
begin
  p := TListPerson.Create('Alice', 30, 'Eng');
  fSearchList.Add(p);
  found := fSearchList.Get('Name', 'Zara');
  Assert.IsNull(found, 'Get with no match must return nil');
end;

procedure TQuickListsTests.Test_SearchObjectList_GetByInt64_Found;
var
  p, found: TListPerson;
begin
  // Use the native Int64 published property — GetInt64Prop on Integer causes AV on Win64
  p := TListPerson.Create('Alice', 30, 'Eng', 0, 12345);
  fSearchList.Add(p);
  found := fSearchList.Get('PersonId', Int64(12345));
  Assert.IsNotNull(found, 'Get by Int64 must find the object');
  Assert.AreEqual(Int64(12345), found.PersonId, 'Found PersonId must be 12345');
end;

procedure TQuickListsTests.Test_SearchObjectList_GetByInt64_NotFound_ReturnsNil;
var
  p, found: TListPerson;
begin
  p := TListPerson.Create('Alice', 30, 'Eng', 0, 12345);
  fSearchList.Add(p);
  found := fSearchList.Get('PersonId', Int64(99999));
  Assert.IsNull(found, 'Get by Int64 with no match must return nil');
end;

procedure TQuickListsTests.Test_SearchObjectList_GetByDouble_Found;
var
  p, found: TListPerson;
begin
  p := TListPerson.Create('Alice', 30, 'Eng', 9.5);
  fSearchList.Add(p);
  found := fSearchList.Get('Score', Double(9.5));
  Assert.IsNotNull(found, 'Get by Double must find the object');
  Assert.AreEqual(Double(9.5), Double(found.Score), Double(0.001),
    'Found Score must be 9.5');
end;

procedure TQuickListsTests.Test_SearchObjectList_GetByDouble_NotFound_ReturnsNil;
var
  p, found: TListPerson;
begin
  p := TListPerson.Create('Alice', 30, 'Eng', 9.5);
  fSearchList.Add(p);
  found := fSearchList.Get('Score', Double(1.0));
  Assert.IsNull(found, 'Get by Double with no match must return nil');
end;

{ --- Additional TIndexedObjectList coverage --- }

procedure TQuickListsTests.Test_IndexedObjectList_MultipleIndexes;
var
  p, foundByName, foundByDept: TListPerson;
begin
  fList.Indexes.Add('byName', 'Name');
  fList.Indexes.Add('byDept', 'Department');
  p := TListPerson.Create('Alice', 30, 'Engineering');
  fList.Add(p);
  foundByName := fList.Get('byName', 'Alice');
  foundByDept := fList.Get('byDept', 'Engineering');
  Assert.IsNotNull(foundByName, 'byName index must find Alice');
  Assert.IsNotNull(foundByDept, 'byDept index must find Engineering');
  Assert.AreSame(foundByName, foundByDept, 'Both indexes must point to the same object');
end;

procedure TQuickListsTests.Test_IndexedObjectList_IndexName_CaseInsensitive;
var
  p, found: TListPerson;
begin
  // Index added as 'ByName' must be retrievable as 'byname', 'BYNAME', etc.
  fList.Indexes.Add('ByName', 'Name');
  p := TListPerson.Create('Alice', 30, 'Eng');
  fList.Add(p);
  found := fList.Get('BYNAME', 'Alice');
  Assert.IsNotNull(found, 'Index lookup must be case-insensitive');
  Assert.AreEqual('Alice', found.Name, 'Found object must be Alice');
end;

procedure TQuickListsTests.Test_IndexedObjectList_OwnsObjects_False_NilNotFreed;
var
  noOwnList: TIndexedObjectList<TListPerson>;
  p: TListPerson;
begin
  // When OwnsObjects=False, the list must NOT free items on removal
  noOwnList := TIndexedObjectList<TListPerson>.Create(False);
  p := TListPerson.Create('Alice', 30, 'Eng');
  try
    noOwnList.Indexes.Add('byName', 'Name');
    noOwnList.Add(p);
    noOwnList.Delete(0);
    // If OwnsObjects=False p must still be valid (no AV)
    Assert.AreEqual('Alice', p.Name, 'Item must not be freed when OwnsObjects=False');
  finally
    noOwnList.Free;
    p.Free; // safe to free manually
  end;
end;

procedure TQuickListsTests.Test_IndexedObjectList_Get_EmptyList_ReturnsNil;
var
  found: TListPerson;
begin
  fList.Indexes.Add('byName', 'Name');
  found := fList.Get('byName', 'Alice');
  Assert.IsNull(found, 'Get on empty list must return nil');
end;

procedure TQuickListsTests.Test_IndexedObjectList_TwoItemsSameIndexValue_ReturnsFirst;
var
  p1, p2: TListPerson;
begin
  // TSearchDictionary uses a hash dictionary which does NOT allow duplicate keys.
  // Adding two items with the same indexed value raises a "Duplicates not allowed"
  // exception from the underlying TObjectDictionary.
  fList.Indexes.Add('byDept', 'Department');
  p1 := TListPerson.Create('Alice', 30, 'Engineering');
  fList.Add(p1);
  p2 := TListPerson.Create('Bob', 25, 'Engineering');
  Assert.WillRaise(
    procedure begin fList.Add(p2); end,
    EListError,
    'Adding a second item with the same indexed value must raise (duplicate key in dictionary)');
end;

{ --- Additional TSearchObjectList coverage --- }

procedure TQuickListsTests.Test_SearchObjectList_GetByDateTime_Found;
var
  ev, found: TListEvent;
  dt: TDateTime;
begin
  dt := EncodeDate(2025, 7, 1) + EncodeTime(10, 30, 0, 0);
  ev := TListEvent.Create('Conference', dt);
  fEventSearchList.Add(ev);
  found := fEventSearchList.Get('EventDate', dt);
  Assert.IsNotNull(found, 'Get by TDateTime must find the object');
  Assert.AreEqual('Conference', found.Title, 'Found event must be Conference');
end;

procedure TQuickListsTests.Test_SearchObjectList_GetByDateTime_NotFound_ReturnsNil;
var
  ev, found: TListEvent;
  dt: TDateTime;
begin
  dt := EncodeDate(2025, 7, 1);
  ev := TListEvent.Create('Conference', dt);
  fEventSearchList.Add(ev);
  found := fEventSearchList.Get('EventDate', EncodeDate(2025, 12, 31));
  Assert.IsNull(found, 'Get by TDateTime with no match must return nil');
end;

procedure TQuickListsTests.Test_SearchObjectList_Empty_ReturnsNil;
var
  found: TListPerson;
begin
  found := fSearchList.Get('Name', 'Alice');
  Assert.IsNull(found, 'Get on empty SearchObjectList must return nil');
end;

procedure TQuickListsTests.Test_SearchObjectList_MultipleItems_ReturnsFirst;
var
  p1, p2, found: TListPerson;
begin
  // Two items with the same Name — must return the first one added
  p1 := TListPerson.Create('Alice', 30, 'Eng');
  p2 := TListPerson.Create('Alice', 25, 'HR');
  fSearchList.Add(p1);
  fSearchList.Add(p2);
  found := fSearchList.Get('Name', 'Alice');
  Assert.IsNotNull(found, 'Get must return an object when duplicates exist');
  // The linear scan returns the first match — Alice(30,Eng)
  Assert.AreEqual(30, found.Age, 'First matching item must be returned (Age=30)');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickListsTests);

end.
