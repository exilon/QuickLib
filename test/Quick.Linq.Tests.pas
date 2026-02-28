unit Quick.Linq.Tests;

{ ***************************************************************************
  Modified : 05/07/2025
 *************************************************************************** }

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  Quick.Linq,
  Quick.Value,
  Quick.Arrays;

type
  TLinqPerson = class
  private
    fName: string;
    fAge: Integer;
    fDepartment: string;
    fSalary: Double;
  public
    constructor Create(const aName: string; aAge: Integer; const aDept: string; aSalary: Double = 0);
  published
    property Name: string read fName write fName;
    property Age: Integer read fAge write fAge;
    property Department: string read fDepartment write fDepartment;
    property Salary: Double read fSalary write fSalary;
  end;

  [TestFixture]
  TQuickLinqTests = class(TObject)
  private
    fList: TObjectList<TLinqPerson>;
    procedure PopulateList;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Test_Where_SingleCondition;
    [Test]
    procedure Test_Where_MultipleConditions_AND;
    [Test]
    procedure Test_Where_MultipleConditions_OR;
    [Test]
    procedure Test_Where_NoMatch;
    [Test]
    procedure Test_SelectFirst;
    [Test]
    procedure Test_SelectFirst_NoMatch_ReturnsNil;
    [Test]
    procedure Test_SelectLast;
    [Test]
    procedure Test_SelectTop;
    [Test]
    procedure Test_Select_ReturnsAll;
    [Test]
    procedure Test_Count;
    [Test]
    procedure Test_Count_NoMatch;
    [Test]
    procedure Test_OrderBy_Ascending;
    [Test]
    procedure Test_OrderByDescending;
    [Test]
    procedure Test_Update_Fields;
    [Test]
    procedure Test_Delete;
    [Test]
    procedure Test_Where_LikeOperator;
    [Test]
    procedure Test_Where_GreaterThan;
    [Test]
    procedure Test_Where_Numeric_Param;
    [Test]
    procedure Test_From_TArray;
    [Test]
    procedure Test_LinqArray_Any_NoArgs_True;
    [Test]
    procedure Test_LinqArray_Any_NoArgs_False;
    [Test]
    procedure Test_LinqArray_Any_WithMatchString_True;
    [Test]
    procedure Test_LinqArray_Any_WithMatchString_False;
    [Test]
    procedure Test_LinqArray_SelectFirst_Match;
    [Test]
    procedure Test_LinqArray_SelectFirst_NoMatch_ReturnsNil;
    [Test]
    procedure Test_LinqArray_SelectLast_NoMatch_ReturnsNil;
    [Test]
    procedure Test_OrderBy_EmptyResult_NoRaise;
    { Additional coverage }
    [Test]
    procedure Test_Where_Chained_Narrows_Results;
    [Test]
    procedure Test_Where_Predicate;
    [Test]
    procedure Test_Select_Property_Names;
    [Test]
    procedure Test_OrderBy_StringField;
    [Test]
    procedure Test_Update_MultipleFields;
    [Test]
    procedure Test_SelectTop_WithOrderBy;
    [Test]
    procedure Test_From_TxArray;
    [Test]
    procedure Test_Where_NotEqual;
    [Test]
    procedure Test_Where_LessThan;
    [Test]
    procedure Test_Where_Salary_GreaterThan;
  end;

implementation

{ TLinqPerson }

constructor TLinqPerson.Create(const aName: string; aAge: Integer; const aDept: string; aSalary: Double);
begin
  inherited Create;
  fName := aName;
  fAge := aAge;
  fDepartment := aDept;
  fSalary := aSalary;
end;

{ TQuickLinqTests }

procedure TQuickLinqTests.SetUp;
begin
  fList := TObjectList<TLinqPerson>.Create(True);
  PopulateList;
end;

procedure TQuickLinqTests.TearDown;
begin
  fList.Free;
end;

procedure TQuickLinqTests.PopulateList;
begin
  fList.Add(TLinqPerson.Create('Alice',   30, 'Engineering', 80000));
  fList.Add(TLinqPerson.Create('Bob',     25, 'Marketing',   60000));
  fList.Add(TLinqPerson.Create('Charlie', 35, 'Engineering', 90000));
  fList.Add(TLinqPerson.Create('Diana',   28, 'HR',          55000));
  fList.Add(TLinqPerson.Create('Eve',     42, 'Engineering', 95000));
  fList.Add(TLinqPerson.Create('Frank',   22, 'Marketing',   50000));
end;

procedure TQuickLinqTests.Test_Where_SingleCondition;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList).Where('Department = ?', ['Engineering']).Select;
  Assert.AreEqual(3, results.Count, 'Should find 3 Engineering members');
end;

procedure TQuickLinqTests.Test_Where_MultipleConditions_AND;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList).Where('(Department = ?) AND (Age > ?)', ['Engineering', 30]).Select;
  Assert.AreEqual(2, results.Count, 'Should find Charlie and Eve in Engineering with Age > 30');
end;

procedure TQuickLinqTests.Test_Where_MultipleConditions_OR;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList).Where('(Department = ?) OR (Department = ?)', ['HR', 'Marketing']).Select;
  Assert.AreEqual(3, results.Count, 'Should find HR + Marketing members');
end;

procedure TQuickLinqTests.Test_Where_NoMatch;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList).Where('Department = ?', ['Finance']).Select;
  Assert.AreEqual(0, results.Count, 'Should find no Finance members');
end;

procedure TQuickLinqTests.Test_SelectFirst;
var
  person: TLinqPerson;
begin
  person := TLinq<TLinqPerson>.From(fList).Where('Department = ?', ['Engineering']).SelectFirst;
  Assert.IsNotNull(person, 'SelectFirst should return a result');
  Assert.AreEqual('Alice', person.Name, 'First Engineering member should be Alice');
end;

procedure TQuickLinqTests.Test_SelectFirst_NoMatch_ReturnsNil;
var
  person: TLinqPerson;
begin
  person := TLinq<TLinqPerson>.From(fList).Where('Department = ?', ['Finance']).SelectFirst;
  Assert.IsNull(person, 'SelectFirst with no match should return nil');
end;

procedure TQuickLinqTests.Test_SelectLast;
var
  person: TLinqPerson;
begin
  person := TLinq<TLinqPerson>.From(fList).Where('Department = ?', ['Engineering']).SelectLast;
  Assert.IsNotNull(person, 'SelectLast should return a result');
  Assert.AreEqual('Eve', person.Name, 'Last Engineering member should be Eve');
end;

procedure TQuickLinqTests.Test_SelectTop;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList).Where('Age > ?', [20]).SelectTop(3);
  Assert.AreEqual(3, results.Count, 'SelectTop(3) should return 3 items');
end;

procedure TQuickLinqTests.Test_Select_ReturnsAll;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList).Where('Age > ?', [0]).Select;
  Assert.AreEqual(6, results.Count, 'Should return all 6 members');
end;

procedure TQuickLinqTests.Test_Count;
var
  n: Integer;
begin
  n := TLinq<TLinqPerson>.From(fList).Where('Department = ?', ['Marketing']).Count;
  Assert.AreEqual(2, n, 'Should count 2 Marketing members');
end;

procedure TQuickLinqTests.Test_Count_NoMatch;
var
  n: Integer;
begin
  n := TLinq<TLinqPerson>.From(fList).Where('Department = ?', ['Legal']).Count;
  Assert.AreEqual(0, n, 'Should count 0 Legal members');
end;

procedure TQuickLinqTests.Test_OrderBy_Ascending;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList).Where('Age > ?', [0]).OrderBy('Age').Select;
  Assert.AreEqual(6, results.Count, 'Should return all 6 members ordered');
  Assert.AreEqual(22, results[0].Age, 'First item should have the lowest Age (22)');
  Assert.AreEqual(42, results[results.Count - 1].Age, 'Last item should have the highest Age (42)');
end;

procedure TQuickLinqTests.Test_OrderByDescending;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList).Where('Age > ?', [0]).OrderByDescending('Age').Select;
  Assert.AreEqual(42, results[0].Age, 'First item should have the highest Age (42) in descending order');
  Assert.AreEqual(22, results[results.Count - 1].Age, 'Last item should have the lowest Age (22) in descending order');
end;

procedure TQuickLinqTests.Test_Update_Fields;
var
  n: Integer;
  person: TLinqPerson;
begin
  n := TLinq<TLinqPerson>.From(fList).Where('Name = ?', ['Bob']).Update(['Department'], ['Finance']);
  Assert.AreEqual(1, n, 'Should update 1 record');
  person := TLinq<TLinqPerson>.From(fList).Where('Name = ?', ['Bob']).SelectFirst;
  Assert.AreEqual('Finance', person.Department, 'Department should be updated to Finance');
end;

procedure TQuickLinqTests.Test_Delete;
var
  n, countBefore, countAfter: Integer;
begin
  countBefore := fList.Count;
  n := TLinq<TLinqPerson>.From(fList).Where('Department = ?', ['Marketing']).Delete;
  countAfter := fList.Count;
  Assert.AreEqual(2, n, 'Should delete 2 Marketing members');
  Assert.AreEqual(countBefore - 2, countAfter, 'List count should decrease by 2');
end;

procedure TQuickLinqTests.Test_Where_LikeOperator;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList).Where('Name LIKE ?', ['%e%']).Select;
  Assert.IsTrue(results.Count > 0, 'LIKE query should return at least one match');
end;

procedure TQuickLinqTests.Test_Where_GreaterThan;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList).Where('Age >= ?', [35]).Select;
  Assert.AreEqual(2, results.Count, 'Should find Charlie (35) and Eve (42)');
end;

procedure TQuickLinqTests.Test_Where_Numeric_Param;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList).Where('(Age > ?) AND (Age < ?)', [24, 35]).Select;
  Assert.IsTrue(results.Count > 0, 'Should return members with age between 25 and 34');
  // Alice(30), Bob(25), Diana(28) -> 3 results (Charlie=35 excluded by Age < 35)
  Assert.AreEqual(3, results.Count, 'Should find Alice, Bob, Diana in age range 25-34');
end;

procedure TQuickLinqTests.Test_From_TArray;
var
  arr: TArray<TLinqPerson>;
  p1, p2: TLinqPerson;
  results: TxArray<TLinqPerson>;
begin
  p1 := TLinqPerson.Create('Tom', 20, 'IT');
  p2 := TLinqPerson.Create('Jerry', 30, 'IT');
  arr := TArray<TLinqPerson>.Create(p1, p2);
  try
    results := TLinq<TLinqPerson>.From(arr).Where('Department = ?', ['IT']).Select;
    Assert.AreEqual(2, results.Count, 'From TArray should work correctly');
  finally
    p1.Free;
    p2.Free;
  end;
end;

procedure TQuickLinqTests.Test_LinqArray_Any_NoArgs_True;
var
  arr: TArray<TLinqPerson>;
  p1: TLinqPerson;
  linq: ILinqArray<TLinqPerson>;
begin
  p1 := TLinqPerson.Create('Tom', 20, 'IT');
  arr := TArray<TLinqPerson>.Create(p1);
  try
    linq := TLinqArray<TLinqPerson>.Create(arr);
    Assert.IsTrue(linq.Any, 'Any on non-empty TArray must return True');
  finally
    p1.Free;
  end;
end;

procedure TQuickLinqTests.Test_LinqArray_Any_NoArgs_False;
var
  arr: TArray<TLinqPerson>;
  linq: ILinqArray<TLinqPerson>;
begin
  arr := [];
  linq := TLinqArray<TLinqPerson>.Create(arr);
  Assert.IsFalse(linq.Any, 'Any on empty TArray must return False');
end;

procedure TQuickLinqTests.Test_LinqArray_Any_WithMatchString_True;
var
  arr: TArray<TLinqPerson>;
  p1: TLinqPerson;
  linq: ILinqArray<TLinqPerson>;
begin
  p1 := TLinqPerson.Create('Alice', 30, 'IT');
  arr := TArray<TLinqPerson>.Create(p1);
  try
    linq := TLinqArray<TLinqPerson>.Create(arr);
    // Any(matchString) must return True when the array is non-empty
    Assert.IsTrue(linq.Any('Department = ?', False),
      'Any(matchString) on non-empty TArray must return True');
  finally
    p1.Free;
  end;
end;

procedure TQuickLinqTests.Test_LinqArray_Any_WithMatchString_False;
var
  arr: TArray<TLinqPerson>;
  linq: ILinqArray<TLinqPerson>;
begin
  arr := [];
  linq := TLinqArray<TLinqPerson>.Create(arr);
  Assert.IsFalse(linq.Any('Department = ?', False),
    'Any(matchString) on empty TArray must return False');
end;

procedure TQuickLinqTests.Test_LinqArray_SelectFirst_Match;
var
  arr: TArray<TLinqPerson>;
  p1, p2: TLinqPerson;
  person: TLinqPerson;
begin
  p1 := TLinqPerson.Create('Alpha', 20, 'Dev');
  p2 := TLinqPerson.Create('Beta',  25, 'Dev');
  arr := TArray<TLinqPerson>.Create(p1, p2);
  try
    person := TLinq<TLinqPerson>.From(arr).Where('Name = ?', ['Alpha']).SelectFirst;
    Assert.IsNotNull(person, 'SelectFirst must return the matching element');
    Assert.AreEqual('Alpha', person.Name, 'SelectFirst must return Alpha');
  finally
    p1.Free;
    p2.Free;
  end;
end;

procedure TQuickLinqTests.Test_LinqArray_SelectFirst_NoMatch_ReturnsNil;
var
  arr: TArray<TLinqPerson>;
  p1: TLinqPerson;
  person: TLinqPerson;
begin
  p1 := TLinqPerson.Create('Alpha', 20, 'Dev');
  arr := TArray<TLinqPerson>.Create(p1);
  try
    // No element matches "Finance" → must return nil, never undefined garbage
    person := TLinq<TLinqPerson>.From(arr).Where('Department = ?', ['Finance']).SelectFirst;
    Assert.IsNull(person,
      'SelectFirst with no match must return nil (Default(T)), not undefined garbage');
  finally
    p1.Free;
  end;
end;

procedure TQuickLinqTests.Test_LinqArray_SelectLast_NoMatch_ReturnsNil;
var
  arr: TArray<TLinqPerson>;
  p1: TLinqPerson;
  person: TLinqPerson;
begin
  p1 := TLinqPerson.Create('Alpha', 20, 'Dev');
  arr := TArray<TLinqPerson>.Create(p1);
  try
    person := TLinq<TLinqPerson>.From(arr).Where('Department = ?', ['Finance']).SelectLast;
    Assert.IsNull(person,
      'SelectLast with no match must return nil (Default(T)), not undefined garbage');
  finally
    p1.Free;
  end;
end;

procedure TQuickLinqTests.Test_OrderBy_EmptyResult_NoRaise;
var
  results: TxArray<TLinqPerson>;
begin
  // OrderBy on a Where that returns 0 items must not crash inside the comparator
  Assert.WillNotRaise(
    procedure
    begin
      results := TLinq<TLinqPerson>.From(fList)
        .Where('Department = ?', ['Finance'])
        .OrderBy('Name')
        .Select;
    end,
    nil,
    'OrderBy on empty result set must not raise');
  Assert.AreEqual(0, Integer(results.Count), 'Empty result after OrderBy must have 0 items');
end;

{ --- Additional coverage --- }

procedure TQuickLinqTests.Test_Where_Chained_Narrows_Results;
var
  results: TxArray<TLinqPerson>;
begin
  // First Where: Dept=Engineering (3 people), then chained Where: Age >= 35 (2 people)
  results := TLinq<TLinqPerson>.From(fList)
    .Where('Department = ?', ['Engineering'])
    .Where('Age >= ?', [35])
    .Select;
  Assert.AreEqual(2, results.Count, 'Chained Where must narrow to 2 results (Charlie+Eve)');
end;

procedure TQuickLinqTests.Test_Where_Predicate;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList)
    .Where(function(p: TLinqPerson): Boolean
    begin
      Result := p.Age < 27;
    end)
    .Select;
  // Bob(25) and Frank(22) are below 27
  Assert.AreEqual(2, results.Count, 'Predicate Where must return 2 items (Bob+Frank)');
end;

procedure TQuickLinqTests.Test_Select_Property_Names;
var
  names: TFlexArray;
  i: Integer;
begin
  names := TLinq<TLinqPerson>.From(fList)
    .Where('Department = ?', ['Marketing'])
    .Select('Name');
  // Bob and Frank are in Marketing
  Assert.AreEqual(2, Integer(names.Count), 'Select(Name) must return 2 name values');
  for i := 0 to names.Count - 1 do
    Assert.IsTrue((names[i].AsString = 'Bob') or (names[i].AsString = 'Frank'),
      'Each selected name must be Bob or Frank');
end;

procedure TQuickLinqTests.Test_OrderBy_StringField;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList)
    .Where('Age > ?', [0])
    .OrderBy('Name')
    .Select;
  Assert.AreEqual(6, results.Count, 'All 6 items must be returned');
  // Alphabetical order: Alice, Bob, Charlie, Diana, Eve, Frank
  Assert.AreEqual('Alice', results[0].Name, 'First alphabetically must be Alice');
  Assert.AreEqual('Frank', results[results.Count - 1].Name, 'Last alphabetically must be Frank');
end;

procedure TQuickLinqTests.Test_Update_MultipleFields;
var
  n: Integer;
  person: TLinqPerson;
begin
  n := TLinq<TLinqPerson>.From(fList)
    .Where('Name = ?', ['Alice'])
    .Update(['Department', 'Age'], ['Management', 40]);
  Assert.AreEqual(1, n, 'Must update exactly 1 record');
  person := TLinq<TLinqPerson>.From(fList).Where('Name = ?', ['Alice']).SelectFirst;
  Assert.AreEqual('Management', person.Department, 'Department must be updated');
  Assert.AreEqual(40, person.Age, 'Age must be updated');
end;

procedure TQuickLinqTests.Test_SelectTop_WithOrderBy;
var
  results: TxArray<TLinqPerson>;
begin
  // Top 2 youngest (order ascending by Age)
  results := TLinq<TLinqPerson>.From(fList)
    .Where('Age > ?', [0])
    .OrderBy('Age')
    .SelectTop(2);
  Assert.AreEqual(2, results.Count, 'SelectTop(2) must return 2 items');
  Assert.AreEqual(22, results[0].Age, 'First item must be youngest (Age=22)');
  Assert.AreEqual(25, results[1].Age, 'Second item must have Age=25');
end;

procedure TQuickLinqTests.Test_From_TxArray;
var
  arr: TArray<TLinqPerson>;
  p1, p2: TLinqPerson;
  results: TxArray<TLinqPerson>;
begin
  // TLinq.From also accepts TArray<T>; verify it works correctly
  p1 := TLinqPerson.Create('Xena', 28, 'IT');
  p2 := TLinqPerson.Create('Yago', 33, 'IT');
  arr := TArray<TLinqPerson>.Create(p1, p2);
  results := TLinq<TLinqPerson>.From(arr).Where('Age > ?', [30]).Select;
  Assert.AreEqual(1, results.Count, 'From TArray: only Yago (33) matches Age>30');
  Assert.AreEqual('Yago', results[0].Name, 'Result must be Yago');
  p1.Free;
  p2.Free;
end;

procedure TQuickLinqTests.Test_Where_NotEqual;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList)
    .Where('Department <> ?', ['Engineering'])
    .Select;
  // Bob(Marketing), Diana(HR), Frank(Marketing) → 3 non-Engineering members
  Assert.AreEqual(3, results.Count, 'Not-equal filter must return 3 non-Engineering members');
end;

procedure TQuickLinqTests.Test_Where_LessThan;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList)
    .Where('Age < ?', [25])
    .Select;
  // Frank(22) is the only one below 25
  Assert.AreEqual(1, results.Count, 'Only Frank (22) must match Age < 25');
  Assert.AreEqual('Frank', results[0].Name, 'Result must be Frank');
end;

procedure TQuickLinqTests.Test_Where_Salary_GreaterThan;
var
  results: TxArray<TLinqPerson>;
begin
  results := TLinq<TLinqPerson>.From(fList)
    .Where('Salary > ?', [80000])
    .Select;
  // Charlie(90000), Eve(95000) → 2 above 80000
  Assert.AreEqual(2, results.Count, 'Must find 2 people with Salary > 80000');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickLinqTests);
end.
