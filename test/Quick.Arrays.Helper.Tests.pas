{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.Arrays.Helper.Tests
  Description : TArrayHelper<T>, TStringArrayHelper and TIntegerArrayHelper tests
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 27/02/2026
  Modified    : 27/02/2026
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

unit Quick.Arrays.Helper.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Quick.Arrays.Helper;

type
  [TestFixture]
  TArrayHelperTests = class(TObject)
  public
    // ── TArrayHelper<string> ─────────────────────────────────────
    [Test] procedure Test_Generic_Count_Empty;
    [Test] procedure Test_Generic_Count_NonEmpty;
    [Test] procedure Test_Generic_Add_Single;
    [Test] procedure Test_Generic_Add_Multiple;
    [Test] procedure Test_Generic_Insert_AtStart;
    [Test] procedure Test_Generic_Insert_AtMiddle;
    [Test] procedure Test_Generic_Insert_AtEnd;
    [Test] procedure Test_Generic_Remove_First;
    [Test] procedure Test_Generic_Remove_Last;
    [Test] procedure Test_Generic_Remove_Middle;
    [Test] procedure Test_Generic_Clear;
    [Test] procedure Test_Generic_Contains_Found;
    [Test] procedure Test_Generic_Contains_NotFound;
    [Test] procedure Test_Generic_Contains_Empty;
    [Test] procedure Test_Generic_IndexOf_Found;
    [Test] procedure Test_Generic_IndexOf_NotFound;
    [Test] procedure Test_Generic_IndexOf_FirstOccurrence;

    // ── TArrayHelper<Integer> ────────────────────────────────────
    [Test] procedure Test_Generic_Integer_Add;
    [Test] procedure Test_Generic_Integer_Remove;
    [Test] procedure Test_Generic_Integer_Contains;
    [Test] procedure Test_Generic_Integer_IndexOf;
    [Test] procedure Test_Generic_Integer_Clear;

    // ── TStringArrayHelper (record helper) ───────────────────────
    [Test] procedure Test_String_Helper_Count_Empty;
    [Test] procedure Test_String_Helper_Count_NonEmpty;
    [Test] procedure Test_String_Helper_Add;
    [Test] procedure Test_String_Helper_Insert_AtStart;
    [Test] procedure Test_String_Helper_Insert_AtMiddle;
    [Test] procedure Test_String_Helper_Remove_First;
    [Test] procedure Test_String_Helper_Remove_Last;
    [Test] procedure Test_String_Helper_Clear;
    [Test] procedure Test_String_Helper_Contains_Found;
    [Test] procedure Test_String_Helper_Contains_NotFound;
    [Test] procedure Test_String_Helper_IndexOf_Found;
    [Test] procedure Test_String_Helper_IndexOf_NotFound;
    [Test] procedure Test_String_Helper_Sort_Ascending;
    [Test] procedure Test_String_Helper_Sort_AlreadySorted;
    [Test] procedure Test_String_Helper_Sort_SingleElement;
    [Test] procedure Test_String_Helper_Sort_Empty;

    // ── TIntegerArrayHelper (record helper) ──────────────────────
    [Test] procedure Test_Integer_Helper_Count_Empty;
    [Test] procedure Test_Integer_Helper_Count_NonEmpty;
    [Test] procedure Test_Integer_Helper_Add;
    [Test] procedure Test_Integer_Helper_Insert_AtStart;
    [Test] procedure Test_Integer_Helper_Insert_AtMiddle;
    [Test] procedure Test_Integer_Helper_Remove_First;
    [Test] procedure Test_Integer_Helper_Remove_Last;
    [Test] procedure Test_Integer_Helper_Clear;
    [Test] procedure Test_Integer_Helper_Contains_Found;
    [Test] procedure Test_Integer_Helper_Contains_NotFound;
    [Test] procedure Test_Integer_Helper_IndexOf_Found;
    [Test] procedure Test_Integer_Helper_IndexOf_NotFound;
    [Test] procedure Test_Integer_Helper_Sort_Ascending;
    [Test] procedure Test_Integer_Helper_Sort_Descending_Input;
    [Test] procedure Test_Integer_Helper_Sort_WithDuplicates;
    [Test] procedure Test_Integer_Helper_Sort_Empty;

    // ── Edge cases ───────────────────────────────────────────────
    [Test] procedure Test_Add_After_Clear;
    [Test] procedure Test_Remove_SingleElement;
    [Test] procedure Test_Insert_PreservesOtherElements;
    [Test] procedure Test_Sort_Stability_AllEqual;
  end;

implementation

// ══════════════════════════════════════════════════════════════════
//  TArrayHelper<string> tests
// ══════════════════════════════════════════════════════════════════

procedure TArrayHelperTests.Test_Generic_Count_Empty;
var
  arr: TArray<string>;
begin
  arr := [];
  Assert.AreEqual(0, TArrayHelper<string>.Count(arr), 'Count of empty array must be 0');
end;

procedure TArrayHelperTests.Test_Generic_Count_NonEmpty;
var
  arr: TArray<string>;
begin
  arr := ['a', 'b', 'c'];
  Assert.AreEqual(3, TArrayHelper<string>.Count(arr), 'Count must be 3');
end;

procedure TArrayHelperTests.Test_Generic_Add_Single;
var
  arr: TArray<string>;
begin
  arr := [];
  TArrayHelper<string>.Add(arr, 'hello');
  Assert.AreEqual(1, TArrayHelper<string>.Count(arr), 'Count must be 1 after Add');
  Assert.AreEqual('hello', arr[0], 'Element must be "hello"');
end;

procedure TArrayHelperTests.Test_Generic_Add_Multiple;
var
  arr: TArray<string>;
begin
  arr := [];
  TArrayHelper<string>.Add(arr, 'one');
  TArrayHelper<string>.Add(arr, 'two');
  TArrayHelper<string>.Add(arr, 'three');
  Assert.AreEqual(3, TArrayHelper<string>.Count(arr), 'Count must be 3');
  Assert.AreEqual('one',   arr[0], 'arr[0]');
  Assert.AreEqual('two',   arr[1], 'arr[1]');
  Assert.AreEqual('three', arr[2], 'arr[2]');
end;

procedure TArrayHelperTests.Test_Generic_Insert_AtStart;
var
  arr: TArray<string>;
begin
  arr := ['b', 'c'];
  TArrayHelper<string>.Insert(arr, 'a', 0);
  Assert.AreEqual(3,   TArrayHelper<string>.Count(arr), 'Count must be 3');
  Assert.AreEqual('a', arr[0], 'Inserted element must be first');
  Assert.AreEqual('b', arr[1], 'Second element must be "b"');
end;

procedure TArrayHelperTests.Test_Generic_Insert_AtMiddle;
var
  arr: TArray<string>;
begin
  arr := ['a', 'c'];
  TArrayHelper<string>.Insert(arr, 'b', 1);
  Assert.AreEqual(3,   TArrayHelper<string>.Count(arr), 'Count must be 3');
  Assert.AreEqual('a', arr[0], 'arr[0]');
  Assert.AreEqual('b', arr[1], 'Inserted element must be at index 1');
  Assert.AreEqual('c', arr[2], 'arr[2]');
end;

procedure TArrayHelperTests.Test_Generic_Insert_AtEnd;
var
  arr: TArray<string>;
begin
  arr := ['a', 'b'];
  TArrayHelper<string>.Insert(arr, 'c', 2);
  Assert.AreEqual(3,   TArrayHelper<string>.Count(arr), 'Count must be 3');
  Assert.AreEqual('c', arr[2], 'Inserted element must be last');
end;

procedure TArrayHelperTests.Test_Generic_Remove_First;
var
  arr: TArray<string>;
begin
  arr := ['a', 'b', 'c'];
  TArrayHelper<string>.Remove(arr, 0);
  Assert.AreEqual(2,   TArrayHelper<string>.Count(arr), 'Count must be 2');
  Assert.AreEqual('b', arr[0], 'First element after remove must be "b"');
  Assert.AreEqual('c', arr[1], 'Second element after remove must be "c"');
end;

procedure TArrayHelperTests.Test_Generic_Remove_Last;
var
  arr: TArray<string>;
begin
  arr := ['a', 'b', 'c'];
  TArrayHelper<string>.Remove(arr, 2);
  Assert.AreEqual(2,   TArrayHelper<string>.Count(arr), 'Count must be 2');
  Assert.AreEqual('a', arr[0], 'arr[0]');
  Assert.AreEqual('b', arr[1], 'arr[1]');
end;

procedure TArrayHelperTests.Test_Generic_Remove_Middle;
var
  arr: TArray<string>;
begin
  arr := ['a', 'b', 'c'];
  TArrayHelper<string>.Remove(arr, 1);
  Assert.AreEqual(2,   TArrayHelper<string>.Count(arr), 'Count must be 2');
  Assert.AreEqual('a', arr[0], 'arr[0]');
  Assert.AreEqual('c', arr[1], 'arr[1]');
end;

procedure TArrayHelperTests.Test_Generic_Clear;
var
  arr: TArray<string>;
begin
  arr := ['a', 'b', 'c'];
  TArrayHelper<string>.Clear(arr);
  Assert.AreEqual(0, TArrayHelper<string>.Count(arr), 'Count must be 0 after Clear');
end;

procedure TArrayHelperTests.Test_Generic_Contains_Found;
var
  arr: TArray<string>;
begin
  arr := ['alpha', 'beta', 'gamma'];
  Assert.IsTrue(TArrayHelper<string>.Contains(arr, 'beta'), '"beta" must be found');
end;

procedure TArrayHelperTests.Test_Generic_Contains_NotFound;
var
  arr: TArray<string>;
begin
  arr := ['alpha', 'beta', 'gamma'];
  Assert.IsFalse(TArrayHelper<string>.Contains(arr, 'delta'), '"delta" must not be found');
end;

procedure TArrayHelperTests.Test_Generic_Contains_Empty;
var
  arr: TArray<string>;
begin
  arr := [];
  Assert.IsFalse(TArrayHelper<string>.Contains(arr, 'x'), 'Empty array must never contain anything');
end;

procedure TArrayHelperTests.Test_Generic_IndexOf_Found;
var
  arr: TArray<string>;
begin
  arr := ['x', 'y', 'z'];
  Assert.AreEqual(1, TArrayHelper<string>.IndexOf(arr, 'y'), 'IndexOf "y" must be 1');
end;

procedure TArrayHelperTests.Test_Generic_IndexOf_NotFound;
var
  arr: TArray<string>;
begin
  arr := ['x', 'y', 'z'];
  Assert.AreEqual(-1, TArrayHelper<string>.IndexOf(arr, 'w'), 'IndexOf missing item must be -1');
end;

procedure TArrayHelperTests.Test_Generic_IndexOf_FirstOccurrence;
var
  arr: TArray<string>;
begin
  arr := ['a', 'b', 'a'];
  Assert.AreEqual(0, TArrayHelper<string>.IndexOf(arr, 'a'), 'IndexOf must return first occurrence (0)');
end;

// ══════════════════════════════════════════════════════════════════
//  TArrayHelper<Integer> tests
// ══════════════════════════════════════════════════════════════════

procedure TArrayHelperTests.Test_Generic_Integer_Add;
var
  arr: TArray<Integer>;
begin
  arr := [];
  TArrayHelper<Integer>.Add(arr, 10);
  TArrayHelper<Integer>.Add(arr, 20);
  Assert.AreEqual(2,  TArrayHelper<Integer>.Count(arr), 'Count must be 2');
  Assert.AreEqual(10, arr[0], 'arr[0] = 10');
  Assert.AreEqual(20, arr[1], 'arr[1] = 20');
end;

procedure TArrayHelperTests.Test_Generic_Integer_Remove;
var
  arr: TArray<Integer>;
begin
  arr := [1, 2, 3, 4];
  TArrayHelper<Integer>.Remove(arr, 1);  // remove 2
  Assert.AreEqual(3, TArrayHelper<Integer>.Count(arr), 'Count must be 3');
  Assert.AreEqual(1, arr[0], 'arr[0]');
  Assert.AreEqual(3, arr[1], 'arr[1]');
  Assert.AreEqual(4, arr[2], 'arr[2]');
end;

procedure TArrayHelperTests.Test_Generic_Integer_Contains;
var
  arr: TArray<Integer>;
begin
  arr := [5, 10, 15];
  Assert.IsTrue (TArrayHelper<Integer>.Contains(arr, 10), '10 must be found');
  Assert.IsFalse(TArrayHelper<Integer>.Contains(arr, 99), '99 must not be found');
end;

procedure TArrayHelperTests.Test_Generic_Integer_IndexOf;
var
  arr: TArray<Integer>;
begin
  arr := [100, 200, 300];
  Assert.AreEqual(2,  TArrayHelper<Integer>.IndexOf(arr, 300), 'IndexOf 300 must be 2');
  Assert.AreEqual(-1, TArrayHelper<Integer>.IndexOf(arr, 400), 'IndexOf 400 must be -1');
end;

procedure TArrayHelperTests.Test_Generic_Integer_Clear;
var
  arr: TArray<Integer>;
begin
  arr := [1, 2, 3];
  TArrayHelper<Integer>.Clear(arr);
  Assert.AreEqual(0, TArrayHelper<Integer>.Count(arr), 'Count must be 0 after Clear');
end;

// ══════════════════════════════════════════════════════════════════
//  TStringArrayHelper record helper tests
// ══════════════════════════════════════════════════════════════════

procedure TArrayHelperTests.Test_String_Helper_Count_Empty;
var
  arr: TArray<string>;
begin
  arr := [];
  Assert.AreEqual(0, arr.Count, 'Count of empty array must be 0');
end;

procedure TArrayHelperTests.Test_String_Helper_Count_NonEmpty;
var
  arr: TArray<string>;
begin
  arr := ['a', 'b'];
  Assert.AreEqual(2, arr.Count, 'Count must be 2');
end;

procedure TArrayHelperTests.Test_String_Helper_Add;
var
  arr: TArray<string>;
begin
  arr := [];
  arr.Add('first');
  arr.Add('second');
  Assert.AreEqual(2,        arr.Count, 'Count must be 2');
  Assert.AreEqual('first',  arr[0],    'arr[0]');
  Assert.AreEqual('second', arr[1],    'arr[1]');
end;

procedure TArrayHelperTests.Test_String_Helper_Insert_AtStart;
var
  arr: TArray<string>;
begin
  arr := ['b', 'c'];
  arr.Insert('a', 0);
  Assert.AreEqual(3,   arr.Count, 'Count must be 3');
  Assert.AreEqual('a', arr[0],    'First element must be "a"');
  Assert.AreEqual('b', arr[1],    'arr[1]');
end;

procedure TArrayHelperTests.Test_String_Helper_Insert_AtMiddle;
var
  arr: TArray<string>;
begin
  arr := ['a', 'c', 'd'];
  arr.Insert('b', 1);
  Assert.AreEqual(4,   arr.Count, 'Count must be 4');
  Assert.AreEqual('b', arr[1],    'Inserted element at index 1');
  Assert.AreEqual('c', arr[2],    'arr[2] shifted to "c"');
end;

procedure TArrayHelperTests.Test_String_Helper_Remove_First;
var
  arr: TArray<string>;
begin
  arr := ['x', 'y', 'z'];
  arr.Remove(0);
  Assert.AreEqual(2,   arr.Count, 'Count must be 2');
  Assert.AreEqual('y', arr[0],    'First element after Remove must be "y"');
end;

procedure TArrayHelperTests.Test_String_Helper_Remove_Last;
var
  arr: TArray<string>;
begin
  arr := ['x', 'y', 'z'];
  arr.Remove(2);
  Assert.AreEqual(2,   arr.Count, 'Count must be 2');
  Assert.AreEqual('y', arr[1],    'Last remaining element must be "y"');
end;

procedure TArrayHelperTests.Test_String_Helper_Clear;
var
  arr: TArray<string>;
begin
  arr := ['a', 'b', 'c', 'd'];
  arr.Clear;
  Assert.AreEqual(0, arr.Count, 'Count must be 0 after Clear');
end;

procedure TArrayHelperTests.Test_String_Helper_Contains_Found;
var
  arr: TArray<string>;
begin
  arr := ['foo', 'bar', 'baz'];
  Assert.IsTrue(arr.Contains('bar'), '"bar" must be found');
end;

procedure TArrayHelperTests.Test_String_Helper_Contains_NotFound;
var
  arr: TArray<string>;
begin
  arr := ['foo', 'bar', 'baz'];
  Assert.IsFalse(arr.Contains('qux'), '"qux" must not be found');
end;

procedure TArrayHelperTests.Test_String_Helper_IndexOf_Found;
var
  arr: TArray<string>;
begin
  arr := ['alpha', 'beta', 'gamma'];
  Assert.AreEqual(2, arr.IndexOf('gamma'), 'IndexOf "gamma" must be 2');
end;

procedure TArrayHelperTests.Test_String_Helper_IndexOf_NotFound;
var
  arr: TArray<string>;
begin
  arr := ['alpha', 'beta', 'gamma'];
  Assert.AreEqual(-1, arr.IndexOf('delta'), 'IndexOf missing element must be -1');
end;

procedure TArrayHelperTests.Test_String_Helper_Sort_Ascending;
var
  arr: TArray<string>;
begin
  arr := ['cherry', 'apple', 'banana'];
  arr.Sort;
  Assert.AreEqual('apple',  arr[0], 'Sort[0] = apple');
  Assert.AreEqual('banana', arr[1], 'Sort[1] = banana');
  Assert.AreEqual('cherry', arr[2], 'Sort[2] = cherry');
end;

procedure TArrayHelperTests.Test_String_Helper_Sort_AlreadySorted;
var
  arr: TArray<string>;
begin
  arr := ['aaa', 'bbb', 'ccc'];
  arr.Sort;
  Assert.AreEqual('aaa', arr[0], 'Sort[0]');
  Assert.AreEqual('bbb', arr[1], 'Sort[1]');
  Assert.AreEqual('ccc', arr[2], 'Sort[2]');
end;

procedure TArrayHelperTests.Test_String_Helper_Sort_SingleElement;
var
  arr: TArray<string>;
begin
  arr := ['only'];
  Assert.WillNotRaise(procedure begin arr.Sort; end, nil, 'Sort on single element must not raise');
  Assert.AreEqual('only', arr[0], 'Single element unchanged after Sort');
end;

procedure TArrayHelperTests.Test_String_Helper_Sort_Empty;
var
  arr: TArray<string>;
begin
  arr := [];
  Assert.WillNotRaise(procedure begin arr.Sort; end, nil, 'Sort on empty array must not raise');
  Assert.AreEqual(0, arr.Count, 'Count must remain 0');
end;

// ══════════════════════════════════════════════════════════════════
//  TIntegerArrayHelper record helper tests
// ══════════════════════════════════════════════════════════════════

procedure TArrayHelperTests.Test_Integer_Helper_Count_Empty;
var
  arr: TArray<Integer>;
begin
  arr := [];
  Assert.AreEqual(0, arr.Count, 'Count of empty array must be 0');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Count_NonEmpty;
var
  arr: TArray<Integer>;
begin
  arr := [1, 2, 3, 4, 5];
  Assert.AreEqual(5, arr.Count, 'Count must be 5');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Add;
var
  arr: TArray<Integer>;
begin
  arr := [];
  arr.Add(42);
  arr.Add(99);
  Assert.AreEqual(2,  arr.Count, 'Count must be 2');
  Assert.AreEqual(42, arr[0],    'arr[0] = 42');
  Assert.AreEqual(99, arr[1],    'arr[1] = 99');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Insert_AtStart;
var
  arr: TArray<Integer>;
begin
  arr := [2, 3];
  arr.Insert(1, 0);
  Assert.AreEqual(3, arr.Count, 'Count must be 3');
  Assert.AreEqual(1, arr[0],    'First element must be 1');
  Assert.AreEqual(2, arr[1],    'arr[1]');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Insert_AtMiddle;
var
  arr: TArray<Integer>;
begin
  arr := [10, 30];
  arr.Insert(20, 1);
  Assert.AreEqual(3,  arr.Count, 'Count must be 3');
  Assert.AreEqual(10, arr[0],    'arr[0]');
  Assert.AreEqual(20, arr[1],    'Inserted element at index 1');
  Assert.AreEqual(30, arr[2],    'arr[2]');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Remove_First;
var
  arr: TArray<Integer>;
begin
  arr := [1, 2, 3];
  arr.Remove(0);
  Assert.AreEqual(2, arr.Count, 'Count must be 2');
  Assert.AreEqual(2, arr[0],    'First element after Remove must be 2');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Remove_Last;
var
  arr: TArray<Integer>;
begin
  arr := [1, 2, 3];
  arr.Remove(2);
  Assert.AreEqual(2, arr.Count, 'Count must be 2');
  Assert.AreEqual(2, arr[1],    'Last remaining element must be 2');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Clear;
var
  arr: TArray<Integer>;
begin
  arr := [10, 20, 30];
  arr.Clear;
  Assert.AreEqual(0, arr.Count, 'Count must be 0 after Clear');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Contains_Found;
var
  arr: TArray<Integer>;
begin
  arr := [7, 14, 21, 28];
  Assert.IsTrue(arr.Contains(14), '14 must be found');
  Assert.IsTrue(arr.Contains(28), '28 must be found');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Contains_NotFound;
var
  arr: TArray<Integer>;
begin
  arr := [7, 14, 21];
  Assert.IsFalse(arr.Contains(0),   '0 must not be found');
  Assert.IsFalse(arr.Contains(100), '100 must not be found');
end;

procedure TArrayHelperTests.Test_Integer_Helper_IndexOf_Found;
var
  arr: TArray<Integer>;
begin
  arr := [10, 20, 30, 40];
  Assert.AreEqual(0, arr.IndexOf(10), 'IndexOf 10 must be 0');
  Assert.AreEqual(3, arr.IndexOf(40), 'IndexOf 40 must be 3');
end;

procedure TArrayHelperTests.Test_Integer_Helper_IndexOf_NotFound;
var
  arr: TArray<Integer>;
begin
  arr := [10, 20, 30];
  Assert.AreEqual(-1, arr.IndexOf(99), 'IndexOf missing element must be -1');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Sort_Ascending;
var
  arr: TArray<Integer>;
begin
  arr := [5, 3, 8, 1, 4];
  arr.Sort;
  Assert.AreEqual(1, arr[0], 'Sort[0] = 1');
  Assert.AreEqual(3, arr[1], 'Sort[1] = 3');
  Assert.AreEqual(4, arr[2], 'Sort[2] = 4');
  Assert.AreEqual(5, arr[3], 'Sort[3] = 5');
  Assert.AreEqual(8, arr[4], 'Sort[4] = 8');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Sort_Descending_Input;
var
  arr: TArray<Integer>;
begin
  arr := [9, 7, 5, 3, 1];
  arr.Sort;
  Assert.AreEqual(1, arr[0], 'Sort[0] = 1');
  Assert.AreEqual(9, arr[4], 'Sort[4] = 9');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Sort_WithDuplicates;
var
  arr: TArray<Integer>;
begin
  arr := [3, 1, 2, 1, 3];
  arr.Sort;
  Assert.AreEqual(1, arr[0], 'Sort[0] = 1');
  Assert.AreEqual(1, arr[1], 'Sort[1] = 1');
  Assert.AreEqual(2, arr[2], 'Sort[2] = 2');
  Assert.AreEqual(3, arr[3], 'Sort[3] = 3');
  Assert.AreEqual(3, arr[4], 'Sort[4] = 3');
end;

procedure TArrayHelperTests.Test_Integer_Helper_Sort_Empty;
var
  arr: TArray<Integer>;
begin
  arr := [];
  Assert.WillNotRaise(procedure begin arr.Sort; end, nil, 'Sort on empty array must not raise');
  Assert.AreEqual(0, arr.Count, 'Count must remain 0');
end;

// ══════════════════════════════════════════════════════════════════
//  Edge cases
// ══════════════════════════════════════════════════════════════════

procedure TArrayHelperTests.Test_Add_After_Clear;
var
  arr: TArray<string>;
begin
  arr := ['a', 'b', 'c'];
  arr.Clear;
  arr.Add('new');
  Assert.AreEqual(1,     arr.Count, 'Count must be 1 after Add on cleared array');
  Assert.AreEqual('new', arr[0],    'First element must be "new"');
end;

procedure TArrayHelperTests.Test_Remove_SingleElement;
var
  arr: TArray<string>;
begin
  arr := ['only'];
  arr.Remove(0);
  Assert.AreEqual(0, arr.Count, 'Count must be 0 after removing only element');
end;

procedure TArrayHelperTests.Test_Insert_PreservesOtherElements;
var
  arr: TArray<Integer>;
  i  : Integer;
begin
  arr := [1, 2, 3, 4, 5];
  arr.Insert(99, 2);
  // expected: [1, 2, 99, 3, 4, 5]
  Assert.AreEqual(6,  arr.Count, 'Count must be 6');
  Assert.AreEqual(1,  arr[0],    'arr[0]');
  Assert.AreEqual(2,  arr[1],    'arr[1]');
  Assert.AreEqual(99, arr[2],    'Inserted element at index 2');
  Assert.AreEqual(3,  arr[3],    'arr[3] shifted from index 2');
  Assert.AreEqual(4,  arr[4],    'arr[4]');
  Assert.AreEqual(5,  arr[5],    'arr[5]');
end;

procedure TArrayHelperTests.Test_Sort_Stability_AllEqual;
var
  arr: TArray<Integer>;
begin
  arr := [7, 7, 7, 7];
  Assert.WillNotRaise(procedure begin arr.Sort; end, nil, 'Sort of all-equal array must not raise');
  for var i := 0 to arr.Count - 1 do
    Assert.AreEqual(7, arr[i], Format('arr[%d] must be 7', [i]));
end;

initialization
  TDUnitX.RegisterTestFixture(TArrayHelperTests);

end.
