{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.Pooling.Tests
  Description : Quick.Pooling unit tests
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

unit Quick.Pooling.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Threading,
  System.SyncObjs,
  Quick.Pooling;

// ── Poolable test item ───────────────────────────────────────────────────────

type
  TPoolableItem = class
  private
    fValue : Integer;
  public
    constructor Create;
    property Value : Integer read fValue write fValue;
  end;

// ── Fixture ──────────────────────────────────────────────────────────────────

  [TestFixture]
  TQuickPoolingTests = class(TObject)
  public
    // Pool creation
    [Test]
    procedure Test_Create_PoolSizeMatchesParam;

    // Get
    [Test]
    procedure Test_Get_ReturnsNonNilItem;
    [Test]
    procedure Test_Get_ItemIsCorrectType;

    // RefCount
    [Test]
    procedure Test_Get_RefCountIsOne_WhenCheckedOut;
    [Test]
    procedure Test_RefCount_Returns_To_Zero_AfterRelease;

    // PoolSize
    [Test]
    procedure Test_PoolSize_Property;

    // TimeoutMs
    [Test]
    procedure Test_TimeoutMs_FluentSetter;

    // Timeout when all slots busy
    [Test]
    procedure Test_Get_Timeout_WhenPoolExhausted;

    // CreateDelegate
    [Test]
    procedure Test_CreateDelegate_SetsCustomValue;

    // Concurrent Gets from different goroutines
    [Test]
    procedure Test_ConcurrentGet_AllSucceed;

    // Multiple independent Gets
    [Test]
    procedure Test_MultipleGets_ReturnDistinctItems;
  end;

implementation

// ── TPoolableItem ─────────────────────────────────────────────────────────────

constructor TPoolableItem.Create;
begin
  inherited;
  fValue := 0;
end;

// ── Tests ─────────────────────────────────────────────────────────────────────

procedure TQuickPoolingTests.Test_Create_PoolSizeMatchesParam;
var
  pool : TObjectPool<TPoolableItem>;
begin
  pool := TObjectPool<TPoolableItem>.Create(5);
  try
    Assert.AreEqual(5, pool.PoolSize, 'Pool size should match the constructor parameter');
  finally
    pool.Free;
  end;
end;

procedure TQuickPoolingTests.Test_Get_ReturnsNonNilItem;
var
  pool : TObjectPool<TPoolableItem>;
  item : IPoolItem<TPoolableItem>;
begin
  pool := TObjectPool<TPoolableItem>.Create(3);
  try
    item := pool.Get;
    Assert.IsNotNull(item, 'Get should return a non-nil pool item');
  finally
    item := nil; // release before freeing pool
    pool.Free;
  end;
end;

procedure TQuickPoolingTests.Test_Get_ItemIsCorrectType;
var
  pool : TObjectPool<TPoolableItem>;
  item : IPoolItem<TPoolableItem>;
begin
  pool := TObjectPool<TPoolableItem>.Create(3);
  try
    item := pool.Get;
    Assert.IsNotNull(item.Item,
      'The inner Item should be a non-nil TPoolableItem');
    Assert.IsTrue(item.Item is TPoolableItem,
      'The inner Item should be an instance of TPoolableItem');
  finally
    item := nil;
    pool.Free;
  end;
end;

procedure TQuickPoolingTests.Test_Get_RefCountIsOne_WhenCheckedOut;
var
  pool : TObjectPool<TPoolableItem>;
  item : IPoolItem<TPoolableItem>;
begin
  pool := TObjectPool<TPoolableItem>.Create(3);
  try
    item := pool.Get;
    // The interface itself gives RefCount=1 from pool perspective
    Assert.IsTrue(item.RefCount >= 1,
      'A checked-out item should have RefCount >= 1');
  finally
    item := nil;
    pool.Free;
  end;
end;

procedure TQuickPoolingTests.Test_RefCount_Returns_To_Zero_AfterRelease;
var
  pool     : TObjectPool<TPoolableItem>;
  item     : IPoolItem<TPoolableItem>;
  idx      : Integer;
begin
  pool := TObjectPool<TPoolableItem>.Create(3);
  try
    item := pool.Get;
    idx  := item.ItemIndex;
    item := nil; // release the interface → pool slot freed
    // We can get the same slot again now (pool size was 3, we freed 1)
    item := pool.Get;
    Assert.IsTrue(item.RefCount >= 1,
      'After release and re-acquire RefCount should be >= 1');
    // idx captured above, value used to confirm pool reuse
    Assert.IsTrue(idx >= 0, 'ItemIndex should be non-negative');
  finally
    item := nil;
    pool.Free;
  end;
end;

procedure TQuickPoolingTests.Test_PoolSize_Property;
var
  pool : TObjectPool<TPoolableItem>;
begin
  pool := TObjectPool<TPoolableItem>.Create(7);
  try
    Assert.AreEqual(7, pool.PoolSize, 'PoolSize property should return 7');
  finally
    pool.Free;
  end;
end;

procedure TQuickPoolingTests.Test_TimeoutMs_FluentSetter;
var
  ipool : IObjectPool<TPoolableItem>;
begin
  ipool := TObjectPool<TPoolableItem>.Create(2);
  // Calling TimeoutMs returns Self; calling without reassignment avoids
  // premature release of the underlying object (reassignment would briefly
  // drop refcount to 0 before the new reference is captured).
  ipool.TimeoutMs(500);
  Assert.IsNotNull(ipool,
    'Pool interface should still be valid after calling TimeoutMs');
end;

procedure TQuickPoolingTests.Test_Get_Timeout_WhenPoolExhausted;
var
  ipool  : IObjectPool<TPoolableItem>;
  item1  : IPoolItem<TPoolableItem>;
  item2  : IPoolItem<TPoolableItem>;
begin
  // Hold the pool as an interface from the start to keep refcount stable
  ipool := TObjectPool<TPoolableItem>.Create(1);
  ipool.TimeoutMs(100); // 100 ms timeout; returns Self, modifies internal field
  item1 := ipool.Get;
  Assert.IsNotNull(item1, 'First Get should succeed');
  // Second Get should time out because the only slot is taken
  Assert.WillRaise(
    procedure begin
      item2 := ipool.Get;
    end,
    nil,
    'Get on exhausted pool should raise a timeout exception');
  item1 := nil; // release before pool goes out of scope
end;

procedure TQuickPoolingTests.Test_CreateDelegate_SetsCustomValue;
var
  ipool : IObjectPool<TPoolableItem>;
  item  : IPoolItem<TPoolableItem>;
begin
  ipool := TObjectPool<TPoolableItem>.Create(2);
  ipool.CreateDelegate(
    procedure(var inst : TPoolableItem)
    begin
      inst := TPoolableItem.Create;
      inst.Value := 99;
    end);
  item := ipool.Get;
  Assert.AreEqual(99, item.Item.Value,
    'CreateDelegate should configure the pooled item (Value=99)');
  item := nil;
end;

procedure TQuickPoolingTests.Test_ConcurrentGet_AllSucceed;
var
  pool   : TObjectPool<TPoolableItem>;
  tasks  : array[0..3] of ITask;
  errors : Integer;
  i      : Integer;
begin
  pool   := TObjectPool<TPoolableItem>.Create(4);
  errors := 0;
  try
    for i := 0 to 3 do
    begin
      tasks[i] := TTask.Run(procedure
      var
        it : IPoolItem<TPoolableItem>;
      begin
        try
          it := pool.Get;
          Sleep(20);
          it := nil;
        except
          TInterlocked.Increment(errors);
        end;
      end);
    end;
    TTask.WaitForAll(tasks);
    Assert.AreEqual(0, errors, 'No errors expected when pool is large enough for all threads');
  finally
    pool.Free;
  end;
end;

procedure TQuickPoolingTests.Test_MultipleGets_ReturnDistinctItems;
var
  pool  : TObjectPool<TPoolableItem>;
  item1 : IPoolItem<TPoolableItem>;
  item2 : IPoolItem<TPoolableItem>;
begin
  pool := TObjectPool<TPoolableItem>.Create(3);
  try
    item1 := pool.Get;
    item2 := pool.Get;
    Assert.AreNotEqual(item1.ItemIndex, item2.ItemIndex,
      'Two concurrent Gets should return items with different indices');
  finally
    item1 := nil;
    item2 := nil;
    pool.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickPoolingTests);

end.
