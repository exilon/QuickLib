{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.MemoryCache.Tests
  Description : Quick.MemoryCache unit tests
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

unit Quick.MemoryCache.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  Quick.MemoryCache;

// ── Test model ──────────────────────────────────────────────────────────────

type
  TCacheItem = class
  private
    fData : string;
  published
    property Data : string read fData write fData;
  end;

// ── Fixture ──────────────────────────────────────────────────────────────────

  [TestFixture]
  TQuickMemoryCacheTests = class(TObject)
  private
    fCache : TMemoryCache;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // String cache
    [Test]
    procedure Test_SetGetString_BasicRoundTrip;
    [Test]
    procedure Test_TryGetValue_Existing_ReturnsTrue;
    [Test]
    procedure Test_TryGetValue_Missing_ReturnsFalse;
    [Test]
    procedure Test_SetValue_Overwrite_UpdatesValue;
    [Test]
    procedure Test_RemoveValue_RemovesEntry;
    [Test]
    procedure Test_RemoveValue_Missing_NoException;

    // CachedObjects count
    [Test]
    procedure Test_CachedObjects_Count_AfterSet;
    [Test]
    procedure Test_CachedObjects_Count_AfterRemove;

    // Flush
    [Test]
    procedure Test_Flush_ClearsAllEntries;
    [Test]
    procedure Test_Flush_ResetsCachedObjectsCount;

    // Object cache
    [Test]
    procedure Test_SetObject_And_TryGetValue;

    // Expiration with explicit date in the past
    [Test]
    procedure Test_SetValue_ExpiredDate_EntryNotFound;

    // StringArray cache
    [Test]
    procedure Test_SetGetStringArray;

    // Refresh
    [Test]
    procedure Test_Refresh_DoesNotRaise;
  end;

// ── Generic fixture ───────────────────────────────────────────────────────────

  [TestFixture]
  TQuickMemoryCacheGenericTests = class(TObject)
  private
    fCache : TMemoryCache<string>;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_Generic_SetAndGet;
    [Test]
    procedure Test_Generic_TryGetValue_Existing_ReturnsTrue;
    [Test]
    procedure Test_Generic_TryGetValue_Missing_ReturnsFalse;
    [Test]
    procedure Test_Generic_RemoveValue;
    [Test]
    procedure Test_Generic_Flush;
    [Test]
    procedure Test_Generic_CachedObjects_Count;
  end;

implementation

// ── TQuickMemoryCacheTests setup ────────────────────────────────────────────

procedure TQuickMemoryCacheTests.SetUp;
begin
  // Use a high purger interval so background jobs don't interfere with tests
  fCache := TMemoryCache.Create(300);
end;

procedure TQuickMemoryCacheTests.TearDown;
begin
  fCache.Free;
end;

// ── String cache tests ───────────────────────────────────────────────────────

procedure TQuickMemoryCacheTests.Test_SetGetString_BasicRoundTrip;
begin
  fCache.SetValue('key1', 'hello');
  Assert.AreEqual('hello', fCache.GetValue('key1'),
    'GetValue should return the stored string');
end;

procedure TQuickMemoryCacheTests.Test_TryGetValue_Existing_ReturnsTrue;
var
  v : string;
begin
  fCache.SetValue('k', 'v');
  Assert.IsTrue(fCache.TryGetValue('k', v), 'TryGetValue should return True for an existing key');
  Assert.AreEqual('v', v, 'TryGetValue should populate the out parameter');
end;

procedure TQuickMemoryCacheTests.Test_TryGetValue_Missing_ReturnsFalse;
var
  v : string;
begin
  Assert.IsFalse(fCache.TryGetValue('no_such_key', v),
    'TryGetValue should return False for a missing key');
end;

procedure TQuickMemoryCacheTests.Test_SetValue_Overwrite_UpdatesValue;
begin
  fCache.SetValue('dup', 'first');
  fCache.SetValue('dup', 'second');
  Assert.AreEqual('second', fCache.GetValue('dup'),
    'Setting the same key twice should store the latest value');
end;

procedure TQuickMemoryCacheTests.Test_RemoveValue_RemovesEntry;
var
  v : string;
begin
  fCache.SetValue('rem', 'gone');
  fCache.RemoveValue('rem');
  Assert.IsFalse(fCache.TryGetValue('rem', v),
    'After RemoveValue the key should no longer be found');
end;

procedure TQuickMemoryCacheTests.Test_RemoveValue_Missing_NoException;
begin
  Assert.WillNotRaise(
    procedure begin fCache.RemoveValue('ghost'); end,
    nil,
    'RemoveValue on a missing key should not raise an exception');
end;

// ── CachedObjects count ──────────────────────────────────────────────────────

procedure TQuickMemoryCacheTests.Test_CachedObjects_Count_AfterSet;
begin
  fCache.Flush;
  fCache.SetValue('a', '1');
  fCache.SetValue('b', '2');
  Assert.AreEqual(2, fCache.CachedObjects,
    'CachedObjects should be 2 after setting 2 entries');
end;

procedure TQuickMemoryCacheTests.Test_CachedObjects_Count_AfterRemove;
begin
  fCache.Flush;
  fCache.SetValue('x', 'val');
  fCache.RemoveValue('x');
  Assert.AreEqual(0, fCache.CachedObjects,
    'CachedObjects should be 0 after removing the only entry');
end;

// ── Flush ────────────────────────────────────────────────────────────────────

procedure TQuickMemoryCacheTests.Test_Flush_ClearsAllEntries;
var
  v : string;
begin
  fCache.SetValue('p', 'q');
  fCache.SetValue('r', 's');
  fCache.Flush;
  Assert.IsFalse(fCache.TryGetValue('p', v), 'After Flush key p should be gone');
  Assert.IsFalse(fCache.TryGetValue('r', v), 'After Flush key r should be gone');
end;

procedure TQuickMemoryCacheTests.Test_Flush_ResetsCachedObjectsCount;
begin
  fCache.SetValue('m', 'n');
  fCache.Flush;
  Assert.AreEqual(0, fCache.CachedObjects,
    'CachedObjects should be 0 after Flush');
end;

// ── Object cache ─────────────────────────────────────────────────────────────

procedure TQuickMemoryCacheTests.Test_SetObject_And_TryGetValue;
var
  src, dest : TCacheItem;
begin
  src := TCacheItem.Create;
  try
    src.Data := 'payload';
    fCache.SetValue('obj', src);
    dest := TCacheItem.Create;
    try
      if fCache.TryGetValue('obj', dest) then
        Assert.AreEqual('payload', dest.Data,
          'TryGetValue for object should populate the out parameter correctly')
      else
        Assert.IsTrue(fCache.TryGetValue('obj', dest),
          'TryGetValue should return True for the stored object');
    finally
      dest.Free;
    end;
  finally
    src.Free;
  end;
end;

// ── Expiration ────────────────────────────────────────────────────────────────

procedure TQuickMemoryCacheTests.Test_SetValue_ExpiredDate_EntryNotFound;
var
  v       : string;
  pastDt  : TDateTime;
begin
  pastDt := IncMilliSecond(Now, -1000); // 1 second in the past
  fCache.SetValue('expired', 'data', pastDt);
  // The entry is expired immediately; TryGetValue should return False
  Assert.IsFalse(fCache.TryGetValue('expired', v),
    'An entry with a past expiration date should not be found');
end;

// ── StringArray cache ─────────────────────────────────────────────────────────

procedure TQuickMemoryCacheTests.Test_SetGetStringArray;
var
  arr    : TArray<string>;
  result : TArray<string>;
begin
  arr := ['alpha', 'beta', 'gamma'];
  fCache.SetValue('sarr', arr);
  Assert.IsTrue(fCache.TryGetValue('sarr', result),
    'TryGetValue should return True for a stored string array');
  Assert.AreEqual(3, Integer(Length(result)), 'Retrieved array should have 3 elements');
  Assert.AreEqual('alpha', result[0], 'First element should be alpha');
end;

// ── Refresh ───────────────────────────────────────────────────────────────────

procedure TQuickMemoryCacheTests.Test_Refresh_DoesNotRaise;
begin
  fCache.SetValue('rfr', 'data', 60000); // 60 seconds TTL
  Assert.WillNotRaise(
    procedure begin fCache.Refresh('rfr', 120000); end,
    nil,
    'Refresh should not raise an exception');
end;

// ── TQuickMemoryCacheGenericTests setup ──────────────────────────────────────

procedure TQuickMemoryCacheGenericTests.SetUp;
begin
  fCache := TMemoryCache<string>.Create(300);
end;

procedure TQuickMemoryCacheGenericTests.TearDown;
begin
  fCache.Free;
end;

// ── Generic cache tests ───────────────────────────────────────────────────────

procedure TQuickMemoryCacheGenericTests.Test_Generic_SetAndGet;
begin
  fCache.SetValue('g1', 'generic_value');
  Assert.AreEqual('generic_value', fCache.GetValue('g1'),
    'Generic GetValue should return the stored value');
end;

procedure TQuickMemoryCacheGenericTests.Test_Generic_TryGetValue_Existing_ReturnsTrue;
var
  v : string;
begin
  fCache.SetValue('gk', 'gv');
  Assert.IsTrue(fCache.TryGetValue('gk', v), 'Generic TryGetValue should return True for existing key');
  Assert.AreEqual('gv', v, 'Generic TryGetValue should populate out value');
end;

procedure TQuickMemoryCacheGenericTests.Test_Generic_TryGetValue_Missing_ReturnsFalse;
var
  v : string;
begin
  Assert.IsFalse(fCache.TryGetValue('missing', v),
    'Generic TryGetValue should return False for missing key');
end;

procedure TQuickMemoryCacheGenericTests.Test_Generic_RemoveValue;
var
  v : string;
begin
  fCache.SetValue('gr', 'grv');
  fCache.RemoveValue('gr');
  Assert.IsFalse(fCache.TryGetValue('gr', v),
    'After RemoveValue the generic key should no longer be found');
end;

procedure TQuickMemoryCacheGenericTests.Test_Generic_Flush;
var
  v : string;
begin
  fCache.SetValue('gf', 'gfv');
  fCache.Flush;
  Assert.IsFalse(fCache.TryGetValue('gf', v),
    'After Flush the generic cache should be empty');
end;

procedure TQuickMemoryCacheGenericTests.Test_Generic_CachedObjects_Count;
begin
  fCache.Flush;
  fCache.SetValue('c1', 'v1');
  fCache.SetValue('c2', 'v2');
  Assert.AreEqual(2, fCache.CachedObjects,
    'Generic CachedObjects should be 2 after setting 2 entries');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickMemoryCacheTests);
  TDUnitX.RegisterTestFixture(TQuickMemoryCacheGenericTests);

end.
