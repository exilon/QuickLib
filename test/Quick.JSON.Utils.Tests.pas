{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.JSON.Utils.Tests
  Description : Quick.JSON.Utils unit tests
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

unit Quick.JSON.Utils.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Quick.JSON.Utils;

type
  [TestFixture]
  TQuickJsonUtilsTests = class(TObject)
  public
    // IncludeJsonBraces
    [Test]
    procedure Test_IncludeJsonBraces_AddsWhenMissing;
    [Test]
    procedure Test_IncludeJsonBraces_NoOpWhenAlreadyWrapped;
    [Test]
    procedure Test_IncludeJsonBraces_EmptyString;

    // RemoveJsonBraces
    [Test]
    procedure Test_RemoveJsonBraces_RemovesOuterBraces;
    [Test]
    procedure Test_RemoveJsonBraces_NoOpWhenNoBraces;
    [Test]
    procedure Test_RemoveJsonBraces_EmptyBraces;

    // JsonFormat
    [Test]
    procedure Test_JsonFormat_SimpleObject_IsFormatted;
    [Test]
    procedure Test_JsonFormat_ContainsIndentation;
    [Test]
    procedure Test_JsonFormat_PreservesKeyValue;
    [Test]
    procedure Test_JsonFormat_NestedObject_IsFormatted;
    [Test]
    procedure Test_JsonFormat_EmptyObject;
  end;

implementation

// ── IncludeJsonBraces ────────────────────────────────────────────────────────

procedure TQuickJsonUtilsTests.Test_IncludeJsonBraces_AddsWhenMissing;
begin
  var result := TJsonUtils.IncludeJsonBraces('"key":"value"');
  Assert.AreEqual('{"key":"value"}', result,
    'IncludeJsonBraces should wrap with { } when missing');
end;

procedure TQuickJsonUtilsTests.Test_IncludeJsonBraces_NoOpWhenAlreadyWrapped;
begin
  var result := TJsonUtils.IncludeJsonBraces('{"key":"value"}');
  Assert.AreEqual('{"key":"value"}', result,
    'IncludeJsonBraces should not add extra braces when already present');
end;

procedure TQuickJsonUtilsTests.Test_IncludeJsonBraces_EmptyString;
begin
  var result := TJsonUtils.IncludeJsonBraces('');
  Assert.AreEqual('{}', result,
    'IncludeJsonBraces of empty string should return {}');
end;

// ── RemoveJsonBraces ─────────────────────────────────────────────────────────

procedure TQuickJsonUtilsTests.Test_RemoveJsonBraces_RemovesOuterBraces;
begin
  var result := TJsonUtils.RemoveJsonBraces('{"key":"value"}');
  Assert.AreEqual('"key":"value"', result,
    'RemoveJsonBraces should strip the outer { and }');
end;

procedure TQuickJsonUtilsTests.Test_RemoveJsonBraces_NoOpWhenNoBraces;
begin
  var result := TJsonUtils.RemoveJsonBraces('"key":"value"');
  Assert.AreEqual('"key":"value"', result,
    'RemoveJsonBraces should leave unchanged when no braces present');
end;

procedure TQuickJsonUtilsTests.Test_RemoveJsonBraces_EmptyBraces;
begin
  var result := TJsonUtils.RemoveJsonBraces('{}');
  Assert.AreEqual('', result,
    'RemoveJsonBraces of {} should return empty string');
end;

// ── JsonFormat ───────────────────────────────────────────────────────────────

procedure TQuickJsonUtilsTests.Test_JsonFormat_SimpleObject_IsFormatted;
begin
  var formatted := TJsonUtils.JsonFormat('{"name":"Alice","age":30}');
  Assert.IsFalse(formatted = '{"name":"Alice","age":30}',
    'JsonFormat should change the layout of a compact JSON string');
end;

procedure TQuickJsonUtilsTests.Test_JsonFormat_ContainsIndentation;
begin
  var formatted := TJsonUtils.JsonFormat('{"a":"b","c":"d"}');
  // Formatted output should have newlines/spaces
  Assert.IsTrue(formatted.Contains(#10) or formatted.Contains(#13),
    'JsonFormat should add newlines to format the JSON');
end;

procedure TQuickJsonUtilsTests.Test_JsonFormat_PreservesKeyValue;
begin
  var formatted := TJsonUtils.JsonFormat('{"hello":"world"}');
  Assert.IsTrue(formatted.Contains('hello'), 'JsonFormat should preserve key names');
  Assert.IsTrue(formatted.Contains('world'), 'JsonFormat should preserve values');
end;

procedure TQuickJsonUtilsTests.Test_JsonFormat_NestedObject_IsFormatted;
begin
  var json      := '{"outer":{"inner":"value"}}';
  var formatted := TJsonUtils.JsonFormat(json);
  Assert.IsTrue(formatted.Contains('inner'), 'JsonFormat should preserve nested keys');
  Assert.IsTrue(formatted.Contains('value'), 'JsonFormat should preserve nested values');
  Assert.IsTrue(formatted.Length > json.Length,
    'Formatted JSON should be longer than the compact version');
end;

procedure TQuickJsonUtilsTests.Test_JsonFormat_EmptyObject;
begin
  Assert.WillNotRaise(
    procedure begin TJsonUtils.JsonFormat('{}'); end,
    nil,
    'JsonFormat on empty object should not raise');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickJsonUtilsTests);

end.
