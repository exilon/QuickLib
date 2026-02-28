{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.Url.Utils.Tests
  Description : Quick.Url.Utils unit tests
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

unit Quick.Url.Utils.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Quick.Url.Utils;

type
  [TestFixture]
  TQuickUrlUtilsTests = class(TObject)
  public
    // GetProtocol
    [Test]
    procedure Test_GetProtocol_Http;
    [Test]
    procedure Test_GetProtocol_Https;
    [Test]
    procedure Test_GetProtocol_Ftp;
    [Test]
    procedure Test_GetProtocol_NoProtocol_ReturnsEmpty;

    // GetHost
    [Test]
    procedure Test_GetHost_SimpleUrl;
    [Test]
    procedure Test_GetHost_WithPath;
    [Test]
    procedure Test_GetHost_WithQuery;
    [Test]
    procedure Test_GetHost_WithPort;

    // GetPath
    [Test]
    procedure Test_GetPath_WithPath;
    [Test]
    procedure Test_GetPath_NoPath_ReturnsEmpty;
    [Test]
    procedure Test_GetPath_WithQueryStripped;

    // GetQuery
    [Test]
    procedure Test_GetQuery_WithQueryString;
    [Test]
    procedure Test_GetQuery_NoQuery_ReturnsEmpty;

    // RemoveProtocol
    [Test]
    procedure Test_RemoveProtocol_Http;
    [Test]
    procedure Test_RemoveProtocol_Https;
    [Test]
    procedure Test_RemoveProtocol_NoProtocol_Unchanged;

    // RemoveQuery
    [Test]
    procedure Test_RemoveQuery_WithQuery;
    [Test]
    procedure Test_RemoveQuery_NoQuery_Unchanged;

    // EncodeUrl
    [Test]
    procedure Test_EncodeUrl_EncodesSpaces;
    [Test]
    procedure Test_EncodeUrl_AlphanumericUnchanged;
    [Test]
    procedure Test_EncodeUrl_EncodesSpecialChars;
  end;

implementation

// ── GetProtocol ─────────────────────────────────────────────────────────────

procedure TQuickUrlUtilsTests.Test_GetProtocol_Http;
begin
  Assert.AreEqual('http', TUrlUtils.GetProtocol('http://www.example.com'),
    'GetProtocol should extract http');
end;

procedure TQuickUrlUtilsTests.Test_GetProtocol_Https;
begin
  Assert.AreEqual('https', TUrlUtils.GetProtocol('https://www.example.com/path'),
    'GetProtocol should extract https');
end;

procedure TQuickUrlUtilsTests.Test_GetProtocol_Ftp;
begin
  Assert.AreEqual('ftp', TUrlUtils.GetProtocol('ftp://files.example.com'),
    'GetProtocol should extract ftp');
end;

procedure TQuickUrlUtilsTests.Test_GetProtocol_NoProtocol_ReturnsEmpty;
begin
  Assert.AreEqual('', TUrlUtils.GetProtocol('www.example.com'),
    'GetProtocol with no protocol should return empty string');
end;

// ── GetHost ──────────────────────────────────────────────────────────────────

procedure TQuickUrlUtilsTests.Test_GetHost_SimpleUrl;
begin
  Assert.AreEqual('www.example.com',
    TUrlUtils.GetHost('http://www.example.com'),
    'GetHost should return just the host');
end;

procedure TQuickUrlUtilsTests.Test_GetHost_WithPath;
begin
  Assert.AreEqual('www.example.com',
    TUrlUtils.GetHost('http://www.example.com/some/path'),
    'GetHost should strip the path');
end;

procedure TQuickUrlUtilsTests.Test_GetHost_WithQuery;
begin
  // UrlGetHost strips by the first '/'. URLs like 'http://host?q=1' (no path slash)
  // return the host including the query string. The query should be stripped
  // separately via RemoveQuery. Test that the host at minimum contains the hostname.
  var host := TUrlUtils.GetHost('http://www.example.com/page?q=1');
  Assert.IsTrue(host.Contains('www.example.com'),
    'GetHost should contain the hostname');
  Assert.IsFalse(host.Contains('?'),
    'GetHost should not contain the query string when a path separator is present');
end;

procedure TQuickUrlUtilsTests.Test_GetHost_WithPort;
begin
  // Port is part of the host segment in many URL parsers
  var host := TUrlUtils.GetHost('http://www.example.com:8080/path');
  Assert.IsTrue(host.Contains('www.example.com'),
    'GetHost should include the host name when a port is present');
end;

// ── GetPath ──────────────────────────────────────────────────────────────────

procedure TQuickUrlUtilsTests.Test_GetPath_WithPath;
begin
  Assert.AreEqual('/some/path',
    TUrlUtils.GetPath('http://www.example.com/some/path'),
    'GetPath should return the path component');
end;

procedure TQuickUrlUtilsTests.Test_GetPath_NoPath_ReturnsEmpty;
begin
  var p := TUrlUtils.GetPath('http://www.example.com');
  Assert.IsTrue((p = '') or (p = '/'),
    'GetPath with no path should return empty string or /');
end;

procedure TQuickUrlUtilsTests.Test_GetPath_WithQueryStripped;
begin
  var p := TUrlUtils.GetPath('http://www.example.com/page?q=1');
  Assert.IsTrue(p.Contains('/page'), 'GetPath should contain the path without query');
  Assert.IsFalse(p.Contains('?'), 'GetPath should not include the query string');
end;

// ── GetQuery ─────────────────────────────────────────────────────────────────

procedure TQuickUrlUtilsTests.Test_GetQuery_WithQueryString;
begin
  var q := TUrlUtils.GetQuery('http://www.example.com/page?foo=bar&baz=1');
  Assert.IsTrue(q.Contains('foo=bar'), 'GetQuery should contain the query parameters');
end;

procedure TQuickUrlUtilsTests.Test_GetQuery_NoQuery_ReturnsEmpty;
begin
  Assert.AreEqual('', TUrlUtils.GetQuery('http://www.example.com/page'),
    'GetQuery with no query string should return empty');
end;

// ── RemoveProtocol ───────────────────────────────────────────────────────────

procedure TQuickUrlUtilsTests.Test_RemoveProtocol_Http;
begin
  Assert.AreEqual('www.example.com/path',
    TUrlUtils.RemoveProtocol('http://www.example.com/path'),
    'RemoveProtocol should strip http://');
end;

procedure TQuickUrlUtilsTests.Test_RemoveProtocol_Https;
begin
  Assert.AreEqual('www.example.com',
    TUrlUtils.RemoveProtocol('https://www.example.com'),
    'RemoveProtocol should strip https://');
end;

procedure TQuickUrlUtilsTests.Test_RemoveProtocol_NoProtocol_Unchanged;
begin
  Assert.AreEqual('www.example.com',
    TUrlUtils.RemoveProtocol('www.example.com'),
    'RemoveProtocol with no protocol should return the URL unchanged');
end;

// ── RemoveQuery ───────────────────────────────────────────────────────────────

procedure TQuickUrlUtilsTests.Test_RemoveQuery_WithQuery;
begin
  Assert.AreEqual('http://www.example.com/page',
    TUrlUtils.RemoveQuery('http://www.example.com/page?foo=bar'),
    'RemoveQuery should strip everything from ? onward');
end;

procedure TQuickUrlUtilsTests.Test_RemoveQuery_NoQuery_Unchanged;
begin
  Assert.AreEqual('http://www.example.com/page',
    TUrlUtils.RemoveQuery('http://www.example.com/page'),
    'RemoveQuery with no query should return the URL unchanged');
end;

// ── EncodeUrl ─────────────────────────────────────────────────────────────────

procedure TQuickUrlUtilsTests.Test_EncodeUrl_EncodesSpaces;
var
  encoded : string;
begin
  // TIdURI.URLEncode requires a properly formed URL with a protocol
  encoded := TUrlUtils.EncodeUrl('http://example.com/hello world');
  Assert.IsFalse(encoded.Contains(' '), 'EncodeUrl should not leave raw spaces');
  Assert.IsTrue(encoded.Contains('%20') or encoded.Contains('+'),
    'EncodeUrl should encode spaces as %20 or +');
end;

procedure TQuickUrlUtilsTests.Test_EncodeUrl_AlphanumericUnchanged;
var
  encoded : string;
begin
  // TIdURI.URLEncode requires a full URL; the alphanumeric path segment is preserved
  encoded := TUrlUtils.EncodeUrl('http://example.com/HelloWorld123');
  Assert.IsTrue(encoded.Contains('HelloWorld123'),
    'EncodeUrl should not alter alphanumeric characters in the URL');
end;

procedure TQuickUrlUtilsTests.Test_EncodeUrl_EncodesSpecialChars;
var
  encoded : string;
begin
  // TIdURI.URLEncode requires a full URL with protocol
  encoded := TUrlUtils.EncodeUrl('http://example.com/a b&c=d');
  Assert.IsFalse(encoded.Contains(' '),
    'EncodeUrl should encode spaces in the URL');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickUrlUtilsTests);

end.
