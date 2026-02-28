{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.Data.Redis.Tests
  Description : TRedisCommand unit tests (no server required)
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 28/02/2026
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

unit Quick.Data.Redis.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Quick.Data.Redis;

// ──────────────────────────────────────────────────────────────────────────
//  These tests exercise TRedisCommand directly: pure RESP encoding, no
//  network required.
// ──────────────────────────────────────────────────────────────────────────

type
  [TestFixture]
  TRedisCommandTests = class
  private
    // Build the expected RESP encoding for a list of tokens.
    // RESP wire format:  *<argc>CRLF  ($<len>CRLF<value>CRLF)*
    function BuildRESP(const aTokens: array of string): string;
  public

    // ── TRedisCommand.ToCommand / RESP encoding ──────────────────
    [Test] procedure Test_ToCommand_SingleToken;
    [Test] procedure Test_ToCommand_TwoTokens;
    [Test] procedure Test_ToCommand_Int64Argument;
    [Test] procedure Test_ToCommand_ExtendedArgument;
    [Test] procedure Test_ToCommand_EmptyStringArgument;
    [Test] procedure Test_ToCommand_ArgumentCount;

    // Verify the RESP wire string produced by RedisSET scenarios.
    // We build the same command manually with TRedisCommand and compare.

    [Test] procedure Test_RedisSET_WithoutTTL_NoPXInCommand;
    [Test] procedure Test_RedisSET_WithTTL_PXPresent;
    [Test] procedure Test_RedisSET_NegativeTTL_NoPXInCommand;
    [Test] procedure Test_RedisSET_ZeroTTL_NoPXInCommand;
    [Test] procedure Test_RedisSET_WithoutTTL_ExactRESP;
    [Test] procedure Test_RedisSET_WithTTL_ExactRESP;
  end;

implementation

const
  CRLF = #13#10;

// ── Helper ────────────────────────────────────────────────────────────────

function TRedisCommandTests.BuildRESP(const aTokens: array of string): string;
var
  tok : string;
begin
  Result := '*' + Integer(Length(aTokens)).ToString + CRLF;
  for tok in aTokens do
    Result := Result + '$' + Integer(tok.Length).ToString + CRLF + tok + CRLF;
end;

// ── TRedisCommand.ToCommand ───────────────────────────────────────────────

procedure TRedisCommandTests.Test_ToCommand_SingleToken;
var
  cmd : IRedisCommand;
begin
  // A command with no extra arguments: *1\r\n$3\r\nGET\r\n  (only the verb)
  cmd := TRedisCommand.Create('GET');
  Assert.AreEqual(BuildRESP(['GET']), cmd.ToCommand,
    'Single-token command must produce correct RESP');
end;

procedure TRedisCommandTests.Test_ToCommand_TwoTokens;
var
  cmd : IRedisCommand;
begin
  cmd := TRedisCommand.Create('DEL').AddArgument('mykey');
  Assert.AreEqual(BuildRESP(['DEL', 'mykey']), cmd.ToCommand,
    'Two-token command must produce correct RESP');
end;

procedure TRedisCommandTests.Test_ToCommand_Int64Argument;
var
  cmd : IRedisCommand;
begin
  cmd := TRedisCommand.Create('EXPIRE').AddArgument('k').AddArgument(Int64(5000));
  Assert.AreEqual(BuildRESP(['EXPIRE', 'k', '5000']), cmd.ToCommand,
    'Int64 argument must be serialised as its decimal string representation');
end;

procedure TRedisCommandTests.Test_ToCommand_ExtendedArgument;
var
  cmd : IRedisCommand;
  resp : string;
begin
  cmd  := TRedisCommand.Create('CMD').AddArgument(Extended(1.5));
  resp := cmd.ToCommand;
  // The exact decimal representation depends on locale, but it must not be empty
  // and must start with the RESP array header for 2 elements.
  Assert.IsTrue(resp.StartsWith('*2' + CRLF),
    'Extended argument: RESP must start with *2CRLF');
  Assert.IsFalse(resp.Contains('$0' + CRLF + CRLF),
    'Extended argument value must not be empty');
end;

procedure TRedisCommandTests.Test_ToCommand_EmptyStringArgument;
var
  cmd : IRedisCommand;
begin
  // Empty string is a valid Redis bulk string: $0\r\n\r\n
  cmd := TRedisCommand.Create('SET').AddArgument('k').AddArgument('');
  Assert.AreEqual(BuildRESP(['SET', 'k', '']), cmd.ToCommand,
    'Empty string argument must produce $0 bulk string');
end;

procedure TRedisCommandTests.Test_ToCommand_ArgumentCount;
var
  cmd  : IRedisCommand;
  resp : string;
begin
  cmd  := TRedisCommand.Create('SET')
            .AddArgument('key')
            .AddArgument('val')
            .AddArgument('PX')
            .AddArgument(Int64(1000));
  resp := cmd.ToCommand;
  // 5 tokens: SET key val PX 1000
  Assert.IsTrue(resp.StartsWith('*5' + CRLF),
    'Five-token SET PX command must start with *5CRLF');
end;

procedure TRedisCommandTests.Test_RedisSET_WithoutTTL_NoPXInCommand;
var
  cmd  : IRedisCommand;
  resp : string;
begin
  // Reproduce what RedisSET builds when aTTLMs = -1 (default, no TTL)
  cmd  := TRedisCommand.Create('SET')
            .AddArgument('mykey')
            .AddArgument('myval');
  resp := cmd.ToCommand;
  Assert.IsFalse(resp.Contains('PX'),
    'Issue #143: SET without TTL must NOT contain PX in RESP command');
  Assert.IsFalse(resp.Contains('-1'),
    'Issue #143: SET without TTL must NOT contain -1 in RESP command');
end;

procedure TRedisCommandTests.Test_RedisSET_WithTTL_PXPresent;
var
  cmd  : IRedisCommand;
  resp : string;
begin
  // Reproduce what RedisSET builds when aTTLMs > 0
  cmd  := TRedisCommand.Create('SET')
            .AddArgument('mykey')
            .AddArgument('myval')
            .AddArgument('PX')
            .AddArgument(Int64(5000));
  resp := cmd.ToCommand;
  Assert.IsTrue(resp.Contains('PX'),
    'Issue #143: SET with TTL must contain PX in RESP command');
  Assert.IsTrue(resp.Contains('5000'),
    'Issue #143: SET with TTL must contain the millisecond value');
end;

procedure TRedisCommandTests.Test_RedisSET_NegativeTTL_NoPXInCommand;
var
  cmd  : IRedisCommand;
  resp : string;
begin
  // aTTLMs = -1 → no PX  (the old bug always appended PX -1 here)
  cmd  := TRedisCommand.Create('SET')
            .AddArgument('k')
            .AddArgument('v');
  // Do NOT add PX when TTL <= 0 (mirrors the fix in RedisSET)
  resp := cmd.ToCommand;
  Assert.IsFalse(resp.Contains('PX'),
    'Issue #143: negative TTL must not add PX to the command');
end;

procedure TRedisCommandTests.Test_RedisSET_ZeroTTL_NoPXInCommand;
var
  cmd  : IRedisCommand;
  resp : string;
begin
  // aTTLMs = 0 → also no PX (0 is not a positive TTL)
  cmd  := TRedisCommand.Create('SET')
            .AddArgument('k')
            .AddArgument('v');
  resp := cmd.ToCommand;
  Assert.IsFalse(resp.Contains('PX'),
    'Issue #143: zero TTL must not add PX to the command');
end;

procedure TRedisCommandTests.Test_RedisSET_WithoutTTL_ExactRESP;
var
  cmd      : IRedisCommand;
  expected : string;
begin
  // Full RESP wire check: SET mykey myval  (3 tokens, no PX)
  cmd      := TRedisCommand.Create('SET')
                .AddArgument('mykey')
                .AddArgument('myval');
  expected := BuildRESP(['SET', 'mykey', 'myval']);
  Assert.AreEqual(expected, cmd.ToCommand,
    'Issue #143: SET without TTL must produce exact 3-token RESP');
end;

procedure TRedisCommandTests.Test_RedisSET_WithTTL_ExactRESP;
var
  cmd      : IRedisCommand;
  expected : string;
begin
  // Full RESP wire check: SET mykey myval PX 2000  (5 tokens)
  cmd      := TRedisCommand.Create('SET')
                .AddArgument('mykey')
                .AddArgument('myval')
                .AddArgument('PX')
                .AddArgument(Int64(2000));
  expected := BuildRESP(['SET', 'mykey', 'myval', 'PX', '2000']);
  Assert.AreEqual(expected, cmd.ToCommand,
    'Issue #143: SET with TTL must produce exact 5-token RESP');
end;

initialization
  TDUnitX.RegisterTestFixture(TRedisCommandTests);

end.
