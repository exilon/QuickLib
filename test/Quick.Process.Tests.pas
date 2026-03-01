{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.Process.Tests
  Description : Quick.Process unit tests
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 01/03/2026
  Modified    : 01/03/2026
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

unit Quick.Process.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  SysUtils,
  Classes,
  Quick.Process;

type
  [TestFixture]
  TQuickProcessTests = class(TObject)
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // ── RunCommand ─────────────────────────────────────────────────────────
    [Test]
    procedure Test_RunCommand_Echo_ReturnsOutput;
    [Test]
    procedure Test_RunCommand_ReturnsStringList;
    [Test]
    procedure Test_RunCommand_EmptyParams_Works;

    // ── KillProcess(pid) ───────────────────────────────────────────────────
    {$IFDEF MSWINDOWS}
    [Test]
    procedure Test_KillProcess_InvalidPid_ReturnsFalse;
    {$ENDIF}

    // ── GetProcessList ─────────────────────────────────────────────────────
    [Test]
    procedure Test_GetProcessList_NotEmpty;
    [Test]
    procedure Test_GetProcessList_ReturnsStringList;

    // ── IsProcessRunning (Linux/macOS) / IsProcessRunnig (Windows) ─────────
    {$IFDEF MSWINDOWS}
    [Test]
    procedure Test_IsProcessRunnig_CurrentExe_IsRunning;
    [Test]
    procedure Test_IsProcessRunnig_NonExistent_IsFalse;
    {$ELSE}
    [Test]
    procedure Test_IsProcessRunning_KnownProcess_IsRunning;
    [Test]
    procedure Test_IsProcessRunning_NonExistent_IsFalse;
    {$ENDIF}

    // ── Windows-only ───────────────────────────────────────────────────────
    {$IFDEF MSWINDOWS}
    [Test]
    procedure Test_GetProcessId_CurrentExe_Found;
    [Test]
    procedure Test_KillProcess_ByName_InvalidName_ReturnsZero;
    {$ENDIF}
  end;

implementation

uses
  Quick.Commons;

procedure TQuickProcessTests.SetUp;
begin
end;

procedure TQuickProcessTests.TearDown;
begin
end;

// ── RunCommand ──────────────────────────────────────────────────────────────

procedure TQuickProcessTests.Test_RunCommand_Echo_ReturnsOutput;
var
  sl : TStringList;
begin
  {$IFDEF MSWINDOWS}
  sl := RunCommand('cmd', '/C echo hello');
  {$ELSE}
  sl := RunCommand('echo', 'hello');
  {$ENDIF}
  try
    Assert.IsTrue(sl.Count > 0, 'RunCommand echo must return at least one line');
    Assert.IsTrue(sl.Text.Contains('hello'), 'RunCommand echo must contain "hello"');
  finally
    sl.Free;
  end;
end;

procedure TQuickProcessTests.Test_RunCommand_ReturnsStringList;
var
  sl : TStringList;
begin
  {$IFDEF MSWINDOWS}
  sl := RunCommand('cmd', '/C echo test');
  {$ELSE}
  sl := RunCommand('echo', 'test');
  {$ENDIF}
  try
    Assert.IsNotNull(sl, 'RunCommand must return a non-nil TStringList');
  finally
    sl.Free;
  end;
end;

procedure TQuickProcessTests.Test_RunCommand_EmptyParams_Works;
var
  sl : TStringList;
begin
  Assert.WillNotRaise(
    procedure
    begin
      {$IFDEF MSWINDOWS}
      sl := RunCommand('cmd', '/C echo ok');
      {$ELSE}
      sl := RunCommand('echo', 'ok');
      {$ENDIF}
      sl.Free;
    end,
    nil,
    'RunCommand with simple params must not raise');
end;

// ── KillProcess(pid) ────────────────────────────────────────────────────────

{$IFDEF MSWINDOWS}
procedure TQuickProcessTests.Test_KillProcess_InvalidPid_ReturnsFalse;
begin
  // PID 0 and $FFFFFFFF are always invalid / protected on Windows
  Assert.IsFalse(KillProcess(Cardinal($FFFFFFFF)),
    'KillProcess with invalid PID must return False on Windows');
end;
{$ENDIF}

// ── GetProcessList ───────────────────────────────────────────────────────────

procedure TQuickProcessTests.Test_GetProcessList_NotEmpty;
var
  sl : TStringList;
begin
  sl := GetProcessList;
  try
    Assert.IsTrue(sl.Count > 0, 'GetProcessList must return at least one running process');
  finally
    sl.Free;
  end;
end;

procedure TQuickProcessTests.Test_GetProcessList_ReturnsStringList;
var
  sl : TStringList;
begin
  sl := GetProcessList;
  try
    Assert.IsNotNull(sl, 'GetProcessList must return a non-nil TStringList');
  finally
    sl.Free;
  end;
end;

// ── IsProcessRunning / IsProcessRunnig ──────────────────────────────────────

{$IFDEF MSWINDOWS}
procedure TQuickProcessTests.Test_IsProcessRunnig_CurrentExe_IsRunning;
var
  exeName : string;
begin
  exeName := ExtractFileName(ParamStr(0));
  Assert.IsTrue(IsProcessRunnig(exeName, False),
    'The current executable must appear in the running process list');
end;

procedure TQuickProcessTests.Test_IsProcessRunnig_NonExistent_IsFalse;
begin
  Assert.IsFalse(IsProcessRunnig('__nonexistent_process_xyz__.exe', False),
    'A made-up process name must not be found');
end;

{$ELSE}

procedure TQuickProcessTests.Test_IsProcessRunning_KnownProcess_IsRunning;
var
  procs : TStringList;
  exeName : string;
begin
  // Pick the first process from GetProcessList and verify IsProcessRunning finds it.
  // This avoids hard-coding init/systemd/ps which vary by container/distro.
  procs := GetProcessList;
  try
    Assert.IsTrue(procs.Count > 0, 'GetProcessList must return at least one process');
    exeName := procs[0].Trim;
  finally
    procs.Free;
  end;
  Assert.IsTrue(IsProcessRunning(exeName),
    'A process name returned by GetProcessList must be found by IsProcessRunning ("' + exeName + '")');
end;

procedure TQuickProcessTests.Test_IsProcessRunning_NonExistent_IsFalse;
begin
  Assert.IsFalse(IsProcessRunning('__nonexistent_xyz_process__'),
    'A made-up process name must not be found');
end;

{$ENDIF}

// ── Windows-only extras ─────────────────────────────────────────────────────

{$IFDEF MSWINDOWS}
procedure TQuickProcessTests.Test_GetProcessId_CurrentExe_Found;
var
  exeName : string;
  pid     : Integer;
begin
  exeName := ExtractFileName(ParamStr(0));
  Assert.IsTrue(Quick.Process.GetProcessId(exeName, pid),
    'GetProcessId must find the current executable');
  Assert.IsTrue(pid > 0, 'Process ID must be positive');
end;

procedure TQuickProcessTests.Test_KillProcess_ByName_InvalidName_ReturnsZero;
begin
  Assert.IsTrue(KillProcess('__no_such_process_xyz__.exe') = 0,
    'KillProcess by name with unknown process must return 0');
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TQuickProcessTests);

end.
