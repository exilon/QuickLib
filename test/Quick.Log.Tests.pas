{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.Log.Tests
  Description : TQuickLog / TMemoryLog unit tests
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

unit Quick.Log.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Quick.Commons,
  Quick.Log;

type
  [TestFixture]
  TQuickLogTests = class(TObject)
  private
    fLog     : TQuickLog;
    fLogFile : string;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // TMemoryLog tests
    [Test]
    procedure Test_MemoryLog_Create_DefaultDisabled;
    [Test]
    procedure Test_MemoryLog_EnableAndCapture;
    [Test]
    procedure Test_MemoryLog_Text_ContainsLoggedMessage;
    [Test]
    procedure Test_MemoryLog_Lines_Count;

    // TQuickLog – basic creation
    [Test]
    procedure Test_Create_DefaultVerbose;
    [Test]
    procedure Test_Create_MemoryLogAssigned;

    // TQuickLog – SetLog
    [Test]
    procedure Test_SetLog_ReturnsTrue;
    [Test]
    procedure Test_SetLog_LogFileNameSet;

    // TQuickLog – Add to MemoryLog
    [Test]
    procedure Test_Add_String_AppearsInMemoryLog;
    [Test]
    procedure Test_Add_Format_AppearsInMemoryLog;
    [Test]
    procedure Test_Add_MultipleMessages_AllAppear;

    // TQuickLog – Verbose filtering
    [Test]
    procedure Test_Verbose_None_FiltersAllMessages;
    [Test]
    procedure Test_Verbose_OnlyErrors_FiltersInfo;
  end;

implementation

procedure TQuickLogTests.SetUp;
begin
  fLog := TQuickLog.Create;
  fLogFile := TPath.Combine(TPath.GetTempPath,
    'quicklib_log_test_' + IntToStr(TThread.CurrentThread.ThreadID) + '.log');
  // SetLog must be called so WriteLog has a valid filename to write to
  fLog.ShowHeaderInfo := False;
  fLog.SetLog(fLogFile, False, 0);
  // Enable MemoryLog for all tests
  fLog.MemoryLog.Enabled := True;
end;

procedure TQuickLogTests.TearDown;
begin
  fLog.Free;
  if TFile.Exists(fLogFile) then TFile.Delete(fLogFile);
end;

// ── TMemoryLog ─────────────────────────────────────────────────────────────

procedure TQuickLogTests.Test_MemoryLog_Create_DefaultDisabled;
var
  ml : TMemoryLog;
begin
  ml := TMemoryLog.Create;
  try
    Assert.IsFalse(ml.Enabled, 'MemoryLog should be disabled by default');
  finally
    ml.Free;
  end;
end;

procedure TQuickLogTests.Test_MemoryLog_EnableAndCapture;
begin
  fLog.MemoryLog.Enabled := True;
  fLog.Add('hello memlog', etInfo);
  Assert.IsTrue(fLog.MemoryLog.Lines.Count > 0, 'MemoryLog should capture when enabled');
end;

procedure TQuickLogTests.Test_MemoryLog_Text_ContainsLoggedMessage;
begin
  fLog.MemoryLog.Enabled := True;
  fLog.Add('unique_token_xyz', etInfo);
  Assert.IsTrue(fLog.MemoryLog.Text.Contains('unique_token_xyz'),
    'MemoryLog.Text should contain the logged message');
end;

procedure TQuickLogTests.Test_MemoryLog_Lines_Count;
begin
  fLog.MemoryLog.Enabled := True;
  fLog.MemoryLog.Lines.Clear;
  fLog.Add('msg1', etInfo);
  fLog.Add('msg2', etInfo);
  fLog.Add('msg3', etInfo);
  Assert.AreEqual(3, Integer(fLog.MemoryLog.Lines.Count),
    'MemoryLog should have 3 lines after 3 Add calls');
end;

// ── TQuickLog creation ──────────────────────────────────────────────────────

procedure TQuickLogTests.Test_Create_DefaultVerbose;
begin
  // LOG_ALL means all event types are captured
  Assert.IsTrue(fLog.Verbose = LOG_ALL,
    'Default Verbose should be LOG_ALL');
end;

procedure TQuickLogTests.Test_Create_MemoryLogAssigned;
begin
  Assert.IsNotNull(fLog.MemoryLog, 'MemoryLog should be assigned after Create');
end;

// ── SetLog ──────────────────────────────────────────────────────────────────

procedure TQuickLogTests.Test_SetLog_ReturnsTrue;
begin
  Assert.IsTrue(fLog.SetLog(fLogFile, False, 0),
    'SetLog should return True for a valid path');
end;

procedure TQuickLogTests.Test_SetLog_LogFileNameSet;
begin
  fLog.ShowHeaderInfo := False; // avoid writing header during test
  fLog.SetLog(fLogFile, False, 0);
  Assert.AreEqual(fLogFile, fLog.LogFileName,
    'LogFileName should match the name passed to SetLog');
end;

// ── Add ─────────────────────────────────────────────────────────────────────

procedure TQuickLogTests.Test_Add_String_AppearsInMemoryLog;
begin
  fLog.MemoryLog.Enabled := True;
  fLog.Add('TestMessage_ABC', etInfo);
  Assert.IsTrue(fLog.MemoryLog.Text.Contains('TestMessage_ABC'),
    'Logged string should appear in MemoryLog');
end;

procedure TQuickLogTests.Test_Add_Format_AppearsInMemoryLog;
begin
  fLog.MemoryLog.Enabled := True;
  fLog.Add('Value=%d Name=%s', [42, 'Alice'], etInfo);
  Assert.IsTrue(fLog.MemoryLog.Text.Contains('Value=42'),
    'Formatted log message should appear in MemoryLog');
  Assert.IsTrue(fLog.MemoryLog.Text.Contains('Name=Alice'),
    'Formatted log message name part should appear in MemoryLog');
end;

procedure TQuickLogTests.Test_Add_MultipleMessages_AllAppear;
begin
  fLog.MemoryLog.Enabled := True;
  fLog.MemoryLog.Lines.Clear;
  fLog.Add('alpha', etInfo);
  fLog.Add('beta', etWarning);
  fLog.Add('gamma', etError);
  Assert.IsTrue(fLog.MemoryLog.Text.Contains('alpha'), 'alpha missing from MemoryLog');
  Assert.IsTrue(fLog.MemoryLog.Text.Contains('beta'),  'beta missing from MemoryLog');
  Assert.IsTrue(fLog.MemoryLog.Text.Contains('gamma'), 'gamma missing from MemoryLog');
end;

// ── Verbose filtering ───────────────────────────────────────────────────────

procedure TQuickLogTests.Test_Verbose_None_FiltersAllMessages;
begin
  fLog.MemoryLog.Enabled := True;
  fLog.MemoryLog.Lines.Clear;
  fLog.Verbose := [];
  fLog.Add('should_be_filtered', etInfo);
  Assert.AreEqual(0, Integer(fLog.MemoryLog.Lines.Count),
    'With Verbose=LOG_NONE no messages should be captured');
end;

procedure TQuickLogTests.Test_Verbose_OnlyErrors_FiltersInfo;
begin
  fLog.MemoryLog.Enabled := True;
  fLog.MemoryLog.Lines.Clear;
  // Use [etError] (just errors) so that etInfo is excluded
  fLog.Verbose := [etError];
  fLog.Add('info_msg', etInfo);
  fLog.Add('error_msg', etError);
  // info should be filtered, error should pass
  Assert.IsFalse(fLog.MemoryLog.Text.Contains('info_msg'),
    'Info message should be filtered when Verbose=[etError]');
  Assert.IsTrue(fLog.MemoryLog.Text.Contains('error_msg'),
    'Error message should appear when Verbose=[etError]');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickLogTests);

end.
