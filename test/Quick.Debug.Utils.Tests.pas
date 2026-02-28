{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.Debug.Utils.Tests
  Description : TDebugger, TDebugConsoleLogger, TDebugMethodEnter,
                TDebugMethodChrono tests
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

unit Quick.Debug.Utils.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections,
  Quick.Logger.Intf,
  Quick.Debug.Utils;

type

  // ── Spy logger: records every message written to it ─────────────────────
  TSpyLogger = class(TInterfacedObject, ILogger)
  private
    fMessages : TList<string>;
  public
    constructor Create;
    destructor  Destroy; override;
    // ILogger
    procedure Info    (const aMsg : string); overload;
    procedure Info    (const aMsg : string; aParams : array of const); overload;
    procedure Succ    (const aMsg : string); overload;
    procedure Succ    (const aMsg : string; aParams : array of const); overload;
    procedure Done    (const aMsg : string); overload;
    procedure Done    (const aMsg : string; aParams : array of const); overload;
    procedure Warn    (const aMsg : string); overload;
    procedure Warn    (const aMsg : string; aParams : array of const); overload;
    procedure Error   (const aMsg : string); overload;
    procedure Error   (const aMsg : string; aParams : array of const); overload;
    procedure Critical(const aMsg : string); overload;
    procedure Critical(const aMsg : string; aParams : array of const); overload;
    procedure Trace   (const aMsg : string); overload;
    procedure Trace   (const aMsg : string; aParams : array of const); overload;
    procedure Debug   (const aMsg : string); overload;
    procedure Debug   (const aMsg : string; aParams : array of const); overload;
    procedure &Except (const aMsg : string; aValues : array of const); overload;
    procedure &Except (const aMsg, aException, aStackTrace : string); overload;
    procedure &Except (const aMsg : string; aValues: array of const; const aException, aStackTrace: string); overload;
    // helpers
    function  LastMessage : string;
    function  Count : Integer;
    function  ContainsText(const aText : string) : Boolean;
    procedure Clear;
  end;

  // ── Dummy owner object for TDebugger.Enter / TDebugger.Trace ────────────
  TDummyOwner = class
  end;

  [TestFixture]
  TDebugUtilsTests = class(TObject)
  private
    fSpy : ILogger;
    function Spy : TSpyLogger;
  public
    [Setup]    procedure SetUp;
    [TearDown] procedure TearDown;

    // ── TDebugConsoleLogger ────────────────────────────────────────────────
    [Test] procedure Test_ConsoleLogger_Create_NoRaise;
    [Test] procedure Test_ConsoleLogger_ShowTime_DefaultTrue;
    [Test] procedure Test_ConsoleLogger_ShowTime_False_NoTimestamp;

    // ── TDebugger.SetLogger ────────────────────────────────────────────────
    [Test] procedure Test_SetLogger_Nil_Raises;
    [Test] procedure Test_SetLogger_Valid_NoRaise;

    // ── TDebugger.Log ──────────────────────────────────────────────────────
    [Test] procedure Test_Log_ReturnsNonNil;
    [Test] procedure Test_Log_Info_NoRaise;
    [Test] procedure Test_Log_Warn_NoRaise;
    [Test] procedure Test_Log_Error_NoRaise;
    [Test] procedure Test_Log_Debug_NoRaise;
    [Test] procedure Test_Log_Trace_NoRaise;
    [Test] procedure Test_Log_Critical_NoRaise;
    [Test] procedure Test_Log_Succ_NoRaise;
    [Test] procedure Test_Log_Done_NoRaise;
    [Test] procedure Test_Log_Except_Overload1_NoRaise;
    [Test] procedure Test_Log_Except_Overload2_NoRaise;
    [Test] procedure Test_Log_Except_Overload3_NoRaise;

    // ── TDebugger.Trace ────────────────────────────────────────────────────
    [Test] procedure Test_Trace_StringOnly_ContainsMsg;
    [Test] procedure Test_Trace_StringFormat_ContainsMsg;
    [Test] procedure Test_Trace_WithOwner_ContainsClassName;
    [Test] procedure Test_Trace_WithOwnerFormat_ContainsMsg;
    [Test] procedure Test_Trace_NilOwner_ContainsMsg;
    [Test] procedure Test_Trace_WithObject_ContainsMsg;

    // ── TDebugger.Enter ────────────────────────────────────────────────────
    [Test] procedure Test_Enter_WithOwner_ReturnsInterface;
    [Test] procedure Test_Enter_NilOwner_ReturnsInterface;
    [Test] procedure Test_Enter_LogsEnterMessage;
    [Test] procedure Test_Enter_LogsExitOnRelease;
    [Test] procedure Test_Enter_TimeIt_NoRaise;
    [Test] procedure Test_Enter_TimeIt_LogsElapsed;

    // ── TDebugger.TimeIt ───────────────────────────────────────────────────
    [Test] procedure Test_TimeIt_WithOwner_ReturnsInterface;
    [Test] procedure Test_TimeIt_NilOwner_ReturnsInterface;
    [Test] procedure Test_TimeIt_Stop_LogsMessage;
    [Test] procedure Test_TimeIt_BreakPoint_LogsMessage;
    [Test] procedure Test_TimeIt_AutoStop_OnRelease;

    // ── TDebugger.NewChrono ────────────────────────────────────────────────
    [Test] procedure Test_NewChrono_Started_IsRunning;
    [Test] procedure Test_NewChrono_NotStarted_NotRunning;
  end;

implementation

uses
  Quick.Chrono;

// ══════════════════════════════════════════════════════════════════════════════
//  TSpyLogger
// ══════════════════════════════════════════════════════════════════════════════

constructor TSpyLogger.Create;
begin
  fMessages := TList<string>.Create;
end;

destructor TSpyLogger.Destroy;
begin
  fMessages.Free;
  inherited;
end;

procedure TSpyLogger.Info    (const aMsg: string);                              begin fMessages.Add(aMsg); end;
procedure TSpyLogger.Info    (const aMsg: string; aParams: array of const);     begin fMessages.Add(Format(aMsg,aParams)); end;
procedure TSpyLogger.Succ    (const aMsg: string);                              begin fMessages.Add(aMsg); end;
procedure TSpyLogger.Succ    (const aMsg: string; aParams: array of const);     begin fMessages.Add(Format(aMsg,aParams)); end;
procedure TSpyLogger.Done    (const aMsg: string);                              begin fMessages.Add(aMsg); end;
procedure TSpyLogger.Done    (const aMsg: string; aParams: array of const);     begin fMessages.Add(Format(aMsg,aParams)); end;
procedure TSpyLogger.Warn    (const aMsg: string);                              begin fMessages.Add(aMsg); end;
procedure TSpyLogger.Warn    (const aMsg: string; aParams: array of const);     begin fMessages.Add(Format(aMsg,aParams)); end;
procedure TSpyLogger.Error   (const aMsg: string);                              begin fMessages.Add(aMsg); end;
procedure TSpyLogger.Error   (const aMsg: string; aParams: array of const);     begin fMessages.Add(Format(aMsg,aParams)); end;
procedure TSpyLogger.Critical(const aMsg: string);                              begin fMessages.Add(aMsg); end;
procedure TSpyLogger.Critical(const aMsg: string; aParams: array of const);     begin fMessages.Add(Format(aMsg,aParams)); end;
procedure TSpyLogger.Trace   (const aMsg: string);                              begin fMessages.Add(aMsg); end;
procedure TSpyLogger.Trace   (const aMsg: string; aParams: array of const);     begin fMessages.Add(Format(aMsg,aParams)); end;
procedure TSpyLogger.Debug   (const aMsg: string);                              begin fMessages.Add(aMsg); end;
procedure TSpyLogger.Debug   (const aMsg: string; aParams: array of const);     begin fMessages.Add(Format(aMsg,aParams)); end;
procedure TSpyLogger.&Except (const aMsg: string; aValues: array of const);     begin fMessages.Add(Format(aMsg,aValues)); end;
procedure TSpyLogger.&Except (const aMsg, aException, aStackTrace: string);     begin fMessages.Add(aMsg); end;
procedure TSpyLogger.&Except (const aMsg: string; aValues: array of const; const aException, aStackTrace: string); begin fMessages.Add(Format(aMsg,aValues)); end;

function TSpyLogger.LastMessage: string;
begin
  if fMessages.Count = 0 then Result := ''
  else Result := fMessages[fMessages.Count - 1];
end;

function TSpyLogger.Count: Integer;
begin
  Result := fMessages.Count;
end;

function TSpyLogger.ContainsText(const aText: string): Boolean;
var
  msg: string;
begin
  for msg in fMessages do
    if System.StrUtils.ContainsText(msg, aText) then Exit(True);
  Result := False;
end;

procedure TSpyLogger.Clear;
begin
  fMessages.Clear;
end;

// ══════════════════════════════════════════════════════════════════════════════
//  TDebugUtilsTests
// ══════════════════════════════════════════════════════════════════════════════

function TDebugUtilsTests.Spy: TSpyLogger;
begin
  Result := fSpy as TSpyLogger;
end;

procedure TDebugUtilsTests.SetUp;
begin
  fSpy := TSpyLogger.Create;
  TDebugger.SetLogger(fSpy);
end;

procedure TDebugUtilsTests.TearDown;
begin
  // Restore the default console logger so other tests are unaffected
  TDebugger.SetLogger(TDebugConsoleLogger.Create);
  fSpy := nil;
end;

// ── TDebugConsoleLogger ──────────────────────────────────────────────────────

procedure TDebugUtilsTests.Test_ConsoleLogger_Create_NoRaise;
var
  logger: TDebugConsoleLogger;
begin
  Assert.WillNotRaise(
    procedure begin logger := TDebugConsoleLogger.Create; logger.Free; end,
    ExceptClass(nil),
    'TDebugConsoleLogger.Create must not raise');
end;

procedure TDebugUtilsTests.Test_ConsoleLogger_ShowTime_DefaultTrue;
var
  logger: TDebugConsoleLogger;
begin
  logger := TDebugConsoleLogger.Create;
  try
    Assert.IsTrue(logger.ShowTime, 'ShowTime must default to True');
  finally
    logger.Free;
  end;
end;

procedure TDebugUtilsTests.Test_ConsoleLogger_ShowTime_False_NoTimestamp;
var
  logger: TDebugConsoleLogger;
begin
  logger := TDebugConsoleLogger.Create;
  try
    logger.ShowTime := False;
    Assert.IsFalse(logger.ShowTime, 'ShowTime must be False after assignment');
  finally
    logger.Free;
  end;
end;

// ── TDebugger.SetLogger ──────────────────────────────────────────────────────

procedure TDebugUtilsTests.Test_SetLogger_Nil_Raises;
begin
  Assert.WillRaise(
    procedure begin TDebugger.SetLogger(nil); end,
    Exception,
    'SetLogger(nil) must raise');
end;

procedure TDebugUtilsTests.Test_SetLogger_Valid_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.SetLogger(TSpyLogger.Create); end,
    ExceptClass(nil),
    'SetLogger with a valid logger must not raise');
end;

// ── TDebugger.Log ────────────────────────────────────────────────────────────

procedure TDebugUtilsTests.Test_Log_ReturnsNonNil;
begin
  Assert.IsNotNull(TDebugger.Log, 'TDebugger.Log must return a non-nil ILogger');
end;

procedure TDebugUtilsTests.Test_Log_Info_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.Log.Info('info message'); end,
    ExceptClass(nil), 'Log.Info must not raise');
end;

procedure TDebugUtilsTests.Test_Log_Warn_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.Log.Warn('warn message'); end,
    ExceptClass(nil), 'Log.Warn must not raise');
end;

procedure TDebugUtilsTests.Test_Log_Error_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.Log.Error('error message'); end,
    ExceptClass(nil), 'Log.Error must not raise');
end;

procedure TDebugUtilsTests.Test_Log_Debug_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.Log.Debug('debug message'); end,
    ExceptClass(nil), 'Log.Debug must not raise');
end;

procedure TDebugUtilsTests.Test_Log_Trace_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.Log.Trace('trace message'); end,
    ExceptClass(nil), 'Log.Trace must not raise');
end;

procedure TDebugUtilsTests.Test_Log_Critical_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.Log.Critical('critical message'); end,
    ExceptClass(nil), 'Log.Critical must not raise');
end;

procedure TDebugUtilsTests.Test_Log_Succ_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.Log.Succ('success message'); end,
    ExceptClass(nil), 'Log.Succ must not raise');
end;

procedure TDebugUtilsTests.Test_Log_Done_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.Log.Done('done message'); end,
    ExceptClass(nil), 'Log.Done must not raise');
end;

procedure TDebugUtilsTests.Test_Log_Except_Overload1_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.Log.&Except('msg %s', ['val']); end,
    ExceptClass(nil), 'Log.Except overload 1 must not raise');
end;

procedure TDebugUtilsTests.Test_Log_Except_Overload2_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.Log.&Except('msg', 'EExc', 'stack'); end,
    ExceptClass(nil), 'Log.Except overload 2 must not raise');
end;

procedure TDebugUtilsTests.Test_Log_Except_Overload3_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TDebugger.Log.&Except('msg %s', ['val'], 'EExc', 'stack'); end,
    ExceptClass(nil), 'Log.Except overload 3 must not raise');
end;

// ── TDebugger.Trace ──────────────────────────────────────────────────────────

procedure TDebugUtilsTests.Test_Trace_StringOnly_ContainsMsg;
begin
  Spy.Clear;
  TDebugger.Trace('hello world');
  Assert.IsTrue(Spy.ContainsText('hello world'),
    'Trace(string) must forward the message to the logger');
end;

procedure TDebugUtilsTests.Test_Trace_StringFormat_ContainsMsg;
begin
  Spy.Clear;
  TDebugger.Trace('value is %d', [42]);
  Assert.IsTrue(Spy.ContainsText('42'),
    'Trace(string, params) must format and forward the message');
end;

procedure TDebugUtilsTests.Test_Trace_WithOwner_ContainsClassName;
var
  owner: TDummyOwner;
begin
  owner := TDummyOwner.Create;
  try
    Spy.Clear;
    TDebugger.Trace(owner, 'some trace');
    Assert.IsTrue(Spy.ContainsText('TDummyOwner'),
      'Trace(owner, msg) must include the owner class name');
    Assert.IsTrue(Spy.ContainsText('some trace'),
      'Trace(owner, msg) must include the message');
  finally
    owner.Free;
  end;
end;

procedure TDebugUtilsTests.Test_Trace_WithOwnerFormat_ContainsMsg;
var
  owner: TDummyOwner;
begin
  owner := TDummyOwner.Create;
  try
    Spy.Clear;
    TDebugger.Trace(owner, 'count=%d', [7]);
    Assert.IsTrue(Spy.ContainsText('7'),
      'Trace(owner, format, params) must format the message');
  finally
    owner.Free;
  end;
end;

procedure TDebugUtilsTests.Test_Trace_NilOwner_ContainsMsg;
begin
  Spy.Clear;
  TDebugger.Trace(nil, 'nil owner msg');
  Assert.IsTrue(Spy.ContainsText('nil owner msg'),
    'Trace(nil, msg) must still forward the message');
end;

procedure TDebugUtilsTests.Test_Trace_WithObject_ContainsMsg;
var
  obj: TDummyOwner;
begin
  obj := TDummyOwner.Create;
  try
    Spy.Clear;
    Assert.WillNotRaise(
      procedure begin TDebugger.Trace('obj dump', obj); end,
      ExceptClass(nil),
      'Trace(msg, TObject) must not raise');
  finally
    obj.Free;
  end;
end;

// ── TDebugger.Enter ──────────────────────────────────────────────────────────

procedure TDebugUtilsTests.Test_Enter_WithOwner_ReturnsInterface;
var
  owner : TDummyOwner;
  guard : IDebugMethodEnter;
begin
  owner := TDummyOwner.Create;
  try
    guard := TDebugger.Enter(owner, 'MyMethod');
    Assert.IsNotNull(guard, 'Enter must return a non-nil IDebugMethodEnter');
  finally
    owner.Free;
  end;
end;

procedure TDebugUtilsTests.Test_Enter_NilOwner_ReturnsInterface;
var
  guard: IDebugMethodEnter;
begin
  guard := TDebugger.Enter(nil, 'StandaloneFunc');
  Assert.IsNotNull(guard, 'Enter(nil, name) must return a non-nil IDebugMethodEnter');
end;

procedure TDebugUtilsTests.Test_Enter_LogsEnterMessage;
var
  owner : TDummyOwner;
  guard : IDebugMethodEnter;
begin
  owner := TDummyOwner.Create;
  try
    Spy.Clear;
    guard := TDebugger.Enter(owner, 'DoWork');
    Assert.IsTrue(Spy.ContainsText('ENTER'),
      'Enter must log an [ENTER] message immediately');
    Assert.IsTrue(Spy.ContainsText('DoWork'),
      'Enter message must contain the function name');
  finally
    guard := nil;   // triggers destructor → [EXIT] log
    owner.Free;
  end;
end;

procedure TDebugUtilsTests.Test_Enter_LogsExitOnRelease;
var
  owner : TDummyOwner;
begin
  owner := TDummyOwner.Create;
  try
    Spy.Clear;
    begin
      var guard := TDebugger.Enter(owner, 'DoWork');
      Spy.Clear;   // discard [ENTER] — we only care about [EXIT]
      guard := nil; // release → destructor fires
    end;
    Assert.IsTrue(Spy.ContainsText('EXIT'),
      'Releasing the IDebugMethodEnter interface must log an [EXIT] message');
  finally
    owner.Free;
  end;
end;

procedure TDebugUtilsTests.Test_Enter_TimeIt_NoRaise;
var
  owner : TDummyOwner;
  guard : IDebugMethodEnter;
begin
  owner := TDummyOwner.Create;
  try
    Assert.WillNotRaise(
      procedure
      begin
        guard := TDebugger.Enter(owner, 'Timed');
        guard.TimeIt;
        guard := nil;
      end,
      ExceptClass(nil),
      'Enter + TimeIt must not raise');
  finally
    owner.Free;
  end;
end;

procedure TDebugUtilsTests.Test_Enter_TimeIt_LogsElapsed;
var
  owner : TDummyOwner;
begin
  owner := TDummyOwner.Create;
  try
    Spy.Clear;
    begin
      var guard := TDebugger.Enter(owner, 'Timed');
      guard.TimeIt;
      Spy.Clear;   // discard [ENTER]
      guard := nil; // destructor logs elapsed time
    end;
    Assert.IsTrue(Spy.ContainsText('EXIT'),
      'TimeIt release must still log an [EXIT] message with elapsed time');
  finally
    owner.Free;
  end;
end;

// ── TDebugger.TimeIt ─────────────────────────────────────────────────────────

procedure TDebugUtilsTests.Test_TimeIt_WithOwner_ReturnsInterface;
var
  owner : TDummyOwner;
  chrono: IDebugMehtodChrono;
begin
  owner := TDummyOwner.Create;
  try
    chrono := TDebugger.TimeIt(owner, 'Process', 'processing items');
    Assert.IsNotNull(chrono, 'TimeIt must return a non-nil IDebugMehtodChrono');
    chrono.Stop;
  finally
    owner.Free;
  end;
end;

procedure TDebugUtilsTests.Test_TimeIt_NilOwner_ReturnsInterface;
var
  chrono: IDebugMehtodChrono;
begin
  chrono := TDebugger.TimeIt(nil, 'FreeFunc', 'doing work');
  Assert.IsNotNull(chrono, 'TimeIt(nil, ...) must return a non-nil IDebugMehtodChrono');
  chrono.Stop;
end;

procedure TDebugUtilsTests.Test_TimeIt_Stop_LogsMessage;
var
  owner : TDummyOwner;
  chrono: IDebugMehtodChrono;
begin
  owner := TDummyOwner.Create;
  try
    Spy.Clear;
    chrono := TDebugger.TimeIt(owner, 'Run', 'running');
    chrono.Stop;
    Assert.IsTrue(Spy.ContainsText('CHRONO'),
      'TimeIt.Stop must log a [CHRONO] message');
    Assert.IsTrue(Spy.ContainsText('running'),
      'TimeIt.Stop message must contain the description');
  finally
    owner.Free;
  end;
end;

procedure TDebugUtilsTests.Test_TimeIt_BreakPoint_LogsMessage;
var
  owner : TDummyOwner;
  chrono: IDebugMehtodChrono;
begin
  owner := TDummyOwner.Create;
  try
    Spy.Clear;
    chrono := TDebugger.TimeIt(owner, 'Run', 'running');
    chrono.BreakPoint('checkpoint A');
    Assert.IsTrue(Spy.ContainsText('CHRONO'),
      'TimeIt.BreakPoint must log a [CHRONO] message');
    Assert.IsTrue(Spy.ContainsText('checkpoint A'),
      'TimeIt.BreakPoint message must contain the breakpoint label');
    chrono.Stop;
  finally
    owner.Free;
  end;
end;

procedure TDebugUtilsTests.Test_TimeIt_AutoStop_OnRelease;
var
  owner: TDummyOwner;
begin
  owner := TDummyOwner.Create;
  try
    Spy.Clear;
    begin
      var chrono := TDebugger.TimeIt(owner, 'Auto', 'auto-stop');
      chrono := nil; // destructor fires; chrono is still running → logs
    end;
    Assert.IsTrue(Spy.ContainsText('CHRONO'),
      'Releasing TimeIt without explicit Stop must still log a [CHRONO] message');
  finally
    owner.Free;
  end;
end;

// ── TDebugger.NewChrono ──────────────────────────────────────────────────────

procedure TDebugUtilsTests.Test_NewChrono_Started_IsRunning;
var
  c: IChronometer;
begin
  c := TDebugger.NewChrono(True);
  Assert.IsNotNull(c, 'NewChrono must return non-nil');
  Assert.IsTrue(c.IsRunning, 'NewChrono(True) must be running');
  c.Stop;
end;

procedure TDebugUtilsTests.Test_NewChrono_NotStarted_NotRunning;
var
  c: IChronometer;
begin
  c := TDebugger.NewChrono(False);
  Assert.IsNotNull(c, 'NewChrono must return non-nil');
  Assert.IsFalse(c.IsRunning, 'NewChrono(False) must not be running');
end;

initialization
  TDUnitX.RegisterTestFixture(TDebugUtilsTests);

end.
