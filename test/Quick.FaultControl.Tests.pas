{ ***************************************************************************

  Copyright (c) 2016-2026 Kike Pérez

  Unit        : Quick.FaultControl.Tests
  Description : Unit Tests for Quick.FaultControl
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 27/02/2026
  Modified    : 27/02/2026

 *************************************************************************** }

unit Quick.FaultControl.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Quick.FaultControl;

type
  { Helper object — provides method pointers compatible with TRetryEvent / TCircuitBreakEvent }
  TFCCallbackHelper = class
  public
    Called: Boolean;
    RetryCount: Integer;
    Cancelled: Boolean;
    procedure OnRetry(aEx: Exception; var vStop: Boolean);
    procedure OnRetryCancel(aEx: Exception; var vStop: Boolean);
    procedure OnRetryCountLimit(aEx: Exception; var vStop: Boolean);
    procedure OnCircuitBreak;
  end;

  [TestFixture]
  TQuickFaultControlTests = class(TObject)
  private
    fFC: TFaultControl;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    { TFaultPolicy }
    [Test]
    procedure Test_FaultPolicy_DefaultValues;

    { TFaultControl — initial state }
    [Test]
    procedure Test_FaultControl_InitialState;

    { SuccessExecution clears failure flag }
    [Test]
    procedure Test_SuccessExecution_ClearsTaskFailed;

    { FailedExecution with MaxRetries=0 raises immediately }
    [Test]
    procedure Test_FailedExecution_MaxRetriesZero_Raises;

    { FailedExecution sets TaskFailed and stores exception }
    [Test]
    procedure Test_FailedExecution_SetsTaskFailed;

    { NeedToRetry returns False when MaxRetries=0 }
    [Test]
    procedure Test_NeedToRetry_MaxRetriesZero_ReturnsFalse;

    { NeedToRetry returns True while under limit }
    [Test]
    procedure Test_NeedToRetry_UnderLimit_ReturnsTrue;

    { NeedToRetry increments NumRetries }
    [Test]
    procedure Test_NeedToRetry_IncrementsNumRetries;

    { NeedToRetry triggers circuit break at MaxRetries }
    [Test]
    procedure Test_NeedToRetry_AtMaxRetries_CircuitBreaks;

    { NeedToRetry returns False when not failed }
    [Test]
    procedure Test_NeedToRetry_NotFailed_ReturnsFalse;

    { Reset restores initial state }
    [Test]
    procedure Test_Reset_RestoresState;

    { WaitTimeMultiplierFactor zero raises }
    [Test]
    procedure Test_WaitTimeMultiplierFactor_Zero_Raises;

    { WaitTimeMultiplierFactor valid value accepted }
    [Test]
    procedure Test_WaitTimeMultiplierFactor_ValidValue;

    { OnRetry callback is called on retry }
    [Test]
    procedure Test_OnRetry_CalledOnRetry;

    { OnRetry callback can cancel retries via vStopRetries }
    [Test]
    procedure Test_OnRetry_CanCancelRetries;

    { OnCircuitBreak callback called when limit reached }
    [Test]
    procedure Test_OnCircuitBreak_CalledWhenBreaking;

    { WaitTimeMSArray drives wait times instead of WaitTimeBetweenRetriesMS }
    [Test]
    procedure Test_WaitTimeMSArray_Accepted;

    { MaxRetries = -1 means retry forever (at least several iterations) }
    [Test]
    procedure Test_MaxRetriesMinusOne_RetriesForever_UntilCancelled;
  end;

implementation

{ TFCCallbackHelper }

procedure TFCCallbackHelper.OnRetry(aEx: Exception; var vStop: Boolean);
begin
  Called := True;
end;

procedure TFCCallbackHelper.OnRetryCancel(aEx: Exception; var vStop: Boolean);
begin
  Called := True;
  vStop := True;
end;

procedure TFCCallbackHelper.OnRetryCountLimit(aEx: Exception; var vStop: Boolean);
begin
  Inc(RetryCount);
  if RetryCount >= 5 then vStop := True;
end;

procedure TFCCallbackHelper.OnCircuitBreak;
begin
  Called := True;
end;

{ TQuickFaultControlTests }

procedure TQuickFaultControlTests.SetUp;
begin
  fFC := TFaultControl.Create;
end;

procedure TQuickFaultControlTests.TearDown;
begin
  fFC.Free;
end;

{ TFaultPolicy }

procedure TQuickFaultControlTests.Test_FaultPolicy_DefaultValues;
var
  policy: TFaultPolicy;
begin
  policy := TFaultPolicy.Create;
  try
    Assert.AreEqual(0, policy.MaxRetries, 'Default MaxRetries should be 0');
    Assert.AreEqual(0, policy.WaitTimeBetweenRetries, 'Default WaitTimeBetweenRetries should be 0');
    Assert.AreEqual(Double(1), Double(policy.WaitTimeMultiplierFactor), Double(0.001),
      'Default WaitTimeMultiplierFactor should be 1');
  finally
    policy.Free;
  end;
end;

{ TFaultControl — initial state }

procedure TQuickFaultControlTests.Test_FaultControl_InitialState;
begin
  Assert.IsFalse(fFC.TaskFailed, 'TaskFailed should be False initially');
  Assert.IsFalse(fFC.CircuitBreaked, 'CircuitBreaked should be False initially');
  Assert.AreEqual(0, fFC.NumRetries, 'NumRetries should be 0 initially');
  Assert.AreEqual(0, fFC.MaxRetries, 'MaxRetries should be 0 initially');
  Assert.IsNull(fFC.LastException, 'LastException should be nil initially');
end;

{ SuccessExecution }

procedure TQuickFaultControlTests.Test_SuccessExecution_ClearsTaskFailed;
begin
  fFC.MaxRetries := 3;
  // Simulate a failure (but don't let it raise — we swallow with try/except)
  try
    fFC.FailedExecution(Exception.Create('oops'));
  except
  end;
  fFC.SuccessExecution;
  Assert.IsFalse(fFC.TaskFailed, 'SuccessExecution must clear TaskFailed');
end;

{ FailedExecution with MaxRetries=0 raises immediately }

procedure TQuickFaultControlTests.Test_FailedExecution_MaxRetriesZero_Raises;
begin
  fFC.MaxRetries := 0;
  Assert.WillRaise(
    procedure begin fFC.FailedExecution(Exception.Create('fail')); end,
    Exception,
    'FailedExecution with MaxRetries=0 must raise');
end;

{ FailedExecution with MaxRetries>0 sets TaskFailed }

procedure TQuickFaultControlTests.Test_FailedExecution_SetsTaskFailed;
begin
  fFC.MaxRetries := 3;
  try
    fFC.FailedExecution(Exception.Create('fail'));
  except
  end;
  Assert.IsTrue(fFC.TaskFailed, 'FailedExecution must set TaskFailed');
  Assert.IsNotNull(fFC.LastException, 'FailedExecution must store LastException');
end;

{ NeedToRetry — MaxRetries=0, no failure }

procedure TQuickFaultControlTests.Test_NeedToRetry_MaxRetriesZero_ReturnsFalse;
begin
  fFC.MaxRetries := 0;
  Assert.IsFalse(fFC.NeedToRetry, 'NeedToRetry with MaxRetries=0 must return False');
end;

{ NeedToRetry — under limit }

procedure TQuickFaultControlTests.Test_NeedToRetry_UnderLimit_ReturnsTrue;
begin
  fFC.MaxRetries := 3;
  try
    fFC.FailedExecution(Exception.Create('fail'));
  except
  end;
  Assert.IsTrue(fFC.NeedToRetry, 'NeedToRetry under limit must return True');
end;

{ NeedToRetry — increments counter }

procedure TQuickFaultControlTests.Test_NeedToRetry_IncrementsNumRetries;
begin
  fFC.MaxRetries := 5;
  try
    fFC.FailedExecution(Exception.Create('fail'));
  except
  end;
  fFC.NeedToRetry;
  Assert.AreEqual(1, fFC.NumRetries, 'NumRetries must be 1 after first NeedToRetry');
end;

{ NeedToRetry — circuit break at limit }

procedure TQuickFaultControlTests.Test_NeedToRetry_AtMaxRetries_CircuitBreaks;
var
  i: Integer;
begin
  fFC.MaxRetries := 2;
  try
    fFC.FailedExecution(Exception.Create('fail'));
  except
  end;
  // Consume the two allowed retries
  for i := 1 to 2 do fFC.NeedToRetry;
  // Third call should circuit-break and raise
  Assert.WillRaise(
    procedure begin fFC.NeedToRetry; end,
    Exception,
    'NeedToRetry beyond MaxRetries must raise and circuit-break');
  Assert.IsTrue(fFC.CircuitBreaked, 'CircuitBreaked must be True after limit');
end;

{ NeedToRetry — not failed }

procedure TQuickFaultControlTests.Test_NeedToRetry_NotFailed_ReturnsFalse;
begin
  fFC.MaxRetries := 5;
  // No FailedExecution called — TaskFailed is False
  Assert.IsFalse(fFC.NeedToRetry, 'NeedToRetry without prior failure must return False');
end;

{ Reset }

procedure TQuickFaultControlTests.Test_Reset_RestoresState;
begin
  fFC.MaxRetries := 3;
  try
    fFC.FailedExecution(Exception.Create('fail'));
  except
  end;
  fFC.NeedToRetry;
  fFC.Reset;
  Assert.IsFalse(fFC.TaskFailed, 'Reset must clear TaskFailed');
  Assert.IsFalse(fFC.CircuitBreaked, 'Reset must clear CircuitBreaked');
  Assert.AreEqual(0, fFC.NumRetries, 'Reset must reset NumRetries to 0');
end;

{ WaitTimeMultiplierFactor = 0 raises }

procedure TQuickFaultControlTests.Test_WaitTimeMultiplierFactor_Zero_Raises;
begin
  Assert.WillRaise(
    procedure begin fFC.WaitTimeMultiplierFactor := 0; end,
    EFaultControlConfigError,
    'Setting WaitTimeMultiplierFactor to 0 must raise EFaultControlConfigError');
end;

{ WaitTimeMultiplierFactor valid value }

procedure TQuickFaultControlTests.Test_WaitTimeMultiplierFactor_ValidValue;
begin
  Assert.WillNotRaise(
    procedure begin fFC.WaitTimeMultiplierFactor := 1.5; end,
    nil,
    'Setting WaitTimeMultiplierFactor to a valid value must not raise');
  Assert.AreEqual(Double(1.5), Double(fFC.WaitTimeMultiplierFactor), Double(0.001),
    'WaitTimeMultiplierFactor must store the assigned value');
end;

{ OnRetry callback }

procedure TQuickFaultControlTests.Test_OnRetry_CalledOnRetry;
var
  helper: TFCCallbackHelper;
begin
  helper := TFCCallbackHelper.Create;
  try
    helper.Called := False;
    fFC.MaxRetries := 3;
    fFC.OnRetry := helper.OnRetry;
    try
      fFC.FailedExecution(Exception.Create('fail'));
    except
    end;
    fFC.NeedToRetry;
    Assert.IsTrue(helper.Called, 'OnRetry must be called when a retry is needed');
  finally
    helper.Free;
  end;
end;

{ OnRetry can cancel retries }

procedure TQuickFaultControlTests.Test_OnRetry_CanCancelRetries;
var
  helper: TFCCallbackHelper;
begin
  helper := TFCCallbackHelper.Create;
  try
    helper.Called := False;
    fFC.MaxRetries := 5;
    fFC.OnRetry := helper.OnRetryCancel;
    try
      fFC.FailedExecution(Exception.Create('fail'));
    except
    end;
    // NeedToRetry sets fCircuitBreaked=True via callback, then raises the stored exception
    Assert.WillRaise(
      procedure begin fFC.NeedToRetry; end,
      Exception,
      'NeedToRetry with cancel callback must raise the stored exception');
    Assert.IsTrue(helper.Called, 'OnRetry callback must have been invoked');
  finally
    helper.Free;
  end;
end;

{ OnCircuitBreak }

procedure TQuickFaultControlTests.Test_OnCircuitBreak_CalledWhenBreaking;
var
  helper: TFCCallbackHelper;
begin
  helper := TFCCallbackHelper.Create;
  try
    helper.Called := False;
    fFC.MaxRetries := 1;
    fFC.OnCircuitBreak := helper.OnCircuitBreak;
    try
      fFC.FailedExecution(Exception.Create('fail'));
    except
    end;
    fFC.NeedToRetry; // consume the 1 allowed retry
    try
      fFC.NeedToRetry; // this should circuit-break
    except
    end;
    Assert.IsTrue(helper.Called, 'OnCircuitBreak must be called when circuit breaks');
  finally
    helper.Free;
  end;
end;

{ WaitTimeMSArray }

procedure TQuickFaultControlTests.Test_WaitTimeMSArray_Accepted;
begin
  fFC.WaitTimeMSArray := [10, 20, 30];
  Assert.AreEqual(3, Integer(Length(fFC.WaitTimeMSArray)),
    'WaitTimeMSArray must store all elements');
  Assert.AreEqual(10, fFC.WaitTimeMSArray[0], 'First element must be 10');
  Assert.AreEqual(30, fFC.WaitTimeMSArray[2], 'Third element must be 30');
end;

{ MaxRetries = -1 }

procedure TQuickFaultControlTests.Test_MaxRetriesMinusOne_RetriesForever_UntilCancelled;
var
  helper: TFCCallbackHelper;
begin
  helper := TFCCallbackHelper.Create;
  try
    helper.RetryCount := 0;
    fFC.MaxRetries := -1;
    fFC.OnRetry := helper.OnRetryCountLimit;
    try
      fFC.FailedExecution(Exception.Create('fail'));
    except
    end;
    // Keep retrying; callback cancels after 5 retries, which raises the stored exception
    try
      while fFC.NeedToRetry do ;
    except
      on Exception do ; // expected: circuit-break raises on cancellation
    end;
    Assert.AreEqual(5, helper.RetryCount,
      'MaxRetries=-1 must keep retrying until callback cancels');
  finally
    helper.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickFaultControlTests);

end.
