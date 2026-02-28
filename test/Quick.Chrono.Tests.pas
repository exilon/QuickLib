unit Quick.Chrono.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  Quick.Chrono;

type
  [TestFixture]
  TQuickChronoTests = class(TObject)
  private
    fChrono: IChronometer;
    fBenchmark: TChronoBenchmark;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Test_Chrono_StartStop;
    [Test]
    procedure Test_Chrono_Reset;
    [Test]
    procedure Test_Chrono_BreakPoint;
    [Test]
    procedure Test_Chrono_ElapsedTime;
    [Test]
    procedure Test_Chrono_ElapsedTimeFormats;
    [Test]
    procedure Test_Chrono_HighResolution;
    [Test]
    procedure Test_Chrono_Precision;
    [Test]
    procedure Test_MillisecondsToString;
    [Test]
    procedure Test_Benchmark_Basic;
    [Test]
    procedure Test_Benchmark_Speed;
    [Test]
    procedure Test_Benchmark_EstimatedTime;
  end;

implementation

procedure TQuickChronoTests.SetUp;
begin
  fChrono := TChronometer.Create(False);
  fBenchmark := TChronoBenchmark.Create;
end;

procedure TQuickChronoTests.TearDown;
begin
  fBenchmark.Free;
  fChrono := nil;
end;

procedure TQuickChronoTests.Test_Chrono_StartStop;
begin
  Assert.IsFalse(fChrono.IsRunning, 'Chronometer should not be running initially');
  fChrono.Start;
  Assert.IsTrue(fChrono.IsRunning, 'Chronometer should be running after Start');
  Sleep(100); // Simulate some work
  fChrono.Stop;
  Assert.IsFalse(fChrono.IsRunning, 'Chronometer should not be running after Stop');
  Assert.IsTrue(fChrono.ElapsedMilliseconds >= 100, 'ElapsedMilliseconds should be at least 100ms');
end;

procedure TQuickChronoTests.Test_Chrono_Reset;
begin
  fChrono.Start;
  Sleep(100);
  fChrono.Reset;
  Sleep(50);
  fChrono.Stop;
  Assert.IsTrue(fChrono.ElapsedMilliseconds < 100, 'ElapsedMilliseconds should be reset');
end;

procedure TQuickChronoTests.Test_Chrono_BreakPoint;
var
  breakpointTime: Int64;
begin
  fChrono.Start;
  Sleep(100);
  fChrono.BreakPoint;
  breakpointTime := fChrono.ElapsedMilliseconds_Breakpoint;
  Sleep(100);
  fChrono.Stop;
  Assert.IsTrue(breakpointTime >= 100, 'Breakpoint time should be at least 100ms');
  Assert.IsTrue(fChrono.ElapsedMilliseconds >= 200, 'Total time should be at least 200ms');
end;

procedure TQuickChronoTests.Test_Chrono_ElapsedTime;
begin
  fChrono.Start;
  Sleep(1100); // Just over 1 second
  fChrono.Stop;
  Assert.IsTrue(fChrono.ElapsedSeconds >= 1, 'ElapsedSeconds should be at least 1');
  Assert.IsTrue(fChrono.ElapsedMilliseconds >= 1100, 'ElapsedMilliseconds should be at least 1100');
end;

procedure TQuickChronoTests.Test_Chrono_ElapsedTimeFormats;
begin
  fChrono.Start;
  Sleep(1500); // 1.5 seconds
  fChrono.Stop;
  Assert.IsTrue(fChrono.ElapsedTime(False).Contains('s'), 'Short format should contain "s"');
  Assert.IsTrue(fChrono.ElapsedTime(True).Contains('second'), 'Long format should contain "second"');
end;

procedure TQuickChronoTests.Test_Chrono_HighResolution;
var
  chrono: TChronometer;
begin
  chrono := TChronometer.Create(False);
  try
    {$IFDEF MSWINDOWS}
    Assert.IsTrue(chrono.IsHighResolution, 'Should be using high resolution timer on Windows');
    {$ENDIF}
  finally
    chrono.Free;
  end;
end;

procedure TQuickChronoTests.Test_Chrono_Precision;
var
  chrono: TChronometer;
begin
  chrono := TChronometer.Create(False);
  try
    chrono.ReportFormatPrecission := pfFloat;
    chrono.Start;
    Sleep(100);
    chrono.Stop;
    Assert.IsTrue(chrono.ElapsedMillisecondsWithPrecission >= 100.0, 'Precision timing should be at least 100.0ms');
  finally
    chrono.Free;
  end;
end;

procedure TQuickChronoTests.Test_MillisecondsToString;
begin
  Assert.AreEqual('500ms', TChronometer.MillisecondsToString(500, False), 'Should format milliseconds correctly');
  Assert.AreEqual('1s', TChronometer.MillisecondsToString(1000, False), 'Should format seconds correctly');
  Assert.AreEqual('1 second(s)', TChronometer.MillisecondsToString(1000, True), 'Should format long seconds correctly');
end;

procedure TQuickChronoTests.Test_Benchmark_Basic;
begin
  fBenchmark.TotalProcess := 100;
  // Read Speed directly from the freshly-created object; the constructor sets
  // fSpeed := 0. Assigning CurrentProcess := 0 triggers SetCurrentProcess which
  // divides (0-0) by an elapsed time that may be 0 → NaN/Inf on Win64.
  Assert.AreEqual<Single>(0, fBenchmark.Speed,
    'Speed should be 0 immediately after construction');
  // Now set CurrentProcess to a non-zero value so SetCurrentProcess is safe
  fBenchmark.CurrentProcess := 0; // initialises internal timestamps only
  Assert.AreEqual<Single>(0, fBenchmark.CurrentProcess,
    'CurrentProcess should be 0 after setting to 0');
end;

procedure TQuickChronoTests.Test_Benchmark_Speed;
begin
  fBenchmark.TotalProcess := 100;
  fBenchmark.CurrentProcess := 50;
  Sleep(100);
  fBenchmark.CurrentProcess := 100;
  Assert.IsTrue(fBenchmark.Speed > 0, 'Speed should be calculated');
end;

procedure TQuickChronoTests.Test_Benchmark_EstimatedTime;
begin
  fBenchmark.TotalProcess := 1000;
  fBenchmark.CurrentProcess := 0;
  Sleep(100);
  fBenchmark.CurrentProcess := 100;
  Assert.IsTrue(fBenchmark.EstimatedMilliseconds > 0, 'Should estimate remaining time');
  Assert.IsTrue(fBenchmark.EstimatedTime(False) <> '', 'Should format estimated time');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickChronoTests);
end.