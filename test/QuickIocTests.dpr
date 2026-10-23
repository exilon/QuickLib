program QuickIocTests;

{ Runs only the Quick.IOC test suite (Quick.IOC.Tests.pas), as a DUnitX console application.
  Open QuickIocTests.dproj in RAD Studio, build and run; the exit code is 0 when every test passes. }

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.TestFramework,
  Quick.IOC.Tests in 'Quick.IOC.Tests.pas';

var
  runner : ITestRunner;
  results : IRunResults;

begin
  try
    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;
    runner.FailsOnNoAsserts := False;
    runner.AddLogger(TDUnitXConsoleLogger.Create(False));
    results := runner.Execute;
    if not results.AllPassed then ExitCode := 1;
  except
    on E : Exception do
    begin
      Writeln(E.ClassName,': ',E.Message);
      ExitCode := 2;
    end;
  end;
end.
