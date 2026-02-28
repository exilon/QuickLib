program QuickLib;
{
  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options 
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.
}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitX.Loggers.Console,
  DUnitX.Loggers.XML.NUnit,
  DUnitX.TestFramework,
  System.SysUtils,
  Quick.RegEx.Utils in 'Quick.RegEx.Utils.pas',
  Quick.Arrays.Helper in 'Quick.Arrays.Helper.pas',
  Quick.Commons in 'Quick.Commons.pas',
  Quick.Console in 'Quick.Console.pas',
  Quick.Chrono in 'Quick.Chrono.pas',
  Quick.Format in 'Quick.Format.pas',
  Quick.Value in 'Quick.Value.pas',
  Quick.Value.RTTI in 'Quick.Value.RTTI.pas',
  Quick.Options in 'Quick.Options.pas',
  Quick.Options.Serializer.Json in 'Quick.Options.Serializer.Json.pas',
  Quick.Options.Serializer.Yaml in 'Quick.Options.Serializer.Yaml.pas',
  Quick.HttpClient in 'Quick.HttpClient.pas',
  Quick.Threads in 'Quick.Threads.pas',
  Quick.Lists in 'Quick.Lists.pas',
  Quick.Linq in 'Quick.Linq.pas',
  Quick.Expression in 'Quick.Expression.pas',
  Quick.Template in 'Quick.Template.pas',
  Quick.Debug.Utils in 'Quick.Debug.Utils.pas',
  {$IFDEF MSWINDOWS}
  Quick.Compression.LZO in 'Quick.Compression.LZO.pas',
  {$ENDIF}
  Quick.YAML in 'Quick.YAML.pas',
  Quick.YAML.Serializer in 'Quick.YAML.Serializer.pas',
  //Quick.Crypto in 'Quick.Crypto.pas',
  Quick.Compression in 'Quick.Compression.pas',
  Quick.Conditions in 'Quick.Conditions.pas',
  Quick.RTTI.Utils in 'Quick.RTTI.Utils.pas',
  Quick.FaultControl in 'Quick.FaultControl.pas',
  Quick.SysInfo in 'Quick.SysInfo.pas',
  Quick.Collections.Tests in 'Quick.Collections.Tests.pas',
  Quick.AutoMapper.Tests in 'Quick.AutoMapper.Tests.pas',
  Quick.IOC.Tests in 'Quick.IOC.Tests.pas',
  //Quick.Crypto.Tests in 'Quick.Crypto.Tests.pas',
  Quick.Compression.Tests in 'Quick.Compression.Tests.pas',
  Quick.Commons.Tests in 'Quick.Commons.Tests.pas',
  Quick.Chrono.Tests in 'Quick.Chrono.Tests.pas',
  Quick.Base64.Tests in 'Quick.Base64.Tests.pas',
  Quick.Arrays.Tests in 'Quick.Arrays.Tests.pas',
  Quick.Arrays.Helper.Tests in 'Quick.Arrays.Helper.Tests.pas',
  Quick.Expression.Tests in 'Quick.Expression.Tests.pas',
  Quick.Debug.Utils.Tests in 'Quick.Debug.Utils.Tests.pas',
  Quick.YAML.Tests in 'Quick.YAML.Tests.pas',
  Quick.YAML.Serializer.Tests in 'Quick.YAML.Serializer.Tests.pas',
  Quick.RegEx.Utils.Tests in 'Quick.RegEx.Utils.Tests.pas',
  Quick.Value.Tests in 'Quick.Value.Tests.pas',
  Quick.Linq.Tests in 'Quick.Linq.Tests.pas',
  Quick.Options.Tests in 'Quick.Options.Tests.pas',
  Quick.Template.Tests in 'Quick.Template.Tests.pas',
  Quick.Conditions.Tests in 'Quick.Conditions.Tests.pas',
  Quick.RTTI.Utils.Tests in 'Quick.RTTI.Utils.Tests.pas',
  Quick.Format.Tests in 'Quick.Format.Tests.pas',
  Quick.FaultControl.Tests in 'Quick.FaultControl.Tests.pas',
  Quick.Lists.Tests in 'Quick.Lists.Tests.pas',
  Quick.SysInfo.Tests in 'Quick.SysInfo.Tests.pas',
  Quick.Threads.Tests in 'Quick.Threads.Tests.pas',
  Quick.Console.Tests in 'Quick.Console.Tests.pas',
  Quick.Log in 'Quick.Log.pas',
  Quick.Log.Tests in 'Quick.Log.Tests.pas',
  Quick.Files in 'Quick.Files.pas',
  Quick.Files.Tests in 'Quick.Files.Tests.pas',
  Quick.Streams in 'Quick.Streams.pas',
  Quick.Streams.Tests in 'Quick.Streams.Tests.pas',
  Quick.Url.Utils in 'Quick.Url.Utils.pas',
  Quick.Url.Utils.Tests in 'Quick.Url.Utils.Tests.pas',
  Quick.JSON.Utils in 'Quick.JSON.Utils.pas',
  Quick.JSON.Utils.Tests in 'Quick.JSON.Utils.Tests.pas',
  Quick.JSONRecord in 'Quick.JSONRecord.pas',
  Quick.JSONRecord.Tests in 'Quick.JSONRecord.Tests.pas',
  Quick.Json.Serializer in 'Quick.Json.Serializer.pas',
  Quick.Json.Serializer.Tests in 'Quick.Json.Serializer.Tests.pas',
  Quick.MemoryCache in 'Quick.MemoryCache.pas',
  Quick.MemoryCache.Types in 'Quick.MemoryCache.Types.pas',
  Quick.MemoryCache.Serializer.Json in 'Quick.MemoryCache.Serializer.Json.pas',
  Quick.MemoryCache.Compressor.GZip in 'Quick.MemoryCache.Compressor.GZip.pas',
  Quick.MemoryCache.Tests in 'Quick.MemoryCache.Tests.pas',
  Quick.Pooling in 'Quick.Pooling.pas',
  Quick.Pooling.Tests in 'Quick.Pooling.Tests.pas';

{$R *.RES}

var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;

begin
  runner := TDUnitX.CreateRunner;

  runner.UseRTTI := True;

  runner.FailsOnNoAsserts := False;

  logger := TDUnitXConsoleLogger.Create;
  runner.AddLogger(logger);

  // runner.AddLogger(TNUnitXmlLogger.Create);

  results := runner.Execute;

  if not results.AllPassed then
    ExitCode := 1;

  {$IFDEF DEBUG}
  System.Write('Test finished. Press <Intro> to exit...');
  System.Readln;
  {$ENDIF}
end.

