{ ***************************************************************************

  Copyright (c) 2016-2026 Kike Pérez

  Unit        : Quick.SysInfo.Tests
  Description : Unit Tests for Quick.SysInfo
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 27/02/2026
  Modified    : 27/02/2026

 *************************************************************************** }

unit Quick.SysInfo.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Quick.SysInfo;

type
  [TestFixture]
  TQuickSysInfoTests = class(TObject)
  public
    // SystemInfo is a global var populated at unit initialization.
    // Tests just verify that fields were populated sensibly.

    [Test]
    procedure Test_AppName_NotEmpty;
    [Test]
    procedure Test_AppPath_NotEmpty;
    [Test]
    procedure Test_AppPath_IsDirectory;
    [Test]
    procedure Test_HostName_NotEmpty;
    [Test]
    procedure Test_UserName_NotEmpty;
    [Test]
    procedure Test_OsVersion_NotEmpty;
    [Test]
    procedure Test_CPUCores_GreaterThanZero;
    {$IFDEF MSWINDOWS}
    [Test]
    procedure Test_ProcessId_GreaterThanZero;
    {$ENDIF}
    [Test]
    procedure Test_GetInfo_CanBeCalledAgain;
    [Test]
    procedure Test_AppVersion_IsDefined;
  end;

implementation

{ TQuickSysInfoTests }

procedure TQuickSysInfoTests.Test_AppName_NotEmpty;
begin
  Assert.IsNotEmpty(SystemInfo.AppName,
    'AppName must not be empty after GetInfo');
end;

procedure TQuickSysInfoTests.Test_AppPath_NotEmpty;
begin
  Assert.IsNotEmpty(SystemInfo.AppPath,
    'AppPath must not be empty after GetInfo');
end;

procedure TQuickSysInfoTests.Test_AppPath_IsDirectory;
begin
  // AppPath should end with a path delimiter or be a valid directory segment
  Assert.IsTrue(
    DirectoryExists(SystemInfo.AppPath) or
    (SystemInfo.AppPath[High(SystemInfo.AppPath)] = PathDelim),
    'AppPath must refer to a valid directory');
end;

procedure TQuickSysInfoTests.Test_HostName_NotEmpty;
begin
  Assert.IsNotEmpty(SystemInfo.HostName,
    'HostName must not be empty after GetInfo');
end;

procedure TQuickSysInfoTests.Test_UserName_NotEmpty;
begin
  Assert.IsNotEmpty(SystemInfo.UserName,
    'UserName must not be empty after GetInfo');
end;

procedure TQuickSysInfoTests.Test_OsVersion_NotEmpty;
begin
  Assert.IsNotEmpty(SystemInfo.OsVersion,
    'OsVersion must not be empty after GetInfo');
end;

procedure TQuickSysInfoTests.Test_CPUCores_GreaterThanZero;
begin
  Assert.IsTrue(SystemInfo.CPUCores > 0,
    'CPUCores must be at least 1');
end;

{$IFDEF MSWINDOWS}
procedure TQuickSysInfoTests.Test_ProcessId_GreaterThanZero;
begin
  Assert.IsTrue(SystemInfo.ProcessId > 0,
    'ProcessId must be greater than 0 on Windows');
end;
{$ENDIF}

procedure TQuickSysInfoTests.Test_GetInfo_CanBeCalledAgain;
var
  info: TSystemInfo;
begin
  Assert.WillNotRaise(
    procedure begin info.GetInfo; end,
    nil,
    'Calling GetInfo on a local TSystemInfo must not raise');
  Assert.IsNotEmpty(info.AppName,
    'Re-calling GetInfo must still populate AppName');
end;

procedure TQuickSysInfoTests.Test_AppVersion_IsDefined;
begin
  // AppVersion may be empty if no version resource exists in the test binary,
  // but the call itself must not raise and the property must be accessible.
  Assert.WillNotRaise(
    procedure
    var s: string;
    begin
      s := SystemInfo.AppVersion;
      // suppress "variable assigned but not used" hint
      if s = '' then ;
    end,
    nil,
    'Accessing AppVersion must not raise');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickSysInfoTests);

end.
