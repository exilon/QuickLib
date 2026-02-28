{ ***************************************************************************

  Copyright (c) 2016-2026 Kike Pérez

  Unit        : Quick.Console.Tests
  Description : Unit Tests for Quick.Console
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 27/02/2026
  Modified    : 27/02/2026

 *************************************************************************** }

unit Quick.Console.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Quick.Commons,
  Quick.Console;

type
  [TestFixture]
  TQuickConsoleTests = class(TObject)
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    { cout — smoke tests: must not raise }
    [Test]
    procedure Test_cout_Integer_NoRaise;
    [Test]
    procedure Test_cout_Double_NoRaise;
    [Test]
    procedure Test_cout_String_NoRaise;
    [Test]
    procedure Test_cout_StringWithColor_NoRaise;
    [Test]
    procedure Test_cout_Format_NoRaise;
    [Test]
    procedure Test_coutSL_NoRaise;
    [Test]
    procedure Test_coutXY_NoRaise;
    [Test]
    procedure Test_coutTL_NoRaise;
    [Test]
    procedure Test_coutBL_NoRaise;
    [Test]
    procedure Test_coutFmt_NoRaise;

    { Color / cursor control }
    [Test]
    procedure Test_TextColor_NoRaise;
    [Test]
    procedure Test_TextBackground_NoRaise;
    [Test]
    procedure Test_ResetColors_NoRaise;
    [Test]
    procedure Test_SetCursorPos_NoRaise;
    [Test]
    procedure Test_ShowHideCursor_NoRaise;
    [Test]
    procedure Test_GetCursorMaxBottom_NonNegative;
    [Test]
    procedure Test_ClearScreen_NoRaise;
    [Test]
    procedure Test_ClearLine_NoRaise;
    [Test]
    procedure Test_ClearLineAtY_NoRaise;

    {$IFDEF MSWINDOWS}
    [Test]
    procedure Test_GetCursorX_NonNegative;
    [Test]
    procedure Test_GetCursorY_NonNegative;
    [Test]
    procedure Test_ProcessMessages_NoRaise;
    [Test]
    procedure Test_ConsoleMenu_CreateAddMenu_NoRaise;
    [Test]
    procedure Test_RunConsoleCommand_Echo_ExitZero;
    {$ENDIF}

    {$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
    [Test]
    procedure Test_SaveRestoreCursor_NoRaise;
    [Test]
    procedure Test_CursorOnOff_NoRaise;
    {$ENDIF}
  end;

implementation

procedure TQuickConsoleTests.SetUp;
begin
  Console.LogVerbose := LOG_ALL;
  ResetColors;
end;

procedure TQuickConsoleTests.TearDown;
begin
  ResetColors;
end;

{ --- cout --- }

procedure TQuickConsoleTests.Test_cout_Integer_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin cout(42, etInfo); end,
    nil, 'cout(Integer) must not raise');
end;

procedure TQuickConsoleTests.Test_cout_Double_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin cout(3.14, etInfo); end,
    nil, 'cout(Double) must not raise');
end;

procedure TQuickConsoleTests.Test_cout_String_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin cout('hello', etInfo); end,
    nil, 'cout(string) must not raise');
end;

procedure TQuickConsoleTests.Test_cout_StringWithColor_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin cout('colored', ccGreen); end,
    nil, 'cout(string, TConsoleColor) must not raise');
end;

procedure TQuickConsoleTests.Test_cout_Format_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin cout('value: %d', [99], etInfo); end,
    nil, 'cout(format) must not raise');
end;

procedure TQuickConsoleTests.Test_coutSL_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin coutSL('same line', ccWhite); end,
    nil, 'coutSL must not raise');
end;

procedure TQuickConsoleTests.Test_coutXY_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin coutXY(1, 1, 'pos', etInfo); end,
    nil, 'coutXY must not raise');
end;

procedure TQuickConsoleTests.Test_coutTL_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin coutTL('top', etInfo); end,
    nil, 'coutTL must not raise');
end;

procedure TQuickConsoleTests.Test_coutBL_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin coutBL('bottom', etInfo); end,
    nil, 'coutBL must not raise');
end;

procedure TQuickConsoleTests.Test_coutFmt_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin coutFmt('item %s', ['x'], etInfo); end,
    nil, 'coutFmt must not raise');
end;

{ --- Color / cursor --- }

procedure TQuickConsoleTests.Test_TextColor_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TextColor(ccRed); end,
    nil, 'TextColor must not raise');
end;

procedure TQuickConsoleTests.Test_TextBackground_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin TextBackground(ccBlue); end,
    nil, 'TextBackground must not raise');
end;

procedure TQuickConsoleTests.Test_ResetColors_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin ResetColors; end,
    nil, 'ResetColors must not raise');
end;

procedure TQuickConsoleTests.Test_SetCursorPos_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin SetCursorPos(0, 0); end,
    nil, 'SetCursorPos must not raise');
end;

procedure TQuickConsoleTests.Test_ShowHideCursor_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin ShowCursor; HideCursor; end,
    nil, 'ShowCursor / HideCursor must not raise');
end;

procedure TQuickConsoleTests.Test_GetCursorMaxBottom_NonNegative;
begin
  Assert.IsTrue(GetCursorMaxBottom >= 0,
    'GetCursorMaxBottom must return a non-negative value');
end;

procedure TQuickConsoleTests.Test_ClearScreen_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin ClearScreen; end,
    nil, 'ClearScreen must not raise');
end;

procedure TQuickConsoleTests.Test_ClearLine_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin ClearLine; end,
    nil, 'ClearLine must not raise');
end;

procedure TQuickConsoleTests.Test_ClearLineAtY_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin ClearLine(0); end,
    nil, 'ClearLine(Y) must not raise');
end;

{ --- Windows-only --- }

{$IFDEF MSWINDOWS}
procedure TQuickConsoleTests.Test_GetCursorX_NonNegative;
begin
  Assert.IsTrue(GetCursorX >= 0, 'GetCursorX must return a non-negative value');
end;

procedure TQuickConsoleTests.Test_GetCursorY_NonNegative;
begin
  Assert.IsTrue(GetCursorY >= 0, 'GetCursorY must return a non-negative value');
end;

procedure TQuickConsoleTests.Test_ProcessMessages_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin ProcessMessages; end,
    nil, 'ProcessMessages must not raise');
end;

procedure TQuickConsoleTests.Test_ConsoleMenu_CreateAddMenu_NoRaise;
var
  menu: TConsoleMenu;
begin
  Assert.WillNotRaise(
    procedure
    begin
      menu := TConsoleMenu.Create;
      try
        menu.MenuColor := ccWhite;
        menu.AddMenu('Option 1', Word($70) {VK_F1}, nil);
      finally
        menu.Free;
      end;
    end,
    nil, 'TConsoleMenu create and AddMenu must not raise');
end;

procedure TQuickConsoleTests.Test_RunConsoleCommand_Echo_ExitZero;
var
  exitCode: Cardinal;
begin
  exitCode := RunConsoleCommand('cmd.exe', '/c echo test');
  Assert.AreEqual(Cardinal(0), exitCode,
    'RunConsoleCommand for "echo test" must exit with code 0');
end;
{$ENDIF}

{ --- Linux / macOS --- }

{$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
procedure TQuickConsoleTests.Test_SaveRestoreCursor_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin SaveCursor; RestoreCursor; end,
    nil, 'SaveCursor/RestoreCursor must not raise');
end;

procedure TQuickConsoleTests.Test_CursorOnOff_NoRaise;
begin
  Assert.WillNotRaise(
    procedure begin CursorOn; CursorOff; end,
    nil, 'CursorOn/CursorOff must not raise');
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TQuickConsoleTests);

end.