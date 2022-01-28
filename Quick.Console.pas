{ ***************************************************************************

  Copyright (c) 2016-2021 Kike Pérez

  Unit        : Quick.Console
  Description : Console output with colors and optional file log
  Author      : Kike Pérez
  Version     : 1.9
  Created     : 10/05/2017
  Modified    : 05/08/2021

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

unit Quick.Console;

{$i QuickLib.inc}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$ifndef VER140}
    {$ifndef LINUX}
      {$define WITHUXTHEME}
    {$endif}
  {$endif}
  {$IFDEF DELPHI2005_UP}
    {$DEFINE INLINES}
  {$ENDIF}
  {$IF RTLVersion >= 14.0}
    {$DEFINE HASERROUTPUT}
  {$ENDIF}
{$ENDIF}

interface

uses
  Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  Messages,
  {$ELSE}
    {$IFDEF FPC}
    crt,
    {$ENDIF}
  {$ENDIF}
  {$IF Defined(DELPHILINUX) OR Defined(MACOS)}
  Quick.SyncObjs.Linux.Compatibility,
  Posix.StdDef,
  {$ENDIF}
  SysUtils,
  Quick.Commons,
  Quick.Log;

type

  //text colors
  {$IFNDEF DELPHILINUX}
  TConsoleColor = (
  ccBlack        = 0,
  ccBlue         = 1,
  ccGreen        = 2,
  ccCyan         = 3,
  ccRed          = 4,
  ccMagenta      = 5,
  ccBrown        = 6,
  ccLightGray    = 7,
  ccDarkGray     = 8,
  ccLightBlue    = 9,
  ccLightGreen   = 10,
  ccLightCyan    = 11,
  ccLightRed     = 12,
  ccLightMagenta = 13,
  ccYellow       = 14,
  ccWhite        = 15);
  {$ELSE}
  TConsoleColor = (
  ccDarkGray     = 90,
  ccLightRed     = 91,
  ccLightGreen   = 92,
  ccYellow       = 93,
  ccLightBlue    = 94,
  ccLightMagenta = 95,
  ccLightCyan    = 96,
  ccWhite        = 97,
  ccBlack        = 30,
  ccRed          = 31,
  ccGreen        = 32,
  ccBrown        = 33,
  ccBlue         = 34,
  ccMagenta      = 35,
  ccCyan         = 36,
  ccLightGray    = 37);
  {$ENDIF}

  TConsoleProperties = record
    LogVerbose : TLogVerbose;
    Log : TQuickLog;
  end;

  {$IFNDEF FPC}
  TOutputProc<T> = reference to procedure(const aLine : T);
  TExecuteProc = reference to procedure;
  {$ELSE}
  TOutputProc<T> = procedure(const aLine : T) of object;
  TExecuteProc = procedure of object;
  {$ENDIF}
  {$IF DEFINED(FPCLINUX) OR DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
    {$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
    tcrtcoord = Byte;
    {$ENDIF}
  TCoord = record
    X : tcrtcoord;
    Y : tcrtcoord;
  end;

  TSmallRect = record
    Left : Byte;
    Top : Byte;
    Right : Byte;
    Bottom : Byte;
  end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  TConsoleMenuOption = record
  private
    fCaption : string;
    fKey : Word;
    fOnKeyPressed : TExecuteProc;
  public
    property Caption : string read fCaption write fCaption;
    property Key : Word read fKey write fKey;
    property OnKeyPressed : TExecuteProc read fOnKeyPressed write fOnKeyPressed;
    procedure DoKeyPressed;
  end;

  TConsoleMenu = class
  private
    fConsoleMenu : array of TConsoleMenuOption;
    fMenuColor : TConsoleColor;
    fIsActive : Boolean;
    procedure WriteMenu;
  public
    constructor Create;
    property MenuColor : TConsoleColor read fMenuColor write fMenuColor;
    property IsActive : Boolean read fIsActive;
    procedure AddMenu(const cMenuCaption : string; const cMenuKey : Word; MenuAction : TExecuteProc); overload;
    procedure AddMenu(MenuOption : TConsoleMenuOption); overload;
    procedure Refresh(aClearScreen : Boolean = False);
    procedure WaitForKeys;
  end;
  {$ENDIF}

  procedure cout(const cMsg : Integer; cEventType : TLogEventType); overload;
  procedure cout(const cMsg : Double; cEventType : TLogEventType); overload;
  procedure cout(const cMsg : string; cEventType : TLogEventType); overload;
  procedure cout(const cMsg : string; cColor : TConsoleColor); overload;
  procedure coutSL(const cMsg : string; cColor : TConsoleColor);
  procedure cout(const cMsg : string; params : array of const; cEventType : TLogEventType); overload;
  procedure coutXY(x,y : Integer; const cMsg : string; cEventType : TLogEventType); overload;
  procedure coutXY(x,y : Integer; const cMsg : string; cColor : TConsoleColor); overload;
  procedure coutXY(x,y : Integer; const cMsg : string; params : array of const; cEventType : TLogEventType); overload;
  procedure coutXY(x,y : Integer; const cMsg : string; params : array of const; cColor : TConsoleColor); overload;
  procedure coutTL(const cMsg : string; cEventType : TLogEventType); overload;
  procedure coutTL(const cMsg : string; cColor : TConsoleColor); overload;
  procedure coutBL(const cMsg : string; cEventType : TLogEventType); overload;
  procedure coutBL(const cMsg : string; cColor : TConsoleColor); overload;
  procedure coutFmt(const cMsg : string; params : array of const; cEventType : TLogEventType);
  procedure TextColor(Color: TConsoleColor); overload;
  procedure TextColor(Color: Byte); overload;
  procedure TextBackground(Color: TConsoleColor); overload;
  procedure TextBackground(Color: Byte); overload;
  procedure ResetColors;
  {$IFDEF MSWINDOWS}
  procedure ConsoleResize(Width, Height : Integer);
  {$ENDIF}
  procedure ClearScreen;
  procedure ClearLine; overload;
  procedure ClearLine(Y : Integer); overload;
  procedure ShowCursor;
  procedure HideCursor;
  {$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
  procedure SaveCursor;
  procedure RestoreCursor;
  procedure CursorOn;
  procedure CursorOff;
  function ReadKey : Char;
  {$ELSE}
  function GetCursorX: Integer; {$IFDEF INLINES}inline;{$ENDIF}
  function GetCursorY: Integer; {$IFDEF INLINES}inline;{$ENDIF}
  {$ENDIF}
  function GetCursorMaxBottom : Integer;
  {$IFDEF DELPHILINUX}
  procedure GotoXY(x,y : Integer);
  {$ENDIF}
  procedure SetCursorPos(NewCoord : TCoord); overload;
  procedure SetCursorPos(x ,y : Integer); overload;
  {$IFDEF MSWINDOWS}
  procedure ProcessMessages;
  {$ENDIF}
  procedure ConsoleWaitForEnterKey;
  {$IFDEF MSWINDOWS}
  function RunConsoleCommand(const aCommand, aParameters : String; CallBack : TOutputProc<PAnsiChar> = nil; OutputLines : TStrings = nil) : Cardinal;
  procedure InitConsole;
  {$ENDIF}


var
  Console : TConsoleProperties;
  CSConsole : TRTLCriticalSection;
  LastMode : Word;
  DefConsoleColor : Byte;
  TextAttr : Byte;
  hStdOut: THandle;
  hStdErr: THandle;
  ConsoleRect: TSmallRect;
  {$IFDEF MSWINDOWS}
  ScreenBufInfo : TConsoleScreenBufferInfo;
  CursorInfo : TConsoleCursorInfo;
  {$ENDIF}

implementation


{$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
const
  AEC =chr($1B)+chr($5b);
  SAVE_CURSOR_POS = chr($1B) + '7';
  RESTORE_CURSOR_POS = chr($1B) + '8';
{$ENDIF}


procedure cout(const cMsg : Integer; cEventType : TLogEventType);
var
  FmtSets : TFormatSettings;
begin
  try
    {$IFNDEF FPC}
    FmtSets := TFormatSettings.Create;
    {$ENDIF}
    FmtSets.ThousandSeparator := '.';
    FmtSets.DecimalSeparator := ',';
    cout(FormatFloat('0,',cMsg,FmtSets),cEventType);
  except
    cout(cMsg.ToString,cEventType);
  end;
end;

procedure cout(const cMsg : Double; cEventType : TLogEventType);
var
  FmtSets : TFormatSettings;
begin
  try
    {$IFNDEF FPC}
    FmtSets := TFormatSettings.Create;
    {$ENDIF}
    FmtSets.ThousandSeparator := '.';
    FmtSets.DecimalSeparator := ',';
    cout(FormatFloat('.0###,',cMsg,FmtSets),cEventType);
  except
    cout(cMsg.ToString,cEventType);
  end;
end;

procedure cout(const cMsg : string; cEventType : TLogEventType);
begin
  if cEventType in Console.LogVerbose then
  begin
    EnterCriticalSection(CSConsole);
    try
      {$IFDEF MSWINDOWS}
      if hStdOut <> 0 then
      {$ENDIF}
      begin
        case cEventType of
          etError : TextColor(ccLightRed);
          etInfo : TextColor(ccWhite);
          etSuccess : TextColor(ccLightGreen);
          etWarning : TextColor(ccYellow);
          etDebug : TextColor(ccLightCyan);
          etTrace : TextColor(ccLightMagenta);
          etCritical : begin TextColor(ccYellow); TextBackground(ccRed); end;
          etException : TextColor(ccRed);
          else TextColor(ccWhite);
        end;
        {$I-}
        Writeln(cMsg{$IFDEF LINUX} +#13{$ENDIF});
        {$I+}
        TextColor(LastMode);
      end;
    finally
      LeaveCriticalSection(CSConsole);
    end;
    if Assigned(Console.Log) then Console.Log.Add(cMsg,cEventType);
  end;
end;

procedure cout(const cMsg : string; cColor : TConsoleColor);
begin
  EnterCriticalSection(CSConsole);
  try
    {$IFDEF MSWINDOWS}
      if hStdOut <> 0 then
    {$ENDIF}
    begin
      TextColor(cColor);
      {$I-}
      Writeln(cMsg{$IFDEF LINUX} +#13{$ENDIF});
      {$I+}
      TextColor(LastMode);
    end;
  finally
    LeaveCriticalSection(CSConsole);
  end;
end;

procedure coutSL(const cMsg : string; cColor : TConsoleColor);
begin
  EnterCriticalSection(CSConsole);
  try
    {$IFDEF MSWINDOWS}
      if hStdOut <> 0 then
    {$ENDIF}
    begin
      TextColor(cColor);
      {$I-}
      Write(cMsg);//{$IFDEF LINUX} +#13{$ENDIF});
      {$I+}
      TextColor(LastMode);
    end;
  finally
    LeaveCriticalSection(CSConsole);
  end;
end;

procedure cout(const cMsg : string; params : array of const; cEventType : TLogEventType);
begin
  cout(Format(cMsg,params),cEventType);
end;

{$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
procedure SaveCursor;
begin
  write(SAVE_CURSOR_POS);
end;

procedure RestoreCursor;
begin
  write(RESTORE_CURSOR_POS);
end;

procedure CursorOn;
begin
  //not implemented yet
end;

procedure CursorOff;
begin
  //not implemented yet
end;

function ReadKey : Char;
begin
  Read(Result);
end;

{$ELSE}
function GetCursorX: Integer; {$IFDEF INLINES}inline;{$ENDIF}
{$IFDEF MSWINDOWS}
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleSCreenBufferInfo(hStdOut, BufferInfo);
  Result := BufferInfo.dwCursorPosition.X;
end;
{$ELSE}
begin
  {$IFDEF FPC}
  Result := WhereX;
  {$ELSE}
  Result := Byte(ConData(CD_CURRX))-lo(windmin);
  {$ENDIF}
end;
{$ENDIF}

function GetCursorY: Integer; {$IFDEF INLINES}inline;{$ENDIF}
{$IFDEF MSWINDOWS}
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleSCreenBufferInfo(hStdOut, BufferInfo);
  Result := BufferInfo.dwCursorPosition.Y;
end;
{$ELSE}
begin
  Result := WhereY;
end;
{$ENDIF}
{$ENDIF DELPHILINUX}

function GetCursorMaxBottom : Integer;
{$IFDEF MSWINDOWS}
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleSCreenBufferInfo(hStdOut, BufferInfo);
  Result := BufferInfo.srWindow.Bottom;
end;
{$ELSE}
begin
  Result := 25;
end;
{$ENDIF}

{$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
procedure GotoXY(x,y : Integer);
begin
  Write(AEC, y, ';', x, 'H');
end;
{$ENDIF}

procedure SetCursorPos(NewCoord : TCoord);
begin
  {$IFDEF MSWINDOWS}
  SetConsoleCursorPosition(hStdOut, NewCoord);
  {$ELSE}
  GotoXY(NewCoord.X,NewCoord.Y);
  {$ENDIF}
end;

procedure SetCursorPos(x ,y : Integer);
var
  NewCoord : TCoord;
begin
  NewCoord.X := x;
  NewCoord.Y := y;
  SetCursorPos(NewCoord);
end;

procedure coutXY(x,y : Integer; const cMsg : string; cEventType : TLogEventType);
var
 NewCoord : TCoord;
 LastCoord : TCoord;
begin
  {$IFDEF MSWINDOWS}
  if hStdOut = 0 then Exit;
  {$ENDIF}
  {$IF NOT DEFINED(DELPHILINUX) AND NOT DEFINED(MACOS)}
  LastCoord.X := GetCursorX;
  LastCoord.Y := GetCursorY;
  {$ELSE}
  write(SAVE_CURSOR_POS);
  {$ENDIF}
  NewCoord.X := x;
  NewCoord.Y := y;
  ClearLine(Y);
  SetCursorPos(NewCoord);
  try
    cout(cMsg,cEventType);
  finally
    {$IFNDEF DELPHILINUX}
    SetCursorPos(LastCoord);
    {$ELSE}
    write(RESTORE_CURSOR_POS);
    {$ENDIF}
  end;
end;

procedure coutXY(x,y : Integer; const cMsg : string; cColor : TConsoleColor); overload;
var
 NewCoord : TCoord;
 LastCoord : TCoord;
begin
  {$IFDEF MSWINDOWS}
  if hStdOut = 0 then Exit;
  {$ENDIF}
  {$IF NOT DEFINED(DELPHILINUX) AND NOT DEFINED(MACOS)}
  LastCoord.X := GetCursorX;
  LastCoord.Y := GetCursorY;
  {$ELSE}
  write(SAVE_CURSOR_POS);
  {$ENDIF}
  NewCoord.X := x;
  NewCoord.Y := y;
  ClearLine(Y);
  SetCursorPos(NewCoord);
  try
    cout(cMsg,cColor);
  finally
    {$IFNDEF DELPHILINUX}
    SetCursorPos(LastCoord);
    {$ELSE}
    write(RESTORE_CURSOR_POS);
    {$ENDIF}
  end;
end;

procedure coutXY(x,y : Integer; const cMsg : string; params : array of const; cEventType : TLogEventType);
begin
  coutXY(x,y,Format(cMsg,params),cEventType);
end;

procedure coutXY(x,y : Integer; const cMsg : string; params : array of const; cColor : TConsoleColor);
begin
  coutXY(x,y,Format(cMsg,params),cColor);
end;

procedure coutTL(const cMsg : string; cEventType : TLogEventType);
begin
  coutXY(0,0,cMsg,cEventType);
end;

procedure coutTL(const cMsg : string; cColor : TConsoleColor);
begin
  coutXY(0,0,cMsg,cColor);
end;

procedure coutBL(const cMsg : string; cEventType : TLogEventType);
begin
  coutXY(0,GetCursorMaxBottom - 1,cMsg,cEventType);
end;

procedure coutBL(const cMsg : string; cColor : TConsoleColor);
begin
  coutXY(0,GetCursorMaxBottom - 1,cMsg,cColor);
end;

procedure coutFmt(const cMsg : string; params : array of const; cEventType : TLogEventType);
begin
  cout(Format(cMsg,params),cEventType);
end;

procedure TextColor(Color: TConsoleColor);
begin
  TextColor(Integer(Color));
end;

procedure TextColor(Color: Byte);
begin
  {$IFDEF MSWINDOWS}
  if hStdOut = 0 then Exit;
  LastMode := TextAttr;
  TextAttr := (TextAttr and $F0) or (Color and $0F);
  if TextAttr <> LastMode then SetConsoleTextAttribute(hStdOut, TextAttr);
  {$ELSE}
    {$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
    write(AEC,';',Color,'m')
    {$ELSE}
    crt.TextColor(Color);
    {$ENDIF}
  {$ENDIF}
end;

procedure TextBackground(Color: TConsoleColor);
begin
  TextBackground(Integer(Color));
end;

procedure TextBackground(Color: Byte);
begin
  {$IFDEF MSWINDOWS}
  if hStdOut = 0 then Exit;
  LastMode := TextAttr;
  TextAttr := (TextAttr and $0F) or ((Color shl 4) and $F0);
  if TextAttr <> LastMode then SetConsoleTextAttribute(hStdOut, TextAttr);
  {$ELSE}
    {$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
    write(AEC,Color,';3m');
    {$ELSE}
    crt.TextBackground(Color);
    {$ENDIF}
  {$ENDIF}
end;

procedure ResetColors;
begin
  {$IFDEF MSWINDOWS}
  SetConsoleTextAttribute(hStdOut, DefConsoleColor);
  TextAttr := DefConsoleColor;
  {$ELSE}
  {$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
    write(AEC,0,'m');
  {$ELSE}
    TextColor(ccLightGray);
    TextBackground(ccBlack);
    {$ENDIF}
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure ConsoleResize(Width, Height : Integer);
var
  Rect: TSmallRect;
  Coord: TCoord;
begin
  Rect.Left := 1;
  Rect.Top := 1;
  Rect.Right := Width;
  Rect.Bottom := Height;
  Coord.X := Rect.Right + 1 - Rect.Left;
  Coord.y := Rect.Bottom + 1 - Rect.Top;
  SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), Coord);
  SetConsoleWindowInfo(GetStdHandle(STD_OUTPUT_HANDLE), True, Rect);
end;
{$ENDIF}

procedure ClearScreen;
{$IFDEF MSWINDOWS}
var
  stdout: THandle;
  bufinfo: TConsoleScreenBufferInfo;
  ConsoleSize: DWORD;
  NumWritten: DWORD;
  Origin: TCoord;
begin
  stdout := GetStdHandle(STD_OUTPUT_HANDLE);
  if stdout<>INVALID_HANDLE_VALUE then
  begin
    GetConsoleScreenBufferInfo(stdout,bufinfo);
    ConsoleSize := bufinfo.dwSize.X * bufinfo.dwSize.Y;
    Origin.X := 0;
    Origin.Y := 0;
    FillConsoleOutputCharacter(stdout,' ',ConsoleSize,Origin,NumWritten);
    FillConsoleOutputAttribute(stdout,bufinfo.wAttributes,ConsoleSize,Origin,NumWritten);
    SetConsoleCursorPosition(stdout, Origin);
  end;
end;
{$ELSE}
begin
  {$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
  write(AEC,2,'J');
  {$ELSE}
  ClrScr;
  {$ENDIF}
end;
{$ENDIF}

procedure ClearLine;
begin
  {$IF NOT DEFINED(DELPHILINUX) AND NOT DEFINED(MACOS)}
  ClearLine(GetCursorY);
  {$ELSE}
  write(AEC,'K');
  {$ENDIF}
end;

procedure ClearLine(Y : Integer);
{$IFDEF MSWINDOWS}
var
 dwWriteCoord: TCoord;
 dwCount, dwSize: DWord;
begin
  if hStdOut = 0 then Exit;
  dwWriteCoord.X := 0;
  dwWriteCoord.Y := Y;
  dwSize := ConsoleRect.Right + 1;
  FillConsoleOutputAttribute(hStdOut, TextAttr, dwSize, dwWriteCoord, dwCount);
  FillConsoleOutputCharacter(hStdOut, ' ', dwSize, dwWriteCoord, dwCount);
end;
{$ELSE}
begin
  GotoXY(1,Y);
  {$IF DEFINED(DELPHILINUX) OR DEFINED(MACOS)}
  write(AEC,'K');
  {$ELSE}
  DelLine;
  {$ENDIF}
  GotoXY(1,Y);
end;
{$ENDIF}

procedure ShowCursor;
begin
  {$IFDEF MSWINDOWS}
  GetConsoleCursorInfo(hStdOut,CursorInfo);
  CursorInfo.bVisible := True;
  SetConsoleCursorInfo(hStdOut,CursorInfo);
  {$ELSE}
  CursorOn;
  {$ENDIF}
end;

procedure HideCursor;
begin
  {$IFDEF MSWINDOWS}
  GetConsoleCursorInfo(hStdOut,CursorInfo);
  CursorInfo.bVisible := False;
  SetConsoleCursorInfo(hStdOut,CursorInfo);
  {$ELSE}
  CursorOff;
  {$ENDIF}
end;

function ConsoleKeyPressed(ExpectedKey: Word): Boolean;
{$IFDEF MSWINDOWS}
var
  lpNumberOfEvents: DWORD;
  lpBuffer: TInputRecord;
  lpNumberOfEventsRead : DWORD;
  nStdHandle: THandle;
begin
  Result := False;
  nStdHandle := GetStdHandle(STD_INPUT_HANDLE);
  lpNumberOfEvents := 0;
  GetNumberOfConsoleInputEvents(nStdHandle, lpNumberOfEvents);
  if lpNumberOfEvents <> 0 then
  begin
    PeekConsoleInput(nStdHandle, lpBuffer, 1, lpNumberOfEventsRead);
    if lpNumberOfEventsRead <> 0 then
    begin
      if lpBuffer.EventType = KEY_EVENT then
      begin
        if lpBuffer.Event.KeyEvent.bKeyDown and ((ExpectedKey = 0) or (lpBuffer.Event.KeyEvent.wVirtualKeyCode = ExpectedKey)) then Result := true
          else FlushConsoleInputBuffer(nStdHandle);
      end
      else FlushConsoleInputBuffer(nStdHandle);
    end;
  end;
end;
{$ELSE}
var
  kp : Char;
begin
  repeat
    kp := Readkey;
  until kp = Char(ExpectedKey);
  Result := True;
end;
{$ENDIF}

function GetConsoleKeyPressed : Word;
{$IFDEF MSWINDOWS}
var
  lpNumberOfEvents: DWORD;
  lpBuffer: TInputRecord;
  lpNumberOfEventsRead : DWORD;
  nStdHandle: THandle;
begin
  Result := 0;
  nStdHandle := GetStdHandle(STD_INPUT_HANDLE);
  lpNumberOfEvents := 0;
  GetNumberOfConsoleInputEvents(nStdHandle, lpNumberOfEvents);
  if lpNumberOfEvents <> 0 then
  begin
    PeekConsoleInput(nStdHandle, lpBuffer, 1, lpNumberOfEventsRead);
    if lpNumberOfEventsRead <> 0 then
    begin
      if lpBuffer.EventType = KEY_EVENT then
      begin
        Result := lpBuffer.Event.KeyEvent.wVirtualKeyCode;
        FlushConsoleInputBuffer(nStdHandle);
      end
      else FlushConsoleInputBuffer(nStdHandle);
    end;
  end;
end;
{$ELSE}
begin
  Result := Ord(ReadKey);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure ProcessMessages;
var
  Msg: TMsg;
begin
  while integer(PeekMessage(Msg, 0, 0, 0, PM_REMOVE)) <> 0 do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure ConsoleWaitForEnterKey;
var
  msg: TMsg;
begin
  while not ConsoleKeyPressed(VK_RETURN) do
  begin
    {$ifndef LVCL}
      {$IFDEF FPC}
      if GetCurrentThreadID = MainThreadID then
      begin
        CheckSynchronize;
        Sleep(1);
      end
      else
      {$ELSE}
      if GetCurrentThreadID = MainThreadID then CheckSynchronize{$IFDEF DELPHI7_UP}(1000){$ENDIF}  else
      {$ENDIF}
    {$endif}
    WaitMessage;
    while PeekMessage(msg,0,0,0,PM_REMOVE) do
    begin
      if Msg.Message = WM_QUIT then Exit
      else
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  end;
end;
{$ELSE}
procedure ConsoleWaitForEnterKey;
begin
  ReadLn;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function RunConsoleCommand(const aCommand, aParameters : String; CallBack : TOutputProc<PAnsiChar> = nil; OutputLines : TStrings = nil) : Cardinal;
const
  CReadBuffer = 2400;
var
  saSecurity: Windows.TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  suiStartup: TStartupInfo;
  piProcess: TProcessInformation;
  pBuffer: array [0..CReadBuffer] of AnsiChar;
  dBuffer: array [0..CReadBuffer] of AnsiChar;
  dRead: DWORD;
  dRunning: DWORD;
  dAvailable: DWORD;
begin
  Result := 0;
  saSecurity.nLength := SizeOf(Windows.TSecurityAttributes);
  saSecurity.bInheritHandle := true;
  saSecurity.lpSecurityDescriptor := nil;
  if CreatePipe(hRead,hWrite,@saSecurity, 0) then
  begin
    try
      FillChar(suiStartup, SizeOf(TStartupInfo), #0);
      suiStartup.cb := SizeOf(TStartupInfo);
      suiStartup.hStdInput := hRead;
      suiStartup.hStdOutput := hWrite;
      suiStartup.hStdError := hWrite;
      suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      suiStartup.wShowWindow := SW_HIDE;
      if CreateProcess(nil,PChar(Format('%s %s',[aCommand,aParameters])),
                            @saSecurity,
                            @saSecurity,
                            True,
                            NORMAL_PRIORITY_CLASS,
                            nil,
                            nil,
                            suiStartup,
                            piProcess) then
      begin
        try
          repeat
            dRunning := WaitForSingleObject(piProcess.hProcess,100);
            PeekNamedPipe(hRead,nil,0,nil,@dAvailable,nil);
            if (dAvailable > 0) then
            begin
              repeat
                dRead := 0;
                ReadFile(hRead,pBuffer[0],CReadBuffer,dRead,nil);
                pBuffer[dRead] := #0;
                OemToCharA(pBuffer,dBuffer);
                if Assigned(CallBack) then CallBack(dBuffer);
                if Assigned(OutputLines) then OutputLines.Add(string(dBuffer));
              until (dRead < CReadBuffer);
            end;
            //Application.ProcessMessages;
          until (dRunning <> WAIT_TIMEOUT);
        finally
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
        end;
      end;
      GetExitCodeProcess(piProcess.hProcess,Result);
    finally
      CloseHandle(hRead);
      CloseHandle(hWrite);
    end;
  end
  else raise Exception.Create('Can''t create pipe!');
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

procedure InitConsole;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  Rewrite(Output);
  hStdOut := TTextRec(Output).Handle;
  {$IFDEF HASERROUTPUT}
    Rewrite(ErrOutput);
    hStdErr := TTextRec(ErrOutput).Handle;
  {$ELSE}
    hStdErr := GetStdHandle(STD_ERROR_HANDLE);
  {$ENDIF}
  if not GetConsoleScreenBufferInfo(hStdOut, BufferInfo) then
  begin
    {$IFNDEF FPC}
    SetInOutRes(GetLastError);
    {$ENDIF}
    Exit;
  end;
  ConsoleRect.Left := 0;
  ConsoleRect.Top := 0;
  ConsoleRect.Right := BufferInfo.dwSize.X - 1;
  ConsoleRect.Bottom := BufferInfo.dwSize.Y - 1;
  TextAttr := BufferInfo.wAttributes and $FF;
  DefConsoleColor := TextAttr;
  LastMode := 3; //CO80;
end;
{$ELSE}
  //AssignCrt(stderr);
  //Rewrite(stderr);
{$ENDIF}

{ TConsoleMenu }

{$IFDEF MSWINDOWS}
procedure TConsoleMenu.AddMenu(const cMenuCaption: string; const cMenuKey: Word; MenuAction: TExecuteProc);
var
  conmenu : TConsoleMenuOption;
begin
  conmenu.Caption := cMenuCaption;
  conmenu.Key := cMenuKey;
  conmenu.OnKeyPressed := MenuAction;
  {$IFDEF DELPHIXE7_UP}
  fConsoleMenu := fConsoleMenu + [conmenu];
  {$ELSE}
  SetLength(fConsoleMenu,High(fConsoleMenu)+1);
  fConsoleMenu[High(fConsoleMenu)] := conmenu;
  {$ENDIF}
end;

procedure TConsoleMenu.AddMenu(MenuOption: TConsoleMenuOption);
begin
  {$IFDEF DELPHIXE7_UP}
  fConsoleMenu := fConsoleMenu + [MenuOption];
  {$ELSE}
  SetLength(fConsoleMenu,High(fConsoleMenu)+1);
  fConsoleMenu[High(fConsoleMenu)] := MenuOption;
  {$ENDIF}
end;

constructor TConsoleMenu.Create;
begin
  fMenuColor := ccLightCyan;
  fIsActive := False;
end;

procedure TConsoleMenu.Refresh(aClearScreen: Boolean);
begin
  if aClearScreen then ClearScreen;
  WriteMenu;
end;

procedure TConsoleMenu.WaitForKeys;
var
  msg: TMsg;
  conmenu : TConsoleMenuOption;
  keypressed : Word;
begin
  fIsActive := True;
  HideCursor;
  WriteMenu;
  while True do
  begin
    //check key pressed
    keypressed := GetConsoleKeyPressed;
    for conmenu in fConsoleMenu do
    begin
      if keypressed = conmenu.Key then
      begin
        ClearScreen;
        WriteMenu;
        conmenu.DoKeyPressed;
      end;
    end;
    if keypressed = VK_ESCAPE then
    begin
      coutXY(50,12,'Exiting...',etInfo);
      Exit;
    end;

    {$ifndef LVCL}
    if GetCurrentThreadID=MainThreadID then CheckSynchronize{$ifdef WITHUXTHEME}(1000){$endif} else
    {$endif}
    WaitMessage;
    while PeekMessage(msg,0,0,0,PM_REMOVE) do
    begin
      if Msg.Message = WM_QUIT then Exit
      else
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  end;
  ShowCursor;
  fIsActive := False;
end;

function GetCharFromVirtualKey(Key: Word): string;
var
    keyboardState: TKeyboardState;
    asciiResult: Integer;
begin
    GetKeyboardState(keyboardState) ;

    SetLength(Result, 2) ;
    asciiResult := ToAscii(key, MapVirtualKey(key, 0), keyboardState, @Result[1], 0) ;
    case asciiResult of
      0: Result := '';
      1: SetLength(Result, 1) ;
      2:;
      else
        Result := '';
    end;
end;

procedure TConsoleMenu.WriteMenu;
var
  conmenu : TConsoleMenuOption;
  ckey : string;
  coord : TCoord;
  oldcoord : TCoord;
begin
  oldcoord.X := GetCursorX;
  oldcoord.Y := GetCursorY;
  coord.X := 0;
  coord.Y := 0;
  SetCursorPos(coord);
  TextColor(fMenuColor);
  ClearLine(0);
  for conmenu in fConsoleMenu do
  begin
    case conmenu.Key of
      VK_F1 : ckey := 'F1';
      VK_F2 : ckey := 'F2';
      VK_F3 : ckey := 'F3';
      VK_F4 : ckey := 'F4';
      VK_F5 : ckey := 'F5';
      VK_F6 : ckey := 'F6';
      VK_F7 : ckey := 'F7';
      VK_F8 : ckey := 'F8';
      VK_F9 : ckey := 'F9';
      VK_F10 : ckey := 'F10';
      VK_F11 : ckey := 'F11';
      VK_F12 : ckey := 'F12';
    else ckey := GetCharFromVirtualKey(conmenu.Key);
    end;
    TextColor(ccWhite);
    Write(Format('[%s]',[ckey]));
    TextColor(Self.MenuColor);
    Write(Format(' %s  ',[conmenu.Caption]));
  end;
  TextColor(ccWhite);
  Write('[ESC]');
  TextColor(Self.MenuColor);
  Write(' Exit');
  TextColor(LastMode);
  SetCursorPos(oldcoord);
end;

{ TConsoleMenuOption }

procedure TConsoleMenuOption.DoKeyPressed;
begin
  if Assigned(fOnKeyPressed) then fOnKeyPressed;
end;
{$ENDIF}

initialization

Console.LogVerbose := LOG_ALL;
{$IF DEFINED(FPC) AND DEFINED(LINUX)}
InitCriticalSection(CSConsole);
{$ELSE}
  {$IF NOT DEFINED(DELPHILINUX) AND NOT DEFINED(MACOS)}
  InitializeCriticalSection(CSConsole);
  //init stdout if not a service
  try
    if HasConsoleOutput then InitConsole;
  except
    //avoid raise exception
  end;
  {$ELSE}
  CSConsole := TRTLCriticalSection.Create;
  {$ENDIF}
{$ENDIF}

finalization
{$IF DEFINED(FPC) AND DEFINED(LINUX)}
DoneCriticalsection(CSConsole);
{$ELSE}
  {$IFNDEF DELPHILINUX}
  DeleteCriticalSection(CSConsole);
  {$ELSE}
  CSConsole.Free;
  {$ENDIF}
{$ENDIF}

end.
