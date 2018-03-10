{ ***************************************************************************

  Copyright (c) 2016-2018 Kike Pérez

  Unit        : Quick.Console
  Description : Console output with colors and optional file log
  Author      : Kike Pérez
  Version     : 1.7
  Created     : 10/05/2017
  Modified    : 07/03/2018

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

{$IFDEF CONDITIONALEXPRESSIONS}
  {$ifndef VER140}
    {$ifndef LINUX}
      {$define WITHUXTHEME}
    {$endif}
  {$endif}
  {$IF CompilerVersion >= 17.0}
    {$DEFINE INLINES}
  {$ENDIF}
  {$IF RTLVersion >= 14.0}
    {$DEFINE HASERROUTPUT}
  {$ENDIF}
{$ENDIF}

interface

uses
  Classes,
  Windows,
  Winapi.Messages,
  System.SysUtils,
  Quick.Commons,
  Quick.Log;

type

  //text colors
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

  TConsoleProperties = record
    LogVerbose : TLogVerbose;
    Log : TQuickLog;
  end;

  TOutputProc<T> = reference to procedure(const aLine : T);
  TExecuteProc = reference to procedure;

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
    procedure WriteMenu;
  public
    constructor Create;
    property MenuColor : TConsoleColor read fMenuColor write fMenuColor;
    procedure AddMenu(const cMenuCaption : string; const cMenuKey : Word; MenuAction : TExecuteProc); overload;
    procedure AddMenu(MenuOption : TConsoleMenuOption); overload;
    procedure WaitForKeys;
  end;

  procedure cout(const cMsg : Integer; cEventType : TLogEventType); overload;
  procedure cout(const cMsg : Double; cEventType : TLogEventType); overload;
  procedure cout(const cMsg : string; cEventType : TLogEventType); overload;
  procedure cout(const cMsg : string; cColor : TConsoleColor); overload;
  procedure coutXY(x,y : Integer; const s : string; cEventType : TLogEventType);
  procedure coutBL(const s : string; cEventType : TLogEventType);
  procedure coutFmt(const cMsg : string; params : array of const; cEventType : TLogEventType);
  procedure TextColor(Color: TConsoleColor); overload;
  procedure TextColor(Color: Byte); overload;
  procedure TextBackground(Color: TConsoleColor); overload;
  procedure TextBackground(Color: Byte); overload;
  procedure ResetColors;
  function ClearScreen : Boolean;
  procedure ClearLine; overload;
  procedure ClearLine(Y : Integer); overload;
  procedure ProcessMessages;
  procedure ConsoleWaitForEnterKey;
  procedure RunConsoleCommand(const aCommand, aParameters : String; CallBack : TOutputProc<PAnsiChar> = nil; OutputLines : TStrings = nil);
  procedure InitConsole;


var
  Console : TConsoleProperties;
  CSConsole : TRTLCriticalSection;
  LastMode : Word;
  DefConsoleColor : Byte;
  TextAttr : Byte;
  hStdOut: THandle;
  hStdErr: THandle;
  ConsoleRect: TSmallRect;
  ScreenBufInfo : TConsoleScreenBufferInfo;

implementation


procedure cout(const cMsg : Integer; cEventType : TLogEventType);
var
  FmtSets : TFormatSettings;
begin
  try
    FmtSets := TFormatSettings.Create;
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
    FmtSets := TFormatSettings.Create;
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
      if hStdOut <> 0 then
      begin
        case cEventType of
          etError : TextColor(ccLightRed);
          etInfo : TextColor(ccWhite);
          etSuccess : TextColor(ccLightGreen);
          etWarning : TextColor(ccYellow);
          etDebug : TextColor(ccLightCyan);
          etTrace : TextColor(ccLightMagenta);
          else TextColor(ccWhite);
        end;
        Writeln(cMsg);
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
    if hStdOut <> 0 then
    begin
      TextColor(cColor);
      Writeln(cMsg);
      TextColor(LastMode);
    end;
  finally
    LeaveCriticalSection(CSConsole);
  end;
end;


function GetCursorX: Integer; {$IFDEF INLINES}inline;{$ENDIF}
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleSCreenBufferInfo(hStdOut, BufferInfo);
  Result := BufferInfo.dwCursorPosition.X;
end;

function GetCursorY: Integer; {$IFDEF INLINES}inline;{$ENDIF}
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleSCreenBufferInfo(hStdOut, BufferInfo);
  Result := BufferInfo.dwCursorPosition.Y;
end;

function GetCursorMaxBottom : Integer;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleSCreenBufferInfo(hStdOut, BufferInfo);
  Result := BufferInfo.srWindow.Bottom;
end;

procedure SetCursorPos(NewCoord : TCoord);
begin
  SetConsoleCursorPosition(hStdOut, NewCoord);
end;

procedure coutXY(x,y : Integer; const s : string; cEventType : TLogEventType);
var
 NewCoord : TCoord;
 LastCoord : TCoord;
begin
  if hStdOut = 0 then Exit;
  LastCoord.X := GetCursorX;
  LastCoord.Y := GetCursorY;
  NewCoord.X := x;
  NewCoord.Y := y;
  ClearLine(Y);
  SetCursorPos(NewCoord);
  try
    cout(s,cEventType);
  finally
    SetCursorPos(LastCoord);
  end;
end;

procedure coutBL(const s : string; cEventType : TLogEventType);
begin
  coutXY(0,GetCurSorMaxBottom - 1,s,cEventType);
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
  if hStdOut = 0 then Exit;
  LastMode := TextAttr;
  TextAttr := (TextAttr and $F0) or (Color and $0F);
  if TextAttr <> LastMode then SetConsoleTextAttribute(hStdOut, TextAttr);
end;

procedure TextBackground(Color: TConsoleColor);
begin
  TextBackground(Integer(Color));
end;

procedure TextBackground(Color: Byte);
begin
  if hStdOut = 0 then Exit;
  LastMode := TextAttr;
  TextAttr := (TextAttr and $0F) or ((Color shl 4) and $F0);
  if TextAttr <> LastMode then SetConsoleTextAttribute(hStdOut, TextAttr);
end;

procedure ResetColors;
begin
  SetConsoleTextAttribute(hStdOut, DefConsoleColor);
  TextAttr := DefConsoleColor;
end;

function ClearScreen : Boolean;
const
  BUFSIZE = 80*25;
var
  Han, Dummy: LongWord;
  buf: string;
  coord: TCoord;
begin
  Result := false;
  Han := GetStdHandle(STD_OUTPUT_HANDLE);
  if Han <> INVALID_HANDLE_VALUE then
  begin
    if SetConsoleTextAttribute(han, FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE) then
    begin
      SetLength(buf,BUFSIZE);
      FillChar(buf[1],Length(buf),' ');
      if WriteConsole(han,PChar(buf),BUFSIZE,Dummy,nil) then
      begin
        coord.X := 0;
        coord.Y := 0;
        if SetConsoleCursorPosition(han,coord) then Result := true;
      end;
    end;
  end;
end;

procedure ClearLine;
begin
  ClearLine(GetCursorY);
end;

procedure ClearLine(Y : Integer);
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

function ConsoleKeyPressed(ExpectedKey: Word): Boolean;
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

function GetConsoleKeyPressed : Word;
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

procedure ProcessMessages;
var
  Msg: TMsg;
begin
  while integer(PeekMessage(Msg, 0, 0, 0, PM_REMOVE)) <> 0 do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

procedure ConsoleWaitForEnterKey;
var
  msg: TMsg;
begin
  while not ConsoleKeyPressed(VK_RETURN) do
  begin
    {$ifndef LVCL}
    if GetCurrentThreadID=MainThreadID then CheckSynchronize{$ifdef WITHUXTHEME}(1000){$endif}  else
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

procedure RunConsoleCommand(const aCommand, aParameters : String; CallBack : TOutputProc<PAnsiChar> = nil; OutputLines : TStrings = nil);
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
                if Assigned(OutputLines) then OutputLines.Add(dBuffer);
              until (dRead < CReadBuffer);
            end;
            //Application.ProcessMessages;
          until (dRunning <> WAIT_TIMEOUT);
        finally
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
        end;
      end;
    finally
      CloseHandle(hRead);
      CloseHandle(hWrite);
    end;
  end
  else raise Exception.Create('Can''t create pipe!');
end;

procedure InitConsole;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  Console.LogVerbose := LOG_ALL;
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
    SetInOutRes(GetLastError);
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

{ TConsoleMenu }

procedure TConsoleMenu.AddMenu(const cMenuCaption: string; const cMenuKey: Word; MenuAction: TExecuteProc);
var
  conmenu : TConsoleMenuOption;
begin
  conmenu.Caption := cMenuCaption;
  conmenu.Key := cMenuKey;
  conmenu.OnKeyPressed := MenuAction;
  fConsoleMenu := fConsoleMenu + [conmenu];
end;

procedure TConsoleMenu.AddMenu(MenuOption: TConsoleMenuOption);
begin
  fConsoleMenu := fConsoleMenu + [MenuOption];
end;

constructor TConsoleMenu.Create;
begin
  fMenuColor := ccLightCyan;
end;

procedure TConsoleMenu.WaitForKeys;
var
  msg: TMsg;
  conmenu : TConsoleMenuOption;
  keypressed : Word;
begin
  WriteMenu;
  while True do
  begin
    //check key pressed
    keypressed := GetConsoleKeyPressed;
    for conmenu in fConsoleMenu do
    begin
      if keypressed = conmenu.Key then conmenu.DoKeyPressed;
    end;
    if keypressed = VK_ESCAPE then
    begin
      coutXY(50,12,'Exiting...',etInfo);
      Exit;
    end;

    {$ifndef LVCL}
    if GetCurrentThreadID=MainThreadID then CheckSynchronize{$ifdef WITHUXTHEME}(1000){$endif}  else
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
begin
  coord.X := 0;
  coord.Y := 0;
  SetCursorPos(coord);
  TextColor(fMenuColor);
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
    Write(Format('[%s] %s  ',[ckey,conmenu.Caption]));
  end;
  write('[ESC] Exit');
  TextColor(ccWhite);
end;

{ TConsoleMenuOption }

procedure TConsoleMenuOption.DoKeyPressed;
begin
  if Assigned(fOnKeyPressed) then fOnKeyPressed;
end;

initialization
InitializeCriticalSection(CSConsole);
//init stdout if not a service
if GetStdHandle(STD_OUTPUT_HANDLE) <> 0 then InitConsole;

finalization
DeleteCriticalSection(CSConsole);

end.
