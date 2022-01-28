{ ***************************************************************************

  Copyright (c) 2016-2021 Kike Pérez

  Unit        : Quick.Process
  Description : Process functions
  Author      : Kike Pérez
  Version     : 1.5
  Created     : 14/07/2017
  Modified    : 08/07/2021

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

unit Quick.Process;

{$i QuickLib.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  ShellAPI,
  Quick.Console,
  {$IFNDEF CONSOLE}
  Controls,
    {$IFNDEF FPC}
    Vcl.Forms,
    Winapi.Messages,
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF FPC}
    TlHelp32,
    psapi,
    {$ELSE}
    JwaTlHelp32,
    Process,
    {$ENDIF}
  {$ELSE}
  Posix.Base,
  Posix.Fcntl,
  {$ENDIF}
  Classes,
  DateUtils,
  SysUtils,
  Quick.Commons;

  {$IFDEF DELPHILINUX}
  type
    TStreamHandle = pointer;
  function popen(const command: PAnsiChar; const _type: PAnsiChar): TStreamHandle; cdecl; external libc name _PU + 'popen';
  function pclose(filehandle: TStreamHandle): int32; cdecl; external libc name _PU + 'pclose';
  function fgets(buffer: pointer; size: int32; Stream: TStreamHAndle): pointer; cdecl; external libc name _PU + 'fgets';
  {$ENDIF}


  //stop a running process
  {$IFDEF MSWINDOWS}
  function KillProcess(const aFileName : string) : Integer; overload;
  {$ELSE}
  function KillProcess(const aProcessName : string) : Integer; overload;
  {$ENDIF}
  function KillProcess(aProcessId : Cardinal) : Boolean; overload;
  //run process as Admin privilegies
  {$IFDEF MSWINDOWS}
  function RunAsAdmin(hWnd: HWND; const aFilename, aParameters: string): Boolean;
  //impersonate logon
  function Impersonate(const aDomain, aUser, aPassword : string): Boolean;
  //revert logon to real logged user
  procedure RevertToSelf;
  //remove dead icons from taskbar tray
  procedure RemoveDeadIcons;
  //get a process of running processes
  function GetProcessList : TstringList;
  //determine if a process is running
  function IsProcessRunnig(const aFileName: string; aFullPath: Boolean): Boolean;
  //get id running process
  function GetProcessId(const aFilename : string; out vProcessId : Integer) : Boolean; overload;
  //get user name is running a process
  function GetProcessUser(aProcessId : DWORD) : string; overload;
  function GetProcessUser(const aFileName : string) : string; overload;
  //executes an aplication and wait for terminate
  function ExecuteAndWait(const aFilename, aCommandLine: string): Boolean;
  function ShellExecuteAndWait(const aOperation, aFileName, aParameter, aDirectory : string; aShowMode : Word; aWaitForTerminate: Boolean) : LongInt;
  {$ENDIF}
  //runs a command and gets console output
  function RunCommand(const aFilename, aParameters : string) : TStringList;
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  //execute an application and return handle
  function ShellExecuteReturnHandle(const aOperation, aFileName, aParameters, aWorkingDir : string; aShowMode: Integer) : THandle;
  {$ENDIF}
  //find an open main window handle
  function FindMainWindow(PID: DWord): DWord;
  //wait for a time period to find an opened main window handle
  function FindMainWindowTimeout(ProcHND : THandle; TimeoutSecs : Integer = 20) : THandle; overload;
  //wait for a time period to find an opened window handle
  function FindWindowTimeout(const aWindowsName : string; TimeoutMSecs : Integer = 1000) : THandle;
  {$IFNDEF CONSOLE}
  //capture a window handle and show it into a wincontrol
  procedure CaptureWindowIntoControl(aWindowHandle: THandle; aContainer: TWinControl);
  {$ENDIF}
  {$ENDIF}


implementation

{$IFDEF MSWINDOWS}
const
  DNLEN = 15;
  UNLEN = 256;

type
  PEnumInfo = ^TEnumInfo;
  TEnumInfo = record
    ProcessID: DWORD;
    HWND: THandle;
  end;

  PTOKEN_USER = ^TOKEN_USER;
  TOKEN_USER = record
    User: TSidAndAttributes;
  end;

function EnumWindowsProc(hwnd : DWord; var einfo: TEnumInfo) : BOOL; stdcall;
var
  PID: DWord;
begin
  GetWindowThreadProcessId(hwnd, @PID);
  Result := (PID  <> einfo.ProcessID) or (not IsWindowVisible(hwnd)) or (not IsWindowEnabled(hwnd));
  if not Result then einfo.HWND := hwnd;
end;

{$IFNDEF FPC}
function CreateWin9xProcessList : TStringList;
var
  hSnapShot: THandle;
  ProcInfo: TProcessEntry32;
begin
  Result := TStringList.Create;
  hSnapShot := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (hSnapShot <> THandle(-1)) then
  begin
    ProcInfo.dwSize := SizeOf(ProcInfo);
    if (Process32First(hSnapshot, ProcInfo)) then
    begin
      Result.Add(ProcInfo.szExeFile);
      while (Process32Next(hSnapShot, ProcInfo)) do Result.Add(ProcInfo.szExeFile);
    end;
    CloseHandle(hSnapShot);
  end;
end;

function CreateWinNTProcessList : TstringList;
var
  PIDArray: array [0..1023] of DWORD;
  cb: DWORD;
  I: Integer;
  ProcCount: Integer;
  hMod: HMODULE;
  hProcess: THandle;
  ModuleName: array [0..300] of Char;
begin
  Result := TStringList.Create;
  EnumProcesses(@PIDArray, SizeOf(PIDArray), cb);
  ProcCount := cb div SizeOf(DWORD);
  for I := 0 to ProcCount - 1 do
  begin
    hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or
      PROCESS_VM_READ,
      False,
      PIDArray[I]);
    if (hProcess <> 0) then
    begin
      EnumProcessModules(hProcess, @hMod, SizeOf(hMod), cb);
      GetModuleFilenameEx(hProcess, hMod, ModuleName, SizeOf(ModuleName));
      Result.Add(ModuleName);
      CloseHandle(hProcess);
    end;
  end;
end;

function GetProcessList : TStringList;
var
  ovinfo: TOSVersionInfo;
begin
  Result := nil;
  ovinfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(ovinfo);
  case ovinfo.dwPlatformId of
    VER_PLATFORM_WIN32_WINDOWS : Result := CreateWin9xProcessList;
    VER_PLATFORM_WIN32_NT : Result := CreateWinNTProcessList;
  end
end;
{$ELSE}
function GetProcessList : TStringList;
var
  pr : THandle;
  pe: TProcessEntry32;
begin
  Result := TStringList.Create;
  pr := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
  try
    pe.dwSize := SizeOf(pe);
    if Process32First(pr,pe) then
    begin
      while Process32Next(pr,pe) do Result.Add(pe.szExeFile);
    end;
  finally
    CloseHandle(pr);
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
function KillProcess(const aFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(aFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(aFileName))) then
      Result := Integer(TerminateProcess(
                        OpenProcess(PROCESS_TERMINATE,
                                    BOOL(0),
                                    FProcessEntry32.th32ProcessID),
                                    0));
     ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;
{$ELSE}
function KillProcess(const aProcessName: string): Integer;
var
  sl : TStringList;
begin
  sl := RunCommand('pkill',aProcessName);
  try
    Result := 1;
  finally
    sl.Free;
  end;
end;
{$ENDIF}

function KillProcess(aProcessId : Cardinal) : Boolean;
{$IFDEF MSWINDOWS}
var
  hProcess : THandle;
begin
  Result := False;
  hProcess := OpenProcess(PROCESS_TERMINATE,False,aProcessId);
  if hProcess > 0 then
  try
    Result := Win32Check(Windows.TerminateProcess(hProcess,0));
  finally
    CloseHandle(hProcess);
  end;
end;
{$ELSE}
var
  sl : TStringList;
begin
  sl := RunCommand('kill',aProcessId.ToString);
  try
    Result := True;
  finally
    sl.Free;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function RunAsAdmin(hWnd: HWND; const aFilename, aParameters: string): Boolean;
var
  shinfo: TShellExecuteInfo;
begin
  ZeroMemory(@shinfo, SizeOf(shinfo));
  shinfo.cbSize := SizeOf(TShellExecuteInfo);
  shinfo.Wnd := hwnd;
  shinfo.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  shinfo.lpVerb := PChar('runas');
  shinfo.lpFile := PChar(aFilename);
  if aParameters <> '' then shinfo.lpParameters := PChar(aParameters);
  shinfo.nShow := SW_SHOWNORMAL;
  {$IFDEF FPC}
  Result := ShellExecuteExW(@shinfo);
  {$ELSE}
  Result := ShellExecuteEx(@shinfo);
  {$ENDIF}
end;

function Impersonate(const aDomain, aUser, aPassword : string): Boolean;
var
  htoken : THandle;
begin
  Result := False;
  if LogonUser(PChar(aUser),PChar(aDomain),PChar(aPassword),LOGON32_LOGON_INTERACTIVE,LOGON32_PROVIDER_DEFAULT,htoken) then
  begin
    Result := ImpersonateLoggedOnUser(htoken);
  end;
end;

procedure RevertToSelf;
begin
  Windows.RevertToSelf;
end;

procedure RemoveDeadIcons;
var
  TrayWindow : HWnd;
  WindowRect : TRect;
  SmallIconWidth : Integer;
  SmallIconHeight : Integer;
  CursorPos : TPoint;
  Row : Integer;
  Col : Integer;
begin
  TrayWindow := FindWindowEx(FindWindow('Shell_TrayWnd',NIL),0,'TrayNotifyWnd',NIL);
  if not GetWindowRect(TrayWindow,WindowRect) then Exit;
  SmallIconWidth := GetSystemMetrics(SM_CXSMICON);
  SmallIconHeight := GetSystemMetrics(SM_CYSMICON);
  GetCursorPos(CursorPos);
  with WindowRect do
  begin
    for Row := 0 to (Bottom - Top) DIV SmallIconHeight do
    begin
      for Col := 0 to (Right - Left) DIV SmallIconWidth do
      begin
        SetCursorPos(Left + Col * SmallIconWidth, Top + Row * SmallIconHeight);
        Sleep(0);
      end;
    end;
  end;
  SetCursorPos(CursorPos.X,CursorPos.Y);
  RedrawWindow(TrayWindow,NIL,0,RDW_INVALIDATE OR RDW_ERASE OR RDW_UPDATENOW);
end;

function IsProcessRunnig(const aFileName: string; aFullPath: Boolean): Boolean;
var
  i: Integer;
  proclist: TstringList;
begin
  try
    proclist := GetProcessList;
    Result := False;
    if proclist = nil then Exit;
    for i := 0 to proclist.Count - 1 do
    begin
      if not aFullPath then
      begin
        if CompareText(ExtractFileName(proclist.Strings[i]), aFileName) = 0 then Result := True
      end
      else if CompareText(proclist.strings[i], aFileName) = 0 then Result := True;
      if Result then Break;
    end;
  finally
    proclist.Free;
  end;
end;

function GetProcessId(const aFilename : string; out vProcessId : Integer) : Boolean;
var
  nproc: BOOL;
  snapHnd : THandle;
  procEntry: TProcessEntry32;
begin
  result := false;
  snapHnd := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    procEntry.dwSize := Sizeof(procEntry);
    nproc := Process32First(snapHnd, procEntry);
    while Integer(nproc) <> 0 do
    begin
      if (StrIComp(PChar(ExtractFileName(procEntry.szExeFile)), PChar(aFilename)) = 0)
         or (StrIComp(procEntry.szExeFile, PChar(aFilename)) = 0)  then
      begin
        vProcessId := procEntry.th32ProcessID;
        Result := true;
        Break;
      end;
      nproc := Process32Next(snapHnd, procEntry);
    end;
  finally
    CloseHandle(snapHnd);
  end;
end;

function GetProcessUser(aProcessId : DWORD): string;
var
  buffer, domain, user: DWORD;
  procHnd, tokenHnd: THandle;
  lpUser: PTOKEN_USER;
  snu: SID_NAME_USE;
  szDomain: array [0..DNLEN] of Char;
  szUser: array [0..UNLEN] of Char;
begin
  Result := '';
  procHnd := OpenProcess(PROCESS_QUERY_INFORMATION, False, aProcessId);
  if procHnd = 0 then Exit;
  try
    if not OpenProcessToken(procHnd, TOKEN_QUERY, tokenHnd) then Exit;
    try
      if not GetTokenInformation(tokenHnd, TokenUser, nil, 0, buffer) then
      begin
        if GetLastError <> ERROR_INSUFFICIENT_BUFFER then Exit;
      end;
      if buffer = 0 then Exit;
      GetMem(lpUser, buffer);
      if not Assigned(lpUser) then Exit;
      try
        if not GetTokenInformation(tokenHnd, TokenUser, lpUser, buffer, buffer) then Exit;
        domain := DNLEN + 1;
        user := UNLEN + 1;
        if LookupAccountSid(nil, lpUser.User.Sid, szUser, user, szDomain,
          domain, snu) then Result := szUser;
      finally
        FreeMem(lpUser);
      end;
    finally
      CloseHandle(tokenHnd);
    end;
  finally
    CloseHandle(procHnd);
  end;
end;

function GetProcessUser(const aFilename : string) : string;
var
  procId : Integer;
begin
  if not GetProcessId(aFilename,procId) then raise Exception.Create('Process not found!')
    else Result := GetProcessUser(procId);
end;

function ExecuteAndWait(const aFilename, aCommandLine: string): Boolean;
var
  dwExitCode: DWORD;
  tpiProcess: TProcessInformation;
  tsiStartup: TStartupInfo;
begin
  Result := False;
  FillChar(tsiStartup, SizeOf(TStartupInfo), 0);
  tsiStartup.cb := SizeOf(TStartupInfo);
  if CreateProcess(PChar(aFilename), PChar(aCommandLine), nil, nil, False, 0,
    nil, nil, tsiStartup, tpiProcess) then
  begin
    if WAIT_OBJECT_0 = WaitForSingleObject(tpiProcess.hProcess, INFINITE) then
    begin
      if GetExitCodeProcess(tpiProcess.hProcess, dwExitCode) then
      begin
        if dwExitCode = 0 then
          Result := True
        else
          SetLastError(dwExitCode + $2000);
      end;
    end;
    dwExitCode := GetLastError;
    CloseHandle(tpiProcess.hProcess);
    CloseHandle(tpiProcess.hThread);
    SetLastError(dwExitCode);
  end;
end;
{$ENDIF}

function RunCommand(const aFilename, aParameters : string) : TStringList;
{$IFDEF MSWINDOWS}
begin
  Result := TStringList.Create;
  RunConsoleCommand(aFilename,aParameters,nil,Result);
end;
{$ELSE}
var
  Handle: TStreamHandle;
  Data: array[0..511] of uint8;
  command : PAnsiChar;
begin
  Result := TStringList.Create;
  try
    if aParameters.IsEmpty then command := PAnsiChar(AnsiString(aFilename))
      else command := PAnsiChar(AnsiString(aFilename + ' ' + aParameters));
    Handle := popen(command, 'r');
    try
      while fgets(@Data[0], Sizeof(Data), Handle) <> nil do Result.Add(Utf8ToString(@Data[0]));
    finally
      pclose(Handle);
    end;
  except
    on E: Exception do
    begin
      Result.Free;
      Exception.CreateFmt('RunCommand: %s',[e.Message]);
    end;
  end;
end;

{$ENDIF}

{$IFDEF MSWINDOWS}
function ShellExecuteAndWait(const aOperation, aFileName, aParameter, aDirectory: string; aShowMode : Word; aWaitForTerminate: Boolean) : LongInt;
var
  done: Boolean;
  shinfo: TShellExecuteInfo;
begin
  FillChar(shinfo, SizeOf(shinfo), Chr(0));
  shinfo.cbSize := SizeOf(shinfo);
  shinfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  shinfo.lpVerb := PChar(aOperation);
  shinfo.lpFile := PChar(aFileName);
  shinfo.lpParameters := PChar(aParameter);
  shinfo.lpDirectory := PChar(aDirectory);
  shinfo.nShow := aShowMode;
  {$IFDEF FPC}
  done := Boolean(ShellExecuteExW(@shinfo));
  {$ELSE}
  done := Boolean(ShellExecuteEx(@shinfo));
  {$ENDIF}
  if done then
  begin
    if aWaitForTerminate then
    begin
      while WaitForSingleObject(shinfo.hProcess, 100) = WAIT_TIMEOUT do
      begin
        {$IFDEF CONSOLE}
        ProcessMessages;
        {$ELSE}
        Application.ProcessMessages;
        {$ENDIF}
      end;
      done := GetExitCodeProcess(shinfo.hProcess, DWORD(Result));
    end
    else Result := 0;
  end;
  if not done then Result := -1;
end;

{$IFNDEF FPC}
function ShellExecuteReturnHandle(const aOperation, aFileName, aParameters, aWorkingDir : string; aShowMode: Integer) : THandle;
var
  exInfo: TShellExecuteInfo;
  Ph: THandle;
begin
  Result := 0;
  FillChar(exInfo, SizeOf(exInfo), 0);
  with exInfo do
  begin
    cbSize := SizeOf(exInfo);
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := GetActiveWindow();
    ExInfo.lpVerb := PChar(aOperation);
    ExInfo.lpParameters := PChar(aParameters);
    exInfo.lpDirectory := PChar(aWorkingDir);
    lpFile := PChar(aFileName);
    nShow := aShowMode;
  end;
  if ShellExecuteEx(@exInfo) then Ph := exInfo.hProcess;
  Result := Windows.GetProcessId(exInfo.hProcess);
End;
{$ENDIF}

function FindMainWindow(PID : DWord): DWORD;
var
  eInfo: TEnumInfo;
begin
  eInfo.ProcessID := PID;
  eInfo.HWND := 0;
  EnumWindows(@EnumWindowsProc, Integer(@eInfo));
  Result := eInfo.HWND;
end;

function FindMainWindowTimeout(ProcHND : THandle; TimeoutSecs : Integer = 20) : THandle;
var
  startime : TDateTime;
begin
  if ProcHND = 0 then Exit;
  startime := Now();
  Result := 0;
  repeat
    Result := FindMainWindow(ProcHND);
    {$IFDEF CONSOLE}
    ProcessMessages;
    {$ELSE}
    Application.ProcessMessages;
    {$ENDIF}
  until (Result <> 0) or (SecondsBetween(Now(),startime) > TimeoutSecs);
end;

function FindWindowTimeout(const aWindowsName : string; TimeoutMSecs : Integer = 1000) : THandle;
var
  startime : TDateTime;
begin
  startime := Now();
  repeat
    Result := FindWindow(0,{$IFDEF FPC}PChar{$ELSE}PWideChar{$ENDIF}(aWindowsName));
    {$IFDEF CONSOLE}
    ProcessMessages;
    {$ELSE}
    Application.ProcessMessages;
    {$ENDIF}
  until (Result <> 0) or (MilliSecondsBetween(Now(),startime) > TimeoutMSecs);
end;

{$IFNDEF CONSOLE}
procedure CaptureWindowIntoControl(aWindowHandle: THandle; aContainer: TWinControl);
var
  WindowStyle : Integer;
  appthreadId: Cardinal;
begin
  WindowStyle := GetWindowLong(aWindowHandle, GWL_STYLE);
  WindowStyle := WindowStyle
  //               - WS_CAPTION
                 - WS_BORDER
  //               - WS_OVERLAPPED
                 - WS_THICKFRAME;
  SetWindowLong(aWindowHandle,GWL_STYLE,WindowStyle);
  appthreadId := GetWindowThreadProcessId(aWindowHandle, nil);
  AttachThreadInput(GetCurrentThreadId, appthreadId, True);
  SetParent(aWindowHandle,aContainer.Handle);
  SendMessage(aContainer.Handle, WM_UPDATEUISTATE, UIS_INITIALIZE, 0);
  UpdateWindow(aWindowHandle);
  SetWindowLong(aContainer.Handle, GWL_STYLE, GetWindowLong(aContainer.Handle,GWL_STYLE) or WS_CLIPCHILDREN);
  SetWindowPos(aWindowHandle,0,0,0,aContainer.ClientWidth,aContainer.ClientHeight,SWP_NOOWNERZORDER);
  SetForegroundWindow(aWindowHandle);
end;
{$ENDIF}
{$ENDIF}

end.
