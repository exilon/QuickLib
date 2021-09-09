{ ***************************************************************************

  Copyright (c) 2016-2018 Kike Pérez

  Unit        : Quick.Service
  Description : Service functions
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 14/07/2017
  Modified    : 30/08/2018

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
unit Quick.Service;

interface

{$i QuickLib.inc}

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  Messages,
  WinSvc,
  {$ENDIF}
  {$IFNDEF FPC}
  System.IOUtils,
  {$ELSE}
  Quick.Files,
  {$ENDIF}
  Quick.Commons,
  Quick.Process;

{$IFDEF MSWINDOWS}
type
  TServiceState = (ssUnknow = -1,
                   ssStopped = SERVICE_STOPPED,
                   ssStartPending = SERVICE_START_PENDING,
                   ssStopPending = SERVICE_STOP_PENDING,
                   ssRunning = SERVICE_RUNNING,
                   ssContinuePending = SERVICE_CONTINUE_PENDING,
                   ssPausePending = SERVICE_PAUSE_PENDING,
                   ssPaused = SERVICE_PAUSED);
{$ENDIF}

  {$IFDEF MSWINDOWS}
  function ServiceIsPresent(const aServiceName : string): Boolean; overload;
  function ServiceIsPresent(const aMachine, aServiceName : string): Boolean; overload;
  function GetServicePath : string;
  function GetServiceState(const aServer, aServiceName : string) : TServiceState;
  function ServiceStart(const aServiceName : string) : Boolean; overload;
  function ServiceStop(const aServiceName : string ) : Boolean; overload;
  function ServiceStart(const aMachine, aServiceName : string) : Boolean; overload;
  function ServiceStop(const aMachine, aServiceName : string ) : Boolean; overload;
  {$ELSE}
  function ServiceIsPresent(const aServiceName : string): Boolean;
  function ServiceStart(const aServiceName : string) : Boolean;
  function ServiceStop(const aServiceName : string ) : Boolean;
  {$ENDIF}
  function ServiceUninstall(const aServiceName : string): Boolean;
  {$IFDEF MSWINDOWS}
  function DeleteServiceEx(svcName : string) : Boolean;
  {$ENDIF}

implementation

function ServiceIsPresent(const aServiceName : string) : Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := ServiceIsPresent('localhost',aServiceName);
end;
{$ELSE}
begin

end;
{$ENDIF}

function ServiceStart(const aServiceName : string) : Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := ServiceStart('localhost',aServiceName);
end;
{$ELSE}
begin

end;
{$ENDIF}

function ServiceStop(const aServiceName : string) : Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := ServiceStop('localhost',aServiceName);
end;
{$ELSE}
begin

end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function ServiceIsPresent(const aMachine, aServiceName : string): Boolean;
var
  smanHnd : SC_Handle;
  svchnd : SC_Handle;
begin
  Result := False;
  smanHnd := OpenSCManager(PChar(aMachine), nil, SC_MANAGER_CONNECT);
  if (smanHnd > 0) then
  begin
    try
      svcHnd := OpenService(smanHnd, PChar(aServiceName), SERVICE_QUERY_STATUS);
      if svcHnd > 0 then
      begin
        Result := True;
        CloseServiceHandle(svchnd);
      end;
    finally
      CloseServiceHandle(smanHnd);
    end;
  end
  else raise Exception.CreateFmt('GetServiceState failed: %s',[GetLastOSError]);
end;

function GetServicePath : string;
var
  filename : array[0..255] of Char;
begin
   GetModuleFileName(hInstance,filename,255);
   Result := TPath.GetDirectoryName(filename);
end;

function GetServiceState(const aServer, aServiceName : string) : TServiceState;
var
  svcStatus : TServiceStatus;
  smanHnd : SC_Handle;
  svcHnd : SC_Handle;
begin
  Result := TServiceState.ssUnknow;
  smanHnd := OpenSCManager(PChar(aServer), Nil, SC_MANAGER_ALL_ACCESS);
  if smanHnd > 0 then
  begin
    try
      svcHnd := OpenService(smanHnd, PChar(aServiceName), SERVICE_ALL_ACCESS);
      if svcHnd > 0 then
      try
        if not QueryServiceStatus(svcHnd,svcStatus) then raise Exception.CreateFmt('GetServiceState failed: %s',[GetLastOSError]);
        Result := TServiceState(svcStatus.dwCurrentState);
      finally
        CloseServiceHandle(svcHnd);
      end;
    finally
      CloseServiceHandle(smanHnd);
    end;
  end
  else raise Exception.CreateFmt('GetServiceState failed: %s',[GetLastOSError]);
end;

function ServiceStart(const aMachine, aServiceName : string) : Boolean;
var
  smanHnd : SC_HANDLE;
  svcHnd : SC_HANDLE;
  svcStatus : TServiceStatus;
  {$IFDEF FPC}
  psTemp : LPPCSTR;
  {$ELSE}
  psTemp : PChar;
  {$ENDIF}
  dwChkP : DWord;
begin
  svcStatus.dwCurrentState := 0;
  smanHnd := OpenSCManager(PChar(aMachine),nil,SC_MANAGER_CONNECT);
  if smanHnd > 0 then
  begin
    try
      svcHnd := OpenService(smanHnd,PChar(aServiceName),SERVICE_START or SERVICE_QUERY_STATUS);
      if svcHnd > 0 then
      try
        psTemp := nil;
        if StartService(svcHnd,0,psTemp) then
        begin
          if QueryServiceStatus(svcHnd,svcStatus) then
          begin
            while svcStatus.dwCurrentState = SERVICE_START_PENDING do
            begin
              dwChkP := svcStatus.dwCheckPoint;
              Sleep(svcStatus.dwWaitHint);
              if not QueryServiceStatus(svcHnd,svcStatus) then Break;

              if svcStatus.dwCheckPoint < dwChkP then Break;
            end;
          end;
        end;
      finally
        CloseServiceHandle(svcHnd);
      end;
    finally
      CloseServiceHandle(smanHnd);
    end;
  end
  else raise Exception.CreateFmt('GetServiceState failed: %s',[GetLastOSError]);
  Result := SERVICE_RUNNING = svcStatus.dwCurrentState;
end;

function ServiceStop(const aMachine, aServiceName : string ) : Boolean;
var
  smanHnd : SC_HANDLE;
  svcHnd : SC_HANDLE;
  svcStatus : TServiceStatus;
  dwChkP : DWord;
begin
  smanHnd := OpenSCManager(PChar(aMachine),nil,SC_MANAGER_CONNECT);
  if smanHnd > 0 then
  try
    svcHnd := OpenService(smanHnd,PChar(aServiceName),SERVICE_STOP or SERVICE_QUERY_STATUS);
    if svcHnd > 0 then
    try
      if ControlService(svcHnd,SERVICE_CONTROL_STOP,svcStatus) then
      begin
        if QueryServiceStatus(svcHnd,svcStatus) then
        begin
          while svcStatus.dwCurrentState <> SERVICE_STOPPED do
          begin
            dwChkP := svcStatus.dwCheckPoint;
            Sleep(svcStatus.dwWaitHint);
            if not QueryServiceStatus(svcHnd,svcStatus) then Break;
            if svcStatus.dwCheckPoint < dwChkP then Break;
          end;
        end;
      end;
    finally
      CloseServiceHandle(svcHnd);
    end;
  finally
    CloseServiceHandle(smanHnd);
  end;
  Result := SERVICE_STOPPED = svcStatus.dwCurrentState;
end;
{$ENDIF}

function ServiceUninstall(const aServiceName : string): Boolean;
{$IFDEF MSWINDOWS}
var
  smanHnd : SC_Handle;
  svchnd : SC_Handle;
  strMachineName: String;
begin
  strMachineName := 'localhost';
  smanHnd := OpenSCManager(PChar(strMachineName), nil, SC_MANAGER_CONNECT);
  if smanHnd > 0 then
  begin
    try
      svchnd := OpenService(smanHnd, PChar(aServiceName), SERVICE_ALL_ACCESS or SERVICE_STOP);
      if svchnd > 0 then
      begin
        try
          {$IFDEF FPC}
          DeleteServiceEx(aServiceName);
          {$ELSE}
          WinSVC.DeleteService(svchnd);
          {$ENDIF}
          Result := True;
        finally
          CloseServiceHandle(svchnd);
        end;
      end
      else if svchnd = 0 Then
        Result := True
      else Result := False;
    finally
      CloseServiceHandle(smanHnd);
    end;
  end;
end;
{$ELSE}
begin

end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function DeleteServiceEx(svcName : string) : Boolean;
begin
  Result := False;
  if ShellExecuteAndWait('open','sc','stop '+svcName,'',0,True) = 0 then
  begin
    Result := ShellExecuteAndWait('open','sc','delete '+svcName,'',0,True) = 0;
  end;
end;
{$ENDIF}

end.
