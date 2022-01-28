{ ***************************************************************************

  Copyright (c) 2016-2017 Kike Pérez

  Unit        : Quick.AppService
  Description : Allow run app as console or service
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 14/09/2017
  Modified    : 01/12/2017

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

unit Quick.AppService;

{$i QuickLib.inc}

interface

{$IFNDEF FPC}
{$IFDEF DELPHI2010_UP}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  {$WEAKLINKRTTI ON}
{$ENDIF}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  {$IFNDEF FPC}
  WinSvc,
  {$ENDIF}
  Quick.Commons;

const
  DEF_SERVICENAME = 'QuickAppService';
  DEF_DISPLAYNAME = 'QuickAppService';
  NUM_OF_SERVICES = 2;

type

  TSvcStatus = (ssStopped = SERVICE_STOPPED,
                ssStopping = SERVICE_STOP_PENDING,
                ssStartPending = SERVICE_START_PENDING,
                ssRunning = SERVICE_RUNNING,
                ssPaused = SERVICE_PAUSED);

  TSvcStartType = (stAuto = SERVICE_AUTO_START,
                   stManual = SERVICE_DEMAND_START,
                   stDisabled = SERVICE_DISABLED);

  TSvcInitializeEvent = procedure of object;
  {$IFDEF FPC}
  TSvcAnonMethod = procedure of object;
  {$ELSE}
  TSvcAnonMethod = reference to procedure;
  {$ENDIF}
  TSvcRemoveEvent = procedure of object;

  TAppService = class
  private
    fSCMHandle : SC_HANDLE;
    fSvHandle : SC_HANDLE;
    fServiceName : string;
    fDisplayName : string;
    fLoadOrderGroup : string;
    fDependencies : string;
    fDesktopInteraction : Boolean;
    fUsername : string;
    fUserPass : string;
    fStartType : TSvcStartType;   
    fFileName : string;
    fSilent : Boolean;
    fStatus : TSvcStatus;
    fCanInstallWithOtherName : Boolean;
    fOnInitialize : TSvcInitializeEvent;
    fOnStart : TSvcAnonMethod;
    fOnStop : TSvcAnonMethod;
    fOnExecute : TSvcAnonMethod;
    fAfterRemove : TSvcRemoveEvent;
    procedure ReportSvcStatus(dwCurrentState, dwWin32ExitCode, dwWaitHint: DWORD);
    procedure Execute;
    procedure Help;
    procedure DoStop;
  public
    constructor Create;
    destructor Destroy; override;
    property ServiceName : string read fServiceName write fServiceName;
    property DisplayName : string read fDisplayName write fDisplayName;
    property LoadOrderGroup : string read fLoadOrderGroup write fLoadOrderGroup;
    property Dependencies : string read fDependencies write fDependencies;
    property DesktopInteraction : Boolean read fDesktopInteraction write fDesktopInteraction;
    property UserName : string read fUserName write fUserName;
    property UserPass : string read fUserPass write fUserPass;
    property StartType : TSvcStartType read fStartType write fStartType;
    property FileName : string read fFileName write fFileName;
    property Silent : Boolean read fSilent write fSilent;
    property CanInstallWithOtherName : Boolean read fCanInstallWithOtherName write fCanInstallWithOtherName;
    property Status : TSvcStatus read fStatus write fStatus;
    property OnStart : TSvcAnonMethod read fOnStart write fOnStart;
    property OnStop : TSvcAnonMethod read fOnStop write fOnStop;
    property OnExecute : TSvcAnonMethod read fOnExecute write fOnExecute;
    property OnInitialize : TSvcInitializeEvent read fOnInitialize write fOnInitialize;
    property AfterRemove : TSvcRemoveEvent read fAfterRemove write fAfterRemove;
    procedure Install;
    procedure Remove;
    procedure CheckParams;
    class function InstallParamsPresent : Boolean;
    class function ConsoleParamPresent : Boolean;
    class function IsRunningAsService : Boolean;
    class function IsRunningAsConsole : Boolean;
  end;

var
  ServiceStatus : TServiceStatus;
  StatusHandle  : SERVICE_STATUS_HANDLE;
  ServiceTable  : array [0..NUM_OF_SERVICES] of TServiceTableEntry;
  ghSvcStopEvent: Cardinal;
  AppService : TAppService;

implementation

procedure ServiceCtrlHandler(Control: DWORD); stdcall;
begin
  case Control of
    SERVICE_CONTROL_STOP:
      begin
        AppService.Status := TSvcStatus.ssStopping;
        SetEvent(ghSvcStopEvent);
        ServiceStatus.dwCurrentState := SERVICE_STOP_PENDING;
        SetServiceStatus(StatusHandle, ServiceStatus);
      end;
    SERVICE_CONTROL_PAUSE:
      begin
        AppService.Status := TSvcStatus.ssPaused;
        ServiceStatus.dwcurrentstate := SERVICE_PAUSED;
        SetServiceStatus(StatusHandle, ServiceStatus);
      end;
    SERVICE_CONTROL_CONTINUE:
      begin
        AppService.Status := TSvcStatus.ssRunning;
        ServiceStatus.dwCurrentState := SERVICE_RUNNING;
        SetServiceStatus(StatusHandle, ServiceStatus);
      end;
    SERVICE_CONTROL_INTERROGATE: SetServiceStatus(StatusHandle, ServiceStatus);
    SERVICE_CONTROL_SHUTDOWN:
      begin
        AppService.Status := TSvcStatus.ssStopped;
        AppService.DoStop;
      end;
  end;
end;

procedure RegisterService(dwArgc: DWORD; var lpszArgv: PChar); stdcall;
begin
  ServiceStatus.dwServiceType := SERVICE_WIN32_OWN_PROCESS;
  ServiceStatus.dwCurrentState := SERVICE_START_PENDING;
  ServiceStatus.dwControlsAccepted := SERVICE_ACCEPT_STOP or SERVICE_ACCEPT_PAUSE_CONTINUE;
  ServiceStatus.dwServiceSpecificExitCode := 0;
  ServiceStatus.dwWin32ExitCode := 0;
  ServiceStatus.dwCheckPoint := 0;
  ServiceStatus.dwWaitHint := 0;

  StatusHandle := RegisterServiceCtrlHandler(PChar(AppService.ServiceName), @ServiceCtrlHandler);

  if StatusHandle <> 0 then
  begin
    AppService.ReportSvcStatus(SERVICE_RUNNING, NO_ERROR, 0);
    try
      AppService.Status := TSvcStatus.ssRunning;
      AppService.Execute;
    finally
      AppService.ReportSvcStatus(SERVICE_STOPPED, NO_ERROR, 0);
    end;
  end;
end;


constructor TAppService.Create;
begin
  inherited;
  fServiceName := DEF_SERVICENAME;
  fDisplayName := DEF_DISPLAYNAME;
  fLoadOrderGroup := '';
  fDependencies := '';
  fDesktopInteraction := False;
  fUserName := '';
  fUserPass := '';
  fStartType := TSvcStartType.stAuto;
  fFileName := ParamStr(0);
  fSilent := True;
  fStatus := TSvcStatus.ssStopped;
  fCanInstallWithOtherName := False;
  fOnExecute := nil;
  IsQuickServiceApp := True;
end;

destructor TAppService.Destroy;
begin
  fOnStart := nil;
  fOnStop := nil;
  fOnExecute := nil;
  if fSCMHandle <> 0 then CloseServiceHandle(fSCMHandle);
  if fSvHandle <> 0 then CloseServiceHandle(fSvHandle);
  inherited;
end;

procedure TAppService.ReportSvcStatus(dwCurrentState, dwWin32ExitCode, dwWaitHint: DWORD);
begin
  //fill in the SERVICE_STATUS structure
  ServiceStatus.dwCurrentState := dwCurrentState;
  ServiceStatus.dwWin32ExitCode := dwWin32ExitCode;
  ServiceStatus.dwWaitHint := dwWaitHint;

  if dwCurrentState = SERVICE_START_PENDING then ServiceStatus.dwControlsAccepted := 0
    else ServiceStatus.dwControlsAccepted := SERVICE_ACCEPT_STOP;

  case (dwCurrentState = SERVICE_RUNNING) or (dwCurrentState = SERVICE_STOPPED) of
    True: ServiceStatus.dwCheckPoint := 0;
    False: ServiceStatus.dwCheckPoint := 1;
  end;

  //report service status to SCM
  SetServiceStatus(StatusHandle,ServiceStatus);
end;

procedure TAppService.Execute;
begin
  //we have to do something or service will stop
  ghSvcStopEvent := CreateEvent(nil,True,False,nil);

  if ghSvcStopEvent = 0 then
  begin
    ReportSvcStatus(SERVICE_STOPPED,NO_ERROR,0);
    Exit;
  end;

  if Assigned(fOnStart) then fOnStart;

  //report running status when initialization is complete
  ReportSvcStatus(SERVICE_RUNNING,NO_ERROR,0);

  //perform work until service stops
  while True do
  begin
    //external callback process
    if Assigned(fOnExecute) then fOnExecute;
    //check whether to stop the service.
    WaitForSingleObject(ghSvcStopEvent,INFINITE);
    ReportSvcStatus(SERVICE_STOPPED,NO_ERROR,0);
    Exit;
  end;
end;

procedure TAppService.DoStop;
begin
   if Assigned(fOnStop) then fOnStop;
end;

procedure TAppService.Remove;
const
  cRemoveMsg = 'Service "%s" removed successfully!';
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    Exit;
  try
    Service := OpenService(SCManager,PChar(fServiceName),SERVICE_ALL_ACCESS);
    ControlService(Service,SERVICE_CONTROL_STOP,ServiceStatus);
    DeleteService(Service);
    CloseServiceHandle(Service);
    if fSilent then Writeln(Format(cRemoveMsg,[fServiceName]))
      else MessageBox(0,cRemoveMsg,PChar(fServiceName),MB_ICONINFORMATION or MB_OK or MB_TASKMODAL or MB_TOPMOST);
  finally
    CloseServiceHandle(SCManager);
    if Assigned(fAfterRemove) then fAfterRemove;
  end;
end;

procedure TAppService.Install;
const
  cInstallMsg = 'Service "%s" installed successfully!';
  cSCMError = 'Error trying to open SC Manager (you need admin permissions)';
var
  servicetype : Cardinal;
  starttype : Cardinal;
  svcloadgroup : PChar;
  svcdependencies : PChar;
  svcusername : PChar;
  svcuserpass : PChar;
begin
  fSCMHandle := OpenSCManager(nil,nil,SC_MANAGER_ALL_ACCESS);

  if fSCMHandle = 0 then
  begin
    if fSilent then Writeln(cSCMError)
      else MessageBox(0,cSCMError,PChar(fServiceName),MB_ICONERROR or MB_OK or MB_TASKMODAL or MB_TOPMOST);
    Exit;
  end;
  //service interacts with desktop
  if fDesktopInteraction then servicetype := SERVICE_WIN32_OWN_PROCESS and SERVICE_INTERACTIVE_PROCESS
    else servicetype := SERVICE_WIN32_OWN_PROCESS; 
  //service load order
  if fLoadOrderGroup.IsEmpty then svcloadgroup := nil
    else svcloadgroup := PChar(fLoadOrderGroup);
  //service dependencies
  if fDependencies.IsEmpty then svcdependencies := nil
    else svcdependencies := PChar(fDependencies);
  //service user name
  if fUserName.IsEmpty then svcusername := nil
    else svcusername := PChar(fUserName);
  //service user password
  if fUserPass.IsEmpty then svcuserpass := nil
    else svcuserpass := PChar(fUserPass);
    
  fSvHandle := CreateService(fSCMHandle,
                              PChar(fServiceName),
                              PChar(fDisplayName),
                              SERVICE_ALL_ACCESS,
                              servicetype,
                              Cardinal(fStartType),
                              SERVICE_ERROR_NORMAL,
                              PChar(fFileName),
                              svcloadgroup,
                              nil,
                              svcdependencies,
                              svcusername, //user
                              svcuserpass); //password

  if fSvHandle <> 0 then
  begin
    if fSilent then Writeln(Format(cInstallMsg,[fServiceName]))
      else MessageBox(0,cInstallMsg,PChar(fServiceName),MB_ICONINFORMATION or MB_OK or MB_TASKMODAL or MB_TOPMOST);
  end;
end;

procedure TAppService.Help;
begin
  Writeln('HELP:');
  if fCanInstallWithOtherName then
  begin
    Writeln(Format('%s [/instance:<Service name>] [/console] [/install] [/remove] [/h] [/help]',[ExtractFileName(ParamStr(0))]));
    WriteLn(' [/instance:<service name>]'+#9+'Install service with a custom name');
  end
  else Writeln(Format('%s [/console] [/install] [/remove] [/h] [/help]',[ExtractFileName(ParamStr(0))]));
  WriteLn(' [/console]'+#9#9#9+'Force run as a console application (when runned from another service)');
  WriteLn(' [/install]'+#9#9#9+'Install as a service');
  WriteLn(' [/remove]'+#9#9#9+'Remove service');
  WriteLn(' [/h /help]'+#9#9#9+'This help');
end;

procedure TAppService.CheckParams;
var
  svcname : string;
begin

  if ParamCount > 0 then
  begin
    if (ParamFindSwitch('h')) or (ParamFindSwitch('help')) then Self.Help
      else if ParamFindSwitch('install') then
      begin
        if (fCanInstallWithOtherName) and (ParamGetSwitch('instance',svcname)) then
        begin
          fServiceName := svcname;
          fDisplayName := svcname;
        end;
        Self.Install;
      end
      else if ParamFindSwitch('remove') then
      begin
        if (fCanInstallWithOtherName) and (ParamGetSwitch('instance',svcname)) then
        begin
          fServiceName := svcname;
          fDisplayName := svcname;
        end;
        Self.Remove;
      end
      else if ParamFindSwitch('console') then
      begin
        Writeln('Forced console mode');
      end
      else Writeln('Unknow parameter specified!');
  end
  else
  begin
    //initialize as a service
    if Assigned(fOnInitialize) then fOnInitialize;
    ServiceTable[0].lpServiceName := PChar(fServiceName);
    ServiceTable[0].lpServiceProc := @RegisterService;
    ServiceTable[1].lpServiceName := nil;
    ServiceTable[1].lpServiceProc := nil;
    {$IFDEF FPC}
    StartServiceCtrlDispatcher(@ServiceTable[0]);
    {$ELSE}
    StartServiceCtrlDispatcher(ServiceTable[0]);
    {$ENDIF}
  end;
end;

class function TAppService.ConsoleParamPresent : Boolean;
begin
  Result := ParamFindSwitch('console');
end;

class function TAppService.InstallParamsPresent : Boolean;
begin
  Result := (ParamFindSwitch('install') or ParamFindSwitch('remove') or ParamFindSwitch('help') or ParamFindSwitch('h'));  
end;

class function TAppService.IsRunningAsService : Boolean;
begin
  Result := (IsService and not ConsoleParamPresent) or InstallParamsPresent;
end;

class function TAppService.IsRunningAsConsole : Boolean;
begin
  Result := (not IsService) or (ConsoleParamPresent);
end;

initialization
AppService := TAppService.Create;

finalization
AppService.Free;

end.
