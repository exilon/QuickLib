{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.FileMonitor
  Description : Watch for single file changes
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 11/09/2017
  Modified    : 07/04/2018

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

unit Quick.FileMonitor;

interface

{$i QuickLib.inc}

uses
  Classes,
  Windows,
  SysUtils,
  {$IFDEF FPC}
  Quick.Files;
  {$ELSE}
  System.IOUtils;
  {$ENDIF}

type

  TMonitorNotify = (mnNone, mnFileCreated, mnFileModified, mnFileDeleted);
  TMonitorWatch = set of TMonitorNotify;
  TFileChangeNotify = procedure(MonitorNofify : TMonitorNotify) of object;

  TQuickFileMonitor = class(TThread)
  private
    fFileName : string;
    fTickEvent : THandle;
    fInterval : Integer;
    fNotifies : TMonitorWatch;
    fEnabled : Boolean;
    fExists : Boolean;
    fModifedDate : TDateTime;
    fCurrentMonitorNotify : TMonitorNotify;
    fOnChangeNotify : TFileChangeNotify;
    procedure Execute; override;
    procedure SetStatus(Status : Boolean);
    procedure NotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;
    property FileName : string read fFileName write fFileName;
    property Interval : Integer read fInterval write fInterval;
    property Notifies : TMonitorWatch read fNotifies write fNotifies;
    property OnFileChange : TFileChangeNotify read fOnChangeNotify write fOnChangeNotify;
    property Enabled : Boolean read fEnabled write SetStatus;
  end;

implementation

constructor TQuickFileMonitor.Create;
begin
  inherited Create(True);
  Self.FreeOnTerminate := False;
  fInterval := 1000;
  fExists := False;
  fModifedDate := 0;
  fCurrentMonitorNotify := mnNone;
  fNotifies := [mnFileCreated,mnFileModified,mnFileDeleted];
  fTickEvent := CreateEvent(nil, True, False, nil);
  Self.Resume;
end;

destructor TQuickFileMonitor.Destroy;
begin
  if not Terminated then Terminate;
  SetEvent(fTickEvent);
  CloseHandle(fTickEvent);
  inherited;
end;

procedure TQuickFileMonitor.Execute;
var
  LastModifiedDate : TDateTime;
begin
  inherited;
  while not Terminated do
  begin
    fCurrentMonitorNotify := mnNone;
    if WaitForSingleObject(fTickEvent,fInterval) = WAIT_TIMEOUT then
    begin
      if fEnabled then
      begin
        if TFile.Exists(fFileName) then
        begin
          if fExists then
          begin
            if mnFileModified in fNotifies then
            begin
              LastModifiedDate := TFile.GetLastWriteTime(fFileName);
              if LastModifiedDate > fModifedDate then
              begin
                fCurrentMonitorNotify := mnFileModified;
                fModifedDate := LastModifiedDate;
              end;
            end;
          end
          else
          begin
            if mnFileCreated in fNotifies then fCurrentMonitorNotify := mnFileCreated;
            LastModifiedDate := TFile.GetLastWriteTime(fFileName);
            fModifedDate := LastModifiedDate;
          end;
          fExists := True;
        end
        else
        begin
          //check if file deleted
          if mnFileDeleted in fNotifies then
          begin
            if fExists then
            begin
              fExists := False;
              fCurrentMonitorNotify := mnFileDeleted;
            end;
          end;
        end;
        if fCurrentMonitorNotify <> mnNone then
        begin
          if IsConsole then NotifyEvent
            else Synchronize(NotifyEvent);
        end;
      end;
    end;
  end;
end;

procedure TQuickFileMonitor.SetStatus(Status : Boolean);
begin
  if fEnabled <> Status then
  begin
    fEnabled := Status;
    //gets current values
    if TFile.Exists(fFileName) then
    begin
      fExists := True;
      fModifedDate := TFile.GetLastWriteTime(fFileName);
    end;
  end;
end;

procedure TQuickFileMonitor.NotifyEvent;
begin
  if Assigned(fOnChangeNotify) then fOnChangeNotify(fCurrentMonitorNotify);
end;

end.
