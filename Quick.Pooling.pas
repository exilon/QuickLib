{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.Pooling
  Description : Pooling objects
  Author      : Kike Pérez
  Version     : 1.9
  Created     : 28/02/2020
  Modified    : 29/02/2020

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

unit Quick.Pooling;

{$i QuickLib.inc}

interface

uses
  {$IFDEF DEBUG_OBJPOOL}
  Quick.Debug.Utils,
  {$ENDIF}
  System.SysUtils,
  System.SyncObjs,
  System.DateUtils,
  Quick.Commons,
  Quick.Threads;

type

  IPoolItem<T : class, constructor> = interface
  ['{D52E794B-FDC1-42C1-94BA-823DB74703E4}']
    function Item : T;
    function GetRefCount : Integer;
    function GetItemIndex : Integer;
    function GetLastAccess: TDateTime;
    property RefCount: Integer read GetRefCount;
    property ItemIndex : Integer read GetItemIndex;
    property LastAccess: TDateTime read GetLastAccess;
  end;

  TCreateDelegator<T> = reference to procedure(var aInstance : T);

  TPoolItem<T : class, constructor> = class(TInterfacedObject,IPoolItem<T>)
  private
    fItem : T;
    fItemIndex : Integer;
    fLastAccess : TDateTime;
    function GetRefCount : Integer;
    function GetLastAccess: TDateTime;
    function GetItemIndex : Integer;
  protected
    fLock : TCriticalSection;
    fSemaphore : TSemaphore;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(aSemaphore : TSemaphore; aLock : TCriticalSection; aItemIndex : Integer; aCreateProc : TCreateDelegator<T>);
    destructor Destroy; override;
    function Item : T;
    property RefCount: Integer read GetRefCount;
    property ItemIndex : Integer read GetItemIndex;
    property LastAccess: TDateTime read GetLastAccess;
  end;

  IObjectPool<T : class, constructor> = interface
  ['{AA856DFB-AE8C-46FE-A107-034677010A58}']
    function GetPoolSize: Integer;
    function Get : IPoolItem<T>;
    property PoolSize : Integer read GetPoolSize;
    function TimeoutMs(aTimeout : Integer) : IObjectPool<T>;
    function CreateDelegate(aCreateProc : TCreateDelegator<T>) : IObjectPool<T>;
    function AutoFreeIdleItemTimeMs(aIdleTimeMs : Integer) : IObjectPool<T>;
  end;

  TObjectPool<T : class, constructor> = class(TInterfacedObject,IObjectPool<T>)
  private
    fPool : TArray<IPoolItem<T>>;
    fPoolSize : Integer;
    fWaitTimeoutMs : Integer;
    fLock : TCriticalSection;
    fDelegate : TCreateDelegator<T>;
    fSemaphore : TSemaphore;
    fAutoFreeIdleItemTimeMs : Integer;
    fScheduler : TScheduledTasks;
    function GetPoolSize: Integer;
    procedure CreateScheduler;
    procedure CheckForIdleItems;
  public
    constructor Create(aPoolSize : Integer; aAutoFreeIdleItemTimeMs : Integer = 30000; aCreateProc : TCreateDelegator<T> = nil);
    destructor Destroy; override;
    property PoolSize : Integer read GetPoolSize;
    function TimeoutMs(aTimeout : Integer) : IObjectPool<T>;
    function CreateDelegate(aCreateProc : TCreateDelegator<T>) : IObjectPool<T>;
    function AutoFreeIdleItemTimeMs(aIdleTimeMs : Integer) : IObjectPool<T>;
    function Get : IPoolItem<T>;
  end;

implementation

{ TObjectPool<T> }

function TObjectPool<T>.AutoFreeIdleItemTimeMs(aIdleTimeMs: Integer): IObjectPool<T>;
begin
  Result := Self;
  fAutoFreeIdleItemTimeMs := aIdleTimeMs;
end;

constructor TObjectPool<T>.Create(aPoolSize : Integer; aAutoFreeIdleItemTimeMs : Integer = 30000; aCreateProc : TCreateDelegator<T> = nil);
begin
  fLock := TCriticalSection.Create;
  fPoolSize := aPoolSize;
  fWaitTimeoutMs := 30000;
  fDelegate := aCreateProc;
  fAutoFreeIdleItemTimeMs := aAutoFreeIdleItemTimeMs;
  fSemaphore := TSemaphore.Create(nil,fPoolSize,fPoolSize,'');
  CreateScheduler;
end;

procedure TObjectPool<T>.CreateScheduler;
begin
  fScheduler := TScheduledTasks.Create;
  fScheduler.AddTask('IdleCleaner',[],True,procedure(task : ITask)
      begin
        CheckForIdleItems;
      end)
      .StartInSeconds(10).RepeatEvery(fAutoFreeIdleItemTimeMs,TTimeMeasure.tmMilliseconds);
  fScheduler.Start;
end;

procedure TObjectPool<T>.CheckForIdleItems;
var
  i : Integer;
begin
  fLock.Enter;
  try
    for i := low(fPool) to High(fPool) do
    begin
      //check if item was not used for long time
      if (fPool[i] <> nil) and (fPool[i].RefCount = 1) and (MilliSecondsBetween(Now,fPool[i].LastAccess) > fAutoFreeIdleItemTimeMs) then
      begin
        fPool[i] := nil;
      end;
    end;
  finally
    fLock.Leave;
  end;
end;

function TObjectPool<T>.CreateDelegate(aCreateProc: TCreateDelegator<T>): IObjectPool<T>;
begin
  Result := Self;
  fDelegate := aCreateProc;
end;

destructor TObjectPool<T>.Destroy;
  var
  i: Integer;
begin
  fScheduler.Stop;
  fScheduler.Free;
  fLock.Enter;
  try
    for i := Low(fPool) to High(fPool) do fPool[i] := nil;
    SetLength(FPool,0);
  finally
    fLock.Leave;
  end;
  fLock.Free;
  fSemaphore.Free;
  inherited;
end;

function TObjectPool<T>.Get: IPoolItem<T>;
var
  i : Integer;
  waitResult: TWaitResult;
begin
  Result := nil;
  {$IFDEF DEBUG_OBJPOOL}
  TDebugger.Trace(Self,'Waiting for get idle Pool Item...');
  {$ENDIF}
  waitResult := fSemaphore.WaitFor(fWaitTimeoutMs);
  if waitResult <> TWaitResult.wrSignaled then raise Exception.Create('Connection Pool Timeout: Cannot obtain a connection');
  fLock.Enter;
  try
    if High(fPool) < fPoolSize then SetLength(fPool,High(fPool)+2);
    for i := Low(fPool) to High(fPool) do
    begin
      if fPool[i] = nil then
      begin
        fPool[i] := TPoolItem<T>.Create(fSemaphore,fLock,i,fDelegate);
        {$IFDEF DEBUG_OBJPOOL}
        TDebugger.Trace(Self,'Create Pool Item: %d',[i]);
        {$ENDIF}
        Exit(fPool[i]);
      end;
      if fPool[i].RefCount = 1 then
      begin
        //writeln('get ' + i.ToString);
        {$IFDEF DEBUG_OBJPOOL}
        TDebugger.Trace(Self,'Get Idle Pool Item: %d',[i]);
        {$ENDIF}
        Exit(fPool[i]);
      end
      {$IFDEF DEBUG_OBJPOOL}
      else
      TDebugger.Trace(Self,'Pool Item: %d is busy (RefCount: %d)',[i,fPool[i].RefCount]);
      {$ENDIF}
    end;
  finally
    fLock.Leave;
  end;
end;

function TObjectPool<T>.GetPoolSize: Integer;
begin
  Result := fPoolSize;
end;

function TObjectPool<T>.TimeoutMs(aTimeout: Integer): IObjectPool<T>;
begin
  fWaitTimeoutMs := aTimeout;
end;

{ TPoolItem<T> }

function TPoolItem<T>.Item: T;
begin
  fLastAccess := Now();
  Result := fItem;
end;

constructor TPoolItem<T>.Create(aSemaphore : TSemaphore; aLock : TCriticalSection; aItemIndex : Integer; aCreateProc : TCreateDelegator<T>);
begin
  fLastAccess := Now();
  fItemIndex := aItemIndex;
  if Assigned(aCreateProc) then aCreateProc(fItem)
    else fItem := T.Create;
  fLock := aLock;
  fSemaphore := aSemaphore;
end;

destructor TPoolItem<T>.Destroy;
begin
  if Assigned(fItem) then fItem.Free;
  inherited;
end;

function TPoolItem<T>.GetItemIndex: Integer;
begin
  Result := fItemIndex;
end;

function TPoolItem<T>.GetLastAccess: TDateTime;
begin
  Result := fLastAccess;
end;

function TPoolItem<T>.GetRefCount: Integer;
begin
  Result := FRefCount;
end;

function TPoolItem<T>._AddRef: Integer;
begin
  fLock.Enter;
  {$IFDEF DEBUG_OBJPOOL}
  TDebugger.Trace(Self,'Got Pool item');
  {$ENDIF}
  try
    Inc(FRefCount);
    Result := FRefCount;
  finally
    fLock.Leave;
  end;
end;

function TPoolItem<T>._Release: Integer;
begin
  fLock.Enter;
  {$IFDEF DEBUG_OBJPOOL}
  TDebugger.Trace(Self,'Released Pool item');
  {$ENDIF}
  try
    Dec(fRefCount);
    Result := fRefCount;
    if Result = 0 then
    begin
      FreeAndNil(fItem);
      Destroy;
    end
    else fLastAccess := Now;
  finally
    if fRefCount = 1 then fSemaphore.Release;
    fLock.Leave;
  end;
end;

end.
