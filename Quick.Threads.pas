{ ***************************************************************************

  Copyright (c) 2016-2018 Kike Pérez

  Unit        : Quick.Threads
  Description : Thread safe collections
  Author      : Kike Pérez
  Version     : 1.2
  Created     : 09/03/2018
  Modified    : 19/12/2018

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

unit Quick.Threads;

{$i QuickLib.inc}

interface

uses
  Classes,
  Types,
  SysUtils,
  //Quick.Chrono,
  {$IFNDEF FPC}
  System.RTLConsts,
  System.Generics.Collections,
  System.SyncObjs;
  {$ELSE}
  RtlConsts,
  Generics.Collections,
  syncobjs;
  {$ENDIF}

type

  TThreadedQueueCS<T> = class
  private
    FQueue: array of T;
    FQueueSize, FQueueOffset: Integer;
    FQueueLock: TCriticalSection;
    {$IFDEF FPC}
    FQueueCondVar : TEventObject;
    {$ELSE}
    FQueueCondVar: TConditionVariableCS;
    {$ENDIF}
    FShutDown: Boolean;
    FPushTimeout, FPopTimeout: Cardinal;
    FTotalItemsPushed, FTotalItemsPopped: Cardinal;
  public
    constructor Create(AQueueDepth: Integer = 10; PushTimeout: Cardinal = INFINITE; PopTimeout: Cardinal = INFINITE);
    destructor Destroy; override;

    procedure Grow(ADelta: Integer);
    function PushItem(const AItem: T): TWaitResult; overload;
    function PushItem(const AItem: T; var AQueueSize: Integer): TWaitResult; overload;
    function PopItem: T; overload;
    function PopItem(var AQueueSize: Integer): T; overload;
    function PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult; overload;
    function PopItem(var AItem: T): TWaitResult; overload;
    procedure DoShutDown;

    property QueueSize: Integer read FQueueSize;
    property ShutDown: Boolean read FShutDown;
    property TotalItemsPushed: Cardinal read FTotalItemsPushed;
    property TotalItemsPopped: Cardinal read FTotalItemsPopped;
  end;

  TThreadedQueueList<T> = class
  private
    fQueue : TQueue<T>;
    fQueueSize : Integer;
    fQueueLock : TCriticalSection;
    {$IFDEF FPC}
    FQueueCondVar : TSimpleEvent;
    {$ELSE}
    FQueueCondVar: TConditionVariableCS;
    {$ENDIF}
    fShutDown : Boolean;
    fPushTimeout : Cardinal;
    fPopTimeout : Cardinal;
    fTotalItemsPushed : Cardinal;
    fTotalItemsPopped : Cardinal;
  public
    constructor Create(AQueueDepth: Integer = 10; PushTimeout: Cardinal = INFINITE; PopTimeout: Cardinal = INFINITE);
    destructor Destroy; override;

    procedure Grow(ADelta: Integer);
    function PushItem(const AItem: T): TWaitResult; overload;
    function PushItem(const AItem: T; var AQueueSize: Integer): TWaitResult; overload;
    function PopItem: T; overload;
    function PopItem(var AQueueSize: Integer): T; overload;
    function PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult; overload;
    function PopItem(var AItem: T): TWaitResult; overload;
    procedure DoShutDown;

    property QueueSize: Integer read FQueueSize;
    property ShutDown: Boolean read FShutDown;
    property TotalItemsPushed: Cardinal read FTotalItemsPushed;
    property TotalItemsPopped: Cardinal read FTotalItemsPopped;
  end;

  TThreadTask<T> = class(TThread)
  private
    fMaxQueue : Integer;
    fInsertTimeout : Integer;
    fExtractTimeout : Integer;
    fTaskQueue : TThreadedQueueCS<T>;
    function GetTaskQueue : Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    property MaxQueue : Integer read fMaxQueue write fMaxQueue;
    property InsertTimeout : Integer read fInsertTimeout write fInsertTimeout;
    property ExtractTimeout : Integer read fExtractTimeout write fExtractTimeout;
    property TaskQueue : Cardinal read GetTaskQueue;
    procedure Execute; override;
    function AddTask(Task : T) : Boolean;
    procedure Start;
  end;

  {$IFNDEF FPC}
  TThreadObjectList<T: class> = class(TList<T>)
    private
      fList: TObjectList<T>;
      fLock: TObject;
      fDuplicates: TDuplicates;
      function GetItem(aIndex : Integer) : T;
      procedure SetItem(aIndex : Integer; aValue : T);
    public
      constructor Create(OwnedObjects : Boolean);
      destructor Destroy; override;
      property Items[Index : Integer] : T read GetItem write SetItem ; default;
      procedure Add(const Item: T);
      procedure Clear;
      function LockList: TObjectList<T>;
      procedure Remove(const Item: T); inline;
      procedure RemoveItem(const Item: T; Direction: TDirection);
      procedure UnlockList; inline;
      property Duplicates: TDuplicates read fDuplicates write fDuplicates;
  end;
  {$ENDIF}

  {$IFDEF FPC}
  TProc = procedure of object;
  {$ENDIF}

  IAnonymousThread = interface
    procedure Start;
    function OnTerminate(aProc : TProc) : IAnonymousThread;
  end;

  TAnonymousThread = class(TInterfacedObject,IAnonymousThread)
  private
    fThread : TThread;
    fTerminateProc : TProc;
    constructor Create(aProc : TProc);
    procedure NotifyTerminate(Sender : TObject);
  public
    class function Execute(aProc : TProc) : IAnonymousThread;
    procedure Start;
    function OnTerminate(aProc : TProc) : IAnonymousThread;
  end;

implementation

{ TThreadedQueueCS<T> }

constructor TThreadedQueueCS<T>.Create(AQueueDepth: Integer = 10; PushTimeout: Cardinal = INFINITE; PopTimeout: Cardinal = INFINITE);
begin
  inherited Create;
  SetLength(FQueue, AQueueDepth);
  FQueueLock := TCriticalSection.Create;
  {$IFDEF FPC}
  FQueueCondVar := TEventObject.Create(nil, True, False, 'TQCS');
  {$ELSE}
  FQueueCondVar := TConditionVariableCS.Create;
  {$ENDIF}
  FPushTimeout := PushTimeout;
  FPopTimeout := PopTimeout;
end;

destructor TThreadedQueueCS<T>.Destroy;
begin
  DoShutDown;
  FQueueLock.Free;
  FQueueCondVar.Free;
  inherited;
end;

procedure TThreadedQueueCS<T>.Grow(ADelta: Integer);
begin
  FQueueLock.Enter;
  try
    SetLength(FQueue, Length(FQueue) + ADelta);
  finally
    FQueueLock.Leave;
  end;
  {$IFDEF FPC}
  FQueueCondVar.SetEvent;
  {$ELSE}
  FQueueCondVar.ReleaseAll;
  {$ENDIF}
end;

function TThreadedQueueCS<T>.PopItem: T;
var
  LQueueSize: Integer;
begin
  PopItem(LQueueSize, Result);
end;

function TThreadedQueueCS<T>.PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult;
begin
  AItem := Default(T);
  FQueueLock.Enter;
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = 0) and not FShutDown do
    begin
      {$IFDEF FPC}
      Result := FQueueCondVar.WaitFor(FPopTimeout);
      {$ELSE}
      Result := FQueueCondVar.WaitFor(FQueueLock, FPopTimeout);
      {$ENDIF}
    end;

    if (FShutDown and (FQueueSize = 0)) or (Result <> wrSignaled) then Exit;

    AItem := FQueue[FQueueOffset];

    FQueue[FQueueOffset] := Default(T);

    if FQueueSize = Length(FQueue) then
    begin
      {$IFDEF FPC}
      FQueueCondVar.SetEvent;
      {$ELSE}
      FQueueCondVar.ReleaseAll;
      {$ENDIF}
    end;

    Dec(FQueueSize);
    Inc(FQueueOffset);
    Inc(FTotalItemsPopped);

    if FQueueOffset = Length(FQueue) then FQueueOffset := 0;
  finally
    AQueueSize := FQueueSize;
    FQueueLock.Leave;
  end;
end;

function TThreadedQueueCS<T>.PopItem(var AItem: T): TWaitResult;
var
  LQueueSize: Integer;
begin
  Result := PopItem(LQueueSize, AItem);
end;

function TThreadedQueueCS<T>.PopItem(var AQueueSize: Integer): T;
begin
  PopItem(AQueueSize, Result);
end;

function TThreadedQueueCS<T>.PushItem(const AItem: T): TWaitResult;
var
  LQueueSize: Integer;
begin
  Result := PushItem(AItem, LQueueSize);
end;

function TThreadedQueueCS<T>.PushItem(const AItem: T; var AQueueSize: Integer): TWaitResult;
begin
  FQueueLock.Enter;
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = Length(FQueue)) and not FShutDown do
    begin
      {$IFDEF FPC}
      Result := FQueueCondVar.WaitFor(FPushTimeout);
      {$ELSE}
      Result := FQueueCondVar.WaitFor(FQueueLock, FPushTimeout);
      {$ENDIF}
    end;

    if FShutDown or (Result <> wrSignaled) then Exit;

    if FQueueSize = 0 then
    begin
      {$IFDEF FPC}
      FQueueCondVar.SetEvent;
      {$ELSE}
      FQueueCondVar.ReleaseAll;
      {$ENDIF}
    end;

    FQueue[(FQueueOffset + FQueueSize) mod Length(FQueue)] := AItem;
    Inc(FQueueSize);
    Inc(FTotalItemsPushed);
  finally
    AQueueSize := FQueueSize;
    FQueueLock.Leave;
  end;
end;

procedure TThreadedQueueCS<T>.DoShutDown;
begin
  FShutDown := True;
  {$IFDEF FPC}
  FQueueCondVar.SetEvent;
  {$ELSE}
  FQueueCondVar.ReleaseAll;
  {$ENDIF}
end;

{ TThreadedQueueList<T> }

constructor TThreadedQueueList<T>.Create(AQueueDepth: Integer = 10; PushTimeout: Cardinal = INFINITE; PopTimeout: Cardinal = INFINITE);
begin
  inherited Create;
  fQueue := TQueue<T>.Create;
  fQueue.Capacity := AQueueDepth;
  fQueueSize := 0;
  fQueueLock := TCriticalSection.Create;
  {$IFDEF FPC}
  FQueueCondVar := TSimpleEvent.Create; //TEventObject.Create(nil, False, False, 'TQL');
  {$ELSE}
  fQueueCondVar := TConditionVariableCS.Create;
  {$ENDIF}
  fPushTimeout := PushTimeout;
  fPopTimeout := PopTimeout;
end;

destructor TThreadedQueueList<T>.Destroy;
begin
  DoShutDown;
  fQueueLock.Free;
  fQueueCondVar.Free;
  fQueue.Free;
  inherited;
end;

procedure TThreadedQueueList<T>.Grow(ADelta: Integer);
begin
  fQueueLock.Enter;
  try
    fQueue.Capacity := fQueue.Capacity + ADelta;
  finally
    fQueueLock.Leave;
  end;
  {$IFDEF FPC}
  FQueueCondVar.SetEvent;
  {$ELSE}
  FQueueCondVar.ReleaseAll;
  {$ENDIF}
end;

function TThreadedQueueList<T>.PopItem: T;
var
  LQueueSize: Integer;
begin
  PopItem(LQueueSize, Result);
end;

{$IFDEF FPC}
function TThreadedQueueList<T>.PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult;
//var
  //crono : TChronometer;
begin
  AItem := Default(T);
  //crono := TChronometer.Create(False);
  try
    Result := wrSignaled;
    //writeln('popitem');
    //crono.Start;
    while (Result = wrSignaled) and (fQueueSize = 0) and not fShutDown do
    begin
      //crono.Start;
      Result := FQueueCondVar.WaitFor(FPopTimeout);
      //crono.Stop;
      //writeln('in: '  + crono.ElapsedTime);
      //if result = twaitresult.wrError then result := twaitresult.wrError;
    end;
    //crono.Stop;
    //writeln('out: ' + crono.ElapsedTime);

    fQueueLock.Enter;
    try
      if (FShutDown and (fQueueSize = 0)) or (Result <> wrSignaled) then Exit;
      AItem := fQueue.Extract;
      Dec(FQueueSize);
      Inc(fTotalItemsPopped);
    finally
      fQueueLock.Leave;
    end;
  finally
    AQueueSize := fQueueSize;
  end;
end;
{$ELSE}
function TThreadedQueueList<T>.PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult;
begin
  AItem := Default(T);
  fQueueLock.Enter;
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (fQueueSize = 0) and not fShutDown do
    begin
      Result := FQueueCondVar.WaitFor(FQueueLock, FPopTimeout);
    end;

    if (FShutDown and (fQueueSize = 0)) or (Result <> wrSignaled) then Exit;

    AItem := fQueue.Extract;

    if fQueueSize = fQueue.Count then
    begin
      FQueueCondVar.ReleaseAll;
    end;

    Dec(FQueueSize);
    Inc(fTotalItemsPopped);
  finally
    AQueueSize := fQueueSize;
    fQueueLock.Leave;
  end;
end;
{$ENDIF}

function TThreadedQueueList<T>.PopItem(var AItem: T): TWaitResult;
var
  LQueueSize: Integer;
begin
  Result := PopItem(LQueueSize, AItem);
end;

function TThreadedQueueList<T>.PopItem(var AQueueSize: Integer): T;
begin
  PopItem(AQueueSize, Result);
end;

function TThreadedQueueList<T>.PushItem(const AItem: T): TWaitResult;
var
  LQueueSize: Integer;
begin
  Result := PushItem(AItem, LQueueSize);
end;

{$IFDEF FPC}
function TThreadedQueueList<T>.PushItem(const AItem: T; var AQueueSize: Integer): TWaitResult;
begin
  FQueueLock.Enter;
  try
    Result := wrSignaled;
    //while (Result = wrSignaled) and (fQueueSize = fQueue.Count) and not fShutDown do
    //begin
    //  Result := fQueueCondVar.WaitFor(fQueueLock, fPushTimeout);
    //end;

    if fShutDown or (Result <> wrSignaled) then Exit;

    //if fQueueSize = 0 then
    //begin
    //  FQueueCondVar.SetEvent;
    //end;

    fQueue.Enqueue(AItem);
    Inc(FQueueSize);
    Inc(fTotalItemsPushed);
  finally
    AQueueSize := fQueueSize;
    FQueueLock.Leave;
    //FQueueCondVar.SetEvent;
  end;
end;
{$ELSE}
function TThreadedQueueList<T>.PushItem(const AItem: T; var AQueueSize: Integer): TWaitResult;
begin
  FQueueLock.Enter;
  try
    Result := wrSignaled;
    //while (Result = wrSignaled) and (fQueueSize = fQueue.Count) and not fShutDown do
    //begin
    //  Result := fQueueCondVar.WaitFor(fQueueLock, fPushTimeout);
    //end;

    if fShutDown or (Result <> wrSignaled) then Exit;

    if fQueueSize = 0 then FQueueCondVar.ReleaseAll;

    fQueue.Enqueue(AItem);
    Inc(FQueueSize);
    Inc(fTotalItemsPushed);
  finally
    AQueueSize := fQueueSize;
    FQueueLock.Leave;
  end;
end;

{$ENDIF}

procedure TThreadedQueueList<T>.DoShutDown;
begin
  fShutDown := True;
  {$IFDEF FPC}
  FQueueCondVar.SetEvent;
  {$ELSE}
  FQueueCondVar.ReleaseAll;
  {$ENDIF}
end;

{ TThreadTask<T> }

function TThreadTask<T>.AddTask(Task: T): Boolean;
begin
  Result := fTaskQueue.PushItem(Task) = TWaitResult.wrSignaled;
end;

constructor TThreadTask<T>.Create;
begin
  inherited Create(True);
  fMaxQueue := 10;
  fInsertTimeout := INFINITE;
  fExtractTimeout := INFINITE;
end;

destructor TThreadTask<T>.Destroy;
begin
  if Assigned(fTaskQueue) then fTaskQueue.Free;
  inherited;
end;

procedure TThreadTask<T>.Execute;
begin
  inherited;

end;

function TThreadTask<T>.GetTaskQueue: Cardinal;
begin
  if Assigned(fTaskQueue) then Result := fTaskQueue.QueueSize
    else Result := 0;
end;

procedure TThreadTask<T>.Start;
begin
  fTaskQueue := TThreadedQueueCS<T>.Create(fMaxQueue,fInsertTimeout,fExtractTimeout);
end;

{$IFNDEF FPC}
{ TThreadObjectList<T> }

procedure TThreadObjectList<T>.Add(const Item: T);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or (fList.IndexOf(Item) = -1) then fList.Add(Item)
    else if Duplicates = dupError then raise EListError.CreateFmt(SDuplicateItem, [fList.ItemValue(Item)]);
  finally
    UnlockList;
  end;
end;

procedure TThreadObjectList<T>.Clear;
begin
  LockList;
  try
    fList.Clear;
  finally
    UnlockList;
  end;
end;

constructor TThreadObjectList<T>.Create(OwnedObjects : Boolean);
begin
  inherited Create;
  fLock := TObject.Create;
  fList := TObjectList<T>.Create;
  fDuplicates := dupIgnore;
end;

destructor TThreadObjectList<T>.Destroy;
begin
  LockList;
  try
    fList.Free;
    inherited Destroy;
  finally
    UnlockList;
    fLock.Free;
  end;
end;

function TThreadObjectList<T>.GetItem(aIndex: Integer): T;
begin
  LockList;
  try
    Result := fList[aIndex];
  finally
    UnlockList;
  end;
end;

function TThreadObjectList<T>.LockList: TObjectList<T>;
begin
  System.TMonitor.Enter(fLock);
  Result := fList;
end;

procedure TThreadObjectList<T>.Remove(const Item: T);
begin
  RemoveItem(Item, TDirection.FromBeginning);
end;

procedure TThreadObjectList<T>.RemoveItem(const Item: T; Direction: TDirection);
begin
  LockList;
  try
    fList.RemoveItem(Item, Direction);
  finally
    UnlockList;
  end;
end;

procedure TThreadObjectList<T>.SetItem(aIndex: Integer; aValue: T);
begin
  LockList;
  try
    fList[aIndex] := aValue;
  finally
    UnlockList;
  end;
end;

procedure TThreadObjectList<T>.UnlockList;
begin
  System.TMonitor.Exit(fLock);
end;
{$ENDIF}

{ TThreadEx }

constructor TAnonymousThread.Create(aProc : TProc);
begin
  {$IFNDEF FPC}
  fThread := TThread.CreateAnonymousThread(aProc);
  {$ELSE}
  fThread := TThread.CreateAnonymousThread(@aProc);
  {$ENDIF}
end;

class function TAnonymousThread.Execute(aProc: TProc): IAnonymousThread;
begin
  Result := TAnonymousThread.Create(aProc);
end;

procedure TAnonymousThread.NotifyTerminate(Sender: TObject);
begin
  fTerminateProc;
end;

function TAnonymousThread.OnTerminate(aProc: TProc): IAnonymousThread;
begin
  Result := Self;
  fTerminateProc := aProc;
  fThread.OnTerminate := Self.NotifyTerminate;
end;

procedure TAnonymousThread.Start;
begin
  fThread.Start;
end;

end.
