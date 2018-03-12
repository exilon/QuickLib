{ ***************************************************************************

  Copyright (c) 2016-2018 Kike Pérez

  Unit        : Quick.Threads
  Description : Thread safe collections
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 09/03/2018
  Modified    : 12/03/2018

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

interface

uses
  Classes,
  System.Generics.Collections,
  System.SyncObjs;

type

  TThreadedQueueCS<T> = class
  private
    FQueue: array of T;
    FQueueSize, FQueueOffset: Integer;
    FQueueLock: TCriticalSection;
    FQueueCondVar: TConditionVariableCS;
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
    fQueueCondVar: TConditionVariableCS;
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

implementation

{ TThreadedQueueCS<T> }

constructor TThreadedQueueCS<T>.Create(AQueueDepth: Integer = 10; PushTimeout: Cardinal = INFINITE; PopTimeout: Cardinal = INFINITE);
begin
  inherited Create;
  SetLength(FQueue, AQueueDepth);
  FQueueLock := TCriticalSection.Create;
  FQueueCondVar := TConditionVariableCS.Create;
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
  FQueueCondVar.ReleaseAll;
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
      Result := FQueueCondVar.WaitFor(FQueueLock, FPopTimeout);
    end;

    if (FShutDown and (FQueueSize = 0)) or (Result <> wrSignaled) then Exit;

    AItem := FQueue[FQueueOffset];

    FQueue[FQueueOffset] := Default(T);

    if FQueueSize = Length(FQueue) then FQueueCondVar.ReleaseAll;

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
      Result := FQueueCondVar.WaitFor(FQueueLock, FPushTimeout);
    end;

    if FShutDown or (Result <> wrSignaled) then Exit;

    if FQueueSize = 0 then FQueueCondVar.ReleaseAll;

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
  FQueueCondVar.ReleaseAll;
end;

{ TThreadedQueueList<T> }

constructor TThreadedQueueList<T>.Create(AQueueDepth: Integer = 10; PushTimeout: Cardinal = INFINITE; PopTimeout: Cardinal = INFINITE);
begin
  inherited Create;
  fQueue := TQueue<T>.Create;
  fQueue.Capacity := AQueueDepth;
  fQueueSize := 0;
  fQueueLock := TCriticalSection.Create;
  fQueueCondVar := TConditionVariableCS.Create;
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
  fQueueCondVar.ReleaseAll;
end;

function TThreadedQueueList<T>.PopItem: T;
var
  LQueueSize: Integer;
begin
  PopItem(LQueueSize, Result);
end;

function TThreadedQueueList<T>.PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult;
begin
  AItem := Default(T);
  fQueueLock.Enter;
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (fQueueSize = 0) and not fShutDown do
    begin
      Result := fQueueCondVar.WaitFor(fQueueLock, fPopTimeout);
    end;

    if (FShutDown and (fQueueSize = 0)) or (Result <> wrSignaled) then Exit;

    AItem := fQueue.Extract;

    if fQueueSize = fQueue.Count then fQueueCondVar.ReleaseAll;

    Dec(FQueueSize);
    Inc(fTotalItemsPopped);
  finally
    AQueueSize := fQueueSize;
    fQueueLock.Leave;
  end;
end;

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

    if fQueueSize = 0 then fQueueCondVar.ReleaseAll;

    fQueue.Enqueue(AItem);
    Inc(FQueueSize);
    Inc(fTotalItemsPushed);
  finally
    AQueueSize := fQueueSize;
    FQueueLock.Leave;
  end;
end;

procedure TThreadedQueueList<T>.DoShutDown;
begin
  fShutDown := True;
  fQueueCondVar.ReleaseAll;
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

end.
