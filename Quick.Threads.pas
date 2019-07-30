{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.Threads
  Description : Thread safe collections
  Author      : Kike Pérez
  Version     : 1.4
  Created     : 09/03/2018
  Modified    : 31/07/2019

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
  //rtti,
  Types,
  SysUtils,
  DateUtils,
  Quick.Commons,
  //Quick.Chrono,
  Quick.Value,
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
    procedure Clear;
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

  TAdvThread = class(TThread)
  private
    fExecuteProc : TProc;
    fTerminateProc : TProc;
    fExecuteWithSync : Boolean;
    fTerminateWithSync : Boolean;
    procedure DoExecute;
    procedure CallToTerminate;
  protected
    procedure DoTerminate; override;
  public
    constructor Create(aProc : TProc; aSynchronize : Boolean);
    procedure OnTerminate(aProc : TProc; aSynchronize : Boolean);
    procedure Execute; override;
  end;

  IAnonymousThread = interface
    procedure Start;
    function OnTerminate(aProc : TProc) : IAnonymousThread;
    function OnTerminate_Sync(aProc : TProc) : IAnonymousThread;
  end;

  TAnonymousThread = class(TInterfacedObject,IAnonymousThread)
  private
    fThread : TAdvThread;
    constructor Create(aProc : TProc; aSynchronize : Boolean);
  public
    class function Execute(aProc : TProc) : IAnonymousThread; overload;
    class function Execute_Sync(aProc : TProc) : IAnonymousThread; overload;
    procedure Start;
    function OnTerminate(aProc : TProc) : IAnonymousThread; overload;
    function OnTerminate_Sync(aProc : TProc) : IAnonymousThread; overload;
  end;

  TParamArray = array of TFlexValue;

  TWorkTaskStatus = (wtsPending, wtsAssigned, wtsRunning, wtsDone, wtsException);

  TScheduleMode = (smRunOnce, smRepeatMode);

  TTimeMeasure = (tmDays, tmHours, tmMinutes, tmSeconds);

  ITask = interface
  ['{0182FD36-5A7C-4C00-BBF8-7CFB1E3F9BB1}']
    function GetParam(aIndex : Integer) : TFlexValue;
    function TaskStatus : TWorkTaskStatus;
    function GetNumWorker : Integer;
    procedure SetNumWorker(Value : Integer);
    function GetIdTask : Int64;
    procedure SetIdTask(Value : Int64);
    procedure DoExecute;
    procedure DoException(aException : Exception);
    procedure DoTerminate;
    property Param[index : Integer] : TFlexValue read GetParam;
    property NumWorker : Integer read GetNumWorker write SetNumWorker;
    property IdTask : Int64 read GetIdTask;
    function IsEnabled : Boolean;
  end;

  {$IFNDEF FPC}
  TTaskProc = reference to procedure(task : ITask);
  TTaskExceptionProc = reference to procedure(task : ITask; aException : Exception);
  {$ELSE}
  TTaskProc = procedure(task : ITask) of object;
  TTaskExceptionProc = procedure(task : ITask; aException : Exception) of object;
  {$ENDIF}

  IWorkTask = interface(ITask)
    function OnException(aTaskProc : TTaskExceptionProc) : IWorkTask;
    function OnTerminated(aTaskProc : TTaskProc) : IWorkTask;
    procedure Run;
  end;

  IScheduledTask = interface(ITask)
  ['{AE551638-ECDE-4F64-89BF-F07BFCB9C9F7}']
    function OnException(aTaskProc : TTaskExceptionProc) : IScheduledTask;
    function OnException_Sync(aTaskProc : TTaskExceptionProc) : IScheduledTask;
    function OnTerminated(aTaskProc : TTaskProc) : IScheduledTask;
    function OnTerminated_Sync(aTaskProc : TTaskProc) : IScheduledTask;
    function OnExpired(aTaskProc : TTaskProc) : IScheduledTask;
    function OnExpired_Sync(aTaskProc : TTaskProc) : IScheduledTask;
    function CheckSchedule : Boolean;
    procedure DoExpire;
    function GetTaskName : string;
    function StartAt(aStartDate : TDateTime) : IScheduledTask;
    function StartTodayAt(aHour, aMinute: Word; aSecond : Word = 0): IScheduledTask;
    function StartTomorrowAt(aHour, aMinute: Word; aSecond : Word = 0): IScheduledTask;
    function StartOnDayChange : IScheduledTask;
    function StartNow : IScheduledTask;
    function StartInMinutes(aMinutes : Word) : IScheduledTask;
    function StartInSeconds(aSeconds : Word) : IScheduledTask;
    procedure RunOnce;
    procedure RepeatEvery(aInterval : Integer; aTimeMeasure : TTimeMeasure); overload;
    procedure RepeatEvery(aInterval : Integer; aTimeMeasure : TTimeMeasure; aEndTime : TDateTime); overload;
    procedure RepeatEvery(aInterval : Integer; aTimeMeasure : TTimeMeasure; aRepeatTimes : Integer); overload;
    procedure RepeatEveryDay;
    procedure RepeatEveryWeek;
    function IsFinished : Boolean;
    procedure Cancel;
    property Name : string read GetTaskName;
  end;

  TTask = class(TInterfacedObject,ITask)
  private
    fIdTask : Int64;
    fNumWorker : Integer;
    fParamArray : TParamArray;
    fExecuteProc : TTaskProc;
    fExceptProc : TTaskExceptionProc;
    fTerminateProc : TTaskProc;
    fExpiredProc : TTaskProc;
    fTaskStatus : TWorkTaskStatus;
    fOwnedParams : Boolean;
    fEnabled : Boolean;
    fExecuteWithSync : Boolean;
    fExceptionWithSync : Boolean;
    fTerminateWithSync : Boolean;
    function GetParam(aIndex : Integer) : TFlexValue;
    procedure DoExecute;
    procedure DoException(aException : Exception);
    procedure DoTerminate;
    function GetNumWorker : Integer;
    procedure SetNumWorker(Value : Integer);
    function GetIdTask : Int64;
    procedure SetIdTask(Value : Int64);
  protected
    property ExecuteWithSync : Boolean read fExecuteWithSync write fExecuteWithSync;
    property TerminateWithSync : Boolean read fTerminateWithSync write fTerminateWithSync;
    property ExceptionWithSync : Boolean read fExceptionWithSync write fExceptionWithSync;
  public
    constructor Create(aParamArray : array of const; aOwnedParams : Boolean; aTaskProc : TTaskProc); virtual;
    destructor Destroy; override;
    property IdTask : Int64 read GetIdTask;
    property OwnedParams : Boolean read fOwnedParams write fOwnedParams;
    function IsEnabled : Boolean;
    function TaskStatus : TWorkTaskStatus;
  end;

  TWorkTask = class(TTask,IWorkTask)
  public
    function OnException(aTaskProc : TTaskExceptionProc) : IWorkTask; virtual;
    function OnTerminated(aTaskProc : TTaskProc) : IWorkTask; virtual;
    procedure Run; virtual;
  end;

  TTaskQueue = TThreadedQueueCS<IWorkTask>;

  TScheduledTask = class(TTask,IScheduledTask)
  private
    fName : string;
    fExecutionTimes : Integer;
    fScheduleMode : TScheduleMode;
    fTimeInterval : Integer;
    fTimeMeasure : TTimeMeasure;
    fStartDate : TDateTime;
    fLastExecution : TDateTime;
    fNextExecution : TDateTime;
    fExpirationDate : TDateTime;
    fExpirationTimes : Integer;
    fFinished : Boolean;
    fExpireWithSync: Boolean;
    procedure ClearSchedule;
    function CheckSchedule : Boolean;
    procedure DoExpire;
    function GetTaskName : string;
  protected
    property ExpireWithSync : Boolean read fExpireWithSync write fExpireWithSync;
  public
    property Name : string read fName write fName;
    function OnException(aTaskProc : TTaskExceptionProc) : IScheduledTask; virtual;
    function OnException_Sync(aTaskProc : TTaskExceptionProc) : IScheduledTask; virtual;
    function OnTerminated(aTaskProc : TTaskProc) : IScheduledTask; virtual;
    function OnTerminated_Sync(aTaskProc : TTaskProc) : IScheduledTask; virtual;
    function OnExpired(aTaskProc : TTaskProc) : IScheduledTask; virtual;
    function OnExpired_Sync(aTaskProc : TTaskProc) : IScheduledTask; virtual;
    function StartAt(aStartDate : TDateTime) : IScheduledTask;
    function StartTodayAt(aHour, aMinute: Word; aSecond : Word = 0): IScheduledTask;
    function StartTomorrowAt(aHour, aMinute: Word; aSecond : Word = 0): IScheduledTask;
    function StartOnDayChange : IScheduledTask;
    function StartNow : IScheduledTask;
    function StartInMinutes(aMinutes : Word) : IScheduledTask;
    function StartInSeconds(aSeconds : Word) : IScheduledTask;
    procedure RunOnce;
    procedure RepeatEvery(aInterval : Integer; aTimeMeasure : TTimeMeasure); overload;
    procedure RepeatEvery(aInterval : Integer; aTimeMeasure : TTimeMeasure; aEndTime : TDateTime); overload;
    procedure RepeatEvery(aInterval : Integer; aTimeMeasure : TTimeMeasure; aRepeatTimes : Integer); overload;
    procedure RepeatEveryDay;
    procedure RepeatEveryWeek;
    function IsFinished : Boolean;
    procedure Cancel;
  end;

  TWorkerStatus = (wsIdle, wsWorking, wsSuspended);

  TWorker = class(TThread)
  private
    fNumWorker : Integer;
    fCurrentIdTask : Integer;
    fStatus : TWorkerStatus;
    fTaskQueue : TTaskQueue;
    procedure ExecuteTask;
    procedure TerminateTask;
  protected
    fCurrentTask : ITask;
  public
    constructor Create(aNumWorker : Integer; aTaskQueue : TTaskQueue);
    property NumWorker : Integer read fNumWorker;
    property Status : TWorkerStatus read fStatus;
    procedure Execute; override;
  end;

  TScheduledWorker = class(TWorker)
  private
    procedure ExpireTask;
  public
    constructor Create(aNumWorker : Integer; aScheduledTask: IScheduledTask);
    procedure Execute; override;
  end;

  TWorkerPool = TObjectList<TWorker>;

  TBackgroundTasks = class
  private
    fMaxQueue : Integer;
    fWorkerPool : TWorkerPool;
    fConcurrentWorkers : Integer;
    fInsertTimeout : Cardinal;
    fExtractTimeout : Cardinal;
    fTaskQueue : TTaskQueue;
    fNumPushedTasks : Int64;
    function GetTaskQueue : Cardinal;
  public
    constructor Create(aConcurrentWorkers : Integer; aMaxQueue : Integer = 100);
    destructor Destroy; override;
    property MaxQueue : Integer read fMaxQueue;
    property InsertTimeout : Cardinal read fInsertTimeout write fInsertTimeout;
    property ExtractTimeout : Cardinal read fExtractTimeout write fExtractTimeout;
    property TaskQueued : Cardinal read GetTaskQueue;
    property NumPushedTasks : Int64 read fNumPushedTasks;
    property ConcurrentWorkers : Integer read fConcurrentWorkers write fConcurrentWorkers;
    function AddTask(aTaskProc : TTaskProc) : IWorkTask; overload;
    function AddTask_Sync(aTaskProc : TTaskProc) : IWorkTask; overload;
    function AddTask(aParamArray : array of const; aOwnedParams : Boolean; aTaskProc : TTaskProc) : IWorkTask; overload;
    function AddTask_Sync(aParamArray : array of const; aOwnedParams : Boolean; aTaskProc : TTaskProc) : IWorkTask; overload;
    procedure Start;
    procedure CancelAll;
  end;

  TScheduledTaskList = TList<IScheduledTask>;

  TScheduler = class(TThread)
  private
    fListLock : TCriticalSection;
    fCondVar : TSimpleEvent;
    fTaskList : TScheduledTaskList;
    fRemoveTaskAfterExpiration : Boolean;
  public
    constructor Create(aTaskList : TScheduledTaskList);
    destructor Destroy; override;
    property RemoveTaskAfterExpiration : Boolean read fRemoveTaskAfterExpiration write fRemoveTaskAfterExpiration;
    procedure Execute; override;
    function Add(aTask : TScheduledTask) : Integer;
    function Get(aIdTask : Int64) : IScheduledTask; overload;
    function Get(const aTaskName : string) : IScheduledTask; overload;
  end;

  TScheduledTasks = class
  private
    fTaskList : TScheduledTaskList;
    fScheduler : TScheduler;
    fNumPushedTasks : Int64;
    fRemoveTaskAfterExpiration : Boolean;
    fIsStarted : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property NumPushedTasks : Int64 read fNumPushedTasks;
    property RemoveTaskAfterExpiration : Boolean read fRemoveTaskAfterExpiration write fRemoveTaskAfterExpiration;
    property IsStarted : Boolean read fIsStarted;
    function AddTask(const aTaskName : string; aTaskProc : TTaskProc) : IScheduledTask; overload;
    function AddTask_Sync(const aTaskName : string; aTaskProc : TTaskProc) : IScheduledTask; overload;
    function AddTask(const aTaskName : string; aParamArray : array of const; aOwnedParams : Boolean; aTaskProc : TTaskProc) : IScheduledTask; overload;
    function AddTask_Sync(const aTaskName : string; aParamArray : array of const; aOwnedParams : Boolean; aTaskProc : TTaskProc) : IScheduledTask; overload;
    function GetTask(aIdTask : Int64) : IScheduledTask; overload;
    function GetTask(const aTaskName : string) : IScheduledTask; overload;
    procedure Start;
    procedure Stop;
  end;

implementation

{ TThreadedQueueCS<T> }

procedure TThreadedQueueCS<T>.Clear;
var
  obj : T;
begin
  FQueueLock.Enter;
  try
    for obj in FQueue do
    begin
      if TypeInfo(T) = TypeInfo(TObject) then PObject(@obj){$IFNDEF FPC}.DisposeOf;{$ELSE}.Free;{$ENDIF}
    end;
    SetLength(FQueue,0);
  finally
    FQueueLock.Leave;
  end;
  {$IFDEF FPC}
  FQueueCondVar.SetEvent;
  {$ELSE}
  FQueueCondVar.ReleaseAll;
  {$ENDIF}
end;

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

{ TAnonymousThread }

constructor TAnonymousThread.Create(aProc : TProc; aSynchronize : Boolean);
begin
  fThread := TAdvThread.Create(aProc,aSynchronize);
end;

class function TAnonymousThread.Execute(aProc: TProc): IAnonymousThread;
begin
  Result := TAnonymousThread.Create(aProc,False);
end;

class function TAnonymousThread.Execute_Sync(aProc: TProc): IAnonymousThread;
begin
  Result := TAnonymousThread.Create(aProc,True);
end;

function TAnonymousThread.OnTerminate(aProc: TProc): IAnonymousThread;
begin
  Result := Self;
  fThread.OnTerminate(aProc,False);
end;

function TAnonymousThread.OnTerminate_Sync(aProc: TProc): IAnonymousThread;
begin
  Result := Self;
  fThread.OnTerminate(aProc,True);
end;

procedure TAnonymousThread.Start;
begin
  fThread.Start;
end;

{ TTask }

constructor TTask.Create(aParamArray : array of const; aOwnedParams : Boolean; aTaskProc : TTaskProc);
var
  i : Integer;
begin
  fTaskStatus := TWorkTaskStatus.wtsPending;
  fExecuteWithSync := False;
  fTerminateWithSync := False;
  fExceptionWithSync := False;
  fOwnedParams := aOwnedParams;
  SetLength(fParamArray,High(aParamArray)+1);
  for i := Low(aParamArray) to High(aParamArray) do
  begin
    fParamArray[i].Create(aParamArray[i]);
    {$IFDEF FPC}
    fParamArray[i]._AddRef;
    {$ENDIF}
  end;
  fExecuteProc := aTaskProc;
  fEnabled := False;
end;

destructor TTask.Destroy;
var
  i : Integer;
begin
  //free passed params
  if fOwnedParams then
  begin
    for i := Low(fParamArray) to High(fParamArray) do
    begin
      {$IFDEF FPC}
      fParamArray[i]._Release;
      {$ENDIF}
      if (fParamArray[i].DataType = dtObject) and (fParamArray[i].AsObject <> nil) then fParamArray[i].AsObject.Free;
    end;
  end;
  fParamArray := nil;
  inherited;
end;

procedure TTask.DoException(aException : Exception);
begin
  fTaskStatus := TWorkTaskStatus.wtsException;
  if Assigned(fExceptProc) then fExceptProc(Self,aException)
    else raise aException;
end;

procedure TTask.DoExecute;
begin
  fTaskStatus := TWorkTaskStatus.wtsRunning;
  if Assigned(fExecuteProc) then fExecuteProc(Self);
  fTaskStatus := TWorkTaskStatus.wtsDone;
end;

procedure TTask.DoTerminate;
begin
  if Assigned(fTerminateProc) then fTerminateProc(Self);
end;

function TTask.GetIdTask: Int64;
begin
  Result := fIdTask;
end;

procedure TTask.SetIdTask(Value : Int64);
begin
  fIdTask := Value;
end;

function TTask.GetNumWorker: Integer;
begin
  Result := fNumWorker;
end;

function TTask.GetParam(aIndex: Integer): TFlexValue;
begin
  Result := fParamArray[aIndex];
end;

function TTask.IsEnabled: Boolean;
begin
  Result := fEnabled;
end;

procedure TTask.SetNumWorker(Value: Integer);
begin
  fTaskStatus := TWorkTaskStatus.wtsAssigned;
  fNumWorker := Value;
end;

function TTask.TaskStatus: TWorkTaskStatus;
begin
  Result := fTaskStatus;
end;

{ TWorkTask }

function TWorkTask.OnException(aTaskProc : TTaskExceptionProc) : IWorkTask;
begin
  fExceptProc := aTaskProc;
  Result := Self;
end;

function TWorkTask.OnTerminated(aTaskProc: TTaskProc): IWorkTask;
begin
  fTerminateProc := aTaskProc;
  Result := Self;
end;

procedure TWorkTask.Run;
begin
  fEnabled := True;
end;

{ TBackgroundTasks }

procedure TBackgroundTasks.CancelAll;
begin
  fTaskQueue.Clear;
end;

constructor TBackgroundTasks.Create(aConcurrentWorkers : Integer; aMaxQueue : Integer = 100);
begin
  fMaxQueue := aMaxQueue;
  fConcurrentWorkers := aConcurrentWorkers;
  fInsertTimeout := INFINITE;
  fExtractTimeout := 5000;
  fNumPushedTasks := 0;
  fTaskQueue := TThreadedQueueCS<IWorkTask>.Create(fMaxQueue,fInsertTimeout,fExtractTimeout);
end;

destructor TBackgroundTasks.Destroy;
begin
  if Assigned(fWorkerPool) then fWorkerPool.Free;
  if Assigned(fTaskQueue) then fTaskQueue.Free;
  inherited;
end;

function TBackgroundTasks.GetTaskQueue: Cardinal;
begin
  Result := fTaskQueue.QueueSize;
end;

function TBackgroundTasks.AddTask(aTaskProc : TTaskProc) : IWorkTask;
begin
  Result := AddTask([],False,aTaskProc);
end;

function TBackgroundTasks.AddTask(aParamArray : array of const; aOwnedParams : Boolean; aTaskProc : TTaskProc) : IWorkTask;
var
  worktask : IWorkTask;
begin
  worktask := TWorkTask.Create(aParamArray,aOwnedParams,aTaskProc);
  Inc(fNumPushedTasks);
  worktask.SetIdTask(fNumPushedTasks);
  if fTaskQueue.PushItem(worktask) = TWaitResult.wrSignaled then
  begin
    Result := worktask;
  end;
end;

function TBackgroundTasks.AddTask_Sync(aParamArray: array of const; aOwnedParams: Boolean; aTaskProc: TTaskProc): IWorkTask;
begin
  Result := AddTask(aParamArray,aOwnedParams,aTaskProc);
  TTask(Result).ExecuteWithSync := True;
end;

function TBackgroundTasks.AddTask_Sync(aTaskProc: TTaskProc): IWorkTask;
begin
  Result := AddTask_Sync([],False,aTaskProc);
end;

procedure TBackgroundTasks.Start;
var
  i : Integer;
  worker : TWorker;
begin
  //create workers
  if fWorkerPool <> nil then fWorkerPool.Free;
  fWorkerPool := TWorkerPool.Create(True);
  for i := 1 to fConcurrentWorkers do
  begin
    worker := TWorker.Create(i,fTaskQueue);
    fWorkerPool.Add(worker);
    worker.Start;
  end;
end;

{ TWorker }

constructor TWorker.Create(aNumWorker : Integer; aTaskQueue : TTaskQueue);
begin
  inherited Create(True);
  fNumWorker := aNumWorker;
  fStatus := TWorkerStatus.wsSuspended;
  fTaskQueue := aTaskQueue;
  FreeOnTerminate := False;
end;

procedure TWorker.ExecuteTask;
begin
  fCurrentTask.DoExecute;
end;

procedure TWorker.TerminateTask;
begin
  fCurrentTask.DoTerminate;
end;

procedure TWorker.Execute;
begin
  fStatus := TWorkerStatus.wsIdle;
  while not Terminated do
  begin
    fCurrentTask := fTaskQueue.PopItem;
    if fCurrentTask <> nil then
    try
      fStatus := TWorkerStatus.wsWorking;
      try
        fCurrentIdTask := fCurrentTask.GetIdTask;
        if TTask(fCurrentTask).ExecuteWithSync then Synchronize(ExecuteTask)
          else fCurrentTask.DoExecute;
      except
        on E : Exception do
        begin
          if fCurrentTask <> nil then fCurrentTask.DoException(E)
            else raise Exception.Create(e.Message);
        end;
      end;
    finally
      if TTask(fCurrentTask).TerminateWithSync then Synchronize(TerminateTask)
        else fCurrentTask.DoTerminate;
      fStatus := TWorkerStatus.wsIdle;
    end;
  end;
  fStatus := TWorkerStatus.wsSuspended
end;

{ TScheduledTasks }

function TScheduledTasks.AddTask(const aTaskName : string; aTaskProc : TTaskProc) : IScheduledTask;
begin
  Result := AddTask(aTaskName,[],False,aTaskProc);
end;

function TScheduledTasks.AddTask(const aTaskName : string; aParamArray: array of const; aOwnedParams : Boolean; aTaskProc: TTaskProc): IScheduledTask;
var
  scheduletask : TScheduledTask;
begin
  scheduletask := TScheduledTask.Create(aParamArray,aOwnedParams,aTaskProc);
  scheduletask.Name := aTaskName;
  Inc(fNumPushedTasks);
  scheduletask.SetIdTask(fNumPushedTasks);
  fTaskList.Add(scheduletask);
  Result := scheduletask;
end;

function TScheduledTasks.AddTask_Sync(const aTaskName: string; aParamArray: array of const; aOwnedParams: Boolean; aTaskProc: TTaskProc): IScheduledTask;
begin
  Result := AddTask(aTaskName,aParamArray,aOwnedParams,aTaskProc);
  TTask(Result).ExecuteWithSync := True;
end;

function TScheduledTasks.AddTask_Sync(const aTaskName: string; aTaskProc: TTaskProc): IScheduledTask;
begin
  Result := AddTask_Sync(aTaskName,[],False,aTaskProc);
end;

constructor TScheduledTasks.Create;
begin
  fNumPushedTasks := 0;
  fIsStarted := False;
  fTaskList := TScheduledTaskList.Create;
end;

destructor TScheduledTasks.Destroy;
begin
  if Assigned(fScheduler) then
  begin
    fScheduler.Terminate;
    fScheduler.WaitFor;
    fScheduler.Free;
  end;
  if Assigned(fTaskList) then fTaskList.Free;
  inherited;
end;

function TScheduledTasks.GetTask(aIdTask: Int64): IScheduledTask;
begin
  Result := fScheduler.Get(aIdTask);
end;

function TScheduledTasks.GetTask(const aTaskName: string): IScheduledTask;
begin
  if not Assigned(fScheduler) then raise Exception.Create('Scheduler must be started to get a task!');
  Result := fScheduler.Get(aTaskName);
end;

procedure TScheduledTasks.Start;
begin
  if fIsStarted then Exit;
  if not Assigned(fScheduler) then
  begin
    fScheduler := TScheduler.Create(fTaskList);
    fScheduler.RemoveTaskAfterExpiration := fRemoveTaskAfterExpiration;
  end;
  fScheduler.Start;
  fIsStarted := True;
end;

procedure TScheduledTasks.Stop;
begin
  if Assigned(fScheduler) then fScheduler.Terminate;
  fIsStarted := False;
end;

{ TScheduledTask }

function TScheduledTask.StartAt(aStartDate: TDateTime) : IScheduledTask;
begin
  Result := Self;
  ClearSchedule;
  fScheduleMode := TScheduleMode.smRunOnce;
  fStartDate := aStartDate;
  fNextExecution := aStartDate;
end;

function TScheduledTask.StartInMinutes(aMinutes: Word): IScheduledTask;
begin
  Result := Self;
  ClearSchedule;
  fScheduleMode := TScheduleMode.smRunOnce;
  fStartDate := IncMinute(Now(),aMinutes);
  fNextExecution := fStartDate;
end;

function TScheduledTask.StartInSeconds(aSeconds: Word): IScheduledTask;
begin
  Result := Self;
  ClearSchedule;
  fScheduleMode := TScheduleMode.smRunOnce;
  fStartDate := IncSecond(Now(),aSeconds);
  fNextExecution := fStartDate;
end;

function TScheduledTask.StartNow: IScheduledTask;
begin
  Result := Self;
  ClearSchedule;
  fScheduleMode := TScheduleMode.smRunOnce;
  fStartDate := Now();
  fNextExecution := fStartDate;
end;

function TScheduledTask.StartOnDayChange: IScheduledTask;
begin
  Result := Self;
  ClearSchedule;
  fScheduleMode := TScheduleMode.smRunOnce;
  fStartDate := ChangeTimeOfADay(Tomorrow(),0,0,0);
  fNextExecution := fStartDate;
end;

function TScheduledTask.StartTodayAt(aHour, aMinute: Word; aSecond : Word = 0): IScheduledTask;
begin
  Result := Self;
  ClearSchedule;
  fScheduleMode := TScheduleMode.smRunOnce;
  fStartDate := ChangeDateOfADay(Now(),aHour,aMinute,aSecond);
  fNextExecution := fStartDate;
end;

function TScheduledTask.StartTomorrowAt(aHour, aMinute: Word; aSecond : Word = 0): IScheduledTask;
begin
  Result := Self;
  ClearSchedule;
  fScheduleMode := TScheduleMode.smRunOnce;
  fStartDate := ChangeTimeOfADay(Tomorrow(),aHour,aMinute,aSecond);
  fNextExecution := fStartDate;
end;

procedure TScheduledTask.RepeatEvery(aInterval: Integer; aTimeMeasure: TTimeMeasure);
begin
  if fStartDate = 0.0 then ClearSchedule;
  fScheduleMode := TScheduleMode.smRepeatMode;
  fTimeMeasure := aTimeMeasure;
  fTimeInterval := aInterval;
  if fStartDate < Now() then fStartDate := Now();
  fNextExecution := fStartDate;
  fEnabled := True;
end;

procedure TScheduledTask.RepeatEvery(aInterval : Integer; aTimeMeasure : TTimeMeasure; aEndTime : TDateTime);
begin
  if fStartDate = 0.0 then ClearSchedule;
  fScheduleMode := TScheduleMode.smRepeatMode;
  fTimeMeasure := aTimeMeasure;
  fTimeInterval := aInterval;
  if fStartDate < Now() then fStartDate := Now();
  fExpirationDate := aEndTime;
  fNextExecution := fStartDate;
  fEnabled := True;
end;

procedure TScheduledTask.RepeatEveryDay;
begin
  RepeatEvery(1,tmDays);
end;

procedure TScheduledTask.RepeatEveryWeek;
begin
  RepeatEvery(7,tmDays);
end;

procedure TScheduledTask.RepeatEvery(aInterval : Integer; aTimeMeasure : TTimeMeasure; aRepeatTimes : Integer);
begin
  if fStartDate = 0.0 then ClearSchedule;
  fScheduleMode := TScheduleMode.smRepeatMode;
  fTimeMeasure := aTimeMeasure;
  fTimeInterval := aInterval;
  if fStartDate < Now() then fStartDate := Now();
  fExpirationTimes := aRepeatTimes;
  fNextExecution := fStartDate;
  fEnabled := True;
end;

procedure TScheduledTask.RunOnce;
begin
  fScheduleMode := TScheduleMode.smRunOnce;
  if fStartDate < Now() then fStartDate := Now();
  fNextExecution := fStartDate;
  fEnabled := True;
end;

procedure TScheduledTask.Cancel;
begin
  fFinished := True;
end;

function TScheduledTask.CheckSchedule: Boolean;
begin
  Result := False;
  if fTaskStatus = TWorkTaskStatus.wtsRunning then Exit;

  if fScheduleMode = TScheduleMode.smRunOnce then
  begin
    //if start date reached
    if (fExecutionTimes = 0) and (Now() >= fNextExecution) then
    begin
      fLastExecution := Now();
      Inc(fExecutionTimes);
      fFinished := True;
      Result := True;
    end;
  end
  else
  begin
    //if next execution reached
    if Now() >= fNextExecution then
    begin
      //check expiration limits
      if ((fExpirationTimes > 0) and (fExecutionTimes > fExpirationTimes)) or
         ((fExpirationDate > 0.0) and (fNextExecution >= fExpirationDate)) then
      begin
        fFinished := True;
        Exit;
      end;
      //calculate next execution
      case fTimeMeasure of
        tmDays : fNextExecution := IncDay(fNextExecution,fTimeInterval);
        tmHours : fNextExecution := IncHour(fNextExecution,fTimeInterval);
        tmMinutes : fNextExecution := IncMinute(fNextExecution,fTimeInterval);
        tmSeconds : fNextExecution := IncSecond(fNextExecution,fTimeInterval);
      end;

      if Now() > fNextExecution then Result := False //avoid execution if system time was altered
      else
      begin
        fLastExecution := Now();
        Inc(fExecutionTimes);
        Result := True;
      end;
    end;
  end;
end;

procedure TScheduledTask.ClearSchedule;
begin
  inherited;
  fFinished := False;
  fStartDate := 0.0;
  fLastExecution := 0.0;
  fNextExecution := 0.0;
  fExpirationDate := 0.0;
  fScheduleMode := TScheduleMode.smRunOnce;
  fTimeMeasure := TTimeMeasure.tmSeconds;
  fTimeInterval := 0;
end;

procedure TScheduledTask.DoExpire;
begin
  if Assigned(fExpiredProc) then fExpiredProc(Self);
  fEnabled := False;
end;

function TScheduledTask.GetTaskName: string;
begin
  Result := fName;
end;

function TScheduledTask.IsFinished: Boolean;
begin
  Result := fFinished;
end;

function TScheduledTask.OnException(aTaskProc: TTaskExceptionProc): IScheduledTask;
begin
  fExceptProc := aTaskProc;
  Result := Self;
end;

function TScheduledTask.OnException_Sync(aTaskProc: TTaskExceptionProc): IScheduledTask;
begin
  Result := OnException(aTaskProc);
  TTask(Result).ExceptionWithSync := True;
end;

function TScheduledTask.OnExpired(aTaskProc: TTaskProc): IScheduledTask;
begin
  fExpiredProc := aTaskProc;
  Result := Self;
end;

function TScheduledTask.OnExpired_Sync(aTaskProc: TTaskProc): IScheduledTask;
begin
  Result := OnExpired(aTaskProc);
  TScheduledTask(Result).ExpireWithSync := True;
end;

function TScheduledTask.OnTerminated(aTaskProc: TTaskProc): IScheduledTask;
begin
  fTerminateProc := aTaskProc;
  Result := Self;
end;

function TScheduledTask.OnTerminated_Sync(aTaskProc: TTaskProc): IScheduledTask;
begin
  Result := OnTerminated(aTaskProc);
  TTask(Result).TerminateWithSync := True;
end;

{ TScheduledWorker }

constructor TScheduledWorker.Create(aNumWorker : Integer; aScheduledTask: IScheduledTask);
begin
  inherited Create(aNumWorker,nil);
  {$IFNDEF DELPHILINUX}
  NameThreadForDebugging(aScheduledTask.Name,aScheduledTask.IdTask);
  {$ENDIF}
  FreeOnTerminate := True;
  fCurrentTask := aScheduledTask;
end;

procedure TScheduledWorker.Execute;
begin
  fStatus := TWorkerStatus.wsIdle;
  if Assigned(fCurrentTask) then
  begin
    try
      fStatus := TWorkerStatus.wsWorking;
      try
        if TTask(fCurrentTask).ExecuteWithSync then Synchronize(ExecuteTask)
          else fCurrentTask.DoExecute;
        fStatus := TWorkerStatus.wsIdle;
      except
        on E : Exception do
        begin
          if fCurrentTask <> nil then fCurrentTask.DoException(E)
            else raise Exception.Create(e.Message);
        end;
      end;
    finally
      if TTask(fCurrentTask).TerminateWithSync then Synchronize(TerminateTask)
        else fCurrentTask.DoTerminate;
      //check if expired
      if (fCurrentTask as IScheduledTask).IsFinished then
      begin
        if TScheduledTask(fCurrentTask).ExpireWithSync then Synchronize(ExpireTask)
          else (fCurrentTask as IScheduledTask).DoExpire;
      end;
    end;
  end;
  fCurrentTask := nil;
  fStatus := TWorkerStatus.wsSuspended;
end;

procedure TScheduledWorker.ExpireTask;
begin
  (fCurrentTask as IScheduledTask).DoExpire;
end;

{ TScheduler }

constructor TScheduler.Create(aTaskList : TScheduledTaskList);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  fListLock := TCriticalSection.Create;
  fRemoveTaskAfterExpiration := False;
  fTaskList := aTaskList;
  {$IFDEF FPC}
  fCondVar := TSimpleEvent.Create;
  {$ELSE}
  fCondVar := TSimpleEvent.Create(nil,True,False,'');
  {$ENDIF}
end;

destructor TScheduler.Destroy;
begin
  fCondVar.SetEvent;
  fCondVar.Free;
  fTaskList := nil;
  fListLock.Free;
  inherited;
end;

procedure TScheduler.Execute;
var
  task : IScheduledTask;
  worker : TScheduledWorker;
  numworker : Int64;
begin
  numworker := 0;
  while not Terminated do
  begin
    fListLock.Enter;
    try
      for task in fTaskList do
      begin
        if (task.IsEnabled) and (not task.IsFinished) then
        begin
          if task.CheckSchedule then
          begin
            Inc(numworker);
            worker := TScheduledWorker.Create(numworker,task);
            worker.Start;
          end;
        end
        else
        begin
          if task.IsEnabled then
          begin
            //if TScheduledTask(task).ExpireWithSync then Synchronize(ExpireTask)
            //  else task.DoExpire;
            if fRemoveTaskAfterExpiration then fTaskList.Remove(task);
          end;
        end;
      end;
      task := nil;
    finally
      fListLock.Leave;
    end;
    fCondVar.WaitFor(250);
  end;
end;

function TScheduler.Add(aTask: TScheduledTask): Integer;
begin
  Result := fTaskList.Add(aTask);
end;

function TScheduler.Get(aIdTask: Int64): IScheduledTask;
var
  task : IScheduledTask;
begin
  fListLock.Enter;
  try
    for task in fTaskList do
    begin
      if task.IdTask = aIdTask then Exit(task);
    end;
  finally
    fListLock.Leave;
  end;
end;

function TScheduler.Get(const aTaskName: string): IScheduledTask;
var
  task : IScheduledTask;
begin
  fListLock.Enter;
  try
    for task in fTaskList do
    begin
      if CompareText(task.Name,aTaskName) = 0 then Exit(task);
    end;
  finally
    fListLock.Leave;
  end;
end;

{ TAdvThread }

constructor TAdvThread.Create(aProc : TProc; aSynchronize : Boolean);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  fExecuteWithSync := aSynchronize;
  fExecuteProc := aProc;
end;

procedure TAdvThread.DoExecute;
begin
  if Assigned(fExecuteProc) then fExecuteProc;
end;

procedure TAdvThread.CallToTerminate;
begin
  if Assigned(fTerminateProc) then fTerminateProc;
end;

procedure TAdvThread.DoTerminate;
begin
  if fTerminateWithSync then Synchronize(CallToTerminate)
    else CallToTerminate;
end;

procedure TAdvThread.Execute;
begin
  if fExecuteWithSync then Synchronize(DoExecute)
    else DoExecute;
end;


procedure TAdvThread.OnTerminate(aProc: TProc; aSynchronize: Boolean);
begin
  fTerminateWithSync := aSynchronize;
  fTerminateProc := aProc;
end;

end.
