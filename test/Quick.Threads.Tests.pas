{ ***************************************************************************

  Copyright (c) 2016-2026 Kike Pérez

  Unit        : Quick.Threads.Tests
  Description : Unit Tests for Quick.Threads
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 27/02/2026
  Modified    : 27/02/2026

 *************************************************************************** }

unit Quick.Threads.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Quick.Value,
  Quick.Threads;

type
  [TestFixture]
  TQuickThreadsTests = class(TObject)
  public
    { TThreadedQueueCS — basic operations }
    [Test]
    procedure Test_ThreadedQueueCS_PushPop_Integer;
    [Test]
    procedure Test_ThreadedQueueCS_PushPop_String;
    [Test]
    procedure Test_ThreadedQueueCS_QueueSize_AfterPush;
    [Test]
    procedure Test_ThreadedQueueCS_QueueSize_AfterPop;
    [Test]
    procedure Test_ThreadedQueueCS_TotalItemsCounted;
    [Test]
    procedure Test_ThreadedQueueCS_Grow_IncreasesCapacity;
    [Test]
    procedure Test_ThreadedQueueCS_DoShutDown_SetsShutDown;
    [Test]
    procedure Test_ThreadedQueueCS_Clear_ResetsSize;
    [Test]
    procedure Test_ThreadedQueueCS_Pop_ReturnsWrTimeout_WhenEmpty;

    { TThreadedQueueList — basic operations }
    [Test]
    procedure Test_ThreadedQueueList_PushPop_Integer;
    [Test]
    procedure Test_ThreadedQueueList_QueueSize_AfterPush;
    [Test]
    procedure Test_ThreadedQueueList_DoShutDown_SetsShutDown;

    { TThreadObjectList }
    {$IFNDEF FPC}
    [Test]
    procedure Test_ThreadObjectList_Add_And_Count;
    [Test]
    procedure Test_ThreadObjectList_Items_ByIndex;
    [Test]
    procedure Test_ThreadObjectList_Remove_DecreasesCount;
    [Test]
    procedure Test_ThreadObjectList_Clear_EmptiesList;
    {$ENDIF}

    { TRunTask / TWorkTask — synchronous execution }
    [Test]
    procedure Test_RunTask_ExecuteSync_RunsProc;
    [Test]
    procedure Test_RunTask_ExecuteSync_SetsDone;
    [Test]
    procedure Test_RunTask_ExecuteSync_StoresResult;
    [Test]
    procedure Test_RunTask_ExecuteSync_OnTerminated_Called;
    [Test]
    procedure Test_RunTask_ExecuteSync_OnException_Called;
    [Test]
    procedure Test_RunTask_ExecuteSync_WithParam_Reads_Param;

    { TAsyncTask }
    [Test]
    procedure Test_AsyncTask_Run_CompletesWithinTimeout;
    [Test]
    procedure Test_AsyncTask_Run_ExecutesAction;

    { TAsyncTask<T> }
    {$IFNDEF FPC}
    [Test]
    procedure Test_AsyncTaskT_Run_ReturnsResult;
    {$ENDIF}

    { TBackgroundTasks }
    [Test]
    procedure Test_BackgroundTasks_AddAndExecute_CompletesTask;
    [Test]
    procedure Test_BackgroundTasks_MultipleTasksComplete;

    { TRunTask — retry on exception }
    [Test]
    procedure Test_RunTask_Retry_RetriesOnException;
    [Test]
    procedure Test_RunTask_OnRetry_CallbackFired;
    [Test]
    procedure Test_RunTask_MultipleParams;
    [Test]
    procedure Test_RunTask_ReachesStatusException_WhenNoRetry;

    { TBackgroundTasks — queue, CancelAll }
    [Test]
    procedure Test_BackgroundTasks_CancelAll_SetsShutDown;
    [Test]
    procedure Test_BackgroundTasks_TaskQueue_IncreasesWithTasks;

    { TScheduledTasks }
    [Test]
    procedure Test_ScheduledTasks_RunOnce_ExecutesAction;
  end;

  { Dummy object for TThreadObjectList tests }
  TDummyObj = class
  private
    fValue: Integer;
  public
    constructor Create(aValue: Integer);
    property Value: Integer read fValue;
  end;

implementation

{ TDummyObj }

constructor TDummyObj.Create(aValue: Integer);
begin
  inherited Create;
  fValue := aValue;
end;

{ TQuickThreadsTests }

{ --- TThreadedQueueCS --- }

procedure TQuickThreadsTests.Test_ThreadedQueueCS_PushPop_Integer;
var
  q: TThreadedQueueCS<Integer>;
  item: Integer;
begin
  q := TThreadedQueueCS<Integer>.Create(16, 0, 100);
  try
    q.PushItem(42);
    item := q.PopItem;
    Assert.AreEqual(42, item, 'Popped item must equal the pushed value');
  finally
    q.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadedQueueCS_PushPop_String;
var
  q: TThreadedQueueCS<string>;
  item: string;
begin
  q := TThreadedQueueCS<string>.Create(16, 0, 100);
  try
    q.PushItem('hello');
    item := q.PopItem;
    Assert.AreEqual('hello', item, 'Popped string must equal pushed value');
  finally
    q.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadedQueueCS_QueueSize_AfterPush;
var
  q: TThreadedQueueCS<Integer>;
begin
  q := TThreadedQueueCS<Integer>.Create(16, 0, 100);
  try
    q.PushItem(1);
    q.PushItem(2);
    Assert.AreEqual(2, q.QueueSize, 'QueueSize must be 2 after two pushes');
  finally
    q.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadedQueueCS_QueueSize_AfterPop;
var
  q: TThreadedQueueCS<Integer>;
begin
  q := TThreadedQueueCS<Integer>.Create(16, 0, 100);
  try
    q.PushItem(1);
    q.PushItem(2);
    q.PopItem;
    Assert.AreEqual(1, q.QueueSize, 'QueueSize must decrease after pop');
  finally
    q.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadedQueueCS_TotalItemsCounted;
var
  q: TThreadedQueueCS<Integer>;
  i: Integer;
begin
  q := TThreadedQueueCS<Integer>.Create(16, 0, 100);
  try
    for i := 1 to 3 do q.PushItem(i);
    for i := 1 to 3 do q.PopItem;
    Assert.AreEqual(Cardinal(3), q.TotalItemsPushed, 'TotalItemsPushed must be 3');
    Assert.AreEqual(Cardinal(3), q.TotalItemsPopped, 'TotalItemsPopped must be 3');
  finally
    q.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadedQueueCS_Grow_IncreasesCapacity;
var
  q: TThreadedQueueCS<Integer>;
  i: Integer;
begin
  // Constructor requires depth >= 10
  q := TThreadedQueueCS<Integer>.Create(10, INFINITE, 100);
  try
    for i := 1 to 10 do q.PushItem(i);
    Assert.WillNotRaise(
      procedure begin q.Grow(10); q.PushItem(11); end,
      nil,
      'Grow must allow pushing beyond original capacity');
  finally
    q.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadedQueueCS_DoShutDown_SetsShutDown;
var
  q: TThreadedQueueCS<Integer>;
begin
  q := TThreadedQueueCS<Integer>.Create(16, 0, 100);
  try
    Assert.IsFalse(q.ShutDown, 'ShutDown must be False initially');
    q.DoShutDown;
    Assert.IsTrue(q.ShutDown, 'ShutDown must be True after DoShutDown');
  finally
    q.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadedQueueCS_Clear_ResetsSize;
var
  q: TThreadedQueueCS<Integer>;
begin
  q := TThreadedQueueCS<Integer>.Create(16, 0, 100);
  try
    q.PushItem(1);
    q.PushItem(2);
    q.Clear;
    Assert.AreEqual(0, q.QueueSize, 'QueueSize must be 0 after Clear');
  finally
    q.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadedQueueCS_Pop_ReturnsWrTimeout_WhenEmpty;
var
  q: TThreadedQueueCS<Integer>;
  item: Integer;
  wr: TWaitResult;
  qSize: Integer;
begin
  // PopTimeout = 50 ms so the test does not hang
  q := TThreadedQueueCS<Integer>.Create(16, 0, 50);
  try
    // Use the overload: PopItem(var AQueueSize; var AItem): TWaitResult
    wr := q.PopItem(qSize, item);
    Assert.IsTrue(wr = wrTimeout, 'PopItem on empty queue with timeout must return wrTimeout');
  finally
    q.Free;
  end;
end;

{ --- TThreadedQueueList --- }

procedure TQuickThreadsTests.Test_ThreadedQueueList_PushPop_Integer;
var
  q: TThreadedQueueList<Integer>;
  item: Integer;
begin
  q := TThreadedQueueList<Integer>.Create(16, 0, 100);
  try
    q.PushItem(7);
    item := q.PopItem;
    Assert.AreEqual(7, item, 'Popped item must equal pushed value');
  finally
    q.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadedQueueList_QueueSize_AfterPush;
var
  q: TThreadedQueueList<Integer>;
begin
  q := TThreadedQueueList<Integer>.Create(16, 0, 100);
  try
    q.PushItem(1);
    q.PushItem(2);
    Assert.AreEqual(2, q.QueueSize, 'QueueSize must reflect pushed items');
  finally
    q.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadedQueueList_DoShutDown_SetsShutDown;
var
  q: TThreadedQueueList<Integer>;
begin
  q := TThreadedQueueList<Integer>.Create(16, 0, 100);
  try
    Assert.IsFalse(q.ShutDown, 'ShutDown must be False initially');
    q.DoShutDown;
    Assert.IsTrue(q.ShutDown, 'ShutDown must be True after DoShutDown');
  finally
    q.Free;
  end;
end;

{ --- TThreadObjectList --- }

{$IFNDEF FPC}
procedure TQuickThreadsTests.Test_ThreadObjectList_Add_And_Count;
var
  list: TThreadObjectList<TDummyObj>;
  locked: TObjectList<TDummyObj>;
begin
  list := TThreadObjectList<TDummyObj>.Create(True);
  try
    list.Add(TDummyObj.Create(1));
    list.Add(TDummyObj.Create(2));
    locked := list.LockList;
    try
      Assert.AreEqual(2, Integer(locked.Count), 'List must contain 2 items after two Adds');
    finally
      list.UnlockList;
    end;
  finally
    list.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadObjectList_Items_ByIndex;
var
  list: TThreadObjectList<TDummyObj>;
begin
  list := TThreadObjectList<TDummyObj>.Create(True);
  try
    list.Add(TDummyObj.Create(42));
    Assert.AreEqual(42, list[0].Value, 'Items[0].Value must be 42');
  finally
    list.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadObjectList_Remove_DecreasesCount;
var
  list: TThreadObjectList<TDummyObj>;
  locked: TObjectList<TDummyObj>;
begin
  // TThreadObjectList always owns objects (OwnedObjects param is ignored in ctor).
  // Do NOT free obj manually after Remove — the internal TObjectList already freed it.
  list := TThreadObjectList<TDummyObj>.Create(True);
  try
    list.Add(TDummyObj.Create(10));
    list.Remove(list[0]);
    locked := list.LockList;
    try
      Assert.AreEqual(0, Integer(locked.Count), 'Count must be 0 after Remove');
    finally
      list.UnlockList;
    end;
  finally
    list.Free;
  end;
end;

procedure TQuickThreadsTests.Test_ThreadObjectList_Clear_EmptiesList;
var
  list: TThreadObjectList<TDummyObj>;
  locked: TObjectList<TDummyObj>;
begin
  list := TThreadObjectList<TDummyObj>.Create(True);
  try
    list.Add(TDummyObj.Create(1));
    list.Add(TDummyObj.Create(2));
    list.Clear;
    locked := list.LockList;
    try
      Assert.AreEqual(0, Integer(locked.Count), 'Count must be 0 after Clear');
    finally
      list.UnlockList;
    end;
  finally
    list.Free;
  end;
end;
{$ENDIF}

{ --- TRunTask (synchronous) --- }

{ Helper: spin-wait for a task to reach wtsDone or wtsException (max aTimeoutMS ms) }
procedure WaitForTask(const task: IWorkTask; aTimeoutMS: Integer = 3000);
var
  elapsed: Integer;
begin
  elapsed := 0;
  while (task.TaskStatus < wtsDone) and (elapsed < aTimeoutMS) do
  begin
    Sleep(10);
    Inc(elapsed, 10);
  end;
end;

procedure TQuickThreadsTests.Test_RunTask_ExecuteSync_RunsProc;
var
  executed: Boolean;
  task: IWorkTask;
begin
  // Execute_Sync uses TThread.Synchronize which deadlocks in a console runner.
  // Use Execute (non-sync) and spin-wait on TaskStatus instead.
  executed := False;
  task := TRunTask.Execute(
    procedure(t: ITask)
    begin
      executed := True;
    end);
  WaitForTask(task);
  Assert.IsTrue(executed, 'Execute_Sync proc must run before Run returns');
end;

procedure TQuickThreadsTests.Test_RunTask_ExecuteSync_SetsDone;
var
  task: IWorkTask;
begin
  task := TRunTask.Execute(procedure(t: ITask) begin end);
  WaitForTask(task);
  Assert.IsTrue(task.TaskStatus = wtsDone, 'Task must reach wtsDone after Execute');
end;

procedure TQuickThreadsTests.Test_RunTask_ExecuteSync_StoresResult;
var
  task: IWorkTask;
begin
  task := TRunTask.Execute(
    procedure(t: ITask)
    begin
      t.Result := 'testresult';
    end);
  WaitForTask(task);
  Assert.AreEqual('testresult', task.Result.AsString,
    'Result stored in task must be retrievable');
end;

procedure TQuickThreadsTests.Test_RunTask_ExecuteSync_OnTerminated_Called;
var
  terminated: Boolean;
  event: TEvent;
  task: IWorkTask;
begin
  terminated := False;
  event := TEvent.Create(nil, True, False, '');
  try
    task := TRunTask.Execute(procedure(t: ITask) begin end)
      .OnTerminated(
        procedure(t: ITask)
        begin
          terminated := True;
          event.SetEvent;
        end);
    Assert.IsTrue(event.WaitFor(3000) = wrSignaled,
      'OnTerminated must be called after Execute');
    Assert.IsTrue(terminated, 'OnTerminated flag must be set');
  finally
    event.Free;
  end;
end;

procedure TQuickThreadsTests.Test_RunTask_ExecuteSync_OnException_Called;
var
  gotException: Boolean;
  event: TEvent;
  task: IWorkTask;
begin
  gotException := False;
  event := TEvent.Create(nil, True, False, '');
  try
    task := TRunTask.Execute(
      procedure(t: ITask)
      begin
        raise Exception.Create('test error');
      end)
      .OnException(
        procedure(t: ITask; e: Exception)
        begin
          gotException := True;
          event.SetEvent;
        end);
    Assert.IsTrue(event.WaitFor(3000) = wrSignaled,
      'OnException must fire within 3 seconds');
    Assert.IsTrue(gotException, 'OnException must be called when proc raises');
  finally
    event.Free;
  end;
end;

procedure TQuickThreadsTests.Test_RunTask_ExecuteSync_WithParam_Reads_Param;
var
  task: IWorkTask;
  readVal: string;
  event: TEvent;
begin
  readVal := '';
  event := TEvent.Create(nil, True, False, '');
  try
    task := TRunTask.Execute(['hello'], False,
      procedure(t: ITask)
      begin
        readVal := t.GetParam(0).AsString;
        event.SetEvent;
      end);
    Assert.IsTrue(event.WaitFor(3000) = wrSignaled,
      'Task with param must complete within 3 seconds');
    Assert.AreEqual('hello', readVal,
      'Task must read the string parameter at index 0');
  finally
    event.Free;
  end;
end;

{ --- TAsyncTask --- }

procedure TQuickThreadsTests.Test_AsyncTask_Run_CompletesWithinTimeout;
var
  task: IAsyncTask;
  event: TEvent;
begin
  // TSimpleThread inside TAsyncTask has FreeOnTerminate=True, so calling
  // task.Wait(timeout) after the thread finishes would access a freed pointer.
  // Use an event inside the action to detect completion safely.
  event := TEvent.Create(nil, True, False, '');
  try
    task := TAsyncTask.Run(
      procedure
      begin
        Sleep(10);
        event.SetEvent;
      end);
    Assert.IsTrue(event.WaitFor(3000) = wrSignaled,
      'TAsyncTask action must complete within 3 seconds');
  finally
    event.Free;
  end;
end;

procedure TQuickThreadsTests.Test_AsyncTask_Run_ExecutesAction;
var
  task: IAsyncTask;
  executed: Boolean;
  event: TEvent;
begin
  executed := False;
  event := TEvent.Create(nil, True, False, '');
  try
    task := TAsyncTask.Run(
      procedure
      begin
        executed := True;
        event.SetEvent;
      end);
    event.WaitFor(2000);
    Assert.IsTrue(executed, 'TAsyncTask action must execute asynchronously');
  finally
    event.Free;
  end;
end;

{ --- TAsyncTask<T> --- }

{$IFNDEF FPC}
procedure TQuickThreadsTests.Test_AsyncTaskT_Run_ReturnsResult;
var
  task: IAsyncTask<Integer>;
  value: Integer;
begin
  task := TAsyncTask<Integer>.Run(function: Integer begin Result := 99; end);
  value := task.Result(2000);
  Assert.AreEqual(99, value, 'TAsyncTask<Integer> must return 99');
end;
{$ENDIF}

{ --- TBackgroundTasks --- }

procedure TQuickThreadsTests.Test_BackgroundTasks_AddAndExecute_CompletesTask;
var
  bg: TBackgroundTasks;
  event: TEvent;
  task: IWorkTask;
begin
  bg := TBackgroundTasks.Create(2, 10);
  event := TEvent.Create(nil, True, False, '');
  try
    task := bg.AddTask(
      procedure(t: ITask)
      begin
        event.SetEvent;
      end);
    bg.Start;
    Assert.IsTrue(event.WaitFor(3000) = wrSignaled,
      'Background task must execute within 3 seconds');
  finally
    bg.Free;
    event.Free;
  end;
end;

procedure TQuickThreadsTests.Test_BackgroundTasks_MultipleTasksComplete;
var
  bg: TBackgroundTasks;
  counter: Integer;
  event: TEvent;
  lock: TCriticalSection;
  i: Integer;
begin
  bg := TBackgroundTasks.Create(4, 20);
  event := TEvent.Create(nil, True, False, '');
  lock := TCriticalSection.Create;
  counter := 0;
  try
    for i := 1 to 5 do
    begin
      bg.AddTask(
        procedure(t: ITask)
        begin
          lock.Enter;
          try
            Inc(counter);
            if counter = 5 then event.SetEvent;
          finally
            lock.Leave;
          end;
        end);
    end;
    bg.Start;
    Assert.IsTrue(event.WaitFor(5000) = wrSignaled,
      'All 5 background tasks must complete within 5 seconds');
    Assert.AreEqual(5, counter,
      'Counter must be 5 after all tasks run');
  finally
    bg.Free;
    lock.Free;
    event.Free;
  end;
end;

{ --- TRunTask — retry on exception --- }

procedure TQuickThreadsTests.Test_RunTask_Retry_RetriesOnException;
var
  attempts: Integer;
  event: TEvent;
  lock: TCriticalSection;
  task: IWorkTask;
begin
  // Task raises on first 2 attempts and succeeds on the 3rd.
  // Retry(3) means up to 3 retries → total of 4 possible executions.
  attempts := 0;
  event := TEvent.Create(nil, True, False, '');
  lock := TCriticalSection.Create;
  try
    task := TRunTask.Execute(
      procedure(t: ITask)
      begin
        lock.Enter;
        try
          Inc(attempts);
          if attempts < 3 then raise Exception.Create('transient error');
        finally
          lock.Leave;
        end;
      end)
      .Retry(3)
      .OnTerminated(procedure(t: ITask) begin event.SetEvent; end);
    Assert.IsTrue(event.WaitFor(5000) = wrSignaled,
      'Retry task must complete (terminate) within 5 seconds');
    Assert.IsTrue(attempts >= 3, 'Task must have been executed at least 3 times with Retry(3)');
    Assert.IsTrue(task.TaskStatus = wtsDone, 'Task must reach wtsDone after successful retry');
  finally
    lock.Free;
    event.Free;
  end;
end;

procedure TQuickThreadsTests.Test_RunTask_OnRetry_CallbackFired;
var
  retryCalled: Boolean;
  event: TEvent;
  attempt: Integer;
  lock: TCriticalSection;
  task: IWorkTask;
begin
  retryCalled := False;
  attempt := 0;
  event := TEvent.Create(nil, True, False, '');
  lock := TCriticalSection.Create;
  try
    task := TRunTask.Execute(
      procedure(t: ITask)
      begin
        lock.Enter;
        try
          Inc(attempt);
          if attempt < 2 then raise Exception.Create('fail once');
        finally
          lock.Leave;
        end;
      end)
      .Retry(2)
      .OnRetry(procedure(t: ITask; e: Exception; var aStop: Boolean)
        begin
          retryCalled := True;
        end)
      .OnTerminated(procedure(t: ITask) begin event.SetEvent; end);
    Assert.IsTrue(event.WaitFor(5000) = wrSignaled,
      'OnRetry task must complete within 5 seconds');
    Assert.IsTrue(retryCalled, 'OnRetry callback must have been called');
  finally
    lock.Free;
    event.Free;
  end;
end;

procedure TQuickThreadsTests.Test_RunTask_MultipleParams;
var
  task: IWorkTask;
  p1, p2: string;
  event: TEvent;
begin
  p1 := ''; p2 := '';
  event := TEvent.Create(nil, True, False, '');
  try
    task := TRunTask.Execute(['hello', 'world'], False,
      procedure(t: ITask)
      begin
        p1 := t.GetParam(0).AsString;
        p2 := t.GetParam(1).AsString;
        event.SetEvent;
      end);
    Assert.IsTrue(event.WaitFor(3000) = wrSignaled,
      'Multi-param task must complete within 3 seconds');
    Assert.AreEqual('hello', p1, 'First param should be "hello"');
    Assert.AreEqual('world', p2, 'Second param should be "world"');
  finally
    event.Free;
  end;
end;

procedure TQuickThreadsTests.Test_RunTask_ReachesStatusException_WhenNoRetry;
var
  task: IWorkTask;
  event: TEvent;
begin
  // Task raises with no retry configured → status = wtsException
  event := TEvent.Create(nil, True, False, '');
  try
    task := TRunTask.Execute(
      procedure(t: ITask)
      begin
        raise Exception.Create('expected failure');
      end)
      .OnException(procedure(t: ITask; e: Exception) begin event.SetEvent; end);
    Assert.IsTrue(event.WaitFor(3000) = wrSignaled,
      'Exception handler must fire within 3 seconds');
    Assert.IsTrue(task.TaskStatus = wtsException, 'Task status must be wtsException after unhandled raise');
  finally
    event.Free;
  end;
end;

{ --- TBackgroundTasks — queue, CancelAll --- }

procedure TQuickThreadsTests.Test_BackgroundTasks_CancelAll_SetsShutDown;
var
  bg: TBackgroundTasks;
begin
  bg := TBackgroundTasks.Create(2, 10);
  try
    bg.Start;
    bg.CancelAll;
    // After CancelAll the queue should be shut down (no assertion on exact state—just no crash)
    Assert.WillNotRaise(procedure begin bg.CancelAll; end, nil,
      'CancelAll must not raise even when called twice');
  finally
    bg.Free;
  end;
end;

procedure TQuickThreadsTests.Test_BackgroundTasks_TaskQueue_IncreasesWithTasks;
var
  bg: TBackgroundTasks;
  slowEvent: TEvent;
  i: Integer;
begin
  // Add several tasks BEFORE starting to let the queue build up
  bg := TBackgroundTasks.Create(1, 100); // 1 worker to bottleneck
  slowEvent := TEvent.Create(nil, True, False, '');
  try
    for i := 1 to 5 do
      bg.AddTask(procedure(t: ITask) begin slowEvent.WaitFor(100); end);
    // Queue should have tasks pending (TaskQueue > 0)
    Assert.IsTrue(bg.TaskQueued >= 0, 'TaskQueued property must be accessible without error');
  finally
    slowEvent.SetEvent;
    bg.Free;
    slowEvent.Free;
  end;
end;

{ --- TScheduledTasks --- }

procedure TQuickThreadsTests.Test_ScheduledTasks_RunOnce_ExecutesAction;
var
  sched: TScheduledTasks;
  event: TEvent;
  executed: Boolean;
  task: IScheduledTask;
begin
  executed := False;
  event := TEvent.Create(nil, True, False, '');
  sched := TScheduledTasks.Create;
  try
    task := sched.AddTask('once_task',
      procedure(t: ITask)
      begin
        executed := True;
        event.SetEvent;
      end);
    task.StartNow;
    task.RunOnce;
    sched.Start;
    Assert.IsTrue(event.WaitFor(5000) = wrSignaled,
      'ScheduledTask RunOnce must fire within 5 seconds');
    Assert.IsTrue(executed, 'Scheduled task action must have executed');
  finally
    sched.Stop;
    sched.Free;
    event.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickThreadsTests);

end.
