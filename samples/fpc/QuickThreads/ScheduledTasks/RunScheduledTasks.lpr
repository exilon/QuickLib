program RunScheduledTasks;

{$APPTYPE CONSOLE}

{$MODE DELPHI}

uses
  SysUtils,
  DateUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Threads;

type

  { TMyJob }

  TMyJob = class
  private
    fId : Integer;
    fName : string;
  public
    property Id : Integer read fId write fId;
    property Name : string read fName write fName;
    procedure DoJob(task : ITask);
    procedure Failed(task : ITask; aException : Exception);
    procedure Retry(task : ITask; aException : Exception; var vStopRetries : Boolean);
    procedure Finished(task : ITask);
    procedure Expired(task : ITask);
  end;

var
  scheduledtasks : TScheduledTasks;
  myjob : TMyJob;
  ScheduledDate : TDateTime;
  ExpirationDate : TDateTime;

{ TMyJob }

procedure TMyJob.DoJob(task : ITask);
var
  a, b, i : Integer;
begin
  cout('[%s] task "%s" doing a %s job %d...',[DateTimeToStr(Now()),fName,task.Param[0].AsString,task.Param[1].AsInteger],etInfo);
  Sleep(Random(1000));
  a := Random(100);
  b := Random(5) + 1;

  i := a div b;
  task.Result := i;
  cout('task "%s" result %d / %d = %d',[fName,a,b,i],etSuccess);
end;

procedure TMyJob.Failed(task : ITask; aException : Exception);
begin
  cout('task "%s" failed (%s)',[fName,aException.Message],etError);
end;

procedure TMyJob.Retry(task: ITask; aException: Exception; var vStopRetries: Boolean);
begin
  cout('task "%s" retrying %d/%d (%s)',[fName,task.NumRetries,task.MaxRetries,aException.Message],etWarning);
end;

procedure TMyJob.Finished(task : ITask);
begin
  cout('task "%s" finished (Result = %d)',[fName,task.Result.AsInteger],etDebug);
end;

procedure TMyJob.Expired(task : ITask);
begin
  cout('task "%s" expired',[fName],etWarning);
end;

begin
  Console.LogVerbose := LOG_DEBUG;
  try
    scheduledtasks := TScheduledTasks.Create;
    scheduledtasks.RemoveTaskAfterExpiration := True;

    myjob := TMyJob.Create;
    myjob.Id := 1;
    myjob.Name := 'Run now and repeat every 1 minute for 5 times';
    scheduledtasks.AddTask('Task1',['blue',7],True,myjob.DoJob
                          ).WaitAndRetry(2,100
                          ).OnRetry(myjob.Retry
                          ).OnException(myjob.Failed
                          ).OnTerminated(myjob.Finished
                          ).OnExpired(myjob.Expired
                          ).RepeatEvery(1,TTimeMeasure.tmSeconds,5);

    myjob := TMyJob.Create;
    myjob.Id := 2;
    myjob.Name := 'Run now and repeat every 1 second forever';
    scheduledtasks.AddTask('Task2',['red',14],True,myjob.DoJob
                          ).WaitAndRetry(2,100
                          ).OnRetry(myjob.Retry
                          ).OnException(myjob.Failed
                          ).OnTerminated(myjob.Finished
                          ).OnExpired(myjob.Expired
                          ).StartAt(Now()
                          ).RepeatEvery(1,TTimeMeasure.tmseconds);

    ScheduledDate := IncSecond(Now(),5);
    ExpirationDate := IncSecond(ScheduledDate,10);

    myjob := TMyJob.Create;
    myjob.Id := 3;
    myjob.Name := Format('Run at %s and repeat every 1 second until %s',[DateTimeToStr(ScheduledDate),DateTimeToStr(ExpirationDate)]);
    scheduledtasks.AddTask('Task3',['white',21],True,myjob.DoJob
                          ).WaitAndRetry(2,100
                          ).OnRetry(myjob.Retry
                          ).OnException(myjob.Failed
                          ).OnTerminated(myjob.Finished
                          ).OnExpired(myjob.Expired
                          ).StartAt(ScheduledDate
                          ).RepeatEvery(1,TTimeMeasure.tmSeconds,ExpirationDate);


    scheduledtasks.Start;
    cout('Running tasks in background!',etInfo);
    ConsoleWaitForEnterKey;
    scheduledtasks.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
