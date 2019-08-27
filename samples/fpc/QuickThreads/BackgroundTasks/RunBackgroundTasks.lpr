program RunBackgroundTasks;

{$IFDEF MSWINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

{$MODE DELPHI}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  DateUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Threads;

type
  TMyTask = class
  private
    fId : Integer;
    fName : string;
  public
    property Id : Integer read fId write fId;
    property Name : string read fName write fName;
    procedure DoJob(task : ITask);
    procedure Retry(task : ITask; aException : Exception; var vStopRetries : Boolean);
    procedure Failed(task : ITask; aException : Exception);
    procedure Finished(task : ITask);
  end;

var
  backgroundtasks : TBackgroundTasks;
  mytask : TMyTask;
  i : Integer;

{ TMyTask }

procedure TMyTask.DoJob(task : ITask);
var
  a, b, i : Integer;
begin
  cout('[%s] task "%s" doing a %s job...',[DateTimeToStr(Now()),fName,task.Param[0].AsString],etInfo);
  Sleep(Random(1000));
  a := Random(100);
  b := Random(5) + 1;

  i := a div b;
  cout('task "%s" result %d / %d = %d',[fName,a,b,i],etSuccess);
end;

procedure TMyTask.Retry(task : ITask; aException : Exception; var vStopRetries : Boolean);
begin
  cout('task "%s" retrying (%s)',[fName,aException.Message],etWarning);
end;

procedure TMyTask.Failed(task : ITask; aException : Exception);
begin
  cout('task "%s" failed (%s)',[fName,aException.Message],etError);
end;

procedure TMyTask.Finished(task : ITask);
begin
  cout('task "%s" finished',[fName],etDebug);
end;

begin
  Console.LogVerbose := LOG_DEBUG;
  try
    backgroundtasks := TBackgroundTasks.Create(10);
    for i := 1 to 100 do
    begin
      mytask := TMyTask.Create;
      mytask.Id := i;
      mytask.Name := 'Task' + i.ToString;
      backgroundtasks.AddTask(['blue'],True,mytask.DoJob
                             ).WaitAndRetry(10,100
                             ).OnRetry(mytask.Retry
                             ).OnException(mytask.Failed
                             ).OnTerminated(mytask.Finished
                             ).Run;
    end;
    backgroundtasks.Start;
    cout('Running tasks in background!',etInfo);
    ConsoleWaitForEnterKey;
    backgroundtasks.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

