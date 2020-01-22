program RunBackgroundtask;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Threads;

type
  TMyJob = class
  private
    fId : Integer;
    fName : string;
  public
    property Id : Integer read fId write fId;
    property Name : string read fName write fName;
    function DoJob : Integer;
  end;

var
  i : Integer;
  backgroundtasks : TBackgroundTasks;
  myjob : TMyJob;

{ TMyJob }

function TMyJob.DoJob : Integer;
var
  a, b : Integer;
begin
  cout('task %d doing job...',[fId],etInfo);
  Sleep(Random(1000));
  a := Random(100);
  b := Random(5) + 1;

  Result := a div b;
  cout('task %d result %d / %d = %d',[fId,a,b,Result],etSuccess);
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  Console.LogVerbose := LOG_DEBUG;
  try
    { TODO -oUser -cConsole Main : Insert code here }
    backgroundtasks := TBackgroundTasks.Create(10);
    for i := 1 to 100 do
    begin
      myjob := TMyJob.Create;
      myjob.Id := i;
      myjob.Name := 'Task' + i.ToString;
      backgroundtasks.AddTask(
                              procedure(task : ITask)
                              begin
                                cout('task %d started',[TMyJob(task['Job']).Id],etDebug);
                                task.Result := TMyJob(task['Job']).DoJob;
                              end
                            ).SetParameter('Job',myjob,True
                            ).WaitAndRetry(10,100
                            ).OnRetry(
                              procedure(task : ITask; aException : Exception;  var vStopRetries : Boolean)
                              begin
                                if not aException.Message.Contains('Division by zero') then vStopRetries := True
                                  else cout('task "%s" retried %d/%d (%s)',[TMyJob(task['Job']).Name,task.NumRetries,task.MaxRetries,aException.Message],etWarning);
                              end
                            ).OnException(
                              procedure(task : ITask; aException : Exception)
                              begin
                                cout('task %d failed (%s)',[TMyJob(task['Job']).Id,aException.Message],etError);
                              end
                            ).OnTerminated(
                              procedure(task : ITask)
                              begin
                                cout('task %d finished result = %d',[TMyJob(task['Job']).Id,task.Result.AsInteger],etDebug);
                              end
                            ).Run;
    end;
    backgroundtasks.Start;
    cout('Running tasks in background!',etInfo);
    ConsoleWaitForEnterKey;
    coutFmt('Task gueue size = %d',[backgroundtasks.TaskQueued],etInfo);
    backgroundtasks.Free;
    cout('finished tasks!!!!',etInfo);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
