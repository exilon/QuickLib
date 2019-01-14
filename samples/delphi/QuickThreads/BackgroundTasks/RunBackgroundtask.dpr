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
    procedure DoJob;
  end;

var
  i : Integer;
  backgroundtasks : TBackgroundTasks;
  myjob : TMyJob;

{ TMyJob }

procedure TMyJob.DoJob;
var
  a, b, i : Integer;
begin
  cout('task %d doing job...',[fId],etInfo);
  Sleep(Random(1000));
  a := Random(100);
  b := Random(5) + 1;

  i := a div b;
  cout('task %d result %d / %d = %d',[fId,a,b,i],etSuccess);
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
      backgroundtasks.AddTask([myjob],False,
                              procedure(task : ITask)
                              begin
                                cout('task %d started',[TMyJob(task.Param[0]).Id],etDebug);
                                TMyJob(task.Param[0].AsObject).DoJob;
                              end
                            ).OnException(
                              procedure(task : ITask; aException : Exception)
                              begin
                                cout('task %d failed (%s)',[TMyJob(task.Param[0]).Id,aException.Message],etError);
                              end
                            ).OnTerminated(
                              procedure(task : ITask)
                              begin
                                cout('task %d finished',[TMyJob(task.Param[0]).Id],etDebug);
                                TMyJob(task.Param[0]).Free;
                              end
                            ).Run;
    end;
    backgroundtasks.Start;
    //backgroundtasks.RunAndFreeOnTerminate;
    cout('Running tasks in background!',etInfo);
    ConsoleWaitForEnterKey;
    backgroundtasks.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
