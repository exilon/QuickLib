program RunScheduledtask;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  DateUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Threads;

type
  TMyJob = class
  private
    fId : Integer;
    fName : string;
  public
    destructor Destroy; override;
    property Id : Integer read fId write fId;
    property Name : string read fName write fName;
    procedure DoJob;
  end;

var
  scheduledtasks : TScheduledTasks;
  myjob : TMyJob;
  ScheduledDate : TDateTime;
  ExpirationDate : TDateTime;

{ TMyTask }

destructor TMyJob.Destroy;
begin

  inherited;
end;

procedure TMyJob.DoJob;
var
  a, b, i : Integer;
begin
  cout('[%s] task "%s" doing job...',[DateTimeToStr(Now()),fName],etInfo);
  //Sleep(Random(1000));
  a := Random(100);
  b := Random(5);// + 1;

  i := a div b;
  cout('task "%s" result %d / %d = %d',[fName,a,b,i],etSuccess);
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  Console.LogVerbose := LOG_DEBUG;
  try
    scheduledtasks := TScheduledTasks.Create;
    scheduledtasks.RemoveTaskAfterExpiration := True;
    scheduledtasks.FaultPolicy.MaxRetries := 5;
    myjob := TMyJob.Create;
    myjob.Id := 1;
    myjob.Name := 'Run now and repeat every 1 second for 30 times';
    scheduledtasks.AddTask('Task1',[myjob,1],True,
                            procedure(task : ITask)
                            begin
                              cout('task "%s" started',[TMyJob(task.Param[0]).Name],etDebug);
                              TMyJob(task.Param[0]).DoJob;
                            end
                          ).WaitAndRetry(10,100
                          ).OnException(
                            procedure(task : ITask; aException : Exception)
                            begin
                              cout('task "%s" failed (%s)',[TMyJob(task.Param[0]).Name,aException.Message],etError);
                            end
                          ).OnRetry(
                            procedure(task : ITask; aException : Exception;  var vStopRetries : Boolean)
                            begin
                              if not aException.Message.Contains('Division by zero') then vStopRetries := True
                                else cout('task "%s" retried %d/%d (%s)',[TMyJob(task.Param[0]).Name,task.NumRetries,task.MaxRetries,aException.Message],etWarning);
                            end
                          ).OnTerminated(
                            procedure(task : ITask)
                            begin
                              cout('task "%s" finished',[TMyJob(task.Param[0]).Name],etDebug);
                            end
                          ).OnExpired(
                            procedure(task : ITask)
                            begin
                              cout('task "%s" expired',[TMyJob(task.Param[0]).Name],etWarning);
                            end
                          ).RepeatEvery(1,TTimeMeasure.tmSeconds,30);

    myjob := TMyJob.Create;
    myjob.Id := 2;
    myjob.Name := 'Run now, repeat every 1 second forever';
    scheduledtasks.AddTask('Task2',[myjob,32,true,3.2,myjob.ClassType],True,
                            procedure(task : ITask)
                            begin
                              cout('task "%s" started with params(Int=%d / Bool=%s / Float=%s /Class=%s)',[TMyJob(task.Param[0]).Name,task.Param[1].AsInteger,task.Param[2].AsString,task.Param[3].AsString,task.Param[4].AsString],etDebug);
                              TMyJob(task.Param[0]).DoJob;
                            end
                          ).WaitAndRetry(10,100
                          ).OnException(
                            procedure(task : ITask; aException : Exception)
                            begin
                              cout('task "%s" failed (%s)',[TMyJob(task.Param[0]).Name,aException.Message],etError);
                            end
                          ).OnTerminated(
                            procedure(task : ITask)
                            begin
                              cout('task "%s" finished',[TMyJob(task.Param[0]).Name],etDebug);
                            end
                          ).OnExpired(
                            procedure(task : ITask)
                            begin
                              cout('task "%s" expired',[TMyJob(task.Param[0]).Name],etWarning);
                            end
                          ).StartAt(Now()
                          ).RepeatEvery(1,TTimeMeasure.tmSeconds);

    ScheduledDate := IncSecond(Now(),5);
    ExpirationDate := IncSecond(ScheduledDate,10);

    myjob := TMyJob.Create;
    myjob.Id := 3;
    myjob.Name := Format('Run at %s and repeat every 1 second until %s',[DateTimeToStr(ScheduledDate),DateTimeToStr(ExpirationDate)]);


    scheduledtasks.AddTask('Task3',[myjob],True,
                            procedure(task : ITask)
                            begin
                              cout('task "%s" started',[TMyJob(task.Param[0]).Name],etDebug);
                              TMyJob(task.Param[0]).DoJob;
                            end
                          ).WaitAndRetry(10,100
                          ).OnException(
                            procedure(task : ITask; aException : Exception)
                            begin
                              cout('task "%s" failed (%s)',[TMyJob(task.Param[0]).Name,aException.Message],etError);
                            end
                          ).OnTerminated(
                            procedure(task : ITask)
                            begin
                              cout('task "%s" finished',[TMyJob(task.Param[0]).Name],etDebug);
                            end
                          ).OnExpired(
                            procedure(task : ITask)
                            begin
                              cout('task "%s" expired',[TMyJob(task.Param[0]).Name],etWarning);
                            end
                          ).StartAt(ScheduledDate
                          ).RepeatEvery(1,TTimeMeasure.tmSeconds,ExpirationDate);


    ScheduledDate := IncSecond(Now(),30);

    myjob := TMyJob.Create;
    myjob.Id := 4;
    myjob.Name := Format('Run at %s and repeat only one time',[DateTimeToStr(ScheduledDate),DateTimeToStr(ExpirationDate)]);


    scheduledtasks.AddTask('Task4',[myjob],True,
                            procedure(task : ITask)
                            begin
                              cout('task "%s" started',[TMyJob(task.Param[0]).Name],etDebug);
                              TMyJob(task.Param[0]).DoJob;
                            end
                          ).WaitAndRetry(10,100
                          ).OnException(
                            procedure(task : ITask; aException : Exception)
                            begin
                              cout('task "%s" failed (%s)',[TMyJob(task.Param[0]).Name,aException.Message],etError);
                            end
                          ).OnTerminated(
                            procedure(task : ITask)
                            begin
                              cout('task "%s" finished',[TMyJob(task.Param[0]).Name],etDebug);
                            end
                          ).OnExpired(
                            procedure(task : ITask)
                            begin
                              cout('task "%s" expired',[TMyJob(task.Param[0]).Name],etWarning);
                            end
                          ).StartAt(ScheduledDate
                          ).RunOnce;


    scheduledtasks.Start;

    cout('Running tasks in background!',etInfo);
    Readln;
    cout('Stopping task2...',etWarning);
    scheduledtasks.GetTask('Task2').Cancel;
    Readln;
    scheduledtasks.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
