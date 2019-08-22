program RunTask;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  Quick.Commons,
  Quick.Console,
  System.SysUtils,
  Quick.Threads;

var
  currentdownload : Integer;

begin
  try
    ReportMemoryLeaksOnShutdown := True;
    currentdownload := 0;
    TRunTask.Execute([currentdownload],False,
      procedure(task : ITask)
      var
        i : Integer;
        a : Integer;
      begin
        //simulate a download task with random fail
        task.Result := 500;
        for i := 0 to 10 do
        begin
          task['currentdownload'] := i*10;
          //task.Param[0] := i*10;
          cout('Downloading %d%%...',[i*10],etTrace);
          Sleep(100);
          a := i Div (Random(5));
        end;
        task['statuscode'] := 200;
        task.Result := 200;
        cout('executed thread',etSuccess);
      end)
    .SetParameter('statuscode',0,False)
    //.RetryForever
    .WaitAndRetry(5,250,2)
    //.WaitAndRetry([250,2000,10000])
    .OnRetry(
      procedure(task : ITask; aException : Exception; var vStopRetries : Boolean)
      begin
        coutFmt('Failed at %d%%. Retrying downloading (Status code: %d [%s])...',[task['currentdownload'].AsInteger,task.Result.AsInteger,aException.Message],etWarning);
      end)
    .OnException(
      procedure(task : ITask; aException : Exception)
      begin
        cout('Exception downloading %d%% : %s',[task['currentdownload'].AsInteger,aException.Message],etError);
      end)
    .OnTerminated(
      procedure(task : ITask)
      begin
        if task.Done then cout('Task finished ok',etSuccess)
          else coutFmt('Task failed after %d retries',[task.NumRetries],etError);
        cout('PRESS <ENTER> TO EXIT',etInfo);
      end)
    .Run;

    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
