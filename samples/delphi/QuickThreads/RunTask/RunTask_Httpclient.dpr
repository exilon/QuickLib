program RunTask_Httpclient;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  Quick.HttpClient,
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
    TRunTask.Execute(
      procedure(task : ITask)
      var
        stream : TStringStream;
        response : IHttpRequestResponse;
      begin
        task.Result := 500;
        cout('Downloading...',etTrace);
        stream := TStringStream.Create;
        try
          response := TJsonHttpClient(task['httpclient'].AsObject).Get(task['url']);
          task.Result := response.StatusCode;
          if response.StatusCode <> 200 then raise Exception.Create(response.StatusText);
        finally
          stream.Free;
        end;
        cout('executed thread',etSuccess);
      end)
    .SetParameter('httpclient',(TJsonHttpClient.Create),True)
    .SetParameter('url','https://github.com/exilon/QuickLib/blob/master/README.md')
    //.RetryForever
    .WaitAndRetry(5,250,2)
    //.WaitAndRetry([250,2000,10000])
    .OnRetry(
      procedure(task : ITask; aException : Exception; var vStopRetries : Boolean)
      begin
        //if error 404 don't try to retry request
        if task.Result = 404 then
        begin
          cout('URL not found. No more retries will be made',etError);
          vStopRetries := True;
        end
        else coutFmt('Retrying downloading (Error: %s / StatusCode: %d)...',[aException.Message,task.Result.AsInteger],etWarning);
      end)
    .OnException(
      procedure(task : ITask; aException : Exception)
      begin
        coutFmt('Exception downloading (Error: %s / StatusCode: %d)...',[aException.Message,task.Result.AsInteger],etError);
      end)
    .OnTerminated(
      procedure(task : ITask)
      begin
        if task.Done then coutFmt('Download "%s" finished ok',[task['url'].AsString],etSuccess)
          else coutFmt('Download "%s" failed after %d retries',[task['url'].AsString,task.NumRetries],etError);
        cout('PRESS <ENTER> TO EXIT',etInfo);
      end)
    .Run;

    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
