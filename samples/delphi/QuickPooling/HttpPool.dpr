program HttpPool;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  System.SysUtils,
  System.Net.HttpClient,
  Quick.Commons,
  Quick.Console,
  Quick.Threads,
  Quick.Pooling;

var
  pool : IObjectPool<THTTPClient>;
  tasks : TBackgroundTasks;
  i : Integer;

begin
  try
    ReportMemoryLeaksOnShutdown := True;
    pool := TObjectPool<THTTPClient>.Create(5,5000,procedure(var aInstance : THTTPClient)
        begin
          aInstance := THTTPClient.Create;
        end);
    tasks := TBackgroundTasks.Create(20);
    for i := 0 to 100 do
    begin
      tasks.AddTask(procedure(task : ITask)
        var
          httpcli : THTTPClient;
          statuscode : Integer;
          poolitem : IPoolItem<THTTPClient>;
        begin
          poolitem := pool.Get;
          cout('Got connection pool: %d',[poolitem.ItemIndex],etInfo);
          httpcli := poolitem.Item;
          statuscode := httpcli.Get('http://www.google.com').StatusCode;
          if statuscode = 200 then cout('Download ok',etSuccess);
          //Sleep(Random(2000));
          cout(statuscode.ToString,etInfo);
        end).Run;
    end;
    tasks.Start;
    ConsoleWaitForEnterKey;
    tasks.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
