program AnonymousThread;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  Quick.Commons,
  Quick.Console,
  System.SysUtils,
  Quick.Threads;

begin
  try
    ReportMemoryLeaksOnShutdown := True;

    TAnonymousThread.Execute(
      procedure
      var
        i : Integer;
        a : Integer;
      begin
        for i := 0 to 100 do cout('Working %d',[i],etTrace);
        a := i Div (Random(3));
        cout('executed thread',etSuccess);
      end
    ).OnException(
      procedure(aException : Exception)
      begin
        cout('Exception %s',[aException.Message],etError);
      end
    ).OnTerminate(
      procedure
      begin
        cout('terminated thread',etSuccess);
        cout('PRESS <ENTER> TO EXIT',etInfo);
      end
    ).Start;

    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
