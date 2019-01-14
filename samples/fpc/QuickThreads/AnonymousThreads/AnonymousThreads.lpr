program AnonymousThreads;

{$MODE DELPHI}

{$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  Quick.Commons,
  Quick.Console,
  SysUtils,
  Quick.Threads;

type
  TMyProcs = class
    class procedure DoWork;
    class procedure DoTerminate;
  end;

  class procedure TMyProcs.DoWork;
  var
    i : Integer;
  begin
    for i := 0 to 10 do cout('Working %d',[i],etTrace);
    cout('executed thread',etSuccess);
  end;

  class procedure TMyProcs.DoTerminate;
  begin
    cout('terminated thread',etSuccess);
    cout('PRESS <ENTER> TO EXIT',TLogEventType.etInfo);
  end;

begin
  try
    console.LogVerbose := LOG_DEBUG;
    TAnonymousThread.Execute(TMyProcs.DoWork)
    .OnTerminate(TMyProcs.DoTerminate).Start;

    cout('Press <Enter> to exit',TLogEventType.etWarning);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
