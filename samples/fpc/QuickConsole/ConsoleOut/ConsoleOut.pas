program ConsoleOut;

{$IFDEF MSWINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

{$MODE DELPHI}

{$R *.res}

uses
  SysUtils,
  Quick.Commons,
  Quick.Console;

begin
  Application.Title:='ConsoleDemo';
  {$IFDEF MSWINDOWS}
  Application.Title:='ConsoleDemo';
  {$ENDIF}
  try
    Console.LogVerbose := LOG_DEBUG;
    writeln('Console Out Example');
    coutXY(10,10,'this line will be replaced by the next',etInfo);
    coutXY(10,10,'this line replaces previous',etSuccess);
    cout('Normal line 1',etInfo);
    coutBL('bottom line: 1',etInfo);
    cout('Normal line 2',etDebug);
    coutXY(10,5,'I''m here',etSuccess);
    cout('Normal line 3',etSuccess);
    coutBL('bottom line: 2',etInfo);
    coutBL('bottom line: 3',etInfo);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
