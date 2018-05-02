program ConsoleOut;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

{$R *.res}

uses
  SysUtils,
  Quick.Commons,
  Quick.Console;

begin
  try
    coutXY(20,10,'this line will be replaced by the next',etInfo);
    coutXY(20,10,'this line replaces previous',etSuccess);
    cout('Normal line 1',etInfo);
    coutBL('bottom line: 1',etInfo);
    cout('Normal line 2',etInfo);
    coutXY(10,5,'I''m here',etSuccess);
    coutBL('bottom line: 2',etInfo);
    cout('Normal line 3',etInfo);
    coutBL('bottomline: 3',etInfo);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
