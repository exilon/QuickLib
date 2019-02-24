program MyServiceConsole;

{$APPTYPE CONSOLE}

{$MODE DELPHI}

{$R *.res}

uses
  SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.AppService;

type

  TSrvFactory = class
    class procedure CreateMyService;
  end;

  TMyService = class
  public
    procedure Execute;
  end;

  procedure TMyService.Execute;
  begin
    //your code
  end;

var

  MyService : TMyService;

class procedure TSrvFactory.CreateMyService;
begin
 MyService := TMyService.Create;
end;

begin
  try
    if not AppService.IsRunningAsService then
    begin
      cout('Running in console mode',etInfo);
      MyService := TMyService.Create;
      MyService.Execute;
      cout('Press [Enter] to exit',etInfo);
      ConsoleWaitForEnterKey;
      cout('Closing app...',etInfo);
      MyService.Free;
    end
    else
    begin
        AppService.ServiceName := 'MyService';
        AppService.DisplayName := 'MyServicesvc';
        AppService.OnStart := TSrvFactory.CreateMyService;
        AppService.OnExecute := MyService.Execute;
        AppService.OnStop := MyService.Free;
        AppService.CheckParams;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
