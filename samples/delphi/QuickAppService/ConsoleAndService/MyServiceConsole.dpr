program MyServiceConsole;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

{$R *.res}

uses
  SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.AppService;

type

  {$IFDEF FPC}
  TSrvFactory = class
    class procedure CreateMyService;
  end;
  {$ENDIF}

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

{$IFDEF FPC}
class procedure TSrvFactory.CreateMyService;
begin
 MyService := TMyService.Create;
end;
{$ENDIF}

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
        AppService.DesktopInteraction := False;
        {$IFDEF FPC}
        AppService.OnStart := TSrvFactory.CreateMyService;
        {$ELSE}
        //you can pass an anonymous method to events
        AppService.OnStart := procedure
                              begin
                                MyService := TMyService.Create;
                              end;
        {$ENDIF}
        AppService.OnExecute := MyService.Execute;
        AppService.OnStop := MyService.Free;
        AppService.CheckParams;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
