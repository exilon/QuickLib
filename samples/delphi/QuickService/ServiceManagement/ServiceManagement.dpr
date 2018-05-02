program ServiceManagement;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Service;

var
  svcname : string;

begin
  try
    svcname := 'aspnet_state';
    cout('Need to run as admin',etWarning);
    if ServiceIsPresent('localhost',svcname) then cout('"%s" service is installed"',[svcname],etSuccess)
      else cout('"%s" service is not installed!"',[svcname],etWarning);
    if GetServiceState('localhost',svcname) = TServiceState.ssStopped then
    begin
      cout('"%s" service is stopped"',[svcname],etWarning);
      cout('Trying to start "%s" service..."',[svcname],etInfo);
      ServiceStart('localhost','aspnet_state');
      Sleep(3000);
      if GetServiceState('localhost','aspnet_state') = TServiceState.ssRunning then cout('%s service is now started!',[svcname],etSuccess);
    end
    else
    begin
       cout('"%s" service is started"',[svcname],etSuccess);
      cout('Trying to stop "%s" service..."',[svcname],etInfo);
      ServiceStop('localhost','aspnet_state');
      Sleep(3000);
      if GetServiceState('localhost','aspnet_state') = TServiceState.ssStopped then cout('%s service is now stopped!',[svcname],etWarning);
    end;
    cout('Press [ENTER] to exit',etInfo);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
