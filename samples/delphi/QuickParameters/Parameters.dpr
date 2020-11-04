program Parameters;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Parameters;

type
  TCommand = (Copy, Move, Remove);
  TMyMode = (mdAdd, mdSelect, mdRemove);

  [CommandDescription('Simple console application example with Quick.Parameters')]
  TMyParameter = class(TParameters)
  private
    fCommand : TCommand;
    fHost : string;
    fPort : Integer;
    fRetries : Integer;
    fUseTCP : Boolean;
    fConfigFile: string;
    fSilent: Boolean;
    fMyMode: TMyMode;
    fLogErrorsConsole: Boolean;
    fLogErrors: Boolean;
    fShowReport: Boolean;
  published
    [ParamCommand(1)]
    [ParamRequired]
    [ParamHelp('Command action.','command-action')]
    property Command : TCommand read fCommand write fCommand;

    [ParamName('HostName'),ParamRequired]
    [ParamHelp('Define host to connect.','host')]
    property Host : string read fHost write fHost;

    [ParamName('Port','p')]
    [ParamValueIsNextParam]
    [ParamHelp('Define Port to connect (default 80)','port')]
    property Port : Integer read fPort write fPort;

    [ParamHelp('Number of max retries.')]
    property Retries : Integer read fRetries write fRetries;

    [ParamHelp('Path to config.','path')]
    [ParamName('Config-file')]
    property ConfigFile : String read fConfigFile write fConfigFile;

    [ParamHelp('Use TCP connection if present.')]
    property UseTCP : Boolean read fUseTCP write fUseTCP;

    [ParamHelp('Silent mode.')]
    property Silent : Boolean read fSilent write fSilent;

    [ParamHelp('Modes (mdAdd, mdSelect, mdRemove)')]
    property Mode : TMyMode read fMyMode write fMyMode;

    [ParamHelp('Show report on finish.')]
    property ShowReport : Boolean read fShowReport write fShowReport;

    [ParamHelp('Log errors to file.')]
    [ParamName('LogErrors-to-file')]
    property LogErrorsFile : Boolean read fLogErrors write fLogErrors;

    [ParamHelp('Log errors to console.')]
    [ParamName('LogErrors-to-console')]
    property LogErrorsConsole : Boolean read fLogErrorsConsole write fLogErrorsConsole;
  end;

var
  params : TMyParameter;

begin
  try
    params := TMyParameter.Create;
    if params.Help then Exit;
      
    coutFmt('Command = %d',[Integer(params.Command)],etInfo);
    coutFmt('Host = %s',[params.Host],etInfo);
    coutFmt('Port = %d',[params.Port],etInfo);
    coutFmt('Retries = %d',[params.Retries],etInfo);
    coutFmt('Config-File = %s',[params.ConfigFile],etInfo);
    coutFmt('Mode = %d',[Integer(params.Mode)],etInfo);
    if params.UseTCP then cout('Use tcp detected',ccYellow);
    if params.Silent then cout('Silent mode detected',ccYellow);
    if params.ShowReport then cout('ShowReport detected',ccYellow);
    if params.LogErrorsFile then cout('LogErrorsFile detected',ccYellow);
    if params.LogErrorsConsole then cout('LogErrorsConsole detected',ccYellow);
    cout('Press <ENTER> to Exit',ccYellow);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
    begin
      cout('%s : %s',[E.ClassName,e.Message],etError);
      cout('Type "Parameters --help" to get documentation',ccYellow);
    end;
  end;
end.
