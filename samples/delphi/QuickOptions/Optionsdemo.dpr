program Optionsdemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Options,
  Quick.Options.Serializer.Json,
  Quick.Options.Serializer.Yaml;

type
  TLogConfig = class
  private
    fVerbose : Boolean;
    fTimePrecission : Boolean;
    fEnvironment : string;
  published
    property Verbose : Boolean read fVerbose write fVerbose;
    property TimePrecission : Boolean read fTimePrecission write fTimePrecission;
    [Required, StringLength(5)]
    property Environment : string read fEnvironment write fEnvironment;
  end;

  TLoggingOptions = class(TOptions)
  private
    fPath : string;
    fConfig : TLogConfig;
  public
    constructor Create;
    destructor Destroy; override;
  published
    [Required, StringLength(255,'Path too long')]
    property Path : string read fPath write fPath;
    [Required]
    property Config : TLogConfig read fConfig write fConfig;
  end;

  TGlobalOptions = class(TOptions)
  private
    fStartMinimized : Boolean;
    fServers : TArray<string>;
    fLevel : Double;
  published
    property StartMinimized : Boolean read fStartMinimized write fStartMinimized;
    property Servers : TArray<string> read fServers write fServers;
    [Range(0.0,5.2)]
    property Level : Double read fLevel write fLevel;
  end;

  TUIOptions = class(TOptions)
  private
    fForeColor : Integer;
    fBackColor : Integer;
  published
    [Range(0, 255)]
    property ForeColor : Integer read fForeColor write fForeColor;
    [Range(0, 255,'Out of range')]
    property BackgroundColor : Integer read fBackColor write fBackColor;
  end;

var
  Options : TFileOptionsContainer;
  LoggingOptions : TLoggingOptions;
  GlobalOptions : TGlobalOptions;
  UIOptions : TUIOptions;

{ TLoggingOptions }

constructor TLoggingOptions.Create;
begin
  fConfig := TLogConfig.Create;
end;

destructor TLoggingOptions.Destroy;
begin
  fConfig.Free;
  inherited;
end;

begin
  try
    ReportMemoryLeaksOnShutdown := True;
    Options := TFileOptionsContainer.Create(TJsonOptionsSerializer.Create('.\options.conf'),True);
    Options.OnFileModified := procedure
                              begin
                                cout('Detected config file modification!',etWarning);
                              end;

    Options.AddSection<TLoggingOptions>('Logging').ConfigureOptions(procedure(aOptions : TLoggingOptions)
                                                            begin
                                                              aOptions.Path := 'C:\';
                                                              aOptions.Config.Verbose := True;
                                                              aOptions.Config.Environment := 'PRO';
                                                            end
                                                            ).ValidateOptions;
    Options.AddSection<TGlobalOptions>('GlobalOptions').ConfigureOptions(procedure(aOptions : TGlobalOptions)
                                                begin
                                                  aOptions.StartMinimized := True;
                                                  aOptions.Servers := ['ServerOne','ServerTwo','ServerThree','ServerFour'];
                                                end
                                                ).ValidateOptions;

    Options.AddSection(TUIOptions).ConfigureOptions<TUIOptions>(procedure(aOptions : TUIOptions)
                                                begin
                                                  aOptions.ForeColor := 77;
                                                  aOptions.BackgroundColor := 100;
                                                end
                                                ).ValidateOptions;

    Options.Load;
    LoggingOptions := Options.GetSection<TLoggingOptions>;
    UIOptions := Options.GetSectionInterface<TUIOptions>.Value;
    GlobalOptions := Options.GetSectionInterface<TGlobalOptions>.Value;

    coutFmt('Logging.Path = %s',[LoggingOptions.Path],etInfo);
    coutFmt('Logging.Config.Environment = %s',[LoggingOptions.Config.Environment],etInfo);
    coutFmt('UIOptions.BackgroundColor = %d',[UIOptions.BackgroundColor],etInfo);
    coutFmt('GlobalOptions.StarMinimized = %s',[BoolToStr(GlobalOptions.StartMinimized,True)],etInfo);

    LoggingOptions.Path := 'D:\OtherTest';
    UIOptions.BackgroundColor := 120;

    Options.Save;

    //get instance of options section
    LoggingOptions := Options.GetSection<TLoggingOptions>;
    //LoggingOptions := Options.GetSection<TLoggingOptions>.Value;

    //gets value from IOptions<TUIOptions> interface (depency injection)
    UIOptions := Options.GetSectionInterface<TUIOptions>.Value;

    coutFmt('Logging.Path = %s',[LoggingOptions.Path],etInfo);
    coutFmt('UIOptions.BackgroundColor = %d',[UIOptions.BackgroundColor],etInfo);

    Readln;
    Options.Free;
  except
    on E: Exception do
      coutFmt('%s:%s',[E.ClassName,E.Message],etError);
  end;
end.
