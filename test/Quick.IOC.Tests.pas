unit Quick.IOC.Tests;

{ ***************************************************************************
  Modified : 05/07/2025
 *************************************************************************** }

interface

uses
  DUnitX.TestFramework,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  Quick.Options,
  Quick.IOC;

type
  // Test interfaces
  ILogger = interface
  ['{47729BFC-8E7E-4E8F-8ADE-97A3CED6C593}']
    procedure Log(const msg: string);
  end;

  IUserService = interface
  ['{0E7F826B-4C6B-4122-B65C-746B3EB5F757}']
    function GetUserName: string;
  end;

  IEmailService = interface
  ['{76C8C593-DEE4-439D-96F6-7E8058FF1870}']
    procedure SendEmail(const mailto, subject, body: string);
  end;

  // classes implementation
  TConsoleLogger = class(TInterfacedObject, ILogger)
  private
    FLastMessage: string;
  public
    procedure Log(const msg: string);
    property LastMessage: string read FLastMessage;
  end;

  TFileLogger = class(TInterfacedObject, ILogger)
  private
    FFileName: string;
    FLastMessage: string;
  public
    constructor Create(const AFileName: string);
    procedure Log(const msg: string);
    property FileName: string read FFileName;
    property LastMessage: string read FLastMessage;
  end;

  TUserService = class(TInterfacedObject, IUserService)
  private
    FLogger: ILogger;
  public
    constructor Create(logger: ILogger);
    function GetUserName: string;
  end;

  TEmailService = class(TInterfacedObject, IEmailService)
  private
    FLogger: ILogger;
  public
    constructor Create(logger: ILogger);
    procedure SendEmail(const mailto, subject, body: string);
  end;

  // Options class for testing RegisterOptions
  TAppSettings = class(TOptions)
  private
    FAppName: string;
    FMaxConnections: Integer;
  published
    property AppName: string read FAppName write FAppName;
    property MaxConnections: Integer read FMaxConnections write FMaxConnections;
  end;

  [TestFixture]
  TQuickIOCTests = class(TObject)
  private
    FContainer: TIocContainer;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Test_RegisterType_Transient;
    [Test]
    procedure Test_RegisterType_Singleton;
    [Test]
    procedure Test_RegisterType_WithName;
    [Test]
    procedure Test_RegisterInstance;
    [Test]
    procedure Test_Resolve_Interface;
    [Test]
    procedure Test_Resolve_WithDependencies;
    [Test]
    procedure Test_Resolve_WithName;
    [Test]
    procedure Test_IsRegistered;
    [Test]
    procedure Test_ResolveAll;
    [Test]
    procedure Test_RegisterFactory;
    { Additional coverage }
    [Test]
    procedure Test_Resolve_Unregistered_Raises;
    [Test]
    procedure Test_RegisterType_DelegateTo;
    [Test]
    procedure Test_RegisterOptions_WithInstance;
    [Test]
    procedure Test_RegisterOptions_WithConfigureProc;
    [Test]
    procedure Test_Build_ResolvesSingletons;
    [Test]
    procedure Test_Singleton_SameInstance_AcrossResolve;
    [Test]
    procedure Test_Transient_DifferentInstance_EachResolve;
    [Test]
    procedure Test_GlobalContainer_IsNotNil;
    [Test]
    procedure Test_IsRegistered_WithImplementation;
    [Test]
    procedure Test_ResolveAll_EmptyWhenNotRegistered;
  end;

implementation

{ TConsoleLogger }
procedure TConsoleLogger.Log(const msg: string);
begin
  FLastMessage := msg;
end;

{ TFileLogger }
constructor TFileLogger.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

procedure TFileLogger.Log(const msg: string);
begin
  FLastMessage := msg;
end;

{ TUserService }
constructor TUserService.Create(logger: ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

function TUserService.GetUserName: string;
begin
  FLogger.Log('Getting username');
  Result := 'TestUser';
end;

{ TEmailService }
constructor TEmailService.Create(logger: ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

procedure TEmailService.SendEmail(const mailto, subject, body: string);
begin
  FLogger.Log(Format('Sending email to %s: %s', [mailto, subject]));
end;

{ TQuickIOCTests }
procedure TQuickIOCTests.SetUp;
begin
  FContainer := TIocContainer.Create;
end;

procedure TQuickIOCTests.TearDown;
begin
  FContainer.Free;
end;

procedure TQuickIOCTests.Test_RegisterType_Transient;
var
  logger1, logger2: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  logger1 := FContainer.Resolve<ILogger>;
  logger2 := FContainer.Resolve<ILogger>;
  Assert.IsNotNull(logger1, 'Logger1 should not be nil');
  Assert.IsNotNull(logger2, 'Logger2 should not be nil');
  Assert.AreNotSame(logger1, logger2, 'Transient instances should be different');
end;

procedure TQuickIOCTests.Test_RegisterType_Singleton;
var
  logger1, logger2: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  logger1 := FContainer.Resolve<ILogger>;
  logger2 := FContainer.Resolve<ILogger>;
  Assert.IsNotNull(logger1, 'Logger1 should not be nil');
  Assert.IsNotNull(logger2, 'Logger2 should not be nil');
  Assert.AreSame(logger1, logger2, 'Singleton instances should be the same');
end;

procedure TQuickIOCTests.Test_RegisterType_WithName;
var
  logger1, logger2: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>('Logger1');
  FContainer.RegisterType<ILogger, TConsoleLogger>('Logger2');
  logger1 := FContainer.Resolve<ILogger>('Logger1');
  logger2 := FContainer.Resolve<ILogger>('Logger2');
  Assert.IsNotNull(logger1, 'Logger1 should not be nil');
  Assert.IsNotNull(logger2, 'Logger2 should not be nil');
  Assert.AreNotSame(logger1, logger2, 'Named instances should be different');
end;

procedure TQuickIOCTests.Test_RegisterInstance;
var
  instance: TConsoleLogger;
  resolved: ILogger;
begin
  instance := TConsoleLogger.Create;
  FContainer.RegisterInstance<ILogger>(instance);
  resolved := FContainer.Resolve<ILogger>();
  Assert.IsNotNull(resolved, 'Resolved instance should not be nil');
  Assert.AreSame(instance, TObject(resolved), 'Should resolve the same instance');
end;

procedure TQuickIOCTests.Test_Resolve_Interface;
var
  logger: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>;
  logger := FContainer.Resolve<ILogger>;
  Assert.IsNotNull(logger, 'Should resolve interface');
  Assert.IsTrue(TObject(logger) is TConsoleLogger, 'Should resolve correct implementation');
end;

procedure TQuickIOCTests.Test_Resolve_WithDependencies;
var
  userService: IUserService;
  logger: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  FContainer.RegisterType<IUserService, TUserService>;
  userService := FContainer.Resolve<IUserService>;
  logger := FContainer.Resolve<ILogger>;
  Assert.IsNotNull(userService, 'UserService should not be nil');
  Assert.IsNotNull(logger, 'Logger should not be nil');
  Assert.AreEqual('TestUser', userService.GetUserName, 'Should get correct username');
  Assert.AreEqual('Getting username', TConsoleLogger(TObject(logger)).LastMessage, 'Should log correct message');
end;

procedure TQuickIOCTests.Test_Resolve_WithName;
var
  logger: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>('MainLogger');
  logger := FContainer.Resolve<ILogger>('MainLogger');
  Assert.IsNotNull(logger, 'Should resolve named instance');
end;

procedure TQuickIOCTests.Test_IsRegistered;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>;
  Assert.IsTrue(FContainer.IsRegistered<ILogger>(''), 'Should be registered');
  Assert.IsFalse(FContainer.IsRegistered<IUserService>(''), 'Should not be registered');
end;

procedure TQuickIOCTests.Test_ResolveAll;
var
  loggers: TList<ILogger>;
  logger: ILogger;
  consoleLogger: TConsoleLogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  loggers := FContainer.ResolveAll<ILogger>();
  try
    Assert.AreEqual<Integer>(2, loggers.Count, 'Should resolve all registered implementations');
    for logger in loggers do
    begin
      if TObject(logger) is TConsoleLogger then
      begin
        consoleLogger := TConsoleLogger(TObject(logger));
        Assert.IsNotNull(consoleLogger, 'ConsoleLogger should not be nil');
      end
      else
      begin
        Assert.Fail('Unexpected logger type resolved');
      end;
    end;
  finally
    loggers.Free;
  end;
end;

procedure TQuickIOCTests.Test_RegisterFactory;
var
  factory: IFactory<IUserService>;
  userService: IUserService;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  FContainer.RegisterSimpleFactory<IUserService, TUserService>;
  factory := FContainer.Resolve<IFactory<IUserService>>;
  Assert.IsNotNull(factory, 'Factory should not be nil');
  userService := factory.New;
  Assert.IsNotNull(userService, 'Factory should create instance');
  Assert.AreEqual('TestUser', userService.GetUserName, 'Factory-created instance should work');
end;

{ --- Additional coverage --- }

procedure TQuickIOCTests.Test_Resolve_Unregistered_Raises;
begin
  // Resolving a non-registered type must raise EIocResolverError
  Assert.WillRaise(
    procedure begin FContainer.Resolve<IEmailService>; end,
    EIocResolverError,
    'Resolving unregistered type must raise EIocResolverError');
end;

procedure TQuickIOCTests.Test_RegisterType_DelegateTo;
var
  logger: ILogger;
  consoleLogger: TConsoleLogger;
begin
  // DelegateTo lets us control object creation with a custom factory delegate
  FContainer.RegisterType<ILogger, TConsoleLogger>
    .AsSingleton
    .DelegateTo(function: TConsoleLogger
    begin
      Result := TConsoleLogger.Create;
      Result.Log('created-via-delegate');
    end);
  logger := FContainer.Resolve<ILogger>;
  Assert.IsNotNull(logger, 'DelegateTo must produce a non-nil instance');
  consoleLogger := TObject(logger) as TConsoleLogger;
  Assert.AreEqual('created-via-delegate', consoleLogger.LastMessage,
    'Delegate constructor side-effect must be visible');
end;

procedure TQuickIOCTests.Test_RegisterOptions_WithInstance;
var
  opts: IOptions<TAppSettings>;
  settings: TAppSettings;
begin
  settings := TAppSettings.Create;
  settings.AppName := 'TestApp';
  settings.MaxConnections := 10;
  FContainer.RegisterOptions<TAppSettings>(settings);
  opts := FContainer.Resolve<IOptions<TAppSettings>>;
  Assert.IsNotNull(opts, 'RegisterOptions must produce a resolvable IOptions<T>');
  Assert.AreEqual('TestApp', opts.Value.AppName, 'Resolved options must carry AppName');
  Assert.AreEqual(10, opts.Value.MaxConnections, 'Resolved options must carry MaxConnections');
end;

procedure TQuickIOCTests.Test_RegisterOptions_WithConfigureProc;
var
  opts: IOptions<TAppSettings>;
begin
  FContainer.RegisterOptions<TAppSettings>(
    procedure(o: TAppSettings)
    begin
      o.AppName := 'ConfiguredApp';
      o.MaxConnections := 20;
    end);
  opts := FContainer.Resolve<IOptions<TAppSettings>>;
  Assert.IsNotNull(opts, 'Configure-proc registration must produce a resolvable IOptions<T>');
  Assert.AreEqual('ConfiguredApp', opts.Value.AppName, 'AppName must be set by configure proc');
  Assert.AreEqual(20, opts.Value.MaxConnections, 'MaxConnections must be set by configure proc');
end;

procedure TQuickIOCTests.Test_Build_ResolvesSingletons;
var
  logger: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  // Build() pre-resolves all singletons; must not raise
  Assert.WillNotRaise(
    procedure begin FContainer.Build; end,
    nil,
    'Build must not raise when all dependencies are registered');
  logger := FContainer.Resolve<ILogger>;
  Assert.IsNotNull(logger, 'After Build, singleton must be resolvable');
end;

procedure TQuickIOCTests.Test_Singleton_SameInstance_AcrossResolve;
var
  a, b: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  a := FContainer.Resolve<ILogger>;
  b := FContainer.Resolve<ILogger>;
  Assert.AreSame(a, b, 'Singleton must return the same instance on repeated resolve');
end;

procedure TQuickIOCTests.Test_Transient_DifferentInstance_EachResolve;
var
  a, b: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  a := FContainer.Resolve<ILogger>;
  b := FContainer.Resolve<ILogger>;
  Assert.AreNotSame(a, b, 'Transient must return a new instance on each resolve');
end;

procedure TQuickIOCTests.Test_GlobalContainer_IsNotNil;
begin
  // GlobalContainer is a class-level singleton, always available
  Assert.IsNotNull(GlobalContainer, 'GlobalContainer must never be nil');
end;

procedure TQuickIOCTests.Test_IsRegistered_WithImplementation;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>;
  Assert.IsTrue(
    FContainer.IsRegistered<ILogger, TConsoleLogger>(''),
    'IsRegistered<Interface, Implementation> must return True');
  Assert.IsFalse(
    FContainer.IsRegistered<ILogger, TFileLogger>(''),
    'IsRegistered<Interface, WrongImpl> must return False');
end;

procedure TQuickIOCTests.Test_ResolveAll_EmptyWhenNotRegistered;
var
  results: TList<IEmailService>;
begin
  results := FContainer.ResolveAll<IEmailService>;
  try
    Assert.AreEqual(0, Integer(results.Count),
      'ResolveAll on unregistered type must return empty list');
  finally
    results.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickIOCTests);
end.
