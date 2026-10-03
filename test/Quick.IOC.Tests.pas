unit Quick.IOC.Tests;

{ ***************************************************************************
  Modified : 02/10/2026
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

  // Dependency graph for IOwned<T>, with X scoped:
  //   A(X, B, C, D, E);  B(X, IOwned<D>, E);  C(X, IOwned<D>, E);  D(X, E);  E(X)
  IGraphX = interface
  ['{5C2E8A41-7D3F-4B19-9E06-A1F4C8D2B735}']
  end;

  IGraphE = interface
  ['{8E1F3C72-4A5B-4D60-B9C7-2F6E0A1D3B48}']
    function X: IGraphX;
  end;

  IGraphD = interface
  ['{2A7C9E15-6B3D-4F82-A0E4-9D1B5C7F3E26}']
    function X: IGraphX;
    function E: IGraphE;
  end;

  IGraphBranch = interface
  ['{D4B6F803-1E2A-4C57-8F39-6A0C2E5D7B91}']
    function X: IGraphX;
    function E: IGraphE;
    function OwnedD: IOwned<IGraphD>;
  end;

  IGraphB = interface(IGraphBranch)
  ['{7F3A1D96-2C4E-4B08-9A57-E0B6D3C1F842}']
  end;

  IGraphC = interface(IGraphBranch)
  ['{1B9E4C27-8D6A-4E31-B5F0-3C7A2E9D6F54}']
  end;

  IGraphA = interface
  ['{9C5D2E68-3F7B-4A14-8E92-B6D0F1A4C375}']
    function X: IGraphX;
    function B: IGraphB;
    function C: IGraphC;
    function D: IGraphD;
    function E: IGraphE;
  end;

  TGraphX = class(TInterfacedObject, IGraphX)
  private class var
    FDestroyed: Integer;
  public
    destructor Destroy; override;
    class property Destroyed: Integer read FDestroyed write FDestroyed;
  end;

  TGraphE = class(TInterfacedObject, IGraphE)
  private
    FX: IGraphX;
  public
    constructor Create(x: IGraphX);
    function X: IGraphX;
  end;

  TGraphD = class(TInterfacedObject, IGraphD)
  private
    FX: IGraphX;
    FE: IGraphE;
  public
    constructor Create(x: IGraphX; e: IGraphE);
    function X: IGraphX;
    function E: IGraphE;
  end;

  TGraphBranch = class(TInterfacedObject, IGraphB, IGraphC)
  private
    FX: IGraphX;
    FE: IGraphE;
    FOwnedD: IOwned<IGraphD>;
  public
    constructor Create(x: IGraphX; ownedD: IOwned<IGraphD>; e: IGraphE);
    function X: IGraphX;
    function E: IGraphE;
    function OwnedD: IOwned<IGraphD>;
  end;

  // own constructors: CreateInstance tries a class's own constructors first and, among
  // inherited ones, the parameterless TObject.Create before any other
  TGraphB = class(TGraphBranch)
  public
    constructor Create(x: IGraphX; ownedD: IOwned<IGraphD>; e: IGraphE);
  end;

  TGraphC = class(TGraphBranch)
  public
    constructor Create(x: IGraphX; ownedD: IOwned<IGraphD>; e: IGraphE);
  end;

  TGraphA = class(TInterfacedObject, IGraphA)
  private
    FX: IGraphX;
    FB: IGraphB;
    FC: IGraphC;
    FD: IGraphD;
    FE: IGraphE;
  public
    constructor Create(x: IGraphX; b: IGraphB; c: IGraphC; d: IGraphD; e: IGraphE);
    function X: IGraphX;
    function B: IGraphB;
    function C: IGraphC;
    function D: IGraphD;
    function E: IGraphE;
  end;

  // [Inject] and constructor diagnostics
  IInjService = interface
  ['{6E2B9D41-3A7C-4F05-B8E1-C4D0A2F7B396}']
    function Logger: ILogger;
  end;

  // default rule would pick the parameterless constructor; [Inject] picks the other one
  TInjMarked = class(TInterfacedObject, IInjService)
  private
    FLogger: ILogger;
  public
    constructor Create; overload;
    [Inject]
    constructor Create(logger: ILogger); overload;
    function Logger: ILogger;
  end;

  TInjBase = class(TInterfacedObject, IInjService)
  private
    FLogger: ILogger;
  public
    [Inject]
    constructor Create(logger: ILogger);
    function Logger: ILogger;
  end;

  // no constructor of its own: inherits the [Inject] one
  TInjDerived = class(TInjBase);

  TNoInjBase = class(TInterfacedObject, IInjService)
  private
    FLogger: ILogger;
  public
    constructor Create(logger: ILogger);
    function Logger: ILogger;
  end;

  // no constructor of its own and no [Inject]: the default rule picks TObject.Create
  TNoInjDerived = class(TNoInjBase);

  TInjTwoMarked = class(TInterfacedObject, IInjService)
  public
    [Inject]
    constructor Create; overload;
    [Inject]
    constructor Create(logger: ILogger); overload;
    function Logger: ILogger;
  end;

  // Logger that counts destructions, to check scope release
  TTrackedLogger = class(TInterfacedObject, ILogger)
  private class var
    FDestroyed: Integer;
  public
    destructor Destroy; override;
    procedure Log(const msg: string);
    class property Destroyed: Integer read FDestroyed write FDestroyed;
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
    { Scoped lifetime }
    [Test]
    procedure Test_Scoped_SameInstance_WithinScope;
    [Test]
    procedure Test_Scoped_DifferentInstance_AcrossScopes;
    [Test]
    procedure Test_Scoped_SharedByDependents_InSameScope;
    [Test]
    procedure Test_Scoped_FromRoot_RaisesScopeError;
    [Test]
    procedure Test_Scoped_AsSingletonDependency_RaisesScopeError;
    [Test]
    procedure Test_Scoped_ValidateScopesOff_BehavesAsTransient;
    [Test]
    procedure Test_Scope_Free_ReleasesScopedInstances;
    [Test]
    procedure Test_Singleton_ResolvedWithinScope_SameAsRoot;
    { IOwned<T> }
    [Test]
    procedure Test_Owned_IsRegisteredAutomatically;
    [Test]
    procedure Test_Owned_Graph_ConsumerScopeSharedOutsideOwnedBranches;
    [Test]
    procedure Test_Owned_Graph_EachBranchGetsItsOwnScope;
    [Test]
    procedure Test_Owned_Release_FreesItsScopedInstances;
    [Test]
    procedure Test_Owned_ResolvedFromRoot_OpensItsOwnScope;
    { [Inject] and constructor diagnostics }
    [Test]
    procedure Test_Inject_UsesMarkedConstructor;
    [Test]
    procedure Test_Inject_InheritedMarkedConstructorIsUsed;
    [Test]
    procedure Test_Inject_Unsatisfiable_RaisesInsteadOfFallback;
    [Test]
    procedure Test_Inject_MoreThanOneMarked_RaisesRegisterError;
    [Test]
    procedure Test_NoInject_KeepsDefaultRule;
    [Test]
    procedure Test_Build_ValidateConstructors_ReportsTObjectFallback;
    [Test]
    procedure Test_Build_ValidateConstructors_ReportsUnsatisfiableInject;
    [Test]
    procedure Test_Build_ValidateConstructorsOffByDefault;
    [Test]
    procedure Test_DiagnoseConstructors_CleanRegistrations;
    { ResolveAll }
    [Test]
    procedure Test_ResolveAll_ReturnsEachRegistration;
    [Test]
    procedure Test_ResolveAll_KeepsEachRegistrationLifetime;
    [Test]
    procedure Test_ResolveAll_InScope_ScopedEntriesPerScope;
    [Test]
    procedure Test_ResolveAll_ScopedFromRoot_RaisesScopeError;
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

{ TTrackedLogger }

destructor TTrackedLogger.Destroy;
begin
  Inc(FDestroyed);
  inherited;
end;

procedure TTrackedLogger.Log(const msg: string);
begin
end;

{ Scoped lifetime }

procedure TQuickIOCTests.Test_Scoped_SameInstance_WithinScope;
var
  scope: TIocScope;
  a, b: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  scope := FContainer.CreateScope;
  try
    a := scope.Resolve<ILogger>;
    b := scope.Resolve<ILogger>;
    Assert.AreSame(a, b, 'Scoped must return the same instance within a scope');
  finally
    a := nil;
    b := nil;
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_Scoped_DifferentInstance_AcrossScopes;
var
  scope1, scope2: TIocScope;
  a, b: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  scope1 := FContainer.CreateScope;
  scope2 := FContainer.CreateScope;
  try
    a := scope1.Resolve<ILogger>;
    b := scope2.Resolve<ILogger>;
    Assert.AreNotSame(a, b, 'Scoped must return a different instance in each scope');
  finally
    a := nil;
    b := nil;
    scope2.Free;
    scope1.Free;
  end;
end;

procedure TQuickIOCTests.Test_Scoped_SharedByDependents_InSameScope;
var
  scope: TIocScope;
  user: IUserService;
  email: IEmailService;
begin
  // the transient services receive the scoped logger through constructor injection
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  FContainer.RegisterType<IUserService, TUserService>.AsTransient;
  FContainer.RegisterType<IEmailService, TEmailService>.AsTransient;
  scope := FContainer.CreateScope;
  try
    user := scope.Resolve<IUserService>;
    email := scope.Resolve<IEmailService>;
    Assert.IsNotNull(TUserService(user as TObject).FLogger, 'Scoped dependency must be injected');
    Assert.AreSame(TUserService(user as TObject).FLogger, TEmailService(email as TObject).FLogger,
      'Dependents resolved in the same scope must share the scoped instance');
  finally
    user := nil;
    email := nil;
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_Scoped_FromRoot_RaisesScopeError;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  Assert.WillRaise(
    procedure
    begin
      FContainer.Resolve<ILogger>;
    end, EIocScopeError, 'Resolving a scoped service from the root must raise EIocScopeError');
end;

procedure TQuickIOCTests.Test_Scoped_AsSingletonDependency_RaisesScopeError;
var
  scope: TIocScope;
begin
  // a singleton would capture the scoped instance for the whole application lifetime
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  FContainer.RegisterType<IUserService, TUserService>.AsSingleton;
  scope := FContainer.CreateScope;
  try
    Assert.WillRaise(
      procedure
      begin
        scope.Resolve<IUserService>;
      end, EIocScopeError, 'A singleton depending on a scoped service must raise EIocScopeError');
  finally
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_Scoped_ValidateScopesOff_BehavesAsTransient;
var
  a, b: ILogger;
begin
  FContainer.ValidateScopes := False;
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  a := FContainer.Resolve<ILogger>;
  b := FContainer.Resolve<ILogger>;
  Assert.IsNotNull(a, 'Legacy mode must still resolve');
  Assert.AreNotSame(a, b, 'With ValidateScopes off, scoped outside a scope keeps the legacy transient behaviour');
end;

procedure TQuickIOCTests.Test_Scope_Free_ReleasesScopedInstances;
var
  scope: TIocScope;
  logger: ILogger;
begin
  FContainer.RegisterType<ILogger, TTrackedLogger>.AsScoped;
  TTrackedLogger.Destroyed := 0;
  scope := FContainer.CreateScope;
  try
    // explicit variable, released before freeing the scope: an implicit interface
    // temporary would only be released at the end of this routine
    logger := scope.Resolve<ILogger>;
    logger.Log('x');
    logger := scope.Resolve<ILogger>;
    logger.Log('y');
    logger := nil;
    Assert.AreEqual(0, TTrackedLogger.Destroyed, 'Scoped instance must live while the scope is alive');
  finally
    logger := nil;
    scope.Free;
  end;
  Assert.AreEqual(1, TTrackedLogger.Destroyed, 'Freeing the scope must release its single scoped instance');
end;

procedure TQuickIOCTests.Test_Singleton_ResolvedWithinScope_SameAsRoot;
var
  scope: TIocScope;
  a, b: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  scope := FContainer.CreateScope;
  try
    a := scope.Resolve<ILogger>;
    b := FContainer.Resolve<ILogger>;
    Assert.AreSame(a, b, 'A singleton is the same instance inside and outside scopes');
  finally
    a := nil;
    b := nil;
    scope.Free;
  end;
end;

{ IOwned<T> test graph }

destructor TGraphX.Destroy;
begin
  Inc(FDestroyed);
  inherited;
end;

constructor TGraphE.Create(x: IGraphX);
begin
  FX := x;
end;

function TGraphE.X: IGraphX;
begin
  Result := FX;
end;

constructor TGraphD.Create(x: IGraphX; e: IGraphE);
begin
  FX := x;
  FE := e;
end;

function TGraphD.X: IGraphX;
begin
  Result := FX;
end;

function TGraphD.E: IGraphE;
begin
  Result := FE;
end;

constructor TGraphBranch.Create(x: IGraphX; ownedD: IOwned<IGraphD>; e: IGraphE);
begin
  FX := x;
  FOwnedD := ownedD;
  FE := e;
end;

function TGraphBranch.X: IGraphX;
begin
  Result := FX;
end;

function TGraphBranch.E: IGraphE;
begin
  Result := FE;
end;

function TGraphBranch.OwnedD: IOwned<IGraphD>;
begin
  Result := FOwnedD;
end;

constructor TGraphB.Create(x: IGraphX; ownedD: IOwned<IGraphD>; e: IGraphE);
begin
  inherited Create(x, ownedD, e);
end;

constructor TGraphC.Create(x: IGraphX; ownedD: IOwned<IGraphD>; e: IGraphE);
begin
  inherited Create(x, ownedD, e);
end;

constructor TGraphA.Create(x: IGraphX; b: IGraphB; c: IGraphC; d: IGraphD; e: IGraphE);
begin
  FX := x;
  FB := b;
  FC := c;
  FD := d;
  FE := e;
end;

function TGraphA.X: IGraphX;
begin
  Result := FX;
end;

function TGraphA.B: IGraphB;
begin
  Result := FB;
end;

function TGraphA.C: IGraphC;
begin
  Result := FC;
end;

function TGraphA.D: IGraphD;
begin
  Result := FD;
end;

function TGraphA.E: IGraphE;
begin
  Result := FE;
end;

{ IOwned<T> }

procedure RegisterGraph(aContainer: TIocContainer);
begin
  aContainer.RegisterType<IGraphX, TGraphX>.AsScoped;
  aContainer.RegisterType<IGraphE, TGraphE>.AsTransient;
  aContainer.RegisterType<IGraphD, TGraphD>.AsTransient;
  aContainer.RegisterType<IGraphB, TGraphB>.AsTransient;
  aContainer.RegisterType<IGraphC, TGraphC>.AsTransient;
  aContainer.RegisterType<IGraphA, TGraphA>.AsTransient;
end;

procedure TQuickIOCTests.Test_Owned_IsRegisteredAutomatically;
begin
  FContainer.RegisterType<IGraphD, TGraphD>.AsTransient;
  Assert.IsTrue(FContainer.IsRegistered<IOwned<IGraphD>>(''),
    'RegisterType<I,T> must also register IOwned<I>');
end;

procedure TQuickIOCTests.Test_Owned_Graph_ConsumerScopeSharedOutsideOwnedBranches;
var
  scope: TIocScope;
  a: IGraphA;
  x0: IGraphX;
begin
  // everything A receives directly, and what B and C receive directly, is in A's scope
  RegisterGraph(FContainer);
  scope := FContainer.CreateScope;
  try
    a := scope.Resolve<IGraphA>;
    x0 := a.X;
    Assert.IsNotNull(x0, 'X must be injected into A');
    Assert.AreSame(x0, a.D.X, 'D received directly by A shares A''s X');
    Assert.AreSame(x0, a.E.X, 'E received directly by A shares A''s X');
    Assert.AreSame(x0, a.D.E.X, 'E inside A''s own D shares A''s X');
    Assert.AreSame(x0, a.B.X, 'B shares A''s X');
    Assert.AreSame(x0, a.B.E.X, 'E received directly by B shares A''s X');
    Assert.AreSame(x0, a.C.X, 'C shares A''s X');
    Assert.AreSame(x0, a.C.E.X, 'E received directly by C shares A''s X');
  finally
    x0 := nil;
    a := nil;
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_Owned_Graph_EachBranchGetsItsOwnScope;
var
  scope: TIocScope;
  a: IGraphA;
  x0, xB, xC: IGraphX;
  dB, dC: IGraphD;
begin
  // the D -> E chain behind each IOwned<D> lives in its own scope, one per consumer
  RegisterGraph(FContainer);
  scope := FContainer.CreateScope;
  try
    a := scope.Resolve<IGraphA>;
    x0 := a.X;
    dB := a.B.OwnedD.Value;
    dC := a.C.OwnedD.Value;
    xB := dB.X;
    xC := dC.X;
    Assert.IsNotNull(xB, 'X must be injected into B''s owned D');
    Assert.IsNotNull(xC, 'X must be injected into C''s owned D');
    Assert.AreNotSame(x0, xB, 'B''s owned D must not share A''s X');
    Assert.AreNotSame(x0, xC, 'C''s owned D must not share A''s X');
    Assert.AreNotSame(xB, xC, 'B and C must each open their own scope');
    Assert.AreSame(xB, dB.E.X, 'D and E in B''s owned chain share that chain''s X');
    Assert.AreSame(xC, dC.E.X, 'D and E in C''s owned chain share that chain''s X');
  finally
    dB := nil;
    dC := nil;
    x0 := nil;
    xB := nil;
    xC := nil;
    a := nil;
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_Owned_Release_FreesItsScopedInstances;
var
  scope: TIocScope;
  owned: IOwned<IGraphD>;
  d: IGraphD;
  x: IGraphX;
begin
  // releasing the IOwned frees its scope (and its X), while the consumer's scope lives on.
  // explicit variables, released before the assertion: chained calls such as owned.Value.X
  // would keep implicit interface temporaries alive until the end of this routine
  RegisterGraph(FContainer);
  TGraphX.Destroyed := 0;
  scope := FContainer.CreateScope;
  try
    owned := scope.Resolve<IOwned<IGraphD>>;
    d := owned.Value;
    x := d.X;
    Assert.IsNotNull(x, 'Owned D must receive an X');
    x := nil;
    d := nil;
    Assert.AreEqual(0, TGraphX.Destroyed, 'Owned scope must live while IOwned is referenced');
    owned := nil;
    Assert.AreEqual(1, TGraphX.Destroyed, 'Releasing IOwned must free its scope''s X');
  finally
    x := nil;
    d := nil;
    owned := nil;
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_Owned_ResolvedFromRoot_OpensItsOwnScope;
var
  owned: IOwned<IGraphD>;
begin
  // IOwned does not need an outer scope: it opens one, so scoped dependencies resolve
  RegisterGraph(FContainer);
  owned := FContainer.Resolve<IOwned<IGraphD>>;
  try
    Assert.IsNotNull(owned.Value.X, 'Scoped X must resolve inside the owned scope');
    Assert.AreSame(owned.Value.X, owned.Value.E.X, 'D and E share the owned scope''s X');
  finally
    owned := nil;
  end;
end;

{ [Inject] test classes }

constructor TInjMarked.Create;
begin
end;

constructor TInjMarked.Create(logger: ILogger);
begin
  FLogger := logger;
end;

function TInjMarked.Logger: ILogger;
begin
  Result := FLogger;
end;

constructor TInjBase.Create(logger: ILogger);
begin
  FLogger := logger;
end;

function TInjBase.Logger: ILogger;
begin
  Result := FLogger;
end;

constructor TNoInjBase.Create(logger: ILogger);
begin
  FLogger := logger;
end;

function TNoInjBase.Logger: ILogger;
begin
  Result := FLogger;
end;

constructor TInjTwoMarked.Create;
begin
end;

constructor TInjTwoMarked.Create(logger: ILogger);
begin
end;

function TInjTwoMarked.Logger: ILogger;
begin
  Result := nil;
end;

{ [Inject] and constructor diagnostics }

procedure TQuickIOCTests.Test_Inject_UsesMarkedConstructor;
var
  svc: IInjService;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IInjService, TInjMarked>.AsTransient;
  svc := FContainer.Resolve<IInjService>;
  Assert.IsNotNull(svc.Logger, 'The [Inject] constructor must be used instead of the parameterless one');
end;

procedure TQuickIOCTests.Test_Inject_InheritedMarkedConstructorIsUsed;
var
  svc: IInjService;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IInjService, TInjDerived>.AsTransient;
  svc := FContainer.Resolve<IInjService>;
  Assert.IsTrue((svc as TObject) is TInjDerived, 'The registered class must be the one created');
  Assert.IsNotNull(svc.Logger, 'The inherited [Inject] constructor must be used, not TObject.Create');
end;

procedure TQuickIOCTests.Test_Inject_Unsatisfiable_RaisesInsteadOfFallback;
begin
  // ILogger is not registered: the default rule would silently use the parameterless constructor
  FContainer.RegisterType<IInjService, TInjMarked>.AsTransient;
  Assert.WillRaise(
    procedure
    begin
      FContainer.Resolve<IInjService>;
    end, EIocResolverError, 'An unsatisfiable [Inject] constructor must raise, not fall back');
end;

procedure TQuickIOCTests.Test_Inject_MoreThanOneMarked_RaisesRegisterError;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IInjService, TInjTwoMarked>.AsTransient;
  Assert.WillRaise(
    procedure
    begin
      FContainer.Resolve<IInjService>;
    end, EIocRegisterError, 'Two constructors marked [Inject] must raise EIocRegisterError');
end;

procedure TQuickIOCTests.Test_NoInject_KeepsDefaultRule;
var
  svc: IInjService;
begin
  // compatibility: without [Inject] the default rule is unchanged (TObject.Create here)
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IInjService, TNoInjDerived>.AsTransient;
  svc := FContainer.Resolve<IInjService>;
  Assert.IsNull(svc.Logger, 'Without [Inject] the default constructor rule must not change');
end;

procedure TQuickIOCTests.Test_Build_ValidateConstructors_ReportsTObjectFallback;
var
  msg: string;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IInjService, TNoInjDerived>.AsTransient;
  FContainer.ValidateConstructors := True;
  msg := '';
  try
    FContainer.Build;
  except
    on E: EIocBuildError do msg := E.Message;
  end;
  Assert.IsTrue(Pos('TNoInjDerived would be created by TObject.Create', msg) > 0,
    'Build must report the TObject.Create fallback. Message: ' + msg);
end;

procedure TQuickIOCTests.Test_Build_ValidateConstructors_ReportsUnsatisfiableInject;
var
  msg: string;
begin
  FContainer.RegisterType<IInjService, TInjMarked>.AsTransient;
  FContainer.ValidateConstructors := True;
  msg := '';
  try
    FContainer.Build;
  except
    on E: EIocBuildError do msg := E.Message;
  end;
  Assert.IsTrue(Pos('marked [Inject] has unregistered parameters: logger: ILogger', msg) > 0,
    'Build must report the unregistered parameter of the [Inject] constructor. Message: ' + msg);
end;

procedure TQuickIOCTests.Test_Build_ValidateConstructorsOffByDefault;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IInjService, TNoInjDerived>.AsTransient;
  Assert.WillNotRaise(
    procedure
    begin
      FContainer.Build;
    end, nil, 'With ValidateConstructors off (default), Build must not run the diagnostics');
end;

procedure TQuickIOCTests.Test_DiagnoseConstructors_CleanRegistrations;
var
  problems: TArray<string>;
begin
  RegisterGraph(FContainer);
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IInjService, TInjDerived>.AsTransient;
  problems := FContainer.DiagnoseConstructors;
  // Length returns NativeInt on Win64: cast so AreEqual can infer a single type
  Assert.AreEqual(0, Integer(Length(problems)), 'No problems expected. Found: ' + string.Join(' | ', problems));
end;

{ ResolveAll }

procedure TQuickIOCTests.Test_ResolveAll_ReturnsEachRegistration;
var
  loggers: TList<ILogger>;
begin
  // different implementations under the same key: each registration must appear once
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  FContainer.RegisterType<ILogger, TFileLogger>.AsTransient;
  loggers := FContainer.ResolveAll<ILogger>;
  try
    Assert.AreEqual<Integer>(2, loggers.Count, 'One instance per registration');
    Assert.IsTrue((loggers[0] as TObject) is TConsoleLogger, 'First registration must be TConsoleLogger');
    Assert.IsTrue((loggers[1] as TObject) is TFileLogger, 'Second registration must be TFileLogger');
  finally
    loggers.Free;
  end;
end;

procedure TQuickIOCTests.Test_ResolveAll_KeepsEachRegistrationLifetime;
var
  first, second: TList<ILogger>;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  FContainer.RegisterType<ILogger, TFileLogger>.AsTransient;
  first := FContainer.ResolveAll<ILogger>;
  second := FContainer.ResolveAll<ILogger>;
  try
    Assert.AreSame(first[0], second[0], 'The singleton registration returns the same instance every time');
    Assert.AreNotSame(first[1], second[1], 'The transient registration returns a new instance every time');
  finally
    first.Free;
    second.Free;
  end;
end;

procedure TQuickIOCTests.Test_ResolveAll_InScope_ScopedEntriesPerScope;
var
  scope1, scope2: TIocScope;
  a, b, c: TList<ILogger>;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  FContainer.RegisterType<ILogger, TFileLogger>.AsScoped;
  scope1 := FContainer.CreateScope;
  scope2 := FContainer.CreateScope;
  a := nil;
  b := nil;
  c := nil;
  try
    a := scope1.ResolveAll<ILogger>;
    b := scope1.ResolveAll<ILogger>;
    c := scope2.ResolveAll<ILogger>;
    Assert.AreEqual<Integer>(2, a.Count, 'One instance per registration in the scope');
    Assert.AreNotSame(a[0], a[1], 'Each scoped registration has its own instance');
    Assert.AreSame(a[0], b[0], 'Same scope: same instance for the first registration');
    Assert.AreSame(a[1], b[1], 'Same scope: same instance for the second registration');
    Assert.AreNotSame(a[0], c[0], 'Other scope: another instance');
  finally
    a.Free;
    b.Free;
    c.Free;
    scope2.Free;
    scope1.Free;
  end;
end;

procedure TQuickIOCTests.Test_ResolveAll_ScopedFromRoot_RaisesScopeError;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  Assert.WillRaise(
    procedure
    begin
      FContainer.ResolveAll<ILogger>().Free;
    end, EIocScopeError, 'ResolveAll of a scoped registration from the root must raise EIocScopeError');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickIOCTests);
end.
