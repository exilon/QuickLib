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

  // Logger that counts destructions, to check scope release
  TTrackedLogger = class(TInterfacedObject, ILogger)
  private class var
    FDestroyed: Integer;
  public
    destructor Destroy; override;
    procedure Log(const msg: string);
    class property Destroyed: Integer read FDestroyed write FDestroyed;
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

  EExplodingDestroy = class(Exception);

  // scoped instance whose destructor raises
  IExploding = interface
  ['{DBAB5F9B-3540-4C65-85D4-946B544F4945}']
  end;

  TExplodingOnDestroy = class(TInterfacedObject, IExploding)
  public
    destructor Destroy; override;
  end;

  // destructor that raises after releasing its scoped logger
  TExplodingWithLogger = class(TExplodingOnDestroy)
  private
    FLogger: ILogger;
  public
    constructor Create(logger: ILogger);
    destructor Destroy; override;
  end;

  // runs OnDestroy in its destructor, as a scoped service released by its scope
  IRunsOnDestroy = interface
  ['{EDDBE5E3-6389-47E1-A4B9-F9D7391A030A}']
  end;

  TRunsOnDestroy = class(TInterfacedObject, IRunsOnDestroy)
  private class var
    FOnDestroy: TProc;
    FOutcome: string;
  public
    destructor Destroy; override;
    class property OnDestroy: TProc read FOnDestroy write FOnDestroy;
    // 'ok', or the exception raised by OnDestroy
    class property Outcome: string read FOutcome write FOutcome;
  end;

  ILoggerConsumer = interface
  ['{6E2B9D41-3A7C-4F05-B8E1-C4D0A2F7B396}']
    function Logger: ILogger;
  end;

  // asks only for IOwned<ILogger>
  TOwnedConsumer = class(TInterfacedObject, ILoggerConsumer)
  private
    FOwned: IOwned<ILogger>;
  public
    constructor Create(owned: IOwned<ILogger>);
    function Logger: ILogger;
  end;

  EFailsAfterExploding = class(Exception);

  // constructor that fails after IExploding was already created in the scope
  TFailsAfterExploding = class(TInterfacedObject, ILoggerConsumer)
  public
    constructor Create(exploding: IExploding);
    function Logger: ILogger;
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
    procedure Test_Scoped_FromRoot_MessageShowsHowToKeepOldBehaviour;
    [Test]
    procedure Test_Scoped_AsSingletonDependency_RaisesScopeError;
    [Test]
    procedure Test_Scoped_ValidateScopesOff_BehavesAsTransient;
    [Test]
    procedure Test_Scope_Free_ReleasesScopedInstances;
    [Test]
    procedure Test_Singleton_ResolvedWithinScope_SameAsRoot;
    [Test]
    procedure Test_Scope_Free_ReleasesAllEvenIfOneDestructorRaises;
    [Test]
    procedure Test_Scope_FreeThatRaises_LosesOnlyItsOwnMemory;
    [Test]
    procedure Test_Scope_ResolveWhileBeingFreed_RaisesScopeError;
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
    [Test]
    procedure Test_Owned_Release_FreesScopeEvenIfValueDestructorRaises;
    [Test]
    procedure Test_Owned_ResolutionFailure_NotHiddenByScopeRelease;
    [Test]
    procedure Test_Owned_AutoRegisterOff_NotRegistered;
    [Test]
    procedure Test_Owned_RegisterOwned_OnePerKeyWrapsWhatResolveReturns;
    [Test]
    procedure Test_Owned_RegisterOwned_WithoutRegistration_Raises;
    [Test]
    procedure Test_Owned_NotRegistered_ConsumerRaisesRegisterError;
    [Test]
    procedure Test_Owned_GivenInstanceOnTop_OwnedWrapsIt;
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

{ TTrackedLogger }

destructor TTrackedLogger.Destroy;
begin
  Inc(FDestroyed);
  inherited;
end;

procedure TTrackedLogger.Log(const msg: string);
begin
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

procedure RegisterGraph(aContainer: TIocContainer);
begin
  aContainer.RegisterType<IGraphX, TGraphX>.AsScoped;
  aContainer.RegisterType<IGraphE, TGraphE>.AsTransient;
  aContainer.RegisterType<IGraphD, TGraphD>.AsTransient;
  aContainer.RegisterType<IGraphB, TGraphB>.AsTransient;
  aContainer.RegisterType<IGraphC, TGraphC>.AsTransient;
  aContainer.RegisterType<IGraphA, TGraphA>.AsTransient;
end;

{ TExplodingOnDestroy }

destructor TExplodingOnDestroy.Destroy;
begin
  inherited;
  raise EExplodingDestroy.Create('Simulated failure in a scoped destructor');
end;

{ TExplodingWithLogger }

constructor TExplodingWithLogger.Create(logger: ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

destructor TExplodingWithLogger.Destroy;
begin
  // released before the inherited destructor raises: a destructor that raises never finalizes
  // the fields, and the logger must depend only on its scope
  FLogger := nil;
  inherited;
end;

{ TRunsOnDestroy }

destructor TRunsOnDestroy.Destroy;
begin
  if Assigned(FOnDestroy) then
  begin
    try
      FOnDestroy();
      FOutcome := 'ok';
    except
      on E: Exception do FOutcome := E.ClassName + ': ' + E.Message;
    end;
  end;
  inherited;
end;

{ TOwnedConsumer }

constructor TOwnedConsumer.Create(owned: IOwned<ILogger>);
begin
  inherited Create;
  FOwned := owned;
end;

function TOwnedConsumer.Logger: ILogger;
begin
  if FOwned <> nil then Result := FOwned.Value
    else Result := nil;
end;

{ TFailsAfterExploding }

constructor TFailsAfterExploding.Create(exploding: IExploding);
begin
  inherited Create;
  raise EFailsAfterExploding.Create('Simulated failure in a constructor');
end;

function TFailsAfterExploding.Logger: ILogger;
begin
  Result := nil;
end;

// blocks allocated by the default memory manager
function AllocatedBlocks: Int64;
var
  state: TMemoryManagerState;
  i: Integer;
begin
  GetMemoryManagerState(state);
  Result := Int64(state.AllocatedMediumBlockCount) + Int64(state.AllocatedLargeBlockCount);
  for i := Low(state.SmallBlockTypeStates) to High(state.SmallBlockTypeStates) do
    Inc(Result, Int64(state.SmallBlockTypeStates[i].AllocatedBlockCount));
end;

// resolved in a routine of its own: the compiler's temporary references to the result are
// finalized on exit, so only the scope holds the instance afterwards
procedure ResolveExplodingAndRelease(aScope: TIocScope);
var
  exploding: IExploding;
begin
  exploding := aScope.Resolve<IExploding>;
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

procedure TQuickIOCTests.Test_Scoped_FromRoot_MessageShowsHowToKeepOldBehaviour;
var
  msg: string;
begin
  // ValidateScopes is True by default: code that resolved AsScoped from the root (as transient)
  // must be told how to keep that behaviour
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  msg := '';
  try
    FContainer.Resolve<ILogger>;
  except
    on E: EIocScopeError do msg := E.Message;
  end;
  Assert.IsTrue(Pos('ValidateScopes := False', msg) > 0,
    'The message must show how to keep the previous behaviour. Message: ' + msg);
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

procedure TQuickIOCTests.Test_Scope_Free_ReleasesAllEvenIfOneDestructorRaises;
var
  scope: TIocScope;
  logger: ILogger;
  exploding: IExploding;
  raised: string;
begin
  FContainer.RegisterType<ILogger, TTrackedLogger>.AsScoped;
  FContainer.RegisterType<IExploding, TExplodingOnDestroy>.AsScoped;
  TTrackedLogger.Destroyed := 0;
  scope := FContainer.CreateScope;
  logger := scope.Resolve<ILogger>;          // created first, released last
  exploding := scope.Resolve<IExploding>;    // released first: its destructor raises
  logger := nil;
  exploding := nil;
  raised := '';
  try
    scope.Free;
  except
    on E: Exception do raised := E.ClassName;
  end;
  Assert.AreEqual(1, TTrackedLogger.Destroyed, 'Every scoped instance must be released even if a destructor raises');
  Assert.AreEqual('EExplodingDestroy', raised, 'The destructor failure must reach the caller');
end;

procedure TQuickIOCTests.Test_Scope_FreeThatRaises_LosesOnlyItsOwnMemory;
var
  scope: TIocScope;
  round: Integer;
  before: Int64;
  kept: Int64;
begin
  // a destructor that raises skips FreeInstance. When a scoped destructor raises, the scope's Free
  // raises too: the instance and the scope's own memory are lost (two blocks), nothing else
  FContainer.RegisterType<IExploding, TExplodingOnDestroy>.AsScoped;
  kept := 0;
  // the first round allocates what is allocated once (RTTI); the second is measured
  for round := 1 to 2 do
  begin
    before := AllocatedBlocks;
    scope := FContainer.CreateScope;
    ResolveExplodingAndRelease(scope);
    try
      scope.Free;
    except
      on EExplodingDestroy do ;
    end;
    kept := AllocatedBlocks - before;
  end;
  Assert.AreEqual<Int64>(2, kept, 'Only the instance whose destructor raised and the scope itself may be lost');
end;

procedure TQuickIOCTests.Test_Scope_ResolveWhileBeingFreed_RaisesScopeError;
var
  scope: TIocScope;
  runner: IRunsOnDestroy;
begin
  FContainer.RegisterType<ILogger, TTrackedLogger>.AsScoped;
  FContainer.RegisterType<IRunsOnDestroy, TRunsOnDestroy>.AsScoped;
  TRunsOnDestroy.Outcome := '';
  scope := FContainer.CreateScope;
  try
    runner := scope.Resolve<IRunsOnDestroy>;
    runner := nil;
    // released by scope.Free: its destructor resolves from that same scope
    TRunsOnDestroy.OnDestroy :=
      procedure
      begin
        scope.Resolve<ILogger>;
      end;
  finally
    scope.Free;
    TRunsOnDestroy.OnDestroy := nil;
  end;
  Assert.IsTrue(TRunsOnDestroy.Outcome.StartsWith('EIocScopeError:'),
    'Resolving from a scope while it is freed must raise EIocScopeError. Got: ' + TRunsOnDestroy.Outcome);
end;

{ IOwned<T> }

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

procedure TQuickIOCTests.Test_Owned_Release_FreesScopeEvenIfValueDestructorRaises;
var
  owned: IOwned<IExploding>;
  raised: string;
begin
  FContainer.RegisterType<ILogger, TTrackedLogger>.AsScoped;
  FContainer.RegisterType<IExploding, TExplodingWithLogger>.AsTransient;
  TTrackedLogger.Destroyed := 0;
  owned := FContainer.Resolve<IOwned<IExploding>>; // its own scope, with its own scoped logger
  raised := '';
  try
    owned := nil; // the value's destructor raises
  except
    on E: Exception do raised := E.ClassName;
  end;
  Assert.AreEqual(1, TTrackedLogger.Destroyed, 'The IOwned scope must be released even if the value destructor raises');
  Assert.AreEqual('EExplodingDestroy', raised, 'The destructor failure must reach the caller');
end;

procedure TQuickIOCTests.Test_Owned_ResolutionFailure_NotHiddenByScopeRelease;
var
  raised: string;
begin
  FContainer.RegisterType<IExploding, TExplodingOnDestroy>.AsScoped;
  FContainer.RegisterType<ILoggerConsumer, TFailsAfterExploding>.AsTransient;
  raised := '';
  try
    // IExploding is created in the IOwned scope, then the constructor fails; releasing that scope
    // raises EExplodingDestroy
    FContainer.Resolve<IOwned<ILoggerConsumer>>;
  except
    on E: Exception do raised := E.ClassName;
  end;
  Assert.AreEqual('EFailsAfterExploding', raised,
    'The resolution failure must reach the caller, not the exception raised while releasing the IOwned scope');
end;

procedure TQuickIOCTests.Test_Owned_AutoRegisterOff_NotRegistered;
begin
  FContainer.AutoRegisterOwned := False;
  FContainer.RegisterType<ILogger, TConsoleLogger>;
  Assert.IsFalse(FContainer.IsRegistered<IOwned<ILogger>>(''),
    'With AutoRegisterOwned off, RegisterType<I,T> must not register IOwned<I>');
end;

procedure TQuickIOCTests.Test_Owned_RegisterOwned_OnePerKeyWrapsWhatResolveReturns;
var
  logger: ILogger;
  owned: IOwned<ILogger>;
begin
  // RegisterInstance does not register IOwned: RegisterOwned adds it
  logger := TFileLogger.Create('given.log');
  FContainer.RegisterInstance<ILogger>(logger);
  FContainer.RegisterOwned<ILogger>;
  owned := FContainer.Resolve<IOwned<ILogger>>;
  Assert.IsTrue((owned.Value as TObject) = (logger as TObject), 'IOwned<I> must wrap what Resolve<I> returns');
  owned := nil;
  // a later registration: its IOwned is already there, and wraps the new one
  FContainer.RegisterType<ILogger, TConsoleLogger>;
  FContainer.RegisterOwned<ILogger>;
  Assert.AreEqual(1, Integer(FContainer.Registrator.Dependencies[FContainer.Registrator.GetKey(TypeInfo(IOwned<ILogger>))].Count),
    'One IOwned per key, not duplicated');
  owned := FContainer.Resolve<IOwned<ILogger>>;
  Assert.IsTrue((owned.Value as TObject) is TConsoleLogger, 'IOwned<I> must follow a registration added later');
end;

procedure TQuickIOCTests.Test_Owned_RegisterOwned_WithoutRegistration_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      FContainer.RegisterOwned<ILogger>;
    end, EIocRegisterError, 'RegisterOwned<I> before any registration of I must raise EIocRegisterError');
end;

procedure TQuickIOCTests.Test_Owned_NotRegistered_ConsumerRaisesRegisterError;
var
  error: string;
begin
  // the non-generic RegisterType never registers IOwned
  FContainer.RegisterType(TypeInfo(ILogger), TConsoleLogger);
  FContainer.RegisterType<ILoggerConsumer, TOwnedConsumer>;
  error := '';
  try
    FContainer.Resolve<ILoggerConsumer>;
  except
    on E: Exception do error := E.ClassName + ': ' + E.Message;
  end;
  Assert.IsTrue(error.StartsWith('EIocRegisterError:'),
    'Asking for an unregistered IOwned must raise, not fall back to TObject.Create. Got: ' + error);
  Assert.IsTrue((Pos('TOwnedConsumer.Create asks for IOwned<', error) > 0) and (Pos('RegisterOwned', error) > 0),
    'The message must name the constructor and the fix. Got: ' + error);
end;

procedure TQuickIOCTests.Test_Owned_GivenInstanceOnTop_OwnedWrapsIt;
var
  mock: ILogger;
  consumer: ILoggerConsumer;
begin
  // a mock given with RegisterInstance<I> on top of the production registration: Resolve<I>
  // returns the mock, and IOwned<I> must wrap it too
  FContainer.RegisterType<ILogger, TConsoleLogger>;
  mock := TFileLogger.Create('mock');
  FContainer.RegisterInstance<ILogger>(mock);
  FContainer.RegisterType<ILoggerConsumer, TOwnedConsumer>;
  Assert.IsTrue(FContainer.Resolve<ILogger> = mock, 'Resolve<ILogger> must return the given instance');
  consumer := FContainer.Resolve<ILoggerConsumer>;
  Assert.IsTrue(consumer.Logger = mock, 'IOwned<ILogger> must wrap what Resolve<ILogger> returns: the given instance');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickIOCTests);
end.
