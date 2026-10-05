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
  System.SyncObjs,
  System.Diagnostics,
  System.Rtti,
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

  // depends on IInjService without [Inject]: the default rule tries Create(service), then
  // the inherited TObject.Create
  IInjOuter = interface
  ['{B3F7C1E9-5D2A-4E86-9A41-0C6E8D3B7F25}']
    function Service: IInjService;
  end;

  TInjOuter = class(TInterfacedObject, IInjOuter)
  private
    FService: IInjService;
  public
    constructor Create(service: IInjService);
    function Service: IInjService;
  end;

  // own Create and own Create(logger) without [Inject]: the default rule picks the
  // parameterless one even with ILogger registered
  TParameterlessHidesLogger = class(TInterfacedObject, IInjService)
  private
    FLogger: ILogger;
  public
    constructor Create; overload;
    constructor Create(logger: ILogger); overload;
    function Logger: ILogger;
  end;

  // own Create and own Create(name): the second one injects nothing
  TParameterlessAndValue = class(TInterfacedObject, IInjService)
  public
    constructor Create; overload;
    constructor Create(const name: string); overload;
    function Logger: ILogger;
  end;

  // counts constructions, to check what Build pre-creates
  TBuildCountedLogger = class(TInterfacedObject, ILogger)
  private class var
    FCreated: Integer;
  public
    constructor Create;
    procedure Log(const msg: string);
    class property Created: Integer read FCreated write FCreated;
  end;

  // Singleton whose constructor is slow, for the concurrency tests
  ISlowSingleton = interface
  ['{4D8A2F6C-1B3E-4975-A0C8-E7F2B5D19A63}']
  end;

  TSlowSingleton = class(TInterfacedObject, ISlowSingleton)
  private class var
    FCreated: Integer;
    FDelayMs: Integer;
    FStarted: TLightweightEvent;
    FRelease: TLightweightEvent;
  public
    constructor Create;
    class property Created: Integer read FCreated write FCreated;
    class property DelayMs: Integer read FDelayMs write FDelayMs;
    // signaled when the constructor starts, i.e. while the singleton lock is held
    class property Started: TLightweightEvent read FStarted write FStarted;
    // when assigned, the constructor waits for it instead of sleeping DelayMs
    class property Release: TLightweightEvent read FRelease write FRelease;
  end;

  // singleton whose constructor waits for a thread that resolves another singleton (ILogger), as a
  // constructor waiting for a worker thread, or for TThread.Synchronize, would
  IWaitsForAnother = interface
  ['{0661C77C-F997-49FF-9CBE-A18613A9783E}']
  end;

  TWaitsForAnother = class(TInterfacedObject, IWaitsForAnother)
  private class var
    FContainer: TIocContainer;
    FResolved: TLightweightEvent;
    FWorker: TThread;
    FWorkerError: string;
    FWaitResult: TWaitResult;
  public
    constructor Create;
    class property Container: TIocContainer read FContainer write FContainer;
    // signaled by the thread once it resolved ILogger
    class property Resolved: TLightweightEvent read FResolved write FResolved;
    // the thread started by the constructor: the test waits for it and frees it
    class property Worker: TThread read FWorker write FWorker;
    class property WorkerError: string read FWorkerError write FWorkerError;
    // wrTimeout: the thread could not resolve ILogger while the constructor ran
    class property WaitResult: TWaitResult read FWaitResult write FWaitResult;
  end;

  // plain class (no interface) registered with RegisterInstance<T>: slow, counted constructor
  TSlowPlainSingleton = class
  private class var
    FCreated: Integer;
  public
    constructor Create;
    class property Created: Integer read FCreated write FCreated;
  end;

  EFlakyCreation = class(Exception);

  // plain class whose constructor fails while FailNext is set
  TFlakySingleton = class
  private class var
    FFailNext: Boolean;
    FCreated: Integer;
  public
    constructor Create;
    class property FailNext: Boolean read FFailNext write FFailNext;
    class property Created: Integer read FCreated write FCreated;
  end;

  // singleton holding a factory
  IFactoryHolder = interface
  ['{9E4F478D-B419-44DC-A4D4-F1F2082F961D}']
    function Factory: IFactory<IUserService>;
  end;

  TFactoryHolder = class(TInterfacedObject, IFactoryHolder)
  private
    FFactory: IFactory<IUserService>;
  public
    constructor Create(factory: IFactory<IUserService>);
    function Factory: IFactory<IUserService>;
  end;

  // subclass for AbstractFactory(aClass)
  TUserServiceChild = class(TUserService)
  public
    constructor Create(logger: ILogger);
  end;

  // declares only a parameterless constructor, which sets a field
  TParentWithInit = class(TInterfacedObject, IInjService)
  private
    FInitialized: Boolean;
  public
    constructor Create;
    function Logger: ILogger;
  end;

  // no constructor of its own: TParentWithInit.Create and TObject.Create both have no parameters
  TChildOfInit = class(TParentWithInit);

  // its own constructor is not satisfiable (IEmailService is not registered): the inherited
  // TParentWithInit.Create would be used
  TOwnUnsatisfiable = class(TParentWithInit)
  public
    constructor Create(email: IEmailService);
  end;

  // plain class for ResolveAll with a class type
  TPlainThing = class(TObject);

  // dependency cycle: TCycleA needs ICycleB, TCycleB needs ICycleA
  ICycleA = interface
  ['{ACBA9127-180D-4ADA-AFC2-512DF25E57CB}']
  end;

  ICycleB = interface
  ['{2ECF492B-3B68-411E-B26E-BFC1C3DCBEAF}']
  end;

  TCycleA = class(TInterfacedObject, ICycleA)
  public
    constructor Create(b: ICycleB);
  end;

  TCycleB = class(TInterfacedObject, ICycleB)
  public
    constructor Create(a: ICycleA);
  end;

  // its constructor runs OnCreate, as Application.ProcessMessages runs a message handler while the
  // service is being built; the handler may resolve this same service again (re-entry, no cycle)
  IReentrant = interface
  ['{491B4B5F-5A7B-47C8-B87D-F52F6C632822}']
  end;

  TReentrant = class(TInterfacedObject, IReentrant)
  private class var
    FOnCreate: TProc;
  public
    constructor Create;
    class property OnCreate: TProc read FOnCreate write FOnCreate;
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

  // singleton whose destructor uses a factory: it runs while the container is being destroyed
  IUsesFactoryOnDestroy = interface
  ['{30E2F7AE-C280-4AAF-8C04-B3715EA3C173}']
  end;

  TUsesFactoryOnDestroy = class(TInterfacedObject, IUsesFactoryOnDestroy)
  private class var
    FOutcome: string;
  private
    FFactory: IFactory<IUserService>;
  public
    constructor Create(factory: IFactory<IUserService>);
    destructor Destroy; override;
    // 'ok', or the exception raised by the factory in the destructor
    class property Outcome: string read FOutcome write FOutcome;
  end;

  // runs OnDestroy in its destructor: used as a class singleton (freed by the container) and as
  // a scoped service (released by its scope)
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

  // class singletons that log their release; TReleaseLoggedA receives TReleaseLoggedB
  TReleaseLogged = class
  private class var
    FReleased: string;
  public
    destructor Destroy; override;
    // class names in the order they were released, separated by ";"
    class property Released: string read FReleased write FReleased;
  end;

  TReleaseLoggedB = class(TReleaseLogged);

  TReleaseLoggedA = class(TReleaseLogged)
  private
    FB: TReleaseLoggedB;
  public
    constructor Create(b: TReleaseLoggedB);
  end;

  // destructor that raises after releasing its scoped logger
  TExplodingWithLogger = class(TExplodingOnDestroy)
  private
    FLogger: ILogger;
  public
    constructor Create(logger: ILogger);
    destructor Destroy; override;
  end;

  // [Inject] constructor that fails on its second parameter (IEmailService is not registered),
  // after the first one was already created
  TInjectFailsAfterExploding = class(TInterfacedObject, IInjService)
  public
    [Inject]
    constructor Create(exploding: IExploding; email: IEmailService);
    function Logger: ILogger;
  end;

  // implements no service interface: registering it for ILogger is a configuration error
  TNotALogger = class(TInterfacedObject);

  // interface declared without a GUID
  INoGuid = interface
    procedure Run;
  end;

  TNoGuidService = class(TInterfacedObject, INoGuid)
  public
    procedure Run;
  end;

  // two own constructors with one parameter each, both satisfiable when both are registered
  TTiedConstructors = class(TInterfacedObject, IInjService)
  public
    constructor Create(logger: ILogger); overload;
    constructor Create(email: IEmailService); overload;
    function Logger: ILogger;
  end;

  // constructor with an untyped parameter: the container has nothing to pass to it
  TUntypedParam = class(TInterfacedObject, IInjService)
  public
    constructor Create(const data);
    function Logger: ILogger;
  end;

  // production e-mail service: needs SMTP settings that a test project does not register
  ISmtpSettings = interface
  ['{A304F1AF-91F2-4EE6-AC1A-94B678B43CD6}']
  end;

  TSmtpEmailService = class(TInterfacedObject, IEmailService)
  private class var
    FCreated: Integer;
  public
    [Inject]
    constructor Create(settings: ISmtpSettings);
    procedure SendEmail(const mailto, subject, body: string);
    class property Created: Integer read FCreated write FCreated;
  end;

  // asks only for IOwned<ILogger>
  TOwnedConsumer = class(TInterfacedObject, IInjService)
  private
    FOwned: IOwned<ILogger>;
  public
    constructor Create(owned: IOwned<ILogger>);
    function Logger: ILogger;
  end;

  // an empty Create (kept for tests) next to the one that asks for IOwned<ILogger>
  TOwnedOrEmpty = class(TInterfacedObject, IInjService)
  public
    constructor Create; overload;
    constructor Create(owned: IOwned<ILogger>); overload;
    function Logger: ILogger;
  end;

  // the mock a test registers on top of the production registration
  TFakeEmailService = class(TInterfacedObject, IEmailService)
  private class var
    FCreated: Integer;
  public
    constructor Create;
    procedure SendEmail(const mailto, subject, body: string);
    class property Created: Integer read FCreated write FCreated;
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

  // another options class: a TOptions, but not a TAppSettings
  TOtherSettings = class(TOptions)
  private
    FPort: Integer;
  published
    property Port: Integer read FPort write FPort;
  end;

  [TestFixture]
  TQuickIOCTests = class(TObject)
  private
    FContainer: TIocContainer;
    FInstances: TArray<Pointer>;
    FErrors: TArray<string>;
    function StartSlowResolver(aIndex: Integer): TThread;
    function StartPlainSingletonResolver(aIndex: Integer): TThread;
    function ResolveCycleInThread(aInScope: Boolean): string;
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
    procedure Test_Inject_UnsatisfiableAsDependency_RaisesInsteadOfFallback;
    [Test]
    procedure Test_Inject_Unsatisfiable_MessageCarriesRealCause;
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
    [Test]
    procedure Test_DiagnoseConstructors_WarnsWhenParameterlessHidesDependencies;
    [Test]
    procedure Test_Build_ConstructorWarnings_DoNotFail;
    [Test]
    procedure Test_DiagnoseConstructors_NoWarningWithoutHiddenDependency;
    { ResolveAll }
    [Test]
    procedure Test_ResolveAll_ReturnsEachRegistration;
    [Test]
    procedure Test_ResolveAll_KeepsEachRegistrationLifetime;
    [Test]
    procedure Test_ResolveAll_InScope_ScopedEntriesPerScope;
    [Test]
    procedure Test_ResolveAll_ScopedFromRoot_RaisesScopeError;
    { Singleton lock }
    [Test]
    procedure Test_Singleton_ConcurrentFirstResolve_CreatesOnce;
    [Test]
    procedure Test_Singleton_CreatedOne_DoesNotWaitForAnotherBeingCreated;
    { Build and IOwned resolve each registration }
    [Test]
    procedure Test_Build_PreCreatesOnlyWhatResolveReturns;
    [Test]
    procedure Test_Build_LastRegistrationScoped_DoesNotFail;
    [Test]
    procedure Test_Build_RegistrationWithoutClass_NamesTheInterface;
    [Test]
    procedure Test_Owned_ResolveAll_EachRegistration;
    [Test]
    procedure Test_Owned_TargetRemoved_NotRegistered;
    { Scope in DelegateTo and factories }
    [Test]
    procedure Test_DelegateTo_Context_ResolvesFromCurrentScope;
    [Test]
    procedure Test_DelegateTo_Context_ScopeIsNilOutsideScope;
    [Test]
    procedure Test_DelegateTo_Context_FromRoot_ScopedDependencyRaises;
    [Test]
    procedure Test_SimpleFactory_InScope_UsesScopedDependencies;
    [Test]
    procedure Test_SimpleFactory_FromRoot_ScopedDependencyRaises;
    [Test]
    procedure Test_TypedFactory_InScope_UsesScopedDependencies;
    [Test]
    procedure Test_Scope_AbstractFactory_UsesScope;
    { RegisterTypedFactory returns its registration }
    [Test]
    procedure Test_TypedFactory_AsSingleton_SharesFactory;
    [Test]
    procedure Test_TypedFactory_AsSingleton_NotBoundToFirstScope;
    { Scope lifetime in factories and resolve context }
    [Test]
    procedure Test_SimpleFactory_UsedAfterScopeFreed_RaisesScopeError;
    [Test]
    procedure Test_TypedFactory_UsedAfterScopeFreed_RaisesScopeError;
    [Test]
    procedure Test_DelegateTo_ContextKeptAfterScopeFreed_RaisesScopeError;
    [Test]
    procedure Test_TypedFactory_AsSingleton_UsableAfterScopeFreed;
    { Coverage of paths changed by the fork }
    [Test]
    procedure Test_Singleton_ClassRegistration_ConcurrentFirstResolve_CreatesOnce;
    [Test]
    procedure Test_Singleton_CreationFails_NextResolveRetries;
    [Test]
    procedure Test_SimpleFactory_InjectedInSingleton_BoundToRoot;
    [Test]
    procedure Test_DelegateTo_Context_SingletonScopeIsNil;
    [Test]
    procedure Test_SimpleFactory_AsSingleton_SharesFactoryBoundToRoot;
    [Test]
    procedure Test_DelegateTo_LastOneWins;
    [Test]
    procedure Test_Scope_AbstractFactory_WithClass_CreatesThatClass;
    [Test]
    procedure Test_DiagnoseConstructors_ReportsUnsatisfiableOwnConstructors;
    [Test]
    procedure Test_DefaultRule_InheritedParameterlessConstructorRuns;
    [Test]
    procedure Test_ResolveAll_WithName_ReturnsOnlyThatName;
    [Test]
    procedure Test_ResolveAll_ClassType_KeepsEachLifetime;
    { Pending items of the first review }
    [Test]
    procedure Test_Owned_RemoveRegistrations_RemovesItsOwned;
    [Test]
    procedure Test_Owned_RemoveAndRegisterAgain_OneOwnedPerRegistration;
    [Test]
    procedure Test_Exceptions_ShareEIocErrorBase;
    [Test]
    procedure Test_Cycle_Transient_RaisesCycleError;
    [Test]
    procedure Test_Cycle_Scoped_RaisesCycleError;
    [Test]
    procedure Test_Scope_Free_ReleasesAllEvenIfOneDestructorRaises;
    [Test]
    procedure Test_Container_Free_ReleasesSingletonsBeforeResolver;
    // robust destruction (package A)
    [Test]
    procedure Test_Container_Free_ReleasesSingletonsInReverseCreationOrder;
    [Test]
    procedure Test_Container_Free_ReleasesAllEvenIfOneDestructorRaises;
    [Test]
    procedure Test_Container_Free_ReleasedSingletonIsNotBuiltAgain;
    [Test]
    procedure Test_Container_Free_SingletonBeingFreedIsNotResolved;
    [Test]
    procedure Test_Scope_ResolveWhileBeingFreed_RaisesScopeError;
    [Test]
    procedure Test_Scope_Destroy_SafeWhenCreateDidNotFinish;
    [Test]
    procedure Test_Owned_Release_FreesScopeEvenIfValueDestructorRaises;
    [Test]
    procedure Test_Owned_ResolutionFailure_NotHiddenByScopeRelease;
    // diagnostics and messages (package B)
    [Test]
    procedure Test_DiagnoseConstructors_ReportsClassNotImplementingInterface;
    [Test]
    procedure Test_DiagnoseConstructors_ReportsInterfaceWithoutGuid;
    [Test]
    procedure Test_Resolve_ClassNotImplementingInterface_RaisesRegisterError;
    [Test]
    procedure Test_Build_ContainerError_KeepsItsClass;
    [Test]
    procedure Test_Build_ConstructorFailure_KeepsOriginalAsInner;
    [Test]
    procedure Test_DiagnoseConstructors_ChecksSingletonAlreadyBuilt;
    [Test]
    procedure Test_DiagnoseConstructors_WarnsOnTiedConstructors;
    [Test]
    procedure Test_Resolve_UntypedConstructorParameter_NotUsed;
    [Test]
    procedure Test_DelegateTo_NamedFunction_Compiles;
    [Test]
    procedure Test_Build_MockOnTop_OriginalNotBuilt;
    [Test]
    procedure Test_Build_ValidateConstructors_OverriddenRegistrationOnlyWarns;
    // IOwned configurable (package G)
    [Test]
    procedure Test_Owned_AutoRegisterOff_NotRegistered;
    [Test]
    procedure Test_Owned_RegisterOwned_CompletesAndKeepsOrder;
    [Test]
    procedure Test_Owned_RegisterOwned_WithoutRegistration_Raises;
    [Test]
    procedure Test_Owned_NotRegistered_ConsumerRaisesRegisterError;
    [Test]
    procedure Test_DiagnoseConstructors_ReportsOwnedNotRegistered;
    // API polish (package C)
    [Test]
    procedure Test_RegisterOptions_WrongClass_RaisesInvalidCast;
    [Test]
    procedure Test_ResolveContext_NotFromContainer_RaisesIocError;
    // singleton lock and re-entry (fourth review)
    [Test]
    procedure Test_Singleton_ConstructorWaitingForAnotherBeingCreated_DoesNotDeadlock;
    [Test]
    procedure Test_Cycle_ReentryInSameThread_MessageExplainsIt;
    [Test]
    procedure Test_Singleton_ClassDelegateReturnsNil_ResolvingKeepsNoMemory;
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

constructor TInjOuter.Create(service: IInjService);
begin
  FService := service;
end;

function TInjOuter.Service: IInjService;
begin
  Result := FService;
end;

{ TParameterlessHidesLogger }

constructor TParameterlessHidesLogger.Create;
begin
  inherited Create;
end;

constructor TParameterlessHidesLogger.Create(logger: ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

function TParameterlessHidesLogger.Logger: ILogger;
begin
  Result := FLogger;
end;

{ TParameterlessAndValue }

constructor TParameterlessAndValue.Create;
begin
  inherited Create;
end;

constructor TParameterlessAndValue.Create(const name: string);
begin
  inherited Create;
end;

function TParameterlessAndValue.Logger: ILogger;
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
  Assert.WillRaiseDescendant(
    procedure
    begin
      FContainer.Resolve<IInjService>;
    end, EIocResolverError, 'An unsatisfiable [Inject] constructor must raise, not fall back');
end;

procedure TQuickIOCTests.Test_Inject_UnsatisfiableAsDependency_RaisesInsteadOfFallback;
var
  outer: IInjOuter;
  failure: string;
begin
  // TInjOuter has no [Inject] and depends on IInjService; the [Inject] constructor of TInjMarked
  // needs ILogger, which is not registered. The consumer must not swallow that error and fall
  // back to TObject.Create, which would leave its service nil
  FContainer.RegisterType<IInjService, TInjMarked>.AsTransient;
  FContainer.RegisterType<IInjOuter, TInjOuter>.AsTransient;
  failure := '';
  try
    outer := FContainer.Resolve<IInjOuter>;
    if outer.Service = nil then
      failure := 'No exception: TInjOuter fell back to TObject.Create and was created with Service = nil'
    else
      failure := 'No exception: TInjOuter was created';
  except
    on EIocResolverError do ; // expected
  end;
  outer := nil;
  if failure <> '' then Assert.Fail(failure);
end;

procedure TQuickIOCTests.Test_Inject_Unsatisfiable_MessageCarriesRealCause;
var
  msg: string;
begin
  // ILogger is not registered: the [Inject] error must say which parameter failed and why, not
  // only that "a dependency could not be resolved"
  FContainer.RegisterType<IInjService, TInjMarked>.AsTransient;
  msg := '';
  try
    FContainer.Resolve<IInjService>;
  except
    on E: EIocInjectError do msg := E.Message;
  end;
  Assert.IsTrue(Pos('parameter "logger: ILogger": Type "ILogger" not registered', msg) > 0,
    'The message must carry the real cause. Message: ' + msg);
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

procedure TQuickIOCTests.Test_DiagnoseConstructors_WarnsWhenParameterlessHidesDependencies;
var
  problems, warnings: TArray<string>;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IInjService, TParameterlessHidesLogger>.AsTransient;
  problems := FContainer.DiagnoseConstructors(warnings);
  Assert.AreEqual(0, Integer(Length(problems)),
    'Not an error: the author may prefer the parameterless constructor. Found: ' + string.Join(' | ', problems));
  Assert.AreEqual(1, Integer(Length(warnings)),
    'The parameterless constructor hides Create(logger) with ILogger registered: one warning expected. Found: ' +
    string.Join(' | ', warnings));
  Assert.IsTrue(Pos('TParameterlessHidesLogger.Create(logger: ILogger)', warnings[0]) > 0,
    'The warning must name the constructor that is not used. Warning: ' + warnings[0]);
end;

procedure TQuickIOCTests.Test_Build_ConstructorWarnings_DoNotFail;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IInjService, TParameterlessHidesLogger>.AsTransient;
  FContainer.ValidateConstructors := True;
  Assert.WillNotRaise(
    procedure
    begin
      FContainer.Build;
    end, nil, 'A warning must not make Build fail');
  Assert.AreEqual(1, Integer(Length(FContainer.ConstructorWarnings)),
    'Build must keep the warning in ConstructorWarnings. Found: ' + string.Join(' | ', FContainer.ConstructorWarnings));
end;

procedure TQuickIOCTests.Test_DiagnoseConstructors_NoWarningWithoutHiddenDependency;
var
  problems, warnings: TArray<string>;
begin
  // single constructors, an inherited [Inject] one and Create(name) next to Create: nothing hidden
  RegisterGraph(FContainer);
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IInjService, TInjDerived>.AsTransient;
  FContainer.RegisterType<IInjService, TParameterlessAndValue>.AsTransient;
  problems := FContainer.DiagnoseConstructors(warnings);
  Assert.AreEqual(0, Integer(Length(problems)), 'No problems expected. Found: ' + string.Join(' | ', problems));
  Assert.AreEqual(0, Integer(Length(warnings)), 'No warnings expected. Found: ' + string.Join(' | ', warnings));
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

{ TSlowSingleton }

constructor TSlowSingleton.Create;
begin
  TInterlocked.Increment(FCreated);
  if FStarted <> nil then FStarted.SetEvent;
  if FRelease <> nil then FRelease.WaitFor(10000)
    else Sleep(FDelayMs);
end;

{ TWaitsForAnother }

constructor TWaitsForAnother.Create;
begin
  inherited Create;
  FWorker := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        FContainer.Resolve<ILogger>;
      except
        on E: Exception do FWorkerError := E.ClassName + ': ' + E.Message;
      end;
      FResolved.SetEvent;
    end);
  FWorker.FreeOnTerminate := False;
  FWorker.Start;
  // without a timeout, a deadlock would hang the test run
  FWaitResult := FResolved.WaitFor(5000);
end;

{ TSlowPlainSingleton }

constructor TSlowPlainSingleton.Create;
begin
  TInterlocked.Increment(FCreated);
  Sleep(100);
end;

{ TFlakySingleton }

constructor TFlakySingleton.Create;
begin
  if FFailNext then raise EFlakyCreation.Create('Simulated failure while creating the singleton');
  TInterlocked.Increment(FCreated);
end;

{ TFactoryHolder }

constructor TFactoryHolder.Create(factory: IFactory<IUserService>);
begin
  FFactory := factory;
end;

function TFactoryHolder.Factory: IFactory<IUserService>;
begin
  Result := FFactory;
end;

{ TUserServiceChild }

constructor TUserServiceChild.Create(logger: ILogger);
begin
  inherited Create(logger);
end;

{ TParentWithInit }

constructor TParentWithInit.Create;
begin
  inherited Create;
  FInitialized := True;
end;

function TParentWithInit.Logger: ILogger;
begin
  Result := nil;
end;

{ TOwnUnsatisfiable }

constructor TOwnUnsatisfiable.Create(email: IEmailService);
begin
  inherited Create;
end;

{ TCycleA }

constructor TCycleA.Create(b: ICycleB);
begin
  inherited Create;
end;

{ TCycleB }

constructor TCycleB.Create(a: ICycleA);
begin
  inherited Create;
end;

{ TReentrant }

constructor TReentrant.Create;
var
  handler: TProc;
begin
  inherited Create;
  // once: the instance the handler resolves does not run it again
  handler := FOnCreate;
  FOnCreate := nil;
  if Assigned(handler) then handler();
end;

{ TExplodingOnDestroy }

destructor TExplodingOnDestroy.Destroy;
begin
  inherited;
  raise EExplodingDestroy.Create('Simulated failure in a scoped destructor');
end;

{ TUsesFactoryOnDestroy }

constructor TUsesFactoryOnDestroy.Create(factory: IFactory<IUserService>);
begin
  inherited Create;
  FFactory := factory;
end;

destructor TUsesFactoryOnDestroy.Destroy;
var
  user: IUserService;
begin
  try
    user := FFactory.New;
    user := nil;
    FOutcome := 'ok';
  except
    on E: Exception do FOutcome := E.ClassName + ': ' + E.Message;
  end;
  FFactory := nil;
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

{ TReleaseLogged }

destructor TReleaseLogged.Destroy;
begin
  FReleased := FReleased + ClassName + ';';
  inherited;
end;

{ TReleaseLoggedA }

constructor TReleaseLoggedA.Create(b: TReleaseLoggedB);
begin
  inherited Create;
  FB := b;
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

{ TInjectFailsAfterExploding }

constructor TInjectFailsAfterExploding.Create(exploding: IExploding; email: IEmailService);
begin
  inherited Create;
end;

function TInjectFailsAfterExploding.Logger: ILogger;
begin
  Result := nil;
end;

// keeps a TTrackedLogger alive only through a delegate of aContainer: it is released when the
// container frees its registrations
procedure HoldInDelegate(aContainer: TIocContainer);
var
  held: ILogger;
begin
  held := TTrackedLogger.Create;
  aContainer.RegisterType(TypeInfo(ILogger), TTrackedLogger, 'held').ActivatorDelegate :=
    function: TValue
    begin
      Result := TValue.From<ILogger>(held);
    end;
end;

{ TNoGuidService }

procedure TNoGuidService.Run;
begin
end;

{ TTiedConstructors }

constructor TTiedConstructors.Create(logger: ILogger);
begin
  inherited Create;
end;

constructor TTiedConstructors.Create(email: IEmailService);
begin
  inherited Create;
end;

function TTiedConstructors.Logger: ILogger;
begin
  Result := nil;
end;

{ TUntypedParam }

constructor TUntypedParam.Create(const data);
begin
  inherited Create;
end;

function TUntypedParam.Logger: ILogger;
begin
  Result := nil;
end;

{ TSmtpEmailService }

constructor TSmtpEmailService.Create(settings: ISmtpSettings);
begin
  inherited Create;
  Inc(FCreated);
end;

procedure TSmtpEmailService.SendEmail(const mailto, subject, body: string);
begin
end;

{ TFakeEmailService }

constructor TFakeEmailService.Create;
begin
  inherited Create;
  Inc(FCreated);
end;

procedure TFakeEmailService.SendEmail(const mailto, subject, body: string);
begin
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

{ TOwnedOrEmpty }

constructor TOwnedOrEmpty.Create;
begin
  inherited Create;
end;

constructor TOwnedOrEmpty.Create(owned: IOwned<ILogger>);
begin
  inherited Create;
end;

function TOwnedOrEmpty.Logger: ILogger;
begin
  Result := nil;
end;

// named function for DelegateTo
function NewConsoleLogger: TConsoleLogger;
begin
  Result := TConsoleLogger.Create;
end;

// transient class instances (RegisterInstance<T>) belong to the caller: free each distinct one
procedure FreeDistinct(const aObjects: TArray<Pointer>);
var
  freed: TList<Pointer>;
  obj: Pointer;
begin
  freed := TList<Pointer>.Create;
  try
    for obj in aObjects do
      if (obj <> nil) and not freed.Contains(obj) then
      begin
        freed.Add(obj);
        TObject(obj).Free;
      end;
  finally
    freed.Free;
  end;
end;

{ Singleton lock }

function TQuickIOCTests.StartSlowResolver(aIndex: Integer): TThread;
begin
  // aIndex is a parameter, so each thread captures its own value
  Result := TThread.CreateAnonymousThread(
    procedure
    var
      svc: ISlowSingleton;
    begin
      try
        svc := FContainer.Resolve<ISlowSingleton>;
        FInstances[aIndex] := Pointer(svc as TObject);
      except
        on E: Exception do FErrors[aIndex] := E.ClassName + ': ' + E.Message;
      end;
    end);
  Result.FreeOnTerminate := False;
  Result.Start;
end;

procedure TQuickIOCTests.Test_Singleton_ConcurrentFirstResolve_CreatesOnce;
const
  // not "THREADS": Delphi identifiers are case-insensitive and it would clash with a variable
  THREAD_COUNT = 8;
var
  resolvers: array[0..THREAD_COUNT - 1] of TThread;
  i: Integer;
begin
  FContainer.RegisterType<ISlowSingleton, TSlowSingleton>.AsSingleton;
  TSlowSingleton.Created := 0;
  TSlowSingleton.DelayMs := 100;
  TSlowSingleton.Started := nil;
  SetLength(FInstances, THREAD_COUNT);
  SetLength(FErrors, THREAD_COUNT);
  for i := 0 to THREAD_COUNT - 1 do resolvers[i] := StartSlowResolver(i);
  for i := 0 to THREAD_COUNT - 1 do
  begin
    resolvers[i].WaitFor;
    resolvers[i].Free;
  end;
  for i := 0 to THREAD_COUNT - 1 do
    Assert.AreEqual('', FErrors[i], 'Thread ' + IntToStr(i) + ' failed');
  Assert.AreEqual(1, TSlowSingleton.Created, 'Concurrent first resolutions must create the singleton once');
  for i := 1 to THREAD_COUNT - 1 do
    Assert.IsTrue(FInstances[i] = FInstances[0], 'All threads must get the same instance');
end;

procedure TQuickIOCTests.Test_Singleton_CreatedOne_DoesNotWaitForAnotherBeingCreated;
var
  slow: TThread;
  other: TThread;
  resolved: TLightweightEvent;
  otherError: string;
begin
  // while a slow singleton is being created (its lock held), resolving another singleton that
  // already exists must not wait for it. Checked by order of events, not by time: the slow
  // constructor is held until the other resolution finished
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  FContainer.RegisterType<ISlowSingleton, TSlowSingleton>.AsSingleton;
  FContainer.Resolve<ILogger>;
  TSlowSingleton.Created := 0;
  TSlowSingleton.Started := TLightweightEvent.Create;
  TSlowSingleton.Release := TLightweightEvent.Create;
  resolved := TLightweightEvent.Create;
  SetLength(FInstances, 1);
  SetLength(FErrors, 1);
  otherError := '';
  try
    slow := StartSlowResolver(0);
    try
      Assert.IsTrue(TSlowSingleton.Started.WaitFor(5000) = wrSignaled, 'The slow singleton must start being created');
      other := TThread.CreateAnonymousThread(
        procedure
        begin
          try
            FContainer.Resolve<ILogger>;
          except
            on E: Exception do otherError := E.ClassName + ': ' + E.Message;
          end;
          resolved.SetEvent;
        end);
      other.FreeOnTerminate := False;
      other.Start;
      try
        // the slow constructor is still held here: only the fast path can finish
        Assert.IsTrue(resolved.WaitFor(5000) = wrSignaled,
          'Resolving an existing singleton must not wait for another one being created');
        Assert.AreEqual('', otherError, 'Resolving the existing singleton failed');
      finally
        TSlowSingleton.Release.SetEvent;
        other.WaitFor;
        other.Free;
      end;
    finally
      TSlowSingleton.Release.SetEvent;
      slow.WaitFor;
      slow.Free;
    end;
    Assert.AreEqual('', FErrors[0], 'The slow resolution failed');
  finally
    resolved.Free;
    TSlowSingleton.Started.Free;
    TSlowSingleton.Started := nil;
    TSlowSingleton.Release.Free;
    TSlowSingleton.Release := nil;
  end;
end;

function TQuickIOCTests.StartPlainSingletonResolver(aIndex: Integer): TThread;
begin
  Result := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        FInstances[aIndex] := FContainer.Resolve<TSlowPlainSingleton>;
      except
        on E: Exception do FErrors[aIndex] := E.ClassName + ': ' + E.Message;
      end;
    end);
  Result.FreeOnTerminate := False;
  Result.Start;
end;

{ TBuildCountedLogger }

constructor TBuildCountedLogger.Create;
begin
  Inc(FCreated);
end;

procedure TBuildCountedLogger.Log(const msg: string);
begin
end;

{ Build and IOwned resolve each registration }

procedure TQuickIOCTests.Test_Build_PreCreatesOnlyWhatResolveReturns;
var
  loggers: TList<ILogger>;
begin
  // two singletons under the same key: Resolve returns the last one, so Build pre-creates only
  // that one; the first is built once, by the first ResolveAll
  FContainer.RegisterType<ILogger, TBuildCountedLogger>.AsSingleton;
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  TBuildCountedLogger.Created := 0;
  FContainer.Build;
  Assert.AreEqual(0, TBuildCountedLogger.Created,
    'Build must not pre-create a singleton that a later registration of the same key overrides');
  FContainer.ResolveAll<ILogger>.Free;
  FContainer.ResolveAll<ILogger>.Free;
  Assert.AreEqual(1, TBuildCountedLogger.Created, 'The overridden singleton is built once, by the first ResolveAll');
end;

procedure TQuickIOCTests.Test_Build_LastRegistrationScoped_DoesNotFail;
begin
  // Build pre-creates only what Resolve returns, and the last registration of the key is scoped:
  // it builds nothing. Neither the scoped one outside a scope (which raised EIocScopeError wrapped
  // in EIocBuildError) nor the singleton the scoped registration overrides
  FContainer.RegisterType<ILogger, TBuildCountedLogger>.AsSingleton;
  FContainer.RegisterType<ILogger, TFileLogger>.AsScoped;
  TBuildCountedLogger.Created := 0;
  Assert.WillNotRaise(
    procedure
    begin
      FContainer.Build;
    end, nil, 'Build must not resolve the scoped registration outside a scope');
  Assert.AreEqual(0, TBuildCountedLogger.Created, 'Build must not create the singleton the scoped registration overrides');
end;

procedure TQuickIOCTests.Test_Build_RegistrationWithoutClass_NamesTheInterface;
var
  raisedClass, msg: string;
begin
  // a singleton registration with no implementation class fails to build; the error message
  // must name the interface instead of reading the class name of a nil class
  FContainer.RegisterType(TypeInfo(ILogger), nil).AsSingleton;
  raisedClass := '';
  msg := '';
  try
    FContainer.Build;
  except
    on E: Exception do
    begin
      raisedClass := E.ClassName;
      msg := E.Message;
    end;
  end;
  Assert.AreEqual('EIocResolverError', raisedClass, 'Build must keep the class of the container error. Message: ' + msg);
  Assert.IsTrue(Pos('Build Error on "ILogger', msg) > 0, 'The message must name the interface. Message: ' + msg);
end;

procedure TQuickIOCTests.Test_Owned_ResolveAll_EachRegistration;
var
  owned: TList<IOwned<ILogger>>;
begin
  // each IOwned<ILogger> registration must wrap its own ILogger registration
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<ILogger, TFileLogger>.AsTransient;
  owned := FContainer.ResolveAll<IOwned<ILogger>>();
  try
    Assert.AreEqual<Integer>(2, owned.Count, 'One IOwned per ILogger registration');
    Assert.IsTrue((owned[0].Value as TObject) is TConsoleLogger,
      'First IOwned must wrap TConsoleLogger. Got ' + (owned[0].Value as TObject).ClassName);
    Assert.IsTrue((owned[1].Value as TObject) is TFileLogger,
      'Second IOwned must wrap TFileLogger. Got ' + (owned[1].Value as TObject).ClassName);
  finally
    owned.Free;
  end;
end;

procedure TQuickIOCTests.Test_Owned_TargetRemoved_NotRegistered;
begin
  // the IOwned is removed with its target; the registrator's RegisterType does not register a
  // new IOwned (only TIocContainer.RegisterType does), so IOwned<ILogger> no longer exists
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.Registrator.RemoveRegistrations(FContainer.Registrator.GetKey(TypeInfo(ILogger)));
  FContainer.Registrator.RegisterType<ILogger, TFileLogger>.AsTransient;
  Assert.WillRaise(
    procedure
    begin
      FContainer.Resolve<IOwned<ILogger>>;
    end, EIocResolverError, 'An IOwned whose target was removed must be removed too');
end;

{ Scope in DelegateTo and factories }

procedure TQuickIOCTests.Test_DelegateTo_Context_ResolvesFromCurrentScope;
var
  scope: TIocScope;
  user: IUserService;
  logger: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  FContainer.RegisterType<IUserService, TUserService>.AsTransient.DelegateTo(
    function(const aContext: TIocResolveContext): TUserService
    begin
      Result := TUserService.Create(aContext.Resolve<ILogger>);
    end);
  scope := FContainer.CreateScope;
  try
    user := scope.Resolve<IUserService>;
    logger := scope.Resolve<ILogger>;
    Assert.AreSame(logger, TUserService(user as TObject).FLogger,
      'The delegate must receive the scoped logger of the current scope');
  finally
    user := nil;
    logger := nil;
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_DelegateTo_Context_ScopeIsNilOutsideScope;
var
  scope: TIocScope;
  seen: TIocScope;
  user: IUserService;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IUserService, TUserService>.AsTransient.DelegateTo(
    function(const aContext: TIocResolveContext): TUserService
    begin
      seen := aContext.Scope;
      Result := TUserService.Create(aContext.Resolve<ILogger>);
    end);
  scope := FContainer.CreateScope;
  try
    user := scope.Resolve<IUserService>;
    Assert.IsTrue(seen = scope, 'Inside a scope the context must carry that scope');
    user := FContainer.Resolve<IUserService>;
    Assert.IsTrue(seen = nil, 'Outside a scope the context scope must be nil');
  finally
    user := nil;
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_DelegateTo_Context_FromRoot_ScopedDependencyRaises;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  FContainer.RegisterType<IUserService, TUserService>.AsTransient.DelegateTo(
    function(const aContext: TIocResolveContext): TUserService
    begin
      Result := TUserService.Create(aContext.Resolve<ILogger>);
    end);
  Assert.WillRaise(
    procedure
    begin
      FContainer.Resolve<IUserService>;
    end, EIocScopeError, 'A scoped dependency resolved by the delegate outside a scope must raise');
end;

procedure TQuickIOCTests.Test_SimpleFactory_InScope_UsesScopedDependencies;
var
  scope: TIocScope;
  factory: IFactory<IUserService>;
  user1, user2: IUserService;
  logger: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  FContainer.RegisterSimpleFactory<IUserService, TUserService>;
  scope := FContainer.CreateScope;
  try
    factory := scope.Resolve<IFactory<IUserService>>;
    user1 := factory.New;
    user2 := factory.New;
    logger := scope.Resolve<ILogger>;
    Assert.AreNotSame(user1, user2, 'New must create a new instance each time');
    Assert.AreSame(logger, TUserService(user1 as TObject).FLogger, 'Created instances get the scope''s logger');
    Assert.AreSame(logger, TUserService(user2 as TObject).FLogger, 'Created instances get the scope''s logger');
  finally
    user1 := nil;
    user2 := nil;
    logger := nil;
    factory := nil;
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_SimpleFactory_FromRoot_ScopedDependencyRaises;
var
  factory: IFactory<IUserService>;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  FContainer.RegisterSimpleFactory<IUserService, TUserService>;
  factory := FContainer.Resolve<IFactory<IUserService>>;
  Assert.WillRaise(
    procedure
    begin
      factory.New;
    end, EIocScopeError, 'A factory resolved outside a scope cannot build scoped dependencies');
end;

procedure TQuickIOCTests.Test_TypedFactory_InScope_UsesScopedDependencies;
var
  scope: TIocScope;
  factory: IFactory<TUserService>;
  user: IUserService;
  logger: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  FContainer.RegisterTypedFactory<IFactory<TUserService>, TUserService>;
  scope := FContainer.CreateScope;
  try
    factory := scope.Resolve<IFactory<TUserService>>;
    user := factory.New;
    logger := scope.Resolve<ILogger>;
    Assert.AreSame(logger, TUserService(user as TObject).FLogger, 'Created instance gets the scope''s logger');
  finally
    user := nil;
    logger := nil;
    factory := nil;
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_Scope_AbstractFactory_UsesScope;
var
  scope: TIocScope;
  user: IUserService;
  logger: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  scope := FContainer.CreateScope;
  try
    user := scope.AbstractFactory<TUserService>;
    logger := scope.Resolve<ILogger>;
    Assert.AreSame(logger, TUserService(user as TObject).FLogger, 'AbstractFactory of a scope uses that scope');
  finally
    user := nil;
    logger := nil;
    scope.Free;
  end;
end;

{ RegisterTypedFactory returns its registration }

procedure TQuickIOCTests.Test_TypedFactory_AsSingleton_SharesFactory;
var
  factory1, factory2: IFactory<TUserService>;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  Assert.WillNotRaise(
    procedure
    begin
      FContainer.RegisterTypedFactory<IFactory<TUserService>, TUserService>.AsSingleton;
    end, nil, 'RegisterTypedFactory must return its registration, so the factory can be made singleton again');
  factory1 := FContainer.Resolve<IFactory<TUserService>>;
  factory2 := FContainer.Resolve<IFactory<TUserService>>;
  Assert.AreSame(factory1, factory2, 'AsSingleton: every resolution must return the same factory');
end;

procedure TQuickIOCTests.Test_TypedFactory_AsSingleton_NotBoundToFirstScope;
var
  scope: TIocScope;
  factory: IFactory<TUserService>;
begin
  // a singleton factory outlives every scope: created inside one, it must not keep it
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  Assert.WillNotRaise(
    procedure
    begin
      FContainer.RegisterTypedFactory<IFactory<TUserService>, TUserService>.AsSingleton;
    end, nil, 'RegisterTypedFactory must return its registration, so the factory can be made singleton again');
  scope := FContainer.CreateScope;
  try
    factory := scope.Resolve<IFactory<TUserService>>;
  finally
    scope.Free;
  end;
  Assert.WillRaise(
    procedure
    begin
      factory.New;
    end, EIocScopeError, 'A singleton factory is bound to the root, not to the scope it was first resolved in');
end;

{ Scope lifetime in factories and resolve context }

procedure TQuickIOCTests.Test_SimpleFactory_UsedAfterScopeFreed_RaisesScopeError;
var
  scope: TIocScope;
  factory: IFactory<IUserService>;
begin
  // transient dependencies only: the freed scope is passed along but never touched, so the
  // factory works on a dead scope without noticing (a scoped dependency would read freed memory)
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterSimpleFactory<IUserService, TUserService>;
  scope := FContainer.CreateScope;
  try
    factory := scope.Resolve<IFactory<IUserService>>;
  finally
    scope.Free;
  end;
  Assert.WillRaise(
    procedure
    begin
      factory.New;
    end, EIocScopeError, 'A factory used after its scope was freed must raise EIocScopeError');
end;

procedure TQuickIOCTests.Test_TypedFactory_UsedAfterScopeFreed_RaisesScopeError;
var
  scope: TIocScope;
  factory: IFactory<TUserService>;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterTypedFactory<IFactory<TUserService>, TUserService>;
  scope := FContainer.CreateScope;
  try
    factory := scope.Resolve<IFactory<TUserService>>;
  finally
    scope.Free;
  end;
  Assert.WillRaise(
    procedure
    begin
      factory.New.Free;
    end, EIocScopeError, 'A typed factory used after its scope was freed must raise EIocScopeError');
end;

procedure TQuickIOCTests.Test_DelegateTo_ContextKeptAfterScopeFreed_RaisesScopeError;
var
  scope: TIocScope;
  kept: TIocResolveContext;
  user: IUserService;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IUserService, TUserService>.AsTransient.DelegateTo(
    function(const aContext: TIocResolveContext): TUserService
    begin
      kept := aContext;
      Result := TUserService.Create(aContext.Resolve<ILogger>);
    end);
  scope := FContainer.CreateScope;
  try
    user := scope.Resolve<IUserService>;
  finally
    user := nil;
    scope.Free;
  end;
  Assert.WillRaise(
    procedure
    begin
      kept.Resolve<ILogger>;
    end, EIocScopeError, 'A context kept beyond the delegate call must not resolve in its freed scope');
end;

procedure TQuickIOCTests.Test_TypedFactory_AsSingleton_UsableAfterScopeFreed;
var
  scope: TIocScope;
  factory: IFactory<TUserService>;
begin
  // a singleton factory is bound to the root: freeing the scope it was first resolved in
  // must not affect it
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterTypedFactory<IFactory<TUserService>, TUserService>.AsSingleton;
  scope := FContainer.CreateScope;
  try
    factory := scope.Resolve<IFactory<TUserService>>;
  finally
    scope.Free;
  end;
  Assert.WillNotRaise(
    procedure
    begin
      factory.New.Free;
    end, nil, 'A singleton factory must keep working after the scope it was first resolved in is freed');
end;

{ Coverage of paths changed by the fork }

procedure TQuickIOCTests.Test_Singleton_ClassRegistration_ConcurrentFirstResolve_CreatesOnce;
const
  THREAD_COUNT = 8;
var
  resolvers: array[0..THREAD_COUNT - 1] of TThread;
  i: Integer;
begin
  // class registrations have their own branch in the singleton lock
  FContainer.RegisterInstance<TSlowPlainSingleton>.AsSingleton;
  TSlowPlainSingleton.Created := 0;
  SetLength(FInstances, THREAD_COUNT);
  SetLength(FErrors, THREAD_COUNT);
  for i := 0 to THREAD_COUNT - 1 do resolvers[i] := StartPlainSingletonResolver(i);
  for i := 0 to THREAD_COUNT - 1 do
  begin
    resolvers[i].WaitFor;
    resolvers[i].Free;
  end;
  // the singleton instance belongs to the container (TIocRegistrator.Destroy frees it)
  for i := 0 to THREAD_COUNT - 1 do
    Assert.AreEqual('', FErrors[i], 'Thread ' + IntToStr(i) + ' failed');
  Assert.AreEqual(1, TSlowPlainSingleton.Created, 'Concurrent first resolutions of a class registration must create it once');
  for i := 1 to THREAD_COUNT - 1 do
    Assert.IsTrue(FInstances[i] = FInstances[0], 'All threads must get the same instance');
end;

procedure TQuickIOCTests.Test_Singleton_CreationFails_NextResolveRetries;
var
  done: TLightweightEvent;
  resolver: TThread;
  fromThread: TObject;
  error: string;
  signaled: Boolean;
begin
  FContainer.RegisterInstance<TFlakySingleton>.AsSingleton;
  TFlakySingleton.Created := 0;
  TFlakySingleton.FailNext := True;
  Assert.WillRaise(
    procedure
    begin
      FContainer.Resolve<TFlakySingleton>;
    end, EFlakyCreation, 'The constructor failure must reach the caller');
  TFlakySingleton.FailNext := False;
  fromThread := nil;
  error := '';
  done := TLightweightEvent.Create;
  // from another thread: a singleton lock left held by the failure would block it
  resolver := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        fromThread := FContainer.Resolve<TFlakySingleton>;
      except
        on E: Exception do error := E.ClassName + ': ' + E.Message;
      end;
      done.SetEvent;
    end);
  resolver.FreeOnTerminate := False;
  resolver.Start;
  signaled := done.WaitFor(5000) = wrSignaled;
  if signaled then
  begin
    resolver.WaitFor;
    resolver.Free;
    done.Free;
  end; // not signaled: the thread is blocked on the lock and is left with the event
  // the singleton instance belongs to the container (TIocRegistrator.Destroy frees it)
  Assert.IsTrue(signaled, 'After a failed creation the singleton lock must be released: another thread could not resolve');
  Assert.AreEqual('', error, 'The retry failed');
  Assert.IsTrue(FContainer.Resolve<TFlakySingleton> = fromThread, 'The retry must create the singleton and keep it');
  Assert.AreEqual(1, TFlakySingleton.Created, 'One successful creation');
end;

procedure TQuickIOCTests.Test_SimpleFactory_InjectedInSingleton_BoundToRoot;
var
  scope: TIocScope;
  holder: IFactoryHolder;
begin
  // a singleton is built without a scope, so a factory it receives is bound to the root,
  // even when the singleton is first resolved inside a scope
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterSimpleFactory<IUserService, TUserService>;
  FContainer.RegisterType<IFactoryHolder, TFactoryHolder>.AsSingleton;
  scope := FContainer.CreateScope;
  try
    holder := scope.Resolve<IFactoryHolder>;
  finally
    scope.Free;
  end;
  Assert.WillNotRaise(
    procedure
    begin
      holder.Factory.New;
    end, nil, 'A factory injected into a singleton must keep working after the scope that first resolved it is freed');
end;

procedure TQuickIOCTests.Test_DelegateTo_Context_SingletonScopeIsNil;
var
  scope: TIocScope;
  seen: TIocScope;
  user: IUserService;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IUserService, TUserService>.AsSingleton.DelegateTo(
    function(const aContext: TIocResolveContext): TUserService
    begin
      seen := aContext.Scope;
      Result := TUserService.Create(aContext.Resolve<ILogger>);
    end);
  seen := nil;
  scope := FContainer.CreateScope;
  try
    user := scope.Resolve<IUserService>;
    Assert.IsNotNull(user, 'The delegate must build the singleton');
    Assert.IsTrue(seen = nil, 'A singleton is built without a scope: the context scope must be nil');
  finally
    user := nil;
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_SimpleFactory_AsSingleton_SharesFactoryBoundToRoot;
var
  scope: TIocScope;
  factory1, factory2: IFactory<IUserService>;
begin
  // RegisterSimpleFactory(...).AsSingleton gives back the original behavior: one factory, no scope
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterSimpleFactory<IUserService, TUserService>.AsSingleton;
  scope := FContainer.CreateScope;
  try
    factory1 := scope.Resolve<IFactory<IUserService>>;
  finally
    scope.Free;
  end;
  factory2 := FContainer.Resolve<IFactory<IUserService>>;
  Assert.AreSame(factory1, factory2, 'AsSingleton: one factory for the whole container');
  Assert.WillNotRaise(
    procedure
    begin
      factory1.New;
    end, nil, 'The singleton factory is bound to the root: it must outlive the scope');
end;

procedure TQuickIOCTests.Test_DelegateTo_LastOneWins;
var
  used: string;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IUserService, TUserService>('contextThenPlain').AsTransient
    .DelegateTo(
      function(const aContext: TIocResolveContext): TUserService
      begin
        used := 'context';
        Result := TUserService.Create(aContext.Resolve<ILogger>);
      end)
    .DelegateTo(
      function: TUserService
      begin
        used := 'plain';
        Result := TUserService.Create(nil);
      end);
  FContainer.RegisterType<IUserService, TUserService>('plainThenContext').AsTransient
    .DelegateTo(
      function: TUserService
      begin
        used := 'plain';
        Result := TUserService.Create(nil);
      end)
    .DelegateTo(
      function(const aContext: TIocResolveContext): TUserService
      begin
        used := 'context';
        Result := TUserService.Create(aContext.Resolve<ILogger>);
      end);
  used := '';
  FContainer.Resolve<IUserService>('contextThenPlain');
  Assert.AreEqual('plain', used, 'The last DelegateTo must win: plain after context');
  used := '';
  FContainer.Resolve<IUserService>('plainThenContext');
  Assert.AreEqual('context', used, 'The last DelegateTo must win: context after plain');
end;

procedure TQuickIOCTests.Test_Scope_AbstractFactory_WithClass_CreatesThatClass;
var
  scope: TIocScope;
  user: TUserService;
  logger: ILogger;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsScoped;
  scope := FContainer.CreateScope;
  try
    user := scope.AbstractFactory<TUserService>(TUserServiceChild);
    try
      logger := scope.Resolve<ILogger>;
      Assert.IsTrue(user is TUserServiceChild, 'AbstractFactory(aClass) must create aClass. Got ' + user.ClassName);
      Assert.AreSame(logger, user.FLogger, 'AbstractFactory(aClass) of a scope uses that scope');
    finally
      user.Free;
    end;
  finally
    logger := nil;
    scope.Free;
  end;
end;

procedure TQuickIOCTests.Test_DiagnoseConstructors_ReportsUnsatisfiableOwnConstructors;
var
  problems: TArray<string>;
begin
  FContainer.RegisterType<IInjService, TOwnUnsatisfiable>.AsTransient;
  problems := FContainer.DiagnoseConstructors;
  Assert.AreEqual(1, Integer(Length(problems)), 'One problem expected. Found: ' + string.Join(' | ', problems));
  Assert.IsTrue(Pos('TOwnUnsatisfiable declares constructors but none is satisfiable; inherited TParentWithInit.Create()', problems[0]) > 0,
    'The diagnostics must report that the inherited constructor would be used. Found: ' + problems[0]);
end;

procedure TQuickIOCTests.Test_DefaultRule_InheritedParameterlessConstructorRuns;
var
  svc: IInjService;
begin
  // TParentWithInit.Create and TObject.Create both have no parameters: the nearest one must run
  FContainer.RegisterType<IInjService, TChildOfInit>.AsTransient;
  svc := FContainer.Resolve<IInjService>;
  Assert.IsTrue(TChildOfInit(svc as TObject).FInitialized,
    'The inherited TParentWithInit.Create must run, not TObject.Create');
end;

procedure TQuickIOCTests.Test_ResolveAll_WithName_ReturnsOnlyThatName;
var
  loggers: TList<ILogger>;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<ILogger, TConsoleLogger>('files').AsTransient;
  FContainer.RegisterType<ILogger, TFileLogger>('files').AsTransient;
  loggers := FContainer.ResolveAll<ILogger>('files');
  try
    Assert.AreEqual<Integer>(2, loggers.Count, 'Only the registrations with that name');
    Assert.IsTrue((loggers[0] as TObject) is TConsoleLogger, 'First named registration: TConsoleLogger');
    Assert.IsTrue((loggers[1] as TObject) is TFileLogger, 'Second named registration: TFileLogger');
  finally
    loggers.Free;
  end;
end;

procedure TQuickIOCTests.Test_ResolveAll_ClassType_KeepsEachLifetime;
var
  first, second: TList<TPlainThing>;
  objects: TArray<Pointer>;
  thing: TPlainThing;
begin
  FContainer.RegisterInstance<TPlainThing>.AsSingleton;
  FContainer.RegisterInstance<TPlainThing>.AsTransient;
  first := FContainer.ResolveAll<TPlainThing>;
  second := FContainer.ResolveAll<TPlainThing>;
  try
    Assert.AreEqual<Integer>(2, first.Count, 'One object per class registration');
    Assert.IsTrue(first[0] = second[0], 'Singleton class registration: the same object every time');
    Assert.IsTrue(first[1] <> second[1], 'Transient class registration: a new object every time');
  finally
    // transient class instances belong to the caller; the singleton one to the container
    objects := nil;
    if first.Count > 1 then objects := objects + [Pointer(first[1])];
    if second.Count > 1 then objects := objects + [Pointer(second[1])];
    FreeDistinct(objects);
    first.Free;
    second.Free;
  end;
end;

{ Pending items of the first review }

procedure TQuickIOCTests.Test_Owned_RemoveRegistrations_RemovesItsOwned;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<ILogger, TFileLogger>.AsTransient;
  FContainer.Registrator.RemoveRegistrations(FContainer.Registrator.GetKey(TypeInfo(ILogger)));
  Assert.IsFalse(FContainer.IsRegistered<IOwned<ILogger>>(''),
    'Removing the registrations of ILogger must remove their IOwned<ILogger> too');
end;

procedure TQuickIOCTests.Test_Owned_RemoveAndRegisterAgain_OneOwnedPerRegistration;
var
  owned: TList<IOwned<ILogger>>;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.Registrator.RemoveRegistrations(FContainer.Registrator.GetKey(TypeInfo(ILogger)));
  FContainer.RegisterType<ILogger, TFileLogger>.AsTransient;
  owned := FContainer.ResolveAll<IOwned<ILogger>>();
  try
    Assert.AreEqual<Integer>(1, owned.Count, 'Only the IOwned of the current registration');
    Assert.IsTrue((owned[0].Value as TObject) is TFileLogger, 'It must wrap the current registration');
  finally
    owned.Free;
  end;
end;

procedure TQuickIOCTests.Test_Exceptions_ShareEIocErrorBase;
begin
  Assert.IsTrue(EIocRegisterError.InheritsFrom(EIocError), 'EIocRegisterError must descend from EIocError');
  Assert.IsTrue(EIocResolverError.InheritsFrom(EIocError), 'EIocResolverError must descend from EIocError');
  Assert.IsTrue(EIocBuildError.InheritsFrom(EIocError), 'EIocBuildError must descend from EIocError');
  Assert.IsTrue(EIocScopeError.InheritsFrom(EIocError), 'EIocScopeError must descend from EIocError');
  Assert.IsTrue(EIocCycleError.InheritsFrom(EIocError), 'EIocCycleError must descend from EIocError');
  Assert.IsTrue(EIocInjectError.InheritsFrom(EIocResolverError), 'EIocInjectError must stay an EIocResolverError');
  // constructor selection swallows EIocResolverError to try another constructor: these must not be one
  Assert.IsFalse(EIocScopeError.InheritsFrom(EIocResolverError), 'EIocScopeError must stay out of EIocResolverError');
  Assert.IsFalse(EIocCycleError.InheritsFrom(EIocResolverError), 'EIocCycleError must stay out of EIocResolverError');
end;

function TQuickIOCTests.ResolveCycleInThread(aInScope: Boolean): string;
var
  error: string;
  worker: TThread;
begin
  // in a thread of its own: without cycle detection the recursion ends in a stack overflow,
  // which must not take the whole test run down
  error := '';
  worker := TThread.CreateAnonymousThread(
    procedure
    var
      scope: TIocScope;
    begin
      try
        if aInScope then
        begin
          scope := FContainer.CreateScope;
          try
            scope.Resolve<ICycleA>;
          finally
            scope.Free;
          end;
        end
        else FContainer.Resolve<ICycleA>;
      except
        on E: Exception do error := E.ClassName + ': ' + E.Message;
      end;
    end);
  worker.FreeOnTerminate := False;
  worker.Start;
  worker.WaitFor;
  worker.Free;
  Result := error;
end;

procedure TQuickIOCTests.Test_Cycle_Transient_RaisesCycleError;
var
  error: string;
begin
  FContainer.RegisterType<ICycleA, TCycleA>.AsTransient;
  FContainer.RegisterType<ICycleB, TCycleB>.AsTransient;
  error := ResolveCycleInThread(False);
  Assert.IsTrue(error.StartsWith('EIocCycleError:'), 'A dependency cycle must raise EIocCycleError. Got: ' + error);
  Assert.IsTrue(Pos('TCycleA -> TCycleB -> TCycleA', error) > 0, 'The message must show the cycle. Got: ' + error);
end;

procedure TQuickIOCTests.Test_Cycle_Scoped_RaisesCycleError;
var
  error: string;
begin
  FContainer.RegisterType<ICycleA, TCycleA>.AsScoped;
  FContainer.RegisterType<ICycleB, TCycleB>.AsScoped;
  error := ResolveCycleInThread(True);
  Assert.IsTrue(error.StartsWith('EIocCycleError:'), 'A dependency cycle between scoped services must raise EIocCycleError. Got: ' + error);
  Assert.IsTrue(Pos('TCycleA -> TCycleB -> TCycleA', error) > 0, 'The message must show the cycle. Got: ' + error);
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

procedure TQuickIOCTests.Test_Container_Free_ReleasesSingletonsBeforeResolver;
var
  container: TIocContainer;
  singleton: IUsesFactoryOnDestroy;
begin
  // a container of its own, freed here: the singleton released by its destructor uses a
  // factory, i.e. the resolver, in its own destructor
  container := TIocContainer.Create;
  try
    container.RegisterType<ILogger, TConsoleLogger>.AsTransient;
    container.RegisterSimpleFactory<IUserService, TUserService>;
    container.RegisterType<IUsesFactoryOnDestroy, TUsesFactoryOnDestroy>.AsSingleton;
    singleton := container.Resolve<IUsesFactoryOnDestroy>;
    singleton := nil;
    TUsesFactoryOnDestroy.Outcome := '';
  finally
    container.Free;
  end;
  Assert.AreEqual('ok', TUsesFactoryOnDestroy.Outcome,
    'A singleton released by the container must still be able to use it in its destructor');
end;

{ Robust destruction (package A) }

procedure TQuickIOCTests.Test_Container_Free_ReleasesSingletonsInReverseCreationOrder;
var
  container: TIocContainer;
begin
  container := TIocContainer.Create;
  try
    container.RegisterInstance<TReleaseLoggedA>.AsSingleton; // registered first...
    container.RegisterInstance<TReleaseLoggedB>.AsSingleton; // ...but B is created first, as A's dependency
    container.Resolve<TReleaseLoggedA>;
    TReleaseLogged.Released := '';
  finally
    container.Free;
  end;
  Assert.AreEqual('TReleaseLoggedA;TReleaseLoggedB;', TReleaseLogged.Released,
    'A singleton must be released before the singletons it received in its constructor');
end;

procedure TQuickIOCTests.Test_Container_Free_ReleasesAllEvenIfOneDestructorRaises;
var
  container: TIocContainer;
  logger: ILogger;
  exploding: IExploding;
  raised: string;
begin
  TTrackedLogger.Destroyed := 0;
  raised := '';
  container := TIocContainer.Create;
  try
    try
      container.RegisterType<ILogger, TTrackedLogger>.AsSingleton;
      container.RegisterType<IExploding, TExplodingOnDestroy>.AsSingleton;
      // a second TTrackedLogger, released only when the container frees its registrations
      HoldInDelegate(container);
      logger := container.Resolve<ILogger>;          // created first, released last
      exploding := container.Resolve<IExploding>;    // released first: its destructor raises
      logger := nil;
      exploding := nil;
    finally
      container.Free;
    end;
  except
    on E: Exception do raised := E.ClassName;
  end;
  Assert.AreEqual(2, TTrackedLogger.Destroyed,
    'The other singletons and the registrations must be released even if a destructor raises');
  Assert.AreEqual('EExplodingDestroy', raised, 'The destructor failure must reach the caller');
end;

procedure TQuickIOCTests.Test_Container_Free_ReleasedSingletonIsNotBuiltAgain;
var
  container: TIocContainer;
begin
  TFlakySingleton.FailNext := False;
  TFlakySingleton.Created := 0;
  TRunsOnDestroy.Outcome := '';
  container := TIocContainer.Create;
  try
    container.RegisterInstance<TRunsOnDestroy>.AsSingleton;
    container.RegisterInstance<TFlakySingleton>.AsSingleton;
    container.Resolve<TRunsOnDestroy>;
    container.Resolve<TFlakySingleton>; // created last, released first
    // the destructor of TRunsOnDestroy looks up TFlakySingleton, already released by then
    TRunsOnDestroy.OnDestroy :=
      procedure
      begin
        container.Resolve<TFlakySingleton>;
      end;
  finally
    container.Free;
    TRunsOnDestroy.OnDestroy := nil;
  end;
  Assert.AreEqual(1, TFlakySingleton.Created, 'A singleton already released must not be built again');
  Assert.IsTrue(TRunsOnDestroy.Outcome.StartsWith('EIocScopeError:'),
    'Resolving a released singleton while the container is freed must raise EIocScopeError. Got: ' + TRunsOnDestroy.Outcome);
end;

procedure TQuickIOCTests.Test_Container_Free_SingletonBeingFreedIsNotResolved;
var
  container: TIocContainer;
begin
  TRunsOnDestroy.Outcome := '';
  container := TIocContainer.Create;
  try
    container.RegisterInstance<TRunsOnDestroy>.AsSingleton;
    container.Resolve<TRunsOnDestroy>;
    // the destructor resolves the very singleton being freed
    TRunsOnDestroy.OnDestroy :=
      procedure
      begin
        container.Resolve<TRunsOnDestroy>;
      end;
  finally
    container.Free;
    TRunsOnDestroy.OnDestroy := nil;
  end;
  Assert.IsTrue(TRunsOnDestroy.Outcome.StartsWith('EIocScopeError:'),
    'The singleton being freed must not be handed out again. Got: ' + TRunsOnDestroy.Outcome);
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

procedure TQuickIOCTests.Test_Scope_Destroy_SafeWhenCreateDidNotFinish;
begin
  // when a constructor raises, Delphi calls the destructor on the partly built object: a fresh
  // instance, with every field still nil, is a scope whose Create failed at the first allocation
  Assert.WillNotRaise(
    procedure
    begin
      TIocScope(TIocScope.NewInstance).Destroy;
    end, nil, 'TIocScope.Destroy must not fail on a scope whose constructor did not finish');
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
  FContainer.RegisterType<IInjService, TInjectFailsAfterExploding>.AsTransient;
  raised := '';
  try
    // IExploding is created in the IOwned scope, then the [Inject] constructor fails; releasing
    // that scope raises EExplodingDestroy
    FContainer.Resolve<IOwned<IInjService>>;
  except
    on E: Exception do raised := E.ClassName;
  end;
  Assert.AreEqual('EIocInjectError', raised,
    'The resolution failure must reach the caller, not the exception raised while releasing the IOwned scope');
end;

{ Diagnostics and messages (package B) }

procedure TQuickIOCTests.Test_DiagnoseConstructors_ReportsClassNotImplementingInterface;
var
  problems: TArray<string>;
begin
  FContainer.RegisterType<ILogger, TNotALogger>;
  problems := FContainer.DiagnoseConstructors;
  Assert.AreEqual(1, Integer(Length(problems)), 'One problem expected. Found: ' + string.Join(' | ', problems));
  Assert.IsTrue(Pos('TNotALogger is registered for ILogger but does not implement it', problems[0]) > 0,
    'The diagnostics must report the class that does not implement its interface. Found: ' + problems[0]);
end;

procedure TQuickIOCTests.Test_DiagnoseConstructors_ReportsInterfaceWithoutGuid;
var
  problems: TArray<string>;
begin
  FContainer.RegisterType<INoGuid, TNoGuidService>;
  problems := FContainer.DiagnoseConstructors;
  Assert.AreEqual(1, Integer(Length(problems)), 'One problem expected. Found: ' + string.Join(' | ', problems));
  Assert.IsTrue(Pos('INoGuid has no GUID', problems[0]) > 0,
    'The diagnostics must report the interface without a GUID. Found: ' + problems[0]);
end;

procedure TQuickIOCTests.Test_Resolve_ClassNotImplementingInterface_RaisesRegisterError;
var
  error: string;
begin
  // TUserService needs ILogger, registered with a class that does not implement it
  FContainer.RegisterType<ILogger, TNotALogger>;
  FContainer.RegisterType<IUserService, TUserService>;
  error := '';
  try
    FContainer.Resolve<IUserService>;
  except
    on E: Exception do error := E.ClassName + ': ' + E.Message;
  end;
  Assert.IsTrue(error.StartsWith('EIocRegisterError:'),
    'The wrong registration must surface, not build TUserService with a nil logger. Got: ' + error);
  Assert.IsTrue(Pos('TNotALogger does not implement ILogger', error) > 0, 'The message must name both. Got: ' + error);
end;

procedure TQuickIOCTests.Test_Build_ContainerError_KeepsItsClass;
var
  raisedClass, msg: string;
begin
  FContainer.RegisterType<ICycleA, TCycleA>.AsSingleton;
  FContainer.RegisterType<ICycleB, TCycleB>.AsSingleton;
  raisedClass := '';
  msg := '';
  try
    FContainer.Build;
  except
    on E: Exception do
    begin
      raisedClass := E.ClassName;
      msg := E.Message;
    end;
  end;
  Assert.AreEqual('EIocCycleError', raisedClass, 'Build must keep the class of the container error. Message: ' + msg);
  Assert.IsTrue(Pos('Build Error on "TCycleA', msg) > 0, 'The message must name the registration. Message: ' + msg);
  Assert.IsTrue(Pos('TCycleA -> TCycleB -> TCycleA', msg) > 0, 'The message must keep the original one. Message: ' + msg);
end;

procedure TQuickIOCTests.Test_Build_ConstructorFailure_KeepsOriginalAsInner;
var
  raisedClass, innerClass: string;
begin
  FContainer.RegisterInstance<TFlakySingleton>.AsSingleton;
  TFlakySingleton.FailNext := True;
  raisedClass := '';
  innerClass := '';
  try
    try
      FContainer.Build;
    except
      on E: Exception do
      begin
        raisedClass := E.ClassName;
        if E.InnerException <> nil then innerClass := E.InnerException.ClassName;
      end;
    end;
  finally
    TFlakySingleton.FailNext := False;
  end;
  Assert.AreEqual('EIocBuildError', raisedClass, 'An exception of a constructor is reported as EIocBuildError');
  Assert.AreEqual('EFlakyCreation', innerClass, 'The original exception must be kept in InnerException');
end;

procedure TQuickIOCTests.Test_DiagnoseConstructors_ChecksSingletonAlreadyBuilt;
var
  problems: TArray<string>;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IInjService, TNoInjDerived>.AsSingleton;
  FContainer.Resolve<IInjService>; // built before the diagnostics run
  problems := FContainer.DiagnoseConstructors;
  Assert.AreEqual(1, Integer(Length(problems)),
    'A singleton already built must still be checked. Found: ' + string.Join(' | ', problems));
  Assert.IsTrue(Pos('TNoInjDerived would be created by TObject.Create', problems[0]) > 0, 'Found: ' + problems[0]);
end;

procedure TQuickIOCTests.Test_DiagnoseConstructors_WarnsOnTiedConstructors;
var
  problems, warnings: TArray<string>;
begin
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsTransient;
  FContainer.RegisterType<IEmailService, TEmailService>.AsTransient;
  FContainer.RegisterType<IInjService, TTiedConstructors>.AsTransient;
  problems := FContainer.DiagnoseConstructors(warnings);
  Assert.AreEqual(0, Integer(Length(problems)), 'Not an error. Found: ' + string.Join(' | ', problems));
  Assert.AreEqual(1, Integer(Length(warnings)),
    'Two satisfiable constructors with as many parameters: one warning expected. Found: ' + string.Join(' | ', warnings));
  Assert.IsTrue((Pos('TTiedConstructors.Create(logger: ILogger)', warnings[0]) > 0) and
    (Pos('TTiedConstructors.Create(email: IEmailService)', warnings[0]) > 0),
    'The warning must name both constructors. Warning: ' + warnings[0]);
end;

procedure TQuickIOCTests.Test_Resolve_UntypedConstructorParameter_NotUsed;
begin
  FContainer.RegisterType<IInjService, TUntypedParam>.AsTransient;
  Assert.WillNotRaise(
    procedure
    begin
      FContainer.Resolve<IInjService>;
    end, nil, 'A constructor with an untyped parameter cannot be satisfied: it must be skipped, not crash');
end;

procedure TQuickIOCTests.Test_DelegateTo_NamedFunction_Compiles;
var
  logger: ILogger;
begin
  // compile-time check: with two DelegateTo overloads, a named function still picks the plain one.
  // DelegateTo(nil) does not compile (E2251, ambiguous), which is documented
  FContainer.RegisterType<ILogger, TConsoleLogger>.DelegateTo(NewConsoleLogger);
  logger := FContainer.Resolve<ILogger>;
  Assert.IsTrue((logger as TObject) is TConsoleLogger, 'The named function must be used as the delegate');
end;

procedure TQuickIOCTests.Test_Build_MockOnTop_OriginalNotBuilt;
var
  email: IEmailService;
begin
  // the production registration (ISmtpSettings is not registered in a test project) and the
  // test's mock on top of it, without removing anything
  FContainer.RegisterType<IEmailService, TSmtpEmailService>.AsSingleton;
  FContainer.RegisterType<IEmailService, TFakeEmailService>.AsSingleton;
  TSmtpEmailService.Created := 0;
  TFakeEmailService.Created := 0;
  Assert.WillNotRaise(
    procedure
    begin
      FContainer.Build;
    end, nil, 'Build must not build a registration that Resolve never returns');
  Assert.AreEqual(0, TSmtpEmailService.Created, 'The overridden production service must not be built');
  Assert.AreEqual(1, TFakeEmailService.Created, 'Build pre-creates the singleton Resolve returns');
  email := FContainer.Resolve<IEmailService>;
  Assert.IsTrue((email as TObject) is TFakeEmailService, 'Resolve returns the mock');
end;

procedure TQuickIOCTests.Test_Build_ValidateConstructors_OverriddenRegistrationOnlyWarns;
begin
  FContainer.RegisterType<IEmailService, TSmtpEmailService>.AsSingleton;
  FContainer.RegisterType<IEmailService, TFakeEmailService>.AsSingleton;
  FContainer.ValidateConstructors := True;
  Assert.WillNotRaise(
    procedure
    begin
      FContainer.Build;
    end, nil, 'The unregistered dependency of an overridden registration must not make Build fail');
  Assert.AreEqual(1, Integer(Length(FContainer.ConstructorWarnings)),
    'It is still reported, as a warning. Found: ' + string.Join(' | ', FContainer.ConstructorWarnings));
  Assert.IsTrue(Pos('TSmtpEmailService', FContainer.ConstructorWarnings[0]) > 0,
    'The warning must name the overridden class. Found: ' + FContainer.ConstructorWarnings[0]);
end;

{ IOwned configurable (package G) }

procedure TQuickIOCTests.Test_Owned_AutoRegisterOff_NotRegistered;
begin
  FContainer.AutoRegisterOwned := False;
  FContainer.RegisterType<ILogger, TConsoleLogger>;
  Assert.IsFalse(FContainer.IsRegistered<IOwned<ILogger>>(''),
    'With AutoRegisterOwned off, RegisterType<I,T> must not register IOwned<I>');
end;

procedure TQuickIOCTests.Test_Owned_RegisterOwned_CompletesAndKeepsOrder;
var
  logger: ILogger;
  owned: TList<IOwned<ILogger>>;
  last: IOwned<ILogger>;
begin
  logger := TFileLogger.Create('given.log');
  FContainer.AutoRegisterOwned := False;
  FContainer.RegisterInstance<ILogger>(logger);      // no IOwned of its own
  FContainer.AutoRegisterOwned := True;
  FContainer.RegisterType<ILogger, TConsoleLogger>;  // IOwned registered automatically; Resolve<ILogger> returns it
  FContainer.RegisterOwned<ILogger>;                 // adds only the missing IOwned
  owned := FContainer.ResolveAll<IOwned<ILogger>>;
  try
    Assert.AreEqual<Integer>(2, owned.Count, 'One IOwned per registration, the automatic one not duplicated');
    Assert.IsTrue((owned[0].Value as TObject) = (logger as TObject),
      'The IOwned keep the order of their targets: the first wraps the given instance');
    Assert.IsTrue((owned[1].Value as TObject) is TConsoleLogger, 'The second wraps TConsoleLogger');
  finally
    owned.Free;
  end;
  last := FContainer.Resolve<IOwned<ILogger>>;
  Assert.IsTrue((last.Value as TObject) is TConsoleLogger, 'Resolve<IOwned<I>> must wrap what Resolve<I> returns');
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
  FContainer.RegisterType<IInjService, TOwnedConsumer>;
  error := '';
  try
    FContainer.Resolve<IInjService>;
  except
    on E: Exception do error := E.ClassName + ': ' + E.Message;
  end;
  Assert.IsTrue(error.StartsWith('EIocRegisterError:'),
    'Asking for an unregistered IOwned must raise, not fall back to TObject.Create. Got: ' + error);
  Assert.IsTrue((Pos('TOwnedConsumer.Create asks for IOwned<', error) > 0) and (Pos('RegisterOwned<', error) > 0),
    'The message must name the constructor and the fix. Got: ' + error);
end;

procedure TQuickIOCTests.Test_DiagnoseConstructors_ReportsOwnedNotRegistered;
var
  problems: TArray<string>;
begin
  // the default rule picks the empty Create in silence; the one asking for IOwned is the intended one
  FContainer.RegisterType(TypeInfo(ILogger), TConsoleLogger);
  FContainer.RegisterType<IInjService, TOwnedOrEmpty>;
  problems := FContainer.DiagnoseConstructors;
  Assert.AreEqual(1, Integer(Length(problems)), 'One problem expected. Found: ' + string.Join(' | ', problems));
  Assert.IsTrue((Pos('TOwnedOrEmpty.Create(owned: IOwned<', problems[0]) > 0) and (Pos('asks for IOwned<', problems[0]) > 0),
    'The diagnostics must report the unregistered IOwned, whichever constructor is chosen. Found: ' + problems[0]);
end;

{ API polish (package C) }

procedure TQuickIOCTests.Test_RegisterOptions_WrongClass_RaisesInvalidCast;
var
  other: TOptions;
  raised: Boolean;
begin
  other := TOtherSettings.Create;
  raised := False;
  try
    FContainer.RegisterOptions<TAppSettings>(other);
  except
    on EInvalidCast do raised := True;
  end;
  // refused: not registered, so it is still ours to free
  if raised then other.Free;
  Assert.IsTrue(raised, 'Options of another class must be refused with EInvalidCast, not registered as TAppSettings');
end;

procedure TQuickIOCTests.Test_ResolveContext_NotFromContainer_RaisesIocError;
var
  context: TIocResolveContext;
begin
  // a context the container did not create, such as a record field never assigned
  context := Default(TIocResolveContext);
  Assert.WillRaise(
    procedure
    begin
      context.Resolve<ILogger>;
    end, EIocError, 'A TIocResolveContext not created by the container must raise EIocError, not an access violation');
end;

{ Singleton lock and re-entry (fourth review) }

procedure TQuickIOCTests.Test_Singleton_ConstructorWaitingForAnotherBeingCreated_DoesNotDeadlock;
begin
  // the constructor of a singleton waits for a thread that resolves another singleton, not created
  // yet. With one lock for every singleton, the thread waits for the lock held during the
  // constructor and the constructor waits for the thread: a deadlock, cut here by a 5 s timeout
  FContainer.RegisterType<ILogger, TConsoleLogger>.AsSingleton;
  FContainer.RegisterType<IWaitsForAnother, TWaitsForAnother>.AsSingleton;
  TWaitsForAnother.Container := FContainer;
  TWaitsForAnother.Resolved := TLightweightEvent.Create;
  TWaitsForAnother.Worker := nil;
  TWaitsForAnother.WorkerError := '';
  TWaitsForAnother.WaitResult := wrError;
  try
    FContainer.Resolve<IWaitsForAnother>;
    TWaitsForAnother.Worker.WaitFor;
    Assert.AreEqual('', TWaitsForAnother.WorkerError, 'The thread failed to resolve ILogger');
    Assert.IsTrue(TWaitsForAnother.WaitResult = wrSignaled,
      'Creating a singleton must not wait for the creation of an unrelated one');
  finally
    TWaitsForAnother.Worker.Free;
    TWaitsForAnother.Worker := nil;
    TWaitsForAnother.Resolved.Free;
    TWaitsForAnother.Resolved := nil;
    TWaitsForAnother.Container := nil;
  end;
end;

procedure TQuickIOCTests.Test_Cycle_ReentryInSameThread_MessageExplainsIt;
var
  error: string;
begin
  // no dependency cycle: while TReentrant is being built, code run by its constructor resolves
  // IReentrant again in the same thread, as a handler run by Application.ProcessMessages would.
  // The container cannot tell this from a cycle, so the message must explain both. The handler
  // catches the exception, as the VCL does with an exception raised in a message handler
  FContainer.RegisterType<IReentrant, TReentrant>.AsTransient;
  error := '';
  TReentrant.OnCreate :=
    procedure
    begin
      try
        FContainer.Resolve<IReentrant>;
      except
        on E: Exception do error := E.ClassName + ': ' + E.Message;
      end;
    end;
  try
    FContainer.Resolve<IReentrant>;
  finally
    TReentrant.OnCreate := nil;
  end;
  Assert.IsTrue(error.StartsWith('EIocCycleError:'), 'The re-entry must raise EIocCycleError. Got: ' + error);
  Assert.IsTrue(Pos('re-entered in this thread while building TReentrant', error) > 0,
    'The message must explain the re-entry. Got: ' + error);
end;

// bytes allocated by the default memory manager
function AllocatedBytes: UInt64;
var
  state: TMemoryManagerState;
  i: Integer;
begin
  GetMemoryManagerState(state);
  Result := state.TotalAllocatedMediumBlockSize + state.TotalAllocatedLargeBlockSize;
  for i := Low(state.SmallBlockTypeStates) to High(state.SmallBlockTypeStates) do
    Inc(Result, UInt64(state.SmallBlockTypeStates[i].UseableBlockSize) * state.SmallBlockTypeStates[i].AllocatedBlockCount);
end;

procedure TQuickIOCTests.Test_Singleton_ClassDelegateReturnsNil_ResolvingKeepsNoMemory;
const
  RESOLUTIONS = 10000;
var
  calls: Integer;
  before: UInt64;
  kept: Int64;
  i: Integer;
begin
  // a class singleton whose delegate returns nil is never created: each resolution calls the
  // delegate again, as before the fork. Each call used to record the registration for release
  // again, so the list of created singletons grew while the program ran (a pointer per resolution)
  calls := 0;
  FContainer.RegisterInstance<TPlainThing>.DelegateTo(
    function: TPlainThing
    begin
      Inc(calls);
      Result := nil;
    end).AsSingleton;
  // first resolution outside the measure: it allocates what is allocated once
  FContainer.Resolve<TPlainThing>;
  before := AllocatedBytes;
  for i := 1 to RESOLUTIONS do FContainer.Resolve<TPlainThing>;
  kept := Int64(AllocatedBytes) - Int64(before);
  Assert.AreEqual(RESOLUTIONS + 1, calls, 'Each resolution calls the delegate again');
  Assert.IsTrue(kept < 16 * 1024,
    Format('%d resolutions must not keep memory; kept %d bytes', [RESOLUTIONS, kept]));
end;

procedure TQuickIOCTests.Test_Owned_GivenInstanceOnTop_OwnedWrapsIt;
var
  mock: ILogger;
  consumer: IInjService;
begin
  // a mock given with RegisterInstance<I> on top of the production registration: Resolve<I>
  // returns the mock, and IOwned<I> must wrap it too. It wrapped the production class, because
  // only RegisterType<I,T> registered IOwned<I>
  FContainer.RegisterType<ILogger, TConsoleLogger>;
  mock := TFileLogger.Create('mock');
  FContainer.RegisterInstance<ILogger>(mock);
  FContainer.RegisterType<IInjService, TOwnedConsumer>;
  Assert.IsTrue(FContainer.Resolve<ILogger> = mock, 'Resolve<ILogger> must return the given instance');
  consumer := FContainer.Resolve<IInjService>;
  Assert.IsTrue(consumer.Logger = mock, 'IOwned<ILogger> must wrap what Resolve<ILogger> returns: the given instance');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickIOCTests);
end.
