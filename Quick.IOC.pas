{ ***************************************************************************

  Copyright (c) 2016-2026 Kike Perez

  Unit        : Quick.IoC
  Description : IoC Dependency Injector
  Author      : Kike Perez
  Version     : 1.0
  Created     : 19/10/2019
  Modified    : 08/05/2026

  This file is part of QuickLib: https://github.com/exilon/QuickLib

 ***************************************************************************

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

 *************************************************************************** }

unit Quick.IoC;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  RTTI,
  {$IFDEF DEBUG_IOC}
    Quick.Debug.Utils,
  {$ENDIF}
  System.TypInfo,
  System.Generics.Collections,
  System.Generics.Defaults,
  Quick.Logger.Intf,
  Quick.Options;

type
  TActivatorDelegate<T> = reference to function: T;

  TIocRegistration = class
  type
    TRegisterMode = (rmTransient, rmSingleton, rmScoped);
  private
    fName : string;
    fRegisterMode : TRegisterMode;
    fIntfInfo : PTypeInfo;
    fImplementation : TClass;
    fActivatorDelegate : TActivatorDelegate<TValue>;
  public
    constructor Create(const aName : string);
    property Name : string read fName;
    property IntfInfo : PTypeInfo read fIntfInfo write fIntfInfo;
    property &Implementation : TClass read fImplementation write fImplementation;
    function IsSingleton : Boolean;
    function IsTransient : Boolean;
    function IsScoped : Boolean;
    function AsSingleton : TIocRegistration;
    function AsTransient : TIocRegistration;
    function AsScoped : TIocRegistration;
    property ActivatorDelegate : TActivatorDelegate<TValue> read fActivatorDelegate write fActivatorDelegate;
  end;

  TIocRegistrationInterface = class(TIocRegistration)
  private
    fInstance : IInterface;
  public
    property Instance : IInterface read fInstance write fInstance;
  end;

  TIocRegistrationInstance = class(TIocRegistration)
  private
    fInstance : TObject;
  public
    property Instance : TObject read fInstance write fInstance;
  end;

  TIocRegistration<T> = record
  private
    fRegistration : TIocRegistration;
  public
    constructor Create(aRegistration : TIocRegistration);
    function AsSingleton : TIocRegistration<T>;
    function AsTransient : TIocRegistration<T>;
    function AsScoped : TIocRegistration<T>;
    function DelegateTo(aDelegate : TActivatorDelegate<T>) : TIocRegistration<T>;
  end;

  IIocRegistrator = interface
  ['{F3B79B15-2874-4B66-9B7F-06E2EBFED1AE}']
    function GetKey(aPInfo : PTypeInfo; const aName : string = ''): string;
    function RegisterType(aTypeInfo : PTypeInfo; aImplementation : TClass; const aName : string = '') : TIocRegistration;
    function RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration;
  end;

  TIocRegistrator = class(TInterfacedObject,IIocRegistrator)
  private
    fDependencies : TDictionary<string, TObjectList<TIocRegistration>>;
    fDependencyOrder : TObjectList<TIocRegistration>;
  public
    constructor Create;
    destructor Destroy; override;
    property Dependencies : TDictionary<string, TObjectList<TIocRegistration>> read fDependencies write fDependencies;
    property DependencyOrder : TObjectList<TIocRegistration> read fDependencyOrder;
    function IsRegistered<TInterface: IInterface; TImplementation: class>(const aName : string = '') : Boolean; overload;
    function IsRegistered<T>(const aName : string = '') : Boolean; overload;
    function GetKey(aPInfo : PTypeInfo; const aName : string = ''): string;
    function RegisterType<TInterface: IInterface; TImplementation: class>(const aName : string = '') : TIocRegistration<TImplementation>; overload;
    function RegisterType(aTypeInfo : PTypeInfo; aImplementation : TClass; const aName : string = '') : TIocRegistration; overload;
    function RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration; overload;
    function RegisterInstance<T : class>(const aName : string = '') : TIocRegistration<T>; overload;
    function RegisterInstance<TInterface : IInterface>(aInstance : TInterface; const aName : string = '') : TIocRegistration; overload;
    function RegisterOptions<T : TOptions>(aOptions : T) : TIocRegistration<T>;
    /// <summary>Remove all registrations for the given key. Frees existing registration objects.</summary>
    function RemoveRegistrations(const aKey: string): Boolean;
  end;

  IIocContainer = interface
  ['{6A486E3C-C5E8-4BE5-8382-7B9BCCFC1BC3}']
    function RegisterType(aInterface: PTypeInfo; aImplementation : TClass; const aName : string = '') : TIocRegistration;
    function RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration;
    function Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue;
    procedure Build;
  end;

  IIocInjector = interface
  ['{F78E6BBC-2A95-41C9-B231-D05A586B4B49}']
  end;

  TIocInjector = class(TInterfacedObject,IIocInjector)
  end;

  IIocResolver = interface
  ['{B7C07604-B862-46B2-BF33-FF941BBE53CA}']
    function Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue; overload;
  end;

  TIocScope = class;

  TIocResolver = class(TInterfacedObject,IIocResolver)
  private
    fRegistrator : TIocRegistrator;
    fInjector : TIocInjector;
    fValidateScopes : Boolean;
    function CreateInstance(aClass : TClass) : TValue; overload;
    function CreateInstance(aClass : TClass; aScope : TIocScope) : TValue; overload;
    function BuildValue(aReg : TIocRegistration; aServiceType : PTypeInfo; aScope : TIocScope) : TValue;
  public
    constructor Create(aRegistrator : TIocRegistrator; aInjector : TIocInjector);
    function Resolve<T>(const aName : string = ''): T; overload;
    function Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue; overload;
    /// <summary>Resolves within aScope. aScope = nil means the root (no scope).</summary>
    function Resolve(aServiceType: PTypeInfo; const aName : string; aScope : TIocScope): TValue; overload;
    function ResolveAll<T>(const aName : string = '') : TList<T>;
    /// <summary>True (default): resolving a scoped service outside a scope (from the root
    /// container or as a dependency of a singleton) raises EIocScopeError.
    /// False: previous behaviour, a scoped service outside a scope is built as transient.</summary>
    property ValidateScopes : Boolean read fValidateScopes write fValidateScopes;
  end;

  /// <summary>A resolution scope (e.g. one per HTTP request). Services registered AsScoped
  /// are created once per scope and released when the scope is freed: the interface instances
  /// first, then the class instances, each in reverse order of creation. If a destructor raises,
  /// the others are still released and the first exception is raised again by Free. Once Free
  /// starts, resolving from the scope raises EIocScopeError. Singletons and transients behave as
  /// usual. A scope is not thread-safe: use it from one thread at a time, and free it before the
  /// container.</summary>
  TIocScope = class
  private
    fResolver : TIocResolver;
    fReleasing : Boolean;
    fInterfaces : TDictionary<TIocRegistration, IInterface>;
    fObjects : TDictionary<TIocRegistration, TObject>;
    fCreated : TList<IInterface>;
    fCreatedObjects : TList<TObject>;
    procedure CheckAlive;
    function GetOrCreate(aReg : TIocRegistration; aServiceType : PTypeInfo) : TValue;
  public
    constructor Create(aResolver : TIocResolver);
    destructor Destroy; override;
    function Resolve<T>(const aName : string = ''): T; overload;
    function Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue; overload;
  end;

  // Non-generic helper for typed factory creation (kept for possible future use)
  TTypedFactoryHelper = class
  end;

  // Stub kept for API/return-type compatibility
  TTypedFactory<T : class, constructor> = class(TInterfacedObject)
  end;

  IFactory<T> = interface
  ['{92D7AB4F-4C0A-4069-A821-B057E193DE65}']
    function New : T;
  end;

  /// <summary>An owned instance (Autofac's Owned&lt;T&gt;): a dependency resolved in a scope of its
  /// own. Asking for IOwned&lt;T&gt; in a constructor opens that scope, resolves T in it and keeps
  /// it alive while the IOwned&lt;T&gt; is referenced: scoped services in T's dependency chain get
  /// their own instances instead of the consumer's. The scope shares only the singletons.
  /// RegisterType&lt;T,TImplementation&gt; registers IOwned&lt;T&gt; (AutoRegisterOwned, True by
  /// default); otherwise call RegisterOwned&lt;T&gt;. A constructor that asks for an IOwned&lt;T&gt;
  /// that is not registered raises EIocRegisterError.</summary>
  IOwned<T> = interface
  ['{3B0E6F52-9C1D-4A7E-8B25-D4F1A0C6E913}']
    function Value : T;
  end;

  TOwned<T> = class(TInterfacedObject,IOwned<T>)
  private
    fScope : TIocScope;
    fValue : T;
  public
    constructor Create(aScope : TIocScope; const aValue : T);
    destructor Destroy; override;
    function Value : T;
  end;

  TSimpleFactory<T : class, constructor> = class(TInterfacedObject,IFactory<T>)
  private
    fResolver : TIocResolver;
  public
    constructor Create(aResolver : TIocResolver);
    function New : T;
  end;

  TSimpleFactory<TInterface : IInterface; TImplementation : class, constructor> = class(TInterfacedObject,IFactory<TInterface>)
  private
    fResolver : TIocResolver;
  public
    constructor Create(aResolver : TIocResolver);
    function New : TInterface;
  end;


  TIocContainer = class(TInterfacedObject,IIocContainer)
  private
    fRegistrator : TIocRegistrator;
    fResolver : TIocResolver;
    fInjector : TIocInjector;
    fLogger : ILogger;
    fAutoRegisterOwned : Boolean;
    function GetValidateScopes : Boolean;
    procedure SetValidateScopes(aValue : Boolean);
    procedure AddOwned<T>(const aName : string);
  class var
    GlobalInstance: TIocContainer;
  protected
    class constructor Create;
    class destructor Destroy;
  public
    constructor Create;
    destructor Destroy; override;
    function IsRegistered<TInterface: IInterface; TImplementation: class>(const aName: string): Boolean; overload;
    function IsRegistered<TInterface : IInterface>(const aName: string): Boolean; overload;
    function RegisterType<TInterface: IInterface; TImplementation: class>(const aName : string = '') : TIocRegistration<TImplementation>; overload;
    function RegisterType(aInterface: PTypeInfo; aImplementation : TClass; const aName : string = '') : TIocRegistration; overload;
    function RegisterInstance<T : class>(const aName: string = ''): TIocRegistration<T>; overload;
    function RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration; overload;
    function RegisterInstance<TInterface : IInterface>(aInstance : TInterface; const aName : string = '') : TIocRegistration; overload;
    function RegisterOptions<T : TOptions>(aOptions : TOptions) : TIocRegistration<T>; overload;
    function RegisterOptions<T : TOptions>(aOptions : TConfigureOptionsProc<T>) : TIocRegistration<T>; overload;
    function Resolve<T>(const aName : string = ''): T; overload;
    function Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue; overload;
    function ResolveAll<T>(const aName : string = '') : TList<T>;
    function AbstractFactory<T : class, constructor>(aClass : TClass) : T; overload;
    function AbstractFactory<T : class, constructor> : T; overload;
    function RegisterTypedFactory<TFactoryInterface : IInterface; TFactoryType : class, constructor>(const aName : string = '') : TIocRegistration<TTypedFactory<TFactoryType>>;
    function RegisterSimpleFactory<TInterface : IInterface; TImplementation : class, constructor>(const aName : string = '') : TIocRegistration;
    procedure Build;
    /// <summary>Opens a new scope. The caller owns it and must free it, before the container.</summary>
    function CreateScope : TIocScope;
    /// <summary>Registers IOwned&lt;TInterface&gt; for the registrations of TInterface (and aName), if
    /// it is not registered yet: needed when AutoRegisterOwned is False, or when TInterface was
    /// registered by the non-generic RegisterType or by RegisterInstance. It resolves what
    /// Resolve&lt;TInterface&gt; returns. Raises EIocRegisterError if TInterface is not registered.</summary>
    procedure RegisterOwned<TInterface : IInterface>(const aName : string = '');
    /// <summary>See TIocResolver.ValidateScopes.</summary>
    property ValidateScopes : Boolean read GetValidateScopes write SetValidateScopes;
    /// <summary>True (default): RegisterType&lt;I,T&gt; also registers IOwned&lt;I&gt;. False: IOwned&lt;I&gt;
    /// is registered only by RegisterOwned&lt;I&gt;. Set it before registering.</summary>
    property AutoRegisterOwned : Boolean read fAutoRegisterOwned write fAutoRegisterOwned;
    /// <summary>Exposes the internal registrator for advanced operations (Replace, Decorate).</summary>
    property Registrator: TIocRegistrator read fRegistrator;
  end;

  TIocServiceLocator = class
  public
    class function GetService<T> : T;
    class function TryToGetService<T: IInterface>(out aService : T) : Boolean;
  end;

  Name = class(TCustomAttribute)
  private
    fName: string;
  public
    constructor Create(aName: string);
    property Name: String read fName;
  end;

  EIocRegisterError = class(Exception);
  EIocResolverError = class(Exception);
  EIocBuildError = class(Exception);
  /// <summary>A scoped service resolved outside a scope, or a scope used while it is being freed.
  /// Deliberately NOT an EIocResolverError: constructor selection swallows EIocResolverError to try
  /// other constructors, and this must surface instead of yielding an object with nil dependencies.</summary>
  EIocScopeError = class(Exception);

  //singleton global instance
  function GlobalContainer : TIocContainer;

  function ServiceLocator : TIocServiceLocator;

implementation

function GlobalContainer: TIocContainer;
begin
  Result := TIocContainer.GlobalInstance;
end;

function ServiceLocator : TIocServiceLocator;
begin
  Result := TIocServiceLocator.Create;
end;

function IsOwnedType(aType : PTypeInfo) : Boolean;
begin
  //every IOwned<T> has the GUID of the generic declaration: what tells them apart from other
  //interfaces without knowing T
  Result := (aType <> nil) and (aType.Kind = tkInterface) and
    IsEqualGUID(GetTypeData(aType).Guid,GetTypeData(TypeInfo(IOwned<IInterface>)).Guid);
end;

{ TIocRegistration }

constructor TIocRegistration.Create(const aName : string);
begin
  fName := aName;
  fRegisterMode := TRegisterMode.rmTransient;
end;

function TIocRegistration.AsTransient: TIocRegistration;
begin
  Result := Self;
  fRegisterMode := TRegisterMode.rmTransient;
end;

function TIocRegistration.AsSingleton : TIocRegistration;
begin
  Result := Self;
  fRegisterMode := TRegisterMode.rmSingleton;
end;

function TIocRegistration.AsScoped: TIocRegistration;
begin
  Result := Self;
  fRegisterMode := TRegisterMode.rmScoped;
end;

function TIocRegistration.IsTransient: Boolean;
begin
  Result := fRegisterMode = TRegisterMode.rmTransient;
end;

function TIocRegistration.IsSingleton: Boolean;
begin
  Result := fRegisterMode = TRegisterMode.rmSingleton;
end;

function TIocRegistration.IsScoped: Boolean;
begin
  Result := fRegisterMode = TRegisterMode.rmScoped;
end;

{ TIocContainer }

class constructor TIocContainer.Create;
begin
  GlobalInstance := TIocContainer.Create;
end;

class destructor TIocContainer.Destroy;
begin
  if GlobalInstance <> nil then GlobalInstance.Free;
  inherited;
end;

function TIocContainer.AbstractFactory<T>(aClass: TClass): T;
begin
  Result := fResolver.CreateInstance(aClass).AsType<T>;
end;

function TIocContainer.AbstractFactory<T> : T;
begin
  Result := fResolver.CreateInstance(TClass(T)).AsType<T>;
end;

procedure TIocContainer.Build;
var
  dependency : TIocRegistration;
begin
  {$IFDEF DEBUG_IOC}
  TDebugger.TimeIt(Self,'Build','Container dependencies building...');
  {$ENDIF}
  for dependency in fRegistrator.DependencyOrder do
  begin
    try
      {$IFDEF DEBUG_IOC}
      TDebugger.Trace(Self,'[Building container]: %s',[dependency.fIntfInfo.Name]);
      {$ENDIF}
      if dependency.IsSingleton then fResolver.Resolve(dependency.fIntfInfo,dependency.Name);
      {$IFDEF DEBUG_IOC}
      TDebugger.Trace(Self,'[Built container]: %s',[dependency.fIntfInfo.Name]);
      {$ENDIF}
    except
      on E : Exception do raise EIocBuildError.CreateFmt('Build Error on "%s(%s)" dependency: %s!',[dependency.fImplementation.ClassName,dependency.Name,e.Message]);
    end;
  end;
end;

constructor TIocContainer.Create;
begin
  fLogger := nil;
  fRegistrator := TIocRegistrator.Create;
  fInjector := TIocInjector.Create;
  fResolver := TIocResolver.Create(fRegistrator,fInjector);
  fAutoRegisterOwned := True;
end;

function TIocContainer.CreateScope: TIocScope;
begin
  Result := TIocScope.Create(fResolver);
end;

function TIocContainer.GetValidateScopes: Boolean;
begin
  Result := fResolver.ValidateScopes;
end;

procedure TIocContainer.SetValidateScopes(aValue: Boolean);
begin
  fResolver.ValidateScopes := aValue;
end;

destructor TIocContainer.Destroy;
begin
  fInjector.Free;
  fResolver.Free;
  fRegistrator.Free;
  fLogger := nil;
  inherited;
end;

function TIocContainer.IsRegistered<TInterface, TImplementation>(const aName: string): Boolean;
begin
  Result := fRegistrator.IsRegistered<TInterface,TImplementation>(aName);
end;

function TIocContainer.IsRegistered<TInterface>(const aName: string): Boolean;
begin
  Result := fRegistrator.IsRegistered<TInterface>(aName);
end;

function TIocContainer.RegisterType<TInterface, TImplementation>(const aName: string): TIocRegistration<TImplementation>;
begin
  Result := fRegistrator.RegisterType<TInterface, TImplementation>(aName);
  //IOwned<TInterface> must be registered here: generic types cannot be instantiated at runtime
  if fAutoRegisterOwned then RegisterOwned<TInterface>(aName);
end;

procedure TIocContainer.RegisterOwned<TInterface>(const aName : string);
var
  named : string;
begin
  if not fRegistrator.IsRegistered<TInterface>(aName) then
  begin
    if aName.IsEmpty then named := ''
      else named := ' with the name "' + aName + '"';
    raise EIocRegisterError.CreateFmt('RegisterOwned<%s>: %s is not registered%s; register it first',
      [GetTypeName(TypeInfo(TInterface)),GetTypeName(TypeInfo(TInterface)),named]);
  end;
  //one IOwned per key: it resolves what Resolve<TInterface> returns, so it also follows a
  //registration added later
  if not fRegistrator.IsRegistered<IOwned<TInterface>>(aName) then AddOwned<TInterface>(aName);
end;

procedure TIocContainer.AddOwned<T>(const aName : string);
var
  container : TIocContainer;
  regName : string;
begin
  container := Self;
  regName := aName;
  //transient: every consumer gets its own scope. Registered through the non-generic
  //RegisterType so it does not register an IOwned of IOwned<T>
  fRegistrator.RegisterType(TypeInfo(IOwned<T>),TOwned<T>,aName).ActivatorDelegate :=
    function : TValue
    var
      scope : TIocScope;
      owned : IOwned<T>;
    begin
      //independent scope: it does not see the consumer's scoped instances, only singletons
      scope := container.CreateScope;
      try
        owned := TOwned<T>.Create(scope,scope.Resolve<T>(regName));
      except
        //the resolution failure is what the caller must see: an exception raised while releasing
        //what was already created in the scope is dropped instead of hiding it
        try
          scope.Free;
        except
        end;
        raise;
      end;
      Result := TValue.From<IOwned<T>>(owned);
    end;
end;

function TIocContainer.RegisterType(aInterface: PTypeInfo; aImplementation: TClass; const aName: string): TIocRegistration;
begin
  Result := fRegistrator.RegisterType(aInterface,aImplementation,aName);
end;

function TIocContainer.RegisterInstance<T>(const aName: string): TIocRegistration<T>;
begin
  Result := fRegistrator.RegisterInstance<T>(aName);
end;

function TIocContainer.RegisterTypedFactory<TFactoryInterface,TFactoryType>(const aName: string): TIocRegistration<TTypedFactory<TFactoryType>>;
var
  factory : TSimpleFactory<TFactoryType>;
  factoryAsIntf : IInterface;
  typedIntf : TFactoryInterface;
begin
  factory := TSimpleFactory<TFactoryType>.Create(fResolver);
  factoryAsIntf := factory;
  if factoryAsIntf.QueryInterface(GetTypeData(TypeInfo(TFactoryInterface))^.Guid, typedIntf) = S_OK then
  begin
    // TFactoryInterface is compatible with IFactory<TFactoryType> - use direct registration
    fRegistrator.RegisterInstance<TFactoryInterface>(typedIntf, aName).AsSingleton;
  end
  else
    raise EIocResolverError.CreateFmt('AddTypedFactory: %s must be IFactory<%s> on Win64',
      [GetTypeName(TypeInfo(TFactoryInterface)), TFactoryType.ClassName]);
  Result := Default(TIocRegistration<TTypedFactory<TFactoryType>>);
end;

function TIocContainer.RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration;
begin
  Result := fRegistrator.RegisterInstance(aTypeInfo,aName);
end;

function TIocContainer.RegisterInstance<TInterface>(aInstance: TInterface; const aName: string): TIocRegistration;
begin
  Result := fRegistrator.RegisterInstance<TInterface>(aInstance,aName);
end;

function TIocContainer.RegisterOptions<T>(aOptions: TOptions): TIocRegistration<T>;
begin
  Result := fRegistrator.RegisterOptions<T>(T(aOptions)).AsSingleton; //Delphi 13 does not convert TOptions to T implicitly (E2010)
end;

function TIocContainer.RegisterOptions<T>(aOptions: TConfigureOptionsProc<T>): TIocRegistration<T>;
var
  options : T;
begin
  options := T.Create;
  aOptions(options);
  Result := Self.RegisterOptions<T>(options);
end;

function TIocContainer.RegisterSimpleFactory<TInterface, TImplementation>(const aName: string): TIocRegistration;
begin
  Result := fRegistrator.RegisterInstance<IFactory<TInterface>>(TSimpleFactory<TInterface,TImplementation>.Create(fResolver),aName).AsSingleton;
end;

function TIocContainer.Resolve(aServiceType: PTypeInfo; const aName: string): TValue;
begin
  Result := fResolver.Resolve(aServiceType,aName);
end;

function TIocContainer.Resolve<T>(const aName : string = ''): T;
begin
  Result := fResolver.Resolve<T>(aName);
end;

function TIocContainer.ResolveAll<T>(const aName : string = ''): TList<T>;
begin
  Result := fResolver.ResolveAll<T>(aName);
end;

{ TIocRegistrator }

constructor TIocRegistrator.Create;
begin
  fDependencies := TDictionary<string, TObjectList<TIocRegistration>>.Create;
  fDependencyOrder := TObjectList<TIocRegistration>.Create(False); // Does not own objects
end;

destructor TIocRegistrator.Destroy;
var
  i : Integer;
  regList : TObjectList<TIocRegistration>;
begin
  // Free singleton instances that are not interfaced (non-reference counted objects)
  for i := fDependencyOrder.Count-1 downto 0 do
  begin
    if fDependencyOrder[i] <> nil then
    begin
      if (fDependencyOrder[i] is TIocRegistrationInstance) and
          (TIocRegistrationInstance(fDependencyOrder[i]).IsSingleton) then
            TIocRegistrationInstance(fDependencyOrder[i]).Instance.Free;
    end;
  end;
  // Manually free all the lists (each list will free its registrations because OwnsObjects = True)
  for regList in fDependencies.Values do
  begin
    regList.Free;
  end;
  fDependencies.Free; // Free the dictionary itself
  fDependencyOrder.Free; // Just frees the list, not the objects (OwnsObjects = False)
  inherited;
end;

function TIocRegistrator.GetKey(aPInfo : PTypeInfo; const aName : string = ''): string;
begin
  {$IFDEF NEXTGEN}
    {$IFDEF DELPHISYDNEY_UP}
    Result := string(aPInfo.Name);
    {$ELSE}
    Result := aPInfo .Name.ToString;
    {$ENDIF}
  {$ELSE}
  Result := string(aPInfo.Name);
  {$ENDIF}
  if not aName.IsEmpty then Result := Result + '.' + aName.ToLower;
end;

function TIocRegistrator.IsRegistered<TInterface, TImplementation>(const aName: string): Boolean;
var
  key : string;
  regList : TObjectList<TIocRegistration>;
  reg : TIocRegistration;
begin
  Result := False;
  key := GetKey(TypeInfo(TInterface),aName);
  if fDependencies.TryGetValue(key, regList) then
  begin
    for reg in regList do
    begin
      if reg.&Implementation = TImplementation then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TIocRegistrator.IsRegistered<T>(const aName: string): Boolean;
begin
  Result := fDependencies.ContainsKey(GetKey(TypeInfo(T),aName));
end;

function TIocRegistrator.RemoveRegistrations(const aKey: string): Boolean;
var
  regList : TObjectList<TIocRegistration>;
  reg     : TIocRegistration;
  idx     : Integer;
begin
  Result := fDependencies.TryGetValue(aKey, regList);
  if Result then
  begin
    // Remove registration references from the dependency-order list (does not own them)
    for reg in regList do
    begin
      idx := fDependencyOrder.IndexOf(reg);
      if idx >= 0 then fDependencyOrder.Delete(idx);
    end;
    fDependencies.Remove(aKey); // removes key but does NOT free regList
    regList.Free;               // free the list (OwnsObjects=True → frees registrations)
  end;
end;

function TIocRegistrator.RegisterInstance<T>(const aName: string): TIocRegistration<T>;
var
  reg : TIocRegistration;
begin
  reg := RegisterInstance(TypeInfo(T),aName);
  Result := TIocRegistration<T>.Create(reg);
end;

function TIocRegistrator.RegisterInstance<TInterface>(aInstance: TInterface; const aName: string): TIocRegistration;
var
  key : string;
  tpinfo : PTypeInfo;
  regList : TObjectList<TIocRegistration>;
begin
  tpinfo := TypeInfo(TInterface);
  key := GetKey(tpinfo,aName);
  
  if not fDependencies.TryGetValue(key, regList) then
  begin
    regList := TObjectList<TIocRegistration>.Create(True); // Owns objects
    fDependencies.Add(key, regList);
  end;
  
  Result := TIocRegistrationInterface.Create(aName);
  Result.IntfInfo := tpinfo;
  TIocRegistrationInterface(Result).Instance := aInstance;
  regList.Add(Result);
  fDependencyOrder.Add(Result);
end;

function TIocRegistrator.RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration;
var
  key : string;
  regList : TObjectList<TIocRegistration>;
begin
  key := GetKey(aTypeInfo,aName);
  
  if not fDependencies.TryGetValue(key, regList) then
  begin
    regList := TObjectList<TIocRegistration>.Create(True); // Owns objects
    fDependencies.Add(key, regList);
  end;
  
  Result := TIocRegistrationInstance.Create(aName);
  Result.IntfInfo := aTypeInfo;
  Result.&Implementation := aTypeInfo.TypeData.ClassType;
  regList.Add(Result);
  fDependencyOrder.Add(Result);
end;

function TIocRegistrator.RegisterOptions<T>(aOptions: T): TIocRegistration<T>;
var
  pInfo : PTypeInfo;
  key : string;
  reg : TIocRegistration;
  regList : TObjectList<TIocRegistration>;
begin
  pInfo := TypeInfo(IOptions<T>);
  key := GetKey(pInfo,'');
  
  if not fDependencies.TryGetValue(key, regList) then
  begin
    regList := TObjectList<TIocRegistration>.Create(True); // Owns objects
    fDependencies.Add(key, regList);
  end;
  
  reg := TIocRegistrationInterface.Create('');
  reg.IntfInfo := pInfo;
  reg.&Implementation := aOptions.ClassType;
  TIocRegistrationInterface(reg).Instance := TOptionValue<T>.Create(aOptions);
  regList.Add(reg);
  fDependencyOrder.Add(reg);
  Result := TIocRegistration<T>.Create(reg);
end;

function TIocRegistrator.RegisterType<TInterface, TImplementation>(const aName: string): TIocRegistration<TImplementation>;
var
  reg : TIocRegistration;
begin
  reg := RegisterType(TypeInfo(TInterface),TImplementation,aName);
  Result := TIocRegistration<TImplementation>.Create(reg);
end;

function TIocRegistrator.RegisterType(aTypeInfo : PTypeInfo; aImplementation : TClass; const aName : string = '') : TIocRegistration;
var
  key : string;
  regList : TObjectList<TIocRegistration>;
begin
  key := GetKey(aTypeInfo,aName);
  
  if not fDependencies.TryGetValue(key, regList) then
  begin
    regList := TObjectList<TIocRegistration>.Create(True); // Owns objects
    fDependencies.Add(key, regList);
  end;
  
  Result := TIocRegistrationInterface.Create(aName);
  Result.IntfInfo := aTypeInfo;
  Result.&Implementation := aImplementation;
  regList.Add(Result);
  fDependencyOrder.Add(Result);
end;

{ TIocResolver }

constructor TIocResolver.Create(aRegistrator : TIocRegistrator; aInjector : TIocInjector);
begin
  fRegistrator := aRegistrator;
  fInjector := aInjector;
  fValidateScopes := True;
end;

function TIocResolver.CreateInstance(aClass: TClass): TValue;
begin
  Result := CreateInstance(aClass,nil);
end;

function TIocResolver.CreateInstance(aClass: TClass; aScope: TIocScope): TValue;
var
  ctx : TRttiContext;
  rtype : TRttiType;
  rmethod : TRttiMethod;
  ownCtors : TList<TRttiMethod>;
  inheritedCtors : TList<TRttiMethod>;
  allCtors : TList<TRttiMethod>;
  bestCtor : TRttiMethod;

  function TryInvoke(aCtor: TRttiMethod; out aResult: TValue): Boolean;
  var
    lParam : TRttiParameter;
    lAtt : TCustomAttribute;
    lName : string;
    lVal : TValue;
    lVals : TArray<TValue>;
  begin
    Result := False;
    lVals := nil;
    for lParam in aCtor.GetParameters do
    begin
      lName := EmptyStr;
      for lAtt in lParam.GetAttributes do
        if lAtt is Name then begin lName := Name(lAtt).Name; Break; end;
      if lParam.ParamType.TypeKind in [tkClass, tkInterface] then
      begin
        //IOwned<I> is asked for on purpose, for a scope of its own: without its registration this
        //is a configuration error, never a reason to try another constructor
        if IsOwnedType(lParam.ParamType.Handle) and
          not fRegistrator.Dependencies.ContainsKey(fRegistrator.GetKey(lParam.ParamType.Handle,lName)) then
          raise EIocRegisterError.CreateFmt('%s.%s asks for %s, which is not registered: call RegisterOwned or ' +
            'enable AutoRegisterOwned',[aCtor.Parent.Name,aCtor.Name,lParam.ParamType.Name]);
        try lVal := Resolve(lParam.ParamType.Handle, lName, aScope);
        except on EIocResolverError do Exit; // required dep not found
        end;
      end
      else
      begin
        try lVal := Resolve(lParam.ParamType.Handle, lName, aScope);
        except on EIocResolverError do TValue.Make(nil, lParam.ParamType.Handle, lVal); end;
      end;
      lVals := lVals + [lVal];
    end;
    aResult := aCtor.Invoke(TRttiInstanceType(rtype).MetaclassType, lVals);
    Result := True;
  end;

begin
  Result := nil;
  rtype := ctx.GetType(aClass);
  if rtype = nil then Exit;

  // Separate own constructors (declared on aClass) from inherited ones
  ownCtors := TList<TRttiMethod>.Create;
  inheritedCtors := TList<TRttiMethod>.Create;
  try
    for rmethod in TRttiInstanceType(rtype).GetMethods do
    begin
      if rmethod.IsConstructor then
      begin
        if rmethod.Parent = rtype then ownCtors.Add(rmethod)
        else inheritedCtors.Add(rmethod);
      end;
    end;

    // Sort own constructors: parameterless first, then by param count ascending
    ownCtors.Sort(TComparer<TRttiMethod>.Construct(
      function(const L, R: TRttiMethod): Integer
      begin Result := Length(L.GetParameters) - Length(R.GetParameters); end));
    inheritedCtors.Sort(TComparer<TRttiMethod>.Construct(
      function(const L, R: TRttiMethod): Integer
      begin Result := Length(L.GetParameters) - Length(R.GetParameters); end));

    allCtors := TList<TRttiMethod>.Create;
    try
      allCtors.AddRange(ownCtors);
      allCtors.AddRange(inheritedCtors);

      for bestCtor in allCtors do
      begin
        if TryInvoke(bestCtor, Result) then Exit;
      end;
    finally
      allCtors.Free;
    end;
  finally
    ownCtors.Free;
    inheritedCtors.Free;
  end;
end;

function TIocResolver.Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue;
begin
  Result := Resolve(aServiceType,aName,nil);
end;

function TIocResolver.Resolve(aServiceType: PTypeInfo; const aName: string; aScope: TIocScope): TValue;
var
  key : string;
  reg : TIocRegistration;
  regList : TObjectList<TIocRegistration>;
  intf : IInterface;
begin
  Result := nil;
  reg := nil;
  key := fRegistrator.GetKey(aServiceType,aName);
  {$IFDEF DEBUG_IOC}
  TDebugger.Trace(Self,'Resolving dependency: %s',[key]);
  {$ENDIF}
  if not fRegistrator.Dependencies.TryGetValue(key, regList) then 
    raise EIocResolverError.CreateFmt('Type "%s" not registered for IOC!',[aServiceType.Name]);
  
  if regList.Count = 0 then
    raise EIocResolverError.CreateFmt('Type "%s" has empty registration list!',[aServiceType.Name]);
    
  reg := regList.Last; // Resolve LAST registered (.NET Core style)

  //scoped: one instance per scope, created and released by the scope
  if reg.IsScoped then
  begin
    if aScope <> nil then Exit(aScope.GetOrCreate(reg,aServiceType));
    if fValidateScopes then
      raise EIocScopeError.CreateFmt('Scoped service "%s" resolved outside a scope. Resolve it from a TIocScope ' +
        '(TIocContainer.CreateScope), not from the root container nor as a dependency of a singleton; or set ' +
        'ValidateScopes := False to keep the previous behaviour (resolved as transient).',[aServiceType.Name]);
    //ValidateScopes = False: previous behaviour, built below as a transient
  end;
  
  //if is singleton return already instance if exists
  if reg.IsSingleton then
  begin
    if reg is TIocRegistrationInterface then
    begin
      if TIocRegistrationInterface(reg).Instance <> nil then
      begin
        if TIocRegistrationInterface(reg).Instance.QueryInterface(GetTypeData(aServiceType).Guid,intf) <> 0 then 
          raise EIocResolverError.CreateFmt('Implementation for "%s" not registered!',[aServiceType.Name]);
        TValue.Make(@intf,aServiceType,Result);
        {$IFDEF DEBUG_IOC}
        TDebugger.Trace(Self,'Resolved dependency: %s',[reg.fIntfInfo.Name]);
        {$ENDIF}
        Exit;
      end;
      // Instance is nil: fall through to create it below
    end
    else
    begin
      if TIocRegistrationInstance(reg).Instance <> nil then
      begin
        Result := TIocRegistrationInstance(reg).Instance;
        {$IFDEF DEBUG_IOC}
        TDebugger.Trace(Self,'Resolved dependency: %s',[reg.fIntfInfo.Name]);
        {$ENDIF}
        Exit;
      end;
      // Instance is nil: fall through to create it below
    end;
    //dependencies of a singleton are resolved with no scope: a scoped one would be captured for
    //the whole application lifetime, so it raises EIocScopeError instead
    aScope := nil;
  end;
  //instance not created yet or needs to be created (transient)
  Result := BuildValue(reg,aServiceType,aScope);
  // Only cache instance for singletons; class registrations keep the last built instance
  if reg is TIocRegistrationInterface then
  begin
    if reg.IsSingleton then TIocRegistrationInterface(reg).Instance := Result.AsInterface;
  end
  else TIocRegistrationInstance(reg).Instance := Result.AsObject;
  {$IFDEF DEBUG_IOC}
  TDebugger.Trace(Self,'Built dependency: %s',[reg.fIntfInfo.Name]);
  {$ENDIF}
end;

function TIocResolver.BuildValue(aReg: TIocRegistration; aServiceType: PTypeInfo; aScope: TIocScope): TValue;
var
  intf : IInterface;
  newInst : IInterface;
begin
  //builds a new instance, never caches: shared by Resolve and by the scopes
  if aReg is TIocRegistrationInterface then
  begin
    //if instance already set (RegisterInstance<TInterface>(obj)), use it directly
    newInst := TIocRegistrationInterface(aReg).Instance;
    if newInst = nil then
    begin
      //otherwise, create new instance from &Implementation
      if aReg.&Implementation = nil then raise EIocResolverError.CreateFmt('Implemention for "%s" not defined!',[aServiceType.Name]);
      {$IFDEF DEBUG_IOC}
      TDebugger.Trace(Self,'Building dependency: %s',[aReg.fIntfInfo.Name]);
      {$ENDIF}
      if Assigned(aReg.ActivatorDelegate) then newInst := aReg.ActivatorDelegate().AsInterface
        else newInst := CreateInstance(aReg.&Implementation,aScope).AsInterface;
    end;
    if (newInst = nil) or (newInst.QueryInterface(GetTypeData(aServiceType).Guid,intf) <> 0) then raise EIocResolverError.CreateFmt('Implementation for "%s" not registered!',[aServiceType.Name]);
    TValue.Make(@intf,aServiceType,Result);
  end
  else
  begin
    {$IFDEF DEBUG_IOC}
    TDebugger.Trace(Self,'Building dependency: %s',[aReg.fIntfInfo.Name]);
    {$ENDIF}
    if Assigned(aReg.ActivatorDelegate) then Result := aReg.ActivatorDelegate().AsObject
      else Result := CreateInstance(aReg.&Implementation,aScope).AsObject;
  end;
end;

function TIocResolver.Resolve<T>(const aName : string = ''): T;
var
  pInfo : PTypeInfo;
begin
  //Result := Default(T);
  pInfo := TypeInfo(T);

  Result := Resolve(pInfo,aName).AsType<T>;
end;

function TIocResolver.ResolveAll<T>(const aName : string = '') : TList<T>;
var
  pInfo : PTypeInfo;
  regList : TObjectList<TIocRegistration>;
  reg : TIocRegistration;
  key : string;
  resolved : TValue;
begin
  Result := TList<T>.Create;
  pInfo := TypeInfo(T);
  key := fRegistrator.GetKey(pInfo, aName);

  if fRegistrator.Dependencies.TryGetValue(key, regList) then
  begin
    for reg in regList do
    begin
      // Resolve each registration individually
      // For singletons, this will return the same instance
      // For transients, this will create new instances
      resolved := Resolve(pInfo, reg.Name);
      Result.Add(resolved.AsType<T>);
    end;
  end;
end;

{ TIocScope }

constructor TIocScope.Create(aResolver: TIocResolver);
begin
  fResolver := aResolver;
  fInterfaces := TDictionary<TIocRegistration, IInterface>.Create;
  fObjects := TDictionary<TIocRegistration, TObject>.Create;
  fCreated := TList<IInterface>.Create;
  fCreatedObjects := TList<TObject>.Create;
end;

destructor TIocScope.Destroy;
var
  i : Integer;
  firstError : TObject;
begin
  //from now on nothing is resolved from this scope, not even by the destructors of its instances
  fReleasing := True;
  //a destructor that raises must not stop the release of the others: the first exception is
  //kept and raised again once everything is released
  firstError := nil;
  //release in reverse order of creation: dependents go before their dependencies
  fInterfaces.Clear;
  for i := fCreated.Count - 1 downto 0 do
  begin
    try
      fCreated[i] := nil;
    except
      if firstError = nil then firstError := TObject(AcquireExceptionObject);
    end;
  end;
  fCreated.Free;
  fInterfaces.Free;
  //class (non-interface) scoped instances are owned by the scope
  fObjects.Clear;
  for i := fCreatedObjects.Count - 1 downto 0 do
  begin
    try
      fCreatedObjects[i].Free;
    except
      if firstError = nil then firstError := TObject(AcquireExceptionObject);
    end;
  end;
  fCreatedObjects.Free;
  fObjects.Free;
  inherited;
  if firstError <> nil then raise firstError;
end;

procedure TIocScope.CheckAlive;
begin
  if fReleasing then
    raise EIocScopeError.Create('The scope is being freed: nothing can be resolved from it anymore ' +
      '(e.g. from the destructor of one of its instances).');
end;

function TIocScope.GetOrCreate(aReg: TIocRegistration; aServiceType: PTypeInfo): TValue;
var
  cached : IInterface;
  intf : IInterface;
  obj : TObject;
begin
  //the single place where the scope creates and keeps instances: reached from Resolve, but also
  //from dependency chains and IOwned
  CheckAlive;
  if aReg is TIocRegistrationInterface then
  begin
    if not fInterfaces.TryGetValue(aReg,cached) then
    begin
      //dependencies are resolved within this same scope
      cached := fResolver.BuildValue(aReg,aServiceType,Self).AsInterface;
      fInterfaces.Add(aReg,cached);
      fCreated.Add(cached);
    end;
    if cached.QueryInterface(GetTypeData(aServiceType).Guid,intf) <> 0 then
      raise EIocResolverError.CreateFmt('Implementation for "%s" not registered!',[aServiceType.Name]);
    TValue.Make(@intf,aServiceType,Result);
  end
  else
  begin
    if not fObjects.TryGetValue(aReg,obj) then
    begin
      obj := fResolver.BuildValue(aReg,aServiceType,Self).AsObject;
      fObjects.Add(aReg,obj);
      fCreatedObjects.Add(obj);
    end;
    Result := obj;
  end;
end;

function TIocScope.Resolve(aServiceType: PTypeInfo; const aName: string): TValue;
begin
  CheckAlive;
  Result := fResolver.Resolve(aServiceType,aName,Self);
end;

function TIocScope.Resolve<T>(const aName: string): T;
begin
  Result := Resolve(TypeInfo(T),aName).AsType<T>;
end;

{ TOwned<T> }

constructor TOwned<T>.Create(aScope: TIocScope; const aValue: T);
begin
  fScope := aScope;
  fValue := aValue;
end;

destructor TOwned<T>.Destroy;
begin
  //release the value before its scope: it may hold references to the scope's instances
  try
    fValue := Default(T);
  except
    //the scope is released anyway, so its instances do not leak; the value's exception is the
    //one raised (one raised while releasing the scope is dropped)
    try
      fScope.Free;
    except
    end;
    raise;
  end;
  fScope.Free;
  inherited;
end;

function TOwned<T>.Value: T;
begin
  Result := fValue;
end;

{ TIocRegistration<T> }

function TIocRegistration<T>.AsScoped: TIocRegistration<T>;
begin
  Result := Self;
  fRegistration.AsScoped;
end;

function TIocRegistration<T>.AsSingleton: TIocRegistration<T>;
begin
  Result := Self;
  fRegistration.AsSingleton;
end;

function TIocRegistration<T>.AsTransient: TIocRegistration<T>;
begin
  Result := Self;
  fRegistration.AsTransient;
end;

constructor TIocRegistration<T>.Create(aRegistration: TIocRegistration);
begin
  fRegistration := aRegistration;
end;

function TIocRegistration<T>.DelegateTo(aDelegate: TActivatorDelegate<T>): TIocRegistration<T>;
begin
  Result := Self;
  fRegistration.ActivatorDelegate := function: TValue
                                     begin
                                       Result := TValue.From<T>(aDelegate()); //invoke the delegate explicitly (Delphi 13, E2010)
                                     end;
end;

{ TTypedFactoryHelper - placeholder }

{ TIocServiceLocator }

class function TIocServiceLocator.GetService<T> : T;
begin
  Result := GlobalContainer.Resolve<T>;
end;

class function TIocServiceLocator.TryToGetService<T>(out aService : T) : Boolean;
begin
  Result := GlobalContainer.IsRegistered<T>('');
  if Result then aService := GlobalContainer.Resolve<T>;
end;

{ TSimpleFactory<T> }

constructor TSimpleFactory<T>.Create(aResolver: TIocResolver);
begin
  fResolver := aResolver;
end;

function TSimpleFactory<T>.New: T;
begin
  Result := fResolver.CreateInstance(TClass(T)).AsType<T>;
end;

{ TSimpleFactory<TInterface, TImplementation> }

constructor TSimpleFactory<TInterface, TImplementation>.Create(aResolver: TIocResolver);
begin
  fResolver := aResolver;
end;

function TSimpleFactory<TInterface, TImplementation>.New: TInterface;
begin
  Result := fResolver.CreateInstance(TClass(TImplementation)).AsType<TInterface>;
end;

{ Name }
constructor Name.Create(aName: string);
begin
  fName := aName;
end;


end.
