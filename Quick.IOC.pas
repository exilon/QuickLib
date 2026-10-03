{ ***************************************************************************

  Copyright (c) 2016-2026 Kike Perez

  Unit        : Quick.IoC
  Description : IoC Dependency Injector
  Author      : Kike Perez
  Version     : 1.0
  Created     : 19/10/2019
  Modified    : 02/10/2026

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
    fSingletonLock : TObject;
    fValidateScopes : Boolean;
    function CreateInstance(aClass : TClass) : TValue; overload;
    function CreateInstance(aClass : TClass; aScope : TIocScope) : TValue; overload;
    function FindInjectConstructor(aType : TRttiInstanceType) : TRttiMethod;
    function GetConstructorCandidates(aType : TRttiInstanceType) : TArray<TRttiMethod>;
    function MissingParameters(aCtor : TRttiMethod) : string;
    function DiagnoseConstructors : TArray<string>;
    function FindRegistration(aServiceType : PTypeInfo; const aName : string) : TIocRegistration;
    function BuildValue(aReg : TIocRegistration; aServiceType : PTypeInfo; aScope : TIocScope) : TValue;
    function ResolveSingleton(aReg : TIocRegistration; aServiceType : PTypeInfo) : TValue;
    function ResolveRegistration(aReg : TIocRegistration; aServiceType : PTypeInfo; aScope : TIocScope) : TValue;
  public
    constructor Create(aRegistrator : TIocRegistrator; aInjector : TIocInjector);
    destructor Destroy; override;
    function Resolve<T>(const aName : string = ''): T; overload;
    function Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue; overload;
    /// <summary>Resolves within aScope. aScope = nil means the root (no scope).</summary>
    function Resolve(aServiceType: PTypeInfo; const aName : string; aScope : TIocScope): TValue; overload;
    /// <summary>One instance per registration of T (and aName), in registration order, each
    /// with its own lifetime. The caller owns the returned list.</summary>
    function ResolveAll<T>(const aName : string = '') : TList<T>; overload;
    /// <summary>Same as ResolveAll&lt;T&gt;, within aScope (nil = root).</summary>
    function ResolveAll<T>(const aName : string; aScope : TIocScope) : TList<T>; overload;
    /// <summary>True (default): resolving a scoped service outside a scope (from the root
    /// container or as a dependency of a singleton) raises EIocScopeError.
    /// False: legacy behaviour, a scoped service outside a scope is built as transient.</summary>
    property ValidateScopes : Boolean read fValidateScopes write fValidateScopes;
  end;

  /// <summary>A resolution scope (e.g. one per HTTP request). Services registered AsScoped
  /// are created once per scope and released when the scope is freed, in reverse order of
  /// creation. Singletons and transients behave as usual. A scope is not thread-safe: use it
  /// from one thread at a time.</summary>
  TIocScope = class
  private
    fResolver : TIocResolver;
    fInterfaces : TDictionary<TIocRegistration, IInterface>;
    fObjects : TDictionary<TIocRegistration, TObject>;
    fCreated : TList<IInterface>;
    fCreatedObjects : TList<TObject>;
    function GetOrCreate(aReg : TIocRegistration; aServiceType : PTypeInfo) : TValue;
  public
    constructor Create(aResolver : TIocResolver);
    destructor Destroy; override;
    function Resolve<T>(const aName : string = ''): T; overload;
    function Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue; overload;
    /// <summary>One instance per registration of T, within this scope. The caller owns the list.</summary>
    function ResolveAll<T>(const aName : string = '') : TList<T>;
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

  /// <summary>A dependency resolved in its own, new scope (like Autofac's Owned&lt;T&gt;).
  /// Asking for IOwned&lt;T&gt; in a constructor opens an independent scope, resolves T in it
  /// and keeps that scope alive while the IOwned&lt;T&gt; is referenced: scoped services in
  /// T's dependency chain get their own instances instead of the consumer's.
  /// IOwned&lt;T&gt; is registered automatically by RegisterType&lt;T,TImplementation&gt;.</summary>
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
    fValidateConstructors : Boolean;
    function GetValidateScopes : Boolean;
    procedure SetValidateScopes(aValue : Boolean);
    procedure RegisterOwned<T>(const aName : string);
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
    /// <summary>Opens a new scope. The caller owns it and must free it.</summary>
    function CreateScope : TIocScope;
    /// <summary>Static check of the constructor each registration would use, without building
    /// anything. Reports classes that would be created by TObject.Create although they declare
    /// other constructors, and [Inject] constructors with unregistered parameters.</summary>
    function DiagnoseConstructors : TArray<string>;
    /// <summary>See TIocResolver.ValidateScopes.</summary>
    property ValidateScopes : Boolean read GetValidateScopes write SetValidateScopes;
    /// <summary>True: Build runs DiagnoseConstructors and raises EIocBuildError listing the
    /// problems. False (default): Build does not run the diagnostics.</summary>
    property ValidateConstructors : Boolean read fValidateConstructors write fValidateConstructors;
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

  /// <summary>Marks the constructor the container must use. It is honored on the class itself
  /// or on the nearest ancestor that declares constructors (so a class that inherits its
  /// constructor does not fall back to TObject.Create). If the marked constructor cannot be
  /// satisfied, resolution fails instead of trying another constructor. Classes without it
  /// keep the default rule: own constructors before inherited ones, fewest parameters first.</summary>
  Inject = class(TCustomAttribute)
  end;

  EIocRegisterError = class(Exception);
  EIocResolverError = class(Exception);
  EIocBuildError = class(Exception);
  /// <summary>Scoped service resolved outside a scope. Deliberately NOT an EIocResolverError:
  /// constructor selection swallows EIocResolverError to try other constructors, and a scope
  /// violation must surface instead of yielding an object with nil dependencies.</summary>
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
  problems : TArray<string>;
begin
  {$IFDEF DEBUG_IOC}
  TDebugger.TimeIt(Self,'Build','Container dependencies building...');
  {$ENDIF}
  if fValidateConstructors then
  begin
    problems := DiagnoseConstructors;
    if Length(problems) > 0 then
      raise EIocBuildError.Create('Constructor diagnostics failed:' + sLineBreak + string.Join(sLineBreak,problems));
  end;
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

function TIocContainer.CreateScope: TIocScope;
begin
  Result := TIocScope.Create(fResolver);
end;

function TIocContainer.DiagnoseConstructors: TArray<string>;
begin
  Result := fResolver.DiagnoseConstructors;
end;

function TIocContainer.GetValidateScopes: Boolean;
begin
  Result := fResolver.ValidateScopes;
end;

procedure TIocContainer.SetValidateScopes(aValue: Boolean);
begin
  fResolver.ValidateScopes := aValue;
end;

constructor TIocContainer.Create;
begin
  fLogger := nil;
  fRegistrator := TIocRegistrator.Create;
  fInjector := TIocInjector.Create;
  fResolver := TIocResolver.Create(fRegistrator,fInjector);
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
  RegisterOwned<TInterface>(aName);
end;

procedure TIocContainer.RegisterOwned<T>(const aName: string);
var
  container : TIocContainer;
  regName : string;
begin
  container := Self;
  regName := aName;
  //transient: every consumer gets its own scope. Registered through the non-generic
  //RegisterType so it does not recurse into RegisterOwned<IOwned<T>>
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
        scope.Free;
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
  fSingletonLock := TObject.Create;
  fValidateScopes := True;
end;

destructor TIocResolver.Destroy;
begin
  fSingletonLock.Free;
  inherited;
end;

function TIocResolver.CreateInstance(aClass: TClass): TValue;
begin
  Result := CreateInstance(aClass, nil);
end;

function TIocResolver.CreateInstance(aClass: TClass; aScope: TIocScope): TValue;
var
  ctx : TRttiContext;
  rtype : TRttiType;
  injectCtor : TRttiMethod;
  bestCtor : TRttiMethod;
  missing : string;

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

  //a constructor marked [Inject] is the only candidate: no silent fallback to another one
  injectCtor := FindInjectConstructor(TRttiInstanceType(rtype));
  if injectCtor <> nil then
  begin
    if not TryInvoke(injectCtor, Result) then
    begin
      missing := MissingParameters(injectCtor);
      if missing.IsEmpty then missing := 'a dependency could not be resolved';
      raise EIocResolverError.CreateFmt('Constructor %s.%s marked [Inject] could not be satisfied: %s',
        [aClass.ClassName,injectCtor.Name,missing]);
    end;
    Exit;
  end;

  for bestCtor in GetConstructorCandidates(TRttiInstanceType(rtype)) do
  begin
    if TryInvoke(bestCtor, Result) then Exit;
  end;
end;

function HasInjectAttribute(aMethod : TRttiMethod) : Boolean;
var
  att : TCustomAttribute;
begin
  Result := False;
  for att in aMethod.GetAttributes do
    if att is Inject then Exit(True);
end;

function ParameterRegName(aParam : TRttiParameter) : string;
var
  att : TCustomAttribute;
begin
  Result := EmptyStr;
  for att in aParam.GetAttributes do
    if att is Name then Exit(Name(att).Name);
end;

function ConstructorSignature(aCtor : TRttiMethod) : string;
var
  lParam : TRttiParameter;
  params : TArray<string>;
begin
  params := nil;
  for lParam in aCtor.GetParameters do
    if lParam.ParamType <> nil then params := params + [lParam.Name + ': ' + lParam.ParamType.Name]
      else params := params + [lParam.Name];
  Result := Format('%s.%s(%s)',[aCtor.Parent.Name,aCtor.Name,string.Join('; ',params)]);
end;

function TIocResolver.FindInjectConstructor(aType: TRttiInstanceType): TRttiMethod;
var
  level : TRttiInstanceType;
  rmethod : TRttiMethod;
  declaresCtor : Boolean;
  marked : Integer;
begin
  //the nearest type in the hierarchy that declares constructors decides: if it marks one with
  //[Inject] that is the constructor; if it marks none, the default rule applies
  Result := nil;
  level := aType;
  while level <> nil do
  begin
    declaresCtor := False;
    marked := 0;
    for rmethod in level.GetDeclaredMethods do
    begin
      if not rmethod.IsConstructor then Continue;
      declaresCtor := True;
      if HasInjectAttribute(rmethod) then
      begin
        Inc(marked);
        Result := rmethod;
      end;
    end;
    //not an EIocResolverError on purpose: it is a configuration error and must not be swallowed
    if marked > 1 then raise EIocRegisterError.CreateFmt('%s declares more than one constructor marked [Inject]',[level.Name]);
    if declaresCtor then Exit;
    level := TRttiInstanceType(level.BaseType);
  end;
end;

function TIocResolver.GetConstructorCandidates(aType: TRttiInstanceType): TArray<TRttiMethod>;
var
  rmethod : TRttiMethod;
  ownCtors : TList<TRttiMethod>;
  inheritedCtors : TList<TRttiMethod>;
  comparer : IComparer<TRttiMethod>;
begin
  //default rule (no [Inject]), unchanged: own constructors before inherited ones
  ownCtors := TList<TRttiMethod>.Create;
  inheritedCtors := TList<TRttiMethod>.Create;
  try
    for rmethod in aType.GetMethods do
    begin
      if rmethod.IsConstructor then
      begin
        if rmethod.Parent = aType then ownCtors.Add(rmethod)
        else inheritedCtors.Add(rmethod);
      end;
    end;
    // Sort own constructors: parameterless first, then by param count ascending
    comparer := TComparer<TRttiMethod>.Construct(
      function(const L, R: TRttiMethod): Integer
      begin Result := Length(L.GetParameters) - Length(R.GetParameters); end);
    ownCtors.Sort(comparer);
    inheritedCtors.Sort(comparer);
    Result := ownCtors.ToArray + inheritedCtors.ToArray;
  finally
    ownCtors.Free;
    inheritedCtors.Free;
  end;
end;

function TIocResolver.MissingParameters(aCtor: TRttiMethod): string;
var
  lParam : TRttiParameter;
  missing : TArray<string>;
begin
  //class/interface parameters with no registration (others get their default value)
  missing := nil;
  for lParam in aCtor.GetParameters do
  begin
    if (lParam.ParamType = nil) or not (lParam.ParamType.TypeKind in [tkClass, tkInterface]) then Continue;
    if not fRegistrator.Dependencies.ContainsKey(fRegistrator.GetKey(lParam.ParamType.Handle,ParameterRegName(lParam))) then
      missing := missing + [lParam.Name + ': ' + lParam.ParamType.Name];
  end;
  Result := string.Join(', ',missing);
end;

function TIocResolver.DiagnoseConstructors: TArray<string>;
var
  ctx : TRttiContext;
  reg : TIocRegistration;
  checked : TList<TClass>;
  rtype : TRttiInstanceType;
  injectCtor : TRttiMethod;
  chosen : TRttiMethod;
  ctor : TRttiMethod;
  candidates : TArray<TRttiMethod>;
  others : TArray<string>;
  declaresOwn : Boolean;
  declaresBelowTObject : Boolean;
  missing : string;
begin
  Result := nil;
  checked := TList<TClass>.Create;
  try
    for reg in fRegistrator.DependencyOrder do
    begin
      //only registrations the container builds through a constructor, each class once
      if (reg.&Implementation = nil) or Assigned(reg.ActivatorDelegate) or checked.Contains(reg.&Implementation) then Continue;
      if (reg is TIocRegistrationInterface) and (TIocRegistrationInterface(reg).Instance <> nil) then Continue;
      checked.Add(reg.&Implementation);
      rtype := TRttiInstanceType(ctx.GetType(reg.&Implementation));
      if rtype = nil then Continue;

      try
        injectCtor := FindInjectConstructor(rtype);
      except
        on E : EIocRegisterError do
        begin
          Result := Result + [E.Message];
          Continue;
        end;
      end;
      if injectCtor <> nil then
      begin
        missing := MissingParameters(injectCtor);
        if not missing.IsEmpty then
          Result := Result + [Format('%s: %s marked [Inject] has unregistered parameters: %s',
            [rtype.Name,ConstructorSignature(injectCtor),missing])];
        Continue;
      end;

      //simulates the default rule: first candidate whose class/interface parameters are registered
      candidates := GetConstructorCandidates(rtype);
      chosen := nil;
      for ctor in candidates do
        if MissingParameters(ctor).IsEmpty then
        begin
          chosen := ctor;
          Break;
        end;
      if chosen = nil then Continue;

      declaresOwn := False;
      declaresBelowTObject := False;
      others := nil;
      for ctor in candidates do
      begin
        if ctor.Parent = rtype then declaresOwn := True;
        if ctor.Parent.Handle <> TypeInfo(TObject) then declaresBelowTObject := True;
        if ctor = chosen then Continue;
        missing := MissingParameters(ctor);
        if missing.IsEmpty then others := others + [ConstructorSignature(ctor)]
          else others := others + [ConstructorSignature(ctor) + ' (unregistered: ' + missing + ')'];
      end;

      if (chosen.Parent.Handle = TypeInfo(TObject)) and declaresBelowTObject then
        Result := Result + [Format('%s would be created by TObject.Create, leaving its dependencies nil. ' +
          'Other constructors: %s. Mark the intended one with [Inject].',[rtype.Name,string.Join('; ',others)])]
      else if declaresOwn and (chosen.Parent <> rtype) then
        Result := Result + [Format('%s declares constructors but none is satisfiable; inherited %s would be used. ' +
          'Other constructors: %s.',[rtype.Name,ConstructorSignature(chosen),string.Join('; ',others)])];
    end;
  finally
    checked.Free;
  end;
end;

function TIocResolver.FindRegistration(aServiceType: PTypeInfo; const aName: string): TIocRegistration;
var
  key : string;
  regList : TObjectList<TIocRegistration>;
begin
  key := fRegistrator.GetKey(aServiceType,aName);
  {$IFDEF DEBUG_IOC}
  TDebugger.Trace(Self,'Resolving dependency: %s',[key]);
  {$ENDIF}
  if not fRegistrator.Dependencies.TryGetValue(key, regList) then
    raise EIocResolverError.CreateFmt('Type "%s" not registered for IOC!',[aServiceType.Name]);
  if regList.Count = 0 then
    raise EIocResolverError.CreateFmt('Type "%s" has empty registration list!',[aServiceType.Name]);
  Result := regList.Last; // Resolve LAST registered (.NET Core style)
end;

function TIocResolver.BuildValue(aReg: TIocRegistration; aServiceType: PTypeInfo; aScope: TIocScope): TValue;
var
  intf : IInterface;
  newInst : IInterface;
begin
  //builds a new instance (or returns the one given to RegisterInstance<TInterface>); never caches
  if aReg is TIocRegistrationInterface then
  begin
    newInst := TIocRegistrationInterface(aReg).Instance;
    if newInst = nil then
    begin
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

function TIocResolver.ResolveSingleton(aReg: TIocRegistration; aServiceType: PTypeInfo): TValue;
var
  intf : IInterface;
begin
  //lazy creation must not race: two threads could otherwise build two "singletons".
  //TMonitor is reentrant, so a singleton depending on another singleton is fine.
  TMonitor.Enter(fSingletonLock);
  try
    if aReg is TIocRegistrationInterface then
    begin
      //dependencies of a singleton are resolved with no scope: a scoped dependency would be
      //captured for the whole application lifetime, so it raises EIocScopeError instead
      if TIocRegistrationInterface(aReg).Instance = nil then
        TIocRegistrationInterface(aReg).Instance := BuildValue(aReg,aServiceType,nil).AsInterface;
      if TIocRegistrationInterface(aReg).Instance.QueryInterface(GetTypeData(aServiceType).Guid,intf) <> 0 then
        raise EIocResolverError.CreateFmt('Implementation for "%s" not registered!',[aServiceType.Name]);
      TValue.Make(@intf,aServiceType,Result);
    end
    else
    begin
      if TIocRegistrationInstance(aReg).Instance = nil then
        TIocRegistrationInstance(aReg).Instance := BuildValue(aReg,aServiceType,nil).AsObject;
      Result := TIocRegistrationInstance(aReg).Instance;
    end;
  finally
    TMonitor.Exit(fSingletonLock);
  end;
end;

function TIocResolver.Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue;
begin
  Result := Resolve(aServiceType,aName,nil);
end;

function TIocResolver.Resolve(aServiceType: PTypeInfo; const aName: string; aScope: TIocScope): TValue;
begin
  Result := ResolveRegistration(FindRegistration(aServiceType,aName),aServiceType,aScope);
end;

function TIocResolver.ResolveRegistration(aReg: TIocRegistration; aServiceType: PTypeInfo; aScope: TIocScope): TValue;
begin
  //applies aReg's own lifetime: shared by Resolve (last registration) and ResolveAll (each one)
  if aReg.IsSingleton then Result := ResolveSingleton(aReg,aServiceType)
  else if aReg.IsScoped then
  begin
    if aScope <> nil then Result := aScope.GetOrCreate(aReg,aServiceType)
    else if fValidateScopes then
      raise EIocScopeError.CreateFmt('Scoped service "%s" resolved outside a scope. Resolve it from a TIocScope ' +
        '(TIocContainer.CreateScope), not from the root container nor as a dependency of a singleton.',[aServiceType.Name])
    else Result := BuildValue(aReg,aServiceType,nil); //legacy (ValidateScopes = False): behaves as transient
  end
  else
  begin
    Result := BuildValue(aReg,aServiceType,aScope);
    //legacy: class registrations kept the last built instance
    if aReg is TIocRegistrationInstance then TIocRegistrationInstance(aReg).Instance := Result.AsObject;
  end;
  {$IFDEF DEBUG_IOC}
  TDebugger.Trace(Self,'Built dependency: %s',[aReg.fIntfInfo.Name]);
  {$ENDIF}
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
begin
  Result := ResolveAll<T>(aName,nil);
end;

function TIocResolver.ResolveAll<T>(const aName : string; aScope : TIocScope) : TList<T>;
var
  pInfo : PTypeInfo;
  regList : TObjectList<TIocRegistration>;
  reg : TIocRegistration;
begin
  Result := TList<T>.Create;
  try
    pInfo := TypeInfo(T);
    if fRegistrator.Dependencies.TryGetValue(fRegistrator.GetKey(pInfo,aName),regList) then
    begin
      //resolve each registration itself: going through Resolve(pInfo, reg.Name) would always
      //find the last registration of the key and return it once per entry
      for reg in regList do Result.Add(ResolveRegistration(reg,pInfo,aScope).AsType<T>);
    end;
  except
    Result.Free;
    raise;
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
begin
  //release in reverse order of creation: dependents go before their dependencies
  fInterfaces.Clear;
  for i := fCreated.Count - 1 downto 0 do fCreated[i] := nil;
  fCreated.Free;
  fInterfaces.Free;
  //class (non-interface) scoped instances are owned by the scope
  fObjects.Clear;
  for i := fCreatedObjects.Count - 1 downto 0 do fCreatedObjects[i].Free;
  fCreatedObjects.Free;
  fObjects.Free;
  inherited;
end;

function TIocScope.GetOrCreate(aReg: TIocRegistration; aServiceType: PTypeInfo): TValue;
var
  cached : IInterface;
  intf : IInterface;
  obj : TObject;
begin
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
  Result := fResolver.Resolve(aServiceType,aName,Self);
end;

function TIocScope.Resolve<T>(const aName: string): T;
begin
  Result := Resolve(TypeInfo(T),aName).AsType<T>;
end;

function TIocScope.ResolveAll<T>(const aName: string): TList<T>;
begin
  Result := fResolver.ResolveAll<T>(aName,Self);
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
  fValue := Default(T);
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
                                       Result := TValue.From<T>(aDelegate()); //invoke the delegate explicitly
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
