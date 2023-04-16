{ ***************************************************************************

  Copyright (c) 2016-2022 Kike Pérez

  Unit        : Quick.IoC
  Description : IoC Dependency Injector
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 19/10/2019
  Modified    : 19/01/2022

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
    fDependencies : TDictionary<string,TIocRegistration>;
    fDependencyOrder : TList<TIocRegistration>;
  public
    constructor Create;
    destructor Destroy; override;
    property Dependencies : TDictionary<string,TIocRegistration> read fDependencies write fDependencies;
    property DependencyOrder : TList<TIocRegistration> read fDependencyOrder;
    function IsRegistered<TInterface: IInterface; TImplementation: class>(const aName : string = '') : Boolean; overload;
    function IsRegistered<T>(const aName : string = '') : Boolean; overload;
    function GetKey(aPInfo : PTypeInfo; const aName : string = ''): string;
    function RegisterType<TInterface: IInterface; TImplementation: class>(const aName : string = '') : TIocRegistration<TImplementation>; overload;
    function RegisterType(aTypeInfo : PTypeInfo; aImplementation : TClass; const aName : string = '') : TIocRegistration; overload;
    function RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration; overload;
    function RegisterInstance<T : class>(const aName : string = '') : TIocRegistration<T>; overload;
    function RegisterInstance<TInterface : IInterface>(aInstance : TInterface; const aName : string = '') : TIocRegistration; overload;
    function RegisterOptions<T : TOptions>(aOptions : T) : TIocRegistration<T>;
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

  TIocResolver = class(TInterfacedObject,IIocResolver)
  private
    fRegistrator : TIocRegistrator;
    fInjector : TIocInjector;
    function CreateInstance(aClass : TClass) : TValue;
  public
    constructor Create(aRegistrator : TIocRegistrator; aInjector : TIocInjector);
    function Resolve<T>(const aName : string = ''): T; overload;
    function Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue; overload;
    function ResolveAll<T>(const aName : string = '') : TList<T>;
  end;

  TTypedFactory<T : class, constructor> = class(TVirtualInterface)
  private
    fResolver : TIocResolver;
  public
    constructor Create(PIID: PTypeInfo; aResolver : TIocResolver);
    procedure DoInvoke(Method: TRttiMethod;  const Args: TArray<TValue>; out Result: TValue);
  end;

  IFactory<T> = interface
  ['{92D7AB4F-4C0A-4069-A821-B057E193DE65}']
    function New : T;
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
  end;

  TIocServiceLocator = class
  public
    class function GetService<T> : T;
    class function TryToGetService<T: IInterface>(aService : T) : Boolean;
  end;

  EIocRegisterError = class(Exception);
  EIocResolverError = class(Exception);
  EIocBuildError = class(Exception);

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

constructor TIocRegistration.Create;
begin
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
begin
  Result := fRegistrator.RegisterType<TFactoryInterface,TTypedFactory<TFactoryType>>(aName).DelegateTo(function : TTypedFactory<TFactoryType>
                                                    begin
                                                      Result := TTypedFactory<TFactoryType>.Create(TypeInfo(TFactoryInterface),fResolver);
                                                    end);
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
  Result := fRegistrator.RegisterOptions<T>(aOptions).AsSingleton;
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
  fDependencies := TDictionary<string,TIocRegistration>.Create;
  fDependencyOrder := TList<TIocRegistration>.Create;
end;

destructor TIocRegistrator.Destroy;
var
  i : Integer;
  regs : TArray<TIocRegistration>;
begin
  for i := fDependencyOrder.Count-1 downto 0 do
  begin
    if fDependencyOrder[i] <> nil then
    begin
      //free singleton instances not interfaced
      if (fDependencyOrder[i] is TIocRegistrationInstance) and
          (TIocRegistrationInstance(fDependencyOrder[i]).IsSingleton) then
            TIocRegistrationInstance(fDependencyOrder[i]).Instance.Free;
      fDependencyOrder[i].Free;
    end;
  end;
  fDependencies.Free;
  fDependencyOrder.Free;
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
  reg : TIocRegistration;
begin
  Result := False;
  key := GetKey(TypeInfo(TInterface),aName);
  if fDependencies.TryGetValue(key,reg) then
  begin
    if reg.&Implementation = TImplementation then Result := True;
  end
end;

function TIocRegistrator.IsRegistered<T>(const aName: string): Boolean;
var
  key : string;
  reg : TIocRegistration;
begin
  Result := False;
  key := GetKey(TypeInfo(T),aName);
  if fDependencies.TryGetValue(key,reg) then
  begin
    if reg is TIocRegistrationInterface then Result := True
      else if (reg is TIocRegistrationInstance) {and (TIocRegistrationInterface(reg).Instance <> nil)} then Result := True;
  end
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
begin
  tpinfo := TypeInfo(TInterface);
  key := GetKey(tpinfo,aName);
  if fDependencies.TryGetValue(key,Result) then
  begin
    if Result.&Implementation = tpinfo.TypeData.ClassType then raise EIocRegisterError.Create('Implementation is already registered!');
  end
  else
  begin
    Result := TIocRegistrationInterface.Create(aName);
    Result.IntfInfo := tpinfo;
    TIocRegistrationInterface(Result).Instance := aInstance;
    //reg.Instance := T.Create;
    fDependencies.Add(key,Result);
    fDependencyOrder.Add(Result);
  end;
end;

function TIocRegistrator.RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration;
var
  key : string;
begin
  key := GetKey(aTypeInfo,aName);
  if fDependencies.TryGetValue(key,Result) then
  begin
    if Result.&Implementation = aTypeInfo.TypeData.ClassType then raise EIocRegisterError.Create('Implementation is already registered!');
  end
  else
  begin
    Result := TIocRegistrationInstance.Create(aName);
    Result.IntfInfo := aTypeInfo;
    Result.&Implementation := aTypeInfo.TypeData.ClassType;
    //reg.Instance := T.Create;
    fDependencies.Add(key,Result);
    fDependencyOrder.Add(Result);
  end;
end;

function TIocRegistrator.RegisterOptions<T>(aOptions: T): TIocRegistration<T>;
var
  pInfo : PTypeInfo;
  key : string;
  reg : TIocRegistration;
begin
  pInfo := TypeInfo(IOptions<T>);
  key := GetKey(pInfo,'');
  if fDependencies.TryGetValue(key,reg) then
  begin
    if reg.&Implementation = aOptions.ClassType then raise EIocRegisterError.Create('Implementation for this interface is already registered!');
  end
  else
  begin
    reg := TIocRegistrationInterface.Create('');
    reg.IntfInfo := pInfo;
    reg.&Implementation := aOptions.ClassType;
    TIocRegistrationInterface(reg).Instance := TOptionValue<T>.Create(aOptions);
    fDependencies.Add(key,reg);
    fDependencyOrder.Add(reg);
  end;
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
begin
  key := GetKey(aTypeInfo,aName);
  if fDependencies.TryGetValue(key,Result) then
  begin
    if Result.&Implementation = aImplementation then raise EIocRegisterError.Create('Implementation for this interface is already registered!')
      else Key := key + '#' + TGUID.NewGuid.ToString;
  end;
  Result := TIocRegistrationInterface.Create(aName);
  Result.IntfInfo := aTypeInfo;
  Result.&Implementation := aImplementation;
  fDependencies.Add(key,Result);
  fDependencyOrder.Add(Result);
end;

{ TIocResolver }

constructor TIocResolver.Create(aRegistrator : TIocRegistrator; aInjector : TIocInjector);
begin
  fRegistrator := aRegistrator;
  fInjector := aInjector;
end;

function TIocResolver.CreateInstance(aClass: TClass): TValue;
var
  ctx : TRttiContext;
  rtype : TRttiType;
  rmethod : TRttiMethod;
  rParam : TRttiParameter;
  value : TValue;
  values : TArray<TValue>;
begin
  Result := nil;
  rtype := ctx.GetType(aClass);
  if rtype = nil then Exit;
  for rmethod in TRttiInstanceType(rtype).GetMethods do
  begin
    if rmethod.IsConstructor then
    begin
      //if create don't have parameters
      if Length(rmethod.GetParameters) = 0 then
      begin
        Result := rmethod.Invoke(TRttiInstanceType(rtype).MetaclassType,[]);
        Break;
      end
      else
      begin
        for rParam in rmethod.GetParameters do
        begin
          value := Resolve(rParam.ParamType.Handle);
          values := values + [value];
        end;
        Result := rmethod.Invoke(TRttiInstanceType(rtype).MetaclassType,values);
        Break;
      end;
    end;
  end;
end;

function TIocResolver.Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue;
var
  key : string;
  reg : TIocRegistration;
  intf : IInterface;
begin
  Result := nil;
  reg := nil;
  key := fRegistrator.GetKey(aServiceType,aName);
  {$IFDEF DEBUG_IOC}
  TDebugger.Trace(Self,'Resolving dependency: %s',[key]);
  {$ENDIF}
  if not fRegistrator.Dependencies.TryGetValue(key,reg) then raise EIocResolverError.CreateFmt('Type "%s" not registered for IOC!',[aServiceType.Name]);
  //if is singleton return already instance if exists
  if reg.IsSingleton then
  begin
    if reg is TIocRegistrationInterface then
    begin
      if TIocRegistrationInterface(reg).Instance <> nil then
      begin
        if TIocRegistrationInterface(reg).Instance.QueryInterface(GetTypeData(aServiceType).Guid,intf) <> 0 then raise EIocResolverError.CreateFmt('Implementation for "%s" not registered!',[aServiceType.Name]);
        TValue.Make(@intf,aServiceType,Result);
        {$IFDEF DEBUG_IOC}
        TDebugger.Trace(Self,'Resolved dependency: %s',[reg.fIntfInfo.Name]);
        {$ENDIF}
        Exit;
      end;
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
    end;
  end;
  //instance not created yet
  if reg.&Implementation = nil then raise EIocResolverError.CreateFmt('Implemention for "%s" not defined!',[aServiceType.Name]);
  //use activator if assigned
  if reg is TIocRegistrationInterface then
  begin
    {$IFDEF DEBUG_IOC}
    TDebugger.Trace(Self,'Building dependency: %s',[reg.fIntfInfo.Name]);
    {$ENDIF}
    if Assigned(reg.ActivatorDelegate) then TIocRegistrationInterface(reg).Instance := reg.ActivatorDelegate().AsInterface
      else TIocRegistrationInterface(reg).Instance := CreateInstance(reg.&Implementation).AsInterface;
    if (TIocRegistrationInterface(reg).Instance = nil) or (TIocRegistrationInterface(reg).Instance.QueryInterface(GetTypeData(aServiceType).Guid,intf) <> 0) then raise EIocResolverError.CreateFmt('Implementation for "%s" not registered!',[aServiceType.Name]);
    TValue.Make(@intf,aServiceType,Result);
  end
  else
  begin
    {$IFDEF DEBUG_IOC}
    TDebugger.Trace(Self,'Building dependency: %s',[reg.fIntfInfo.Name]);
    {$ENDIF}
    if Assigned(reg.ActivatorDelegate) then TIocRegistrationInstance(reg).Instance := reg.ActivatorDelegate().AsObject
    else
    begin
      TIocRegistrationInstance(reg).Instance := CreateInstance(reg.&Implementation).AsObject;
    end;
    Result := TIocRegistrationInstance(reg).Instance;
  end;
  {$IFDEF DEBUG_IOC}
  TDebugger.Trace(Self,'Built dependency: %s',[reg.fIntfInfo.Name]);
  {$ENDIF}
end;

function TIocResolver.Resolve<T>(const aName : string = ''): T;
var
  pInfo : PTypeInfo;
begin
  Result := Default(T);
  pInfo := TypeInfo(T);

  Result := Resolve(pInfo,aName).AsType<T>;
end;

function TIocResolver.ResolveAll<T>(const aName : string = '') : TList<T>;
var
  pInfo : PTypeInfo;
  reg : TIocRegistration;
begin
  Result := TList<T>.Create;
  pInfo := TypeInfo(T);

  for reg in fRegistrator.fDependencyOrder do
  begin
    if reg.IntfInfo = pInfo then Self.Resolve(pInfo,aName);
  end;
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
                                       Result := TValue.From<T>(aDelegate);
                                     end;
end;

{ TTypedFactory<T> }

constructor TTypedFactory<T>.Create(PIID: PTypeInfo; aResolver : TIocResolver);
begin
  inherited Create(PIID, DoInvoke);
  fResolver := aResolver;
end;

procedure TTypedFactory<T>.DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
begin
  if CompareText(Method.Name,'New') <> 0 then raise Exception.Create('TTypedFactory needs a method "New"');
  Result := fResolver.CreateInstance(TClass(T)).AsType<T>;
end;

{ TIocServiceLocator }

class function TIocServiceLocator.GetService<T> : T;
begin
  Result := GlobalContainer.Resolve<T>;
end;

class function TIocServiceLocator.TryToGetService<T>(aService : T) : Boolean;
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

end.
