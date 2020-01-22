{ ***************************************************************************

  Copyright (c) 2016-2020 Kike P�rez

  Unit        : Quick.IoC
  Description : IoC Dependency Injector
  Author      : Kike P�rez
  Version     : 1.0
  Created     : 19/10/2019
  Modified    : 11/01/2020

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
    fRegisterMode : TRegisterMode;
    fIntfInfo : PTypeInfo;
    fImplementation : TClass;
    fActivatorDelegate : TActivatorDelegate<TValue>;
  public
    constructor Create;
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
  public
    constructor Create;
    destructor Destroy; override;
    property Dependencies : TDictionary<string,TIocRegistration> read fDependencies write fDependencies;
    function IsRegistered<TInterface: IInterface; TImplementation: class>(const aName : string = '') : Boolean;
    function GetKey(aPInfo : PTypeInfo; const aName : string = ''): string;
    function RegisterType<TInterface: IInterface; TImplementation: class>(const aName : string = '') : TIocRegistration<TImplementation>; overload;
    function RegisterType(aTypeInfo : PTypeInfo; aImplementation : TClass; const aName : string = '') : TIocRegistration; overload;
    function RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration; overload;
    function RegisterInstance<T : class>(const aName : string = '') : TIocRegistration<T>; overload;
    function RegisterOptions<T : TOptions>(aOptions : T) : TIocRegistration<T>;
  end;

  IIocContainer = interface
  ['{6A486E3C-C5E8-4BE5-8382-7B9BCCFC1BC3}']
    function RegisterType(aInterface: PTypeInfo; aImplementation : TClass; const aName : string = '') : TIocRegistration;
    function RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration;
    function Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue;
    //procedure Build;
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
  end;

  TIocContainer = class(TInterfacedObject,IIocContainer)
  private
    fRegistrator : TIocRegistrator;
    fResolver : TIocResolver;
    fInjector : TIocInjector;
    fLogger : ILogger;
    function InterfaceTypeInfo(const AGUID : TGUID) : PTypeInfo;
  class var
    GlobalInstance: TIocContainer;
  protected
    class constructor Create;
    class destructor Destroy;
  public
    constructor Create;
    destructor Destroy; override;
    function IsRegistered<TInterface: IInterface; TImplementation: class>(const aName: string): Boolean;
    function RegisterType<TInterface: IInterface; TImplementation: class>(const aName : string = '') : TIocRegistration<TImplementation>; overload;
    function RegisterType(aInterface: PTypeInfo; aImplementation : TClass; const aName : string = '') : TIocRegistration; overload;
    function RegisterInstance<T : class>(const aName: string = ''): TIocRegistration<T>; overload;
    function RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration; overload;
    function RegisterOptions<T : TOptions>(aOptions : TOptions) : TIocRegistration<T>;
    function Resolve<T>(const aName : string = ''): T; overload;
    function Resolve(aServiceType: PTypeInfo; const aName : string = ''): TValue; overload;
    function AbstractFactory<T : class, constructor>(aClass : TClass) : T;
  end;

  EIocRegisterError = class(Exception);
  EIocResolverError = class(Exception);

  //singleton global instance
  function GlobalContainer: TIocContainer;

implementation

function GlobalContainer: TIocContainer;
begin
  Result := TIocContainer.GlobalInstance;
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
  inherited;
end;

function TIocContainer.InterfaceTypeInfo(const AGUID : TGUID) : PTypeInfo;
var
  ctx : TRttiContext;
  rtype : TRttiType;
  rtypei : TRttiInterfaceType;
begin
  ctx := TRttiContext.Create;
  try
    for rtype in ctx.GetTypes do
      begin
        if rtype.TypeKind = TTypeKind.tkInterface then
        begin
          rtypei := (rtype as TRttiInterfaceType);
          if IsEqualGUID(rtypei.GUID,AGUID) then Exit(rtypei.Handle);
        end;
      end;
  finally
    ctx.Free;
  end;
  Result := nil;
end;

function TIocContainer.IsRegistered<TInterface, TImplementation>(const aName: string): Boolean;
begin
  Result := fRegistrator.IsRegistered<TInterface,TImplementation>(aName);
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

function TIocContainer.RegisterInstance(aTypeInfo : PTypeInfo; const aName : string = '') : TIocRegistration;
begin
  Result := fRegistrator.RegisterInstance(aTypeInfo,aName);
end;

function TIocContainer.RegisterOptions<T>(aOptions: TOptions): TIocRegistration<T>;
begin
  Result := fRegistrator.RegisterOptions<T>(aOptions).AsSingleton;
end;

function TIocContainer.Resolve(aServiceType: PTypeInfo; const aName: string): TValue;
begin
  Result := fResolver.Resolve(aServiceType,aName);
end;

function TIocContainer.Resolve<T>(const aName : string = ''): T;
begin
  Result := fResolver.Resolve<T>(aName);
end;

{ TIocRegistrator }

constructor TIocRegistrator.Create;
begin
  fDependencies := TDictionary<string,TIocRegistration>.Create;
end;

destructor TIocRegistrator.Destroy;
var
  reg : TIocRegistration;
begin
  for reg in fDependencies.Values do
  begin
    if reg <> nil then
    begin
      //free singleton instances not interfaced
      if (reg is TIocRegistrationInstance) and (TIocRegistrationInstance(reg).IsSingleton) then TIocRegistrationInstance(reg).Instance.Free;
      reg.Free;
    end;
  end;
  fDependencies.Free;
  inherited;
end;

function TIocRegistrator.GetKey(aPInfo : PTypeInfo; const aName : string = ''): string;
begin
  {$IFDEF NEXTGEN}
  Result := aPInfo.Name.ToString;
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

function TIocRegistrator.RegisterInstance<T>(const aName: string): TIocRegistration<T>;
var
  reg : TIocRegistration;
begin
  reg := RegisterInstance(TypeInfo(T),aName);
  Result := TIocRegistration<T>.Create(reg);
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
    Result := TIocRegistrationInstance.Create;
    Result.IntfInfo := aTypeInfo;
    Result.&Implementation := aTypeInfo.TypeData.ClassType;
    //reg.Instance := T.Create;
    fDependencies.Add(key,Result);
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
    reg := TIocRegistrationInterface.Create;
    reg.IntfInfo := pInfo;
    reg.&Implementation := aOptions.ClassType;
    TIocRegistrationInterface(reg).Instance := TOptionValue<T>.Create(aOptions);
    fDependencies.Add(key,reg);
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
    if Result.&Implementation = aImplementation then raise EIocRegisterError.Create('Implementation for this interface is already registered!');
  end
  else
  begin
    Result := TIocRegistrationInterface.Create;
    Result.IntfInfo := aTypeInfo;
    Result.&Implementation := aImplementation;
    fDependencies.Add(key,Result);
  end;
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
  ctx := TRttiContext.Create;
  try
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
  finally
    ctx.Free;
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
  if not fRegistrator.Dependencies.TryGetValue(key,reg) then raise EIocResolverError.CreateFmt('Type "%s" not register for IOC!',[aServiceType.Name]);
  //if is singleton return already instance if exists
  if reg.IsSingleton then
  begin
    if reg is TIocRegistrationInterface then
    begin
      if TIocRegistrationInterface(reg).Instance <> nil then
      begin
        if TIocRegistrationInterface(reg).Instance.QueryInterface(GetTypeData(aServiceType).Guid,intf) <> 0 then raise EIocResolverError.CreateFmt('Implementation for "%s" not registered!',[aServiceType.Name]);
        TValue.Make(@intf,aServiceType,Result);
        Exit;
      end;
    end
    else
    begin
      if TIocRegistrationInstance(reg).Instance <> nil then
      begin
        Result := TIocRegistrationInstance(reg).Instance;
        Exit;
      end;
    end;
  end;
  //instance not created yet
  if reg.&Implementation = nil then raise EIocResolverError.CreateFmt('Implemention for "%s" not defined!',[aServiceType.Name]);
  //use activator if assigned
  if reg is TIocRegistrationInterface then
  begin
    if Assigned(reg.ActivatorDelegate) then TIocRegistrationInterface(reg).Instance := reg.ActivatorDelegate().AsInterface
      else TIocRegistrationInterface(reg).Instance := CreateInstance(reg.&Implementation).AsInterface;
    if (TIocRegistrationInterface(reg).Instance = nil) or (TIocRegistrationInterface(reg).Instance.QueryInterface(GetTypeData(aServiceType).Guid,intf) <> 0) then raise EIocResolverError.CreateFmt('Implementation for "%s" not registered!',[aServiceType.Name]);
    TValue.Make(@intf,aServiceType,Result);
  end
  else
  begin
    if Assigned(reg.ActivatorDelegate) then TIocRegistrationInstance(reg).Instance := reg.ActivatorDelegate().AsObject
    else
    begin
      TIocRegistrationInstance(reg).Instance := CreateInstance(reg.&Implementation).AsObject;
    end;
    Result := TIocRegistrationInstance(reg).Instance;
  end;
end;

function TIocResolver.Resolve<T>(const aName : string = ''): T;
var
  pInfo : PTypeInfo;
begin
  Result := Default(T);
  pInfo := TypeInfo(T);

  Result := Resolve(pInfo,aName).AsType<T>;
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

end.
