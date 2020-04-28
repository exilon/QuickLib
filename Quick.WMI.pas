{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.WMI
  Description : Common functions
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 04/04/2019
  Modified    : 22/04/2020

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

unit Quick.WMI;

{$i QuickLib.inc}

{$TYPEDADDRESS OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}

interface

uses
  SysUtils,
  ActiveX,
  ComObj,
  Quick.Commons,
  Quick.Collections,
  Quick.Arrays,
  Quick.Value,
  Variants;

type

  IWMIInstance = interface
  ['{4C81A6A6-4A65-46FB-B05B-5898DF51F9B7}']
    function GetProperty(const aPropertyName : string) : TFlexValue;
    function GetProperties(const aProperties : TArray<string>) : IList<TFlexPair>;
    function GetName : string;
  end;

  TWMIInstance = class(TInterfacedObject,IWMIInstance)
  private
    fInstance : string;
    fWMIItem : OleVariant;
  public
    constructor Create(const aInstanceName : string; aWMIItem: OleVariant);
    destructor Destroy; override;
    function GetProperty(const aPropertyName : string) : TFlexValue;
    function GetProperties(const aProperties : TArray<string>) : IList<TFlexPair>;
    function GetName : string;
  end;

  TWMIInstances = TArray<IWMIInstance>;

  IWMIClass = interface
  ['{FAFE26ED-28BC-4591-AE5A-9E4543074B5C}']
    function GetInstance(const aInstance : string) : IWMIInstance;
    function GetInstances(const aInstances : TArray<string>) : TWMIInstances;
  end;

  TWMIClass = class(TInterfacedObject,IWMIClass)
  private
    fClassName : string;
    fWMIService : OleVariant;
    fWMIClassItems : IEnumvariant;
    function GetInstanceName(aWMIClass : OleVariant) : string;
  public
    constructor Create(aWMIService : OleVariant; const aClassName : string; aWMIClassItems : IEnumvariant);
    destructor Destroy; override;
    function GetInstance(const aInstance : string) : IWMIInstance;
    function GetInstances(const aInstances : TArray<string>) : TWMIInstances;
  end;

  IWMICollector = interface
  ['{3FFFF0DC-F533-4FE1-A511-73E76EC6BCC8}']
    function From(const aRoot, aWMIClass : string) : IWMIClass; overload;
    function From(const aHost, aRoot, aWMIClass : string) : IWMIClass; overload;
  end;

  TWMICollector = class(TInterfacedObject,IWMICollector)
  private
    fNamespace : string;
    fWMIService : OleVariant;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetObject(const aObjectName: string) : IDispatch;
    function From(const aRoot, aWMIClass : string) : IWMIClass; overload;
    function From(const aHost, aRoot, aWMIClass : string) : IWMIClass; overload;
  end;

  EWMICollector = class(Exception);

implementation

const
  wbemFlagUseAmendedQualifiers = $00020000;
  wbemFlagReturnImmediately    = $00000010;
  wbemFlagReturnWhenComplete   = $00000000;
  wbemFlagForwardOnly          = $00000020;

{ TWMICollector }

constructor TWMICollector.Create;
begin
  fNamespace := '';
  CoInitialize(nil);
end;

destructor TWMICollector.Destroy;
begin
  fWMIService := Unassigned;
  CoUninitialize;
  inherited;
end;

class function TWMICollector.GetObject(const aObjectName: string) : IDispatch;
var
  chEaten : Integer;
  bindCtx : IBindCtx;
  moniker : IMoniker;
begin
  OleCheck(CreateBindCtx(0, bindCtx));
  OleCheck(MkParseDisplayName(bindCtx, PWideChar(aObjectName), chEaten, moniker));
  OleCheck(Moniker.BindToObject(bindCtx, nil, IDispatch, Result));
end;

function TWMICollector.From(const aRoot, aWMIClass : string) : IWMIClass;
begin
  Result := From('.',aRoot,aWMIClass);
end;

function TWMICollector.From(const aHost, aRoot, aWMIClass : string) : IWMIClass;
var
  colItems : OLEVariant;
  oEnum : IEnumvariant;
begin
  try
    //only connect if namespace is different from previous connection
    if fNamespace <> aHost + '\' + aRoot then
    begin
      fWMIService := GetObject(Format('winmgmts:\\%s\%s',[aHost,aRoot]));
      fNamespace := aHost + '\' + aRoot;
    end;
    colItems := fWMIService.ExecQuery(Format('SELECT * FROM %s',[aWMIClass]),'WQL',wbemFlagForwardOnly and wbemFlagReturnImmediately);
    oEnum := IUnknown(colItems._NewEnum) as IEnumVariant;
    Result := TWMIClass.Create(fWMIService,aWMIClass,oEnum);
    oEnum := nil;
    colItems := Unassigned;
  except
    on E : Exception do raise EWMICollector.CreateFmt('Error getting WMI Class "\\%s\%s\%s": %s',[aHost,aRoot,aWMIClass,e.Message]);
  end;
end;

{ TWMIInstance }

constructor TWMIInstance.Create(const aInstanceName : string; aWMIItem: OleVariant);
begin
  fInstance := aInstanceName;
  fWMIItem := aWMIItem;
end;

destructor TWMIInstance.Destroy;
begin
  fWMIItem := Unassigned;
  inherited;
end;

function TWMIInstance.GetName: string;
begin
  Result := fInstance;
end;

function TWMIInstance.GetProperties(const aProperties : TArray<string>) : IList<TFlexPair>;
var
  prop : string;
  item : OleVariant;
begin
  Result := TxList<TFlexPair>.Create;
  for prop in aProperties do
  begin
    try
      item := fWMIItem.Properties_.Item(prop, 0);
      try
        Result.Add(TFlexPair.Create(prop,item));
      finally
        item := Unassigned;
      end;
    except
      on E : Exception do raise EWMICollector.CreateFmt('Retrieving "%s" (%s)',[prop,e.message]);
    end;
  end;
end;

function TWMIInstance.GetProperty(const aPropertyName: string): TFlexValue;
var
  item : OleVariant;
begin
  item := fWMIItem.Properties_.Item(aPropertyName, 0);
  try
    Result := item;
  finally
    item := Unassigned;
  end;
end;

{ TWMIClass }

constructor TWMIClass.Create(aWMIService : OleVariant; const aClassName : string; aWMIClassItems : IEnumvariant);
begin
  fWMIService := aWMIService;
  fClassName := aClassName;
  fWMIClassItems := aWMIClassItems;
end;

destructor TWMIClass.Destroy;
begin
  fWMIClassItems := nil;
  fWMIService := Unassigned;
  inherited;
end;

function TWMIClass.GetInstance(const aInstance: string): IWMIInstance;
var
  propItem : OLEVariant;
  iValue : LongWord;
  instanceName : string;
begin
  while fWMIClassItems.Next(1, propItem, iValue) = 0 do
  begin
    try
      instanceName := GetInstanceName(propItem);
      if CompareText(aInstance,instanceName) = 0 then
      begin
        Result := TWMIInstance.Create(instanceName,propItem);
        Break;
      end;
    finally
      propItem := Unassigned;
    end;
  end;
end;

function TWMIClass.GetInstanceName(aWMIClass : OleVariant) : string;
var
  qualifiers : OleVariant;
  enumQualif : IEnumVariant;
  qualifItem : OleVariant;
  pceltFetched : Cardinal;
  propItem : OleVariant;
  enumProp : IEnumVariant;
  iValue : Cardinal;
  properties : OleVariant;
  objSWbemObjectSet : OleVariant;
  item : OleVariant;
begin
  Result := '';
  objSWbemObjectSet:= fWMIService.Get(fClassName, wbemFlagUseAmendedQualifiers and wbemFlagReturnWhenComplete);
  properties := objSWbemObjectSet.Properties_;
  enumProp := IUnknown(properties._NewEnum) as IENumVariant;
  while enumProp.Next(1, propItem, iValue) = 0 do
  begin
    qualifiers := propItem.Qualifiers_;
    enumQualif := IUnknown(qualifiers._NewEnum) as IEnumVariant;
    //iterate over the qualifiers
    while enumQualif.Next(1, qualifItem, pceltFetched) = 0 do
    begin
      //check the name of the qualifier
      //Result := rgvarQualif.Name;
      //Result := rgvarQualif.Value;
      if qualifItem.Name = 'key' then
      begin
        item := aWMIClass.Properties_.Item(propItem.Name,0);
        try
          if qualifItem.Value then if Result = '' then Result := item
            else Result := Format('%s %s',[Result,item]);
        finally
          item := Unassigned;
        end;
      end;
      qualifItem := Unassigned;
    end;
    enumQualif := nil;
    qualifiers := Unassigned;
    propItem := Unassigned;
  end;
  enumProp := nil;
  properties := Unassigned;
  objSWbemObjectSet := Unassigned;
end;

function TWMIClass.GetInstances(const aInstances: TArray<string>): TWMIInstances;
var
  propItem : OLEVariant;
  iValue : LongWord;
  getAllInstances : Boolean;
  instanceName : string;
begin
  getAllInstances := (High(aInstances) = 0) and (aInstances[0] = '*');
  while fWMIClassItems.Next(1, propItem, iValue) = 0 do
  begin
    instanceName := GetInstanceName(propItem);
    if (getAllInstances) or (StrInArray(instancename,aInstances)) then
    begin
      Result := Result + [TWMIInstance.Create(instanceName,propItem)];
    end;
    propItem := Unassigned;
  end;
end;

end.
