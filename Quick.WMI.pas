{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.WMI
  Description : Common functions
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 04/04/2019
  Modified    : 08/04/2019

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
  WbemScripting_TLB,
  ActiveX,
  ComObj,
  Quick.Arrays,
  Quick.Value,
  Variants;

type

  TFlexArray = TArray<TFlexValue>;

  TWMIObject = record
  private
    fInstance : string;
    fProperty : string;
  end;

  TWMICollector = class
  private
    class function GetObject(const aObjectName: string) : IDispatch;
  public
    class function GetProperty(const aWMIHost, aRoot, aWMIClass, aWMIProperty : string) : TFlexValue;
    class function GetPropertyInstances(const aWMIHost, aRoot, aWMIClass,aWMIProperty : string; const aInstances : string = '*') : TFlexPairArray;
  end;

  TWMIInstance = class
  private
    fInstance : string;
  public
    property Instance : 
  end;

  TWMIObject2 = class
  public
    function FromInstance(const aInstanceName : string) : TWMIInstance;
  end;

  EWMICollector = class(Exception);

implementation

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

class function TWMICollector.GetProperty(const aWMIHost, aRoot, aWMIClass, aWMIProperty: string) : TFlexValue;
var
  objWMIService : OLEVariant;
  colItems : OLEVariant;
  colItem : OLEVariant;
  oEnum : IEnumvariant;
  iValue : LongWord;
begin
  CoInitialize(nil);
  try
    try
      objWMIService := GetObject(Format('winmgmts:\\%s\%s',[aWMIHost,aRoot]));
      colItems := objWMIService.ExecQuery(Format('SELECT * FROM %s',[aWMIClass]),'WQL',0);
      oEnum := IUnknown(colItems._NewEnum) as IEnumVariant;
      while oEnum.Next(1, colItem, iValue) = 0 do
      begin
        Result := colItem.Properties_.Item(aWMIProperty, 0);
        Break;
      end;
    except
      on E : Exception do raise EWMICollector.CreateFmt('Error getting WMI property: %s',[e.Message]);
    end;
  finally
    CoUninitialize;
  end;
end;

class function TWMICollector.GetPropertyInstances(const aWMIHost, aRoot, aWMIClass,aWMIProperty : string; const aInstances : string = '*') : TFlexPairArray;
var
  objWMIService : OLEVariant;
  colItems : OLEVariant;
  colItem : OLEVariant;
  oEnum : IEnumvariant;
  iValue : LongWord;
begin
  CoInitialize(nil);
  try
    try
      objWMIService := GetObject(Format('winmgmts:\\%s\%s',[aWMIHost,aRoot]));
      colItems := objWMIService.ExecQuery(Format('SELECT * FROM %s',[aWMIClass]),'WQL',0);
      oEnum := IUnknown(colItems._NewEnum) as IEnumVariant;
      while oEnum.Next(1, colItem, iValue) = 0 do
      begin
        if (aInstances = '*') or (CompareText(aInstances,colItem.Name) = 0)  then
        begin
          Result.Add(colItem.Name,colItem.Properties_.Item(aWMIProperty, 0));
        end;
      end;
    except
      on E : Exception do raise EWMICollector.CreateFmt('Error getting WMI property: %s',[e.Message]);
    end;
  finally
    CoUninitialize;
  end;
end;

end.
