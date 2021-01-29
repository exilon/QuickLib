{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.Registry
  Description : Util registry info
  Author      : Kike Pérez
  Version     : 2.0
  Created     : 22/01/2021
  Modified    : 25/01/2021

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

unit Quick.Registry;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  Winapi.Windows,
  System.Win.Registry;

type
  TRegRootKey = (rootCU, rootLM);

  TRegistryUtils = class
  public
    class function GetNewReg(aRootKey : TRegRootKey; aReadOnly : Boolean = False) : TRegistry;
    class function GetUniqueMachineId: TGUID; static;
    class function IsDarkMode : Boolean;
  end;

implementation


class function TRegistryUtils.GetNewReg(aRootKey : TRegRootKey; aReadOnly : Boolean = False) : TRegistry;
begin
  if aReadOnly then Result := TRegistry.Create(KEY_READ)
    else Result := TRegistry.Create(KEY_ALL_ACCESS);
  if aRootKey = TRegRootKey.rootCU then Result.RootKey := HKEY_CURRENT_USER
    else Result.RootKey := HKEY_LOCAL_MACHINE;
end;

class function TRegistryUtils.IsDarkMode : Boolean;
var
  reg : TRegistry;
begin
  reg := GetNewReg(TRegRootKey.rootCU,True);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize');
    Result := not reg.ReadBool('AppsUseLightTheme');
  finally
    reg.Free;
  end;
end;

class function TRegistryUtils.GetUniqueMachineId : TGUID;
var
  reg : TRegistry;
begin
  reg := GetNewReg(TRegRootKey.rootLM,True);
  try
    reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Cryptography');
    Result := StringToGUID(reg.ReadString('MachineGuid'));
  finally
    reg.Free;
  end;
end;

end.
