{ ***************************************************************************

  Copyright (c) 2016-2018 Kike Pérez

  Unit        : Quick.SysInfo
  Description : System Info functions
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 17/05/2018
  Modified    : 08/09/2018

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

unit Quick.SysInfo;

interface

{$IFDEF FPC}
{$modeSwitch advancedRecords}
{$ENDIF}

uses
  SysUtils,
  {$IFNDEF FPC}
    {$IFDEF ANDROID}
    System.IOUtils,
    Androidapi.Helpers,
    {$ENDIF}
  {$ENDIF}
  Quick.Commons;

type

  TSystemInfo = record
  private
    fAppName : string;
    fAppVersion : string;
    fAppPath : string;
    fHostName : string;
    fUserName : string;
    fOSVersion : string;
    fCPUCores : Integer;
    function GetOSVersion : string;
  public
    procedure GetInfo;
    property AppName : string read fAppName write fAppName;
    property AppVersion : string read fAppVersion write fAppVersion;
    property AppPath : string read fAppPath write fAppPath;
    property HostName : string read fHostName write fHostName;
    property UserName : string read fUserName write fUserName;
    property OsVersion : string read fOSVersion write fOSVersion;
    property CPUCores : Integer read fCPUCores write fCPUCores;
  end;

var
  SystemInfo : TSystemInfo;

implementation

{ TSystemInfo }

procedure TSystemInfo.GetInfo;
begin
  {$IFNDEF NEXTGEN}
  fAppName := ExtractFilenameWithoutExt(ParamStr(0));
  {$ELSE}
  fAppName := JStringToString(SharedActivityContext.getPackageName);
  {$ENDIF}
  fAppVersion := GetAppVersionFullStr;
  {$IFNDEF NEXTGEN}
  fAppPath := ExtractFilePath(ParamStr(0));
  {$ELSE}
  fAppPath := TPath.GetDocumentsPath;
  {$ENDIf}
  fUserName := Trim(GetLoggedUserName);
  fHostName := GetComputerName;
  fOSVersion := GetOSVersion;
  fCPUCores := CPUCount;
end;

function TSystemInfo.GetOSVersion: string;
begin
  Result := {$IFDEF FPC}{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%}{$ELSE}TOSVersion.ToString{$ENDIF};
end;

initialization
  SystemInfo.GetInfo;

end.
