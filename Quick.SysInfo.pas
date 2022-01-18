{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.SysInfo
  Description : System Info functions
  Author      : Kike Pérez
  Version     : 1.2
  Created     : 17/05/2018
  Modified    : 05/12/2019

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

{$i QuickLib.inc}

interface

{$IFDEF FPC}
{$modeSwitch advancedRecords}
{$ENDIF}

uses
  SysUtils,
  Types,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFNDEF FPC}
    {$IFDEF NEXTGEN}
    System.IOUtils,
      {$IFDEF ANDROID}
      Androidapi.Helpers,
        {$IFDEF DELPHIRX103_UP}
        Androidapi.JNI.GraphicsContentViewText,
        Androidapi.JNI.JavaTypes,
        Androidapi.JNI.App,
        {$ENDIF}
      {$ENDIF}
      {$IFDEF IOS}
      Macapi.CoreFoundation,
      iOSApi.Foundation,
      {$ENDIF}
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
    fProcessId : DWORD;
    function GetOSVersion : string;
  public
    procedure GetInfo;
    property AppName : string read fAppName;
    property AppVersion : string read fAppVersion;
    property AppPath : string read fAppPath;
    property HostName : string read fHostName;
    property UserName : string read fUserName;
    property OsVersion : string read fOSVersion;
    property CPUCores : Integer read fCPUCores;
    property ProcessId : DWORD read fProcessId;
  end;

var
  SystemInfo : TSystemInfo;

implementation

{ TSystemInfo }

procedure TSystemInfo.GetInfo;
begin
  {$IFNDEF NEXTGEN}
  if IsLibrary then fAppName := ExtractFileNameWithoutExt(GetModuleName(0))
    else fAppName := ExtractFilenameWithoutExt(ParamStr(0));
  {$ELSE}
    {$IFDEF ANDROID}
      {$IFDEF DELPHIRX103_UP}
      fAppName := JStringToString(TAndroidHelper.Context.getPackageName);
      {$ELSE}
      fAppName := JStringToString(SharedActivityContext.getPackageName);
      {$ENDIF}
    {$ELSE}
    fAppName := TNSString.Wrap(CFBundleGetValueForInfoDictionaryKey(CFBundleGetMainBundle, kCFBundleIdentifierKey)).UTF8String;
    {$ENDIF}
  {$ENDIF}
  fAppVersion := GetAppVersionFullStr;
  {$IFNDEF NEXTGEN}
  if IsLibrary then fAppPath := ExtractFilePath(GetModuleName(0))
    else fAppPath := ExtractFilePath(ParamStr(0));
  {$ELSE}
  fAppPath := TPath.GetDocumentsPath;
  {$ENDIf}
    {$IFDEF DELPHILINUX}
    fUserName := GetLoggedUserName;
    {$ELSE}
    fUserName := Trim(GetLoggedUserName);
    {$ENDIF}
  fHostName := GetComputerName;
  fOSVersion := GetOSVersion;
  fCPUCores := CPUCount;
  {$IFDEF MSWINDOWS}
  fProcessId := GetCurrentProcessID;
  {$ELSE}

  {$ENDIF}
end;

function TSystemInfo.GetOSVersion: string;
begin
  Result := {$IFDEF FPC}
              {$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%}
            {$ELSE}
            TOSVersion.ToString
            {$ENDIF};
end;

initialization
  try
    SystemInfo.GetInfo;
  except
    {$IFDEF MSWINDOWS}
    on E : Exception do
    begin
      if (not IsLibrary) and (not IsService) then raise Exception.CreateFmt('Error getting SystemInfo: %s',[e.Message]);
    end;
    {$ENDIF}
  end;

end.
