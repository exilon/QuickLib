{ ***************************************************************************

  Copyright (c) 2016-2022 Kike Pérez

  Unit        : Quick.Log
  Description : Threadsafe Log
  Author      : Kike Pérez
  Version     : 1.19
  Created     : 10/04/2016
  Modified    : 10/02/2022

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

unit Quick.Log;

{$i QuickLib.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes,
  Quick.Commons,
  {$IFNDEF FPC}
  System.IOUtils,
    {$IFDEF DELPHILINUX}
    Quick.SyncObjs.Linux.Compatibility,
    {$ENDIF}
  {$ELSE}
  Quick.Files,
  {$ENDIF}
  {$IF Defined(NEXTGEN) OR Defined(LINUX) or Defined(MACOS)}
  syncObjs,
  Posix.Unistd,
  {$ENDIF}
  SysUtils;

type

  TMemoryLog = class
  private
    fLines : TStrings;
    fEnabled : Boolean;
    function GetLogText : string;
  public
    constructor Create;
    destructor Destroy; override;
    property Lines : TStrings read fLines write fLines;
    property Text : string read GetLogText;
    property Enabled : Boolean read fEnabled write fEnabled;
  end;

  TQuickLog = class
  private
    fLogFileName : string;
    fCurrentDateToFileName : Boolean;
    fHideHour : Boolean;
    fFMTName : string;
    fVerbose : TLogVerbose;
    fLimitLogSize : Int64;
    fCurrentLogSize : Int64;
    fShowEventTypes : Boolean;
    fShowHeaderInfo : Boolean;
    fCheckFileInUse : Boolean;
    fMemoryLog : TMemoryLog;
    function GetLogFileName : string;
    procedure WriteLog(cMsg : string);
  public
    property Verbose : TLogVerbose read fVerbose write fVerbose;
    property LogFileName : string read GetLogFileName;
    property HideHour : Boolean read fHideHour write fHideHour;
    property ShowEventType : Boolean read fShowEventTypes write fShowEventTypes;
    property ShowHeaderInfo : Boolean read fShowHeaderInfo write fShowHeaderInfo;
    property CheckFileInUse : Boolean read fCheckFileInUse write fCheckFileInUse;
    property MemoryLog : TMemoryLog read fMemoryLog write fMemoryLog;
    constructor Create;
    destructor Destroy; override;
    function SetLog(logname : string; AddCurrentDateToFileName : Boolean; LimitSizeInMB : Integer = 0) : Boolean;
    procedure Add(const cMsg : string; cEventType : TLogEventType = TLogEventType.etInfo); overload;
    procedure Add(const cFormat : string; cParams : array of TVarRec ; cEventType : TLogEventType = TLogEventType.etInfo); overload;
  end;

var
  {$IF Defined(MACOS) OR Defined(ANDROID)}
  CS : TCriticalSection;
  {$ELSE}
  CS : TRTLCriticalSection;
  {$ENDIF}
  Log : TQuickLog;

implementation


constructor TMemoryLog.Create;
begin
  inherited;
  fLines := TStringList.Create;
  fEnabled := False;
end;

destructor TMemoryLog.Destroy;
begin
  if Assigned(fLines) then fLines.Free;
  inherited;
end;

function TMemoryLog.GetLogText : string;
begin
  Result := fLines.Text;
end;


constructor TQuickLog.Create;
begin
  inherited;
  fVerbose := LOG_ALL;
  fLimitLogSize := 0;
  fShowEventTypes := True;
  fShowHeaderInfo := True;
  fCheckFileInUse := False;
  fMemoryLog := TMemoryLog.Create;
end;

destructor TQuickLog.Destroy;
begin
  if Assigned(fMemoryLog) then fMemoryLog.Free;
  inherited;
end;


{Returns date + time in english format (to add to filename}
function TQuickLog.GetLogFileName : string;
begin
  Result := '';
  if fCurrentDateToFileName then
  begin
    try
      //Checks if needs to show time or not
      if HideHour then Result := FormatDateTime('yyyymmdd', Now())
        else Result := FormatDateTime('yyyymmdd_hhmm', Now());
      Result := Format(fFMTName,[Result]);
    except
      Result := 'Error';
    end;
  end
  else Result := fLogFileName;
end;

{$IFDEF FPC}
function OSVersion: String;
begin
  Result:={$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%};
end;
{$ENDIF}

function TQuickLog.SetLog(logname : string; AddCurrentDateToFileName : Boolean; LimitSizeInMB : Integer = 0) : Boolean;
begin
  if logname = '' then logname := TPath.GetDirectoryName(ParamStr(0)) + PathDelim + TPath.GetFileNameWithoutExtension(ParamStr(0)) + '.log';
  fFMTName := ExtractFilePath(logname) + ExtractFileNameWithoutExt(logname) + '_%s' + ExtractFileExt(logname);
  fHideHour := True;
  fCurrentDateToFileName := AddCurrentDateToFileName;
  fLogFileName := logname;
  //Checks if logfile is too big and deletes
  fLimitLogSize := LimitSizeInMB * 1024 * 1024;
  if (fLimitLogSize > 0) and (TFile.Exists(logname)) then
  begin
    try
      fCurrentLogSize := TFile.GetSize(logname);
      if fCurrentLogSize > fLimitLogSize then if DeleteFile(logname) then fCurrentLogSize := 0;
    except
      raise Exception.Create('can''t access to log file!');
    end;
  end;
  //Checks if it's in use
  {$IF NOT Defined(MACOS) AND NOT Defined(ANDROID)}
  if (fCheckFileInUse) and (TFile.IsInUse(logname)) Then fLogFileName := Format('%s_(1).%s',[ExtractFileNameWithoutExt(logname),ExtractFileExt(logname)]);
  {$ENDIF}

  //writes header info
  if fShowHeaderInfo then
  begin
    try
      Self.WriteLog(FillStr('-',70));
      Self.WriteLog(Format('Application : %s %s',[ExtractFilenameWithoutExt(ParamStr(0)),GetAppVersionFullStr]));
      Self.WriteLog(Format('Path        : %s',[ExtractFilePath(ParamStr(0))]));
      Self.WriteLog(Format('CPU cores   : %d',[CPUCount]));
      Self.WriteLog(Format('OS version  : %s',{$IFDEF FPC}[OSVersion]{$ELSE}[TOSVersion.ToString]{$ENDIF}));
      Self.WriteLog(Format('Host        : %s',[GetComputerName]));
      Self.WriteLog(Format('Username    : %s',[Trim(GetLoggedUserName)]));
      Self.WriteLog(Format('Started     : %s',[NowStr]));
      {$IFDEF MSWINDOWS}
      if IsService then Self.WriteLog('AppType     : Service')
        else if System.IsConsole then Self.WriteLog('AppType     : Console');
      {$ENDIF}
      if IsDebug then Self.WriteLog('Debug mode  : On');
      Self.WriteLog(FillStr('-',70));
    except
      on E : Exception do Self.WriteLog('Can''t get info: ' + e.message);
    end;
  end;
  Result := True;
end;

procedure TQuickLog.Add(const cMsg : string; cEventType : TLogEventType = TLogEventType.etInfo);
begin
  //Check Log Verbose level
  if cEventType in fVerbose then
  begin
  if fShowEventTypes then Self.WriteLog(Format('%s [%s] %s',[DateTimeToStr(Now()),EventStr[Integer(cEventType)],cMsg]))
    else Self.WriteLog(Format('%s %s',[DateTimeToStr(Now()),cMsg]));
  end;
end;

procedure TQuickLog.WriteLog(cMsg : string);
var
  stream: TFileStream;
  logname : string;
  LBuffer      : TBytes;
  LByteOrderMark: TBytes;
  LOffset      : Integer;
  LEncoding, DestEncoding: TEncoding;
begin
  //Checks if need to add date to filename
  logname := GetLogFileName;
  {$IF Defined(MACOS) OR Defined(ANDROID)}
  CS.Enter;
  {$ELSE}
  EnterCriticalSection(CS);
  {$ENDIF}
  try
    cMsg := cMsg + #13#10;
    LEncoding:= TEncoding.Unicode;
    DestEncoding := TEncoding.Unicode;
    SetLength(LBuffer, length(cMsg) * sizeof(char));
    if cMsg <> '' then Move(cMsg[1], lbuffer[0], Length(lbuffer));
    LOffset := TEncoding.GetBufferEncoding(LBuffer, LEncoding);
    LBuffer := LEncoding.Convert(LEncoding, DestEncoding, LBuffer,
                                  LOffset, Length(LBuffer) - LOffset);

    if TFile.Exists(logname) then
    begin
      stream := TFileStream.Create(logname, fmOpenReadWrite or fmShareDenyWrite);
      stream.Position := stream.Size;
    end
    else
    begin
      stream := TFileStream.Create(logname, fmCreate or fmShareDenyWrite);
      LByteOrderMark := DestEncoding.GetPreamble;
      stream.Write(LByteOrderMark[0], Length(LByteOrderMark));
    end;

    with stream do
    begin
      try
        Write(LBuffer[0], Length(LBuffer));
        fCurrentLogSize := fCurrentLogSize + Length(LBuffer);
      finally
        Free;
      end;
    end;

    //writes internal log
    if fMemoryLog.Enabled then
    begin
      fMemoryLog.Lines.Add(cMsg);
    end;

    //check if limits max size
    try
      if (fLimitLogSize > 0) and (fCurrentLogSize > fLimitLogSize) then if DeleteFile(logname) then fCurrentLogSize := 0;
    except
      //
    end;
  finally
    {$IF Defined(MACOS) OR Defined(ANDROID)}
    CS.Leave;
    {$ELSE}
    LeaveCriticalSection(CS);
    {$ENDIF}
  end;
end;

procedure TQuickLog.Add(const cFormat : string; cParams : array of TVarRec ; cEventType : TLogEventType = TLogEventType.etInfo);
begin
  Self.Add(Format(cFormat,cParams),cEventType);
end;

initialization
{$IF DEFINED(FPC) AND DEFINED(LINUX)}
InitCriticalSection(CS);
{$ELSE}
  {$IF Defined(MACOS) OR Defined(ANDROID)}
  CS := TCriticalSection.Create;
  {$ELSE}
    {$IFDEF DELPHILINUX}
    CS := TRTLCriticalSection.Create;
    {$ELSE}
    InitializeCriticalSection(CS);
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

finalization
{$IF DEFINED(FPC) AND DEFINED(LINUX)}
DoneCriticalsection(CS);
{$ELSE}
  {$IF Defined(MACOS) OR Defined(ANDROID) OR Defined(DELPHILINUX)}
  CS.Free;
  {$ELSE}
  DeleteCriticalSection(CS);
  {$ENDIF}
{$ENDIF}

end.
