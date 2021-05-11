{ ***************************************************************************

  Copyright (c) 2015-2021 Kike Pérez

  Unit        : Quick.Chrono
  Description : Chronometers time elapsed and estimated time to do a task
  Author      : Kike Pérez
  Version     : 1.5
  Created     : 27/08/2015
  Modified    : 06/05/2021

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

unit Quick.Chrono;

interface

{$IFDEF DELPHIXE7_UP}
  {$HPPEMIT LEGACYHPP}
{$ENDIF}

{$i QuickLib.inc}

uses
  Classes,
  {$IF defined(MSWINDOWS)}
  Windows,
  {$ELSEIF defined(MACOS)}
  Macapi.Mach,
  {$ELSEIF defined(POSIX)}
  Posix.Time,
  {$ENDIF}
  {$IFDEF FPC}
    {$IFDEF LINUX}
    unixtype, linux,
    {$ENDIF}
  {$ELSE}
  System.TimeSpan,
  {$ENDIF}
  SysUtils,
  DateUtils;


resourcestring
  strDAY = 'day';
  strHOUR = 'hour';
  strMINUTE = 'minute';
  strSECOND = 'second';
  strMILLISECOND = 'millisecond';
  strMICROSECOND = 'microsecond';
  strNANOSECOND = 'nanosecond';
  strFMTSHORT_HOURS_MINUTES = 'hh:nn:ss';
  strFMTSHORT_MINUTES_SECONDS = 'hh:nn:ss';
  strFMTLONG_HOURS_MINUTES = 'h "hour(s) and" n "minute(s)"';
  strFMTLONG_MINUTES_SECONDS = 'n "minute(s) and" s "second(s)"';

type

  TTimeValue = (utDay, utHour, utMinute, utSecond, utMillisecond,utMicrosecond,utNanosecond);
  TTimeFmt = (tfHoursAndMinutes, tfMinutesAndSeconds);
  TPrecissionFormat = (pfFloat, pfRound, pfTruncate);

const
  UnitShortTime : array[utDay..utNanosecond] of string = ('d','h','m','s','ms','μs','ns');
  UnitLongTime : array[utDay..utNanosecond] of string = (strDAY,strHOUR,strMINUTE,strSECOND,strMILLISECOND,strMICROSECOND,strNANOSECOND);
  FmtShortTime : array[tfHoursAndMinutes..tfMinutesAndSeconds] of string = (strFMTSHORT_HOURS_MINUTES,strFMTSHORT_MINUTES_SECONDS);
  FmtLongTime : array[tfHoursAndMinutes..tfMinutesAndSeconds] of string = (strFMTLONG_HOURS_MINUTES,strFMTLONG_MINUTES_SECONDS);

  {$IFDEF FPC}
  SecsPerHour = 3600;
  {$ENDIF}

type

  IChronometer = interface
  ['{F742C1AD-69DF-4EAA-AB0D-6E571C887901}']
    function GetIsRunning : Boolean;
    function GetElapsedTicks: Int64;
    function GetElapsedMilliseconds: Int64;
    function GetElapsedMillisecondsWithPrecission: Extended;
    function GetElapsedMilliseconds_BreakPoint: Int64;
    function GetElapsedMillisecondsWithPrecission_BreakPoint: Extended;
    function GetElapsedSeconds : Int64;
    property IsRunning: Boolean read GetIsRunning;
    procedure Start;
    procedure Stop;
    procedure Reset;
    procedure Check;
    procedure BreakPoint;
    property ElapsedTicks: Int64 read GetElapsedTicks;
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
    property ElapsedMilliseconds_Breakpoint: Int64 read GetElapsedMilliseconds_BreakPoint;
    property ElapsedMillisecondsWithPrecission: Extended read GetElapsedMillisecondsWithPrecission;
    property ElapsedMillisecondsWithPrecission_BreakPoint: Extended read GetElapsedMillisecondsWithPrecission_BreakPoint;
    property ElapsedSeconds: Int64 read GetElapsedSeconds;
    function ElapsedTime(LongFormat : Boolean = False) : string;
    function ElapsedTime_BreakPoint(LongFormat : Boolean = False) : string;
  end;

  TChronometer = class(TInterfacedObject,IChronometer)
  private
    fFrequency: Int64;
    fIsRunning: Boolean;
    fIsHighResolution: Boolean;
    fStartCount, fStopCount: Int64;
    fStartBreakPoint, fStopBreakPoint : Int64;
    fReportFormatPrecission : TPrecissionFormat;
    class function Precission(aValue : Extended; FormatPrecission : TPrecissionFormat) : Extended;
    function GetTickStamp : Int64;
    function GetElapsedTicks: Int64;
    function GetElapsedMilliseconds: Int64;
    function GetElapsedMillisecondsWithPrecission: Extended;
    function GetElapsedMilliseconds_BreakPoint: Int64;
    function GetElapsedMillisecondsWithPrecission_BreakPoint: Extended;
    function GetElapsedSeconds : Int64;
    class function GetUnitTime(TimeValue : TTimeValue; LongFormat : Boolean) : string;
    class function GetFmtTime(TimeFmt : TTimeFmt; LongFormat : Boolean) : string;
    function GetIsRunning: Boolean;
  public
    constructor Create(const StartOnCreate: Boolean = false);
    class function NewChrono(const StartOnCreate: Boolean = True) : IChronometer;
    procedure Start;
    procedure Stop;
    procedure Reset;
    procedure Check;
    procedure BreakPoint;
    property IsHighResolution: Boolean read fIsHighResolution;
    property IsRunning: Boolean read GetIsRunning;
    property ReportFormatPrecission: TPrecissionFormat read fReportFormatPrecission write fReportFormatPrecission;
    property ElapsedTicks: Int64 read GetElapsedTicks;
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
    property ElapsedMilliseconds_Breakpoint: Int64 read GetElapsedMilliseconds_BreakPoint;
    property ElapsedMillisecondsWithPrecission: Extended read GetElapsedMillisecondsWithPrecission;
    property ElapsedMillisecondsWithPrecission_BreakPoint: Extended read GetElapsedMillisecondsWithPrecission_BreakPoint;
    property ElapsedSeconds: Int64 read GetElapsedSeconds;
    function ElapsedTime(LongFormat : Boolean = False) : string;
    function ElapsedTime_BreakPoint(LongFormat : Boolean = False) : string;
    class function MillisecondsToString(aMilliseconds : Int64; LongFormat : Boolean = False) : string; overload;
    class function MillisecondsToString(aMilliseconds : Extended; FormatPrecission : TPrecissionFormat = pfFloat; LongFormat : Boolean = False) : string; overload;

  end;

  TChronoBenchmark = class
  private
    fTotalProcess : Int64;
    fLastUpdateTime : TDateTime;
    fCurrentProcess : Int64;
    fFirstUpdateTime : TDateTime;
    fEstimatedMilliseconds : Int64;
    fSpeed : Single;
    procedure SetCurrentProcess(NewCurrentProcess : Int64);
    function GetElapsedMilliseconds : Int64;
  public
    constructor Create;
    property TotalProcess : Int64 read fTotalProcess write fTotalProcess;
    property CurrentProcess : Int64 read fCurrentProcess write SetCurrentProcess;
    property Speed : Single read fSpeed write fSpeed;
    property ElapsedMilliseconds : Int64 read GetElapsedMilliseconds;
    property EstimatedMilliseconds : Int64 read fEstimatedMilliseconds write fEstimatedMilliseconds;
    function ElapsedTime(LongFormat : Boolean) : string;
    function EstimatedTime(LongFormat : Boolean) : string;
    procedure Reset;
  end;


implementation

{ TChronometer Class }

constructor TChronometer.Create(const StartOnCreate: Boolean = false);
begin
  inherited Create;
  fIsRunning := False;
  fReportFormatPrecission := pfFloat;
  fStartCount := 0;
  fStopCount := 0;
  fStartBreakPoint := 0;
  fStopBreakPoint := 0;
  {$IF Defined(MSWINDOWS)}
    if not QueryPerformanceFrequency(fFrequency) then
    begin
      fIsHighResolution := False;
      //fFrequency := TTimeSpan.TicksPerSecond;
      fFrequency := MSecsPerSec;
      //TickFrequency := 1.0;
    end else
    begin
      fIsHighResolution := True;
      //TickFrequency := 10000000.0 / fFrequency;
    end;
  {$ELSEIF Defined(POSIX) OR Defined(LINUX)}
    fIsHighResolution := True;
    fFrequency := 10000000;
    //TickFrequency := 10000000.0 / fFrequency;
  {$ENDIF}
  if StartOnCreate then Start;
end;

function TChronometer.GetElapsedTicks: Int64;
begin
  Result := fStopCount - fStartCount;
end;

function TChronometer.GetTickStamp : Int64;
{$IF (Defined(POSIX) OR Defined(LINUX)) AND NOT Defined(MACOS)}
var
  res: timespec;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if fIsHighResolution then QueryPerformanceCounter(Result)
    else Result := MilliSecondOf(Now);
  {$ELSE}
    {$IFDEF MACOS}
    Result := Int64(AbsoluteToNanoseconds(mach_absolute_time) div 100);
    {$ENDIF}
    {$IF (Defined(POSIX) OR Defined(LINUX)) AND NOT Defined(MACOS)}
    clock_gettime(CLOCK_MONOTONIC, @res);
    Result := (Int64(1000000000) * res.tv_sec + res.tv_nsec) div 100;
    {$ENDIF}
  {$ENDIF}
end;

function TChronometer.ElapsedTime(LongFormat : Boolean = False) : string;
begin
  Result := MillisecondsToString(ElapsedMillisecondsWithPrecission,fReportFormatPrecission,LongFormat);
end;

function TChronometer.ElapsedTime_BreakPoint(LongFormat : Boolean = False) : string;
begin
  Result := MillisecondsToString(ElapsedMillisecondsWithPrecission_BreakPoint,fReportFormatPrecission,LongFormat);
end;

class function TChronometer.GetUnitTime(TimeValue : TTimeValue; LongFormat : Boolean) : string;
begin
  if LongFormat then Result := ' ' + UnitLongTime[TimeValue] + '(s)'
    else Result := UnitShortTime[TimeValue];
end;

class function TChronometer.GetFmtTime(TimeFmt : TTimeFmt; LongFormat : Boolean) : string;
begin
  if LongFormat then Result := FmtLongTime[TimeFmt]
    else Result := FmtShortTime[TimeFmt];
end;

function TChronometer.GetIsRunning: Boolean;
begin
  Result := fIsRunning;
end;

class function TChronometer.NewChrono(const StartOnCreate: Boolean = True) : IChronometer;
begin
  Result := TChronometer.Create(StartOnCreate);
end;

class function TChronometer.MillisecondsToString(aMilliseconds : Int64; LongFormat : Boolean = False) : string;
begin
  Result := MillisecondsToString(aMilliseconds.ToExtended,pfTruncate,LongFormat);
end;

class function TChronometer.Precission(aValue : Extended; FormatPrecission : TPrecissionFormat) : Extended;
begin
  case FormatPrecission of
    pfRound : Result := Round(aValue).ToExtended;
    pfTruncate : Result := Int(aValue);
    else Result := aValue;
  end;
end;

class function TChronometer.MillisecondsToString(aMilliseconds : Extended; FormatPrecission : TPrecissionFormat = pfFloat; LongFormat : Boolean = False) : string;
var
  dt : TDateTime;
  mc : Extended;
begin
  if aMilliseconds < 1.0 then
  begin
    mc := frac(aMilliseconds) * 1000;
    if Int(mc) = 0 then Result := Format('%d%s',[Trunc(frac(mc) * 1000),GetUnitTime(utNanosecond,LongFormat)]) //nanoseconds
      else Result := Format('%d%s',[Trunc(mc),GetUnitTime(utMicrosecond,LongFormat)]); //microseconds
  end
  else
  begin
    if aMilliseconds < MSecsPerSec then //milliseconds
    begin
      aMilliseconds := Precission(aMilliseconds,FormatPrecission);
      if (FormatPrecission = pfFloat) or (frac(aMilliseconds) > 0) then Result := Format('%f%s',[aMilliseconds,GetUnitTime(utMillisecond,LongFormat)])
        else Result := Format('%d%s',[Trunc(aMilliseconds),GetUnitTime(utMillisecond,LongFormat)])
    end
    else if (aMilliseconds / MSecsPerSec) < SecsPerMin then //seconds
    begin
      aMilliseconds := Precission((aMilliseconds / MSecsPerSec),FormatPrecission);
      if (FormatPrecission = pfFloat) or (frac(aMilliseconds) > 0) then Result := Format('%f%s',[aMilliseconds,GetUnitTime(utSecond,LongFormat)])
        else Result := Format('%d%s',[Trunc(aMilliseconds),GetUnitTime(utSecond,LongFormat)]);
    end
    else if ((aMilliseconds / MSecsPerSec) < SecsPerHour) and ((Round(aMilliseconds) mod (SecsPerMin * MSecsPerSec)) = 0) then //minutes
    begin
      aMilliseconds := Precission((aMilliseconds / (SecsPerMin * MSecsPerSec)),FormatPrecission);
      if (FormatPrecission = pfFloat) or (frac(aMilliseconds) > 0) then Result := Format('%f%s',[aMilliseconds,GetUnitTime(utMinute,LongFormat)])
        else Result := Format('%d%s',[Trunc(aMilliseconds),GetUnitTime(utMinute,LongFormat)])
    end
    else if (aMilliseconds / MSecsPerSec) < SecsPerDay then //hours
    begin
      dt := aMilliseconds / MSecsPerSec / SecsPerDay;
      if LongFormat then
      begin
        if (aMilliseconds / MSecsPerSec) > SecsPerHour then Result := FormatDateTime(GetFmtTime(tfHoursAndMinutes,LongFormat),Frac(dt))
          else Result := FormatDateTime(GetFmtTime(tfMinutesAndSeconds,LongFormat),Frac(dt));
      end
      else
      begin
        Result := FormatDateTime(GetFmtTime(tfHoursAndMinutes,LongFormat),Frac(dt));
      end;
    end
    else //días
    begin
      dt := aMilliseconds / MSecsPerSec / SecsPerDay;
      Result := Format('%d%s, %s', [trunc(dt),GetUnitTime(utDay,LongFormat),FormatDateTime(GetFmtTime(tfHoursAndMinutes,LongFormat),Frac(dt))]);
    end;
  end;
end;

function TChronometer.GetElapsedMilliseconds : Int64;
begin
  result := (MSecsPerSec * (fStopCount - fStartCount)) div fFrequency;
end;

function TChronometer.GetElapsedMilliseconds_BreakPoint : Int64;
begin
  result := (MSecsPerSec * (fStopBreakPoint - fStartBreakPoint)) div fFrequency;
end;

function TChronometer.GetElapsedMillisecondsWithPrecission : Extended;
begin
  result := (MSecsPerSec * (fStopCount - fStartCount)) / fFrequency;
end;

function TChronometer.GetElapsedMillisecondsWithPrecission_BreakPoint : Extended;
begin
  result := (MSecsPerSec * (fStopBreakPoint - fStartBreakPoint)) / fFrequency;
end;

function TChronometer.GetElapsedSeconds : Int64;
begin
  result := ((MSecsPerSec * (fStopCount - fStartCount)) div fFrequency) div MSecsPerSec;
end;

procedure TChronometer.Start;
begin
  fStartCount := GetTickStamp;
  fIsRunning := true;
end;

procedure TChronometer.Stop;
begin
  fStopCount := GetTickStamp;
  fIsRunning := false;
end;

procedure TChronometer.Reset;
begin
  fStartCount := GetTickStamp;
end;

procedure TChronometer.Check;
begin
  if fIsRunning then fStopCount := GetTickStamp;
end;

procedure TChronometer.BreakPoint;
begin
  if fIsRunning then
  begin
    if fStartBreakPoint = 0 then
    begin
      fStopBreakPoint := GetTickStamp;
      fStartBreakPoint := fStartCount;
    end
    else
    begin
      fStartBreakPoint := fStopBreakPoint;
      fStopBreakPoint := GetTickStamp;
    end;
  end
  else fStopBreakPoint := fStopCount;
end;

{ TChronoBenchmark Class }

constructor TChronoBenchmark.Create;
begin
  inherited;
  fTotalProcess := 0;
  fSpeed := 0;
  fLastUpdateTime := Now();
  fCurrentProcess := 0;
  fFirstUpdateTime := 0;
  fEstimatedMilliseconds := 0;
end;

procedure TChronoBenchmark.SetCurrentProcess(NewCurrentProcess : Int64);
begin
  //corrects first time run
  if fLastUpdateTime = 0 then fLastUpdateTime := Now();
  if fFirstUpdateTime = 0 then fFirstUpdateTime := Now();

  //calculates operation speed
  fSpeed := (NewCurrentProcess - fCurrentProcess) / ((Now() - fLastUpdateTime) * 86400);
  if fSpeed = 0 then fSpeed := 0.1;
  //returns estimated time string
  fEstimatedMilliseconds := Round(((TotalProcess - NewCurrentProcess) / fSpeed) * 1000);
  //save current values
  fLastUpdateTime := Now();
  fCurrentProcess := NewCurrentProcess;
end;

function TChronoBenchmark.GetElapsedMilliseconds : Int64;
begin
  Result := Round(((Now() - fFirstUpdateTime) * 86400 * 1000));
end;

function TChronoBenchmark.ElapsedTime(LongFormat : Boolean) : string;
begin
  Result := TChronometer.MillisecondsToString(GetElapsedMilliseconds,LongFormat);
end;

function TChronoBenchmark.EstimatedTime(LongFormat : Boolean) : string;
begin
  Result := TChronometer.MillisecondsToString(fEstimatedMilliseconds,LongFormat);
end;

procedure TChronoBenchmark.Reset;
begin
  fLastUpdateTime := Now();
  fSpeed := 0;
  fCurrentProcess := 0;
  fFirstUpdateTime := 0;
  fEstimatedMilliseconds := 0;
end;

end.
