{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.Chrono
  Description : Chronometers time elapsed and estimated time to do a task
  Author      : Kike Pérez
  Version     : 1.2
  Created     : 27/08/2015
  Modified    : 27/02/2018

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

uses
  Windows,
  SysUtils,
  DateUtils;


resourcestring
  strDAY = 'day';
  strHOUR = 'hour';
  strMINUTE = 'minute';
  strSECOND = 'second';
  strMILLISECOND = 'millisecond';
  strFMTSHORT_HOURS_MINUTES = 'hh:nn:ss';
  strFMTSHORT_MINUTES_SECONDS = 'hh:nn:ss';
  strFMTLONG_HOURS_MINUTES = 'h "hour(s) and" n "minute(s)"';
  strFMTLONG_MINUTES_SECONDS = 'n "minute(s) and" s "second(s)"';

type

  TTimeValue = (utDay, utHour, utMinute, utSecond, utMillisecond);
  TTimeFmt = (tfHoursAndMinutes, tfMinutesAndSeconds);

const
  UnitShortTime : array[utDay..utMillisecond] of string = ('d','h','m','s','ms');
  UnitLongTime : array[utDay..utMillisecond] of string = (strDAY,strHOUR,strMINUTE,strSECOND,strMILLISECOND);
  FmtShortTime : array[tfHoursAndMinutes..tfMinutesAndSeconds] of string = (strFMTSHORT_HOURS_MINUTES,strFMTSHORT_MINUTES_SECONDS);
  FmtLongTime : array[tfHoursAndMinutes..tfMinutesAndSeconds] of string = (strFMTLONG_HOURS_MINUTES,strFMTLONG_MINUTES_SECONDS);

type

  TChronometer = class
  private
    fFrequency: TLargeInteger;
    fIsRunning: Boolean;
    fIsHighResolution: Boolean;
    fStartCount, fStopCount: TLargeInteger;
    fStartBreakPoint, fStopBreakPoint : TLargeInteger;
    fReportFormatPrecission : Boolean;
    procedure SetTickStamp(var lInt: TLargeInteger);
    function GetElapsedTicks: TLargeInteger;
    function GetElapsedMilliseconds: TLargeInteger;
    function GetElapsedMillisecondsWithPrecission: Extended;
    function GetElapsedMilliseconds_BreakPoint: TLargeInteger;
    function GetElapsedMillisecondsWithPrecission_BreakPoint: Extended;
    function GetElapsedSeconds : TLargeInteger;
    class function GetUnitTime(TimeValue : TTimeValue; LongFormat : Boolean) : string;
    class function GetFmtTime(TimeFmt : TTimeFmt; LongFormat : Boolean) : string;
  public
    constructor Create(const StartOnCreate: Boolean = false);
    procedure Start;
    procedure Stop;
    procedure Reset;
    procedure Check;
    procedure BreakPoint;
    property IsHighResolution: Boolean read fIsHighResolution;
    property IsRunning: Boolean read fIsRunning;
    property ReportFormatPrecission: boolean read fReportFormatPrecission write fReportFormatPrecission;
    property ElapsedTicks: TLargeInteger read GetElapsedTicks;
    property ElapsedMilliseconds: TLargeInteger read GetElapsedMilliseconds;
    property ElapsedMilliseconds_Breakpoint: TLargeInteger read GetElapsedMilliseconds_BreakPoint;
    property ElapsedMillisecondsWithPrecission: Extended read GetElapsedMillisecondsWithPrecission;
    property ElapsedMillisecondsWithPrecission_BreakPoint: Extended read GetElapsedMillisecondsWithPrecission_BreakPoint;
    property ElapsedSeconds: TLargeInteger read GetElapsedSeconds;
    function ElapsedTime(LongFormat : Boolean = False) : string;
    function ElapsedTime_BreakPoint(LongFormat : Boolean = False) : string;
    class function MillisecondsToString(aMilliseconds : TLargeInteger; LongFormat : Boolean = False) : string; overload;
    class function MillisecondsToString(aMillisecondsWithPrecission : Extended; LongFormat : Boolean = False) : string; overload;

  end;

  TChronoBenchmark = class
  private
    fTotalProcess : Int64;
    fLastUpdateTime : TDateTime;
    fCurrentProcess : Int64;
    fFirstUpdateTime : TDateTime;
    fEstimatedMilliseconds : TLargeInteger;
    fSpeed : Single;
    procedure SetCurrentProcess(NewCurrentProcess : Int64);
    function GetElapsedMilliseconds : TLargeInteger;
  public
    constructor Create;
    property TotalProcess : Int64 read fTotalProcess write fTotalProcess;
    property CurrentProcess : Int64 read fCurrentProcess write SetCurrentProcess;
    property Speed : Single read fSpeed write fSpeed;
    property ElapsedMilliseconds : TLargeInteger read GetElapsedMilliseconds;
    property EstimatedMilliseconds : TLargeInteger read fEstimatedMilliseconds write fEstimatedMilliseconds;
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
  fIsHighResolution := QueryPerformanceFrequency(fFrequency);
  fReportFormatPrecission := True;
  fStartCount := 0;
  fStopCount := 0;
  fStartBreakPoint := 0;
  fStopBreakPoint := 0;
  if not fIsHighResolution then fFrequency := MSecsPerSec;
  if StartOnCreate then Start;
end;

function TChronometer.GetElapsedTicks: TLargeInteger;
begin
  Result := fStopCount - fStartCount;
end;

procedure TChronometer.SetTickStamp(var lInt: TLargeInteger);
begin
  if fIsHighResolution then QueryPerformanceCounter(lInt)
    else lInt := MilliSecondOf(Now);
end;

function TChronometer.ElapsedTime(LongFormat : Boolean = False) : string;
begin
  if LongFormat then
  begin
    if fReportFormatPrecission then Result := MillisecondsToString(ElapsedMillisecondsWithPrecission,True)
      else Result := MillisecondsToString(ElapsedMilliseconds,True);
  end
  else
  begin
    if fReportFormatPrecission then Result := MillisecondsToString(ElapsedMillisecondsWithPrecission)
      else Result := MillisecondsToString(ElapsedMilliseconds);
  end;
end;

function TChronometer.ElapsedTime_BreakPoint(LongFormat : Boolean = False) : string;
begin
  if LongFormat then
  begin
    if fReportFormatPrecission then Result := MillisecondsToString(ElapsedMillisecondsWithPrecission_BreakPoint,True)
      else Result := MillisecondsToString(ElapsedMilliseconds_BreakPoint,True);
  end
  else
  begin
    if fReportFormatPrecission then Result := MillisecondsToString(ElapsedMillisecondsWithPrecission_BreakPoint)
      else Result := MillisecondsToString(ElapsedMilliseconds_BreakPoint);
  end;
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

class function TChronometer.MillisecondsToString(aMilliseconds : TLargeInteger; LongFormat : Boolean = False) : string;
var
  dt : TDateTime;
  sp : string;
begin
  if LongFormat then sp := ' ' else sp := '';

  if aMilliseconds < MSecsPerSec then //milliseconds
  begin
    Result := Format('%d%s%s',[aMilliseconds,sp,GetUnitTime(utMillisecond,LongFormat)]);
  end
  else if (aMilliseconds / MSecsPerSec) < SecsPerMin then //seconds
  begin
    Result := Format('%d%s%s',[(aMilliseconds div MSecsPerSec),sp,GetUnitTime(utSecond,LongFormat)]);
  end
  else if ((aMilliseconds / MSecsPerSec) < SecsPerHour) and ((aMilliseconds mod (SecsPerMin * MSecsPerSec)) = 0) then //minutes
  begin
    Result := Format('%d%s%s',[(aMilliseconds div (SecsPerMin * MSecsPerSec)),sp,GetUnitTime(utMinute,LongFormat)]);
  end
  else if (aMilliseconds / MSecsPerSec) < SecsPerDay then //hours
  begin
    dt := aMilliseconds / MSecsPerSec / SecsPerDay;
    if LongFormat then
    begin
      if (aMilliseconds / MSecsPerSec) > SecsPerHour then Result := FormatDateTime(GetFmtTime(tfHoursAndMinutes,LongFormat),Frac(dt))
        else Result := FormatDateTime(GetFmtTime(tfMinutesAndSeconds,LongFormat),Frac(dt))
    end
    else
    begin
      Result := FormatDateTime(GetFmtTime(tfHoursAndMinutes,LongFormat),Frac(dt));
    end;
  end
  else //days
  begin
    dt := aMilliseconds / MSecsPerSec / SecsPerDay;
    Result := Format('%d%s%s, %s', [trunc(dt),sp,GetUnitTime(utDay,LongFormat),FormatDateTime(GetFmtTime(tfHoursAndMinutes,LongFormat),Frac(dt))]);
  end;
end;

class function TChronometer.MillisecondsToString(aMillisecondsWithPrecission : Extended; LongFormat : Boolean = False) : string;
var
  dt : TDateTime;
  sp : string;
begin
  if LongFormat then sp := '' else sp := ' ';
  if aMillisecondsWithPrecission < MSecsPerSec then //milliseconds
  begin
    Result := Format('%f%s%s',[aMillisecondsWithPrecission,sp,GetUnitTime(utMillisecond,LongFormat)]);
  end
  else if (aMillisecondsWithPrecission / MSecsPerSec) < 60 then //seconds
  begin
    Result := Format('%f%s%s',[(aMillisecondsWithPrecission / MSecsPerSec),sp,GetUnitTime(utSecond,LongFormat)]);
  end
  else if (aMillisecondsWithPrecission / MSecsPerSec) < SecsPerHour then //minutes
  begin
    Result := Format('%f%s%s',[(aMillisecondsWithPrecission / (SecsPerMin * MSecsPerSec)),sp,GetUnitTime(utMinute,LongFormat)]);
  end
  else if (aMillisecondsWithPrecission / MSecsPerSec) < SecsPerDay then //hours
  begin
    dt := aMillisecondsWithPrecission / MSecsPerSec / SecsPerDay;
    if LongFormat then
    begin
      if (aMillisecondsWithPrecission / MSecsPerSec) > SecsPerHour then Result := FormatDateTime(GetFmtTime(tfHoursAndMinutes,LongFormat),Frac(dt))
        else Result := FormatDateTime(GetFmtTime(tfMinutesAndSeconds,LongFormat),Frac(dt));
    end
    else
    begin
      Result := FormatDateTime(GetFmtTime(tfHoursAndMinutes,LongFormat),Frac(dt));
    end;
  end
  else //días
  begin
    dt := aMillisecondsWithPrecission / MSecsPerSec / SecsPerDay;
    Result := Format('%d%s%s, %s', [trunc(dt),sp,GetUnitTime(utDay,LongFormat),FormatDateTime(GetFmtTime(tfHoursAndMinutes,LongFormat),Frac(dt))]);
  end;
end;

function TChronometer.GetElapsedMilliseconds : TLargeInteger;
begin
  result := (MSecsPerSec * (fStopCount - fStartCount)) div fFrequency;
end;

function TChronometer.GetElapsedMilliseconds_BreakPoint : TLargeInteger;
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

function TChronometer.GetElapsedSeconds : TLargeInteger;
begin
  result := ((MSecsPerSec * (fStopCount - fStartCount)) div fFrequency) div MSecsPerSec;
end;

procedure TChronometer.Start;
begin
  SetTickStamp(fStartCount);
  fIsRunning := true;
end;

procedure TChronometer.Stop;
begin
  SetTickStamp(fStopCount);
  fIsRunning := false;
end;

procedure TChronometer.Reset;
begin
  SetTickStamp(fStartCount);
end;

procedure TChronometer.Check;
begin
  if fIsRunning then SetTickStamp(fStopCount);
end;

procedure TChronometer.BreakPoint;
begin
  if fIsRunning then
  begin
    if fStartBreakPoint = 0 then
    begin
      SetTickStamp(fStopBreakPoint);
      fStartBreakPoint := fStartCount;
    end
    else
    begin
      fStartBreakPoint := fStopBreakPoint;
      SetTickStamp(fStopBreakPoint);
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

function TChronoBenchmark.GetElapsedMilliseconds : TLargeInteger;
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
