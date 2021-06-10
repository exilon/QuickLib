{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.Debug.Utils
  Description : Debug Utils
  Author      : Kike Pérez
  Version     : 1.9
  Created     : 05/06/2020
  Modified    : 07/07/2020

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

unit Quick.Debug.Utils;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  Quick.Logger.Intf,
  Quick.Serializer.Intf,
  Quick.Commons,
  {$IFNDEF NEXTGEN}
  Quick.Console,
  {$ELSE}
   {$IFNDEF DELPHILINUX}
    FMX.Types,
   {$ENDIF}
  {$ENDIF}
  Quick.Chrono;

type

  TDebugConsoleLogger = class(TInterfacedObject,ILogger)
  private
    fShowTime : Boolean;
    fFormatSettings : TFormatSettings;
    function FormatMsg(const aMsg : string) : string;
  public
    constructor Create;
    property ShowTime : Boolean read fShowTime write fShowTime;
    property FormatSettings : TFormatSettings read fFormatSettings write fFormatSettings;
    procedure Info(const aMsg : string); overload;
    procedure Info(const aMsg : string; aParams : array of const); overload;
    procedure Succ(const aMsg : string); overload;
    procedure Succ(const aMsg : string; aParams : array of const); overload;
    procedure Done(const aMsg : string); overload;
    procedure Done(const aMsg : string; aParams : array of const); overload;
    procedure Warn(const aMsg : string); overload;
    procedure Warn(const aMsg : string; aParams : array of const); overload;
    procedure Error(const aMsg : string); overload;
    procedure Error(const aMsg : string; aParams : array of const); overload;
    procedure Critical(const aMsg : string); overload;
    procedure Critical(const aMsg : string; aParams : array of const); overload;
    procedure Trace(const aMsg : string); overload;
    procedure Trace(const aMsg : string; aParams : array of const); overload;
    procedure Debug(const aMsg : string); overload;
    procedure Debug(const aMsg : string; aParams : array of const); overload;
    procedure &Except(const aMsg : string; aValues : array of const); overload;
    procedure &Except(const aMsg, aException, aStackTrace : string); overload;
    procedure &Except(const aMsg : string; aValues: array of const; const aException, aStackTrace: string); overload;
  end;

  IDebugMethodEnter = interface
  ['{3BE4E8C2-CBCF-43BC-B3BE-0A0235C49BF1}']
    procedure TimeIt;
  end;

  TDebugMethodEnter = class(TInterfacedObject,IDebugMethodEnter)
  private
    fLogger : ILogger;
    fCallerMethod : string;
    fTimeIt : Boolean;
    fChrono : IChronometer;
  public
    constructor Create(aLogger : ILogger; const aCallerMethod : string);
    destructor Destroy; override;
    procedure TimeIt;
  end;

  IDebugMehtodChrono = interface
  ['{3DDD5389-D55A-4DEA-81FA-980CF41ACE38}']
    procedure BreakPoint(const aMessage : string);
    procedure Stop;
  end;

  TDebugMethodChrono = class(TInterfacedObject,IDebugMehtodChrono)
  private
    fLogger : ILogger;
    fCallerMethod : string;
    fMsg : string;
    fChrono : IChronometer;
  public
    constructor Create(aLogger: ILogger; const aCallerMethod, aMsg : string);
    destructor Destroy; override;
    procedure BreakPoint(const aMsg : string);
    procedure Stop;
  end;

  TDebugger = class
  private class var
    fLogger : ILogger;
    fSerializer : ISerializer;
    fShowTime : Boolean;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure SetLogger(aLogger : ILogger);
    class property ShowTime : Boolean read fShowTime write fShowTime;
    class function NewChrono(aStarted : Boolean) : IChronometer;
    class function TimeIt(aOwner : TObject; const aFunctionName, aDescription : string) : IDebugMehtodChrono;
    class function Log : ILogger;
    class procedure Trace(aOwner : TObject; const aMsg : string); overload;
    class procedure Trace(aOwner : TObject; const aMsg : string; aParams : array of const); overload;
    class procedure Trace(const aMsg : string); overload;
    class procedure Trace(const aMsg : string; aParams : array of const); overload;
    class procedure Trace(const aMsg : string; const aObject : TObject); overload;
    class function Enter(aOwner : TObject; const aFunctionName: string) : IDebugMethodEnter;
  end;

  {$IFDEF NEXTGEN}
  procedure cout(const cMsg : string; params : array of const; cEventType : TLogEventType); overload;
  procedure cout(const cMsg : string; cEventType : TLogEventType); overload;
  {$ENDIF}

implementation

uses
  Quick.Json.Serializer;


{$IFDEF NEXTGEN}
procedure cout(const cMsg : string; params : array of const; cEventType : TLogEventType);
begin
  FMX.Types.Log.d(Format(cMsg,params));
end;

procedure cout(const cMsg : string; cEventType : TLogEventType); overload;
begin
  FMX.Types.Log.d(cMsg);
end;
{$ENDIF}


{ TDebugger }

class constructor TDebugger.Create;
begin
  fSerializer := TJsonSerializer.Create(TSerializeLevel.slPublicProperty);
  fLogger := TDebugConsoleLogger.Create;
  fShowTime := True;
end;

class destructor TDebugger.Destroy;
begin
  fSerializer := nil;
  fLogger := nil;
end;

class function TDebugger.TimeIt(aOwner : TObject; const aFunctionName, aDescription: string): IDebugMehtodChrono;
begin
  if aOwner <> nil then Result := TDebugMethodChrono.Create(fLogger,Format('%s.%s',[aOwner.ClassName,aFunctionName]),aDescription)
    else Result := TDebugMethodChrono.Create(fLogger,Format('%s',[aFunctionName]),aDescription);
end;

class function TDebugger.Enter(aOwner : TObject; const aFunctionName: string) : IDebugMethodEnter;
begin
  if aOwner <> nil then
  begin
    fLogger.Debug(Format('[ENTER] >> %s.%s',[aOwner.ClassName,aFunctionName]));
    Result := TDebugMethodEnter.Create(fLogger,Format('%s.%s',[aOwner.ClassName,aFunctionName]));
  end
  else
  begin
    fLogger.Debug(Format('[ENTER] >> %s',[aFunctionName]));
    Result := TDebugMethodEnter.Create(fLogger,aFunctionName);
  end;
end;

class function TDebugger.NewChrono(aStarted : Boolean) : IChronometer;
begin
  Result := TChronometer.Create(aStarted);
end;

class function TDebugger.Log: ILogger;
begin
  Result := fLogger;
end;

class procedure TDebugger.SetLogger(aLogger: ILogger);
begin
  if aLogger = nil then raise Exception.Create('Debugger logger cannot be nil!');
  fLogger := aLogger;
  fShowTime := False;
end;

class procedure TDebugger.Trace(aOwner: TObject; const aMsg: string);
begin
  if aOwner <> nil then fLogger.Trace(Format('[TRACE] %s -> %s',[aOwner.ClassName,aMsg]))
    else fLogger.Trace(Format('[TRACE] -> %s',[aMsg]))
end;

class procedure TDebugger.Trace(aOwner: TObject; const aMsg: string; aParams: array of const);
begin
  Self.Trace(aOwner,Format(aMsg,aParams));
end;

class procedure TDebugger.Trace(const aMsg: string);
begin
  fLogger.Trace(Format('[TRACE] %s',[aMsg]));
end;

class procedure TDebugger.Trace(const aMsg: string; aParams: array of const);
begin
  Self.Trace(Format(aMsg,aParams));
end;

class procedure TDebugger.Trace(const aMsg: string; const aObject: TObject);
begin
  Self.Trace(aMsg + ' ' + fSerializer.ObjectToJson(aObject));
end;

{ TDebugConsoleLogger }

constructor TDebugConsoleLogger.Create;
begin
  fFormatSettings.DateSeparator := '/';
  fFormatSettings.TimeSeparator := ':';
  fFormatSettings.ShortDateFormat := 'DD-MM-YYY HH:NN:SS.ZZZ';
  fFormatSettings.ShortTimeFormat := 'HH:NN:SS';
  {$IFNDEF NEXTGEN}
  Console.LogVerbose := LOG_ALL;
  {$ENDIF}
  fShowTime := True;
end;

procedure TDebugConsoleLogger.Critical(const aMsg: string; aParams: array of const);
begin
  cout(FormatMsg(aMsg),aParams,TLogEventType.etCritical);
end;

procedure TDebugConsoleLogger.Critical(const aMsg: string);
begin
  cout(FormatMsg(aMsg),TLogEventType.etCritical);
end;

procedure TDebugConsoleLogger.Debug(const aMsg: string; aParams: array of const);
begin
  cout(FormatMsg(aMsg),aParams,TLogEventType.etDebug);
end;

procedure TDebugConsoleLogger.Debug(const aMsg: string);
begin
  cout(FormatMsg(aMsg),TLogEventType.etDebug);
end;

procedure TDebugConsoleLogger.Done(const aMsg: string; aParams: array of const);
begin
  cout(FormatMsg(aMsg),aParams,TLogEventType.etDone);
end;

procedure TDebugConsoleLogger.Done(const aMsg: string);
begin
  cout(FormatMsg(aMsg),TLogEventType.etDone);
end;

procedure TDebugConsoleLogger.Error(const aMsg: string);
begin
  cout(FormatMsg(aMsg),TLogEventType.etError);
end;

procedure TDebugConsoleLogger.Error(const aMsg: string; aParams: array of const);
begin
  cout(FormatMsg(aMsg),aParams,TLogEventType.etError);
end;

procedure TDebugConsoleLogger.&Except(const aMsg: string; aValues: array of const);
begin
  cout(FormatMsg(aMsg),aValues,TLogEventType.etException);
end;

procedure TDebugConsoleLogger.&Except(const aMsg, aException, aStackTrace: string);
begin
  cout(FormatMsg(aMsg),TLogEventType.etException);
end;

procedure TDebugConsoleLogger.&Except(const aMsg: string; aValues: array of const; const aException, aStackTrace: string);
begin
  cout(FormatMsg(aMsg),aValues,TLogEventType.etException);
end;

function TDebugConsoleLogger.FormatMsg(const aMsg: string): string;
begin
  if fShowTime then Result := DateTimeToStr(Now(),fFormatSettings) + ' ' + aMsg
    else Result := aMsg
end;

procedure TDebugConsoleLogger.Info(const aMsg: string; aParams: array of const);
begin
  cout(FormatMsg(aMsg),aParams,TLogEventType.etInfo);
end;

procedure TDebugConsoleLogger.Info(const aMsg: string);
begin
  cout(FormatMsg(aMsg),TLogEventType.etInfo);
end;

procedure TDebugConsoleLogger.Succ(const aMsg: string; aParams: array of const);
begin
  cout(FormatMsg(aMsg),aParams,TLogEventType.etSuccess);
end;

procedure TDebugConsoleLogger.Succ(const aMsg: string);
begin
  cout(FormatMsg(aMsg),TLogEventType.etSuccess);
end;

procedure TDebugConsoleLogger.Trace(const aMsg: string; aParams: array of const);
begin
  cout(FormatMsg(aMsg),aParams,TLogEventType.etTrace);
end;

procedure TDebugConsoleLogger.Trace(const aMsg: string);
begin
  cout(FormatMsg(aMsg),TLogEventType.etTrace);
end;

procedure TDebugConsoleLogger.Warn(const aMsg: string; aParams: array of const);
begin
  cout(FormatMsg(aMsg),aParams,TLogEventType.etWarning);
end;

procedure TDebugConsoleLogger.Warn(const aMsg: string);
begin
  cout(FormatMsg(aMsg),TLogEventType.etWarning);
end;

{ TDebugFunctionEnter }

constructor TDebugMethodEnter.Create(aLogger: ILogger; const aCallerMethod: string);
begin
  fLogger := aLogger;
  fCallerMethod := aCallerMethod;
end;

destructor TDebugMethodEnter.Destroy;
begin
  if fTimeIt then
  begin
    fChrono.Stop;
    fLogger.Debug(Format('[EXIT] >> %s in %s',[fCallerMethod,fChrono.ElapsedTime]));
  end
  else fLogger.Debug(Format('[EXIT] >> %s',[fCallerMethod]));
  inherited;
end;

procedure TDebugMethodEnter.TimeIt;
begin
  fTimeIt := True;
  fChrono := TChronometer.Create(True);
end;

{ TDebugMethodChrono }

constructor TDebugMethodChrono.Create(aLogger: ILogger; const aCallerMethod, aMsg : string);
begin
  fLogger := aLogger;
  fCallerMethod := aCallerMethod;
  fMsg := aMsg;
  fChrono := TChronometer.Create(True);
end;

destructor TDebugMethodChrono.Destroy;
begin
  if fChrono.IsRunning then
  begin
    fChrono.Stop;
    fLogger.Trace(Format('[CHRONO] %s -> %s = %s',[fCallerMethod,fMsg,fChrono.ElapsedTime]));
  end;
  inherited;
end;

procedure TDebugMethodChrono.BreakPoint(const aMsg: string);
begin
  fChrono.BreakPoint;
  fLogger.Trace(Format('[CHRONO] %s -> %s = %s',[fCallerMethod,aMsg,fChrono.ElapsedTime_BreakPoint]));
end;

procedure TDebugMethodChrono.Stop;
begin
  fChrono.Stop;
  fLogger.Trace(Format('[CHRONO] %s -> %s = %s',[fCallerMethod,fMsg,fChrono.ElapsedTime]));
end;

end.
