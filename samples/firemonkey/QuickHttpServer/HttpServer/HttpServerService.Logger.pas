unit HttpServerService.Logger;

interface

uses
  System.SysUtils,
  Quick.Logger.Intf,
  Quick.Logger,
  Quick.Logger.Provider.Console;

type
  TQuickLogger = class(TInterfacedObject,ILogger)
  public
    procedure Init;
    procedure Info(const aMsg : string); overload;
    procedure Info(const aMsg : string; aValues : array of const); overload;
    procedure Succ(const aMsg : string); overload;
    procedure Succ(const aMsg : string; aParams : array of const); overload;
    procedure Done(const aMsg : string); overload;
    procedure Done(const aMsg : string; aValues : array of const); overload;
    procedure Warn(const aMsg : string); overload;
    procedure Warn(const aMsg : string; aValues : array of const); overload;
    procedure Error(const aMsg : string); overload;
    procedure Error(const aMsg : string; aValues : array of const); overload;
    procedure Critical(const aMsg : string); overload;
    procedure Critical(const aMsg : string; aValues : array of const); overload;
    procedure Trace(const aMsg : string); overload;
    procedure Trace(const aMsg : string; aValues : array of const); overload;
    procedure Debug(const aMsg : string); overload;
    procedure Debug(const aMsg : string; aValues : array of const); overload;
    procedure &Except(const aMsg : string; aValues : array of const); overload;
    procedure &Except(const aMsg, aException, aStackTrace : string); overload;
    procedure &Except(const aMsg : string; aValues: array of const; const aException, aStackTrace: string); overload;
  end;

implementation

{ TQuickLogger }


procedure TQuickLogger.Init;
begin
  GlobalLogConsoleProvider.LogLevel := LOG_DEBUG;
  Logger.Providers.Add(GlobalLogConsoleProvider);
  GlobalLogConsoleProvider.Enabled := True;
end;

procedure TQuickLogger.Info(const aMsg: string);
begin
  Logger.Info(aMsg);
end;

procedure TQuickLogger.Info(const aMsg: string; aValues: array of const);
begin
  Logger.Info(aMsg,aValues);
end;

procedure TQuickLogger.Succ(const aMsg: string);
begin
  Logger.Succ(aMsg);
end;

procedure TQuickLogger.Succ(const aMsg: string; aParams: array of const);
begin
  Logger.Succ(aMsg,aParams);
end;

procedure TQuickLogger.Done(const aMsg: string);
begin
  Logger.Done(aMsg);
end;

procedure TQuickLogger.Done(const aMsg: string; aValues: array of const);
begin
  Logger.Done(aMsg,aValues);
end;

procedure TQuickLogger.Warn(const aMsg: string);
begin
  Logger.Warn(aMsg);
end;

procedure TQuickLogger.Warn(const aMsg: string; aValues: array of const);
begin
  Logger.Warn(aMsg,aValues);
end;

procedure TQuickLogger.Error(const aMsg: string);
begin
  Logger.Error(aMsg);
end;

procedure TQuickLogger.Error(const aMsg: string; aValues: array of const);
begin
  Logger.Error(aMsg,aValues);
end;

procedure TQuickLogger.Critical(const aMsg: string);
begin
  Logger.Critical(aMsg);
end;

procedure TQuickLogger.Critical(const aMsg: string; aValues: array of const);
begin
  Logger.Critical(aMsg,aValues);
end;

procedure TQuickLogger.Trace(const aMsg: string);
begin
  Logger.Trace(aMsg);
end;

procedure TQuickLogger.Trace(const aMsg: string; aValues: array of const);
begin
  Logger.Trace(aMsg,aValues);
end;

procedure TQuickLogger.Debug(const aMsg: string);
begin
  Logger.Debug(aMsg);
end;

procedure TQuickLogger.Debug(const aMsg: string; aValues: array of const);
begin
  Logger.Debug(aMsg,aValues);
end;

procedure TQuickLogger.&Except(const aMsg: string; aValues: array of const);
begin
  Logger.&Except(aMsg,aValues);
end;

procedure TQuickLogger.&Except(const aMsg: string; aValues: array of const; const aException, aStackTrace: string);
begin
  Logger.&Except(aMsg,aValues,aException,aStacktrace);
end;

procedure TQuickLogger.&Except(const aMsg, aException, aStackTrace: string);
begin
  Logger.&Except(aMsg,aException,aStackTrace);
end;

end.

