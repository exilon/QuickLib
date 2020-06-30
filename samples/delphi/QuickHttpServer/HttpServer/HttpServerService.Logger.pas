unit HttpServerService.Logger;

interface

uses
  System.SysUtils,
  Quick.Logger.Intf;

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

uses
  Quick.Console,
  Quick.Commons;

procedure TQuickLogger.Init;
begin

end;

procedure TQuickLogger.Info(const aMsg: string);
begin
  cout(aMsg, etInfo);
end;

procedure TQuickLogger.Info(const aMsg: string; aValues: array of const);
begin
  cout(aMsg, aValues, etInfo);
end;

procedure TQuickLogger.Succ(const aMsg: string);
begin
  cout(aMsg, etSuccess);
end;

procedure TQuickLogger.Succ(const aMsg: string; aParams: array of const);
begin
  cout(aMsg,aParams, etSuccess);
end;

procedure TQuickLogger.Done(const aMsg: string);
begin
  cout(aMsg, ccGreen);
end;

procedure TQuickLogger.Done(const aMsg: string; aValues: array of const);
begin
  cout(format(aMsg, aValues), ccGreen);
end;

procedure TQuickLogger.Warn(const aMsg: string);
begin
  cout(aMsg, etWarning);
end;

procedure TQuickLogger.Warn(const aMsg: string; aValues: array of const);
begin
  cout(aMsg, aValues, etWarning);
end;

procedure TQuickLogger.Error(const aMsg: string);
begin
  cout(aMsg, etError);
end;

procedure TQuickLogger.Error(const aMsg: string; aValues: array of const);
begin
  cout(aMsg, aValues, etError);
end;

procedure TQuickLogger.Critical(const aMsg: string);
begin
  cout(aMsg, ccRed);
end;

procedure TQuickLogger.Critical(const aMsg: string; aValues: array of const);
begin
  cout(format(aMsg, aValues), ccRed);
end;

procedure TQuickLogger.Trace(const aMsg: string);
begin
  cout(aMsg, etTrace);
end;

procedure TQuickLogger.Trace(const aMsg: string; aValues: array of const);
begin
  cout(aMsg, aValues, etTrace);
end;

procedure TQuickLogger.Debug(const aMsg: string);
begin
  cout(aMsg, etDebug);
end;

procedure TQuickLogger.Debug(const aMsg: string; aValues: array of const);
begin
  cout(aMsg, aValues, etDebug);
end;

procedure TQuickLogger.&Except(const aMsg: string; aValues: array of const);
begin
  cout('Exception: '+ aMsg, aValues, etError);
end;

procedure TQuickLogger.&Except(const aMsg: string; aValues: array of const; const aException, aStackTrace: string);
begin
  cout('Exception: ' + aMsg + ' ('+aException+' : '+aStackTrace+')', aValues, etError);
end;

procedure TQuickLogger.&Except(const aMsg, aException, aStackTrace: string);
begin
  cout('Exception: ' + aMsg + ' ('+aException+' : '+aStackTrace+')', etError);
end;

end.

