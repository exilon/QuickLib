{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.Logger.Intf
  Description : Quick Logger Interface
  Author      : Kike Pérez
  Version     : 1.8
  Created     : 30/08/2019
  Modified    : 11/09/2019

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

unit Quick.Logger.Intf;

{$i QuickLib.inc}

interface

uses
  {$IFNDEF FPC}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

type
  ILogger = interface
  ['{1065BB46-7EB8-480A-8B30-ED3B865DDB18}']
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

  TNullLogger = class(TInterfacedObject,ILogger)
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
    procedure &Except(MsgObject : TObject); overload;
  end;

implementation

{ TNullLogger }


procedure TNullLogger.Info(const aMsg: string);
begin

end;

procedure TNullLogger.Info(const aMsg: string; aParams: array of const);
begin

end;

procedure TNullLogger.Succ(const aMsg: string; aParams: array of const);
begin

end;

procedure TNullLogger.Succ(const aMsg: string);
begin

end;

procedure TNullLogger.Done(const aMsg: string);
begin

end;

procedure TNullLogger.Debug(const aMsg: string; aParams: array of const);
begin

end;

procedure TNullLogger.Done(const aMsg: string; aParams: array of const);
begin

end;

procedure TNullLogger.Warn(const aMsg: string);
begin

end;

procedure TNullLogger.Warn(const aMsg: string; aParams: array of const);
begin

end;

procedure TNullLogger.Error(const aMsg: string);
begin

end;

procedure TNullLogger.Error(const aMsg: string; aParams: array of const);
begin

end;

procedure TNullLogger.Critical(const aMsg: string);
begin

end;

procedure TNullLogger.&Except(MsgObject: TObject);
begin

end;

procedure TNullLogger.Critical(const aMsg: string; aParams: array of const);
begin

end;

procedure TNullLogger.Trace(const aMsg: string);
begin

end;

procedure TNullLogger.Trace(const aMsg: string; aParams: array of const);
begin

end;

procedure TNullLogger.Debug(const aMsg: string);
begin

end;

procedure TNullLogger.&Except(const aMsg: string; aValues: array of const);
begin

end;

procedure TNullLogger.&Except(const aMsg: string; aValues: array of const; const aException, aStackTrace: string);
begin

end;

procedure TNullLogger.&Except(const aMsg, aException, aStackTrace: string);
begin

end;

end.
