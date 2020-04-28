{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.FaultControl
  Description : Thread workers retry and circuit break
  Author      : Kike Pérez
  Version     : 1.5
  Created     : 20/06/2019
  Modified    : 02/12/2019

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

unit Quick.FaultControl;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  SyncObjs,
  Quick.Commons;

type

  TRetryEvent = procedure(aRaisedException : Exception; var vStopRetries : Boolean) of object;
  TMaxRetriesEvent = procedure of object;
  TCircuitBreakEvent = procedure of object;

  TFaultPolicy = class
  private
    fMaxRetries : Integer;
    fWaitTimeBetweenRetries : Integer;
    fWaitTimeMultiplierFactor : Double;
  public
    constructor Create;
    property MaxRetries : Integer read fMaxRetries write fMaxRetries;
    property WaitTimeBetweenRetries : Integer read fWaitTimeBetweenRetries write fWaitTimeBetweenRetries;
    property WaitTimeMultiplierFactor : Double read fWaitTimeMultiplierFactor write fWaitTimeMultiplierFactor;
  end;

  TFaultControl = class
  private
    fMaxRetries : Integer;
    fWaitTimeBetweenRetries : Integer;
    fWaitTimeMultiplierFactor : Double;
    fWaitTimeArray : TArray<Integer>;
    fNumRetries : Integer;
    fLastException : Exception;
    fCircuitBreaked : Boolean;
    fOnRetry : TRetryEvent;
    fOnCircuitBreak : TCircuitBreakEvent;
    fTaskFailed : Boolean;
    procedure WaitBeforeRetry(aNumWaitMilliseconds : Integer);
    procedure SetWaitTimeMultiplierFactor(const Value: Double);
  public
    constructor Create;
    destructor Destroy; override;
    property MaxRetries : Integer read fMaxRetries write fMaxRetries;
    property WaitTimeBetweenRetriesMS : Integer read fWaitTimeBetweenRetries write fWaitTimeBetweenRetries;
    property WaitTimeMultiplierFactor : Double read fWaitTimeMultiplierFactor write SetWaitTimeMultiplierFactor;
    property WaitTimeMSArray : TArray<Integer> read fWaitTimeArray write fWaitTimeArray;
    property OnRetry : TRetryEvent read fOnRetry write fOnRetry;
    property OnCircuitBreak : TCircuitBreakEvent read fOnCircuitBreak write fOnCircuitBreak;
    property TaskFailed : Boolean read fTaskFailed;
    property CircuitBreaked : Boolean read fCircuitBreaked;
    property LastException : Exception read fLastException;
    property NumRetries : Integer read fNumRetries;
    procedure FailedExecution(aException : Exception);
    procedure SuccessExecution;
    procedure Reset;
    function NeedToRetry : Boolean;
  end;

  EFaultControlConfigError = class(Exception);

implementation

{ TFaultControl }

constructor TFaultControl.Create;
begin
  fTaskFailed := False;
  fLastException := nil;
  fOnRetry := nil;
  fOnCircuitBreak := nil;
  fCircuitBreaked := False;
  fNumRetries := 0;
  fMaxRetries := 0;
  fWaitTimeBetweenRetries := 0;
  fWaitTimeMultiplierFactor := 1;
  {$IFDEF DELPHIXE7_UP}
  fWaitTimeArray := [];
  {$ELSE}
  fWaitTimeArray := nil;
  {$ENDIF}
end;

function TFaultControl.NeedToRetry: Boolean;
var
  waitretryMS : Integer;
begin
  Result := False;
  if fTaskFailed then
  begin
    if (fMaxRetries <> 0) and (not fCircuitBreaked) then
    begin
      if (fNumRetries < fMaxRetries) or (fMaxRetries = -1) then
      begin
        Inc(fNumRetries);
        if Assigned(fOnRetry) then
        begin
          //can cancel next retries
          fOnRetry(fLastException,fCircuitBreaked);
          //if cancelled next retries, decrease current retries
          if fCircuitBreaked then fNumRetries := fNumRetries - 1;
          Result := not fCircuitBreaked;
        end
        else Result := True;
        //wait between retries
        if (Result) and (fMaxRetries <> 0) then
        begin
          if IsEmptyArray(fWaitTimeArray) then
          begin
            if fWaitTimeMultiplierFactor = 1 then waitretryMS := fWaitTimeBetweenRetries
              else waitretryMS := fWaitTimeBetweenRetries * Round(fNumRetries * fWaitTimeMultiplierFactor);
          end
          else waitretryMS := fWaitTimeArray[fNumRetries - 1];
          if waitretryMS > 0 then WaitBeforeRetry(waitretryMS);
        end;
      end
      else fCircuitBreaked := True;
      if fCircuitBreaked then
      begin
        if Assigned(fOnCircuitBreak) then fOnCircuitBreak;
        if Assigned(fLastException) then raise fLastException;
      end;
    end;
  end;
end;

procedure TFaultControl.Reset;
begin
  fCircuitBreaked := False;
  fTaskFailed := False;
  fNumRetries := 0;
  fLastException := nil;
end;

procedure TFaultControl.SetWaitTimeMultiplierFactor(const Value: Double);
begin
  if Value = 0 then raise EFaultControlConfigError.Create('WaitTimeMultiplierFactor cannot be 0')
    else fWaitTimeMultiplierFactor := Value;
end;

procedure TFaultControl.SuccessExecution;
begin
  fTaskFailed := False;
end;

procedure TFaultControl.WaitBeforeRetry(aNumWaitMilliseconds: Integer);
var
  fEvent : TSimpleEvent;
begin
  //Sleep(aNumWaitMilliseconds);
  {$IFDEF FPC}
  fEvent := TSimpleEvent.Create;
  {$ELSE}
  fEvent := TSimpleEvent.Create(nil,True,False,'');
  {$ENDIF}
  try
    fEvent.WaitFor(aNumWaitMilliseconds);
  finally
    fEvent.SetEvent;
    fEvent.Free;
  end;
end;

destructor TFaultControl.Destroy;
begin
  //if Assigned(fLastException) then fLastException.Free;
  inherited;
end;

procedure TFaultControl.FailedExecution(aException: Exception);
begin
  fTaskFailed := True;
  //free older exception
  if Assigned(fLastException) then fLastException.Free;
  fLastException := aException;
  if fMaxRetries = 0 then raise aException
  else if fNumRetries = fMaxRetries then
  begin
    aException.Message := Format('Max %d retries reached: %s',[fMaxRetries,aException.Message]);
    raise fLastException;
  end;
end;


{ TFaultPolicy }

constructor TFaultPolicy.Create;
begin
  fMaxRetries := 0;
  fWaitTimeBetweenRetries := 0;
  fWaitTimeMultiplierFactor := 1;
end;

end.
