{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.Data.Base
  Description : Data Metrics
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 08/04/2019
  Modified    : 08/04/2019

  This file is part of QuickLogger: https://github.com/exilon/QuickLogger

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

unit Quick.Data.Custom;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  Quick.Commons;

type

  TOutputOptions = class
  private
    fUseUTCTime : Boolean;
  public
    property UseUTCTime : Boolean read fUseUTCTime write fUseUTCTime;
  end;

  IDataProvider = interface
  ['{D4D03D84-9BD7-49A0-8A46-0E89E0988F58}']
    function Send(const aName, aValue : string): Boolean; overload;
    function Send(const aName : string; aValue : Integer): Boolean; overload;
    function Send(const aName : string; aValue : Extended): Boolean; overload;
    function Send(const aName : string; aValue : TDateTime): Boolean; overload;
  end;

  TDataProvider = class(TInterfacedObject,IDataProvider)
  private
    fOutputOptions : TOutputOptions;
  protected
    fInitiated : Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property OutputOptions : TOutputOptions read fOutputOptions write fOutputOptions;
    procedure Init; virtual;
    procedure Stop; virtual;
    procedure Restart; virtual; abstract;
    function Send(const aName, aValue : string): Boolean; overload; virtual; abstract;
    function Send(const aName : string; aValue : Integer): Boolean; overload; virtual; abstract;
    function Send(const aName : string; aValue : Extended): Boolean; overload; virtual; abstract;
    function Send(const aName : string; aValue : TDateTime): Boolean; overload; virtual; abstract;
  end;

const
  DEF_USER_AGENT = 'Quick.Data.Base Agent';

implementation

{ TDataProvider }

constructor TDataProvider.Create;
begin
  fOutputOptions := TOutputOptions.Create;
  fInitiated := False;
end;

destructor TDataProvider.Destroy;
begin
  fOutputOptions.Free;
  fInitiated := False;
  inherited;
end;

procedure TDataProvider.Init;
begin
  fInitiated := True;
end;

procedure TDataProvider.Stop;
begin
  fInitiated := False;
end;

end.
