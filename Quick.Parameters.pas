{ ***************************************************************************

  Copyright (c) 2016-2021 Kike Pérez

  Unit        : Quick.Parameters
  Description : Map comandline to class
  Author      : Kike Pérez
  Version     : 1.4
  Created     : 12/07/2020
  Modified    : 01/08/2021

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
 
unit Quick.Parameters;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Generics.Collections,
  Quick.Commons,
  {$IFDEF CONSOLE}
  Quick.Console,
  {$ENDIF}
  rtti,
  TypInfo,
  Quick.RTTI.Utils;

type

  CommandDescription = class(TCustomAttribute)
  private
    fDescription : string;
  public
    constructor Create(const aDescription : string);
    property Description : string read fDescription;
  end;

  ParamCommand = class(TCustomAttribute)
  private
    fPosition : Integer;
  public
    constructor Create(aPosition : Integer);
    property Position : Integer read fPosition;
  end;

  ParamName = class(TCustomAttribute)
  private
    fName : string;
    fAlias : string;
  public
    constructor Create(const aName: string; const aAlias : string = '');
    property Name : string read fName;
    property Alias : string read fAlias;
  end;

  ParamValueIsNextParam = class(TCustomAttribute);

  ParamHelp = class(TCustomAttribute)
  private
    fHelp : string;
    fValueName : string;
  public
    constructor Create(const aHelp : string; const aValueName : string = '');
    property Help : string read fHelp;
    property ValueName : string read fValueName;
  end;

  ParamSwitchChar = class(TCustomAttribute)
  private
    fSwithChar : string;
  public
    constructor Create(const aSwitchChar : string);
    property SwitchChar : string read fSwithChar write fSwithChar;
  end;

  ParamValueSeparator = class(TCustomAttribute)
  private
    fValueSeparator : string;
  public
    constructor Create(const aValueSeparator : string);
    property ValueSeparator : string read fValueSeparator write fValueSeparator;
  end;

  ParamRequired = class(TCustomAttribute);

  {$IFDEF CONSOLE}
  TColorizeHelp = class
  private
    fCommandName : TConsoleColor;
    fCommandDescription : TConsoleColor;
    fCommandUsage : TConsoleColor;
    fSections : TConsoleColor;
    fArgumentName : TConsoleColor;
    fArgumentDescription : TConsoleColor;
  public
    property CommandName : TConsoleColor read fCommandName write fCommandName;
    property CommandDescription : TConsoleColor read fCommandDescription write fCommandDescription;
    property CommandUsage : TConsoleColor read fCommandUsage write fCommandUsage;
    property Sections : TConsoleColor read fSections write fSections;
    property ArgumentName : TConsoleColor read fArgumentName write fArgumentName;
    property ArgumentDescription : TConsoleColor read fArgumentDescription write fArgumentDescription;
  end;
  {$ENDIF}

  {$M+}
  TParameters = class
  type
    TValueType = (vtString, vtInteger, vtFloat, vtBoolean, vtEnumeration);
    TParam = class
    private
      fName : string;
      fAlias : string;
      fValue : string;
      fPrecisePosition : Integer;
      fParamType: TValueType;
      fRequired : Boolean;
      fHelp : string;
      fValueName : string;
      fValueIsNextParam: Boolean;
      fSwitchChar: string;
      fValueSeparator: string;
      fIsPresent: Boolean;
    public
      constructor Create;
      property Name : string read fName write fName;
      property Alias : string read fAlias write fAlias;
      property Value : string read fValue write fValue;
      property PrecisePosition : Integer read fPrecisePosition write fPrecisePosition;
      property ParamType : TValueType read fParamType write fParamType;
      property Required : Boolean read fRequired write fRequired;
      property SwitchChar : string read fSwitchChar write fSwitchChar;
      property ValueSeparator : string read fValueSeparator write fValueSeparator;
      property Help : string read fHelp write fHelp;
      property HepValueName : string read fValueName write fValueName;
      property ValueIsNextParam : Boolean read fValueIsNextParam write fValueIsNextParam;
      property IsPresent : Boolean read fIsPresent write fIsPresent;
      function IsSwitch : Boolean;
      function IsCommand : Boolean;
      function ValueIsSwitch : Boolean;
    end;
  private
    fParams : TObjectList<TParam>;
    fDescription : string;
    fHelp: Boolean;
    {$IFDEF CONSOLE}
    fColorizeHelp: TColorizeHelp;
    {$ENDIF}
    function ExistParam(aParameter : TParam; const aParam : string) : Boolean; overload;
    function GetParamName(aParameter : TParam; const aParam : string) : string;
    function GetParamValue(aParameter : TParam; const aParam : string) : string;
    function ValueType(const aProp : TRttiProperty) : TValueType;
    procedure ParseParams;
    function CheckHelpSwitch : Boolean;
  protected
  {$IFDEF CONSOLE}
    procedure GetColors; virtual;
  {$ENDIF}
    procedure Validate; virtual;
  public
    constructor Create(aAutoHelp : Boolean = True); virtual;
    destructor Destroy; override;
    property Description : string read fDescription write fDescription;
    {$IFDEF CONSOLE}
    property ColorizeHelp : TColorizeHelp read fColorizeHelp write fColorizeHelp;
    procedure ShowHelp; virtual;
    {$ENDIF}
    function GetHelp : TStringList;
    property Help : Boolean read fHelp write fHelp;
    function ExistsParam(const aParam : string): Boolean; overload;
  end;
  {$M-}

  TServiceParameters = class(TParameters)
  private
    fInstance : string;
    fInstall : Boolean;
    fRemove : Boolean;
    fConsole : Boolean;
  published
    [ParamHelp('Install service with a custom name','Service name')]
    property Instance : string read fInstance write fInstance;

    [ParamHelp('Install as a service')]
    property Install : Boolean read fInstall write fInstall;

    [ParamHelp('Remove service')]
    property &Remove : Boolean read fRemove write fRemove;

    [ParamHelp('Force run as a console application (when runned from another service)')]
    property Console : Boolean read fConsole write fConsole;
  end;

  ENotValidCommandlineParameter = class(Exception);
  ERequiredParameterNotFound = class(Exception);
  EParameterValueNotFound = class(Exception);
  EParamValueNotSupported = class(Exception);

implementation

{ TParameter }

constructor TParameters.Create(aAutoHelp : Boolean = True);
begin
  {$IFDEF CONSOLE}
  fColorizeHelp := TColorizeHelp.Create;
  GetColors;
  {$ENDIF}
  fParams := TObjectList<TParam>.Create(True);
  ParseParams;
  {$IFDEF CONSOLE}
  if (aAutoHelp) and (fHelp) then
  begin
    ShowHelp;
    Halt;
  end;
  {$ENDIF}
  Validate;
end;

destructor TParameters.Destroy;
begin
  fParams.Free;
  {$IFDEF CONSOLE}
  if Assigned(fColorizeHelp) then fColorizeHelp.Free;
  fColorizeHelp := nil;
  {$ENDIF}
  inherited;
end;

function TParameters.ExistParam(aParameter : TParam; const aParam : string) : Boolean;
var
  i : Integer;
  parName : string;
begin
  Result := False;
  if aParam.IsEmpty then Exit;
  for i := 1 to ParamCount do
  begin
    parName := ParamStr(i);
    if parName = aParameter.ValueSeparator then raise ENotValidCommandlineParameter.CreateFmt('Not valid commandline "%s"', [parName]);
    parName := GetParamName(aParameter,ParamStr(i));
    if CompareText(parName,aParam) = 0 then Exit(True);
  end;
end;

function TParameters.GetParamName(aParameter : TParam; const aParam : string) : string;
var
  switch : string;
begin
  if CompareText(aParam,'-' + aParameter.Alias) = 0 then switch := '-'
    else switch := aParameter.SwitchChar;

  if aParam.StartsWith(switch) then Result := aParam.Substring(switch.Length);
  if Result.Contains(aParameter.ValueSeparator) then Result := Result.Substring(0,Result.IndexOf(aParameter.ValueSeparator));
end;

function TParameters.GetParamValue(aParameter : TParam; const aParam : string) : string;
var
  i : Integer;
  parName : string;
  param : string;
begin
  Result := '';
  for i := 1 to ParamCount do
  begin
    param := ParamStr(i);
    parName := GetParamName(aParameter,param);
    if CompareText(parName,aParam) = 0 then
    begin
      if aParameter.ValueIsNextParam then
      begin
        if i < ParamCount then Result := ParamStr(i+1);
      end
      else
      begin
        if param.Contains(aParameter.ValueSeparator) then Result := param.Substring(param.IndexOf(aParameter.ValueSeparator)+(aParameter.ValueSeparator.Length));
      end;
      Exit;
    end;
  end;
end;

function TParameters.CheckHelpSwitch: Boolean;
var
  param : TParam;
begin
  param := TParam.Create;
  param.Name := 'help';
  param.Alias := 'h';
  try
    Result := ExistParam(param,param.Name);
  finally
    param.Free;
  end;
end;

function TParameters.ExistsParam(const aParam : string): Boolean;
var
  param : TParam;
begin
  param := TParam.Create;
  param.Name := aParam;
  param.Alias := '';
  try
    Result := ExistParam(param,param.Name);
  finally
    param.Free;
  end;
end;

procedure TParameters.ParseParams;
var
  param : TParam;
  value : TValue;
  valueint : Int64;
  valuefloat : Extended;
  rType : TRttiType;
  rProp : TRttiProperty;
  attr : TCustomAttribute;
  pinfo : PTypeInfo;
  found : Boolean;
begin
  fHelp := CheckHelpSwitch;
  rType := TRTTI.GetType(Self.ClassInfo);
  //get main info
  for attr in rType.GetAttributes do
  begin
    if attr is CommandDescription then Self.Description := CommandDescription(attr).Description;
  end;
  //get parameters
  for rProp in TRTTI.GetProperties(rType,TRttiPropertyOrder.roFirstBase) do
  begin
    if rProp.Visibility <> TMemberVisibility.mvPublished then continue;
    param := TParam.Create;
    fParams.Add(param);
    param.Name := rProp.Name;
    for attr in rProp.GetAttributes do
    begin
      if attr is ParamHelp then
      begin
        param.Help := ParamHelp(attr).Help;
        param.HepValueName := ParamHelp(attr).ValueName;
      end;
      if attr is ParamName then
      begin
        param.Name := ParamName(attr).Name;
        param.Alias := ParamName(attr).Alias;
      end;
      if attr is ParamCommand then param.PrecisePosition := ParamCommand(attr).Position;      
      if attr is ParamRequired then param.Required := True;
      if attr is ParamSwitchChar then param.SwitchChar := ParamSwitchChar(attr).SwitchChar;
      if attr is ParamValueSeparator then param.ValueSeparator := ParamValueSeparator(attr).ValueSeparator;
      if attr is ParamValueIsNextParam then param.ValueIsNextParam := True;
      
    end;
    param.ParamType := ValueType(rProp);
    if param.IsCommand then
    begin
      found := ParamCount >= param.PrecisePosition;
      param.SwitchChar := '  ';
      if param.ValueIsSwitch then found := False;
    end
    else found := (ExistParam(param,param.Name)) or (ExistParam(param,param.Alias));
    value := nil;
    if found then
    begin
      if param.IsSwitch then
      begin
        value := True;
      end
      else
      begin
        if param.IsCommand then param.Value := ParamStr(param.PrecisePosition)
          else param.Value := GetParamValue(param,param.Name);
        if (param.Value.IsEmpty) and (not param.Alias.IsEmpty) then param.Value := GetParamValue(param,param.Alias);

        if (not param.Value.IsEmpty) and (not fHelp) then
        case param.ParamType of
          TValueType.vtString :
            begin
              value := param.Value;
            end;
          TValueType.vtInteger :
            begin
              if not TryStrToInt64(param.Value,valueint) then raise EParamValueNotSupported.CreateFmt('Parameter "%s" needs a numeric value',[param.Name]);
              value := valueint;
            end;
          TValueType.vtFloat :
            begin
             if not TryStrToFloat(param.Value,valuefloat) then raise EParamValueNotSupported.CreateFmt('Parameter "%s" needs a float value',[param.Name]);
             value := valuefloat;
            end;
          TValueType.vtEnumeration :
            begin
              pinfo := TRTTI.GetPropertyValue(Self,param.Name).TypeInfo;
              if not IsInteger(param.Value) then TValue.Make(GetEnumValue(pinfo,param.Value),pinfo,value)
                else TValue.Make(StrToInt(param.Value),pinfo,value);
            end;
        end;
      end;
      param.IsPresent := True;
      if not value.IsEmpty then rProp.SetValue(Self,value);
    end;
  end;
  //add help
  param := TParam.Create;
  param.Name := 'Help';
  param.Alias := 'h';
  param.ParamType := TValueType.vtBoolean;
  param.Help := 'Show this documentation';
  fParams.Add(param);
end;

procedure TParameters.Validate;
var
  param : TParam;
begin
  if help then Exit;

  for param in fParams do
  begin
    if param.IsPresent then
    begin
      if (not param.IsSwitch) and (param.Value.IsEmpty) then raise EParamValueNotSupported.CreateFmt('Value for parameter "%s" not specified',[param.Name]);
    end
    else
    begin
      if param.Required then raise ERequiredParameterNotFound.CreateFmt('Required parameter "%s" not found',[param.Name]);
    end;
  end;
end;

function TParameters.ValueType(const aProp: TRttiProperty): TValueType;
var
  rType : TRttiType;
begin
  rType := aProp.PropertyType;
  case rType.TypeKind of
    tkString, tkWideString, tkChar, tkUnicodeString : Result := TValueType.vtString;
    tkInteger, tkInt64 : Result := TValueType.vtInteger;
    tkFloat : Result := TValueType.vtFloat;
    tkEnumeration :
      begin
        if TRTTI.GetPropertyValue(Self,aProp.Name).TypeInfo = System.TypeInfo(Boolean) then Result := TValueType.vtBoolean
          else Result := TValueType.vtEnumeration;
      end;
    else raise EParamValueNotSupported.CreateFmt('Parameter "%s": Value not supported',[aProp.Name]);
  end;
end;

{$IFDEF CONSOLE}
procedure TParameters.ShowHelp;
var
  version : string;
  arg : string;
  value : string;
  usage : string;
  commands : string;
  param : TParam;
  maxlen : Integer;
  arglen : Integer;
begin
  //show app and version
  version := GetAppVersionStr;
  if version.IsEmpty then cout(GetAppName,fColorizeHelp.CommandName)
    else cout(Format('%s v.%s',[GetAppName,GetAppVersionStr]),fColorizeHelp.CommandName);
  usage := '';
  maxlen := 0;
  commands := '';
  //show usage
  arglen := 0;
  for param in fParams do
  begin
    if (param.Name.Length + param.Alias.Length) > maxlen then maxlen := param.Name.Length + param.Alias.Length;

    if param.Required then arg := '<' + param.SwitchChar + param.Name +'%s>'
      else arg := '[' + param.SwitchChar + param.Name + '%s]';

    if param.IsSwitch then
    begin
      arg := Format(arg,['']);
    end
    else if param.IsCommand then
    begin
      if param.HepValueName.IsEmpty then value := param.Name
        else value := param.HepValueName;
      if param.Required then  commands := commands + Format('<%s> ',[value])
        else commands := commands + Format('[%s] ',[value]);
      Continue;
    end
    else
    begin
      if param.ValueIsNextParam then value := ' <value>'
        else value := param.ValueSeparator + '<value>';
      if not param.HepValueName.IsEmpty then value := StringReplace(value,'value',param.HepValueName,[rfIgnoreCase,rfReplaceAll]);
      arg := Format(arg,[value]);
    end; 

    //fit usage line
    arglen := arglen + arg.Length;
    if arglen > 80 then
    begin
      usage := usage + #10 + FillStr(' ',8 + (GetAppName.Length));
      arglen := arg.Length;
    end;
        
    usage := usage + arg + ' ';
  end;
    
  maxlen := maxlen + 5;
  coutSL('Usage: ',fColorizeHelp.Sections); 
  coutSL(Format('%s %s%s',[GetAppName,commands,usage]),fColorizeHelp.CommandUsage);
  cout('',ccWhite);
  cout('',ccWhite);
  //show description
  cout(Description,fColorizeHelp.CommandDescription);
  cout('',ccWhite);
  //show arguments
  cout('Arguments:',fColorizeHelp.Sections);
  cout('',ccWhite);
  for param in fParams do
  begin
    //if param.IsCommand then Continue;
    
    if param.Alias.IsEmpty then
    begin
      coutSL(Format('  %s%s%s',[param.SwitchChar,param.Name,FillStr(' ',maxlen - param.Name.Length)]),fColorizeHelp.ArgumentName);
    end
    else
    begin
      coutSL(Format('  %s%s, -%s%s',[param.SwitchChar,param.Name,param.Alias,FillStr(' ',maxlen - (param.Name.Length + param.Alias.Length + 3))]),fColorizeHelp.ArgumentName);
    end;
    coutSL(param.Help,fColorizeHelp.ArgumentDescription);
    cout('',ccWhite);
  end;
  cout('',ccWhite);
end;

procedure TParameters.GetColors;
begin
  fColorizeHelp.CommandName := ccLightCyan;
  fColorizeHelp.CommandDescription := ccDarkGray;
  fColorizeHelp.CommandUsage := ccLightGray;
  fColorizeHelp.fSections := ccWhite;
  fColorizeHelp.ArgumentName := ccWhite;
  fColorizeHelp.ArgumentDescription := ccLightGray;
end;
{$ENDIF}

function TParameters.GetHelp : TStringList;
var
  line : string;
  version : string;
  arg : string;
  value : string;
  usage : string;
  commands : string;
  param : TParam;
  maxlen : Integer;
  arglen : Integer;
begin
  Result := TStringList.Create;
  line := '';
  //show app and version
  version := GetAppVersionStr;
  if version.IsEmpty then Result.Add(GetAppName)
    else Result.Add(Format('%s v.%s',[GetAppName,GetAppVersionStr]));
  usage := '';
  maxlen := 0;
  commands := '';
  //show usage
  arglen := 0;
  for param in fParams do
  begin
    if (param.Name.Length + param.Alias.Length) > maxlen then maxlen := param.Name.Length + param.Alias.Length;

    if param.Required then arg := '<' + param.SwitchChar + param.Name +'%s>'
      else arg := '[' + param.SwitchChar + param.Name + '%s]';

    if param.IsSwitch then
    begin
      arg := Format(arg,['']);
    end
    else if param.IsCommand then
    begin
      if param.HepValueName.IsEmpty then value := param.Name
        else value := param.HepValueName;
      if param.Required then  commands := commands + Format('<%s> ',[value])
        else commands := commands + Format('[%s] ',[value]);
      Continue;
    end
    else
    begin
      if param.ValueIsNextParam then value := ' <value>'
        else value := param.ValueSeparator + '<value>';
      if not param.HepValueName.IsEmpty then value := StringReplace(value,'value',param.HepValueName,[rfIgnoreCase,rfReplaceAll]);
      arg := Format(arg,[value]);
    end;

    //fit usage line
    arglen := arglen + arg.Length;
    if arglen > 80 then
    begin
      usage := usage + #10 + FillStr(' ',8 + (GetAppName.Length));
      arglen := arg.Length;
    end;

    usage := usage + arg + ' ';
  end;

  maxlen := maxlen + 5;
  Result.Add(Format('Usage: %s %s%s',[GetAppName,commands,usage]));
  Result.Add('');
  Result.Add('');
  //show description
  Result.Add(Description);
  Result.Add('');
  //show arguments
  Result.Add('Arguments:');
  Result.Add('');
  for param in fParams do
  begin
    //if param.IsCommand then Continue;
    line := '';
    if param.Alias.IsEmpty then
    begin
      line := line + Format('  %s%s%s',[param.SwitchChar,param.Name,FillStr(' ',maxlen - param.Name.Length)]);
    end
    else
    begin
      line := line + Format('  %s%s, -%s%s',[param.SwitchChar,param.Name,param.Alias,FillStr(' ',maxlen - (param.Name.Length + param.Alias.Length + 3))]);
    end;
    line := line + param.Help;
    Result.Add(line);
    Result.Add('');
  end;
end;

{ CommandDescription }

constructor CommandDescription.Create(const aDescription: string);
begin
  fDescription := aDescription;
end;

{ ParamName }

constructor ParamName.Create(const aName: string; const aAlias : string = '');
begin
  fName := aName;
  fAlias := aAlias;
end;

{ ParamHelp }

constructor ParamHelp.Create(const aHelp : string; const aValueName : string = '');
begin
  fHelp := aHelp;
  if not aValueName.IsEmpty then fValueName := aValueName
    else fValueName := 'value';
end;

{ TParameters.TParam }

constructor TParameters.TParam.Create;
begin
  IsPresent := False;
  fSwitchChar := '--';
  fValueSeparator := '=';
  fPrecisePosition := 0;
end;

function TParameters.TParam.IsCommand: Boolean;
begin
  Result := fPrecisePosition > 0;
end;

function TParameters.TParam.IsSwitch: Boolean;
begin
  Result := fParamType = TValueType.vtBoolean;
end;

function TParameters.TParam.ValueIsSwitch: Boolean;
begin
  Result := (fValue.StartsWith('/')) or (fValue.StartsWith('-')) or (fValue.StartsWith(fSwitchChar));
end;

{ ParamSwitchChar }

constructor ParamSwitchChar.Create(const aSwitchChar: string);
begin
  fSwithChar := aSwitchChar;
end;

{ ParamValueSeparator }

constructor ParamValueSeparator.Create(const aValueSeparator: string);
begin
  fValueSeparator := aValueSeparator;
end;

{ ParamCommand }

constructor ParamCommand.Create(aPosition: Integer);
begin
  fPosition := aPosition;
end;

end.
