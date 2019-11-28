{ ***************************************************************************

  Copyright (c) 2015-2019 Kike Pérez

  Unit        : Quick.Options
  Description : Configuration group settings
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 18/10/2019
  Modified    : 28/11/2019

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

unit Quick.Options;

{$i QuickLib.inc}

interface

uses
  Classes,
  RTTI,
  Quick.RTTI.Utils,
  System.TypInfo,
  System.SysUtils,
  System.Generics.Collections,
  System.Json,
  Quick.Commons,
  Quick.FileMonitor;

type
  Required = class(TCustomAttribute);

  TValidationCustomAttribute = class(TCustomAttribute)
  protected
    fErrorMsg : string;
  public
    property ErrorMsg : string read fErrorMsg write fErrorMsg;
  end;

  Range = class(TValidationCustomAttribute)
  private
    fRangeMin : Double;
    fRangeMax : Double;
  public
    constructor Create(aMin, aMax : Integer; const aErrorMsg : string = ''); overload;
    constructor Create(aMin, aMax : Double; const aErrorMsg : string = ''); overload;
    property Min : Double read fRangeMin write fRangeMax;
    property Max : Double read fRangeMax write fRangeMax;
  end;

  StringLength = class(TValidationCustomAttribute)
  private
    fMaxLength : Integer;
  public
    constructor Create(aMaxLength : Integer; const aErrorMsg : string = '');
    property MaxLength : Integer read fMaxLength write fMaxLength;
  end;

  TOptions = class;

  TConfigureOptionsProc<T : TOptions> = reference to procedure(aOptions : T);

  IOptionsValidator = interface
  ['{C6A09F78-8E34-4689-B943-83620437B9EF}']
    procedure ValidateOptions;
  end;

  IOptionsConfigure<T> = interface
  ['{49258BEB-A21D-4C64-BA71-767B8DBD4D92}']
    //function ConfigureOptions(aOptionsFunc : TConfigureOptionsProc<T>) : IOptionsValidator;
  end;

  TOptions = class(TInterfacedObject,IOptionsValidator)
  private
    fName : string;
    procedure ValidateRequired(aProperty : TRttiProperty);
    procedure ValidateStringLength(aProperty: TRttiProperty; aValidation : StringLength);
    procedure ValidateRange(aProperty : TRttiProperty; aValidation : Range);
    procedure DoValidateOptions; virtual;
  public
    constructor Create;
    property Name : string read fName write fName;
    procedure DefaultValues; virtual; abstract;
    function ConfigureOptions<T : TOptions>(aOptionsFunc : TConfigureOptionsProc<T>) : IOptionsValidator;
    procedure ValidateOptions;
  end;

  TOptions<T : TOptions> = record
  private
    fOptions : T;
  public
    constructor Create(aOptions : T);
    function ConfigureOptions(aOptionsFunc : TConfigureOptionsProc<T>) : IOptionsValidator;
  end;

  IOptions<T : TOptions> = interface
  ['{2779F946-2692-4F74-88AD-F35F5137057A}']
    function GetSectionValue : T;
    property Value : T read GetSectionValue;
  end;

  TOptionsClass = class of TOptions;

  IOptionsContainer = interface
  ['{A593C8BB-53CF-4AA4-9641-BF974E45CBD1}']
    function AddSection(aOption : TOptionsClass; const aOptionsName : string = '') : TOptions;
    function GetOptions(aOptionClass : TOptionsClass): TOptions;
    function GetSection(aOptionsSection : TOptionsClass; aOptions : TOptions) : Boolean; overload;
  end;

  TSectionList = TObjectList<TOptions>;

  IOptionsSerializer = interface
  ['{7DECE203-4AAE-4C9D-86C8-B3D583DF7C8B}']
    function Load(const aFilename : string; aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean;
    procedure Save(const aFilename : string; aSections : TSectionList);
  end;

  TOptionsSerializer = class(TInterfacedObject,IOptionsSerializer)
  public
    function Load(const aFilename : string; aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean; virtual; abstract;
    procedure Save(const aFilename : string; aSections : TSectionList); virtual; abstract;
  end;

  TFileModifiedEvent = reference to procedure;
  TLoadConfigEvent = reference to procedure;

  TOptionValue<T : TOptions> = class(TInterfacedObject,IOptions<T>)
  private
    fValue : T;
    function GetSectionValue : T;
  public
    constructor Create(aValue : T);
    property Value : T read GetSectionValue;
  end;

  TOptionsContainer = class(TInterfacedObject,IOptionsContainer)
  private
    fFilename : string;
    fSerializer : IOptionsSerializer;
    fSections : TSectionList;
    fFileMonitor : TFileMonitor;
    fOnFileModified : TFileModifiedEvent;
    fLoaded : Boolean;
    fReloadIfFileChanged : Boolean;
    fOnConfigLoaded : TLoadConfigEvent;
    fOnConfigReloaded : TLoadConfigEvent;
    procedure CreateFileMonitor;
    procedure FileModifiedNotify(MonitorNotify : TMonitorNotify);
    procedure SetReloadIfFileChanged(const Value: Boolean);
    function GetOptions(aOptionClass : TOptionsClass): TOptions; overload;
    function GetOptions(aIndex : Integer) : TOptions; overload;
    function GetSection(aOptionsSection : TOptionsClass; aOptions : TOptions) : Boolean; overload;
  public
    constructor Create(const aFilename : string; aOptionsSerializer : IOptionsSerializer; aReloadIfFileChanged : Boolean = False);
    destructor Destroy; override;
    property FileName : string read fFilename write fFilename;
    property ReloadIfFileChanged : Boolean read fReloadIfFileChanged write SetReloadIfFileChanged;
    property IsLoaded : Boolean read fLoaded;
    property OnFileModified : TFileModifiedEvent read fOnFileModified write fOnFileModified;
    property OnConfigLoaded : TLoadConfigEvent read fOnConfigLoaded write fOnConfigLoaded;
    property OnConfigReloaded : TLoadConfigEvent read fOnConfigReloaded write fOnConfigReloaded;
    property Items[aOptionClass : TOptionsClass] : TOptions read GetOptions; default;
    property Items[aIndex : Integer] : TOptions read GetOptions; default;
    function AddSection(aOption : TOptionsClass; const aSectionName : string = '') : TOptions; overload;
    function AddSection<T : TOptions>(const aSectionName : string = '') : TOptions<T>; overload;
    function GetSectionInterface<T : TOptions> : IOptions<T>;
    function GetSection<T : TOptions>(const aSectionName : string = '') : T; overload;
    function Count : Integer;
    procedure Load(aFailOnSectionNotExists : Boolean = False);
    procedure Save;
  end;

  IOptionsBuilder<T : TOptions> = interface
  ['{1A1DC9A9-7F2D-4CC4-A772-6C7DBAB34424}']
    function Options : T;
  end;

  TOptionsBuilder<T : TOptions> = class(TInterfacedObject,IOptionsBuilder<T>)
  protected
    fOptions : T;
  public
    constructor Create;
    function Options : T;
  end;

  EOptionConfigureError = class(Exception);
  EOptionLoadError = class(Exception);
  EOptionSaveError = class(Exception);
  EOptionValidationError = class(Exception);

implementation

{ TOptionsContainer }

constructor TOptionsContainer.Create(const aFilename : string; aOptionsSerializer : IOptionsSerializer; aReloadIfFileChanged : Boolean = False);
begin
  fSerializer := aOptionsSerializer;
  fSections := TSectionList.Create(True);
  fFilename := aFilename;
  fLoaded := False;
  fReloadIfFileChanged := aReloadIfFileChanged;
  if aReloadIfFileChanged then CreateFileMonitor;
end;

procedure TOptionsContainer.CreateFileMonitor;
begin
  fFileMonitor := TQuickFileMonitor.Create;
  fFileMonitor.FileName := fFilename;
  fFileMonitor.Interval := 2000;
  fFileMonitor.Notifies := [TMonitorNotify.mnFileModified];
  fFileMonitor.OnFileChange := FileModifiedNotify;
  fFileMonitor.Enabled := True;
end;

destructor TOptionsContainer.Destroy;
begin
  if Assigned(fFileMonitor) then fFileMonitor.Free;
  fSerializer := nil;
  fSections.Free;
  inherited;
end;

procedure TOptionsContainer.FileModifiedNotify(MonitorNotify: TMonitorNotify);
begin
  if MonitorNotify = TMonitorNotify.mnFileModified then
  begin
    if Assigned(fOnFileModified) then fOnFileModified;
    if fReloadIfFileChanged then
    begin
      Load(False);
    end;
  end;
end;

function TOptionsContainer.AddSection(aOption : TOptionsClass; const aSectionName : string = '') : TOptions;
var
  option : TOptions;
begin
  option := aOption.Create;
  if aSectionName.IsEmpty then option.Name := Copy(aOption.ClassName,2,aOption.ClassName.Length)
    else option.Name := aSectionName;
  fSections.Add(option);
  Result := option;
end;

function TOptionsContainer.AddSection<T>(const aSectionName: string): TOptions<T>;
var
  option : TOptions;
begin
  option := TRTTI.CreateInstance<T>;
  if aSectionName.IsEmpty then option.Name := Copy(T.ClassName,2,T.ClassName.Length)
    else option.Name := aSectionName;
  fSections.Add(option);
  Result.Create(option);
end;

function TOptionsContainer.Count: Integer;
begin
  Result := fSections.Count;
end;

function TOptionsContainer.GetOptions(aIndex: Integer): TOptions;
begin
  Result := fSections[aIndex];
end;

function TOptionsContainer.GetSection(aOptionsSection: TOptionsClass; aOptions: TOptions): Boolean;
var
  option : TOptions;
begin
  Result := False;
  for option in fSections do
  begin
    if option is TOptionsClass then
    begin
      aOptions := option as TOptionsClass;
      Exit;
    end;
  end;
end;

function TOptionsContainer.GetOptions(aOptionClass : TOptionsClass) : TOptions;
var
  option : TOptions;
begin
  Result := nil;
  for option in fSections do
  begin
    if option is TOptionsClass then Result := option as TOptionsClass;
  end;
end;

function TOptionsContainer.GetSection<T>(const aSectionName : string = '') : T;
var
  option : TOptions;
begin
  for option in fSections do
  begin
    if option is T then
    begin
      if (aSectionName.IsEmpty) or (CompareText(option.Name,aSectionName) = 0) then
      begin
        Result := option as T;
        Exit;
      end;
    end;
  end;
end;

function TOptionsContainer.GetSectionInterface<T>: IOptions<T>;
begin
  Result := TOptionValue<T>.Create(Self.GetSection<T>);
end;

procedure TOptionsContainer.Load(aFailOnSectionNotExists : Boolean = False);
var
  option : TOptions;
begin
  if FileExists(fFilename) then
  begin
    if not fSerializer.Load(fFilename,fSections,aFailOnSectionNotExists) then Save;
    if not fLoaded then
    begin
      fLoaded := True;
      if Assigned(fOnConfigLoaded) then fOnConfigLoaded;
    end
    else if Assigned(fOnConfigReloaded) then fOnConfigReloaded;
  end
  else
  begin
    //if not exists file get default values
    for option in fSections do option.DefaultValues;
    //creates default file
    Save;
  end;
end;

procedure TOptionsContainer.Save;
var
  laststate : Boolean;
begin
  //disable filemonitor to avoid detect manual save as a external file change
  laststate := fFileMonitor.Enabled;
  fFileMonitor.Enabled := False;
  try
    //save config file
    fSerializer.Save(fFilename,fSections);
  finally
    //set last state
    fFileMonitor.Enabled := laststate;
  end;
end;

procedure TOptionsContainer.SetReloadIfFileChanged(const Value: Boolean);
begin
  if Value = fReloadIfFileChanged then Exit;
  fReloadIfFileChanged := Value;
  if Assigned(fFileMonitor) then fFileMonitor.Free;
  if fReloadIfFileChanged then CreateFileMonitor;
end;

{ TOptions }

function TOptions.ConfigureOptions<T>(aOptionsFunc: TConfigureOptionsProc<T>): IOptionsValidator;
var
  value : TValue;
begin
  Result := Self;
  if Assigned(aOptionsFunc) then
  begin
    value := Self;
    aOptionsFunc(value.AsType<T>);
  end;
end;

constructor TOptions.Create;
begin
  fName := '';
end;

procedure TOptions.DoValidateOptions;
var
  ctx : TRttiContext;
  rtype : TRttiType;
  rprop : TRttiProperty;
  attrib : TCustomAttribute;
begin
  ctx := TRttiContext.Create;
  try
    rtype := ctx.GetType(Self.ClassInfo);
    for rprop in rtype.GetProperties do
    begin
      //check only published properties
      if rprop.Visibility = TMemberVisibility.mvPublished then
      begin
        //check validation option attributes
        for attrib in rprop.GetAttributes do
        begin
          if attrib is Required  then ValidateRequired(rprop)
          else if attrib is StringLength then ValidateStringLength(rprop,StringLength(attrib))
          else if attrib is Range then ValidateRange(rprop,Range(attrib));
        end;
      end;
    end;
  finally
    ctx.Free;
  end;
end;

procedure TOptions.ValidateOptions;
begin
  try
    DoValidateOptions;
  except
    on E : Exception do
    begin
      raise EOptionConfigureError.CreateFmt('Validation Options Error : %s',[e.Message]);
    end;
  end;
end;

procedure TOptions.ValidateRange(aProperty: TRttiProperty; aValidation : Range);
var
  value : TValue;
  msg : string;
begin
  value := aProperty.GetValue(Self);
  if not value.IsEmpty then
  begin
    if value.Kind = tkFloat then
    begin
      if (value.AsExtended < aValidation.Min) or (value.AsExtended > aValidation.Max) then
      begin
        if aValidation.ErrorMsg.IsEmpty then msg := Format('Option "%s.%s" exceeds predefined range (%2f - %2f)',[Self.Name,aProperty.Name,aValidation.Min,aValidation.Max])
          else msg := aValidation.ErrorMsg;
        raise EOptionValidationError.Create(msg);
      end;
    end
    else if value.Kind in [tkInteger,tkInt64] then
    begin
      if (value.AsInt64 < aValidation.Min) or (value.AsInt64 > aValidation.Max) then
      begin
        if aValidation.ErrorMsg.IsEmpty then msg := Format('Option "%s.%s" exceeds predefined range (%d - %d)',[Self.Name,aProperty.Name,Round(aValidation.Min),Round(aValidation.Max)])
          else msg := aValidation.ErrorMsg;
        raise EOptionValidationError.Create(msg);
      end;
    end;
  end;
end;

procedure TOptions.ValidateRequired(aProperty: TRttiProperty);
begin
  if aProperty.GetValue(Self).IsEmpty then raise EOptionValidationError.CreateFmt('Option "%s.%s" is required',[Self.Name,aProperty.Name]);
end;

procedure TOptions.ValidateStringLength(aProperty: TRttiProperty; aValidation : StringLength);
var
  value : TValue;
  msg : string;
begin
  value := aProperty.GetValue(Self);
  if (not value.IsEmpty) and (value.AsString.Length > aValidation.MaxLength) then
  begin
    if aValidation.ErrorMsg.IsEmpty then msg := Format('Option "%s.%s" exceeds max length (%d)',[Self.Name,aProperty.Name,aValidation.MaxLength])
      else msg := aValidation.ErrorMsg;

    raise EOptionValidationError.Create(msg);
  end;
end;

{ Range }

constructor Range.Create(aMin, aMax: Integer; const aErrorMsg : string = '');
begin
  fRangeMin := aMin;
  fRangeMax := aMax;
  fErrorMsg := aErrorMsg;
end;

constructor Range.Create(aMin, aMax: Double; const aErrorMsg: string);
begin
  fRangeMin := aMin;
  fRangeMax := aMax;
  fErrorMsg := aErrorMsg;
end;

{ StringLength }

constructor StringLength.Create(aMaxLength: Integer; const aErrorMsg : string = '');
begin
  fMaxLength := aMaxLength;
  fErrorMsg := aErrorMsg;
end;


{ TOptionValue<T> }

constructor TOptionValue<T>.Create(aValue: T);
begin
  fValue := aValue;
end;

function TOptionValue<T>.GetSectionValue: T;
begin
  Result := fValue;
end;

{ TOptions<T> }

function TOptions<T>.ConfigureOptions(aOptionsFunc: TConfigureOptionsProc<T>): IOptionsValidator;
begin
  if Assigned(aOptionsFunc) then Result := fOptions.ConfigureOptions<T>(aOptionsFunc)
    else Result := fOptions;
  fOptions._AddRef;
end;

constructor TOptions<T>.Create(aOptions: T);
begin
  fOptions := aOptions;
end;

{ TOptionsBuilder<T> }

constructor TOptionsBuilder<T>.Create;
begin
  fOptions := (PTypeInfo(TypeInfo(T)).TypeData.ClassType.Create) as T;
end;

function TOptionsBuilder<T>.Options: T;
begin
  Result := fOptions;
end;

end.
