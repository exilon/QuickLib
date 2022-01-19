{ ***************************************************************************

  Copyright (c) 2015-2021 Kike Pérez

  Unit        : Quick.Options
  Description : Configuration group settings
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 18/10/2019
  Modified    : 15/12/2021

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
  System.TypInfo,
  System.SysUtils,
  System.Generics.Collections,
  Quick.RTTI.Utils,
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

  TOptionsBase = class(TInterfacedObject)

  end;

  {$IFDEF DELPHIRX102_UP}
  {$M+}
  TOptions = class;
  {$M-}

  TConfigureOptionsProc<T : TOptions> = reference to procedure(aOptions : T);
  {$ELSE}
  TConfigureOptionsProc<T> = reference to procedure(aOptions : T);
  {$ENDIF}

  IOptionsValidator = interface
  ['{C6A09F78-8E34-4689-B943-83620437B9EF}']
    procedure ValidateOptions;
  end;

  {$M+}
  TOptions = class(TInterfacedObject,IOptionsValidator)
  private
    fName : string;
    fHideOptions : Boolean;
    procedure DoValidateOptions; virtual;
  public
    constructor Create; virtual;
    property Name : string read fName write fName;
    property HideOptions : Boolean read fHideOptions write fHideOptions;
    procedure DefaultValues; virtual;
    {$IFDEF DELPHIRX102_UP}
    function ConfigureOptions<T : TOptions>(aOptionsFunc : TConfigureOptionsProc<T>) : IOptionsValidator;
    {$ELSE}
    function ConfigureOptions<T>(aOptionsFunc : TConfigureOptionsProc<T>) : IOptionsValidator;
    {$ENDIF}
    procedure ValidateOptions;
  end;
  {$M-}

  TOptionsValidator = class(TInterfacedObject,IOptionsValidator)
  private
    fOptions : TOptions;
  public
    constructor Create(aOptions : TOptions);
    procedure ValidateRequired(const aInstance : TObject; aProperty: TRttiProperty);
    procedure ValidateStringLength(const aInstance : TObject; aProperty: TRttiProperty; aValidation : StringLength);
    procedure ValidateRange(const aInstance : TObject; aProperty: TRttiProperty; aValidation : Range);
    procedure ValidateObject(aObj : TObject);
    procedure ValidateArray(aValue : TValue);
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
    function GetSection(aOptionsSection : TOptionsClass; var vOptions : TOptions) : Boolean; overload;
    procedure AddOption(aOption : TOptions);
    function ExistsSection(aOption : TOptionsClass; const aSectionName : string = '') : Boolean;
  end;

  TSectionList = TObjectList<TOptions>;

  IOptionsSerializer = interface
  ['{7DECE203-4AAE-4C9D-86C8-B3D583DF7C8B}']
    function Load(aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean;
    function LoadSection(aSections : TSectionList; aOptions: TOptions) : Boolean;
    procedure Save(aSections : TSectionList);
    function GetFileSectionNames(out oSections : TArray<string>) : Boolean;
    function ConfigExists : Boolean;
  end;

  IFileOptionsSerializer = interface(IOptionsSerializer)
  ['{3417B142-2879-4DA6-86CA-19F0F427A92C}']
    function GetFileName : string;
    procedure SetFileName(const aFilename : string);
    property Filename : string read GetFilename write SetFilename;
  end;

  TOptionsSerializer = class(TInterfacedObject,IOptionsSerializer)
  public
    function Load(aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean; virtual; abstract;
    function LoadSection(aSections : TSectionList; aOptions: TOptions) : Boolean; virtual; abstract;
    procedure Save(aSections : TSectionList); virtual; abstract;
    function GetFileSectionNames(out oSections : TArray<string>) : Boolean; virtual; abstract;
    function ConfigExists : Boolean; virtual; abstract;
  end;

  TOptionsFileSerializer = class(TOptionsSerializer,IFileOptionsSerializer)
  private
    fFilename : string;
    function GetFileName : string;
    procedure SetFileName(const aFilename : string);
  public
    property Filename : string read GetFilename write SetFilename;
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
    fSerializer : IOptionsSerializer;
    fSections : TSectionList;
    fLoaded : Boolean;
    fOnConfigLoaded : TLoadConfigEvent;
    fOnConfigReloaded : TLoadConfigEvent;
    function GetOptions(aOptionClass : TOptionsClass): TOptions; overload;
    function GetOptions(aIndex : Integer) : TOptions; overload;
    function GetSection(aOptionsSection : TOptionsClass; var vOptions : TOptions) : Boolean; overload;
  public
    constructor Create(aOptionsSerializer : IOptionsSerializer);
    destructor Destroy; override;
    property IsLoaded : Boolean read fLoaded;
    function ExistsSection(aOption : TOptionsClass; const aSectionName : string = '') : Boolean; overload;
    function ExistsSection<T : TOptions>(const aSectionName : string = '') : Boolean; overload;
    property Items[aOptionClass : TOptionsClass] : TOptions read GetOptions; default;
    property Items[aIndex : Integer] : TOptions read GetOptions; default;
    property OnConfigLoaded : TLoadConfigEvent read fOnConfigLoaded write fOnConfigLoaded;
    property OnConfigReloaded : TLoadConfigEvent read fOnConfigReloaded write fOnConfigReloaded;
    function AddSection(aOption : TOptionsClass; const aSectionName : string = '') : TOptions; overload;
    function AddSection<T : TOptions>(const aSectionName : string = '') : TOptions<T>; overload;
    procedure AddOption(aOption : TOptions);
    function GetSectionInterface<T : TOptions> : IOptions<T>;
    function GetSection<T : TOptions>(const aSectionName : string = '') : T; overload;
    function GetFileSectionNames(out oSections : TArray<string>) : Boolean;
    function Count : Integer;
    procedure Load(aFailOnSectionNotExists : Boolean = False); virtual;
    procedure LoadSection(aOptions : TOptions);
    procedure Save; virtual;
  end;

  TFileOptionsContainer = class(TOptionsContainer)
  private
    fFilename : string;
    fFileMonitor : TFileMonitor;
    fOnFileModified : TFileModifiedEvent;
    fReloadIfFileChanged : Boolean;
    procedure CreateFileMonitor;
    procedure FileModifiedNotify(MonitorNotify : TMonitorNotify);
    procedure SetReloadIfFileChanged(const Value: Boolean);
  public
    constructor Create(aOptionsSerializer : IFileOptionsSerializer; aReloadIfFileChanged : Boolean = False);
    destructor Destroy; override;
    property FileName : string read fFilename;
    property ReloadIfFileChanged : Boolean read fReloadIfFileChanged write SetReloadIfFileChanged;
    property OnFileModified : TFileModifiedEvent read fOnFileModified write fOnFileModified;
    procedure Save; override;
    function GetFileSectionNames(out oSections: TArray<string>): Boolean;
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

{ TCustomOptionsContainer}

function TOptionsContainer.ExistsSection(aOption: TOptionsClass;const aSectionName: string): Boolean;
var
  option : TOptions;
begin
  Result := False;
  for option in fSections do
  begin
    if CompareText(option.ClassName,aOption.ClassName) = 0 then
    begin
      if (not aSectionName.IsEmpty) and (CompareText(option.Name,aSectionName) = 0) then Exit(True);
    end;
  end;
end;

function TOptionsContainer.ExistsSection<T>(const aSectionName: string): Boolean;
begin
  Result := GetSection<T>(aSectionName) <> nil;
end;

procedure TOptionsContainer.AddOption(aOption: TOptions);
begin
  if aOption.Name.IsEmpty then aOption.Name := Copy(aOption.ClassName,2,aOption.ClassName.Length);
  fSections.Add(aOption);
end;

function TOptionsContainer.AddSection(aOption : TOptionsClass; const aSectionName : string = '') : TOptions;
var
  option : TOptions;
begin
  //if section already exists, returns it
  option := Self.GetOptions(aOption);
  if option <> nil then Exit(option);
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
  //if section already exists, returns it
  option := Self.GetSection<T>(aSectionName);
  if option <> nil then Exit(TOptions<T>(option));
  //new section
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

constructor TOptionsContainer.Create(aOptionsSerializer: IOptionsSerializer);
begin
  fSerializer := aOptionsSerializer;
  fSections := TSectionList.Create(False);
  fLoaded := False;
end;

destructor TOptionsContainer.Destroy;
var
  option : TOptions;
begin
  fSerializer := nil;
  for option in fSections do
  begin
    if option.RefCount = 0 then option.Free;
  end;
  fSections.Free;
  inherited;
end;

function TOptionsContainer.GetFileSectionNames(out oSections: TArray<string>): Boolean;
begin
  Result := fSerializer.GetFileSectionNames(oSections);
end;

function TOptionsContainer.GetOptions(aIndex: Integer): TOptions;
begin
  Result := fSections[aIndex];
end;

function TOptionsContainer.GetSection(aOptionsSection: TOptionsClass; var vOptions: TOptions): Boolean;
var
  option : TOptions;
begin
  Result := False;
  for option in fSections do
  begin
    if option is TOptionsClass then
    begin
      vOptions := option as TOptionsClass;
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
    if option is aOptionClass then Result := option as TOptionsClass;
  end;
end;

function TOptionsContainer.GetSection<T>(const aSectionName : string = '') : T;
var
  option : TOptions;
begin
  Result := nil;
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
  if fSerializer.ConfigExists then
  begin
    if not fSerializer.Load(fSections,aFailOnSectionNotExists) then Save;
    if not fLoaded then
    begin
      fLoaded := True;
      if Assigned(fOnConfigLoaded) then fOnConfigLoaded;
    end
    else if Assigned(fOnConfigReloaded) then fOnConfigReloaded;
  end
  else
  begin
    //if config not exists get default values
    for option in fSections do option.DefaultValues;
    //saves default
    Save;
  end;
end;

procedure TOptionsContainer.LoadSection(aOptions : TOptions);
begin
  if fSerializer.ConfigExists then
  begin
    if not fSerializer.LoadSection(fSections,aOptions) then Save;
  end;
end;

procedure TOptionsContainer.Save;
begin
  fSerializer.Save(fSections);
end;

{ TOptionsContainer }

constructor TFileOptionsContainer.Create(aOptionsSerializer : IFileOptionsSerializer; aReloadIfFileChanged : Boolean = False);
begin
  inherited Create(aOptionsSerializer);
  fFilename := aOptionsSerializer.Filename;
  if aReloadIfFileChanged then CreateFileMonitor;
end;

procedure TFileOptionsContainer.Save;
var
  laststate : Boolean;
begin
  //disable filemonitor to avoid detect manual save as a external file change
  if fReloadIfFileChanged then
  begin
    laststate := fFileMonitor.Enabled;
    fFileMonitor.Enabled := False;
    try
      //save config file
      inherited;
    finally
      //set last state
      Sleep(0);
      fFileMonitor.Enabled := laststate;
    end;
  end
  else inherited;
end;

procedure TFileOptionsContainer.SetReloadIfFileChanged(const Value: Boolean);
begin
  if Value = fReloadIfFileChanged then Exit;
  fReloadIfFileChanged := Value;
  if Assigned(fFileMonitor) then fFileMonitor.Free;
  if fReloadIfFileChanged then CreateFileMonitor;
end;

procedure TFileOptionsContainer.CreateFileMonitor;
begin
  fFileMonitor := TQuickFileMonitor.Create;
  fFileMonitor.FileName := fFilename;
  fFileMonitor.Interval := 2000;
  fFileMonitor.Notifies := [TMonitorNotify.mnFileModified];
  fFileMonitor.OnFileChange := FileModifiedNotify;
  fFileMonitor.Enabled := True;
end;

destructor TFileOptionsContainer.Destroy;
begin
  if Assigned(fFileMonitor) then fFileMonitor.Free;
  inherited;
end;

procedure TFileOptionsContainer.FileModifiedNotify(MonitorNotify: TMonitorNotify);
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

function TFileOptionsContainer.GetFileSectionNames(out oSections : TArray<string>) : Boolean;
begin
  Result := fSerializer.GetFileSectionNames(oSections);
end;

{ TOptions }

function TOptions.ConfigureOptions<T>(aOptionsFunc: TConfigureOptionsProc<T>): IOptionsValidator;
var
  value : TValue;
begin
  Result := TOptionsValidator.Create(Self);
  if Assigned(aOptionsFunc) then
  begin
    value := Self;
    aOptionsFunc(value.AsType<T>);
  end;
end;

constructor TOptions.Create;
begin
  fName := '';
  fHideOptions := False;
end;

procedure TOptions.DefaultValues;
begin
  //nothing
end;

procedure TOptions.DoValidateOptions;
var
  ivalidator : IOptionsValidator;
begin
  ivalidator := TOptionsValidator.Create(Self);
  ivalidator.ValidateOptions;
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

{ TOptionsValidator }

procedure TOptionsValidator.ValidateObject(aObj : TObject);
var
  ctx : TRttiContext;
  rtype : TRttiType;
  rprop : TRttiProperty;
  attrib : TCustomAttribute;
  rvalue : TValue;
begin
  rtype := ctx.GetType(aObj.ClassInfo);
  for rprop in rtype.GetProperties do
  begin
    //check only published properties
    if rprop.Visibility = TMemberVisibility.mvPublished then
    begin
      //check validation option attributes
      for attrib in rprop.GetAttributes do
      begin
        if attrib is Required  then ValidateRequired(aObj,rprop)
        else if attrib is StringLength then ValidateStringLength(aObj,rprop,StringLength(attrib))
        else if attrib is Range then ValidateRange(aObj,rprop,Range(attrib));
      end;
      rvalue := rprop.GetValue(aObj);
      if not rvalue.IsEmpty then
      begin
        case rvalue.Kind of
          tkClass : ValidateObject(rvalue.AsObject);
          tkDynArray : ValidateArray(rvalue);
        end;
      end;
    end;
  end;
end;

constructor TOptionsValidator.Create(aOptions: TOptions);
begin
  fOptions := aOptions;
end;

procedure TOptionsValidator.ValidateOptions;
begin
  ValidateObject(fOptions);
end;

procedure TOptionsValidator.ValidateArray(aValue : TValue);
type
  PPByte = ^PByte;
var
  ctx : TRttiContext;
  rDynArray : TRttiDynamicArrayType;
  itvalue : TValue;
  i : Integer;
begin
  rDynArray := ctx.GetType(aValue.TypeInfo) as TRTTIDynamicArrayType;
  for i := 0 to aValue.GetArrayLength - 1 do
  begin
    TValue.Make(PPByte(aValue.GetReferenceToRawData)^ + rDynArray.ElementType.TypeSize * i, rDynArray.ElementType.Handle,itvalue);
    if not itvalue.IsEmpty then
    begin
      case itvalue.Kind of
        tkClass : ValidateObject(itvalue.AsObject);
        tkDynArray : ValidateArray(itvalue);
      end;
    end;
  end;
end;

procedure TOptionsValidator.ValidateRange(const aInstance : TObject; aProperty: TRttiProperty; aValidation : Range);
var
  value : TValue;
  msg : string;
begin
  value := aProperty.GetValue(aInstance);
  if not value.IsEmpty then
  begin
    if value.Kind = tkFloat then
    begin
      if (value.AsExtended < aValidation.Min) or (value.AsExtended > aValidation.Max) then
      begin
        if aValidation.ErrorMsg.IsEmpty then msg := Format('Option %s "%s.%s" exceeds predefined range (%2f - %2f)',[fOptions.Name,aInstance.ClassName,aProperty.Name,aValidation.Min,aValidation.Max])
          else msg := aValidation.ErrorMsg;
        raise EOptionValidationError.Create(msg);
      end;
    end
    else if value.Kind in [tkInteger,tkInt64] then
    begin
      if (value.AsInt64 < aValidation.Min) or (value.AsInt64 > aValidation.Max) then
      begin
        if aValidation.ErrorMsg.IsEmpty then msg := Format('Option %s "%s.%s" exceeds predefined range (%d - %d)',[fOptions.Name,aInstance.ClassName,aProperty.Name,Round(aValidation.Min),Round(aValidation.Max)])
          else msg := aValidation.ErrorMsg;
        raise EOptionValidationError.Create(msg);
      end;
    end;
  end;
end;

procedure TOptionsValidator.ValidateRequired(const aInstance : TObject; aProperty: TRttiProperty);
begin
  if aProperty.GetValue(aInstance).IsEmpty then raise EOptionValidationError.CreateFmt('Option %s "%s.%s" is required',[fOptions.Name,aInstance.ClassName,aProperty.Name]);
end;

procedure TOptionsValidator.ValidateStringLength(const aInstance : TObject; aProperty: TRttiProperty; aValidation : StringLength);
var
  value : TValue;
  msg : string;
begin
  value := aProperty.GetValue(aInstance);
  if (not value.IsEmpty) and (value.AsString.Length > aValidation.MaxLength) then
  begin
    if aValidation.ErrorMsg.IsEmpty then msg := Format('Option %s "%s.%s" exceeds max length (%d)',[fOptions.Name,aInstance.ClassName,aProperty.Name,aValidation.MaxLength])
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
    else Result := TOptionsValidator.Create(fOptions);
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

{ TOptionsFileSerializer }

function TOptionsFileSerializer.GetFileName: string;
begin
  Result := fFilename;
end;

procedure TOptionsFileSerializer.SetFileName(const aFilename: string);
begin
  fFilename := aFilename;
end;

end.
