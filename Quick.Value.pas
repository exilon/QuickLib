{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.Value
  Description : Autofree value record
  Author      : Kike Pérez
  Version     : 1.5
  Created     : 07/01/2019
  Modified    : 20/04/2020

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

unit Quick.Value;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  Variants;

type

  TValueDataType = (dtNull, dtString, dtAnsiString, dtWideString, dtInteger, dtInt64, dtDouble, dtExtended, dtDateTime, dtBoolean, dtObject, dtOwnedObject,
                    dtPointer, dtClass, dtInterface, dtRecord, dtArray, dtVariant);

  TValueData = class(TInterfacedObject);

  IValueString = interface
  ['{CECEF8BB-5C77-4291-8927-FB090577F27D}']
    function GetValue : string;
    procedure SetValue(const Value : string);
    property Value : string read GetValue write SetValue;
  end;

  TValueString = class(TValueData,IValueString)
  strict private
    fData : string;
  private
    function GetValue : string;
    procedure SetValue(const Value : string);
  public
    constructor Create(const Value : string);
    property Value : string read GetValue write SetValue;
  end;

  {$IFDEF MSWINDOWS}
   IValueAnsiString = interface
   ['{75775F25-6F7A-49F0-A1E0-BDE1F55EC378}']
    function GetValue : AnsiString;
    procedure SetValue(const Value : AnsiString);
    property Value : AnsiString read GetValue write SetValue;
  end;

  TValueAnsiString = class(TValueData,IValueAnsiString)
  strict private
    fData : AnsiString;
  private
    function GetValue : AnsiString;
    procedure SetValue(const Value : AnsiString);
  public
    constructor Create(const Value : AnsiString);
    property Value : AnsiString read GetValue write SetValue;
  end;

  IValueWideString = interface
  ['{9094B9CF-46AE-4FE0-AE1D-6E6CDABDAF36}']
    function GetValue : WideString;
    procedure SetValue(const Value : WideString);
    property Value : WideString read GetValue write SetValue;
  end;

  TValueWideString = class(TValueData,IValueWideString)
  strict private
    fData : WideString;
  private
    function GetValue : WideString;
    procedure SetValue(const Value : WideString);
  public
    constructor Create(const Value : WideString);
    property Value : WideString read GetValue write SetValue;
  end;
  {$ENDIF}

  IValueInteger = interface
  ['{5AB05017-C6F3-49BA-A92C-ECCA252B3E1D}']
    function GetValue : Int64;
    procedure SetValue(const Value : Int64);
    property Value : Int64 read GetValue write SetValue;
  end;

  { TValueInteger }

  TValueInteger= class(TValueData,IValueInteger)
  strict private
    fData : Int64;
  private
    function GetValue : Int64;
    procedure SetValue(const Value : Int64);
  public
    constructor Create(const Value : Int64);
    property Value : Int64 read GetValue write SetValue;
  end;

  IValueExtended = interface
  ['{D341182F-D4E5-4C07-9E03-68DA118B90B1}']
    function GetValue : Extended;
    procedure SetValue(const Value : Extended);
    property Value : Extended read GetValue write SetValue;
  end;

  TValueExtended = class(TValueData,IValueExtended)
  strict private
    fData : Extended;
  private
    function GetValue : Extended;
    procedure SetValue(const Value : Extended);
  public
    constructor Create(const Value : Extended);
    property Value : Extended read GetValue write SetValue;
  end;

  IValueObject = interface
  ['{5828FABC-6C5D-4954-941E-B3580F918A8B}']
    function GetValue : TObject;
    procedure SetValue(const Value : TObject);
    property Value : TObject read GetValue write SetValue;
  end;

  TValueObject = class(TValueData,IValueObject)
  strict private
    fData : TObject;
  private
    function GetValue : TObject;
    procedure SetValue(const Value : TObject);
  public
    constructor Create(const Value : TObject);
    property Value : TObject read GetValue write SetValue;
  end;

  IValuePointer = interface
  ['{9FE4E499-C487-4D24-8190-14FF3F9FE86B}']
    function GetValue : Pointer;
    procedure SetValue(const Value : Pointer);
    property Value : Pointer read GetValue write SetValue;
  end;

  TValuePointer = class(TValueData,IValuePointer)
  strict private
    fData : Pointer;
  private
    function GetValue : Pointer;
    procedure SetValue(const Value : Pointer);
  public
    constructor Create(const Value : Pointer);
    property Value : Pointer read GetValue write SetValue;
  end;

  IValueVariant = interface
  ['{8B1F8469-B872-47AD-83BB-F51920012943}']
    function GetValue : Variant;
    procedure SetValue(const Value : Variant);
    property Value : Variant read GetValue write SetValue;
  end;

  TValueVariant = class(TValueData,IValueVariant)
  strict private
    fData : Variant;
  private
    function GetValue : Variant;
    procedure SetValue(const Value : Variant);
  public
    constructor Create(const Value : Variant);
    property Value : Variant read GetValue write SetValue;
  end;

  { TFlexValue }

  TFlexValue = record
  private
    {$IFNDEF FPC}
    fDataIntf : IInterface;
    {$ELSE}
    fDataIntf : TValueData;
    {$ENDIF}
    fDataType : TValueDataType;
    function CastToString : string;
    {$IFDEF MSWINDOWS}
    function CastToAnsiString : AnsiString;
    function CastToWideString : WideString;
    {$ENDIF}
    function CastToBoolean: Boolean;
    function CastToClass: TClass;
    function CastToExtended: Extended;
    function CastToInt64: Int64;
    function CastToInteger: Integer;
    function CastToDateTime : TDateTime;
    function CastToObject: TObject;
    function CastToPointer: Pointer;
    function CastToInterface: IInterface;
    function CastToVariant: Variant;
    function CastToCardinal : Cardinal;
    function CastToVarRec: TVarRec;
    procedure SetAsString(const Value : string);
    procedure SetAsVarRec(const Value: TVarRec);
    {$IFDEF MSWINDOWS}
    procedure SetAsAnsiString(const Value : AnsiString);
    procedure SetAsWideString(const Value : WideString);
    {$ENDIF}
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsClass(const Value: TClass);
    procedure SetAsExtended(const Value: Extended);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsObject(const Value: TObject);
    procedure SetAsPointer(const Value: Pointer);
    procedure SetAsDateTime(const Value : TDateTime);
    procedure SetAsVariant(const Value: Variant);
    procedure SetAsCardinal(const Value : Cardinal);
    procedure SetAsInterface(const Value: IInterface);
  public
    constructor Create(const Value: TVarRec);
    {$IFNDEF FPC}
    property Data : IInterface read fDataIntf;
    {$ELSE}
    property Data : TValueData read fDataIntf;
    {$ENDIF}
    property DataType : TValueDataType read fDataType;
    {$IFNDEF FPC}
    procedure SetAsCustom(aData : IInterface; aType : TValueDataType);
    {$ENDIF}
    property AsString : string read CastToString write SetAsString;
    {$IFDEF MSWINDOWS}
    property AsAnsiString : AnsiString read CastToAnsiString write SetAsAnsiString;
    property AsWideString : WideString read CastToWideString write SetAsWideString;
    {$ENDIF}
    property AsInteger : Integer read CastToInteger write SetAsInteger;
    property AsInt64 : Int64 read CastToInt64 write SetAsInt64;
    property AsExtended : Extended read CastToExtended write SetAsExtended;
    property AsBoolean : Boolean read CastToBoolean write SetAsBoolean;
    property AsPointer : Pointer read CastToPointer write SetAsPointer;
    property AsClass : TClass read CastToClass write SetAsClass;
    property AsInterface : IInterface read CastToInterface write SetAsInterface;
    property AsObject : TObject read CastToObject write SetAsObject;
    property AsVariant : Variant  read CastToVariant write SetAsVariant;
    property AsCardinal : Cardinal read CastToCardinal write SetAsCardinal;
    property AsDateTime : TDateTime read CastToDateTime write SetAsDateTime;
    property AsVarRec : TVarRec read CastToVarRec write SetAsVarRec;
    //function AsType<T> : T;
    function  IsNullOrEmpty : Boolean; inline;
    function  IsString : Boolean; inline;
    function  IsInteger : Boolean; inline;
    function  IsFloating : Boolean; inline;
    function  IsDateTime : Boolean; inline;
    function  IsBoolean : Boolean; inline;
    function  IsInterface : Boolean; inline;
    function  IsObject : Boolean; inline;
    function  IsPointer : Boolean; inline;
    function  IsVariant : Boolean; inline;
    function IsArray : Boolean; inline;
    function IsRealInteger : Boolean;
    function IsRealExtended : Boolean;
    procedure Clear; inline;
    procedure _AddRef; inline;
    procedure _Release; inline;
    class operator Implicit(const Value : TFlexValue) : string;
    class operator Implicit(Value : TFlexValue) : Integer;
    class operator Implicit(Value : TFlexValue) : Int64;
    class operator Implicit(Value : TFlexValue) : Extended;
    class operator Implicit(Value : TFlexValue) : TDateTime;
    class operator Implicit(Value : TFlexValue) : Boolean;
    class operator Implicit(Value : TFlexValue) : TClass;
    class operator Implicit(Value : TFlexValue) : TObject;
    class operator Implicit(Value : TFlexValue) : Pointer;
    class operator Implicit(Value : TFlexValue) : Variant;
    class operator Implicit(Value : TFlexValue) : TVarRec;
    class operator Implicit(const Value : string) : TFlexValue;
    class operator Implicit(Value : Integer) : TFlexValue;
    class operator Implicit(Value : Int64) : TFlexValue;
    class operator Implicit(Value : Extended) : TFlexValue;
    class operator Implicit(Value : TDateTime) : TFlexValue;
    class operator Implicit(Value : Boolean) : TFlexValue;
    class operator Implicit(Value : TClass) : TFlexValue;
    class operator Implicit(Value : TObject) : TFlexValue;
    class operator Implicit(Value : Pointer) : TFlexValue;
    class operator Implicit(Value : Variant) : TFlexValue;
    class operator Implicit(Value : TVarRec) : TFlexValue;
    class operator Equal(a : TFlexValue; b : string) : Boolean;
    class operator Equal(a : TFlexValue; b : Integer) : Boolean;
    class operator Equal(a : TFlexValue; b : Int64) : Boolean;
    class operator Equal(a : TFlexValue; b : Extended) : Boolean;
    class operator Equal(a : TFlexValue; b : Boolean) : Boolean;
    class operator NotEqual(a : TFlexValue; b : string) : Boolean;
    class operator NotEqual(a : TFlexValue; b : Integer) : Boolean;
    class operator NotEqual(a : TFlexValue; b : Int64) : Boolean;
    class operator NotEqual(a : TFlexValue; b : Extended) : Boolean;
    class operator NotEqual(a : TFlexValue; b : Boolean) : Boolean;
    class operator GreaterThan(a : TFlexValue; b : Integer) : Boolean;
    class operator GreaterThan(a : TFlexValue; b : Int64) : Boolean;
    class operator GreaterThan(a : TFlexValue; b : Extended) : Boolean;
    class operator GreaterThanOrEqual(a : TFlexValue; b : Integer) : Boolean;
    class operator GreaterThanOrEqual(a : TFlexValue; b : Int64) : Boolean;
    class operator GreaterThanOrEqual(a : TFlexValue; b : Extended) : Boolean;
    class operator LessThan(a : TFlexValue; b : Integer) : Boolean;
    class operator LessThan(a : TFlexValue; b : Int64) : Boolean;
    class operator LessThan(a : TFlexValue; b : Extended) : Boolean;
    class operator LessThanOrEqual(a : TFlexValue; b : Integer) : Boolean;
    class operator LessThanOrEqual(a : TFlexValue; b : Int64) : Boolean;
    class operator LessThanOrEqual(a : TFlexValue; b : Extended) : Boolean;
  end;

  PFlexValue = ^TFlexValue;

  TFlexPair = record
    Name : string;
    Value : TFlexValue;
    constructor Create(const aName : string; aValue : TFlexValue);
  end;

implementation


function TFlexValue.CastToString: string;
begin
  try
    case fDataType of
      dtNull : Result := '';
      dtString : Result := (fDataIntf as IValueString).Value;
      {$IFDEF MSWINDOWS}
      dtAnsiString : Result := string((fDataIntf as IValueAnsiString).Value);
      dtWideString : Result := (fDataIntf as IValueWideString).Value;
      {$ENDIF}
      dtInteger,
      dtInt64 : Result := IntToStr(AsInt64);
      dtBoolean : Result := BoolToStr(AsBoolean,True);
      dtDouble,
      dtExtended : Result := FloatToStr(AsExtended);
      dtDateTime : Result := DateTimeToStr(AsExtended);
      dtVariant : Result := string(AsVariant);
      dtClass : Result := AsClass.ClassName;
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to String error: %s',[e.message]);
  end;
end;

{$IFDEF MSWINDOWS}
function TFlexValue.CastToAnsiString: AnsiString;
begin
  try
    case fDataType of
      dtNull : Result := '';
      dtString : Result := AnsiString((fDataIntf as IValueString).Value);
      dtAnsiString : Result := (fDataIntf as IValueAnsiString).Value;
      dtWideString : Result := AnsiString((fDataIntf as IValueWideString).Value);
      dtInteger,
      dtInt64 : Result := AnsiString(IntToStr(AsInt64));
      dtBoolean : Result := AnsiString(BoolToStr(AsBoolean,True));
      dtDouble,
      dtExtended : Result := AnsiString(FloatToStr(AsExtended));
      dtDateTime : Result := AnsiString(DateTimeToStr(AsExtended));
      dtVariant : Result := AnsiString(AsVariant);
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to AnsiString error: %s',[e.message]);
  end;
end;

function TFlexValue.CastToWideString: WideString;
begin
  try
    case fDataType of
      dtNull : Result := '';
      dtString : Result := Widestring((fDataIntf as IValueString).Value);
      {$IFDEF MSWINDOWS}
      dtAnsiString : Result := Widestring((fDataIntf as IValueAnsiString).Value);
      dtWideString : Result := (fDataIntf as IValueWideString).Value;
      {$ENDIF}
      dtInteger,
      dtInt64 : Result := Widestring(IntToStr(AsInt64));
      dtBoolean : Result := Widestring(BoolToStr(AsBoolean,True));
      dtDouble,
      dtExtended : Result := Widestring(FloatToStr(AsExtended));
      dtDateTime : Result := Widestring(DateTimeToStr(AsExtended));
      dtVariant : Result := Widestring(AsVariant);
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to WideString error: %s',[e.message]);
  end;
end;
{$ENDIF}

function TFlexValue.CastToBoolean: Boolean;
begin
  try
    case fDataType of
      dtNull : Result := False;
      dtString : Result := StrToBool((fDataIntf as IValueString).Value);
      {$IFDEF MSWINDOWS}
      dtAnsiString : Result := StrToBool(string((fDataIntf as IValueAnsiString).Value));
      dtWideString : Result := StrToBool((fDataIntf as IValueWideString).Value);
      {$ENDIF}
      dtInteger,
      dtInt64 :
        begin
          if (fDataIntf as IValueInteger).Value = 1 then Result := True
            else if (fDataIntf as IValueInteger).Value = 0 then Result := False
              else raise Exception.Create('Integer value not in 0-1 range');
        end;
      dtBoolean : Result := Boolean((fDataIntf as IValueInteger).Value);
      dtVariant : Result := Boolean(AsVariant);
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to Boolean error: %s',[e.message]);
  end;
end;

function TFlexValue.CastToCardinal: Cardinal;
begin
  Result := AsInt64;
end;

function TFlexValue.CastToClass: TClass;
begin
  try
    case fDataType of
      dtNull : Result := nil;
      dtClass : Result := (fDataIntf as TValuePointer).Value;
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to TClass error: %s',[e.message]);
  end;
end;

function TFlexValue.CastToDateTime: TDateTime;
begin
  try
    case fDataType of
      dtNull : Result := 0.0;
      dtString : Result := StrToDateTime((fDataIntf as IValueString).Value);
      {$IFDEF MSWINDOWS}
      dtAnsiString : Result := StrToDateTime(string((fDataIntf as IValueAnsiString).Value));
      dtWideString : Result := StrToDateTime((fDataIntf as IValueWideString).Value);
      {$ENDIF}
      dtInteger,
      dtInt64 : Result := FileDateToDateTime(AsInt64);
      dtDouble,
      dtExtended,
      dtDateTime : Result := (fDataIntf as IValueExtended).Value;
      dtVariant : Result := Extended(AsVariant);
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to Extended error: %s',[e.message]);
  end;
end;

function TFlexValue.CastToExtended: Extended;
begin
  try
    case fDataType of
      dtNull : Result := 0.0;
      dtString : Result := StrToFloat((fDataIntf as IValueString).Value);
      {$IFDEF MSWINDOWS}
      dtAnsiString : Result := StrToFloat(string((fDataIntf as IValueAnsiString).Value));
      dtWideString : Result := StrToFloat((fDataIntf as IValueWideString).Value);
      {$ENDIF}
      dtInteger,
      dtInt64 : Result := AsInt64;
      dtBoolean : Result := AsInt64;
      dtDouble,
      dtExtended,
      dtDateTime : Result := (fDataIntf as IValueExtended).Value;
      dtVariant : Result := Extended(AsVariant);
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to Extended error: %s',[e.message]);
  end;
end;

function TFlexValue.CastToInt64: Int64;
begin
  try
    case fDataType of
      dtNull : Result := 0;
      dtString : Result := StrToInt((fDataIntf as IValueString).Value);
      {$IFDEF MSWINDOWS}
      dtAnsiString : Result := StrToInt(string((fDataIntf as IValueAnsiString).Value));
      dtWideString : Result := StrToInt((fDataIntf as IValueWideString).Value);
      {$ENDIF}
      dtInteger,
      dtInt64 : Result := (fDataIntf as IValueInteger).Value;
      dtBoolean : Result := Integer(AsBoolean);
      dtDateTime : Result := DateTimeToFileDate((fDataIntf as IValueExtended).Value);
      dtVariant : Result := Integer(AsVariant);
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to Integer error: %s',[e.message]);
  end;
end;

function TFlexValue.CastToInteger: Integer;
begin
  Result := AsInt64;
end;

function TFlexValue.CastToObject: TObject;
begin
  try
    case fDataType of
      dtObject,
      dtOwnedObject : Result := (fDataIntf as IValueObject).Value;
      {$IFNDEF FPC}
      dtPointer : Result := TObject((fDataIntf as IValueObject).Value);
      {$ELSE}
      dtPointer : Result := TObject((fDataIntf as IValuePointer).Value);
      {$ENDIF}
      dtNull : Result := nil;
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to Object error: %s',[e.message]);
  end;
end;

function TFlexValue.CastToPointer: Pointer;
begin
  try
    case fDataType of
      dtObject,
      dtOwnedObject : Result := Pointer((fDataIntf as IValueObject).Value);
      dtPointer : Result := (fDataIntf as IValuePointer).Value;
      dtNull : Result := nil;
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to Pointer error: %s',[e.message]);
  end;
end;

function TFlexValue.CastToVariant: Variant;
begin
  try
    case fDataType of
      dtNull : Result := Variants.Null;
      dtBoolean : Result := AsVariant;
      {$IFDEF MSWINDOWS}
      dtAnsiString : Result := string((fDataIntf as IValueAnsiString).Value);
      dtWideString : Result := (fDataIntf as IValueWideString).Value;
      {$ENDIF}
      dtString : Result := (fDataIntf as IValueString).Value;
      dtInteger,
      dtInt64 : Result := (fDataIntf as IValueInteger).Value;
      dtVariant : Result := (fDataIntf as IValueVariant).Value;
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to Variant error: %s',[e.message]);
  end;
end;

function TFlexValue.CastToVarRec: TVarRec;
begin
  try
    case fDataType of
      dtNull : Result.VPointer := nil;
      dtBoolean : Result.VBoolean := AsBoolean;
      {$IFDEF MSWINDOWS}
      dtAnsiString : Result.VAnsiString := Pointer((fDataIntf as IValueAnsiString).Value);
      dtWideString : Result.VWideString := Pointer((fDataIntf as IValueWideString).Value);
      {$ENDIF}
      {$IFNDEF NEXTGEN}
      dtString : Result.VString := Pointer((fDataIntf as IValueString).Value);
      {$ELSE}
      dtString : Result.VUnicodeString := Pointer((fDataIntf as IValueString));
      {$ENDIF}
      dtInteger : Result.VInteger := (fDataIntf as IValueInteger).Value;
      dtInt64 : Result.VInt64 := Pointer((fDataIntf as IValueInteger).Value);
      //dtVariant : Result.VVariant := ^fDataIntf as IValueVariant).Value;
      dtObject : Result.VObject := AsObject;
      dtPointer : Result.VPointer := AsPointer;
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to TVarRec error: %s',[e.message]);
  end;
end;

function TFlexValue.CastToInterface: IInterface;
begin
  try
    case fDataType of
      dtNull : Result := nil;
      dtInterface : Result := fDataIntf;
      dtPointer : Result := IInterface(fDataIntf);
      dtVariant : Result := IInterface(fDataIntf);
      else raise Exception.Create('DataType not supported');
    end;
  except
    on E : Exception do raise Exception.CreateFmt('TFlexValue conversion to Interface error: %s',[e.message]);
  end;
end;

procedure TFlexValue.Clear;
begin
   if Pointer(fDataIntf) <> nil then fDataIntf := nil;
  fDataType := dtNull;
end;

constructor TFlexValue.Create(const Value: TVarRec);
begin
  case Value.VType of
    {$IFNDEF NEXTGEN}
    vtString : AsString := string(Value.VString^);
    vtChar : AsString := string(Value.VChar);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    vtAnsiString : AsAnsiString := AnsiString(Value.VAnsiString);
    vtWideString : AsWideString := WideString(Value.VWideString);
    {$ENDIF}
    {$IFDEF UNICODE}
    vtUnicodeString: AsString := string(Value.VUnicodeString);
    {$ENDIF UNICODE}
    vtInteger : AsInteger := Value.VInteger;
    vtInt64 : AsInt64 := Value.VInt64^;
    vtExtended : AsExtended := Value.VExtended^;
    vtBoolean : AsBoolean := Value.VBoolean;
    vtVariant : AsVariant := Value.VVariant^;
    vtInterface : AsInterface := IInterface(Value.VInterface);
    vtClass : AsClass := Value.VClass;
    vtObject : AsObject := Value.VObject;
    vtPointer : AsPointer := Value.VPointer;
    else raise Exception.Create('DataType not supported by TFlexValue');
  end;
  {$IFDEF FPC}
  fDataIntf._AddRef;
  {$ENDIF}
end;

class operator TFlexValue.Implicit(Value: TFlexValue): Boolean;
begin
  Result := Value.AsBoolean;
end;

class operator TFlexValue.Implicit(const Value: TFlexValue): string;
begin
  Result := Value.AsString;
end;

class operator TFlexValue.Implicit(Value: TFlexValue): TObject;
begin
  Result := Value.AsObject;
end;

class operator TFlexValue.Implicit(Value: TFlexValue): Pointer;
begin
  Result := Value.AsPointer;
end;

class operator TFlexValue.Implicit(Value: TFlexValue): TDateTime;
begin
  Result := Value.AsDateTime;
end;

class operator TFlexValue.Implicit(Value: TFlexValue): TClass;
begin
  Result := Value.AsClass;
end;

class operator TFlexValue.Implicit(Value: TFlexValue): Int64;
begin
  Result := Value.AsInt64;
end;

class operator TFlexValue.Implicit(Value: TFlexValue): Integer;
begin
  Result := Value.AsInteger;
end;

class operator TFlexValue.Implicit(Value: TFlexValue): Extended;
begin
  Result := Value.AsExtended;
end;

class operator TFlexValue.Implicit(Value: TFlexValue): Variant;
begin
  Result := Value.AsVariant;
end;

class operator TFlexValue.Implicit(Value: TFlexValue): TVarRec;
begin
  Result := Value.AsVarRec;
end;

class operator TFlexValue.Implicit(Value: Variant): TFlexValue;
begin
  Result.AsVariant := Value;
end;

class operator TFlexValue.Implicit(const Value : string) : TFlexValue;
begin
  Result.AsString := Value;
end;

class operator TFlexValue.Implicit(Value : Integer) : TFlexValue;
begin
  Result.AsInteger := Value;
end;

class operator TFlexValue.Implicit(Value : Int64) : TFlexValue;
begin
  Result.AsInt64 := Value;
end;

class operator TFlexValue.Implicit(Value : Extended) : TFlexValue;
begin
  Result.AsExtended := Value;
end;

class operator TFlexValue.Implicit(Value : TDateTime) : TFlexValue;
begin
  Result.AsDateTime := Value;
end;

class operator TFlexValue.Implicit(Value : Boolean) : TFlexValue;
begin
  Result.AsBoolean := Value;
end;

class operator TFlexValue.Implicit(Value : TClass) : TFlexValue;
begin
  Result.AsClass := Value;
end;

class operator TFlexValue.Implicit(Value : TObject) : TFlexValue;
begin
  Result.AsObject := Value;
end;

class operator TFlexValue.Implicit(Value : Pointer) : TFlexValue;
begin
  Result.AsPointer := Value;
end;

class operator TFlexValue.Implicit(Value: TVarRec): TFlexValue;
begin
  Result.AsVarRec := Value;
end;

class operator TFlexValue.Equal(a: TFlexValue; b: string): Boolean;
begin
  Result := a.AsString = b;
end;

class operator TFlexValue.Equal(a: TFlexValue; b: Int64): Boolean;
begin
  Result := a.AsInt64 = b;
end;

class operator TFlexValue.Equal(a: TFlexValue; b: Extended): Boolean;
begin
  Result := a.AsExtended = b;
end;

class operator TFlexValue.Equal(a: TFlexValue; b: Boolean): Boolean;
begin
  Result := a.AsBoolean = b;
end;

class operator TFlexValue.Equal(a : TFlexValue; b : Integer) : Boolean;
begin
  Result := a.AsInteger = b;
end;

class operator TFlexValue.NotEqual(a: TFlexValue; b: Int64): Boolean;
begin
  Result := a.AsInt64 <> b;
end;

class operator TFlexValue.NotEqual(a: TFlexValue; b: Integer): Boolean;
begin
  Result := a.AsInteger <> b;
end;

class operator TFlexValue.NotEqual(a: TFlexValue; b: string): Boolean;
begin
  Result := a.AsString <> b;
end;

class operator TFlexValue.NotEqual(a: TFlexValue; b: Boolean): Boolean;
begin
  Result := a.AsBoolean <> b;
end;

class operator TFlexValue.NotEqual(a: TFlexValue; b: Extended): Boolean;
begin
  Result := a.AsExtended <> b;
end;

class operator TFlexValue.GreaterThan(a: TFlexValue; b: Integer): Boolean;
begin
  Result := a.AsInteger > b;
end;

class operator TFlexValue.GreaterThan(a: TFlexValue; b: Int64): Boolean;
begin
  Result := a.AsInt64 > b;
end;

class operator TFlexValue.GreaterThan(a: TFlexValue; b: Extended): Boolean;
begin
  Result := a.AsExtended > b;
end;

class operator TFlexValue.GreaterThanOrEqual(a: TFlexValue; b: Integer): Boolean;
begin
  Result := a.AsInteger >= b;
end;

class operator TFlexValue.GreaterThanOrEqual(a: TFlexValue; b: Int64): Boolean;
begin
  Result := a.AsInt64 >= b;
end;

class operator TFlexValue.GreaterThanOrEqual(a: TFlexValue; b: Extended): Boolean;
begin
  Result := a.AsExtended >= b;
end;

class operator TFlexValue.LessThan(a: TFlexValue; b: Integer): Boolean;
begin
  Result := a.AsInteger < b;
end;

class operator TFlexValue.LessThan(a: TFlexValue; b: Int64): Boolean;
begin
  Result := a.AsInt64 < b;
end;

class operator TFlexValue.LessThan(a: TFlexValue; b: Extended): Boolean;
begin
  Result := a.AsExtended < b;
end;

class operator TFlexValue.LessThanOrEqual(a: TFlexValue; b : Integer): Boolean;
begin
  Result := a.AsInteger <= b;
end;

class operator TFlexValue.LessThanOrEqual(a: TFlexValue; b : Int64): Boolean;
begin
  Result := a.AsInt64 <= b;
end;

class operator TFlexValue.LessThanOrEqual(a: TFlexValue; b: Extended): Boolean;
begin
  Result := a.AsExtended <= b;
end;

function TFlexValue.IsArray: Boolean;
begin
  Result := fDataType = dtArray;
end;

function TFlexValue.IsBoolean: Boolean;
begin
  Result := fDataType = dtBoolean;
end;

function TFlexValue.IsDateTime: Boolean;
begin
  Result := fDataType = dtDateTime;
end;

function TFlexValue.IsFloating: Boolean;
begin
  Result := fDataType in [dtDouble,dtExtended];
end;

function TFlexValue.IsInteger: Boolean;
begin
  Result := fDataType in [dtInteger,dtInt64];
end;

function TFlexValue.IsInterface: Boolean;
begin
  Result := fDataType = dtInterface;
end;

function TFlexValue.IsNullOrEmpty: Boolean;
begin
  Result := fDataIntf = nil;
end;

function TFlexValue.IsObject: Boolean;
begin
  Result := fDataType = dtObject;
end;

function TFlexValue.IsPointer: Boolean;
begin
  Result := fDataType = dtPointer;
end;

function TFlexValue.IsRealExtended: Boolean;
var
  i : Extended;
begin
  Result := TryStrToFloat(AsString,i);
end;

function TFlexValue.IsRealInteger: Boolean;
var
  i : Int64;
begin
  Result := TryStrToInt64(AsString,i);
end;

function TFlexValue.IsString: Boolean;
begin
  Result := fDataType in [dtString,dtAnsiString,dtWideString];
end;

function TFlexValue.IsVariant: Boolean;
begin
  Result := fDataType = dtVariant;
end;

{$IFDEF MSWINDOWS}
procedure TFlexValue.SetAsAnsiString(const Value: AnsiString);
begin
  Clear;
  fDataIntf := TValueAnsiString.Create(Value);
  fDataType := TValueDataType.dtAnsiString;
end;
{$ENDIF}

procedure TFlexValue.SetAsBoolean(const Value: Boolean);
begin
  Clear;
  fDataIntf := TValueInteger.Create(Value.ToInteger);
  fDataType := TValueDataType.dtBoolean;
end;

procedure TFlexValue.SetAsCardinal(const Value: Cardinal);
begin
  Clear;
  fDataIntf := TValueInteger.Create(Value);
  fDataType := TValueDataType.dtInt64;
end;

procedure TFlexValue.SetAsClass(const Value: TClass);
begin
  Clear;
  fDataIntf := TValuePointer.Create(Value);
  fDataType := TValueDataType.dtClass;
end;

{$IFNDEF FPC}
procedure TFlexValue.SetAsCustom(aData: IInterface; aType: TValueDataType);
begin
  fDataIntf := aData;
  fDataType := aType;
end;
{$ENDIF}

procedure TFlexValue.SetAsDateTime(const Value: TDateTime);
begin
  Clear;
  fDataIntf := TValueExtended.Create(Value);
  fDataType := TValueDataType.dtDateTime;
end;

procedure TFlexValue.SetAsExtended(const Value: Extended);
begin
  Clear;
  fDataIntf := TValueExtended.Create(Value);
  fDataType := TValueDataType.dtExtended;
end;

procedure TFlexValue.SetAsInt64(const Value: Int64);
begin
  Clear;
  fDataIntf := TValueInteger.Create(Value);
  fDataType := TValueDataType.dtInt64;
end;

procedure TFlexValue.SetAsInteger(const Value: Integer);
begin
  Clear;
  fDataIntf := TValueInteger.Create(Value);
  fDataType := TValueDataType.dtInteger;
end;

procedure TFlexValue.SetAsInterface(const Value: IInterface);
begin
  {$IFNDEF FPC}
  fDataIntf := Value;
  {$ELSE}
  fDataIntf := Pointer(Value);
  {$ENDIF}
  fDataType := TValueDataType.dtInterface;
end;

procedure TFlexValue.SetAsObject(const Value: TObject);
begin
  Clear;
  fDataIntf := TValueObject.Create(Value);
  fDataType := TValueDataType.dtObject;
end;

procedure TFlexValue.SetAsPointer(const Value: Pointer);
begin
  Clear;
  fDataIntf := TValuePointer.Create(Value);
  fDataType := TValueDataType.dtPointer;
end;

procedure TFlexValue.SetAsString(const Value: string);
begin
  Clear;
  fDataIntf := TValueString.Create(Value);
  fDataType := TValueDataType.dtString;
end;

function TryVarAsType(aValue : Variant; aVarType : Word) : Boolean;
begin
  try
    VarAsType(aValue,aVarType);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TFlexValue.SetAsVariant(const Value: Variant);
var
  i : Int64;
  b : Boolean;
  f : Extended;
begin
  Clear;
  case VarType(Value) and varTypeMask of
    varEmpty,
    varNull      : Clear;
    varSmallInt,
    varInteger,
    varByte,
    varWord,
    varLongWord,
    varInt64     : SetAsInt64(Value);

    varSingle,
    varDouble,
    varCurrency  : SetAsExtended(Value);
    varDate      : SetAsDateTime(Value);
    varOleStr    : SetAsString(Value);
    varDispatch  : begin
                     if TryStrToInt64(Value,i) then SetAsInt64(i)
                     else if TryStrToFloat(Value,f) then SetAsExtended(f)
                     else if TryStrToBool(Value,b) then SetAsBoolean(b)
                     else if TryVarAsType(Value,varString) then SetAsString(Value)
                     else
                     begin
                       fDataIntf := TValueVariant.Create(Value);
                       fDataType := TValueDataType.dtVariant;
                     end;
                   end;
    //varError     : typeString := 'varError';
    varBoolean   : SetAsBoolean(Value);
    //varStrArg    : typeString := 'varStrArg';
    varString    : SetAsString(Value);
    //varAny       : typeString := 'varAny';
    //varTypeMask  : typeString := 'varTypeMask';
    else
    begin
      fDataIntf := TValueVariant.Create(Value);
      fDataType := TValueDataType.dtVariant;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TFlexValue.SetAsWideString(const Value: WideString);
begin
  Clear;
  fDataIntf := TValueWideString.Create(Value);
  fDataType := TValueDataType.dtWideString;
end;
{$ENDIF}

procedure TFlexValue.SetAsVarRec(const Value: TVarRec);
begin
  case Value.VType of
    {$IFNDEF NEXTGEN}
    vtString : AsString := string(Value.VString^);
    vtChar : AsString := string(Value.VChar);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    vtAnsiString : AsAnsiString := AnsiString(Value.VAnsiString);
    vtWideString : AsWideString := WideString(Value.VWideString);
    {$ENDIF}
    {$IFDEF UNICODE}
    vtUnicodeString: AsString := string(Value.VUnicodeString);
    {$ENDIF UNICODE}
    vtInteger : AsInteger := Value.VInteger;
    vtInt64 : AsInt64 := Value.VInt64^;
    vtExtended : AsExtended := Value.VExtended^;
    vtBoolean : AsBoolean := Value.VBoolean;
    vtVariant : AsVariant := Value.VVariant^;
    vtInterface : AsInterface := IInterface(Value.VInterface);
    vtClass : AsClass := Value.VClass;
    vtObject : AsObject := Value.VObject;
    vtPointer : AsPointer := Value.VPointer;
    else raise Exception.Create('DataType not supported by TFlexValue');
  end;
  {$IFDEF FPC}
  fDataIntf._AddRef;
  {$ENDIF}
end;

procedure TFlexValue._AddRef;
begin
  if Assigned(fDataIntf) then fDataIntf._AddRef;
end;

procedure TFlexValue._Release;
begin
  if Assigned(fDataIntf) then fDataIntf._Release;
end;

{ TValueStringData }

constructor TValueString.Create(const Value: string);
begin
  fData := Value;
end;

function TValueString.GetValue: string;
begin
  Result := fData;
end;

procedure TValueString.SetValue(const Value: string);
begin
  fData := Value;
end;

{ TValueVariantData }

constructor TValueVariant.Create(const Value: Variant);
begin
  fData := Value;
end;

function TValueVariant.GetValue: Variant;
begin
  Result := fData;
end;

procedure TValueVariant.SetValue(const Value: Variant);
begin
  fData := Value;
end;

{ TValueAnsiStringData }

{$IFDEF MSWINDOWS}
constructor TValueAnsiString.Create(const Value: AnsiString);
begin
  fData := Value;
end;

function TValueAnsiString.GetValue: AnsiString;
begin
  Result := fData;
end;

procedure TValueAnsiString.SetValue(const Value: AnsiString);
begin
  fData := Value;
end;

{ TValueWideStringData }

constructor TValueWideString.Create(const Value: WideString);
begin
  fData := Value;
end;

function TValueWideString.GetValue: WideString;
begin
  Result := fData;
end;

procedure TValueWideString.SetValue(const Value: WideString);
begin
  fData := Value;
end;
{$ENDIF}

{ TValueInteger }

constructor TValueInteger.Create(const Value: Int64);
begin
  fData := Value;
end;

function TValueInteger.GetValue: Int64;
begin
  Result := fData;
end;

procedure TValueInteger.SetValue(const Value: Int64);
begin
  fData := Value;
end;

{ TValuePointer }

constructor TValuePointer.Create(const Value: Pointer);
begin
  fData := Value;
end;

function TValuePointer.GetValue: Pointer;
begin
  Result := fData;
end;

procedure TValuePointer.SetValue(const Value: Pointer);
begin
  fData := Value;
end;

{ TValueExtended }

constructor TValueExtended.Create(const Value: Extended);
begin
  fData := Value;
end;

function TValueExtended.GetValue: Extended;
begin
  Result := fData;
end;

procedure TValueExtended.SetValue(const Value: Extended);
begin
  fData := Value;
end;

{ TValueObject }

constructor TValueObject.Create(const Value: TObject);
begin
  fData := Value;
end;

function TValueObject.GetValue: TObject;
begin
  Result := fData;
end;

procedure TValueObject.SetValue(const Value: TObject);
begin
  fData := Value;
end;


{ TFlexPair }

constructor TFlexPair.Create(const aName: string; aValue: TFlexValue);
begin
  Name := aName;
  Value := aValue;
end;

end.
