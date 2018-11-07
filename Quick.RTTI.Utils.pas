unit Quick.RTTI.Utils;

interface

uses
  SysUtils,
  Rtti;

type

  TRTTI = class
  private class var
    fCtx : TRttiContext;
  public
    //class function GetProperties();
    class function GetField(aInstance : TObject; const aFieldName : string) : TRttiField; overload;
    class function GetField(aTypeInfo : Pointer; const aFieldName : string) : TRttiField; overload;
    class function FieldExists(aTypeInfo : Pointer; const aFieldName : string) : Boolean;
    class function GetFieldValue(aInstance : TObject; const aFieldName : string) : TValue; overload;
    class function GetFieldValue(aTypeInfo : Pointer; const aFieldName: string) : TValue; overload;
    class function GetProperty(aInstance : TObject; const aPropertyName : string) : TRttiProperty; overload;
    class function GetProperty(aTypeInfo : Pointer; const aPropertyName : string) : TRttiProperty; overload;
    class function PropertyExists(aTypeInfo : Pointer; const aPropertyName : string) : Boolean;
    class function GetPropertyValue(aInstance : TObject; const aPropertyName : string) : TValue; overload;
    class function GetPropertyValue(aTypeInfo : Pointer; const aPropertyName : string) : TValue; overload;
  end;

  ERTTIError = class(Exception);

implementation

{ TRTTIUtils }

class function TRTTI.FieldExists(aTypeInfo: Pointer; const aFieldName: string): Boolean;
var
  rtype : TRttiType;
begin
  rtype := fCtx.GetType(aTypeInfo);
  Result := rtype.GetField(aFieldName) <> nil;
end;

class function TRTTI.GetField(aInstance: TObject; const aFieldName: string): TRttiField;
var
  rtype : TRttiType;
begin
  rtype := fCtx.GetType(aInstance.ClassInfo);
  if rtype <> nil then
  begin
    Result := rtype.GetField(aFieldName);
  end;
end;

class function TRTTI.GetField(aTypeInfo: Pointer; const aFieldName: string): TRttiField;
var
  rtype : TRttiType;
begin
  rtype := fCtx.GetType(aTypeInfo);
  if rtype <> nil then
  begin
    Result := rtype.GetField(aFieldName);
  end;
end;

class function TRTTI.GetFieldValue(aInstance : TObject; const aFieldName: string): TValue;
var
  rfield: TRttiField;
begin
  rfield := GetField(aInstance,aFieldName);
  if rfield <> nil then Result := rfield.GetValue(aInstance);
end;

class function TRTTI.GetFieldValue(aTypeInfo : Pointer; const aFieldName: string): TValue;
var
  rfield: TRttiField;
begin
  rfield := GetField(aTypeInfo,aFieldName);
  if rfield <> nil then rfield.GetValue(aTypeInfo);
end;

class function TRTTI.GetProperty(aInstance: TObject; const aPropertyName: string): TRttiProperty;
var
  rtype : TRttiType;
begin
  rtype := fCtx.GetType(aInstance.ClassInfo);
  if rtype <> nil then Result := rtype.GetProperty(aPropertyName);
end;

class function TRTTI.GetProperty(aTypeInfo: Pointer; const aPropertyName: string): TRttiProperty;
var
  rtype : TRttiType;
begin
  rtype := fCtx.GetType(aTypeInfo);
  if rtype <> nil then  Result := rtype.GetProperty(aPropertyName);
end;

class function TRTTI.GetPropertyValue(aInstance: TObject; const aPropertyName: string): TValue;
var
  rprop : TRttiProperty;
begin
  rprop := GetProperty(aInstance,aPropertyName);
  if rprop <> nil then Result := rprop.GetValue(aInstance);
end;

class function TRTTI.GetPropertyValue(aTypeInfo: Pointer; const aPropertyName: string): TValue;
var
  rprop : TRttiProperty;
begin
  rprop := GetProperty(aTypeInfo,aPropertyName);
  if rprop <> nil then Result := rprop.GetValue(aTypeInfo);
end;

class function TRTTI.PropertyExists(aTypeInfo: Pointer; const aPropertyName: string): Boolean;
begin
  Result := fCtx.GetType(aTypeInfo).GetProperty(aPropertyName) <> nil;
end;


end.
