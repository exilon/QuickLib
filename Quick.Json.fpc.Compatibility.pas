unit Quick.Json.fpc.Compatibility;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  jsonparser;

type

  TJsonPair = class
  private
    fJsonString : string;
    fJsonValue : TJsonData;
  public
    constructor Create(const aName : string; aValue : TJsonData);
    property JsonString : string read fJsonString write fJsonString;
    property JsonValue : TJsonData read fJsonValue write fJsonValue;
  end;

  { TJsonArray }

  TJsonArray = class(fpjson.TJSONArray)
  public
    procedure AddElement(aValue : TJsonData);
  end;

  { TJsonObject }

  TJsonObject = class(fpjson.TJsonObject)
  private
    function GetPair(Index : Integer): TJsonPair;
  public
    procedure AddPair(aValue : TJsonPair); overload;
    procedure AddPair(const aName : TJsonStringType; aValue : TJsonData); overload;
    class function ParseJSONValue(const JSON: string; const UseUTF8: Boolean = True): TJSONData;
    function GetValue(const aName : string) : TJsonData;
    function ToJson : TJSONStringType;
    property Pairs[Index : Integer] : TJsonPair read GetPair; default;
  end;

  TJsonValue = TJsonData;

  TJsonNumber = class(fpjson.TJsonIntegerNumber)
  public
    constructor Create(aValue : Integer); overload;
    constructor Create(aValue : Extended); overload;
  end;

  TJsonString = class(fpjson.TJsonString)
  public
    constructor Create(const aValue : string); overload;
  end;

  TJsonBool = class(fpjson.TJSONBoolean)
  public
    constructor Create(aValue : Boolean);
  end;

implementation

{ TJsonArray }

procedure TJsonArray.AddElement(aValue: TJsonData);
begin
  Add(aValue);
end;

function TJsonObject.GetPair(Index : Integer): TJsonPair;
begin
  Result := TJsonPair.Create(Self.Names[Index],Self.Items[Index]);
end;

procedure TJsonObject.AddPair(aValue : TJsonPair);
begin
  Add(aValue.JsonString,aValue.JsonValue);
end;

procedure TJsonObject.AddPair(const aName: TJsonStringType; aValue: TJsonData);
begin
  Add(aName,aValue);
end;

class function TJsonObject.ParseJSONValue(const JSON: string; const UseUTF8: Boolean = True): TJSONData;
begin
  Result := GetJson(JSON,UseUTF8);
end;

function TJsonObject.ToJson : TJSONStringType;
begin
  try
    Result := AsJson;
  except
    raise Exception.Create('Json not valid');
  end;
end;

function TJsonObject.GetValue(const aName : string) : TJsonData;
begin
  Result := Find(aName);
end;

constructor TJsonPair.Create(const aName : string; aValue : TJsonData);
begin
  fJsonString := aName;
  fJsonValue := aValue;
end;

constructor TJsonNumber.Create(aValue : Integer);
begin
  inherited Create(aValue);
end;

constructor TJsonNumber.Create(aValue : Extended);
begin
  Create(aValue);
end;

constructor TJsonBool.Create(aValue : Boolean);
begin
  inherited Create(aValue);
end;

constructor TJsonString.Create(const aValue : string);
begin
  inherited Create(aValue);
end;

end.

