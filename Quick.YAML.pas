{ ***************************************************************************

  Copyright (c) 2015-2020 Kike Pérez

  Unit        : Quick.YAML
  Description : YAML Object parser
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 17/04/2019
  Modified    : 05/02/2020

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

unit Quick.YAML;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  Quick.Commons,
  Generics.Collections,
  Quick.Value;

type
  TYamlScalar = TFlexValue;

  TYamlAncestor = class abstract
  protected
    fOwned : Boolean;
    function IsNull : Boolean; virtual;
    procedure AddDescendant(const aDescendent: TYamlAncestor); virtual;
  public
    constructor Create;
    property Owned : Boolean read fOwned write fOwned;
    property Null : Boolean read IsNull;
    function IsScalar : Boolean; virtual;
  end;

  TYamlValue = class abstract(TYamlAncestor)
  public
    function Value : TFlexValue; virtual;
    function AsString : string; virtual; abstract;
  end;

  TYamlString = class(TYamlValue)
  private
    fValue : string;
    fIsNull : Boolean;
  protected
    function IsNull : Boolean; override;
  public
    constructor Create; overload;
    constructor Create(const aValue : string); overload;
    function Value : TFlexValue; override;
    function IsScalar : Boolean; override;
    function AsString : string; override;
  end;

  TYamlInteger = class(TYamlValue)
  private
    fValue : Integer;
    fIsNull : Boolean;
  protected
    function IsNull : Boolean; override;
  public
    constructor Create; overload;
    constructor Create(const aValue : Integer); overload;
    function Value : TFlexValue; override;
    function IsScalar : Boolean; override;
    function AsString : string; override;
  end;

  TYamlFloat = class(TYamlValue)
  private
    fValue : Double;
    fIsNull : Boolean;
  protected
    function IsNull : Boolean; override;
  public
    constructor Create; overload;
    constructor Create(const aValue : Double); overload;
    function Value : TFlexValue; override;
    function IsScalar : Boolean; override;
    function AsString : string; override;
  end;

  TYamlBoolean = class(TYamlValue)
  private
    fValue : Boolean;
    fIsNull : Boolean;
  protected
    function IsNull : Boolean; override;
  public
    constructor Create; overload;
    constructor Create(const aValue : Boolean); overload;
    function Value : TFlexValue; override;
    function IsScalar : Boolean; override;
    function AsString : string; override;
  end;

  TYamlNull = class(TYamlValue)
  protected
    function IsNull: Boolean; override;
  public
    function Value : TFlexValue; override;
    function AsString : string; override;
  end;

  TYamlComment = class(TYamlValue)
  private
    fValue : string;
    fIsNull : Boolean;
  protected
    function IsNull : Boolean; override;
  public
    constructor Create; overload;
    constructor Create(const aComment : string); overload;
    function Value : TFlexValue; override;
    function IsScalar : Boolean; override;
    function AsString : string; override;
  end;

  TYamlPair = class(TYamlAncestor)
  private
    fName : string;
    fValue : TYamlValue;
  protected
    procedure AddDescendant(const aDescendent: TYamlAncestor); override;
  public
    constructor Create(const aName : string; const aValue : TYamlValue); overload;
    constructor Create(const aName : string; const aValue : string); overload;
    constructor Create(const aName : string; const aValue : Integer); overload;
    constructor Create(const aName : string; const aValue : Double); overload;
    destructor Destroy; override;
    property Name : string read fName write fName;
    property Value : TYamlValue read fValue write fValue;
    function ToYaml : string;
  end;

  TYamlWriter = class
  private
    fData : string;
  public
    constructor Create;
    property Text : string read fData;
    procedure Write(const aValue : string);
    procedure Writeln(const aValue : string);
  end;

  TYamlObject = class(TYamlValue)
  public type
    TEnumerator = class
    private
      fIndex: Integer;
      fObject: TYamlObject;
    public
      constructor Create(const aObject: TYamlObject);
      function GetCurrent: TYamlPair; inline;
      function MoveNext: Boolean; inline;
      property Current: TYamlPair read GetCurrent;
    end;
  private
    fMembers : TList<TYamlPair>;
    function GetCount : Integer;
    class function ParseValue(yaml : TList<string>; var vIndex : Integer): TYamlAncestor;
    class function ParsePairName(const aPair : string) : string;
    class function ParsePairValue(const aPair : string) : string;
    class function ParseArrayValue(const aValue : string) : TYamlValue;
    class function GetItemLevel(const aValue : string) : Integer;
    function InSameLevel(const aValue1, aValue2 : string) : Boolean;
    function ParseToYaml(aIndent : Integer) : string;
  protected
    procedure AddDescendant(const aDescendent: TYamlAncestor); override;
  public
    constructor Create; overload;
    constructor Create(const aData : string); overload;
    destructor Destroy; override;
    function GetValue(const aName: string): TYamlValue;
    property Values[const aName: string] : TYamlValue read GetValue;
    procedure ParseYaml(const aData : string);
    property Count : Integer read GetCount;
    class function ParseYamlValue(const aData : string) : TYamlAncestor;
    function GetPair(const aIndex : Integer) : TYamlPair;
    function GetPairByName(const aPairName : string) : TYamlPair;
    function AddPair(const aPair : TYamlPair): TYamlObject; overload;
    function AddPair(const aName : string; const aValue : TYamlValue): TYamlObject; overload;
    function AddPair(const aName : string; const aValue : string): TYamlObject; overload;
    function RemovePair(const aPairName: string): TYamlPair;
    function GetEnumerator: TEnumerator; inline;
    property Pairs[const aIndex: Integer]: TYamlPair read GetPair;
    function ToYaml : string;
  end;

  { TYamlArray }

  TYamlArray = class(TYamlValue)
  public type
    TEnumerator = class
    private
      fIndex : Integer;
      fArray : TYamlArray;
    public
      constructor Create(const aArray: TYamlArray);
      function GetCurrent: TYamlValue; inline;
      function MoveNext: Boolean; inline;
      property Current: TYamlValue read GetCurrent;
    end;
  private
    fElements: TList<TYamlValue>;
    function ParseToYaml(aIndent : Integer; var vIsScalar : Boolean) : string;
  protected
    procedure AddDescendant(const aDescendant: TYamlAncestor); override;
    function GetCount: Integer; inline;
    function GetValue(const aIndex: Integer): TYamlValue; overload; inline;
  public
    constructor Create; overload;
    constructor Create(const aFirstElem: TYamlValue); overload;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[const aIndex: Integer]: TYamlValue read GetValue;
    procedure AddElement(const aElement: TYamlValue);
    function GetEnumerator: TEnumerator; inline;
  end;

  EYAMLException = class(Exception);

implementation

const
  CRLF = #13#10;
  NUM_INDENT = 2;


{ TYamlAncestor }

procedure TYamlAncestor.AddDescendant(const aDescendent: TYamlAncestor);
begin
  raise EYAMLException.CreateFmt('Cannot add value %s to %s',[aDescendent.ClassName,ClassName]);
end;

constructor TYamlAncestor.Create;
begin
  inherited Create;
  fOwned := True;
end;

function TYamlAncestor.IsNull: Boolean;
begin
  Result := False;
end;

function TYamlAncestor.IsScalar: Boolean;
begin
  Result := False;
end;

{ TYamlObject }

function TYamlObject.AddPair(const aPair: TYamlPair): TYamlObject;
begin
  if aPair <> nil then AddDescendant(aPair);
  Result := Self;
end;

function TYamlObject.AddPair(const aName: string; const aValue: TYamlValue): TYamlObject;
begin
  if (not aName.IsEmpty) and (aValue <> nil) then AddPair(TYamlPair.Create(aName,aValue));
  Result := Self;
end;

procedure TYamlObject.AddDescendant(const aDescendent: TYamlAncestor);
begin
  if aDescendent <> nil then fMembers.Add(TYamlPair(aDescendent));
end;

function TYamlObject.AddPair(const aName, aValue: string): TYamlObject;
begin
  if not aName.IsEmpty and (not aValue.IsEmpty) then AddPair(TYamlPair.Create(aName,aValue));
  Result := Self;
end;

constructor TYamlObject.Create(const aData: string);
begin
  inherited Create;
  ParseYaml(aData);
end;

constructor TYamlObject.Create;
begin
  inherited Create;
  fMembers := TList<TYamlPair>.Create;
end;

destructor TYamlObject.Destroy;
var
  member: TYamlAncestor;
  i: Integer;
begin
  if Assigned(fMembers) then
  for i := 0 to fMembers.Count - 1 do
  begin
    {$IFNDEF FPC}
    member := fMembers.List[i];
    {$ELSE}
    member := fMembers.Items[i];
    {$ENDIF}
    if Assigned(member) and member.Owned then member.Free;
  end;
  FreeAndNil(fMembers);
  inherited;
end;

function TYamlObject.GetCount: Integer;
begin
  Result := fMembers.Count;
end;

function TYamlObject.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

class function TYamlObject.GetItemLevel(const aValue: string): Integer;
var
  i : Integer;
  trimed : string;
begin
  trimed := aValue.Trim;
  if trimed.IsEmpty or trimed.StartsWith('#') then Exit(99999);

  for i := Low(aValue) to aValue.Length do
  begin
    if aValue[i] <> ' ' then Exit(i);
  end;
  Result := Low(aValue);
end;

function TYamlObject.GetPair(const aIndex: Integer): TYamlPair;
begin
  Result := fMembers[aIndex];
end;

function TYamlObject.GetPairByName(const aPairName: string): TYamlPair;
var
  yamlpair : TYamlPair;
  I: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    {$IFNDEF FPC}
    yamlpair := fMembers.List[i];
    {$ELSE}
    yamlpair := fMembers.Items[i];
    {$ENDIF}
    if CompareText(yamlpair.Name,aPairName) = 0 then Exit(yamlpair);
  end;
  Result := nil;
end;

function TYamlObject.GetValue(const aName: string): TYamlValue;
var
  ymlpair: TYamlPair;
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    {$IFNDEF FPC}
    ymlpair := fMembers.List[i];
    {$ELSE}
    ymlpair := fMembers.Items[i];
    {$ENDIF}
    if CompareText(ymlpair.Name,aName) = 0 then Exit(ymlpair.Value);
  end;
  Result := nil;
end;

function TYamlObject.InSameLevel(const aValue1, aValue2: string): Boolean;
begin
  Result := GetItemLevel(aValue1) = GetItemLevel(aValue2);
end;

class function TYamlObject.ParseArrayValue(const aValue: string): TYamlValue;
var
  nint : Int64;
  nfloat : Double;
begin
  if TryStrToInt64(aValue,nint) then Result := TYamlInteger.Create(nint)
  else if TryStrToFloat(aValue,nfloat) then Result := TYamlFloat.Create(nfloat)
  else Result := TYamlString.Create(aValue);
end;

class function TYamlObject.ParsePairName(const aPair: string): string;
begin
  Result := Copy(aPair,0,aPair.IndexOf(':'));
end;

class function TYamlObject.ParsePairValue(const aPair: string): string;
begin
  Result := Copy(aPair,aPair.IndexOf(':')+2,aPair.Length).Trim;
end;

class function TYamlObject.ParseValue(yaml : TList<string>; var vIndex : Integer): TYamlAncestor;
type
  TYamlType = (ytObject, ytArray, ytScalarArray, ytScalar);
var
  name : string;
  value : string;
  yvalue : TYamlAncestor;
  level : Integer;
  nextlevel : Integer;
  aitem : string;
  yamlType : TYamlType;
begin
  while yaml.Count > vIndex do
  begin
    value := yaml[vIndex].Trim;

    name := ParsePairName(value);
    if (name.IsEmpty) or (value.IsEmpty) or (value.StartsWith('#')) or (value.StartsWith(#9)) then Exit(nil)
    //else if value.StartsWith('#') then Exit(TYamlComment.Create(value))
    else if value.StartsWith('-') then
    begin
      yaml[vIndex] := StringReplace(yaml[vIndex],'-','',[]).TrimLeft;
      yamlType := ytObject;
      Dec(vIndex);
    end
    else if value.EndsWith(':') then
    begin
      if yaml[vIndex + 1].TrimLeft.StartsWith('-') then yamlType := ytArray
        else yamlType := ytObject;
    end
    else if value.IndexOf(':') < value.Length then
    begin
      value := ParsePairValue(value);
      if (value.StartsWith('[')) and (value.EndsWith(']')) then yamlType := ytScalarArray
        else yamlType := ytScalar;
    end;

    case yamlType of
      ytArray : //is array
        begin
          yvalue := TYamlArray.Create;
          level := GetItemLevel(yaml[vIndex + 1]);
          repeat
            Inc(vIndex);
            yvalue.AddDescendant(ParseValue(yaml,vIndex));
          until (yvalue = nil) or (vIndex >= yaml.Count - 1) or (GetItemLevel(yaml[vIndex + 1]) < level);
          Exit(TYamlPair.Create(name,TYamlValue(yvalue)));
        end;
      ytObject : //is object
        begin
          yvalue := TYamlObject.Create;
          repeat
            Inc(vIndex);
            nextlevel := GetItemLevel(yaml[vIndex]);
            if nextlevel <> 99999 then level := nextlevel;

            yvalue.AddDescendant(ParseValue(yaml,vIndex));
            //level := GetItemLevel(yaml[vIndex]);
            //var level2 := GetItemLevel(yaml[offset + 1]);
          until (yvalue = nil) or (vIndex >= yaml.Count - 1) or (GetItemLevel(yaml[vIndex + 1]) < level);
          Exit(TYamlPair.Create(name,TYamlValue(yvalue)));
        end;
      ytScalarArray : //is scalar array
        begin
          yvalue := TYamlArray.Create;
          value := StringReplace(Copy(value,2,Value.Length-2),', ',#9,[rfReplaceAll]);
          for aitem in value.Split([#9]) do
          begin
            yvalue.AddDescendant(ParseArrayValue(aitem));
          end;
          Exit(TYamlPair.Create(name,TYamlValue(yvalue)));
        end;
    else Exit(TYamlPair.Create(name,value)); //is scalar
    end;
    Inc(vIndex);
  end;
end;

procedure TYamlObject.ParseYaml(const aData: string);
var
  yaml : TList<string>;
  line : string;
  data : string;
  yamlvalue : TYamlAncestor;
  vIndex : Integer;
begin
  yaml := TList<string>.Create;
  try
    vIndex := 0;
    //normalize tabs
    data := StringReplace(aData,#9,Spaces(4),[rfReplaceAll]);
    {$IFNDEF LINUX}
    for line in data.Split([#13]) do yaml.Add(StringReplace(line,#10,'',[rfReplaceAll]));
    {$ELSE}
    for line in data.Split([#10]) do yaml.Add(StringReplace(line,#13,'',[rfReplaceAll]));
    {$ENDIF}
    while yaml.Count > vIndex do
    begin
      yamlvalue := ParseValue(yaml,vIndex);
      if yamlvalue <> nil then AddDescendant(yamlvalue);
      Inc(vIndex);
    end;
  finally
    yaml.Free;
  end;
end;

class function TYamlObject.ParseYamlValue(const aData : string) : TYamlAncestor;
var
  yaml : TList<string>;
  line : string;
  data : string;
  yamlvalue : TYamlAncestor;
  vIndex : Integer;
begin
  yaml := TList<string>.Create;
  try
    vIndex := 0;
    //normalize tabs
    data := StringReplace(aData,#9,Spaces(4),[rfReplaceAll]);
    {$IFNDEF LINUX}
    for line in data.Split([#13]) do yaml.Add(StringReplace(line,#10,'',[rfReplaceAll]));
    {$ELSE}
    for line in data.Split([#10]) do yaml.Add(StringReplace(line,#13,'',[rfReplaceAll]));
    {$ENDIF}
    if yaml[0].TrimLeft.StartsWith('- ') then Result := TYamlArray.Create
      else Result := TYamlObject.Create;
    while yaml.Count > vIndex do
    begin
      yamlvalue := ParseValue(yaml,vIndex);
      if yamlvalue <> nil then Result.AddDescendant(yamlvalue);
      Inc(vIndex);
    end;
  finally
    yaml.Free;
  end;
end;

function TYamlObject.RemovePair(const aPairName: string): TYamlPair;
var
  yamlpair: TYamlPair;
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    {$IFNDEF FPC}
    yamlpair := TYamlPair(FMembers.List[i]);
    {$ELSE}
    yamlpair := TYamlPair(fMembers.Items[i]);
    {$ENDIF}
    if CompareText(yamlpair.Name,aPairName) = 0 then
    begin
      fMembers.Remove(yamlpair);
      Exit(yamlpair);
    end;
  end;
  Result := nil;
end;

function TYamlObject.ToYaml: string;
begin
  Result := ParseToYaml(0);
end;

function TYamlObject.ParseToYaml(aIndent : Integer) : string;
var
  i : Integer;
  member : TYamlPair;
  yaml : TYamlWriter;
  yvalue : TYamlAncestor;
  indent : string;
  isscalar : Boolean;
  scalar : string;
  rarray : string;
begin
  yaml := TYamlWriter.Create;
  try
    indent := StringOfChar(' ',aIndent);
    for i := 0 to fMembers.Count - 1 do
    begin
      member := fMembers[i];
      if member = nil then continue;

      yvalue := member.Value;
      if (yvalue.IsScalar) or (yvalue is TYamlNull) then
      begin
        if yvalue is TYamlComment then yaml.Writeln(Format('#%s%s',[indent,TYamlComment(member.Value).AsString]))
        else
        begin
          if yvalue is TYamlNull then scalar := 'null'
            else scalar := member.Value.Value.AsString;
          if scalar.IsEmpty then scalar := '""';
          yaml.Writeln(Format('%s%s: %s',[indent,member.Name,scalar]));
          if (i < fMembers.Count - 1) and (fMembers[i+1].Value is TYamlComment) then yaml.Writeln('');
        end;
      end
      else if (yvalue is TYamlObject) then
      begin
        yaml.Writeln(Format('%s%s:',[indent,member.Name]));
        yaml.Write((yvalue as TYamlObject).ParseToYaml(aIndent + NUM_INDENT));
        if aIndent = 0 then yaml.Writeln('');
      end
      else if (yvalue is TYamlArray) then
      begin
        isscalar := False;
        rarray := (yvalue as TYamlArray).ParseToYaml(aIndent + NUM_INDENT,isscalar);
        if isscalar then yaml.Writeln(Format('%s%s: %s',[indent,member.Name,rarray]))
        else
        begin
          yaml.Writeln(Format('%s%s:',[indent,member.Name]));
          yaml.Write(rarray);
        end;
      end;
    end;
    Result := yaml.Text;
  finally
    yaml.Free;
  end;
end;

{ TYamlString }

constructor TYamlString.Create(const aValue: string);
begin
  inherited Create;
  fValue := aValue;
  fIsNull := False;
end;

constructor TYamlString.Create;
begin
  inherited Create;
  fIsNull := True;
end;

function TYamlString.IsNull: Boolean;
begin
  Result := fIsNull;
end;

function TYamlString.IsScalar: Boolean;
begin
  Result := True;
end;

function TYamlString.AsString: string;
begin
  Result := fValue;
end;

function TYamlString.Value: TFlexValue;
begin
  Result := fValue;
end;

{ TYamlInteger }

constructor TYamlInteger.Create(const aValue: Integer);
begin
  inherited Create;
  fValue := aValue;
  fIsNull := False;
end;

constructor TYamlInteger.Create;
begin
  inherited Create;
  fIsNull := True;
end;

function TYamlInteger.IsNull: Boolean;
begin
  Result := fIsNull;
end;

function TYamlInteger.IsScalar: Boolean;
begin
  Result := True;
end;

function TYamlInteger.AsString: string;
begin
  Result := IntToStr(fValue);
end;

function TYamlInteger.Value: TFlexValue;
begin
  Result := fValue;
end;

{ TYamlFloat }

constructor TYamlFloat.Create(const aValue: Double);
begin
  inherited Create;
  fValue := aValue;
  fIsNull := False;
end;

constructor TYamlFloat.Create;
begin
  inherited Create;
  fIsNull := True;
end;

function TYamlFloat.IsNull: Boolean;
begin
  Result := fIsNull;
end;

function TYamlFloat.IsScalar: Boolean;
begin
  Result := True;
end;

function TYamlFloat.AsString: string;
begin
  Result := FloatToStr(fValue);
end;

function TYamlFloat.Value: TFlexValue;
begin
  Result := fValue;
end;

{ TYamlPair }

constructor TYamlPair.Create(const aName: string; const aValue: TYamlValue);
begin
  inherited Create;
  fName := aName;
  fValue := aValue;
end;

constructor TYamlPair.Create(const aName, aValue: string);
begin
  inherited Create;
  fName := aName;
  fValue := TYamlString.Create(aValue);
end;

constructor TYamlPair.Create(const aName: string; const aValue: Double);
begin
  inherited Create;
  fName := aName;
  fValue := TYamlFloat.Create(aValue);
end;

constructor TYamlPair.Create(const aName: string; const aValue: Integer);
begin
  inherited Create;
  fName := aName;
  fValue := TYamlInteger.Create(aValue);
end;

destructor TYamlPair.Destroy;
begin
  if (fValue <> nil) and fValue.Owned then FreeAndNil(fValue);
  inherited Destroy;
end;

function TYamlPair.ToYaml: string;
var
  isscalar : Boolean;
begin
  if fValue = nil then Exit('null');

  if fValue is TYamlObject then Result := TYamlObject(fValue).ToYaml
    else if fValue is TYamlArray then Result := TYamlArray(fValue).ParseToYaml(0,isscalar)
      else Result := Format('%s: %s',[fName,fValue.Value.AsString]);

end;

procedure TYamlPair.AddDescendant(const aDescendent: TYamlAncestor);
begin
  if fName = '' then
    fName := TYamlString(aDescendent).Value
  else if fValue = nil then
    fValue:= TYamlValue(aDescendent)
  else inherited AddDescendant(aDescendent);
end;

{ TYamlObject.TEnumerator }

constructor TYamlObject.TEnumerator.Create(const aObject: TYamlObject);
begin
  inherited Create;
  fIndex := -1;
  fObject := aObject;
end;

function TYamlObject.TEnumerator.GetCurrent: TYamlPair;
begin
  {$IFNDEF FPC}
  Result := fObject.fMembers.List[fIndex];
  {$ELSE}
  Result := fObject.fMembers.Items[fIndex];
  {$ENDIF}
end;

function TYamlObject.TEnumerator.MoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex < fObject.Count;
end;


{ TYamlValue }

function TYamlValue.Value: TFlexValue;
begin
  Result := '';
end;

{ TYamlArray.TEnumerator }

constructor TYamlArray.TEnumerator.Create(const aArray: TYamlArray);
begin
  inherited Create;
  fIndex := -1;
  fArray := aArray;
end;

function TYamlArray.TEnumerator.GetCurrent: TYamlValue;
begin
  {$IFNDEF FPC}
  Result := fArray.fElements.List[fIndex];
  {$ELSE}
  Result := fArray.fElements.Items[fIndex];
  {$ENDIF}
end;

function TYamlArray.TEnumerator.MoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex < fArray.Count;
end;

{ TYamlArray }

procedure TYamlArray.AddDescendant(const aDescendant: TYamlAncestor);
begin
  fElements.Add(TYamlValue(aDescendant));
end;

constructor TYamlArray.Create;
begin
  inherited Create;
  fElements := TList<TYamlValue>.Create;
end;

constructor TYamlArray.Create(const aFirstElem: TYamlValue);
begin
  inherited Create;
  AddElement(aFirstElem);
end;

procedure TYamlArray.AddElement(const aElement: TYamlValue);
begin
  if aElement <> nil then AddDescendant(aElement);
end;

destructor TYamlArray.Destroy;
var
  element: TYamlAncestor;
  i: Integer;
begin
  if Assigned(fElements) then
  for i := 0 to fElements.Count - 1 do
  begin
    element := fElements[i];
    if Assigned(element) and (element.Owned) then element.Free;
  end;
  if Assigned(fElements) then FreeAndNil(fElements);
  inherited Destroy;
end;

function TYamlArray.GetCount: Integer;
begin
  Result := fElements.Count;
end;

function TYamlArray.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TYamlArray.GetValue(const aIndex: Integer): TYamlValue;
begin
  Result := fElements[aIndex];
end;

function TYamlArray.ParseToYaml(aIndent : Integer; var vIsScalar : Boolean) : string;
var
  element : TYamlValue;
  yaml : TYamlWriter;
  yvalue : TYamlAncestor;
  indent : string;
  isscalar : Boolean;
begin
  Result := '';
  yaml := TYamlWriter.Create;
  try
    indent := StringOfChar(' ',aIndent);
    if fElements.Count = 0 then
    begin
      vIsScalar := True;
      Exit('[]');
    end;
    for element in fElements do
    begin
      yvalue := element;
      if yvalue is TYamlPair then yvalue := TYamlPair(yvalue).value;

      if yvalue.IsScalar then
      begin
        {$IFNDEF FPC}
        if Result = '' then Result := element.AsString
          else Result := Result + ', ' + element.AsString;
        {$ELSE}
        if Result = '' then Result := TYamlPair(element).Value.AsString
          else Result := Result + ', ' + TYamlPair(element).Value.AsString;
        {$ENDIF}
      end
      else if (yvalue is TYamlObject) then
      begin
        yaml.Write(indent + '- ' + (yvalue as TYamlObject).ParseToYaml(aIndent + NUM_INDENT).TrimLeft);
      end
      else if (yvalue is TYamlArray) then
      begin
        yaml.Write(Format('%s%s',[indent,(yvalue as TYamlArray).ParseToYaml(aIndent + NUM_INDENT,isscalar)]))
      end;
      yaml.Writeln('');
    end;
    if yvalue.IsScalar then
    begin
      Result := '[' + Result + ']';
      vIsScalar := True;
    end
    else Result := yaml.Text;
  finally
    yaml.Free;
  end;
end;

{ TYamlWriter }

procedure TYamlWriter.Write(const aValue: string);
begin
  fData := fData + aValue;
end;

procedure TYamlWriter.Writeln(const aValue: string);
begin
  fData := fData + aValue + CRLF;
end;

constructor TYamlWriter.Create;
begin
  fData := '';
end;

{ TYamlNull }

function TYamlNull.IsNull: Boolean;
begin
  Result := True;
end;

function TYamlNull.AsString: string;
begin
  Result := 'null';
end;

function TYamlNull.Value: TFlexValue;
begin
  Result := nil;
end;

{ TYamlBoolean }

constructor TYamlBoolean.Create;
begin
  inherited Create;
  fIsNull := True;
end;

constructor TYamlBoolean.Create(const aValue: Boolean);
begin
  inherited Create;
  fIsNull := False;
  fValue := aValue;
end;

function TYamlBoolean.IsNull: Boolean;
begin
  Result := fIsNull;
end;

function TYamlBoolean.IsScalar: Boolean;
begin
  Result := True;
end;

function TYamlBoolean.AsString: string;
begin
  Result := fValue.ToString(True);
end;

function TYamlBoolean.Value: TFlexValue;
begin
  Result := fValue;
end;

{ TYamlComment }

function TYamlComment.AsString: string;
begin
  Result := fValue;
end;

constructor TYamlComment.Create;
begin
  inherited Create;
  fIsNull := True;
end;

constructor TYamlComment.Create(const aComment: string);
begin
  inherited Create;
  fIsNull := False;
  fValue := aComment;
end;

function TYamlComment.IsNull: Boolean;
begin
  Result := fIsNull;
end;

function TYamlComment.IsScalar: Boolean;
begin
  Result := True;
end;

function TYamlComment.Value: TFlexValue;
begin

end;

end.
