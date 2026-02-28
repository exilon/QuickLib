{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.YAML.Serializer.Tests
  Description : YAML Serializer unit tests
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 27/02/2026
  Modified    : 28/02/2026
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

unit Quick.YAML.Serializer.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  Quick.YAML,
  Quick.YAML.Serializer;

// ──────────────────────────────────────────────────────────────────
//  Test model classes  (published properties required for RTTI)
// ──────────────────────────────────────────────────────────────────

type
  TUserRole = (urGuest, urUser, urAdmin);

  TAddressModel = class
  private
    fStreet  : string;
    fCity    : string;
    fZipCode : string;
    fCountry : string;
  published
    property Street  : string  read fStreet  write fStreet;
    property City    : string  read fCity    write fCity;
    property ZipCode : string  read fZipCode write fZipCode;
    property Country : string  read fCountry write fCountry;
  end;

  TPersonModel = class
  private
    fName    : string;
    fAge     : Integer;
    fSalary  : Double;
    fActive  : Boolean;
  published
    property Name    : string  read fName   write fName;
    property Age     : Integer read fAge    write fAge;
    property Salary  : Double  read fSalary write fSalary;
    property Active  : Boolean read fActive write fActive;
  end;

  TPersonWithAddress = class
  private
    fName    : string;
    fAge     : Integer;
    fAddress : TAddressModel;
  public
    constructor Create;
    destructor  Destroy; override;
  published
    property Name    : string        read fName    write fName;
    property Age     : Integer       read fAge     write fAge;
    property Address : TAddressModel read fAddress write fAddress;
  end;

  TUserModel = class
  private
    fUsername : string;
    fRole     : TUserRole;
    fActive   : Boolean;
  published
    property Username : string    read fUsername write fUsername;
    property Role     : TUserRole read fRole     write fRole;
    property Active   : Boolean   read fActive   write fActive;
  end;

  TTaggedPerson = class
  private
    fName  : string;
    fScore : Integer;
    fTags  : TArray<string>;
  published
    property Name  : string         read fName  write fName;
    property Score : Integer        read fScore write fScore;
    property Tags  : TArray<string> read fTags  write fTags;
  end;

  TConfigDatabase = class
  private
    fHost           : string;
    fPort           : Integer;
    fName           : string;
    fMaxConnections : Integer;
  published
    property Host           : string  read fHost           write fHost;
    property Port           : Integer read fPort           write fPort;
    property Name           : string  read fName           write fName;
    property MaxConnections : Integer read fMaxConnections write fMaxConnections;
  end;

  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TConfigLogging = class
  private
    fLevel    : TLogLevel;
    fFilePath : string;
  published
    property Level    : TLogLevel read fLevel    write fLevel;
    property FilePath : string    read fFilePath write fFilePath;
  end;

  TAppConfig = class
  private
    fAppName  : string;
    fVersion  : Integer;
    fDebug    : Boolean;
    fDatabase : TConfigDatabase;
    fLogging  : TConfigLogging;
  public
    constructor Create;
    destructor  Destroy; override;
  published
    property AppName  : string          read fAppName  write fAppName;
    property Version  : Integer         read fVersion  write fVersion;
    property Debug    : Boolean         read fDebug    write fDebug;
    property Database : TConfigDatabase read fDatabase write fDatabase;
    property Logging  : TConfigLogging  read fLogging  write fLogging;
  end;

  TLocationModel = class
  private
    fCity       : string;
    fCountry    : string;
    fPostalCode : string;
  published
    property City       : string read fCity       write fCity;
    property Country    : string read fCountry    write fCountry;
    property PostalCode : string read fPostalCode write fPostalCode;
  end;

  TCompanyModel = class
  private
    fName : string;
    fHQ   : TLocationModel;
  public
    constructor Create;
    destructor  Destroy; override;
  published
    property Name : string         read fName write fName;
    property HQ   : TLocationModel read fHQ   write fHQ;
  end;

  TDeepModel = class
  private
    fId      : Integer;
    fCompany : TCompanyModel;
  public
    constructor Create;
    destructor  Destroy; override;
  published
    property Id      : Integer       read fId      write fId;
    property Company : TCompanyModel read fCompany write fCompany;
  end;

  {$IFNDEF FPC}
  // Attributes only supported on Delphi
  TAnnotatedModel = class
  private
    fPublicName  : string;
    fInternalKey : string;
    fSecret      : string;
  published
    [TCustomNameProperty('display_name')]
    property PublicName  : string read fPublicName  write fPublicName;
    [TCustomNameProperty('key')]
    property InternalKey : string read fInternalKey write fInternalKey;
    [TNotSerializableProperty]
    property Secret      : string read fSecret      write fSecret;
  end;
  {$ENDIF}

  // ── Complex multi-level model ────────────────────────────────────

  TContactInfo = class
  private
    fEmail  : string;
    fPhone  : string;
    fMobile : string;
  published
    property Email  : string read fEmail  write fEmail;
    property Phone  : string read fPhone  write fPhone;
    property Mobile : string read fMobile write fMobile;
  end;

  TGeoPoint = class
  private
    fLatitude  : Double;
    fLongitude : Double;
    fAltitude  : Double;
  published
    property Latitude  : Double read fLatitude  write fLatitude;
    property Longitude : Double read fLongitude write fLongitude;
    property Altitude  : Double read fAltitude  write fAltitude;
  end;

  TOfficeAddress = class
  private
    fStreet  : string;
    fCity    : string;
    fCountry : string;
    fGeo     : TGeoPoint;
  public
    constructor Create;
    destructor  Destroy; override;
  published
    property Street  : string    read fStreet  write fStreet;
    property City    : string    read fCity    write fCity;
    property Country : string    read fCountry write fCountry;
    property Geo     : TGeoPoint read fGeo     write fGeo;
  end;

  TDepartmentKind = (dkEngineering, dkMarketing, dkHR, dkFinance);

  TDepartment = class
  private
    fName     : string;
    fKind     : TDepartmentKind;
    fBudget   : Double;
    fActive   : Boolean;
    fHeadCount: Integer;
  published
    property Name      : string         read fName      write fName;
    property Kind      : TDepartmentKind read fKind     write fKind;
    property Budget    : Double         read fBudget    write fBudget;
    property Active    : Boolean        read fActive    write fActive;
    property HeadCount : Integer        read fHeadCount write fHeadCount;
  end;

  TEmployeeStatus = (esActive, esOnLeave, esTerminated);

  TEmployee = class
  private
    fId         : Integer;
    fFullName   : string;
    fStatus     : TEmployeeStatus;
    fSalary     : Double;
    fContact    : TContactInfo;
    fOffice     : TOfficeAddress;
    fSkills     : TArray<string>;
    fYearsExp   : Integer;
  public
    constructor Create;
    destructor  Destroy; override;
  published
    property Id        : Integer        read fId       write fId;
    property FullName  : string         read fFullName  write fFullName;
    property Status    : TEmployeeStatus read fStatus   write fStatus;
    property Salary    : Double         read fSalary    write fSalary;
    property Contact   : TContactInfo   read fContact   write fContact;
    property Office    : TOfficeAddress read fOffice    write fOffice;
    property Skills    : TArray<string> read fSkills    write fSkills;
    property YearsExp  : Integer        read fYearsExp  write fYearsExp;
  end;

  TOrganisation = class
  private
    fName        : string;
    fFoundedYear : Integer;
    fIsPublic    : Boolean;
    fRevenue     : Double;
    fHeadOffice  : TOfficeAddress;
    fDepartment  : TDepartment;
    fTags        : TArray<string>;
  public
    constructor Create;
    destructor  Destroy; override;
  published
    property Name        : string        read fName        write fName;
    property FoundedYear : Integer       read fFoundedYear write fFoundedYear;
    property IsPublic    : Boolean       read fIsPublic    write fIsPublic;
    property Revenue     : Double        read fRevenue     write fRevenue;
    property HeadOffice  : TOfficeAddress read fHeadOffice write fHeadOffice;
    property Department  : TDepartment   read fDepartment  write fDepartment;
    property Tags        : TArray<string> read fTags       write fTags;
  end;

  // Model for issue #147 – null YAML pair must not construct an empty object
  TNullableChild = class
  private
    fText : string;
    fId   : Integer;
  published
    property Text : string  read fText write fText;
    property Id   : Integer read fId   write fId;
  end;

  TNullableOwner = class
  private
    fId     : Integer;
    fChild  : TNullableChild;
    fChild2 : TNullableChild;
  public
    destructor Destroy; override;
  published
    property Id     : Integer        read fId     write fId;
    property Child  : TNullableChild read fChild  write fChild;
    property Child2 : TNullableChild read fChild2 write fChild2;
  end;

// ──────────────────────────────────────────────────────────────────

  [TestFixture]
  TYamlSerializerTests = class
  private
    fSerializer : TYamlSerializer;
    function BuildOrganisation: TOrganisation;
    function BuildEmployee: TEmployee;

  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    // ── Serialization (ObjectToYaml) ─────────────────────────────
    [Test] procedure Test_Serialize_SimpleClass_ContainsAllFields;
    [Test] procedure Test_Serialize_Integer_CorrectValue;
    [Test] procedure Test_Serialize_Float_CorrectValue;
    [Test] procedure Test_Serialize_Boolean_True;
    [Test] procedure Test_Serialize_Boolean_False;
    [Test] procedure Test_Serialize_NestedObject_ContainsNestedKey;
    [Test] procedure Test_Serialize_Enum_ByName;
    [Test] procedure Test_Serialize_StringArray_ContainsItems;
    [Test] procedure Test_Serialize_DeepNested_ContainsLeafValue;
    [Test] procedure Test_Serialize_NilObject_ReturnsEmptyOrNull;
    {$IFNDEF FPC}
    [Test] procedure Test_Serialize_CustomNameAttribute;
    [Test] procedure Test_Serialize_NotSerializableAttribute_Excluded;
    {$ENDIF}

    // ── Deserialization (YamlToObject) ───────────────────────────
    [Test] procedure Test_Deserialize_SimpleClass_FromFixture;
    [Test] procedure Test_Deserialize_StringField;
    [Test] procedure Test_Deserialize_IntegerField;
    [Test] procedure Test_Deserialize_FloatField;
    [Test] procedure Test_Deserialize_BooleanTrue;
    [Test] procedure Test_Deserialize_BooleanFalse;
    [Test] procedure Test_Deserialize_NestedObject;
    [Test] procedure Test_Deserialize_DeepNestedObject;
    [Test] procedure Test_Deserialize_Enum;
    [Test] procedure Test_Deserialize_StringArray;
    [Test] procedure Test_Deserialize_ZeroAndFalse;
    [Test] procedure Test_Deserialize_IntoExistingObject;
    [Test] procedure Test_Deserialize_CaseInsensitiveKeys;

    // ── Round-trip ────────────────────────────────────────────────
    [Test] procedure Test_RoundTrip_SimpleClass;
    [Test] procedure Test_RoundTrip_NestedObject;
    [Test] procedure Test_RoundTrip_Enum;
    [Test] procedure Test_RoundTrip_StringArray;
    [Test] procedure Test_RoundTrip_DeepNested;
    [Test] procedure Test_RoundTrip_AppConfig;

    // ── slPublishedProperty level ─────────────────────────────────
    [Test] procedure Test_SerializeLevel_Published_HidesPublicOnly;

    // ── Error handling ────────────────────────────────────────────
    [Test] procedure Test_Deserialize_EmptyYaml_ReturnsDefault;
    [Test] procedure Test_Deserialize_UnknownKeys_Ignored;

    // ── Complex multi-level scenarios ─────────────────────────────
    [Test] procedure Test_Complex_Serialize_Organisation;
    [Test] procedure Test_Complex_Deserialize_Organisation;
    [Test] procedure Test_Complex_RoundTrip_Organisation;
    [Test] procedure Test_Complex_Serialize_Employee;
    [Test] procedure Test_Complex_Deserialize_Employee;
    [Test] procedure Test_Complex_RoundTrip_Employee;
    [Test] procedure Test_Complex_GeoPoint_FloatPrecision;
    [Test] procedure Test_Complex_MultipleStringArrays;

    // ── Issue #147: null YAML pair must leave property nil ────────
    [Test] procedure Test_Deserialize_NullPair_ObjectPropertyRemainsNil;
    [Test] procedure Test_Deserialize_NullPair_ListPropertyRemainsNil;
    [Test] procedure Test_Deserialize_NullPair_AlreadyInitializedPropertyBecomesNil;
    [Test] procedure Test_Deserialize_NullPair_EmptyScalarTreatedAsNull;
    [Test] procedure Test_Deserialize_NullPair_MixedNullAndNonNull;
    [Test] procedure Test_Deserialize_NullPair_DeepNestedNull;
  end;

implementation

// ──────────────────────────────────────────────────────────────────
//  Model constructors / destructors
// ──────────────────────────────────────────────────────────────────

constructor TPersonWithAddress.Create;
begin
  inherited;
  fAddress := TAddressModel.Create;
end;

destructor TPersonWithAddress.Destroy;
begin
  fAddress.Free;
  inherited;
end;

constructor TCompanyModel.Create;
begin
  inherited;
  fHQ := TLocationModel.Create;
end;

destructor TCompanyModel.Destroy;
begin
  fHQ.Free;
  inherited;
end;

constructor TDeepModel.Create;
begin
  inherited;
  fCompany := TCompanyModel.Create;
end;

destructor TDeepModel.Destroy;
begin
  fCompany.Free;
  inherited;
end;

destructor TNullableOwner.Destroy;
begin
  fChild.Free;
  fChild2.Free;
  inherited;
end;

constructor TAppConfig.Create;
begin
  inherited;
  fDatabase := TConfigDatabase.Create;
  fLogging  := TConfigLogging.Create;
end;

destructor TAppConfig.Destroy;
begin
  fDatabase.Free;
  fLogging.Free;
  inherited;
end;

constructor TOfficeAddress.Create;
begin
  inherited;
  fGeo := TGeoPoint.Create;
end;

destructor TOfficeAddress.Destroy;
begin
  fGeo.Free;
  inherited;
end;

constructor TEmployee.Create;
begin
  inherited;
  fContact := TContactInfo.Create;
  fOffice  := TOfficeAddress.Create;
end;

destructor TEmployee.Destroy;
begin
  fContact.Free;
  fOffice.Free;
  inherited;
end;

constructor TOrganisation.Create;
begin
  inherited;
  fHeadOffice := TOfficeAddress.Create;
  fDepartment := TDepartment.Create;
end;

destructor TOrganisation.Destroy;
begin
  fHeadOffice.Free;
  fDepartment.Free;
  inherited;
end;

// ──────────────────────────────────────────────────────────────────
//  Fixture helper
// ──────────────────────────────────────────────────────────────────



// ──────────────────────────────────────────────────────────────────
//  Setup / TearDown
// ──────────────────────────────────────────────────────────────────

procedure TYamlSerializerTests.Setup;
begin
  fSerializer := TYamlSerializer.Create(TSerializeLevel.slPublishedProperty);
end;

procedure TYamlSerializerTests.TearDown;
begin
  fSerializer.Free;
end;

// ══════════════════════════════════════════════════════════════════
//  SERIALIZATION TESTS
// ══════════════════════════════════════════════════════════════════

procedure TYamlSerializerTests.Test_Serialize_SimpleClass_ContainsAllFields;
var
  obj  : TPersonModel;
  yaml : string;
begin
  obj := TPersonModel.Create;
  try
    obj.Name   := 'John Doe';
    obj.Age    := 35;
    obj.Salary := 75000.50;
    obj.Active := True;
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.Contains(yaml, 'Name',    'YAML must contain Name key');
    Assert.Contains(yaml, 'John Doe','YAML must contain Name value');
    Assert.Contains(yaml, 'Age',     'YAML must contain Age key');
    Assert.Contains(yaml, '35',      'YAML must contain Age value');
    Assert.Contains(yaml, 'Salary',  'YAML must contain Salary key');
    Assert.Contains(yaml, 'Active',  'YAML must contain Active key');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Serialize_Integer_CorrectValue;
var
  obj  : TPersonModel;
  yaml : string;
begin
  obj := TPersonModel.Create;
  try
    obj.Name := 'Test';
    obj.Age  := 42;
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.Contains(yaml, '42', 'Serialized integer value must appear in YAML');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Serialize_Float_CorrectValue;
var
  obj  : TPersonModel;
  yaml : string;
begin
  obj := TPersonModel.Create;
  try
    obj.Name   := 'Test';
    obj.Salary := 1234.56;
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.Contains(yaml, 'Salary', 'YAML must contain Salary key');
    Assert.IsTrue(yaml.Length > 0, 'YAML output should not be empty');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Serialize_Boolean_True;
var
  obj  : TPersonModel;
  yaml : string;
begin
  obj := TPersonModel.Create;
  try
    obj.Active := True;
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.Contains(yaml, 'true', 'Boolean true should be serialized as "true"');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Serialize_Boolean_False;
var
  obj  : TPersonModel;
  yaml : string;
begin
  obj := TPersonModel.Create;
  try
    obj.Active := False;
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.Contains(yaml, 'false', 'Boolean false should be serialized as "false"');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Serialize_NestedObject_ContainsNestedKey;
var
  obj  : TPersonWithAddress;
  yaml : string;
begin
  obj := TPersonWithAddress.Create;
  try
    obj.Name              := 'Alice';
    obj.Age               := 28;
    obj.Address.Street    := 'Gran Via';
    obj.Address.City      := 'Madrid';
    obj.Address.ZipCode   := '28001';
    obj.Address.Country   := 'Spain';
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.Contains(yaml, 'Address', 'YAML must contain Address key');
    Assert.Contains(yaml, 'Gran Via','YAML must contain Street value');
    Assert.Contains(yaml, 'Madrid',  'YAML must contain City value');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Serialize_Enum_ByName;
var
  obj  : TUserModel;
  yaml : string;
begin
  obj := TUserModel.Create;
  try
    obj.Username := 'admin';
    obj.Role     := urAdmin;
    obj.Active   := True;
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.Contains(yaml, 'Role',    'YAML must contain Role key');
    Assert.Contains(yaml, 'urAdmin', 'Enum should be serialized by name');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Serialize_StringArray_ContainsItems;
var
  obj  : TTaggedPerson;
  yaml : string;
begin
  obj := TTaggedPerson.Create;
  try
    obj.Name  := 'Charlie';
    obj.Score := 99;
    obj.Tags  := ['delphi', 'programming', 'quicklib'];
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.Contains(yaml, 'Tags',        'YAML must contain Tags key');
    Assert.Contains(yaml, 'delphi',      'YAML must contain first tag');
    Assert.Contains(yaml, 'programming', 'YAML must contain second tag');
    Assert.Contains(yaml, 'quicklib',    'YAML must contain third tag');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Serialize_DeepNested_ContainsLeafValue;
var
  obj  : TDeepModel;
  yaml : string;
begin
  obj := TDeepModel.Create;
  try
    obj.Id               := 1;
    obj.Company.Name     := 'Acme Corp';
    obj.Company.HQ.City  := 'New York';
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.Contains(yaml, 'Company',  'YAML must contain Company');
    Assert.Contains(yaml, 'Acme Corp','YAML must contain Company name');
    Assert.Contains(yaml, 'HQ',       'YAML must contain nested HQ');
    Assert.Contains(yaml, 'New York', 'YAML must contain leaf city value');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Serialize_NilObject_ReturnsEmptyOrNull;
var
  yaml : string;
begin
  Assert.WillNotRaise(
    procedure begin yaml := fSerializer.ObjectToYaml(nil); end,
    nil, 'Serializing nil should not raise');
  // result is either empty string or 'null'
  Assert.IsTrue((yaml = '') or (yaml.Trim = 'null') or (yaml.Trim = ''),
    'Nil object should produce empty or null YAML');
end;

{$IFNDEF FPC}
procedure TYamlSerializerTests.Test_Serialize_CustomNameAttribute;
var
  obj  : TAnnotatedModel;
  yaml : string;
begin
  obj := TAnnotatedModel.Create;
  try
    obj.PublicName  := 'visible';
    obj.InternalKey := 'mykey';
    obj.Secret      := 'topsecret';
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.Contains(yaml, 'display_name', 'Custom name attribute should rename the property');
    Assert.Contains(yaml, 'key',          'Custom name attribute should rename the property');
    Assert.Contains(yaml, 'visible',      'Renamed property value must appear');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Serialize_NotSerializableAttribute_Excluded;
var
  obj  : TAnnotatedModel;
  yaml : string;
begin
  obj := TAnnotatedModel.Create;
  try
    obj.PublicName := 'shown';
    obj.Secret     := 'hidden';
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.IsFalse(yaml.Contains('hidden'),
      'NotSerializableProperty value must not appear in YAML');
    Assert.IsFalse(yaml.Contains('Secret') and yaml.Contains('hidden'),
      'Secret field should be excluded');
  finally
    obj.Free;
  end;
end;
{$ENDIF}

// ══════════════════════════════════════════════════════════════════
//  DESERIALIZATION TESTS
// ══════════════════════════════════════════════════════════════════

procedure TYamlSerializerTests.Test_Deserialize_SimpleClass_FromFixture;
const
  FIXTURE =
    'Name: John Doe'#13#10 +
    'Age: 35'#13#10 +
    'Salary: 75000.50'#13#10 +
    'Active: true'#13#10;
var
  yaml : string;
  obj  : TPersonModel;
begin
  yaml := FIXTURE;
  obj  := fSerializer.YamlToObject(TPersonModel, yaml) as TPersonModel;
  try
    Assert.AreEqual('John Doe', obj.Name,   'Name must match fixture');
    Assert.AreEqual(35,         obj.Age,    'Age must match fixture');
    Assert.IsTrue(obj.Active,               'Active must be true from fixture');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_StringField;
var
  obj  : TPersonModel;
const
  YAML = 'Name: Hello World'#13#10'Age: 1'#13#10;
begin
  obj := fSerializer.YamlToObject(TPersonModel, YAML) as TPersonModel;
  try
    Assert.AreEqual('Hello World', obj.Name, 'String field must be deserialized correctly');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_IntegerField;
var
  obj  : TPersonModel;
const
  YAML = 'Name: X'#13#10'Age: 123'#13#10;
begin
  obj := fSerializer.YamlToObject(TPersonModel, YAML) as TPersonModel;
  try
    Assert.AreEqual(123, obj.Age, 'Integer field must be deserialized correctly');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_FloatField;
var
  obj  : TPersonModel;
const
  YAML = 'Name: X'#13#10'Age: 1'#13#10'Salary: 9999.99'#13#10;
begin
  obj := fSerializer.YamlToObject(TPersonModel, YAML) as TPersonModel;
  try
    Assert.AreEqual(Double(9999.99), Double(obj.Salary), 0.001,
      'Float field must be deserialized correctly');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_BooleanTrue;
var
  obj  : TPersonModel;
const
  YAML = 'Name: X'#13#10'Age: 1'#13#10'Active: true'#13#10;
begin
  obj := fSerializer.YamlToObject(TPersonModel, YAML) as TPersonModel;
  try
    Assert.IsTrue(obj.Active, 'Boolean true must be deserialized correctly');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_BooleanFalse;
var
  obj  : TPersonModel;
const
  YAML = 'Name: X'#13#10'Age: 1'#13#10'Active: false'#13#10;
begin
  obj := fSerializer.YamlToObject(TPersonModel, YAML) as TPersonModel;
  try
    Assert.IsFalse(obj.Active, 'Boolean false must be deserialized correctly');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_NestedObject;
const
  FIXTURE =
    'Name: Alice Smith'#13#10 +
    'Age: 28'#13#10 +
    'Address:'#13#10 +
    '  Street: Gran Via'#13#10 +
    '  City: Madrid'#13#10 +
    '  ZipCode: 28001'#13#10 +
    '  Country: Spain'#13#10;
var
  yaml : string;
  obj  : TPersonWithAddress;
begin
  yaml := FIXTURE;
  obj  := fSerializer.YamlToObject(TPersonWithAddress, yaml) as TPersonWithAddress;
  try
    Assert.AreEqual('Alice Smith', obj.Name,           'Name must match');
    Assert.AreEqual(28,            obj.Age,            'Age must match');
    Assert.IsNotNull(obj.Address,                      'Address should not be nil');
    Assert.AreEqual('Gran Via',    obj.Address.Street, 'Nested Street must match');
    Assert.AreEqual('Madrid',      obj.Address.City,   'Nested City must match');
    Assert.AreEqual('28001',       obj.Address.ZipCode,'Nested ZipCode must match');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_DeepNestedObject;
const
  FIXTURE =
    'Id: 1'#13#10 +
    'Company:'#13#10 +
    '  Name: Acme Corp'#13#10 +
    '  HQ:'#13#10 +
    '    City: New York'#13#10 +
    '    Country: USA'#13#10 +
    '    PostalCode: 10001'#13#10;
var
  yaml : string;
  obj  : TDeepModel;
begin
  yaml := FIXTURE;
  obj  := fSerializer.YamlToObject(TDeepModel, yaml) as TDeepModel;
  try
    Assert.AreEqual(1,          obj.Id,               'Id must match');
    Assert.IsNotNull(obj.Company,                     'Company must not be nil');
    Assert.AreEqual('Acme Corp',obj.Company.Name,     'Company name must match');
    Assert.IsNotNull(obj.Company.HQ,                  'HQ must not be nil');
    Assert.AreEqual('New York', obj.Company.HQ.City,  'City must match');
    Assert.AreEqual('USA',      obj.Company.HQ.Country,'Country must match');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_Enum;
const
  FIXTURE =
    'Username: admin'#13#10 +
    'Role: urAdmin'#13#10 +
    'Active: true'#13#10;
var
  yaml : string;
  obj  : TUserModel;
begin
  yaml := FIXTURE;
  obj  := fSerializer.YamlToObject(TUserModel, yaml) as TUserModel;
  try
    Assert.AreEqual('admin', obj.Username, 'Username must match');
    Assert.AreEqual(Ord(urAdmin), Ord(obj.Role), 'Enum must deserialize to urAdmin');
    Assert.IsTrue(obj.Active, 'Active must be true');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_StringArray;
const
  FIXTURE =
    'Name: Charlie'#13#10 +
    'Tags:'#13#10 +
    '  - delphi'#13#10 +
    '  - programming'#13#10 +
    '  - quicklib'#13#10 +
    'Score: 99'#13#10;
var
  yaml : string;
  obj  : TTaggedPerson;
begin
  yaml := FIXTURE;
  obj  := fSerializer.YamlToObject(TTaggedPerson, yaml) as TTaggedPerson;
  try
    Assert.AreEqual('Charlie', obj.Name,  'Name must match');
    Assert.AreEqual(99,        obj.Score, 'Score must match');
    Assert.AreEqual(3, Integer(Length(obj.Tags)),  'Tags array should have 3 items');
    Assert.AreEqual('delphi',       obj.Tags[0], 'First tag must match');
    Assert.AreEqual('programming',  obj.Tags[1], 'Second tag must match');
    Assert.AreEqual('quicklib',     obj.Tags[2], 'Third tag must match');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_ZeroAndFalse;
const
  FIXTURE =
    'Name: Ghost'#13#10 +
    'Age: 0'#13#10 +
    'Salary: 0'#13#10 +
    'Active: false'#13#10;
var
  yaml : string;
  obj  : TPersonModel;
begin
  yaml := FIXTURE;
  obj  := fSerializer.YamlToObject(TPersonModel, yaml) as TPersonModel;
  try
    Assert.AreEqual('Ghost', obj.Name,   'Name must match');
    Assert.AreEqual(0,       obj.Age,    'Age 0 must deserialize correctly');
    Assert.AreEqual(Double(0), Double(obj.Salary), 0.001, 'Salary 0 must deserialize correctly');
    Assert.IsFalse(obj.Active,           'Active false must deserialize correctly');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_IntoExistingObject;
var
  obj  : TPersonModel;
const
  YAML = 'Name: Updated'#13#10'Age: 50'#13#10;
begin
  obj := TPersonModel.Create;
  try
    obj.Name := 'Original';
    obj.Age  := 10;
    fSerializer.YamlToObject(obj, YAML);
    Assert.AreEqual('Updated', obj.Name, 'Existing object Name must be updated');
    Assert.AreEqual(50,        obj.Age,  'Existing object Age must be updated');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_CaseInsensitiveKeys;
var
  obj  : TPersonModel;
const
  // YAML keys in different case
  YAML = 'name: CaseSensitivity Test'#13#10'age: 77'#13#10;
begin
  obj := fSerializer.YamlToObject(TPersonModel, YAML) as TPersonModel;
  try
    Assert.AreEqual('CaseSensitivity Test', obj.Name,
      'Keys should be matched case-insensitively');
    Assert.AreEqual(77, obj.Age, 'Age must match with lowercase key');
  finally
    obj.Free;
  end;
end;

// ══════════════════════════════════════════════════════════════════
//  ROUND-TRIP TESTS
// ══════════════════════════════════════════════════════════════════

procedure TYamlSerializerTests.Test_RoundTrip_SimpleClass;
var
  src  : TPersonModel;
  yaml : string;
  dst  : TPersonModel;
begin
  src := TPersonModel.Create;
  try
    src.Name   := 'RoundTrip Person';
    src.Age    := 42;
    src.Salary := 55000.75;
    src.Active := True;
    yaml := fSerializer.ObjectToYaml(src);
    dst  := fSerializer.YamlToObject(TPersonModel, yaml) as TPersonModel;
    try
      Assert.AreEqual(src.Name,   dst.Name,   'Round-trip Name must match');
      Assert.AreEqual(src.Age,    dst.Age,    'Round-trip Age must match');
      Assert.AreEqual(Double(src.Salary), Double(dst.Salary), 0.001,
        'Round-trip Salary must match');
      Assert.AreEqual(src.Active, dst.Active, 'Round-trip Active must match');
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TYamlSerializerTests.Test_RoundTrip_NestedObject;
var
  src  : TPersonWithAddress;
  yaml : string;
  dst  : TPersonWithAddress;
begin
  src := TPersonWithAddress.Create;
  try
    src.Name            := 'Nested Person';
    src.Age             := 30;
    src.Address.Street  := 'Calle Mayor';
    src.Address.City    := 'Barcelona';
    src.Address.ZipCode := '08001';
    src.Address.Country := 'Spain';
    yaml := fSerializer.ObjectToYaml(src);
    dst  := fSerializer.YamlToObject(TPersonWithAddress, yaml) as TPersonWithAddress;
    try
      Assert.AreEqual(src.Name,             dst.Name,             'Name');
      Assert.AreEqual(src.Address.Street,   dst.Address.Street,   'Street');
      Assert.AreEqual(src.Address.City,     dst.Address.City,     'City');
      Assert.AreEqual(src.Address.ZipCode,  dst.Address.ZipCode,  'ZipCode');
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TYamlSerializerTests.Test_RoundTrip_Enum;
var
  src  : TUserModel;
  yaml : string;
  dst  : TUserModel;
begin
  src := TUserModel.Create;
  try
    src.Username := 'testuser';
    src.Role     := urUser;
    src.Active   := False;
    yaml := fSerializer.ObjectToYaml(src);
    dst  := fSerializer.YamlToObject(TUserModel, yaml) as TUserModel;
    try
      Assert.AreEqual(src.Username,     dst.Username,     'Username');
      Assert.AreEqual(Ord(src.Role),    Ord(dst.Role),    'Role enum');
      Assert.AreEqual(src.Active,       dst.Active,       'Active');
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TYamlSerializerTests.Test_RoundTrip_StringArray;
var
  src  : TTaggedPerson;
  yaml : string;
  dst  : TTaggedPerson;
begin
  src := TTaggedPerson.Create;
  try
    src.Name  := 'ArrayPerson';
    src.Score := 100;
    src.Tags  := ['alpha', 'beta', 'gamma'];
    yaml := fSerializer.ObjectToYaml(src);
    dst  := fSerializer.YamlToObject(TTaggedPerson, yaml) as TTaggedPerson;
    try
      Assert.AreEqual(src.Name,  dst.Name,  'Name');
      Assert.AreEqual(src.Score, dst.Score, 'Score');
      Assert.AreEqual(3,         Integer(Length(dst.Tags)), 'Tag count');
      Assert.AreEqual('alpha',   dst.Tags[0], 'Tag[0]');
      Assert.AreEqual('beta',    dst.Tags[1], 'Tag[1]');
      Assert.AreEqual('gamma',   dst.Tags[2], 'Tag[2]');
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TYamlSerializerTests.Test_RoundTrip_DeepNested;
var
  src  : TDeepModel;
  yaml : string;
  dst  : TDeepModel;
begin
  src := TDeepModel.Create;
  try
    src.Id               := 7;
    src.Company.Name     := 'DeepCorp';
    src.Company.HQ.City  := 'Berlin';
    src.Company.HQ.Country := 'Germany';
    src.Company.HQ.PostalCode := '10115';
    yaml := fSerializer.ObjectToYaml(src);
    dst  := fSerializer.YamlToObject(TDeepModel, yaml) as TDeepModel;
    try
      Assert.AreEqual(src.Id,                    dst.Id,                    'Id');
      Assert.AreEqual(src.Company.Name,          dst.Company.Name,          'Company.Name');
      Assert.AreEqual(src.Company.HQ.City,       dst.Company.HQ.City,       'HQ.City');
      Assert.AreEqual(src.Company.HQ.Country,    dst.Company.HQ.Country,    'HQ.Country');
      Assert.AreEqual(src.Company.HQ.PostalCode, dst.Company.HQ.PostalCode, 'HQ.PostalCode');
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TYamlSerializerTests.Test_RoundTrip_AppConfig;
const
  FIXTURE =
    'AppName: MyApp'#13#10 +
    'Version: 2'#13#10 +
    'Debug: false'#13#10 +
    'Database:'#13#10 +
    '  Host: localhost'#13#10 +
    '  Port: 5432'#13#10 +
    '  Name: mydb'#13#10 +
    '  MaxConnections: 10'#13#10 +
    'Logging:'#13#10 +
    '  Level: llWarning'#13#10 +
    '  FilePath: logs/app.log'#13#10;
var
  yaml : string;
  src  : TAppConfig;
  dst  : TAppConfig;
begin
  yaml := FIXTURE;
  src  := fSerializer.YamlToObject(TAppConfig, yaml) as TAppConfig;
  try
    Assert.AreEqual('MyApp',      src.AppName,               'AppName from fixture');
    Assert.AreEqual(2,            src.Version,               'Version from fixture');
    Assert.IsFalse(src.Debug,                                'Debug must be false');
    Assert.AreEqual('localhost',  src.Database.Host,         'DB Host');
    Assert.AreEqual(5432,         src.Database.Port,         'DB Port');
    Assert.AreEqual('mydb',       src.Database.Name,         'DB Name');
    Assert.AreEqual(10,           src.Database.MaxConnections,'DB MaxConnections');
    Assert.AreEqual(Ord(llWarning), Ord(src.Logging.Level),  'Logging level');
    Assert.AreEqual('logs/app.log', src.Logging.FilePath,    'Log file path');

    // Round-trip: serialize back then re-parse
    yaml := fSerializer.ObjectToYaml(src);
    dst  := fSerializer.YamlToObject(TAppConfig, yaml) as TAppConfig;
    try
      Assert.AreEqual(src.AppName,          dst.AppName,          'RT AppName');
      Assert.AreEqual(src.Version,          dst.Version,          'RT Version');
      Assert.AreEqual(src.Database.Host,    dst.Database.Host,    'RT DB Host');
      Assert.AreEqual(src.Database.Port,    dst.Database.Port,    'RT DB Port');
      Assert.AreEqual(Ord(src.Logging.Level), Ord(dst.Logging.Level), 'RT LogLevel');
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

// ══════════════════════════════════════════════════════════════════
//  SERIALIZE LEVEL TESTS
// ══════════════════════════════════════════════════════════════════

procedure TYamlSerializerTests.Test_SerializeLevel_Published_HidesPublicOnly;
var
  obj  : TPersonModel;
  yaml : string;
begin
  // TPersonModel only has published properties, so all should appear
  obj := TPersonModel.Create;
  try
    obj.Name   := 'PublishedOnly';
    obj.Age    := 10;
    yaml := fSerializer.ObjectToYaml(obj);
    Assert.Contains(yaml, 'Name',   'Published Name must appear');
    Assert.Contains(yaml, 'Age',    'Published Age must appear');
    Assert.Contains(yaml, 'Salary', 'Published Salary must appear');
    Assert.Contains(yaml, 'Active', 'Published Active must appear');
  finally
    obj.Free;
  end;
end;

// ══════════════════════════════════════════════════════════════════
//  ERROR-HANDLING TESTS
// ══════════════════════════════════════════════════════════════════

procedure TYamlSerializerTests.Test_Deserialize_EmptyYaml_ReturnsDefault;
var
  obj : TPersonModel;
begin
  // Empty YAML should not raise; returns an object with default field values
  Assert.WillNotRaise(
    procedure
    begin
      obj := fSerializer.YamlToObject(TPersonModel, '') as TPersonModel;
      if obj <> nil then obj.Free;
    end,
    nil, 'Deserializing empty YAML must not raise');
end;

procedure TYamlSerializerTests.Test_Deserialize_UnknownKeys_Ignored;
var
  obj  : TPersonModel;
const
  YAML =
    'Name: Known'#13#10 +
    'Age: 5'#13#10 +
    'UnknownField: whatever'#13#10 +
    'AnotherBogus: 999'#13#10;
begin
  Assert.WillNotRaise(
    procedure
    begin
      obj := fSerializer.YamlToObject(TPersonModel, YAML) as TPersonModel;
      try
        Assert.AreEqual('Known', obj.Name, 'Known field must still be read');
        Assert.AreEqual(5, obj.Age, 'Known integer field must still be read');
      finally
        obj.Free;
      end;
    end,
    nil, 'Unknown YAML keys must be silently ignored');
end;

// ══════════════════════════════════════════════════════════════════
//  COMPLEX MULTI-LEVEL TESTS
// ══════════════════════════════════════════════════════════════════

function TYamlSerializerTests.BuildOrganisation: TOrganisation;
begin
  Result := TOrganisation.Create;
  Result.Name                    := 'TechCorp International';
  Result.FoundedYear             := 1998;
  Result.IsPublic                := True;
  Result.Revenue                 := 4500000.75;
  Result.Tags                    := ['software', 'cloud', 'saas'];
  Result.HeadOffice.Street       := 'Innovation Blvd 42';
  Result.HeadOffice.City         := 'San Francisco';
  Result.HeadOffice.Country      := 'USA';
  Result.HeadOffice.Geo.Latitude  := 37.7749;
  Result.HeadOffice.Geo.Longitude := -122.4194;
  Result.HeadOffice.Geo.Altitude  := 16.0;
  Result.Department.Name         := 'Engineering';
  Result.Department.Kind         := dkEngineering;
  Result.Department.Budget       := 1200000.00;
  Result.Department.Active       := True;
  Result.Department.HeadCount    := 120;
end;

function TYamlSerializerTests.BuildEmployee: TEmployee;
begin
  Result := TEmployee.Create;
  Result.Id                    := 42;
  Result.FullName              := 'Jane Doe';
  Result.Status                := esActive;
  Result.Salary                := 95000.50;
  Result.YearsExp              := 8;
  Result.Skills                := ['Delphi', 'Python', 'Docker'];
  Result.Contact.Email         := 'jane.doe@techcorp.com';
  Result.Contact.Phone         := '+1-415-555-0100';
  Result.Contact.Mobile        := '+1-415-555-0199';
  Result.Office.Street         := 'Market St 1';
  Result.Office.City           := 'San Francisco';
  Result.Office.Country        := 'USA';
  Result.Office.Geo.Latitude   := 37.7751;
  Result.Office.Geo.Longitude  := -122.4180;
  Result.Office.Geo.Altitude   := 12.5;
end;

procedure TYamlSerializerTests.Test_Complex_Serialize_Organisation;
var
  org  : TOrganisation;
  yaml : string;
begin
  org := BuildOrganisation;
  try
    yaml := fSerializer.ObjectToYaml(org);
    // Top-level scalars
    Assert.Contains(yaml, 'TechCorp International', 'Name');
    Assert.Contains(yaml, '1998',                   'FoundedYear');
    Assert.Contains(yaml, 'true',                   'IsPublic');
    // Nested HeadOffice
    Assert.Contains(yaml, 'HeadOffice',             'HeadOffice key');
    Assert.Contains(yaml, 'San Francisco',          'HeadOffice.City');
    Assert.Contains(yaml, 'Innovation Blvd 42',     'HeadOffice.Street');
    // Deep nested Geo
    Assert.Contains(yaml, 'Geo',                    'Geo key');
    Assert.Contains(yaml, 'Latitude',               'Geo.Latitude key');
    Assert.Contains(yaml, 'Longitude',              'Geo.Longitude key');
    // Nested Department
    Assert.Contains(yaml, 'Department',             'Department key');
    Assert.Contains(yaml, 'Engineering',            'Department.Name');
    Assert.Contains(yaml, 'dkEngineering',          'Department.Kind enum');
    Assert.Contains(yaml, '120',                    'Department.HeadCount');
    // Array of strings
    Assert.Contains(yaml, 'Tags',                   'Tags key');
    Assert.Contains(yaml, 'software',               'Tags[0]');
    Assert.Contains(yaml, 'cloud',                  'Tags[1]');
    Assert.Contains(yaml, 'saas',                   'Tags[2]');
  finally
    org.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Complex_Deserialize_Organisation;
const
  YAML =
    'Name: TechCorp International'#13#10 +
    'FoundedYear: 1998'#13#10 +
    'IsPublic: true'#13#10 +
    'Revenue: 4500000.75'#13#10 +
    'Tags:'#13#10 +
    '  - software'#13#10 +
    '  - cloud'#13#10 +
    '  - saas'#13#10 +
    'HeadOffice:'#13#10 +
    '  Street: Innovation Blvd 42'#13#10 +
    '  City: San Francisco'#13#10 +
    '  Country: USA'#13#10 +
    '  Geo:'#13#10 +
    '    Latitude: 37.7749'#13#10 +
    '    Longitude: -122.4194'#13#10 +
    '    Altitude: 16.0'#13#10 +
    'Department:'#13#10 +
    '  Name: Engineering'#13#10 +
    '  Kind: dkEngineering'#13#10 +
    '  Budget: 1200000.0'#13#10 +
    '  Active: true'#13#10 +
    '  HeadCount: 120'#13#10;
var
  org : TOrganisation;
begin
  org := fSerializer.YamlToObject(TOrganisation, YAML) as TOrganisation;
  try
    Assert.AreEqual('TechCorp International', org.Name,             'Name');
    Assert.AreEqual(1998,                     org.FoundedYear,      'FoundedYear');
    Assert.IsTrue(org.IsPublic,                                      'IsPublic');
    Assert.IsNotNull(org.HeadOffice,                                 'HeadOffice not nil');
    Assert.AreEqual('San Francisco',          org.HeadOffice.City,  'HeadOffice.City');
    Assert.AreEqual('Innovation Blvd 42',     org.HeadOffice.Street,'HeadOffice.Street');
    Assert.AreEqual('USA',                    org.HeadOffice.Country,'HeadOffice.Country');
    Assert.IsNotNull(org.HeadOffice.Geo,                             'Geo not nil');
    Assert.AreEqual(Double(37.7749),   Double(org.HeadOffice.Geo.Latitude),  0.0001, 'Geo.Latitude');
    Assert.AreEqual(Double(-122.4194), Double(org.HeadOffice.Geo.Longitude), 0.0001, 'Geo.Longitude');
    Assert.IsNotNull(org.Department,                                 'Department not nil');
    Assert.AreEqual('Engineering',            org.Department.Name,  'Department.Name');
    Assert.AreEqual(Ord(dkEngineering),       Ord(org.Department.Kind), 'Department.Kind');
    Assert.AreEqual(120,                      org.Department.HeadCount, 'Department.HeadCount');
    Assert.IsTrue(org.Department.Active,                             'Department.Active');
    Assert.AreEqual(3, Integer(Length(org.Tags)),                    'Tags count');
    Assert.AreEqual('software', org.Tags[0],                         'Tags[0]');
    Assert.AreEqual('cloud',    org.Tags[1],                         'Tags[1]');
    Assert.AreEqual('saas',     org.Tags[2],                         'Tags[2]');
  finally
    org.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Complex_RoundTrip_Organisation;
var
  src  : TOrganisation;
  yaml : string;
  dst  : TOrganisation;
begin
  src := BuildOrganisation;
  try
    yaml := fSerializer.ObjectToYaml(src);
    dst  := fSerializer.YamlToObject(TOrganisation, yaml) as TOrganisation;
    try
      Assert.AreEqual(src.Name,                    dst.Name,                    'Name');
      Assert.AreEqual(src.FoundedYear,             dst.FoundedYear,             'FoundedYear');
      Assert.AreEqual(src.IsPublic,                dst.IsPublic,                'IsPublic');
      Assert.AreEqual(Double(src.Revenue),         Double(dst.Revenue), 0.01,   'Revenue');
      Assert.AreEqual(src.HeadOffice.City,         dst.HeadOffice.City,         'HeadOffice.City');
      Assert.AreEqual(src.HeadOffice.Street,       dst.HeadOffice.Street,       'HeadOffice.Street');
      Assert.AreEqual(Double(src.HeadOffice.Geo.Latitude),
                      Double(dst.HeadOffice.Geo.Latitude), 0.0001,              'Geo.Latitude');
      Assert.AreEqual(Double(src.HeadOffice.Geo.Longitude),
                      Double(dst.HeadOffice.Geo.Longitude), 0.0001,             'Geo.Longitude');
      Assert.AreEqual(src.Department.Name,         dst.Department.Name,         'Department.Name');
      Assert.AreEqual(Ord(src.Department.Kind),    Ord(dst.Department.Kind),    'Department.Kind');
      Assert.AreEqual(src.Department.HeadCount,    dst.Department.HeadCount,    'Department.HeadCount');
      Assert.AreEqual(Double(src.Department.Budget),
                      Double(dst.Department.Budget), 0.01,                      'Department.Budget');
      Assert.AreEqual(3, Integer(Length(dst.Tags)),                             'Tags count');
      Assert.AreEqual(src.Tags[0], dst.Tags[0],                                'Tags[0]');
      Assert.AreEqual(src.Tags[2], dst.Tags[2],                                'Tags[2]');
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Complex_Serialize_Employee;
var
  emp  : TEmployee;
  yaml : string;
begin
  emp := BuildEmployee;
  try
    yaml := fSerializer.ObjectToYaml(emp);
    Assert.Contains(yaml, 'Jane Doe',                  'FullName');
    Assert.Contains(yaml, '42',                        'Id');
    Assert.Contains(yaml, 'esActive',                  'Status enum');
    Assert.Contains(yaml, 'Contact',                   'Contact key');
    Assert.Contains(yaml, 'jane.doe@techcorp.com',     'Contact.Email');
    Assert.Contains(yaml, '+1-415-555-0100',           'Contact.Phone');
    Assert.Contains(yaml, 'Office',                    'Office key');
    Assert.Contains(yaml, 'Market St 1',               'Office.Street');
    Assert.Contains(yaml, 'Geo',                       'Geo key');
    Assert.Contains(yaml, 'Skills',                    'Skills key');
    Assert.Contains(yaml, 'Delphi',                    'Skills[0]');
    Assert.Contains(yaml, 'Python',                    'Skills[1]');
    Assert.Contains(yaml, 'Docker',                    'Skills[2]');
    Assert.Contains(yaml, '8',                         'YearsExp');
  finally
    emp.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Complex_Deserialize_Employee;
const
  YAML =
    'Id: 42'#13#10 +
    'FullName: Jane Doe'#13#10 +
    'Status: esActive'#13#10 +
    'Salary: 95000.50'#13#10 +
    'YearsExp: 8'#13#10 +
    'Skills:'#13#10 +
    '  - Delphi'#13#10 +
    '  - Python'#13#10 +
    '  - Docker'#13#10 +
    'Contact:'#13#10 +
    '  Email: jane.doe@techcorp.com'#13#10 +
    '  Phone: +1-415-555-0100'#13#10 +
    '  Mobile: +1-415-555-0199'#13#10 +
    'Office:'#13#10 +
    '  Street: Market St 1'#13#10 +
    '  City: San Francisco'#13#10 +
    '  Country: USA'#13#10 +
    '  Geo:'#13#10 +
    '    Latitude: 37.7751'#13#10 +
    '    Longitude: -122.4180'#13#10 +
    '    Altitude: 12.5'#13#10;
var
  emp : TEmployee;
begin
  emp := fSerializer.YamlToObject(TEmployee, YAML) as TEmployee;
  try
    Assert.AreEqual(42,        emp.Id,        'Id');
    Assert.AreEqual('Jane Doe',emp.FullName,  'FullName');
    Assert.AreEqual(Ord(esActive), Ord(emp.Status), 'Status');
    Assert.AreEqual(Double(95000.50), Double(emp.Salary), 0.01, 'Salary');
    Assert.AreEqual(8,         emp.YearsExp,  'YearsExp');
    // Skills array
    Assert.AreEqual(3, Integer(Length(emp.Skills)),    'Skills count');
    Assert.AreEqual('Delphi',  emp.Skills[0],          'Skills[0]');
    Assert.AreEqual('Python',  emp.Skills[1],          'Skills[1]');
    Assert.AreEqual('Docker',  emp.Skills[2],          'Skills[2]');
    // Nested Contact
    Assert.IsNotNull(emp.Contact,                       'Contact not nil');
    Assert.AreEqual('jane.doe@techcorp.com', emp.Contact.Email,  'Contact.Email');
    Assert.AreEqual('+1-415-555-0100',       emp.Contact.Phone,  'Contact.Phone');
    Assert.AreEqual('+1-415-555-0199',       emp.Contact.Mobile, 'Contact.Mobile');
    // Nested Office
    Assert.IsNotNull(emp.Office,                        'Office not nil');
    Assert.AreEqual('Market St 1',           emp.Office.Street,  'Office.Street');
    Assert.AreEqual('San Francisco',         emp.Office.City,    'Office.City');
    // Deep nested Geo
    Assert.IsNotNull(emp.Office.Geo,                    'Geo not nil');
    Assert.AreEqual(Double(37.7751),  Double(emp.Office.Geo.Latitude),  0.0001, 'Geo.Latitude');
    Assert.AreEqual(Double(-122.4180),Double(emp.Office.Geo.Longitude), 0.0001, 'Geo.Longitude');
    Assert.AreEqual(Double(12.5),     Double(emp.Office.Geo.Altitude),  0.001,  'Geo.Altitude');
  finally
    emp.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Complex_RoundTrip_Employee;
var
  src  : TEmployee;
  yaml : string;
  dst  : TEmployee;
begin
  src := BuildEmployee;
  try
    yaml := fSerializer.ObjectToYaml(src);
    dst  := fSerializer.YamlToObject(TEmployee, yaml) as TEmployee;
    try
      Assert.AreEqual(src.Id,                          dst.Id,       'Id');
      Assert.AreEqual(src.FullName,                    dst.FullName, 'FullName');
      Assert.AreEqual(Ord(src.Status),                 Ord(dst.Status), 'Status');
      Assert.AreEqual(Double(src.Salary), Double(dst.Salary), 0.01,  'Salary');
      Assert.AreEqual(src.YearsExp,                    dst.YearsExp, 'YearsExp');
      Assert.AreEqual(3, Integer(Length(dst.Skills)),               'Skills count');
      Assert.AreEqual(src.Skills[0],                   dst.Skills[0],'Skills[0]');
      Assert.AreEqual(src.Contact.Email,               dst.Contact.Email,  'Contact.Email');
      Assert.AreEqual(src.Contact.Phone,               dst.Contact.Phone,  'Contact.Phone');
      Assert.AreEqual(src.Contact.Mobile,              dst.Contact.Mobile, 'Contact.Mobile');
      Assert.AreEqual(src.Office.Street,               dst.Office.Street,  'Office.Street');
      Assert.AreEqual(src.Office.City,                 dst.Office.City,    'Office.City');
      Assert.AreEqual(Double(src.Office.Geo.Latitude),
                      Double(dst.Office.Geo.Latitude), 0.0001,             'Geo.Latitude');
      Assert.AreEqual(Double(src.Office.Geo.Longitude),
                      Double(dst.Office.Geo.Longitude), 0.0001,            'Geo.Longitude');
      Assert.AreEqual(Double(src.Office.Geo.Altitude),
                      Double(dst.Office.Geo.Altitude), 0.001,              'Geo.Altitude');
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Complex_GeoPoint_FloatPrecision;
var
  src  : TGeoPoint;
  yaml : string;
  dst  : TGeoPoint;
begin
  src := TGeoPoint.Create;
  try
    src.Latitude  :=  48.858844;
    src.Longitude :=   2.294351;
    src.Altitude  := 330.500;
    yaml := fSerializer.ObjectToYaml(src);
    dst  := fSerializer.YamlToObject(TGeoPoint, yaml) as TGeoPoint;
    try
      Assert.AreEqual(Double(src.Latitude),  Double(dst.Latitude),  0.000001, 'Latitude precision');
      Assert.AreEqual(Double(src.Longitude), Double(dst.Longitude), 0.000001, 'Longitude precision');
      Assert.AreEqual(Double(src.Altitude),  Double(dst.Altitude),  0.001,    'Altitude precision');
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Complex_MultipleStringArrays;
var
  org  : TOrganisation;
  yaml : string;
  dst  : TOrganisation;
begin
  // Build an organisation with both Department and Tags, round-trip multiple arrays
  org := TOrganisation.Create;
  try
    org.Name        := 'MultiArray Corp';
    org.FoundedYear := 2000;
    org.IsPublic    := False;
    org.Revenue     := 0;
    org.Tags        := ['go', 'rust', 'zig', 'delphi', 'fpc'];
    org.HeadOffice.Street  := 'Test St';
    org.HeadOffice.City    := 'TestCity';
    org.HeadOffice.Country := 'TC';
    org.HeadOffice.Geo.Latitude  := 0;
    org.HeadOffice.Geo.Longitude := 0;
    org.HeadOffice.Geo.Altitude  := 0;
    org.Department.Name      := 'QA';
    org.Department.Kind      := dkEngineering;
    org.Department.Budget    := 50000;
    org.Department.Active    := True;
    org.Department.HeadCount := 5;
    yaml := fSerializer.ObjectToYaml(org);
    dst  := fSerializer.YamlToObject(TOrganisation, yaml) as TOrganisation;
    try
      Assert.AreEqual(5, Integer(Length(dst.Tags)), 'Tags count after round-trip');
      Assert.AreEqual('go',     dst.Tags[0], 'Tags[0]');
      Assert.AreEqual('rust',   dst.Tags[1], 'Tags[1]');
      Assert.AreEqual('zig',    dst.Tags[2], 'Tags[2]');
      Assert.AreEqual('delphi', dst.Tags[3], 'Tags[3]');
      Assert.AreEqual('fpc',    dst.Tags[4], 'Tags[4]');
      Assert.AreEqual('QA',     dst.Department.Name, 'Department.Name');
      Assert.AreEqual(5,        dst.Department.HeadCount, 'Department.HeadCount');
    finally
      dst.Free;
    end;
  finally
    org.Free;
  end;
end;

// ── Issue #147: null YAML pair must leave object property nil ────────────

procedure TYamlSerializerTests.Test_Deserialize_NullPair_ObjectPropertyRemainsNil;
const
  // Use CRLF line endings: on Windows TYamlObject.ParseYamlValue splits on #13
  // so LF-only constants result in a single unparseable line.
  YAML_WITH_NULLS =
    'id: 1'        + #13#10 +
    'child: null'  + #13#10 +
    'child2: null';
var
  obj : TNullableOwner;
begin
  // deserialise a YAML where nested object properties are explicitly null
  obj := fSerializer.YamlToObject(TNullableOwner, YAML_WITH_NULLS) as TNullableOwner;
  try
    Assert.AreEqual(1, obj.Id, 'Id must be 1');
    // Issue #147: Child must stay nil, not be constructed as an empty object
    Assert.IsNull(obj.Child,  'Child property must be nil when YAML pair is null');
    Assert.IsNull(obj.Child2, 'Child2 property must be nil when YAML pair is null');
  finally
    obj.Free;
  end;
end;

procedure TYamlSerializerTests.Test_Deserialize_NullPair_ListPropertyRemainsNil;
var
  src  : TNullableOwner;
  yaml : string;
  obj  : TNullableOwner;
begin
  // Verify the fix via round-trip: serialise an owner with nil children,
  // then deserialise – the children must remain nil (issue #147)
  src := TNullableOwner.Create;
  try
    src.Id := 99;
    // Child and Child2 remain nil (never assigned)
    yaml := fSerializer.ObjectToYaml(src);
  finally
    src.Free;
  end;
  // YAML should contain 'null' for the object properties
  Assert.IsTrue(yaml.Contains('null'), 'Serialised YAML must contain null for nil objects');
  obj := fSerializer.YamlToObject(TNullableOwner, yaml) as TNullableOwner;
  try
    Assert.AreEqual(99, obj.Id, 'Id must round-trip correctly');
    // Issue #147: nil object properties must remain nil after deserialisation
    Assert.IsNull(obj.Child,  'Child must be nil after round-trip');
    Assert.IsNull(obj.Child2, 'Child2 must be nil after round-trip');
  finally
    obj.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TYamlSerializerTests);

end.
