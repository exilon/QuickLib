{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.Json.Serializer.Tests
  Description : Quick.Json.Serializer unit tests
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 27/02/2026
  Modified    : 27/02/2026
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

unit Quick.Json.Serializer.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  Quick.Json.Serializer;

// ── Test model classes ───────────────────────────────────────────────────────

type
  TColor = (clRed, clGreen, clBlue);

  TSimpleModel = class
  private
    fName    : string;
    fAge     : Integer;
    fSalary  : Double;
    fActive  : Boolean;
  published
    property Name   : string  read fName   write fName;
    property Age    : Integer read fAge    write fAge;
    property Salary : Double  read fSalary write fSalary;
    property Active : Boolean read fActive write fActive;
  end;

  TAddressModel = class
  private
    fCity    : string;
    fCountry : string;
  published
    property City    : string read fCity    write fCity;
    property Country : string read fCountry write fCountry;
  end;

  TPersonWithAddress = class
  private
    fName    : string;
    fAge     : Integer;
    fAddress : TAddressModel;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Name    : string        read fName    write fName;
    property Age     : Integer       read fAge     write fAge;
    property Address : TAddressModel read fAddress write fAddress;
  end;

  TEnumModel = class
  private
    fColor : TColor;
  published
    property Color : TColor read fColor write fColor;
  end;

// ── Fixture ──────────────────────────────────────────────────────────────────

  [TestFixture]
  TQuickJsonSerializerTests = class(TObject)
  private
    fSerializer : TJsonSerializer;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // ObjectToJson
    [Test]
    procedure Test_ObjectToJson_SimpleModel_ContainsFields;
    [Test]
    procedure Test_ObjectToJson_StringField_Quoted;
    [Test]
    procedure Test_ObjectToJson_IntegerField;
    [Test]
    procedure Test_ObjectToJson_BooleanField_True;
    [Test]
    procedure Test_ObjectToJson_BooleanField_False;
    [Test]
    procedure Test_ObjectToJson_Indent_IsMultiLine;
    [Test]
    procedure Test_ObjectToJson_NoIndent_IsSingleLine;

    // JsonToObject
    [Test]
    procedure Test_JsonToObject_SetsStringField;
    [Test]
    procedure Test_JsonToObject_SetsIntegerField;
    [Test]
    procedure Test_JsonToObject_SetsBooleanField;
    [Test]
    procedure Test_JsonToObject_SetsDoubleField;

    // Round-trip
    [Test]
    procedure Test_RoundTrip_SimpleModel;
    [Test]
    procedure Test_RoundTrip_NestedObject;

    // UseEnumNames
    [Test]
    procedure Test_UseEnumNames_True_SerializesName;
    [Test]
    procedure Test_UseEnumNames_False_SerializesOrdinal;

    // UseNullStringsAsEmpty
    [Test]
    procedure Test_UseNullStringsAsEmpty_True_DeserializesNullAsEmpty;

    // ObjectToJsonString / JsonToObject overload
    [Test]
    procedure Test_ObjectToJsonString_ReturnsString;

    // Array serialization
    [Test]
    procedure Test_ArrayToJson_IntArray;
    [Test]
    procedure Test_JsonToArray_IntArray;
  end;

// ── Implementations of model helpers ─────────────────────────────────────────

implementation

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

// ── Setup / TearDown ─────────────────────────────────────────────────────────

procedure TQuickJsonSerializerTests.SetUp;
begin
  fSerializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
end;

procedure TQuickJsonSerializerTests.TearDown;
begin
  fSerializer.Free;
end;

// ── ObjectToJson ─────────────────────────────────────────────────────────────

procedure TQuickJsonSerializerTests.Test_ObjectToJson_SimpleModel_ContainsFields;
var
  m : TSimpleModel;
  j : string;
begin
  m := TSimpleModel.Create;
  try
    m.Name   := 'Alice';
    m.Age    := 30;
    m.Salary := 1234.56;
    m.Active := True;
    j := fSerializer.ObjectToJson(m);
    Assert.IsTrue(j.Contains('Alice'), 'JSON should contain Name value');
    Assert.IsTrue(j.Contains('30') or j.Contains('"Age"'), 'JSON should contain Age');
  finally
    m.Free;
  end;
end;

procedure TQuickJsonSerializerTests.Test_ObjectToJson_StringField_Quoted;
var
  m : TSimpleModel;
  j : string;
begin
  m := TSimpleModel.Create;
  try
    m.Name := 'Test';
    j := fSerializer.ObjectToJson(m);
    Assert.IsTrue(j.Contains('"Test"'), 'String values should be quoted in JSON');
  finally
    m.Free;
  end;
end;

procedure TQuickJsonSerializerTests.Test_ObjectToJson_IntegerField;
var
  m : TSimpleModel;
  j : string;
begin
  m := TSimpleModel.Create;
  try
    m.Age := 42;
    j := fSerializer.ObjectToJson(m);
    Assert.IsTrue(j.Contains('42'), 'JSON should contain integer value 42');
  finally
    m.Free;
  end;
end;

procedure TQuickJsonSerializerTests.Test_ObjectToJson_BooleanField_True;
var
  m : TSimpleModel;
  j : string;
begin
  m := TSimpleModel.Create;
  try
    m.Active := True;
    j := fSerializer.ObjectToJson(m);
    Assert.IsTrue(j.Contains('true'), 'JSON should contain lowercase true for Boolean');
  finally
    m.Free;
  end;
end;

procedure TQuickJsonSerializerTests.Test_ObjectToJson_BooleanField_False;
var
  m : TSimpleModel;
  j : string;
begin
  m := TSimpleModel.Create;
  try
    m.Active := False;
    j := fSerializer.ObjectToJson(m);
    Assert.IsTrue(j.Contains('false'), 'JSON should contain lowercase false for Boolean');
  finally
    m.Free;
  end;
end;

procedure TQuickJsonSerializerTests.Test_ObjectToJson_Indent_IsMultiLine;
var
  m : TSimpleModel;
  j : string;
begin
  m := TSimpleModel.Create;
  try
    j := fSerializer.ObjectToJson(m, True);
    Assert.IsTrue(j.Contains(#10) or j.Contains(#13),
      'Indented JSON should contain newline characters');
  finally
    m.Free;
  end;
end;

procedure TQuickJsonSerializerTests.Test_ObjectToJson_NoIndent_IsSingleLine;
var
  m : TSimpleModel;
  j : string;
begin
  m := TSimpleModel.Create;
  try
    j := fSerializer.ObjectToJson(m, False);
    Assert.IsFalse(j.Contains(#10),
      'Non-indented JSON should not contain newline characters');
  finally
    m.Free;
  end;
end;

// ── JsonToObject ─────────────────────────────────────────────────────────────

procedure TQuickJsonSerializerTests.Test_JsonToObject_SetsStringField;
var
  m : TSimpleModel;
begin
  m := TSimpleModel.Create;
  try
    fSerializer.JsonToObject(m, '{"Name":"Bob"}');
    Assert.AreEqual('Bob', m.Name, 'JsonToObject should set the Name field');
  finally
    m.Free;
  end;
end;

procedure TQuickJsonSerializerTests.Test_JsonToObject_SetsIntegerField;
var
  m : TSimpleModel;
begin
  m := TSimpleModel.Create;
  try
    fSerializer.JsonToObject(m, '{"Age":25}');
    Assert.AreEqual(25, m.Age, 'JsonToObject should set the Age field');
  finally
    m.Free;
  end;
end;

procedure TQuickJsonSerializerTests.Test_JsonToObject_SetsBooleanField;
var
  m : TSimpleModel;
begin
  m := TSimpleModel.Create;
  try
    fSerializer.JsonToObject(m, '{"Active":true}');
    Assert.IsTrue(m.Active, 'JsonToObject should set Active to true');
  finally
    m.Free;
  end;
end;

procedure TQuickJsonSerializerTests.Test_JsonToObject_SetsDoubleField;
var
  m : TSimpleModel;
begin
  m := TSimpleModel.Create;
  try
    fSerializer.JsonToObject(m, '{"Salary":9876.5}');
    Assert.AreEqual(Double(9876.5), Double(m.Salary), 0.01,
      'JsonToObject should set the Salary double field');
  finally
    m.Free;
  end;
end;

// ── Round-trip ────────────────────────────────────────────────────────────────

procedure TQuickJsonSerializerTests.Test_RoundTrip_SimpleModel;
var
  src, dest : TSimpleModel;
  j         : string;
begin
  src := TSimpleModel.Create;
  dest := TSimpleModel.Create;
  try
    src.Name   := 'RoundTrip';
    src.Age    := 77;
    src.Active := True;
    j := fSerializer.ObjectToJson(src);
    fSerializer.JsonToObject(dest, j);
    Assert.AreEqual('RoundTrip', dest.Name,   'Round-trip Name should match');
    Assert.AreEqual(77,          dest.Age,    'Round-trip Age should match');
    Assert.IsTrue(dest.Active,               'Round-trip Active should be true');
  finally
    src.Free;
    dest.Free;
  end;
end;

procedure TQuickJsonSerializerTests.Test_RoundTrip_NestedObject;
var
  src, dest : TPersonWithAddress;
  j         : string;
begin
  src := TPersonWithAddress.Create;
  dest := TPersonWithAddress.Create;
  try
    src.Name            := 'Nested';
    src.Age             := 50;
    src.Address.City    := 'Madrid';
    src.Address.Country := 'Spain';
    j := fSerializer.ObjectToJson(src);
    fSerializer.JsonToObject(dest, j);
    Assert.AreEqual('Nested', dest.Name,            'Nested round-trip Name should match');
    Assert.AreEqual('Madrid', dest.Address.City,    'Nested round-trip City should match');
    Assert.AreEqual('Spain',  dest.Address.Country, 'Nested round-trip Country should match');
  finally
    src.Free;
    dest.Free;
  end;
end;

// ── UseEnumNames ─────────────────────────────────────────────────────────────

procedure TQuickJsonSerializerTests.Test_UseEnumNames_True_SerializesName;
var
  m  : TEnumModel;
  s  : TJsonSerializer;
  j  : string;
begin
  s := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty, True);
  m := TEnumModel.Create;
  try
    m.Color := clGreen;
    j := s.ObjectToJson(m);
    Assert.IsTrue(j.Contains('clGreen') or j.Contains('Green'),
      'With UseEnumNames=True the enum name should appear in JSON');
  finally
    m.Free;
    s.Free;
  end;
end;

procedure TQuickJsonSerializerTests.Test_UseEnumNames_False_SerializesOrdinal;
var
  m : TEnumModel;
  s : TJsonSerializer;
  j : string;
begin
  s := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty, False);
  m := TEnumModel.Create;
  try
    m.Color := clBlue;  // ordinal 2
    j := s.ObjectToJson(m);
    Assert.IsTrue(j.Contains('2'),
      'With UseEnumNames=False the ordinal (2) should appear in JSON');
  finally
    m.Free;
    s.Free;
  end;
end;

// ── UseNullStringsAsEmpty ─────────────────────────────────────────────────────

procedure TQuickJsonSerializerTests.Test_UseNullStringsAsEmpty_True_DeserializesNullAsEmpty;
var
  m : TSimpleModel;
  s : TJsonSerializer;
begin
  s := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty, True, True);
  m := TSimpleModel.Create;
  try
    s.JsonToObject(m, '{"Name":"Null"}');
    Assert.AreEqual('', m.Name,
      'With UseNullStringsAsEmpty=True a "Null" string should deserialize as empty');
  finally
    m.Free;
    s.Free;
  end;
end;

// ── ObjectToJsonString ────────────────────────────────────────────────────────

procedure TQuickJsonSerializerTests.Test_ObjectToJsonString_ReturnsString;
var
  m : TSimpleModel;
  j : string;
begin
  m := TSimpleModel.Create;
  try
    m.Name := 'StrTest';
    j := fSerializer.ObjectToJsonString(m);
    Assert.IsTrue(j.StartsWith('{'), 'ObjectToJsonString should return a JSON object string');
    Assert.IsTrue(j.EndsWith('}'),   'ObjectToJsonString should end with }');
  finally
    m.Free;
  end;
end;

// ── Array serialization ───────────────────────────────────────────────────────

procedure TQuickJsonSerializerTests.Test_ArrayToJson_IntArray;
var
  arr : TArray<Integer>;
  j   : string;
begin
  arr := [10, 20, 30];
  j := fSerializer.ArrayToJson<Integer>(arr);
  Assert.IsTrue(j.Contains('10'), 'ArrayToJson should contain 10');
  Assert.IsTrue(j.Contains('20'), 'ArrayToJson should contain 20');
  Assert.IsTrue(j.Contains('30'), 'ArrayToJson should contain 30');
  Assert.IsTrue(j.StartsWith('['), 'ArrayToJson should return a JSON array starting with [');
end;

procedure TQuickJsonSerializerTests.Test_JsonToArray_IntArray;
var
  arr : TArray<Integer>;
begin
  arr := fSerializer.JsonToArray<Integer>('[5,10,15]');
  Assert.AreEqual(3, Integer(Length(arr)), 'JsonToArray should return 3 elements');
  Assert.AreEqual(5,  Integer(arr[0]), 'First element should be 5');
  Assert.AreEqual(10, Integer(arr[1]), 'Second element should be 10');
  Assert.AreEqual(15, Integer(arr[2]), 'Third element should be 15');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickJsonSerializerTests);

end.
