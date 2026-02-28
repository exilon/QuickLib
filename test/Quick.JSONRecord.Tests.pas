{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.JSONRecord.Tests
  Description : Quick.JSONRecord unit tests
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

unit Quick.JSONRecord.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Quick.JSONRecord;

// ── Test model ──────────────────────────────────────────────────────────────

type
  TPersonRecord = class(TJsonRecord)
  private
    fName : string;
    fAge  : Integer;
    fActive : Boolean;
  published
    property Name   : string  read fName   write fName;
    property Age    : Integer read fAge    write fAge;
    property Active : Boolean read fActive write fActive;
  end;

  TAddressRecord = class(TJsonRecord)
  private
    fCity    : string;
    fCountry : string;
  published
    property City    : string read fCity    write fCity;
    property Country : string read fCountry write fCountry;
  end;

// ── Fixture ─────────────────────────────────────────────────────────────────

  [TestFixture]
  TQuickJsonRecordTests = class(TObject)
  private
    fPerson : TPersonRecord;
    fTempFile : string;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // ToJson
    [Test]
    procedure Test_ToJson_ContainsProperties;
    [Test]
    procedure Test_ToJson_DefaultIndent_IsSingleLine;
    [Test]
    procedure Test_ToJson_WithIndent_IsMultiLine;

    // FromJson
    [Test]
    procedure Test_FromJson_SetsProperties;
    [Test]
    procedure Test_FromJson_PartialJson_SetsSuppliedFields;

    // CreateFromJson
    [Test]
    procedure Test_CreateFromJson_CreatesWithData;

    // SaveToFile / LoadFromFile
    [Test]
    procedure Test_SaveToFile_And_LoadFromFile_RoundTrip;

    // Clone
    [Test]
    procedure Test_Clone_ProducesEqualObject;
    [Test]
    procedure Test_Clone_IsIndependentCopy;

    // MapTo / MapFrom
    [Test]
    procedure Test_MapTo_CopiesProperties;
    [Test]
    procedure Test_MapFrom_LoadsProperties;
  end;

implementation

// ── Setup / TearDown ─────────────────────────────────────────────────────────

procedure TQuickJsonRecordTests.SetUp;
begin
  fPerson := TPersonRecord.Create;
  fPerson.Name   := 'Alice';
  fPerson.Age    := 30;
  fPerson.Active := True;
  fTempFile := TPath.Combine(TPath.GetTempPath,
    'qljsonrec_' + IntToStr(TThread.CurrentThread.ThreadID) + '.json');
end;

procedure TQuickJsonRecordTests.TearDown;
begin
  fPerson.Free;
  if TFile.Exists(fTempFile) then TFile.Delete(fTempFile);
end;

// ── ToJson ───────────────────────────────────────────────────────────────────

procedure TQuickJsonRecordTests.Test_ToJson_ContainsProperties;
var
  j : string;
begin
  j := fPerson.ToJson;
  Assert.IsTrue(j.Contains('Alice'), 'ToJson should contain the Name value');
  Assert.IsTrue(j.Contains('30') or j.Contains('"Age"'), 'ToJson should contain the Age value');
end;

procedure TQuickJsonRecordTests.Test_ToJson_DefaultIndent_IsSingleLine;
var
  j : string;
begin
  j := fPerson.ToJson(False);
  // A non-indented JSON should have no newlines
  Assert.IsFalse(j.Contains(#10), 'Non-indented ToJson should not contain newline characters');
end;

procedure TQuickJsonRecordTests.Test_ToJson_WithIndent_IsMultiLine;
var
  j : string;
begin
  j := fPerson.ToJson(True);
  Assert.IsTrue(j.Contains(#10) or j.Contains(#13),
    'Indented ToJson should contain newline characters');
end;

// ── FromJson ─────────────────────────────────────────────────────────────────

procedure TQuickJsonRecordTests.Test_FromJson_SetsProperties;
var
  p : TPersonRecord;
begin
  p := TPersonRecord.Create;
  try
    p.FromJson('{"Name":"Bob","Age":25,"Active":false}');
    Assert.AreEqual('Bob', p.Name, 'FromJson should set Name');
    Assert.AreEqual(25,    p.Age,  'FromJson should set Age');
    Assert.IsFalse(p.Active, 'FromJson should set Active to false');
  finally
    p.Free;
  end;
end;

procedure TQuickJsonRecordTests.Test_FromJson_PartialJson_SetsSuppliedFields;
var
  p : TPersonRecord;
begin
  p := TPersonRecord.Create;
  try
    p.Name := 'Original';
    p.FromJson('{"Age":99}');
    Assert.AreEqual(99, p.Age, 'FromJson should set the supplied Age field');
  finally
    p.Free;
  end;
end;

// ── CreateFromJson ────────────────────────────────────────────────────────────

procedure TQuickJsonRecordTests.Test_CreateFromJson_CreatesWithData;
var
  p : TPersonRecord;
begin
  p := TPersonRecord.CreateFromJson('{"Name":"Carol","Age":42,"Active":true}') as TPersonRecord;
  try
    Assert.AreEqual('Carol', p.Name, 'CreateFromJson should set Name');
    Assert.AreEqual(42, p.Age, 'CreateFromJson should set Age');
  finally
    p.Free;
  end;
end;

// ── SaveToFile / LoadFromFile ─────────────────────────────────────────────────

procedure TQuickJsonRecordTests.Test_SaveToFile_And_LoadFromFile_RoundTrip;
var
  loaded : TPersonRecord;
begin
  fPerson.SaveToFile(fTempFile);
  Assert.IsTrue(TFile.Exists(fTempFile), 'SaveToFile should create the file');

  loaded := TPersonRecord.Create;
  try
    loaded.LoadFromFile(fTempFile);
    Assert.AreEqual('Alice', loaded.Name,  'Loaded Name should match saved');
    Assert.AreEqual(30,      loaded.Age,   'Loaded Age should match saved');
    Assert.IsTrue(loaded.Active,           'Loaded Active should match saved');
  finally
    loaded.Free;
  end;
end;

// ── Clone ─────────────────────────────────────────────────────────────────────

procedure TQuickJsonRecordTests.Test_Clone_ProducesEqualObject;
var
  cloned : TPersonRecord;
begin
  cloned := fPerson.Clone as TPersonRecord;
  try
    Assert.AreEqual(fPerson.Name,   cloned.Name,   'Cloned Name should equal original');
    Assert.AreEqual(fPerson.Age,    cloned.Age,    'Cloned Age should equal original');
    Assert.AreEqual(fPerson.Active, cloned.Active, 'Cloned Active should equal original');
  finally
    cloned.Free;
  end;
end;

procedure TQuickJsonRecordTests.Test_Clone_IsIndependentCopy;
var
  cloned : TPersonRecord;
begin
  cloned := fPerson.Clone as TPersonRecord;
  try
    cloned.Name := 'Modified';
    Assert.AreEqual('Alice', fPerson.Name,
      'Changing clone Name should not affect the original');
  finally
    cloned.Free;
  end;
end;

// ── MapTo / MapFrom ───────────────────────────────────────────────────────────

procedure TQuickJsonRecordTests.Test_MapTo_CopiesProperties;
var
  dest : TPersonRecord;
begin
  dest := TPersonRecord.Create;
  try
    fPerson.MapTo(dest);
    Assert.AreEqual(fPerson.Name, dest.Name, 'MapTo should copy Name');
    Assert.AreEqual(fPerson.Age,  dest.Age,  'MapTo should copy Age');
  finally
    dest.Free;
  end;
end;

procedure TQuickJsonRecordTests.Test_MapFrom_LoadsProperties;
var
  src  : TPersonRecord;
  dest : TPersonRecord;
begin
  src := TPersonRecord.Create;
  src.Name := 'Dave';
  src.Age  := 55;
  dest := TPersonRecord.Create;
  try
    dest.MapFrom(src);
    Assert.AreEqual('Dave', dest.Name, 'MapFrom should load Name from source');
    Assert.AreEqual(55,     dest.Age,  'MapFrom should load Age from source');
  finally
    src.Free;
    dest.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickJsonRecordTests);

end.
