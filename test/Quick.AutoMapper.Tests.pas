{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.AutoMapper.Tests
  Description : AutoMapper unit tests
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

unit Quick.AutoMapper.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  System.DateUtils,
  Quick.AutoMapper,
  rtti;

type
  TPrimitivesSource = class
  public
    IntField: Integer;
    StrField: string;
    BoolField: Boolean;
    FloatField: Double;
    DateField: TDateTime;
    constructor Create;
  end;

  TPrimitivesTarget = class
  public
    IntField: Integer;
    StrField: string;
    BoolField: Boolean;
    FloatField: Double;
    DateField: TDateTime;
  end;

  TNestedSource = class
  public
    Nested: TPrimitivesSource;
    constructor Create;
    destructor Destroy; override;
  end;

  TNestedTarget = class
  public
    Nested: TPrimitivesTarget;
    constructor Create;
    destructor Destroy; override;
  end;

  TArraySource = class
  public
    Values: TArray<Integer>;
    constructor Create;
  end;

  TArrayTarget = class
  public
    Values: TArray<Integer>;
  end;

  TListSource = class
  public
    List: TObjectList<TPrimitivesSource>;
    constructor Create;
    destructor Destroy; override;
  end;

  TListTarget = class
  public
    List: TObjectList<TPrimitivesTarget>;
    constructor Create;
    destructor Destroy; override;
  end;

  TFieldSource = class
  public
    IntValue: Integer;
    StrValue: string;
    constructor Create;
  end;

  TFieldTarget = class
  public
    IntValue: Integer;
    StrValue: string;
  end;

  TMixedSource = class
  public
    FieldValue: Integer;
  private
    FPropValue: string;
  published
    property PropValue: string read FPropValue write FPropValue;
    constructor Create;
  end;

  TMixedTarget = class
  public
    FieldValue: Integer;
  private
    FPropValue: string;
  published
    property PropValue: string read FPropValue write FPropValue;
  end;

  TNumericSource = class
  public
    IntValue: Integer;
    Int64Value: Int64;
    DoubleValue: Double;
    ExtValue: Extended;
    constructor Create;
  end;

  TNumericTarget = class
  public
    IntFromDouble: Integer;
    DoubleFromInt: Double;
    Int64FromExt: Int64;
    ExtFromInt64: Extended;
  end;

  TPrimitiveArraySource = class
  public
    IntArray: TArray<Integer>;
    StrArray: TArray<string>;
    constructor Create;
  end;

  TPrimitiveArrayTarget = class
  public
    IntArray: TArray<Integer>;
    StrArray: TArray<string>;
  end;

  // ── Complex multi-level models ──────────────────────────────────

  TAddressInfo = class
  public
    Street  : string;
    City    : string;
    Country : string;
    ZipCode : string;
  end;

  TContactDetails = class
  public
    Email  : string;
    Phone  : string;
  end;

  TGeoCoord = class
  public
    Lat : Double;
    Lon : Double;
  end;

  TCompanyInfo = class
  public
    Name       : string;
    Industry   : string;
    Employees  : Integer;
    Revenue    : Double;
    Address    : TAddressInfo;
    constructor Create;
    destructor  Destroy; override;
  end;

  TPersonFull = class
  public
    Id       : Integer;
    Name     : string;
    Age      : Integer;
    Active   : Boolean;
    Salary   : Double;
    Contact  : TContactDetails;
    Address  : TAddressInfo;
    Company  : TCompanyInfo;
    Tags     : TArray<string>;
    Scores   : TArray<Integer>;
    constructor Create;
    destructor  Destroy; override;
  end;

  // Flat DTO — receives selected fields via custom mapping
  TPersonDTO = class
  public
    PersonId    : Integer;
    FullName    : string;
    Email       : string;
    CompanyName : string;
    City        : string;
    IsActive    : Boolean;
  end;

  // Mirror of TPersonFull for round-trip mapping
  TPersonFullTarget = class
  public
    Id      : Integer;
    Name    : string;
    Age     : Integer;
    Active  : Boolean;
    Salary  : Double;
    Contact : TContactDetails;
    Address : TAddressInfo;
    Company : TCompanyInfo;
    Tags    : TArray<string>;
    Scores  : TArray<Integer>;
    constructor Create;
    destructor  Destroy; override;
  end;

  // Renamed fields target
  TPersonRenamed = class
  public
    Identifier : Integer;
    FullName   : string;
    Employed   : Boolean;
    Wage       : Double;
  end;

  // Models for nil-nested-object test
  TInnerObj = class
  public
    Value : string;
  end;

  TOuterSrc = class
  public
    Id    : Integer;
    Inner : TInnerObj;   // will be nil in test
  end;

  TOuterTgt = class
  public
    Id    : Integer;
    Inner : TInnerObj;   // will be nil
    constructor Create;
    destructor  Destroy; override;
  end;

  // ── Models for nested TObjectList tests ───────────────────────

  TOrderLine = class
  public
    ProductName : string;
    Quantity    : Integer;
    UnitPrice   : Double;
  end;

  TOrderLineDst = class
  public
    ProductName : string;
    Quantity    : Integer;
    UnitPrice   : Double;
  end;

  TOrderSrc = class
  public
    OrderId   : Integer;
    Customer  : string;
    Lines     : TObjectList<TOrderLine>;
    constructor Create;
    destructor  Destroy; override;
  end;

  TOrderDst = class
  public
    OrderId   : Integer;
    Customer  : string;
    Lines     : TObjectList<TOrderLineDst>;
    constructor Create;
    destructor  Destroy; override;
  end;

  // ── Models for nested TList<string> / TList<Integer> tests ────

  TTagsSrc = class
  public
    Name   : string;
    Tags   : TList<string>;
    Scores : TList<Integer>;
    constructor Create;
    destructor  Destroy; override;
  end;

  TTagsDst = class
  public
    Name   : string;
    Tags   : TList<string>;
    Scores : TList<Integer>;
    constructor Create;
    destructor  Destroy; override;
  end;

  // ── Models for mixed scenario: object + objectlist + arrays ────

  TProductSrc = class
  public
    Code  : string;
    Price : Double;
  end;

  TProductDst = class
  public
    Code  : string;
    Price : Double;
  end;

  TCatalogSrc = class
  public
    Title    : string;
    Products : TObjectList<TProductSrc>;
    Tags     : TArray<string>;
    Ids      : TArray<Integer>;
    constructor Create;
    destructor  Destroy; override;
  end;

  TCatalogDst = class
  public
    Title    : string;
    Products : TObjectList<TProductDst>;
    Tags     : TArray<string>;
    Ids      : TArray<Integer>;
    constructor Create;
    destructor  Destroy; override;
  end;

  // ── TList<Double> and TList<Boolean> nested tests ────────────

  TMetricsSrc = class
  public
    Caption : string;
    Values  : TList<Double>;
    Flags   : TList<Boolean>;
    constructor Create;
    destructor  Destroy; override;
  end;

  TMetricsDst = class
  public
    Caption : string;
    Values  : TList<Double>;
    Flags   : TList<Boolean>;
    constructor Create;
    destructor  Destroy; override;
  end;

  // ── Object with two independent TObjectList fields ────────────

  TInvoiceLine = class
  public
    Description : string;
    Amount      : Double;
  end;

  TPaymentLine = class
  public
    Method  : string;
    Paid    : Double;
  end;

  TInvoiceSrc = class
  public
    Number   : string;
    Lines    : TObjectList<TInvoiceLine>;
    Payments : TObjectList<TPaymentLine>;
    constructor Create;
    destructor  Destroy; override;
  end;

  TInvoiceDst = class
  public
    Number   : string;
    Lines    : TObjectList<TInvoiceLine>;
    Payments : TObjectList<TPaymentLine>;
    constructor Create;
    destructor  Destroy; override;
  end;

  // ── Inherited fields: base + descendant ──────────────────────

  TBaseEntity = class
  public
    Id        : Integer;
    CreatedAt : TDateTime;
  end;

  TDerivedSrc = class(TBaseEntity)
  public
    Name  : string;
    Score : Double;
    constructor Create;
  end;

  TDerivedDst = class(TBaseEntity)
  public
    Name  : string;
    Score : Double;
  end;

  // ── All-zero / false / empty source ──────────────────────────

  TZeroSrc = class
  public
    IntVal  : Integer;
    StrVal  : string;
    BoolVal : Boolean;
    DblVal  : Double;
  end;

  TZeroDst = class
  public
    IntVal  : Integer;
    StrVal  : string;
    BoolVal : Boolean;
    DblVal  : Double;
  end;

  // ── Int64 ↔ Integer cross-type field mapping ──────────────────

  TWideSrc = class
  public
    BigId    : Int64;
    SmallVal : Integer;
    constructor Create;
  end;

  TWideDst = class
  public
    BigId    : Int64;
    SmallVal : Integer;
  end;

  // ── 3-level: owner → TObjectList → nested object ─────────────

  TItemDetail = class
  public
    Sku   : string;
    Stock : Integer;
  end;

  TDeepLine = class
  public
    Ref    : string;
    Detail : TItemDetail;
    constructor Create;
    destructor  Destroy; override;
  end;

  TDeepLineDst = class
  public
    Ref    : string;
    Detail : TItemDetail;
    constructor Create;
    destructor  Destroy; override;
  end;

  TDeepOrderSrc = class
  public
    Code  : string;
    Lines : TObjectList<TDeepLine>;
    constructor Create;
    destructor  Destroy; override;
  end;

  TDeepOrderDst = class
  public
    Code  : string;
    Lines : TObjectList<TDeepLineDst>;
    constructor Create;
    destructor  Destroy; override;
  end;

  // ── Large list stress (100 elements) ─────────────────────────

  TSimpleItem = class
  public
    Index : Integer;
    Name  : string;
  end;

  TSimpleItemDst = class
  public
    Index : Integer;
    Name  : string;
  end;

  TBigListSrc = class
  public
    Title : string;
    Items : TObjectList<TSimpleItem>;
    constructor Create;
    destructor  Destroy; override;
  end;

  TBigListDst = class
  public
    Title : string;
    Items : TObjectList<TSimpleItemDst>;
    constructor Create;
    destructor  Destroy; override;
  end;

  // ── Multiple renames in single mapping ───────────────────────

  TRichSrc = class
  public
    FirstName : string;
    LastName  : string;
    BirthYear : Integer;
    Active    : Boolean;
    constructor Create;
  end;

  TRichDst = class
  public
    GivenName   : string;
    FamilyName  : string;
    YearOfBirth : Integer;
    Enabled     : Boolean;
  end;

  // ── OnDoMapping via TAutoMapper interface ────────────────────

  TAutoMapSrc = class
  public
    X : Integer;
    Y : Integer;
    Z : Integer;
    constructor Create;
  end;

  TAutoMapDst = class
  public
    X : Integer;
    Y : Integer;
    Z : Integer;
  end;

  [TestFixture]
  TQuickAutoMapperTests = class(TObject)
  private
    function BuildPersonFull: TPersonFull;
  public
    // Original tests
    [Test] procedure Test_Primitives;
    [Test] procedure Test_Nested;
    [Test] procedure Test_Array;
    [Test] procedure Test_List;
    [Test] procedure Test_AutoMapper_Interface;
    [Test] procedure Test_CustomMapping;
    [Test] procedure Test_ManualProc;
    [Test] procedure Test_Empty_Nil;
    [Test] procedure Test_Field_To_Property;
    [Test] procedure Test_Mixed_Fields_Properties;
    [Test] procedure Test_CustomMapping_WithPath;
    [Test] procedure Test_NumericConversions;
    [Test] procedure Test_Array_Primitives;

    // Complex multi-level tests
    [Test] procedure Test_Complex_MultiLevel_Serialize;
    [Test] procedure Test_Complex_MultiLevel_RoundTrip;
    [Test] procedure Test_Complex_FlatDTO_CustomMapping;
    [Test] procedure Test_Complex_RenamedFields_CustomMapping;
    [Test] procedure Test_Complex_ManualProc_WithNestedData;
    [Test] procedure Test_Complex_TMapper_Generic;
    [Test] procedure Test_Complex_StringArray_Preserved;
    [Test] procedure Test_Complex_IntArray_Preserved;
    [Test] procedure Test_Complex_AfterMapping_Callback;
    [Test] procedure Test_Complex_NilNestedObject_Handled;

    // Additional coverage tests
    [Test] procedure Test_ObjListMapper_WithCustomMapping;
    [Test] procedure Test_AutoMapper_CustomMapping_Property;
    [Test] procedure Test_AutoMapper_ReverseMap;
    [Test] procedure Test_Mapper_IgnoresExtraTargetFields;
    [Test] procedure Test_DeepNested_ThreeLevel;

    // Nested collection tests
    [Test] procedure Test_NestedObjectList_AutoMap;
    [Test] procedure Test_NestedObjectList_RoundTrip;
    [Test] procedure Test_NestedObjectList_WithManualProc;
    [Test] procedure Test_NestedListString_AutoMap;
    [Test] procedure Test_NestedListInteger_AutoMap;
    [Test] procedure Test_NestedListString_RoundTrip;
    [Test] procedure Test_Mixed_ObjectListAndArrays;
    [Test] procedure Test_NestedObjectList_CustomMapping;
    [Test] procedure Test_NestedObjectList_Empty;

    // Advanced / hardening tests
    [Test] procedure Test_NestedListDouble_AutoMap;
    [Test] procedure Test_NestedListBoolean_AutoMap;
    [Test] procedure Test_TwoObjectLists_SameObject;
    [Test] procedure Test_InheritedFields_Mapped;
    [Test] procedure Test_AllZeroFields_Mapped;
    [Test] procedure Test_Int64_Integer_Fields;
    [Test] procedure Test_ThreeLevel_ListWithNestedObject;
    [Test] procedure Test_LargeObjectList_Stress;
    [Test] procedure Test_MultipleRenames_CustomMapping;
    [Test] procedure Test_OnDoMapping_ViaInterface;
    [Test] procedure Test_ManualProc_OverridesOnlySpecified;
    [Test] procedure Test_EmptyStringArray_NoRaise;
    [Test] procedure Test_BoolField_FalseNotLostAfterMap;
  end;

implementation

{ TPrimitivesSource }
constructor TPrimitivesSource.Create;
begin
  IntField := 42;
  StrField := 'hello';
  BoolField := True;
  FloatField := 3.14;
  DateField := EncodeDate(2022, 1, 1);
end;

{ TNestedSource }
constructor TNestedSource.Create;
begin
  Nested := TPrimitivesSource.Create;
end;
destructor TNestedSource.Destroy;
begin
  Nested.Free;
  inherited;
end;

{ TNestedTarget }
constructor TNestedTarget.Create;
begin
  Nested := TPrimitivesTarget.Create;
end;
destructor TNestedTarget.Destroy;
begin
  Nested.Free;
  inherited;
end;

{ TArraySource }
constructor TArraySource.Create;
begin
  Values := TArray<Integer>.Create(1,2,3,4,5);
end;

{ TListSource }
constructor TListSource.Create;
begin
  List := TObjectList<TPrimitivesSource>.Create;
  List.Add(TPrimitivesSource.Create);
  List.Add(TPrimitivesSource.Create);
  List[1].IntField := 99;
end;
destructor TListSource.Destroy;
begin
  List.Free;
  inherited;
end;

{ TListTarget }
constructor TListTarget.Create;
begin
  List := TObjectList<TPrimitivesTarget>.Create;
end;
destructor TListTarget.Destroy;
begin
  List.Free;
  inherited;
end;

{ TFieldSource }
constructor TFieldSource.Create;
begin
  IntValue := 123;
  StrValue := 'field_test';
end;

{ TMixedSource }
constructor TMixedSource.Create;
begin
  FieldValue := 456;
  FPropValue := 'mixed_test';
end;

{ TNumericSource }
constructor TNumericSource.Create;
begin
  IntValue := 42;
  Int64Value := 9876543210;
  DoubleValue := 3.14159;
  ExtValue := 2.71828;
end;

{ TPrimitiveArraySource }
constructor TPrimitiveArraySource.Create;
begin
  IntArray := TArray<Integer>.Create(10, 20, 30);
  StrArray := TArray<string>.Create('one', 'two', 'three');
end;

{ TOuterTgt }
constructor TOuterTgt.Create;
begin
  inherited;
  Inner := nil;
end;
destructor TOuterTgt.Destroy;
begin
  Inner.Free;
  inherited;
end;

{ TOrderSrc }
constructor TOrderSrc.Create;
begin
  inherited;
  Lines := TObjectList<TOrderLine>.Create(True);
end;
destructor TOrderSrc.Destroy;
begin
  Lines.Free;
  inherited;
end;

{ TOrderDst }
constructor TOrderDst.Create;
begin
  inherited;
  Lines := TObjectList<TOrderLineDst>.Create(True);
end;
destructor TOrderDst.Destroy;
begin
  Lines.Free;
  inherited;
end;

{ TTagsSrc }
constructor TTagsSrc.Create;
begin
  inherited;
  Tags   := TList<string>.Create;
  Scores := TList<Integer>.Create;
end;
destructor TTagsSrc.Destroy;
begin
  Tags.Free;
  Scores.Free;
  inherited;
end;

{ TTagsDst }
constructor TTagsDst.Create;
begin
  inherited;
  Tags   := TList<string>.Create;
  Scores := TList<Integer>.Create;
end;
destructor TTagsDst.Destroy;
begin
  Tags.Free;
  Scores.Free;
  inherited;
end;

{ TCatalogSrc }
constructor TCatalogSrc.Create;
begin
  inherited;
  Products := TObjectList<TProductSrc>.Create(True);
end;
destructor TCatalogSrc.Destroy;
begin
  Products.Free;
  inherited;
end;

{ TCatalogDst }
constructor TCatalogDst.Create;
begin
  inherited;
  Products := TObjectList<TProductDst>.Create(True);
end;
destructor TCatalogDst.Destroy;
begin
  Products.Free;
  inherited;
end;

{ TMetricsSrc }
constructor TMetricsSrc.Create;
begin
  inherited;
  Values := TList<Double>.Create;
  Flags  := TList<Boolean>.Create;
end;
destructor TMetricsSrc.Destroy;
begin
  Values.Free; Flags.Free;
  inherited;
end;

{ TMetricsDst }
constructor TMetricsDst.Create;
begin
  inherited;
  Values := TList<Double>.Create;
  Flags  := TList<Boolean>.Create;
end;
destructor TMetricsDst.Destroy;
begin
  Values.Free; Flags.Free;
  inherited;
end;

{ TInvoiceSrc }
constructor TInvoiceSrc.Create;
begin
  inherited;
  Lines    := TObjectList<TInvoiceLine>.Create(True);
  Payments := TObjectList<TPaymentLine>.Create(True);
end;
destructor TInvoiceSrc.Destroy;
begin
  Lines.Free; Payments.Free;
  inherited;
end;

{ TInvoiceDst }
constructor TInvoiceDst.Create;
begin
  inherited;
  Lines    := TObjectList<TInvoiceLine>.Create(True);
  Payments := TObjectList<TPaymentLine>.Create(True);
end;
destructor TInvoiceDst.Destroy;
begin
  Lines.Free; Payments.Free;
  inherited;
end;

{ TDerivedSrc }
constructor TDerivedSrc.Create;
begin
  inherited;
  Id        := 11;
  CreatedAt := EncodeDate(2025, 6, 15);
  Name      := 'Derived';
  Score     := 99.5;
end;

{ TWideSrc }
constructor TWideSrc.Create;
begin
  inherited;
  BigId    := 9876543210;
  SmallVal := 42;
end;

{ TDeepLine }
constructor TDeepLine.Create;
begin
  inherited;
  Detail := TItemDetail.Create;
end;
destructor TDeepLine.Destroy;
begin
  Detail.Free;
  inherited;
end;

{ TDeepLineDst }
constructor TDeepLineDst.Create;
begin
  inherited;
  Detail := TItemDetail.Create;
end;
destructor TDeepLineDst.Destroy;
begin
  Detail.Free;
  inherited;
end;

{ TDeepOrderSrc }
constructor TDeepOrderSrc.Create;
begin
  inherited;
  Lines := TObjectList<TDeepLine>.Create(True);
end;
destructor TDeepOrderSrc.Destroy;
begin
  Lines.Free;
  inherited;
end;

{ TDeepOrderDst }
constructor TDeepOrderDst.Create;
begin
  inherited;
  Lines := TObjectList<TDeepLineDst>.Create(True);
end;
destructor TDeepOrderDst.Destroy;
begin
  Lines.Free;
  inherited;
end;

{ TBigListSrc }
constructor TBigListSrc.Create;
begin
  inherited;
  Items := TObjectList<TSimpleItem>.Create(True);
end;
destructor TBigListSrc.Destroy;
begin
  Items.Free;
  inherited;
end;

{ TBigListDst }
constructor TBigListDst.Create;
begin
  inherited;
  Items := TObjectList<TSimpleItemDst>.Create(True);
end;
destructor TBigListDst.Destroy;
begin
  Items.Free;
  inherited;
end;

{ TRichSrc }
constructor TRichSrc.Create;
begin
  inherited;
  FirstName := 'Ada';
  LastName  := 'Lovelace';
  BirthYear := 1815;
  Active    := True;
end;

{ TAutoMapSrc }
constructor TAutoMapSrc.Create;
begin
  inherited;
  X := 10; Y := 20; Z := 30;
end;

{ TCompanyInfo }
constructor TCompanyInfo.Create;
begin
  inherited;
  Address := TAddressInfo.Create;
end;
destructor TCompanyInfo.Destroy;
begin
  Address.Free;
  inherited;
end;

{ TPersonFull }
constructor TPersonFull.Create;
begin
  inherited;
  Contact := TContactDetails.Create;
  Address := TAddressInfo.Create;
  Company := TCompanyInfo.Create;
end;
destructor TPersonFull.Destroy;
begin
  Contact.Free;
  Address.Free;
  Company.Free;
  inherited;
end;

{ TPersonFullTarget }
constructor TPersonFullTarget.Create;
begin
  inherited;
  Contact := TContactDetails.Create;
  Address := TAddressInfo.Create;
  Company := TCompanyInfo.Create;
end;
destructor TPersonFullTarget.Destroy;
begin
  Contact.Free;
  Address.Free;
  Company.Free;
  inherited;
end;

{ TQuickAutoMapperTests }

function TQuickAutoMapperTests.BuildPersonFull: TPersonFull;
begin
  Result := TPersonFull.Create;
  Result.Id              := 7;
  Result.Name            := 'Jane Doe';
  Result.Age             := 34;
  Result.Active          := True;
  Result.Salary          := 88000.50;
  Result.Tags            := ['delphi', 'cloud', 'testing'];
  Result.Scores          := [95, 87, 100];
  Result.Contact.Email   := 'jane@example.com';
  Result.Contact.Phone   := '+34 600 000 001';
  Result.Address.Street  := 'Gran Via 1';
  Result.Address.City    := 'Madrid';
  Result.Address.Country := 'Spain';
  Result.Address.ZipCode := '28001';
  Result.Company.Name       := 'TechCorp';
  Result.Company.Industry   := 'Software';
  Result.Company.Employees  := 250;
  Result.Company.Revenue    := 12000000.0;
  Result.Company.Address.Street  := 'Silicon Ave 99';
  Result.Company.Address.City    := 'San Francisco';
  Result.Company.Address.Country := 'USA';
end;

// ══════════════════════════════════════════════════════════════════
//  ORIGINAL TESTS
// ══════════════════════════════════════════════════════════════════

procedure TQuickAutoMapperTests.Test_Primitives;
var src: TPrimitivesSource; tgt: TPrimitivesTarget;
begin
  src := TPrimitivesSource.Create;
  tgt := TPrimitivesTarget.Create;
  try
    TObjMapper.Map(src, tgt);
    Assert.AreEqual(src.IntField,              tgt.IntField,                       'IntField');
    Assert.AreEqual(src.StrField,              tgt.StrField,                       'StrField');
    Assert.AreEqual(src.BoolField,             tgt.BoolField,                      'BoolField');
    Assert.AreEqual(Double(src.FloatField),    Double(tgt.FloatField),  0.0001,    'FloatField');
    Assert.AreEqual(src.DateField,             tgt.DateField,                      'DateField');
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Nested;
var src: TNestedSource; tgt: TNestedTarget;
begin
  src := TNestedSource.Create;
  tgt := TNestedTarget.Create;
  try
    TObjMapper.Map(src, tgt);
    Assert.AreEqual(src.Nested.IntField, tgt.Nested.IntField);
    Assert.AreEqual(src.Nested.StrField, tgt.Nested.StrField);
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Array;
var src: TArraySource; tgt: TArrayTarget; i: Integer;
begin
  src := TArraySource.Create;
  tgt := TArrayTarget.Create;
  try
    TObjMapper.Map(src, tgt);
    Assert.AreEqual(Integer(Length(src.Values)), Integer(Length(tgt.Values)), 'Values length');
    for i := 0 to High(src.Values) do
      Assert.AreEqual(src.Values[i], tgt.Values[i]);
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_List;
var src: TListSource; tgt: TListTarget; i: Integer;
begin
  src := TListSource.Create;
  tgt := TListTarget.Create;
  try
    TObjListMapper.Map(src.List, tgt.List);
    Assert.AreEqual(Integer(src.List.Count), Integer(tgt.List.Count));
    for i := 0 to src.List.Count-1 do
      Assert.AreEqual(src.List[i].IntField, tgt.List[i].IntField);
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_AutoMapper_Interface;
var src: TPrimitivesSource; auto: IAutoMapper<TPrimitivesSource, TPrimitivesTarget>; tgt: TPrimitivesTarget;
begin
  src := TPrimitivesSource.Create;
  auto := TAutoMapper<TPrimitivesSource, TPrimitivesTarget>.Create;
  tgt := auto.Map(src);
  try
    Assert.AreEqual(src.IntField, tgt.IntField);
    Assert.AreEqual(src.StrField, tgt.StrField);
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_CustomMapping;
var src: TPrimitivesSource; tgt: TPrimitivesTarget; mapping: TCustomMapping;
begin
  src := TPrimitivesSource.Create;
  mapping := TCustomMapping.Create;
  try
    mapping.AddMap('IntField', 'FloatField');
    tgt := TMapper<TPrimitivesTarget>.Map(src, mapping);
    Assert.AreEqual<Integer>(src.IntField, Trunc(tgt.FloatField));
  finally
    src.Free; tgt.Free; mapping.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_ManualProc;
var src: TPrimitivesSource; tgt: TPrimitivesTarget;
begin
  src := TPrimitivesSource.Create;
  tgt := TMapper<TPrimitivesTarget>.Map<TPrimitivesSource>(src,
    procedure(const aSrcObj: TPrimitivesSource; const aTargetName: string; out Value: TFlexValue)
    begin
      if aTargetName = 'StrField' then Value := 'manual' else Value := TFlexValue.Empty;
    end);
  try
    Assert.AreEqual('manual', tgt.StrField);
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Empty_Nil;
var src: TPrimitivesSource; tgt: TPrimitivesTarget;
begin
  src := TPrimitivesSource.Create;
  src.StrField := '';
  tgt := TPrimitivesTarget.Create;
  try
    TObjMapper.Map(src, tgt);
    Assert.AreEqual('', tgt.StrField);
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Field_To_Property;
var src: TFieldSource; tgt: TFieldTarget;
begin
  src := TFieldSource.Create;
  tgt := TFieldTarget.Create;
  try
    TObjMapper.Map(src, tgt);
    Assert.AreEqual(src.IntValue, tgt.IntValue, 'Field to Field mapping failed for IntValue');
    Assert.AreEqual(src.StrValue, tgt.StrValue, 'Field to Field mapping failed for StrValue');
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Mixed_Fields_Properties;
var src: TMixedSource; tgt: TMixedTarget;
begin
  src := TMixedSource.Create;
  tgt := TMixedTarget.Create;
  try
    TObjMapper.Map(src, tgt);
    Assert.AreEqual(src.FieldValue, tgt.FieldValue, 'Field mapping failed');
    Assert.AreEqual(src.PropValue, tgt.PropValue, 'Property mapping failed');
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_CustomMapping_WithPath;
var src: TNestedSource; tgt: TPrimitivesTarget; mapping: TCustomMapping;
begin
  src := TNestedSource.Create;
  tgt := TPrimitivesTarget.Create;
  mapping := TCustomMapping.Create;
  try
    mapping.AddMap('IntField', 'Nested.IntField');
    mapping.AddMap('StrField', 'Nested.StrField');
    TObjMapper.Map(src, tgt, mapping);
    Assert.AreEqual(src.Nested.IntField, tgt.IntField, 'Path mapping failed for IntField');
    Assert.AreEqual(src.Nested.StrField, tgt.StrField, 'Path mapping failed for StrField');
  finally
    src.Free; tgt.Free; mapping.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_NumericConversions;
var src: TNumericSource; tgt: TNumericTarget; mapping: TCustomMapping;
begin
  src := TNumericSource.Create;
  tgt := TNumericTarget.Create;
  mapping := TCustomMapping.Create;
  try
    mapping.AddMap('IntFromDouble', 'DoubleValue');
    mapping.AddMap('DoubleFromInt', 'IntValue');
    mapping.AddMap('Int64FromExt', 'ExtValue');
    mapping.AddMap('ExtFromInt64', 'Int64Value');
    TObjMapper.Map(src, tgt, mapping);
    Assert.AreEqual<Int64>(Trunc(src.DoubleValue), tgt.IntFromDouble, 'Double to Int conversion failed');
    Assert.AreEqual<Double>(src.IntValue, tgt.DoubleFromInt, 'Int to Double conversion failed');
    Assert.AreEqual<Int64>(Int64(Trunc(src.ExtValue)), tgt.Int64FromExt, 'Extended to Int64 conversion failed');
    Assert.AreEqual<Int64>(src.Int64Value, Int64(Trunc(tgt.ExtFromInt64)), 'Int64 to Extended conversion failed');
  finally
    src.Free; tgt.Free; mapping.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Array_Primitives;
var src: TPrimitiveArraySource; tgt: TPrimitiveArrayTarget; i: Integer;
begin
  src := TPrimitiveArraySource.Create;
  tgt := TPrimitiveArrayTarget.Create;
  try
    TObjMapper.Map(src, tgt);
    Assert.AreEqual(Integer(Length(src.IntArray)), Integer(Length(tgt.IntArray)), 'IntArray length mismatch');
    Assert.AreEqual(Integer(Length(src.StrArray)), Integer(Length(tgt.StrArray)), 'StrArray length mismatch');
    for i := 0 to High(src.IntArray) do
      Assert.AreEqual(src.IntArray[i], tgt.IntArray[i], 'IntArray element mismatch');
    for i := 0 to High(src.StrArray) do
      Assert.AreEqual(src.StrArray[i], tgt.StrArray[i], 'StrArray element mismatch');
  finally
    src.Free; tgt.Free;
  end;
end;

// ══════════════════════════════════════════════════════════════════
//  COMPLEX MULTI-LEVEL TESTS
// ══════════════════════════════════════════════════════════════════

procedure TQuickAutoMapperTests.Test_Complex_MultiLevel_Serialize;
// Maps TPersonFull → TPersonFullTarget: nested Contact, Address, Company (3 levels)
var
  src : TPersonFull;
  tgt : TPersonFullTarget;
begin
  src := BuildPersonFull;
  tgt := TPersonFullTarget.Create;
  try
    TObjMapper.Map(src, tgt);
    Assert.AreEqual(src.Id,             tgt.Id,             'Id');
    Assert.AreEqual(src.Name,           tgt.Name,           'Name');
    Assert.AreEqual(src.Age,            tgt.Age,            'Age');
    Assert.AreEqual(src.Active,         tgt.Active,         'Active');
    Assert.AreEqual(Double(src.Salary), Double(tgt.Salary), 0.001, 'Salary');
    // Level 2: Contact
    Assert.IsNotNull(tgt.Contact,                           'Contact not nil');
    Assert.AreEqual(src.Contact.Email,  tgt.Contact.Email,  'Contact.Email');
    Assert.AreEqual(src.Contact.Phone,  tgt.Contact.Phone,  'Contact.Phone');
    // Level 2: Address
    Assert.IsNotNull(tgt.Address,                           'Address not nil');
    Assert.AreEqual(src.Address.Street, tgt.Address.Street, 'Address.Street');
    Assert.AreEqual(src.Address.City,   tgt.Address.City,   'Address.City');
    Assert.AreEqual(src.Address.ZipCode,tgt.Address.ZipCode,'Address.ZipCode');
    // Level 2: Company
    Assert.IsNotNull(tgt.Company,                           'Company not nil');
    Assert.AreEqual(src.Company.Name,   tgt.Company.Name,   'Company.Name');
    Assert.AreEqual(src.Company.Employees, tgt.Company.Employees, 'Company.Employees');
    // Level 3: Company.Address
    Assert.IsNotNull(tgt.Company.Address,                        'Company.Address not nil');
    Assert.AreEqual(src.Company.Address.City, tgt.Company.Address.City, 'Company.Address.City');
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Complex_MultiLevel_RoundTrip;
// src → tgt → back to another src2 via TMapper<T>.Map
var
  src  : TPersonFull;
  tgt  : TPersonFullTarget;
  src2 : TPersonFull;
begin
  src := BuildPersonFull;
  try
    tgt := TMapper<TPersonFullTarget>.Map(src);
    try
      src2 := TMapper<TPersonFull>.Map(tgt);
      try
        Assert.AreEqual(src.Id,              src2.Id,              'RT Id');
        Assert.AreEqual(src.Name,            src2.Name,            'RT Name');
        Assert.AreEqual(src.Age,             src2.Age,             'RT Age');
        Assert.AreEqual(src.Active,          src2.Active,          'RT Active');
        Assert.AreEqual(src.Contact.Email,   src2.Contact.Email,   'RT Contact.Email');
        Assert.AreEqual(src.Address.City,    src2.Address.City,    'RT Address.City');
        Assert.AreEqual(src.Company.Name,    src2.Company.Name,    'RT Company.Name');
        Assert.AreEqual(src.Company.Address.City, src2.Company.Address.City, 'RT Company.Address.City');
      finally
        src2.Free;
      end;
    finally
      tgt.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Complex_FlatDTO_CustomMapping;
// Maps deep fields of TPersonFull into a flat TPersonDTO using custom mapping paths
var
  src     : TPersonFull;
  dto     : TPersonDTO;
  mapping : TCustomMapping;
begin
  src     := BuildPersonFull;
  dto     := TPersonDTO.Create;
  mapping := TCustomMapping.Create;
  try
    mapping.AddMap('PersonId',    'Id');
    mapping.AddMap('FullName',    'Name');
    mapping.AddMap('IsActive',    'Active');
    mapping.AddMap('Email',       'Contact.Email');
    mapping.AddMap('CompanyName', 'Company.Name');
    mapping.AddMap('City',        'Address.City');
    TObjMapper.Map(src, dto, mapping);
    Assert.AreEqual(src.Id,             dto.PersonId,    'DTO PersonId');
    Assert.AreEqual(src.Name,           dto.FullName,    'DTO FullName');
    Assert.AreEqual(src.Active,         dto.IsActive,    'DTO IsActive');
    Assert.AreEqual(src.Contact.Email,  dto.Email,       'DTO Email from Contact.Email');
    Assert.AreEqual(src.Company.Name,   dto.CompanyName, 'DTO CompanyName from Company.Name');
    Assert.AreEqual(src.Address.City,   dto.City,        'DTO City from Address.City');
  finally
    src.Free; dto.Free; mapping.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Complex_RenamedFields_CustomMapping;
// Maps TPersonFull to TPersonRenamed via field renaming
var
  src     : TPersonFull;
  tgt     : TPersonRenamed;
  mapping : TCustomMapping;
begin
  src     := BuildPersonFull;
  tgt     := TPersonRenamed.Create;
  mapping := TCustomMapping.Create;
  try
    mapping.AddMap('Identifier', 'Id');
    mapping.AddMap('FullName',   'Name');
    mapping.AddMap('Employed',   'Active');
    mapping.AddMap('Wage',       'Salary');
    TObjMapper.Map(src, tgt, mapping);
    Assert.AreEqual(src.Id,     tgt.Identifier, 'Identifier ← Id');
    Assert.AreEqual(src.Name,   tgt.FullName,   'FullName ← Name');
    Assert.AreEqual(src.Active, tgt.Employed,   'Employed ← Active');
    Assert.AreEqual(Double(src.Salary), Double(tgt.Wage), 0.001, 'Wage ← Salary');
  finally
    src.Free; tgt.Free; mapping.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Complex_ManualProc_WithNestedData;
// Uses a manual mapping proc to override specific fields while auto-mapping the rest
var
  src : TPersonFull;
  tgt : TPersonFullTarget;
begin
  src := BuildPersonFull;
  try
    tgt := TMapper<TPersonFullTarget>.Map<TPersonFull>(src,
      procedure(const aSrcObj: TPersonFull; const aTargetName: string; out Value: TFlexValue)
      begin
        if aTargetName = 'Name' then Value := 'OVERRIDE'
        else if aTargetName = 'Age' then Value := TValue.From<Integer>(99)
        else Value := TFlexValue.Empty;
      end);
    try
      Assert.AreEqual('OVERRIDE', tgt.Name,  'Name overridden by manual proc');
      Assert.AreEqual(99,         tgt.Age,   'Age overridden by manual proc');
      Assert.AreEqual(src.Id,     tgt.Id,    'Id auto-mapped');
      Assert.AreEqual(src.Active, tgt.Active,'Active auto-mapped');
    finally
      tgt.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Complex_TMapper_Generic;
// TMapper<T>.Map creates and populates a new instance in one call
var
  src : TPersonFull;
  tgt : TPersonFullTarget;
begin
  src := BuildPersonFull;
  try
    tgt := TMapper<TPersonFullTarget>.Map(src);
    try
      Assert.AreEqual(src.Id,            tgt.Id,            'Id via TMapper');
      Assert.AreEqual(src.Name,          tgt.Name,          'Name via TMapper');
      Assert.AreEqual(src.Contact.Email, tgt.Contact.Email, 'Contact.Email via TMapper');
      Assert.AreEqual(src.Address.City,  tgt.Address.City,  'Address.City via TMapper');
      Assert.AreEqual(src.Company.Name,  tgt.Company.Name,  'Company.Name via TMapper');
    finally
      tgt.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Complex_StringArray_Preserved;
var
  src : TPersonFull;
  tgt : TPersonFullTarget;
  i   : Integer;
begin
  src := BuildPersonFull;
  tgt := TPersonFullTarget.Create;
  try
    TObjMapper.Map(src, tgt);
    Assert.AreEqual(Integer(Length(src.Tags)), Integer(Length(tgt.Tags)),
      'Tags array length must match');
    for i := 0 to High(src.Tags) do
      Assert.AreEqual(src.Tags[i], tgt.Tags[i],
        Format('Tags[%d] must match', [i]));
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Complex_IntArray_Preserved;
var
  src : TPersonFull;
  tgt : TPersonFullTarget;
  i   : Integer;
begin
  src := BuildPersonFull;
  tgt := TPersonFullTarget.Create;
  try
    TObjMapper.Map(src, tgt);
    Assert.AreEqual(Integer(Length(src.Scores)), Integer(Length(tgt.Scores)),
      'Scores array length must match');
    for i := 0 to High(src.Scores) do
      Assert.AreEqual(src.Scores[i], tgt.Scores[i],
        Format('Scores[%d] must match', [i]));
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Complex_AfterMapping_Callback;
// TAutoMapper.OnAfterMapping fires after the mapping and can post-process
var
  src         : TPersonFull;
  autoMapper  : TAutoMapper<TPersonFull, TPersonFullTarget>;
  tgt         : TPersonFullTarget;
  callbackFired: Boolean;
begin
  src          := BuildPersonFull;
  callbackFired := False;
  autoMapper   := TAutoMapper<TPersonFull, TPersonFullTarget>.Create;
  try
    autoMapper.OnAfterMapping :=
      procedure(const aSrcObj: TPersonFull; aTgtObj: TPersonFullTarget)
      begin
        callbackFired    := True;
        aTgtObj.Name     := aTgtObj.Name + ' (mapped)';
      end;
    tgt := autoMapper.Map(src);
    try
      Assert.IsTrue(callbackFired,                          'OnAfterMapping must fire');
      Assert.IsTrue(tgt.Name.EndsWith('(mapped)'),          'OnAfterMapping must be able to post-process');
      Assert.AreEqual(src.Id,     tgt.Id,                   'Id still correct after callback');
      Assert.AreEqual(src.Active, tgt.Active,               'Active still correct after callback');
    finally
      tgt.Free;
    end;
  finally
    src.Free;
    autoMapper.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Complex_NilNestedObject_Handled;
// Source has nil for Inner; mapper must not raise an AV
var
  src : TOuterSrc;
  tgt : TOuterTgt;
begin
  src := TOuterSrc.Create;
  tgt := TOuterTgt.Create;
  try
    src.Id    := 99;
    src.Inner := nil;
    Assert.WillNotRaise(
      procedure begin TObjMapper.Map(src, tgt); end,
      nil, 'Mapping with nil nested object must not raise');
    Assert.AreEqual(99, tgt.Id, 'Id must be mapped even when Inner is nil');
  finally
    src.Free; tgt.Free;
  end;
end;

// ══════════════════════════════════════════════════════════════════
//  ADDITIONAL COVERAGE TESTS
// ══════════════════════════════════════════════════════════════════

procedure TQuickAutoMapperTests.Test_ObjListMapper_WithCustomMapping;
// TObjListMapper.Map with a TCustomMapping that maps IntField ← FloatField
var
  src     : TListSource;
  tgt     : TListTarget;
  mapping : TCustomMapping;
begin
  src     := TListSource.Create;
  tgt     := TListTarget.Create;
  mapping := TCustomMapping.Create;
  try
    mapping.AddMap('FloatField', 'IntField');
    TObjListMapper.Map(src.List, tgt.List, mapping);
    Assert.AreEqual(Integer(src.List.Count), Integer(tgt.List.Count), 'List count with custom mapping');
    Assert.AreEqual(Double(src.List[0].IntField), Double(tgt.List[0].FloatField), 0.001,
      'IntField → FloatField via list custom mapping');
  finally
    src.Free; tgt.Free; mapping.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_AutoMapper_CustomMapping_Property;
// TAutoMapper.CustomMapping renames a field before mapping
var
  src        : TPrimitivesSource;
  autoMapper : TAutoMapper<TPrimitivesSource, TPrimitivesTarget>;
  tgt        : TPrimitivesTarget;
  mapping    : TCustomMapping;
begin
  src        := TPrimitivesSource.Create;
  mapping    := TCustomMapping.Create;
  autoMapper := TAutoMapper<TPrimitivesSource, TPrimitivesTarget>.Create;
  try
    mapping.AddMap('FloatField', 'IntField');
    autoMapper.CustomMapping := mapping;
    tgt := autoMapper.Map(src);
    try
      Assert.AreEqual(Double(src.IntField), Double(tgt.FloatField), 0.001,
        'FloatField set from IntField via TAutoMapper.CustomMapping');
    finally
      tgt.Free;
    end;
  finally
    src.Free;
    autoMapper.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_AutoMapper_ReverseMap;
// IAutoMapper.Map(TClass2) → TClass1  (reverse direction)
var
  autoMapper : TAutoMapper<TPrimitivesSource, TPrimitivesTarget>;
  src        : TPrimitivesTarget;
  tgt        : TPrimitivesSource;
begin
  autoMapper := TAutoMapper<TPrimitivesSource, TPrimitivesTarget>.Create;
  src        := TPrimitivesTarget.Create;
  try
    src.IntField  := 77;
    src.StrField  := 'reverse';
    src.BoolField := False;
    tgt := autoMapper.Map(src);
    try
      Assert.AreEqual(src.IntField,  tgt.IntField,  'Reverse IntField');
      Assert.AreEqual(src.StrField,  tgt.StrField,  'Reverse StrField');
      Assert.AreEqual(src.BoolField, tgt.BoolField, 'Reverse BoolField');
    finally
      tgt.Free;
    end;
  finally
    src.Free;
    autoMapper.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Mapper_IgnoresExtraTargetFields;
// Target has fields with no match on source; mapper must not raise
var
  src : TPrimitivesSource;
  tgt : TNumericTarget;
begin
  src := TPrimitivesSource.Create;
  tgt := TNumericTarget.Create;
  try
    Assert.WillNotRaise(
      procedure begin TObjMapper.Map(src, tgt); end,
      nil, 'Mapping with unmatched target fields must not raise');
  finally
    src.Free; tgt.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_DeepNested_ThreeLevel;
// Verifies that auto-mapping a 3-level object graph copies all levels
var
  src : TPersonFull;
  tgt : TPersonFullTarget;
begin
  src := BuildPersonFull;
  tgt := TPersonFullTarget.Create;
  try
    TObjMapper.Map(src, tgt);
    // Level 1
    Assert.AreEqual(src.Id,   tgt.Id,   'L1 Id');
    Assert.AreEqual(src.Name, tgt.Name, 'L1 Name');
    // Level 2
    Assert.AreEqual(src.Company.Name,      tgt.Company.Name,      'L2 Company.Name');
    Assert.AreEqual(src.Company.Industry,  tgt.Company.Industry,  'L2 Company.Industry');
    Assert.AreEqual(src.Company.Employees, tgt.Company.Employees, 'L2 Company.Employees');
    Assert.AreEqual(Double(src.Company.Revenue), Double(tgt.Company.Revenue), 0.01,
      'L2 Company.Revenue');
    // Level 3
    Assert.AreEqual(src.Company.Address.Street,  tgt.Company.Address.Street,  'L3 Street');
    Assert.AreEqual(src.Company.Address.City,    tgt.Company.Address.City,    'L3 City');
    Assert.AreEqual(src.Company.Address.Country, tgt.Company.Address.Country, 'L3 Country');
  finally
    src.Free; tgt.Free;
  end;
end;

// ════════════════════════════════════════════════════════════════
//  NESTED COLLECTION TESTS
// ════════════════════════════════════════════════════════════════

procedure TQuickAutoMapperTests.Test_NestedObjectList_AutoMap;
// Object with a TObjectList<TOrderLine> nested field auto-maps to
// a target with TObjectList<TOrderLineDst> — verifies deep list copy
var
  src : TOrderSrc;
  dst : TOrderDst;
  line: TOrderLine;
begin
  src := TOrderSrc.Create;
  dst := TOrderDst.Create;
  try
    src.OrderId  := 101;
    src.Customer := 'ACME';
    line := TOrderLine.Create;
    line.ProductName := 'Widget A'; line.Quantity := 3; line.UnitPrice := 9.99;
    src.Lines.Add(line);
    line := TOrderLine.Create;
    line.ProductName := 'Widget B'; line.Quantity := 1; line.UnitPrice := 49.95;
    src.Lines.Add(line);

    TObjMapper.Map(src, dst);

    Assert.AreEqual(src.OrderId,  dst.OrderId,  'OrderId');
    Assert.AreEqual(src.Customer, dst.Customer, 'Customer');
    Assert.AreEqual(2, Integer(dst.Lines.Count), 'Lines.Count');
    Assert.AreEqual('Widget A', dst.Lines[0].ProductName, 'Lines[0].ProductName');
    Assert.AreEqual(3,          dst.Lines[0].Quantity,    'Lines[0].Quantity');
    Assert.AreEqual(Double(9.99),  Double(dst.Lines[0].UnitPrice), 0.001, 'Lines[0].UnitPrice');
    Assert.AreEqual('Widget B', dst.Lines[1].ProductName, 'Lines[1].ProductName');
    Assert.AreEqual(1,          dst.Lines[1].Quantity,    'Lines[1].Quantity');
    Assert.AreEqual(Double(49.95), Double(dst.Lines[1].UnitPrice), 0.001, 'Lines[1].UnitPrice');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_NestedObjectList_RoundTrip;
// src → dst → src2  via TMapper<T>.Map  (round-trip through nested list)
var
  src  : TOrderSrc;
  dst  : TOrderDst;
  src2 : TOrderSrc;
  line : TOrderLine;
begin
  src := TOrderSrc.Create;
  try
    src.OrderId  := 202;
    src.Customer := 'Round-Trip Corp';
    line := TOrderLine.Create;
    line.ProductName := 'Part X'; line.Quantity := 5; line.UnitPrice := 3.50;
    src.Lines.Add(line);

    dst := TMapper<TOrderDst>.Map(src);
    try
      src2 := TMapper<TOrderSrc>.Map(dst);
      try
        Assert.AreEqual(src.OrderId,                   src2.OrderId,  'RT OrderId');
        Assert.AreEqual(src.Customer,                  src2.Customer, 'RT Customer');
        Assert.AreEqual(1, Integer(src2.Lines.Count), 'RT Lines.Count');
        Assert.AreEqual(src.Lines[0].ProductName,      src2.Lines[0].ProductName, 'RT ProductName');
        Assert.AreEqual(src.Lines[0].Quantity,         src2.Lines[0].Quantity,    'RT Quantity');
        Assert.AreEqual(Double(src.Lines[0].UnitPrice),
                        Double(src2.Lines[0].UnitPrice), 0.001,                   'RT UnitPrice');
      finally
        src2.Free;
      end;
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_NestedObjectList_WithManualProc;
// ManualProc overrides scalar fields while the nested ObjectList is still auto-mapped
var
  src  : TOrderSrc;
  dst  : TOrderDst;
  line : TOrderLine;
begin
  src := TOrderSrc.Create;
  try
    src.OrderId  := 303;
    src.Customer := 'OriginalCorp';
    line := TOrderLine.Create;
    line.ProductName := 'Item 1'; line.Quantity := 2; line.UnitPrice := 5.0;
    src.Lines.Add(line);

    dst := TMapper<TOrderDst>.Map<TOrderSrc>(src,
      procedure(const aSrc: TOrderSrc; const aTargetName: string; out Value: TFlexValue)
      begin
        if aTargetName = 'Customer' then Value := 'OverriddenCorp'
        else Value := TFlexValue.Empty;
      end);
    try
      Assert.AreEqual('OverriddenCorp', dst.Customer, 'Customer overridden by manual proc');
      Assert.AreEqual(src.OrderId,      dst.OrderId,  'OrderId auto-mapped');
      Assert.AreEqual(1, Integer(dst.Lines.Count), 'Lines still auto-mapped');
      Assert.AreEqual('Item 1', dst.Lines[0].ProductName, 'ProductName from nested list');
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_NestedListString_AutoMap;
// TList<string> nested inside an object is auto-mapped element by element
var
  src : TTagsSrc;
  dst : TTagsDst;
begin
  src := TTagsSrc.Create;
  dst := TTagsDst.Create;
  try
    src.Name := 'ListStrTest';
    src.Tags.Add('alpha');
    src.Tags.Add('beta');
    src.Tags.Add('gamma');

    TObjMapper.Map(src, dst);

    Assert.AreEqual('ListStrTest', dst.Name,       'Name');
    Assert.AreEqual(3, Integer(dst.Tags.Count), 'Tags.Count');
    Assert.AreEqual('alpha',       dst.Tags[0],    'Tags[0]');
    Assert.AreEqual('beta',        dst.Tags[1],    'Tags[1]');
    Assert.AreEqual('gamma',       dst.Tags[2],    'Tags[2]');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_NestedListInteger_AutoMap;
// TList<Integer> nested inside an object is auto-mapped element by element
var
  src : TTagsSrc;
  dst : TTagsDst;
begin
  src := TTagsSrc.Create;
  dst := TTagsDst.Create;
  try
    src.Name := 'ListIntTest';
    src.Scores.Add(10);
    src.Scores.Add(20);
    src.Scores.Add(30);

    TObjMapper.Map(src, dst);

    Assert.AreEqual('ListIntTest', dst.Name,         'Name');
    Assert.AreEqual(3, Integer(dst.Scores.Count), 'Scores.Count');
    Assert.AreEqual(10,            dst.Scores[0],    'Scores[0]');
    Assert.AreEqual(20,            dst.Scores[1],    'Scores[1]');
    Assert.AreEqual(30,            dst.Scores[2],    'Scores[2]');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_NestedListString_RoundTrip;
// TList<string> round-trip via TMapper<T>.Map
var
  src  : TTagsSrc;
  dst  : TTagsDst;
  src2 : TTagsSrc;
begin
  src := TTagsSrc.Create;
  try
    src.Name := 'RT-Tags';
    src.Tags.Add('x');
    src.Tags.Add('y');
    src.Scores.Add(1);
    src.Scores.Add(2);
    src.Scores.Add(3);

    dst := TMapper<TTagsDst>.Map(src);
    try
      src2 := TMapper<TTagsSrc>.Map(dst);
      try
        Assert.AreEqual(src.Name,         src2.Name,         'RT Name');
        Assert.AreEqual(Integer(src.Tags.Count),   Integer(src2.Tags.Count),   'RT Tags.Count');
        Assert.AreEqual(src.Tags[0],      src2.Tags[0],      'RT Tags[0]');
        Assert.AreEqual(src.Tags[1],      src2.Tags[1],      'RT Tags[1]');
        Assert.AreEqual(Integer(src.Scores.Count), Integer(src2.Scores.Count), 'RT Scores.Count');
        Assert.AreEqual(src.Scores[0],    src2.Scores[0],    'RT Scores[0]');
        Assert.AreEqual(src.Scores[2],    src2.Scores[2],    'RT Scores[2]');
      finally
        src2.Free;
      end;
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Mixed_ObjectListAndArrays;
// Object that has: TObjectList<T>, TArray<string>, TArray<Integer> — all mapped together
var
  src  : TCatalogSrc;
  dst  : TCatalogDst;
  prod : TProductSrc;
begin
  src := TCatalogSrc.Create;
  dst := TCatalogDst.Create;
  try
    src.Title := 'MixedCatalog';
    prod := TProductSrc.Create;
    prod.Code := 'SKU-001'; prod.Price := 19.99; src.Products.Add(prod);
    prod := TProductSrc.Create;
    prod.Code := 'SKU-002'; prod.Price := 5.49;  src.Products.Add(prod);
    src.Tags := ['electronics', 'sale'];
    src.Ids  := [1001, 1002, 1003];

    TObjMapper.Map(src, dst);

    Assert.AreEqual('MixedCatalog', dst.Title, 'Title');
    // ObjectList
    Assert.AreEqual(2, Integer(dst.Products.Count), 'Products.Count');
    Assert.AreEqual('SKU-001', dst.Products[0].Code, 'Products[0].Code');
    Assert.AreEqual(Double(19.99), Double(dst.Products[0].Price), 0.001, 'Products[0].Price');
    Assert.AreEqual('SKU-002', dst.Products[1].Code, 'Products[1].Code');
    // TArray<string>
    Assert.AreEqual(2, Integer(Length(dst.Tags)), 'Tags.Length');
    Assert.AreEqual('electronics', dst.Tags[0], 'Tags[0]');
    Assert.AreEqual('sale',        dst.Tags[1], 'Tags[1]');
    // TArray<Integer>
    Assert.AreEqual(3, Integer(Length(dst.Ids)), 'Ids.Length');
    Assert.AreEqual(1001, dst.Ids[0], 'Ids[0]');
    Assert.AreEqual(1002, dst.Ids[1], 'Ids[1]');
    Assert.AreEqual(1003, dst.Ids[2], 'Ids[2]');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_NestedObjectList_CustomMapping;
// Custom mapping renames a scalar field while the nested ObjectList auto-maps
var
  src     : TOrderSrc;
  dst     : TOrderDst;
  mapping : TCustomMapping;
  line    : TOrderLine;
begin
  src     := TOrderSrc.Create;
  dst     := TOrderDst.Create;
  mapping := TCustomMapping.Create;
  try
    mapping.AddMap('OrderId', 'OrderId'); // identity, just to prove custom+list coexist
    src.OrderId  := 55;
    src.Customer := 'Mapped Corp';
    line := TOrderLine.Create;
    line.ProductName := 'Gear'; line.Quantity := 7; line.UnitPrice := 12.00;
    src.Lines.Add(line);

    TObjMapper.Map(src, dst, mapping);

    Assert.AreEqual(55,            dst.OrderId,  'OrderId via custom mapping');
    Assert.AreEqual('Mapped Corp', dst.Customer, 'Customer auto-mapped');
    Assert.AreEqual(1, Integer(dst.Lines.Count), 'Lines.Count with custom mapping active');
    Assert.AreEqual('Gear', dst.Lines[0].ProductName, 'Nested list item ProductName');
    Assert.AreEqual(7,      dst.Lines[0].Quantity,    'Nested list item Quantity');
  finally
    src.Free; dst.Free; mapping.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_NestedObjectList_Empty;
// Empty TObjectList maps to empty TObjectList — no crash, zero elements
var
  src : TOrderSrc;
  dst : TOrderDst;
begin
  src := TOrderSrc.Create;
  dst := TOrderDst.Create;
  try
    src.OrderId  := 1;
    src.Customer := 'Empty';
    // Lines intentionally left empty

    Assert.WillNotRaise(
      procedure begin TObjMapper.Map(src, dst); end,
      nil, 'Mapping empty nested ObjectList must not raise');
    Assert.AreEqual(0, Integer(dst.Lines.Count), 'Dst Lines must be empty');
  finally
    src.Free; dst.Free;
  end;
end;

// ════════════════════════════════════════════════════════════════
//  ADVANCED / HARDENING TESTS
// ════════════════════════════════════════════════════════════════

procedure TQuickAutoMapperTests.Test_NestedListDouble_AutoMap;
// TList<Double> nested inside an object maps element by element
var
  src : TMetricsSrc;
  dst : TMetricsDst;
begin
  src := TMetricsSrc.Create;
  dst := TMetricsDst.Create;
  try
    src.Caption := 'temps';
    src.Values.Add(36.6); src.Values.Add(37.1); src.Values.Add(38.0);
    TObjMapper.Map(src, dst);
    Assert.AreEqual('temps', dst.Caption, 'Caption');
    Assert.AreEqual(3, Integer(dst.Values.Count), 'Values.Count');
    Assert.AreEqual(Double(36.6), Double(dst.Values[0]), 0.001, 'Values[0]');
    Assert.AreEqual(Double(37.1), Double(dst.Values[1]), 0.001, 'Values[1]');
    Assert.AreEqual(Double(38.0), Double(dst.Values[2]), 0.001, 'Values[2]');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_NestedListBoolean_AutoMap;
// TList<Boolean> nested inside an object maps element by element
var
  src : TMetricsSrc;
  dst : TMetricsDst;
begin
  src := TMetricsSrc.Create;
  dst := TMetricsDst.Create;
  try
    src.Caption := 'flags';
    src.Flags.Add(True); src.Flags.Add(False); src.Flags.Add(True);
    TObjMapper.Map(src, dst);
    Assert.AreEqual(3, Integer(dst.Flags.Count), 'Flags.Count');
    Assert.IsTrue (dst.Flags[0], 'Flags[0] = True');
    Assert.IsFalse(dst.Flags[1], 'Flags[1] = False');
    Assert.IsTrue (dst.Flags[2], 'Flags[2] = True');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_TwoObjectLists_SameObject;
// Object containing two independent TObjectList fields — both mapped correctly
var
  src  : TInvoiceSrc;
  dst  : TInvoiceDst;
  line : TInvoiceLine;
  pay  : TPaymentLine;
begin
  src := TInvoiceSrc.Create;
  dst := TInvoiceDst.Create;
  try
    src.Number := 'INV-001';
    line := TInvoiceLine.Create;
    line.Description := 'Widget'; line.Amount := 100.0;
    src.Lines.Add(line);
    line := TInvoiceLine.Create;
    line.Description := 'Gadget'; line.Amount := 250.0;
    src.Lines.Add(line);
    pay := TPaymentLine.Create;
    pay.Method := 'Card'; pay.Paid := 350.0;
    src.Payments.Add(pay);

    TObjMapper.Map(src, dst);

    Assert.AreEqual('INV-001', dst.Number, 'Number');
    Assert.AreEqual(2, Integer(dst.Lines.Count),    'Lines.Count');
    Assert.AreEqual(1, Integer(dst.Payments.Count), 'Payments.Count');
    Assert.AreEqual('Widget', dst.Lines[0].Description,      'Lines[0].Description');
    Assert.AreEqual(Double(100.0), Double(dst.Lines[0].Amount),   0.001, 'Lines[0].Amount');
    Assert.AreEqual('Gadget', dst.Lines[1].Description,      'Lines[1].Description');
    Assert.AreEqual('Card',   dst.Payments[0].Method,        'Payments[0].Method');
    Assert.AreEqual(Double(350.0), Double(dst.Payments[0].Paid), 0.001, 'Payments[0].Paid');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_InheritedFields_Mapped;
// Fields declared in the base class must also be mapped
var
  src : TDerivedSrc;
  dst : TDerivedDst;
begin
  src := TDerivedSrc.Create;
  dst := TDerivedDst.Create;
  try
    TObjMapper.Map(src, dst);
    // Base-class fields
    Assert.AreEqual(src.Id,        dst.Id,        'Inherited Id');
    Assert.AreEqual(src.CreatedAt, dst.CreatedAt, 'Inherited CreatedAt');
    // Descendant fields
    Assert.AreEqual(src.Name,                       dst.Name,  'Name');
    Assert.AreEqual(Double(src.Score), Double(dst.Score), 0.001, 'Score');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_AllZeroFields_Mapped;
// Source with all zero/false/empty values: mapper must not skip them
var
  src : TZeroSrc;
  dst : TZeroDst;
begin
  src := TZeroSrc.Create;
  dst := TZeroDst.Create;
  try
    // pre-fill dst with non-zero sentinel values to detect if they get overwritten
    dst.IntVal  := 999;
    dst.StrVal  := 'NOT_EMPTY';
    dst.BoolVal := True;
    dst.DblVal  := -1.0;

    TObjMapper.Map(src, dst);

    Assert.AreEqual(0,   dst.IntVal,  'IntVal must be 0');
    Assert.AreEqual('',  dst.StrVal,  'StrVal must be empty');
    Assert.IsFalse(dst.BoolVal,       'BoolVal must be False');
    Assert.AreEqual(Double(0.0), Double(dst.DblVal), 0.0001, 'DblVal must be 0.0');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_Int64_Integer_Fields;
// Int64 and Integer fields with matching names are mapped without loss
var
  src : TWideSrc;
  dst : TWideDst;
begin
  src := TWideSrc.Create;
  dst := TWideDst.Create;
  try
    TObjMapper.Map(src, dst);
    Assert.AreEqual(src.BigId,    dst.BigId,    'BigId (Int64)');
    Assert.AreEqual(src.SmallVal, dst.SmallVal, 'SmallVal (Integer)');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_ThreeLevel_ListWithNestedObject;
// 3-level graph: owner → TObjectList<TDeepLine> → TDeepLine.Detail (TItemDetail)
var
  src  : TDeepOrderSrc;
  dst  : TDeepOrderDst;
  line : TDeepLine;
begin
  src := TDeepOrderSrc.Create;
  dst := TDeepOrderDst.Create;
  try
    src.Code := 'ORD-3L';
    line := TDeepLine.Create;
    line.Ref          := 'REF-A';
    line.Detail.Sku   := 'SKU-001';
    line.Detail.Stock := 5;
    src.Lines.Add(line);
    line := TDeepLine.Create;
    line.Ref          := 'REF-B';
    line.Detail.Sku   := 'SKU-002';
    line.Detail.Stock := 12;
    src.Lines.Add(line);

    TObjMapper.Map(src, dst);

    Assert.AreEqual('ORD-3L', dst.Code, 'Code');
    Assert.AreEqual(2,        Integer(dst.Lines.Count), 'Lines.Count');
    // Level 2
    Assert.AreEqual('REF-A', dst.Lines[0].Ref, 'Lines[0].Ref');
    Assert.AreEqual('REF-B', dst.Lines[1].Ref, 'Lines[1].Ref');
    // Level 3: Detail nested inside list element
    Assert.IsNotNull(dst.Lines[0].Detail,                          'Lines[0].Detail not nil');
    Assert.AreEqual('SKU-001', dst.Lines[0].Detail.Sku,   'Lines[0].Detail.Sku');
    Assert.AreEqual(5,         dst.Lines[0].Detail.Stock, 'Lines[0].Detail.Stock');
    Assert.AreEqual('SKU-002', dst.Lines[1].Detail.Sku,   'Lines[1].Detail.Sku');
    Assert.AreEqual(12,        dst.Lines[1].Detail.Stock, 'Lines[1].Detail.Stock');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_LargeObjectList_Stress;
// Maps a TObjectList with 100 elements — checks no element is lost or corrupted
var
  src  : TBigListSrc;
  dst  : TBigListDst;
  item : TSimpleItem;
  i    : Integer;
begin
  src := TBigListSrc.Create;
  dst := TBigListDst.Create;
  try
    src.Title := 'BigList';
    for i := 0 to 99 do
    begin
      item := TSimpleItem.Create;
      item.Index := i;
      item.Name  := Format('Item_%d', [i]);
      src.Items.Add(item);
    end;

    TObjMapper.Map(src, dst);

    Assert.AreEqual('BigList', dst.Title,                    'Title');
    Assert.AreEqual(100, Integer(dst.Items.Count),           'Items.Count = 100');
    Assert.AreEqual(0,   dst.Items[0].Index,                 'Items[0].Index');
    Assert.AreEqual('Item_0',  dst.Items[0].Name,            'Items[0].Name');
    Assert.AreEqual(99,  dst.Items[99].Index,                'Items[99].Index');
    Assert.AreEqual('Item_99', dst.Items[99].Name,           'Items[99].Name');
    Assert.AreEqual(50,  dst.Items[50].Index,                'Items[50].Index (mid)');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_MultipleRenames_CustomMapping;
// CustomMapping with 4 renames all applied in a single Map call
var
  src     : TRichSrc;
  dst     : TRichDst;
  mapping : TCustomMapping;
begin
  src     := TRichSrc.Create;
  dst     := TRichDst.Create;
  mapping := TCustomMapping.Create;
  try
    mapping.AddMap('GivenName',   'FirstName');
    mapping.AddMap('FamilyName',  'LastName');
    mapping.AddMap('YearOfBirth', 'BirthYear');
    mapping.AddMap('Enabled',     'Active');
    TObjMapper.Map(src, dst, mapping);
    Assert.AreEqual(src.FirstName, dst.GivenName,   'GivenName ← FirstName');
    Assert.AreEqual(src.LastName,  dst.FamilyName,  'FamilyName ← LastName');
    Assert.AreEqual(src.BirthYear, dst.YearOfBirth, 'YearOfBirth ← BirthYear');
    Assert.AreEqual(src.Active,    dst.Enabled,     'Enabled ← Active');
  finally
    src.Free; dst.Free; mapping.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_OnDoMapping_ViaInterface;
// TAutoMapper.OnDoMapping intercepts every field; we override Y and leave others auto
var
  src        : TAutoMapSrc;
  autoMapper : TAutoMapper<TAutoMapSrc, TAutoMapDst>;
  dst        : TAutoMapDst;
begin
  src        := TAutoMapSrc.Create;
  autoMapper := TAutoMapper<TAutoMapSrc, TAutoMapDst>.Create;
  try
    autoMapper.OnDoMapping :=
      procedure(const aSrcObj: TAutoMapSrc; const aTargetName: string; out Value: TFlexValue)
      begin
        if aTargetName = 'Y' then Value := TValue.From<Integer>(999)
        else Value := TFlexValue.Empty; // let auto-map handle the rest
      end;
    dst := autoMapper.Map(src);
    try
      Assert.AreEqual(10,  dst.X, 'X auto-mapped');
      Assert.AreEqual(999, dst.Y, 'Y overridden by OnDoMapping');
      Assert.AreEqual(30,  dst.Z, 'Z auto-mapped');
    finally
      dst.Free;
    end;
  finally
    src.Free; autoMapper.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_ManualProc_OverridesOnlySpecified;
// ManualProc returning Empty for un-handled fields must NOT zero them out
var
  src : TPrimitivesSource;
  dst : TPrimitivesTarget;
begin
  src := TPrimitivesSource.Create; // IntField=42, StrField='hello', BoolField=True
  try
    dst := TMapper<TPrimitivesTarget>.Map<TPrimitivesSource>(src,
      procedure(const aSrcObj: TPrimitivesSource; const aTargetName: string; out Value: TFlexValue)
      begin
        // Only override BoolField; leave everything else to auto-mapping
        if aTargetName = 'BoolField' then Value := TValue.From<Boolean>(False)
        else Value := TFlexValue.Empty;
      end);
    try
      Assert.AreEqual(42,      dst.IntField,  'IntField must still be auto-mapped to 42');
      Assert.AreEqual('hello', dst.StrField,  'StrField must still be auto-mapped');
      Assert.IsFalse(dst.BoolField,           'BoolField must be overridden to False');
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_EmptyStringArray_NoRaise;
// Mapping a source with a nil/empty TArray<string> must not raise
var
  src : TPrimitiveArraySource;
  dst : TPrimitiveArrayTarget;
begin
  src := TPrimitiveArraySource.Create;
  dst := TPrimitiveArrayTarget.Create;
  try
    src.StrArray := [];  // empty array
    src.IntArray := [];
    Assert.WillNotRaise(
      procedure begin TObjMapper.Map(src, dst); end,
      nil, 'Mapping empty TArray<string> must not raise');
    Assert.AreEqual(0, Integer(Length(dst.StrArray)), 'StrArray must be empty');
    Assert.AreEqual(0, Integer(Length(dst.IntArray)), 'IntArray must be empty');
  finally
    src.Free; dst.Free;
  end;
end;

procedure TQuickAutoMapperTests.Test_BoolField_FalseNotLostAfterMap;
// Boolean field set to False must survive the mapping (regression: 0 might be skipped)
var
  src : TPrimitivesSource;
  dst : TPrimitivesTarget;
begin
  src := TPrimitivesSource.Create;
  dst := TPrimitivesTarget.Create;
  try
    src.BoolField := False;
    TObjMapper.Map(src, dst);
    Assert.IsFalse(dst.BoolField, 'BoolField = False must be preserved after mapping');
  finally
    src.Free; dst.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickAutoMapperTests);

end.
