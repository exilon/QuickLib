unit Quick.RTTI.Utils.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Diagnostics,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  RTTI,
  Quick.RTTI.Utils;

type
  // Simple test classes

  TAddressInfo = class
  private
    fCity: string;
    fZip: string;
  published
    property City: string read fCity write fCity;
    property Zip: string read fZip write fZip;
  end;

  TPersonBase = class
  private
    fId: Integer;
  published
    property Id: Integer read fId write fId;
  end;

  TPerson = class(TPersonBase)
  private
    fName: string;
    fAge: Integer;
    fSalary: Double;
    fActive: Boolean;
    fAddress: TAddressInfo;
  public
    constructor Create;
    destructor Destroy; override;
    function FullName: string;
  published
    property Name: string read fName write fName;
    property Age: Integer read fAge write fAge;
    property Salary: Double read fSalary write fSalary;
    property Active: Boolean read fActive write fActive;
    property Address: TAddressInfo read fAddress write fAddress;
  end;

  [TestFixture]
  TQuickRTTIUtilsTests = class(TObject)
  private
    fPerson: TPerson;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // PropertyExists
    [Test]
    procedure Test_PropertyExists_ExistingProperty_ReturnsTrue;
    [Test]
    procedure Test_PropertyExists_NonExistingProperty_ReturnsFalse;

    // GetPropertyValue / SetPropertyValue
    [Test]
    procedure Test_GetPropertyValue_String;
    [Test]
    procedure Test_GetPropertyValue_Integer;
    [Test]
    procedure Test_GetPropertyValue_Float;
    [Test]
    procedure Test_GetPropertyValue_Boolean;
    [Test]
    procedure Test_SetPropertyValue_String;
    [Test]
    procedure Test_SetPropertyValue_Integer;
    [Test]
    procedure Test_SetPropertyValue_Float;

    // GetPropertyValueEx
    [Test]
    procedure Test_GetPropertyValueEx_String;
    [Test]
    procedure Test_GetPropertyValueEx_Integer;

    // GetPathValue / SetPathValue
    [Test]
    procedure Test_GetPathValue_SimpleProperty;
    [Test]
    procedure Test_GetPathValue_NestedProperty;
    [Test]
    procedure Test_SetPathValue_SimpleProperty;
    [Test]
    procedure Test_SetPathValue_NestedProperty;

    // PathExists
    [Test]
    procedure Test_PathExists_SimpleProperty_ReturnsTrue;
    [Test]
    procedure Test_PathExists_NestedProperty_ReturnsTrue;
    [Test]
    procedure Test_PathExists_NonExisting_ReturnsFalse;
    [Test]
    procedure Test_PathExists_NilInstance_ReturnsFalse;

    // GetProperty
    [Test]
    procedure Test_GetProperty_ReturnsCorrectRttiProperty;
    [Test]
    procedure Test_GetProperty_NonExisting_ReturnsNil;

    // TryGetProperty / TryGetMethod
    [Test]
    procedure Test_TryGetProperty_Existing_ReturnsTrue;
    [Test]
    procedure Test_TryGetProperty_NonExisting_ReturnsFalse;
    [Test]
    procedure Test_TryGetProperty_CaseInsensitive_ReturnsTrue;
    [Test]
    procedure Test_TryGetMethod_Existing_ReturnsTrue;
    [Test]
    procedure Test_TryGetMethod_NonExisting_ReturnsFalse;

    // Benchmark (Phase D)
    [Test]
    procedure Test_ZZ_Benchmark_RttiCache;

    // GetProperties (inheritance order)
    [Test]
    procedure Test_GetProperties_FirstBase_BasePropertiesFirst;
    [Test]
    procedure Test_GetProperties_FirstInherited_DerivedPropertiesFirst;

    // GetType
    [Test]
    procedure Test_GetType_ReturnsCorrectType;

    // CreateInstance
    [Test]
    procedure Test_CreateInstance_Generic;
    [Test]
    procedure Test_CreateInstance_ByClass;

    // FindClass
    [Test]
    procedure Test_FindClass_KnownClass_ReturnsClass;
    [Test]
    procedure Test_FindClass_UnknownClass_ReturnsNil;
  end;

implementation

{ TPerson }

constructor TPerson.Create;
begin
  inherited Create;
  fAddress := TAddressInfo.Create;
end;

destructor TPerson.Destroy;
begin
  fAddress.Free;
  inherited;
end;

function TPerson.FullName: string;
begin
  Result := Name;
end;

{ TQuickRTTIUtilsTests }

procedure TQuickRTTIUtilsTests.SetUp;
begin
  fPerson := TPerson.Create;
  fPerson.Id := 1;
  fPerson.Name := 'Alice';
  fPerson.Age := 30;
  fPerson.Salary := 75000.50;
  fPerson.Active := True;
  fPerson.Address.City := 'Madrid';
  fPerson.Address.Zip := '28001';
end;

procedure TQuickRTTIUtilsTests.TearDown;
begin
  fPerson.Free;
end;

{ PropertyExists }

procedure TQuickRTTIUtilsTests.Test_PropertyExists_ExistingProperty_ReturnsTrue;
begin
  Assert.IsTrue(TRTTI.PropertyExists(TypeInfo(TPerson), 'Name'), 'Name property should exist');
  Assert.IsTrue(TRTTI.PropertyExists(TypeInfo(TPerson), 'Age'), 'Age property should exist');
  Assert.IsTrue(TRTTI.PropertyExists(TypeInfo(TPerson), 'Active'), 'Active property should exist');
end;

procedure TQuickRTTIUtilsTests.Test_PropertyExists_NonExistingProperty_ReturnsFalse;
begin
  Assert.IsFalse(TRTTI.PropertyExists(TypeInfo(TPerson), 'NonExisting'), 'NonExisting property should not exist');
  Assert.IsFalse(TRTTI.PropertyExists(TypeInfo(TPerson), ''), 'Empty property name should not exist');
end;

{ GetPropertyValue / SetPropertyValue }

procedure TQuickRTTIUtilsTests.Test_GetPropertyValue_String;
begin
  Assert.AreEqual('Alice', TRTTI.GetPropertyValue(fPerson, 'Name').AsString, 'Should return Name value');
end;

procedure TQuickRTTIUtilsTests.Test_GetPropertyValue_Integer;
begin
  Assert.AreEqual(30, TRTTI.GetPropertyValue(fPerson, 'Age').AsInteger, 'Should return Age value');
end;

procedure TQuickRTTIUtilsTests.Test_GetPropertyValue_Float;
begin
  Assert.AreEqual(Double(75000.50), Double(TRTTI.GetPropertyValue(fPerson, 'Salary').AsExtended), 'Should return Salary value');
end;

procedure TQuickRTTIUtilsTests.Test_GetPropertyValue_Boolean;
begin
  Assert.IsTrue(TRTTI.GetPropertyValue(fPerson, 'Active').AsBoolean, 'Should return Active value');
end;

procedure TQuickRTTIUtilsTests.Test_SetPropertyValue_String;
begin
  TRTTI.SetPropertyValue(fPerson, 'Name', TValue.From<string>('Bob'));
  Assert.AreEqual('Bob', fPerson.Name, 'Name should be updated to Bob');
end;

procedure TQuickRTTIUtilsTests.Test_SetPropertyValue_Integer;
begin
  TRTTI.SetPropertyValue(fPerson, 'Age', TValue.From<Integer>(25));
  Assert.AreEqual(25, fPerson.Age, 'Age should be updated to 25');
end;

procedure TQuickRTTIUtilsTests.Test_SetPropertyValue_Float;
begin
  TRTTI.SetPropertyValue(fPerson, 'Salary', TValue.From<Double>(99999.99));
  Assert.AreEqual(Double(99999.99), fPerson.Salary, 'Salary should be updated');
end;

{ GetPropertyValueEx }

procedure TQuickRTTIUtilsTests.Test_GetPropertyValueEx_String;
begin
  Assert.AreEqual('Alice', TRTTI.GetPropertyValueEx(fPerson, 'Name').AsString, 'GetPropertyValueEx should return Name');
end;

procedure TQuickRTTIUtilsTests.Test_GetPropertyValueEx_Integer;
begin
  Assert.AreEqual(30, TRTTI.GetPropertyValueEx(fPerson, 'Age').AsInteger, 'GetPropertyValueEx should return Age');
end;

{ GetPathValue / SetPathValue }

procedure TQuickRTTIUtilsTests.Test_GetPathValue_SimpleProperty;
begin
  Assert.AreEqual('Alice', TRTTI.GetPathValue(fPerson, 'Name').AsString, 'GetPathValue should return Name');
  Assert.AreEqual(30, TRTTI.GetPathValue(fPerson, 'Age').AsInteger, 'GetPathValue should return Age');
end;

procedure TQuickRTTIUtilsTests.Test_GetPathValue_NestedProperty;
begin
  Assert.AreEqual('Madrid', TRTTI.GetPathValue(fPerson, 'Address.City').AsString, 'GetPathValue should traverse nested Address.City');
  Assert.AreEqual('28001', TRTTI.GetPathValue(fPerson, 'Address.Zip').AsString, 'GetPathValue should traverse nested Address.Zip');
end;

procedure TQuickRTTIUtilsTests.Test_SetPathValue_SimpleProperty;
begin
  TRTTI.SetPathValue(fPerson, 'Name', TValue.From<string>('Charlie'));
  Assert.AreEqual('Charlie', fPerson.Name, 'SetPathValue should update Name');
end;

procedure TQuickRTTIUtilsTests.Test_SetPathValue_NestedProperty;
begin
  TRTTI.SetPathValue(fPerson, 'Address.City', TValue.From<string>('Barcelona'));
  Assert.AreEqual('Barcelona', fPerson.Address.City, 'SetPathValue should update nested Address.City');
end;

{ PathExists }

procedure TQuickRTTIUtilsTests.Test_PathExists_SimpleProperty_ReturnsTrue;
begin
  Assert.IsTrue(TRTTI.PathExists(fPerson, 'Name'), 'Name path should exist');
  Assert.IsTrue(TRTTI.PathExists(fPerson, 'Age'), 'Age path should exist');
end;

procedure TQuickRTTIUtilsTests.Test_PathExists_NestedProperty_ReturnsTrue;
begin
  Assert.IsTrue(TRTTI.PathExists(fPerson, 'Address.City'), 'Address.City path should exist');
end;

procedure TQuickRTTIUtilsTests.Test_PathExists_NonExisting_ReturnsFalse;
begin
  Assert.IsFalse(TRTTI.PathExists(fPerson, 'NonExisting'), 'Non-existing path should return False');
  Assert.IsFalse(TRTTI.PathExists(fPerson, 'Address.NonExisting'), 'Non-existing nested path should return False');
end;

procedure TQuickRTTIUtilsTests.Test_PathExists_NilInstance_ReturnsFalse;
begin
  Assert.IsFalse(TRTTI.PathExists(nil, 'Name'), 'Nil instance should return False');
end;

{ GetProperty }

procedure TQuickRTTIUtilsTests.Test_GetProperty_ReturnsCorrectRttiProperty;
var
  prop: TRttiProperty;
begin
  prop := TRTTI.GetProperty(fPerson, 'Name');
  Assert.IsNotNull(prop, 'GetProperty should return a valid TRttiProperty');
  Assert.AreEqual('Name', prop.Name, 'Property name should be Name');
end;

procedure TQuickRTTIUtilsTests.Test_GetProperty_NonExisting_ReturnsNil;
var
  prop: TRttiProperty;
begin
  prop := TRTTI.GetProperty(fPerson, 'NonExisting');
  Assert.IsNull(prop, 'GetProperty for non-existing property should return nil');
end;

procedure TQuickRTTIUtilsTests.Test_TryGetProperty_Existing_ReturnsTrue;
var
  prop: TRttiProperty;
begin
  Assert.IsTrue(TRTTI.TryGetProperty(fPerson, 'Name', prop));
  Assert.IsNotNull(prop);
  Assert.AreEqual('Name', prop.Name);
end;

procedure TQuickRTTIUtilsTests.Test_TryGetProperty_NonExisting_ReturnsFalse;
var
  prop: TRttiProperty;
begin
  Assert.IsFalse(TRTTI.TryGetProperty(fPerson, 'Nope', prop));
  Assert.IsNull(prop);
end;

procedure TQuickRTTIUtilsTests.Test_TryGetProperty_CaseInsensitive_ReturnsTrue;
var
  prop: TRttiProperty;
begin
  Assert.IsTrue(TRTTI.TryGetProperty(TypeInfo(TPerson), 'name', prop));
  Assert.IsNotNull(prop);
  Assert.AreEqual('Name', prop.Name);
end;

procedure TQuickRTTIUtilsTests.Test_TryGetMethod_Existing_ReturnsTrue;
var
  m: TRttiMethod;
begin
  Assert.IsTrue(TRTTI.TryGetMethod(fPerson, 'FullName', m));
  Assert.IsNotNull(m);
  Assert.AreEqual('FullName', m.Name);
end;

procedure TQuickRTTIUtilsTests.Test_TryGetMethod_NonExisting_ReturnsFalse;
var
  m: TRttiMethod;
begin
  Assert.IsFalse(TRTTI.TryGetMethod(TypeInfo(TPerson), 'NoMethod', m));
  Assert.IsNull(m);
end;

procedure TQuickRTTIUtilsTests.Test_ZZ_Benchmark_RttiCache;
const
  WARMUP = 2000;
  N = 200000;
  NUMTHREADS = 8;
  N_MT = 100000;
var
  i: Integer;
  sw: TStopwatch;
  prop: TRttiProperty;
  method: TRttiMethod;
  ctx: TRttiContext;
  rtype: TRttiType;
  startEvent: TEvent;
  threads: array of TThread;
  readyCount: Integer;
  totalOpsMt: Int64;
  elapsedUncachedTryGetPropertyMs: Int64;
  elapsedUncachedTryGetMethodMs: Int64;
  elapsedUncachedPropertyExistsMs: Int64;
  elapsedMtTryGetPropertyMs: Int64;
  elapsedTryGetPropertyMs: Int64;
  elapsedTryGetMethodMs: Int64;
  elapsedPropertyExistsMs: Int64;

  function Speedup(const uncachedMs, cachedMs: Int64): Double;
  begin
    if cachedMs <= 0 then
      Exit(0);
    Result := uncachedMs / cachedMs;
  end;

  function OpsPerSec(const ops, elapsedMs: Int64): Double;
  begin
    if elapsedMs <= 0 then
      Exit(0);
    Result := ops / (elapsedMs / 1000.0);
  end;
begin
  // Warm-up RTTI + caches
  for i := 1 to WARMUP do
  begin
    TRTTI.TryGetProperty(fPerson, 'Name', prop);
    TRTTI.TryGetMethod(fPerson, 'FullName', method);
    TRTTI.PropertyExists(TypeInfo(TPerson), 'Name');
  end;

  sw := TStopwatch.StartNew;
  for i := 1 to N do
    TRTTI.TryGetProperty(fPerson, 'Name', prop);
  sw.Stop;
  elapsedTryGetPropertyMs := sw.ElapsedMilliseconds;

  sw := TStopwatch.StartNew;
  for i := 1 to N do
    TRTTI.TryGetMethod(fPerson, 'FullName', method);
  sw.Stop;
  elapsedTryGetMethodMs := sw.ElapsedMilliseconds;

  sw := TStopwatch.StartNew;
  for i := 1 to N do
    TRTTI.PropertyExists(TypeInfo(TPerson), 'Name');
  sw.Stop;
  elapsedPropertyExistsMs := sw.ElapsedMilliseconds;

  // Uncached baseline (direct RTTI lookups)
  ctx := TRttiContext.Create;
  try
    sw := TStopwatch.StartNew;
    for i := 1 to N do
    begin
      rtype := ctx.GetType(fPerson.ClassInfo);
      prop := rtype.GetProperty('Name');
    end;
    sw.Stop;
    elapsedUncachedTryGetPropertyMs := sw.ElapsedMilliseconds;

    sw := TStopwatch.StartNew;
    for i := 1 to N do
    begin
      rtype := ctx.GetType(fPerson.ClassInfo);
      method := rtype.GetMethod('FullName');
    end;
    sw.Stop;
    elapsedUncachedTryGetMethodMs := sw.ElapsedMilliseconds;

    sw := TStopwatch.StartNew;
    for i := 1 to N do
    begin
      rtype := ctx.GetType(TypeInfo(TPerson));
      prop := rtype.GetProperty('Name');
    end;
    sw.Stop;
    elapsedUncachedPropertyExistsMs := sw.ElapsedMilliseconds;
  finally
    ctx.Free;
  end;

  // Multi-thread cached throughput (TryGetProperty)
  startEvent := TEvent.Create(nil, True, False, '');
  try
    readyCount := 0;
    SetLength(threads, NUMTHREADS);
    for i := 0 to NUMTHREADS - 1 do
    begin
      threads[i] := TThread.CreateAnonymousThread(
        procedure
        var
          j: Integer;
          p: TRttiProperty;
        begin
          TInterlocked.Increment(readyCount);
          startEvent.WaitFor(INFINITE);
          for j := 1 to N_MT do
            TRTTI.TryGetProperty(fPerson, 'Name', p);
        end);
      threads[i].FreeOnTerminate := False;
      threads[i].Start;
    end;

    while readyCount < NUMTHREADS do
      TThread.Sleep(1);

    sw := TStopwatch.StartNew;
    startEvent.SetEvent;
    for i := 0 to NUMTHREADS - 1 do
    begin
      threads[i].WaitFor;
      threads[i].Free;
    end;
    sw.Stop;
    elapsedMtTryGetPropertyMs := sw.ElapsedMilliseconds;
  finally
    startEvent.Free;
  end;
  totalOpsMt := Int64(NUMTHREADS) * Int64(N_MT);

  // Visible in test run output/console runner
  Writeln('');
  Writeln('RTTI Cache Benchmark (Quick.RTTI.Utils):');
  Writeln(Format('  TryGetProperty x%d (cached)   : %d ms', [N, elapsedTryGetPropertyMs]));
  Writeln(Format('  TryGetProperty x%d (uncached) : %d ms', [N, elapsedUncachedTryGetPropertyMs]));
  Writeln(Format('    Speedup: %.2fx', [Speedup(elapsedUncachedTryGetPropertyMs, elapsedTryGetPropertyMs)]));

  Writeln(Format('  TryGetMethod   x%d (cached)   : %d ms', [N, elapsedTryGetMethodMs]));
  Writeln(Format('  TryGetMethod   x%d (uncached) : %d ms', [N, elapsedUncachedTryGetMethodMs]));
  Writeln(Format('    Speedup: %.2fx', [Speedup(elapsedUncachedTryGetMethodMs, elapsedTryGetMethodMs)]));

  Writeln(Format('  PropertyExists x%d (cached)   : %d ms', [N, elapsedPropertyExistsMs]));
  Writeln(Format('  PropertyExists x%d (uncached) : %d ms', [N, elapsedUncachedPropertyExistsMs]));
  Writeln(Format('    Speedup: %.2fx', [Speedup(elapsedUncachedPropertyExistsMs, elapsedPropertyExistsMs)]));

  Writeln(Format('  TryGetProperty MT x%d (threads=%d) : %d ms', [N_MT, NUMTHREADS, elapsedMtTryGetPropertyMs]));
  Writeln(Format('    Throughput: %.0f ops/s', [OpsPerSec(totalOpsMt, elapsedMtTryGetPropertyMs)]));

  Assert.IsTrue(elapsedTryGetPropertyMs >= 0);
  Assert.IsTrue(elapsedTryGetMethodMs >= 0);
  Assert.IsTrue(elapsedPropertyExistsMs >= 0);
  Assert.IsTrue(elapsedUncachedTryGetPropertyMs >= 0);
  Assert.IsTrue(elapsedUncachedTryGetMethodMs >= 0);
  Assert.IsTrue(elapsedUncachedPropertyExistsMs >= 0);
  Assert.IsTrue(elapsedMtTryGetPropertyMs >= 0);
end;

{ GetProperties (inheritance order) }

procedure TQuickRTTIUtilsTests.Test_GetProperties_FirstBase_BasePropertiesFirst;
var
  rtype: TRttiType;
  props: TArray<TRttiProperty>;
  ctx: TRttiContext;
begin
  ctx := TRttiContext.Create;
  try
    rtype := ctx.GetType(TypeInfo(TPerson));
    props := TRTTI.GetProperties(rtype, roFirstBase);
    Assert.IsTrue(Length(props) > 0, 'Should return properties');
    // With roFirstBase, inherited Id (from TPersonBase) should appear before Name
    Assert.AreEqual('Id', props[0].Name, 'First property should be Id from base class');
  finally
    ctx.Free;
  end;
end;

procedure TQuickRTTIUtilsTests.Test_GetProperties_FirstInherited_DerivedPropertiesFirst;
var
  rtype: TRttiType;
  props: TArray<TRttiProperty>;
  ctx: TRttiContext;
begin
  ctx := TRttiContext.Create;
  try
    rtype := ctx.GetType(TypeInfo(TPerson));
    props := TRTTI.GetProperties(rtype, roFirstInherited);
    Assert.IsTrue(Length(props) > 0, 'Should return properties');
    // With roFirstInherited, TPerson's own properties appear first
    Assert.AreNotEqual('Id', props[0].Name, 'First property should NOT be the base class Id');
  finally
    ctx.Free;
  end;
end;

{ GetType }

procedure TQuickRTTIUtilsTests.Test_GetType_ReturnsCorrectType;
var
  rtype: TRttiType;
begin
  rtype := TRTTI.GetType(TypeInfo(TPerson));
  Assert.IsNotNull(rtype, 'GetType should return a valid TRttiType');
  Assert.AreEqual('TPerson', rtype.Name, 'Type name should be TPerson');
end;

{ CreateInstance }

procedure TQuickRTTIUtilsTests.Test_CreateInstance_Generic;
var
  obj: TAddressInfo;
begin
  obj := TRTTI.CreateInstance<TAddressInfo>;
  try
    Assert.IsNotNull(obj, 'CreateInstance<TAddressInfo> should return a valid instance');
    Assert.IsTrue(obj is TAddressInfo, 'Instance should be of type TAddressInfo');
  finally
    obj.Free;
  end;
end;

procedure TQuickRTTIUtilsTests.Test_CreateInstance_ByClass;
var
  obj: TObject;
begin
  obj := TRTTI.CreateInstance(TAddressInfo);
  try
    Assert.IsNotNull(obj, 'CreateInstance(TAddressInfo) should return a valid object');
    Assert.IsTrue(obj is TAddressInfo, 'Instance should be TAddressInfo');
  finally
    obj.Free;
  end;
end;

{ FindClass }

procedure TQuickRTTIUtilsTests.Test_FindClass_KnownClass_ReturnsClass;
var
  cls: TClass;
begin
  cls := TRTTI.FindClass('TPerson');
  Assert.IsNotNull(cls, 'FindClass should find TPerson');
  Assert.AreEqual('TPerson', cls.ClassName, 'Found class should be TPerson');
end;

procedure TQuickRTTIUtilsTests.Test_FindClass_UnknownClass_ReturnsNil;
var
  cls: TClass;
begin
  cls := TRTTI.FindClass('TThisClassDoesNotExist');
  Assert.IsNull(cls, 'FindClass with unknown name should return nil');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickRTTIUtilsTests);
end.
