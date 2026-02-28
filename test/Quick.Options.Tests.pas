unit Quick.Options.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  Quick.Options,
  Quick.Options.Serializer.Json;

type
  TDatabaseOptions = class(TOptions)
  private
    fHost: string;
    fPort: Integer;
    fDatabase: string;
    fTimeout: Double;
  published
    [Required]
    [StringLength(100, 'Host name too long')]
    property Host: string read fHost write fHost;
    [Range(1, 65535, 'Port must be between 1 and 65535')]
    property Port: Integer read fPort write fPort;
    [Required]
    property Database: string read fDatabase write fDatabase;
    [Range(0.0, 300.0)]
    property Timeout: Double read fTimeout write fTimeout;
  end;

  TLoggingOptions = class(TOptions)
  private
    fLevel: Integer;
    fPath: string;
    fEnabled: Boolean;
  published
    property Level: Integer read fLevel write fLevel;
    [StringLength(255, 'Log path too long')]
    property Path: string read fPath write fPath;
    property Enabled: Boolean read fEnabled write fEnabled;
  end;

  { Extra options class used only in new tests }
  TCacheOptions = class(TOptions)
  private
    fMaxItems: Integer;
    fTTLSeconds: Integer;
  published
    [Range(1, 100000)]
    property MaxItems: Integer read fMaxItems write fMaxItems;
    property TTLSeconds: Integer read fTTLSeconds write fTTLSeconds;
  end;

  [TestFixture]
  TQuickOptionsTests = class(TObject)
  private
    fContainer: TOptionsContainer;
    fSerializer: IOptionsSerializer;
    fTempFile: string;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Test_AddSection_Generic;
    [Test]
    procedure Test_GetSection_Generic;
    [Test]
    procedure Test_AddSection_MultipleSections;
    [Test]
    procedure Test_ExistsSection_True;
    [Test]
    procedure Test_ExistsSection_False;
    [Test]
    procedure Test_Options_DefaultValues;
    [Test]
    procedure Test_ValidateOptions_Required_Passes;
    [Test]
    procedure Test_ValidateOptions_Required_Fails;
    [Test]
    procedure Test_ValidateOptions_StringLength_Passes;
    [Test]
    procedure Test_ValidateOptions_StringLength_Fails;
    [Test]
    procedure Test_ValidateOptions_Range_Passes;
    [Test]
    procedure Test_ValidateOptions_Range_Fails;
    [Test]
    procedure Test_Save_And_Load_Json;
    [Test]
    procedure Test_GetSectionInterface;
    [Test]
    procedure Test_ConfigureOptions;

    { --- New coverage tests --- }
    [Test]
    procedure Test_ValidateOptions_DoubleRange_Fails;
    [Test]
    procedure Test_ValidateOptions_DoubleRange_Passes;
    [Test]
    procedure Test_ConfigureOptions_Then_Validate_Passes;
    [Test]
    procedure Test_Logging_BoolEnabled_SaveLoad;
    [Test]
    procedure Test_ThreeSections_Count;
    [Test]
    procedure Test_Cache_Range_MaxItems_Fails;
    [Test]
    procedure Test_Cache_Range_MaxItems_Passes;
    [Test]
    procedure Test_SaveLoad_PreservesAllFields;
    [Test]
    procedure Test_Options_ModifyAfterAdd;
  end;

implementation

procedure TQuickOptionsTests.SetUp;
begin
  fTempFile := TPath.Combine(TPath.GetTempPath, 'quicklib_options_test.json');
  fSerializer := TJsonOptionsSerializer.Create(fTempFile);
  fContainer := TOptionsContainer.Create(fSerializer);
end;

procedure TQuickOptionsTests.TearDown;
begin
  fContainer.Free;
  if TFile.Exists(fTempFile) then
    TFile.Delete(fTempFile);
end;

procedure TQuickOptionsTests.Test_AddSection_Generic;
begin
  fContainer.AddSection<TDatabaseOptions>;
  Assert.IsTrue(fContainer.ExistsSection<TDatabaseOptions>, 'AddSection should return a valid TOptions<T>');
  Assert.IsTrue(fContainer.ExistsSection<TDatabaseOptions>, 'Section should exist after AddSection');
end;

procedure TQuickOptionsTests.Test_GetSection_Generic;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db := fContainer.GetSection<TDatabaseOptions>;
  Assert.IsNotNull(db, 'GetSection should return non-nil TDatabaseOptions');
  Assert.IsTrue(db is TDatabaseOptions, 'GetSection should return TDatabaseOptions instance');
end;

procedure TQuickOptionsTests.Test_AddSection_MultipleSections;
begin
  fContainer.AddSection<TDatabaseOptions>;
  fContainer.AddSection<TLoggingOptions>;
  Assert.AreEqual(2, fContainer.Count, 'Container should have 2 sections');
  Assert.IsTrue(fContainer.ExistsSection<TDatabaseOptions>, 'DatabaseOptions section should exist');
  Assert.IsTrue(fContainer.ExistsSection<TLoggingOptions>, 'LoggingOptions section should exist');
end;

procedure TQuickOptionsTests.Test_ExistsSection_True;
begin
  fContainer.AddSection<TLoggingOptions>;
  Assert.IsTrue(fContainer.ExistsSection<TLoggingOptions>, 'ExistsSection should return true for added section');
end;

procedure TQuickOptionsTests.Test_ExistsSection_False;
begin
  Assert.IsFalse(fContainer.ExistsSection<TDatabaseOptions>, 'ExistsSection should return false for non-added section');
end;

procedure TQuickOptionsTests.Test_Options_DefaultValues;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db := fContainer.GetSection<TDatabaseOptions>;
  Assert.AreEqual('', db.Host, 'Default Host should be empty string');
  Assert.AreEqual(0, db.Port, 'Default Port should be 0');
end;

procedure TQuickOptionsTests.Test_ValidateOptions_Required_Passes;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db := fContainer.GetSection<TDatabaseOptions>;
  db.Host := 'localhost';
  db.Database := 'mydb';
  db.Port := 5432;
  Assert.WillNotRaise(
    procedure begin db.ValidateOptions; end,
    nil,
    'ValidateOptions should not raise when required fields are set'
  );
end;

procedure TQuickOptionsTests.Test_ValidateOptions_Required_Fails;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db := fContainer.GetSection<TDatabaseOptions>;
  db.Host := '';
  db.Database := '';
  Assert.WillRaise(
    procedure begin db.ValidateOptions; end,
    EOptionConfigureError,
    'ValidateOptions should raise EOptionConfigureError when required fields are empty'
  );
end;

procedure TQuickOptionsTests.Test_ValidateOptions_StringLength_Passes;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db := fContainer.GetSection<TDatabaseOptions>;
  db.Host := 'localhost';
  db.Database := 'testdb';
  db.Port := 5432;
  Assert.WillNotRaise(
    procedure begin db.ValidateOptions; end,
    nil,
    'ValidateOptions should not raise when Host length is within limit'
  );
end;

procedure TQuickOptionsTests.Test_ValidateOptions_StringLength_Fails;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db := fContainer.GetSection<TDatabaseOptions>;
  db.Host := StringOfChar('x', 101); // exceeds StringLength(100)
  db.Database := 'testdb';
  db.Port := 5432;
  Assert.WillRaise(
    procedure begin db.ValidateOptions; end,
    EOptionConfigureError,
    'ValidateOptions should raise when Host exceeds StringLength(100)'
  );
end;

procedure TQuickOptionsTests.Test_ValidateOptions_Range_Passes;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db := fContainer.GetSection<TDatabaseOptions>;
  db.Host := 'localhost';
  db.Database := 'testdb';
  db.Port := 5432;
  db.Timeout := 30.0;
  Assert.WillNotRaise(
    procedure begin db.ValidateOptions; end,
    nil,
    'ValidateOptions should not raise when Port and Timeout are in range'
  );
end;

procedure TQuickOptionsTests.Test_ValidateOptions_Range_Fails;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db := fContainer.GetSection<TDatabaseOptions>;
  db.Host := 'localhost';
  db.Database := 'testdb';
  db.Port := 99999; // exceeds Range(1, 65535)
  Assert.WillRaise(
    procedure begin db.ValidateOptions; end,
    EOptionConfigureError,
    'ValidateOptions should raise when Port exceeds range'
  );
end;

procedure TQuickOptionsTests.Test_Save_And_Load_Json;
var
  db: TDatabaseOptions;
  container2: TOptionsContainer;
  serializer2: IOptionsSerializer;
  dbLoaded: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db := fContainer.GetSection<TDatabaseOptions>;
  db.Host := 'dbserver';
  db.Port := 1433;
  db.Database := 'production';
  fContainer.Save;

  Assert.IsTrue(TFile.Exists(fTempFile), 'Options file should exist after Save');

  serializer2 := TJsonOptionsSerializer.Create(fTempFile);
  container2 := TOptionsContainer.Create(serializer2);
  try
    container2.AddSection<TDatabaseOptions>;
    container2.Load;
    dbLoaded := container2.GetSection<TDatabaseOptions>;
    Assert.AreEqual('dbserver', dbLoaded.Host, 'Loaded Host should match saved value');
    Assert.AreEqual(1433, dbLoaded.Port, 'Loaded Port should match saved value');
    Assert.AreEqual('production', dbLoaded.Database, 'Loaded Database should match saved value');
  finally
    container2.Free;
  end;
end;

procedure TQuickOptionsTests.Test_GetSectionInterface;
var
  iOpts: IOptions<TLoggingOptions>;
begin
  fContainer.AddSection<TLoggingOptions>;
  iOpts := fContainer.GetSectionInterface<TLoggingOptions>;
  Assert.IsNotNull(iOpts, 'GetSectionInterface should return a valid interface');
  Assert.IsNotNull(iOpts.Value, 'IOptions.Value should return the TLoggingOptions instance');
end;

procedure TQuickOptionsTests.Test_ConfigureOptions;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>.ConfigureOptions(
    procedure(aOpts: TDatabaseOptions)
    begin
      aOpts.Host := 'configured-host';
      aOpts.Port := 9000;
      aOpts.Database := 'configured-db';
    end
  );
  db := fContainer.GetSection<TDatabaseOptions>;
  Assert.AreEqual('configured-host', db.Host, 'Host should be set by ConfigureOptions');
  Assert.AreEqual(9000, db.Port, 'Port should be set by ConfigureOptions');
end;

{ ======================================================================
  New coverage tests
  ====================================================================== }

procedure TQuickOptionsTests.Test_ValidateOptions_DoubleRange_Fails;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db := fContainer.GetSection<TDatabaseOptions>;
  db.Host := 'localhost';
  db.Database := 'testdb';
  db.Port := 5432;
  db.Timeout := 500.0; // exceeds Range(0.0, 300.0)
  Assert.WillRaise(
    procedure begin db.ValidateOptions; end,
    EOptionConfigureError,
    'Timeout=500 exceeds [Range(0.0,300.0)] — should raise EOptionConfigureError'
  );
end;

procedure TQuickOptionsTests.Test_ValidateOptions_DoubleRange_Passes;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db := fContainer.GetSection<TDatabaseOptions>;
  db.Host := 'localhost';
  db.Database := 'testdb';
  db.Port := 5432;
  db.Timeout := 0.0; // boundary — inclusive
  Assert.WillNotRaise(
    procedure begin db.ValidateOptions; end,
    nil,
    'Timeout=0.0 is at lower boundary of [Range(0.0,300.0)] — must not raise'
  );
end;

procedure TQuickOptionsTests.Test_ConfigureOptions_Then_Validate_Passes;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>.ConfigureOptions(
    procedure(aOpts: TDatabaseOptions)
    begin
      aOpts.Host     := 'prod-server';
      aOpts.Database := 'production';
      aOpts.Port     := 1433;
      aOpts.Timeout  := 60.0;
    end);
  db := fContainer.GetSection<TDatabaseOptions>;
  Assert.WillNotRaise(
    procedure begin db.ValidateOptions; end,
    nil,
    'Fully configured options should pass validation'
  );
  Assert.AreEqual('prod-server', db.Host, 'Host must match configured value');
end;

procedure TQuickOptionsTests.Test_Logging_BoolEnabled_SaveLoad;
var
  log: TLoggingOptions;
  container2: TOptionsContainer;
  serializer2: IOptionsSerializer;
  logLoaded: TLoggingOptions;
begin
  fContainer.AddSection<TLoggingOptions>;
  log := fContainer.GetSection<TLoggingOptions>;
  log.Level   := 3;
  log.Path    := '/var/log/app.log';
  log.Enabled := False;  // non-default value
  fContainer.Save;

  serializer2 := TJsonOptionsSerializer.Create(fTempFile);
  container2  := TOptionsContainer.Create(serializer2);
  try
    container2.AddSection<TLoggingOptions>;
    container2.Load;
    logLoaded := container2.GetSection<TLoggingOptions>;
    Assert.AreEqual(3, logLoaded.Level, 'Level must be restored to 3');
    Assert.AreEqual('/var/log/app.log', logLoaded.Path, 'Path must be restored');
    Assert.IsFalse(logLoaded.Enabled, 'Enabled=False must survive save/load round-trip');
  finally
    container2.Free;
  end;
end;

procedure TQuickOptionsTests.Test_ThreeSections_Count;
begin
  fContainer.AddSection<TDatabaseOptions>;
  fContainer.AddSection<TLoggingOptions>;
  fContainer.AddSection<TCacheOptions>;
  Assert.AreEqual(3, fContainer.Count, 'Container must hold exactly 3 sections');
  Assert.IsTrue(fContainer.ExistsSection<TCacheOptions>, 'CacheOptions section must exist');
end;

procedure TQuickOptionsTests.Test_Cache_Range_MaxItems_Fails;
var
  cache: TCacheOptions;
begin
  fContainer.AddSection<TCacheOptions>;
  cache := fContainer.GetSection<TCacheOptions>;
  cache.MaxItems := 0; // below Range(1, 100000)
  Assert.WillRaise(
    procedure begin cache.ValidateOptions; end,
    EOptionConfigureError,
    'MaxItems=0 is below lower bound — should raise EOptionConfigureError'
  );
end;

procedure TQuickOptionsTests.Test_Cache_Range_MaxItems_Passes;
var
  cache: TCacheOptions;
begin
  fContainer.AddSection<TCacheOptions>;
  cache := fContainer.GetSection<TCacheOptions>;
  cache.MaxItems    := 500;
  cache.TTLSeconds  := 120;
  Assert.WillNotRaise(
    procedure begin cache.ValidateOptions; end,
    nil,
    'MaxItems=500 is in range — must not raise'
  );
end;

procedure TQuickOptionsTests.Test_SaveLoad_PreservesAllFields;
var
  db: TDatabaseOptions;
  container2: TOptionsContainer;
  serializer2: IOptionsSerializer;
  dbLoaded: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db          := fContainer.GetSection<TDatabaseOptions>;
  db.Host     := 'full-host';
  db.Port     := 3306;
  db.Database := 'full-db';
  db.Timeout  := 120.5;
  fContainer.Save;

  serializer2 := TJsonOptionsSerializer.Create(fTempFile);
  container2  := TOptionsContainer.Create(serializer2);
  try
    container2.AddSection<TDatabaseOptions>;
    container2.Load;
    dbLoaded := container2.GetSection<TDatabaseOptions>;
    Assert.AreEqual('full-host', dbLoaded.Host,     'Host round-trip failed');
    Assert.AreEqual(3306,        dbLoaded.Port,     'Port round-trip failed');
    Assert.AreEqual('full-db',   dbLoaded.Database, 'Database round-trip failed');
    Assert.AreEqual(Double(120.5), Double(dbLoaded.Timeout), Double(0.001), 'Timeout round-trip failed');
  finally
    container2.Free;
  end;
end;

procedure TQuickOptionsTests.Test_Options_ModifyAfterAdd;
var
  db: TDatabaseOptions;
begin
  fContainer.AddSection<TDatabaseOptions>;
  db      := fContainer.GetSection<TDatabaseOptions>;
  db.Host := 'initial';
  Assert.AreEqual('initial', fContainer.GetSection<TDatabaseOptions>.Host,
    'Modification to returned instance should be visible via GetSection');
  db.Host := 'modified';
  Assert.AreEqual('modified', fContainer.GetSection<TDatabaseOptions>.Host,
    'Second modification should also be visible (same instance)');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickOptionsTests);
end.
