unit Tests.Pooling;

interface

uses
  DUnitX.TestFramework,
  System.SyncObjs,
  Quick.Pooling;

type
  TPoolTestItem = class
  private
    fName: string;
  public
    constructor Create(const aName: string);
    property Name: string read fName write fName;
  end;

  [TestFixture]
  TTestPoolItem = class
  private
    fItemName: string;
    fPoolItem: IPoolItem<TPoolTestItem>;
    fSemaphore: TSemaphore;
    fCS: TCriticalSection;
  public
    [SetupFixture]
    procedure setupFixture;
    [TearDownFixture]
    procedure tearDownFixture;
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure createDelegate;
    [Test]
    procedure item;
    [Test]
    procedure refCount;
    [Test]
    procedure itemIndex;
    [Test]
    procedure lastAccess;
  end;

  [TextFixture]
  TTestTestObjectPool = class
  private
  public
    [Test]
    procedure poolSize;
    [Test]
    procedure get;

    [Test]
    [TestCase ('Less', '5,3,true')]
    [TestCase ('More', '2,3,false')]
    procedure tryGet(const aPoolSize, aExtractNum: integer; const aExpected: boolean);

    [Test]
    procedure WillRaiseWhenTimeout;
    [Test]
    procedure timeout;
    [Test]
    procedure createDelegate;
    [Test]
    procedure autoFree;
    [Test]
    procedure poolExpansion;
 end;

implementation

uses
  SysUtils;

procedure TTestPoolItem.createDelegate;
begin
  Assert.AreEqual(fItemName, fPoolItem.Item.Name);
end;

procedure TTestPoolItem.item;
begin
  Assert.IsNotNull(fPoolItem.Item);
end;

procedure TTestPoolItem.itemIndex;
begin
  Assert.AreEqual(0, fPoolItem.ItemIndex);
end;

procedure TTestPoolItem.lastAccess;
begin
  var dt: TDateTime:=Now;
  var item:TPoolTestItem:=fPoolItem.Item;
  Assert.AreEqual(dt, fPoolItem.LastAccess);
end;

procedure TTestPoolItem.refCount;
begin
  var item:TPoolTestItem:=fPoolItem.Item;
  Assert.AreEqual(1, fPoolItem.RefCount, ' - 1');
end;

procedure TTestPoolItem.Setup;
begin
  fItemName:=Random(1000).ToString;
  fPoolItem:=TPoolItem<TPoolTestItem>.Create(fSemaphore, fCS, 0,
                              procedure(var aInstance : TPoolTestItem)
                              begin
                                aInstance:=TPoolTestItem.Create(fItemName);
                              end);
end;

procedure TTestPoolItem.setupFixture;
begin
  fSemaphore:=TSemaphore.Create;
  fCS:=TCriticalSection.Create;
end;

procedure TTestPoolItem.TearDown;
begin
end;

procedure TTestPoolItem.tearDownFixture;
begin
  // When free is called an AV is raised....????
  // Dropping the _AddRef and _Release fixed the issue
  fCS.free;
  fSemaphore.Free;
end;

{ TPoolTestItem }

constructor TPoolTestItem.Create(const aName: string);
begin
  inherited Create;
  fName:=aName;
end;

{ TTestTestObjectPool }

procedure TTestTestObjectPool.autoFree;
var
  pool: IObjectPool<TPoolTestItem>;
begin
  pool:=TObjectPool<TPoolTestItem>.Create(5, 300000);
  Assert.IsNotNull(pool.AutoFreeIdleItemTimeMs(1000));
  pool:=nil;
end;

procedure TTestTestObjectPool.createDelegate;
var
  pool: IObjectPool<TPoolTestItem>;
  item: TPoolTestItem;
begin
  pool:=TObjectPool<TPoolTestItem>.Create(5, 300000,
                                              procedure(var aInstance : TPoolTestItem)
                                              begin
                                                aInstance:=TPoolTestItem.Create('abc');
                                              end);
  item:=pool.Get.Item;
  Assert.AreEqual('abc', item.Name);
  pool:=nil;
end;

procedure TTestTestObjectPool.get;
var
  pool: IObjectPool<TPoolTestItem>;
  item: TPoolTestItem;
begin
  pool:=TObjectPool<TPoolTestItem>.Create(5, 300000,
                                              procedure(var aInstance : TPoolTestItem)
                                              begin
                                                aInstance:=TPoolTestItem.Create('get');
                                              end);
  item:=pool.Get.Item;
  Assert.IsNotNull(item, ' - 1');
  Assert.AreEqual('get', item.Name, ' - 2');
  pool:=nil;
end;

procedure TTestTestObjectPool.poolExpansion;
var
  pool: IObjectPool<TPoolTestItem>;
  item: TPoolTestItem;
begin
  pool:=TObjectPool<TPoolTestItem>.Create(2, 2000,
                                              procedure(var aInstance : TPoolTestItem)
                                              begin
                                                aInstance:=TPoolTestItem.Create(Random(1000).ToString);
                                              end);

  Assert.AreEqual(2, pool.PoolSize);
  // Not a mistake
  item:=pool.Get.Item;
  item:=pool.Get.Item;
  pool.PoolSize:=3;
  Assert.AreEqual(3, pool.PoolSize, ' - 1');
  item:=pool.Get.Item;
  Assert.IsNotNull(item, ' - 2');
  pool.PoolSize:=5;
  Assert.AreEqual(5, pool.PoolSize, ' - 3');
  item:=pool.Get.Item;
  item:=pool.Get.Item;
  Assert.IsNotNull(item, ' - 4');
end;

procedure TTestTestObjectPool.poolSize;
var
  pool: IObjectPool<TPoolTestItem>;
begin
  pool:=TObjectPool<TPoolTestItem>.Create(5);
  Assert.AreEqual(5, pool.PoolSize);
  pool:=nil;
end;

procedure TTestTestObjectPool.WillRaiseWhenTimeout;
var
  pool: IObjectPool<TPoolTestItem>;
  num: integer;
  item: IPoolItem<TPoolTestItem>;
begin
  pool:=TObjectPool<TPoolTestItem>.Create(1);
  pool.TimeoutMs(1000);
  item:=pool.Get;
  Assert.WillRaise(procedure
                   begin
                     item:=pool.Get;
                   end, EObjectPool);
  pool:=nil;
end;

procedure TTestTestObjectPool.timeout;
var
  pool: IObjectPool<TPoolTestItem>;
begin
  pool:=TObjectPool<TPoolTestItem>.Create(5, 300000);
  Assert.IsNotNull(pool.TimeoutMs(1000));
  pool:=nil;
end;

procedure TTestTestObjectPool.tryGet(const aPoolSize, aExtractNum: integer;
    const aExpected: boolean);
var
  pool: IObjectPool<TPoolTestItem>;
  num: integer;
  item: IPoolItem<TPoolTestItem>;
begin
  pool:=TObjectPool<TPoolTestItem>.Create(aPoolSize);
  pool.TimeoutMs(1000);
  for num := 1 to aExtractNum - 1 do
    item:=pool.Get;
  item:=nil;
  Assert.AreEqual(aExpected, pool.TryGet(item));
  if aExpected then
    Assert.IsNotNull(item);
  pool:=nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPoolItem);
  TDUnitX.RegisterTestFixture(TTestTestObjectPool);
end.
