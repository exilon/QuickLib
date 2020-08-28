unit Tests.RTTIUtils;

interface

uses
  DUnitX.TestFramework,
  Quick.RTTI.Utils, System.Generics.Collections;

type
  TSubItem = class
  private
    fName: String;
    fList: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: String read fName write fName;
    property List: TList<string> read fList write fList;
  end;

  TItem = class
  private
    fPrice: Integer;
    fTag: string;
    fSubItem: TSubItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure changeTag (const aTag: string);

    property Price: Integer read fPrice write fPrice;
    property Tag: string read fTag write fTag;
    property SubItem: TSubItem read fSubItem write fSubItem;
  end;

  [TestFixture]
  TestRTTIUtils = class
  private
    item: TItem;
    rttiUtils: TRTTI;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFicture;

    [Test]
    procedure getFieldInstance;
    [Test]
    procedure getFieldPointer;
    [Test]
    procedure fieldExists;
    [Test]
    procedure getFieldValueInstance;
    [Test]
    procedure getFieldValuePointer;

    [Test]
    procedure getProperties;
    [Test]
    procedure getTypePointer;
    [Test]
    procedure getPropertyInstance;
    [Test]
    procedure getPropertyObject;
    [Test]
    procedure getPropertyPath;

    [Test]
    procedure getMemberPath;
    [Test]
    procedure pathExists;
    [Test]
    procedure getPathValue;
    [Test]
    procedure setPathValue;

    [Test]
    procedure setPropertyaValue;
    [Test]
    procedure propertyExists;
    [Test]
    procedure getPropertyValueInstance;
    [Test]
    procedure getPropertyValuePointer;
    [Test]
    procedure getPropertyValueEx;
    [Test]
    procedure findClass;
    [Test]
    procedure createInstance;
    [Test]
    procedure createInstanceBase;

    [Test]
    procedure callMethod;
  end;

implementation

uses
  System.Rtti;


{ TestRTTIUtils }

procedure TestRTTIUtils.callMethod;
var
  old: string;
begin
  old:=item.Tag;
  rttiUtils.CallMethod(item, 'changeTag', ['NEW']);
  Assert.AreEqual('NEW', item.Tag);
  item.Tag:=old;
end;

procedure TestRTTIUtils.createInstance;
var
  nItem: TItem;
begin
  nItem:=rttiUtils.CreateInstance<TItem>;
  Assert.IsNotNull(nItem);
  nItem.Free;
end;

procedure TestRTTIUtils.createInstanceBase;
var
  nItem: TObject;
begin
  nItem:=rttiUtils.CreateInstance(TItem);
  Assert.IsNotNull(nItem);
  nItem.Free;
end;

procedure TestRTTIUtils.fieldExists;
begin
  Assert.IsTrue(rttiUtils.FieldExists(item.ClassInfo, 'fTag'));
  Assert.IsFalse(rttiUtils.FieldExists(item.ClassInfo, 'Score'));
end;

procedure TestRTTIUtils.findClass;
var
  cl: TClass;
begin
  Assert.AreEqual(TItem, rttiUtils.FindClass('TItem'));
  Assert.AreEqual(TSubItem, rttiUtils.FindClass('TSubItem'));
  cl:=rttiUtils.FindClass('Item');
  Assert.IsNull(cl);
end;

procedure TestRTTIUtils.getFieldInstance;
var
  field: TRTTIField;
begin
  field:=rttiUtils.GetField(item, 'fTag');
  Assert.IsNotNull(field, 'GetField - 1');
  Assert.AreEqual(item.Tag, field.GetValue(item).AsString, 'GetField - 2');
end;

procedure TestRTTIUtils.getFieldPointer;
var
  field: TRTTIField;
begin
  field:=rttiUtils.GetField(item.ClassInfo, 'fTag');
  Assert.IsNotNull(field, 'GetField - 1');
  Assert.AreEqual(item.Tag, field.GetValue(item).AsString, 'GetField - 2');
end;

procedure TestRTTIUtils.getFieldValueInstance;
begin
  Assert.AreEqual(item.Tag, rttiUtils.GetFieldValue(item, 'fTag').AsString);
end;

procedure TestRTTIUtils.getFieldValuePointer;
begin
  //Assert.AreEqual(item.Tag, rttiUtils.GetFieldValue(item.ClassInfo, 'fTag').AsString);
end;

procedure TestRTTIUtils.getMemberPath;
var
  member: TRttiMember;
begin
  member:=rttiUtils.GetMemberPath(item, 'SubItem.Name');
  Assert.IsNotNull(member);
end;

procedure TestRTTIUtils.getPathValue;
begin
//  Assert.Fail('TODO: AV in PathExists - see assertion');
  Assert.AreEqual(item.Tag, rttiUtils.GetPathValue(item, 'Tag').AsString, 'PV - 1');
  Assert.AreEqual(item.SubItem.Name, rttiUtils.GetPathValue(item, 'SubItem.Name').AsString, 'PV - 2');
end;

procedure TestRTTIUtils.getProperties;
var
  list: TArray<TRttiProperty>;
  ctx: TRttiContext;
  prop: TRttiProperty;
begin
  list:=rttiUtils.GetProperties(ctx.GetType(TItem), roFirstBase);
  Assert.AreEqual(3, Length(list), 'getProp - 1');
  Assert.AreEqual('Price', list[0].Name, 'getProp - 2');
end;

procedure TestRTTIUtils.getPropertyInstance;
var
  prop: TRttiProperty;
begin
  prop:=rttiUtils.GetProperty(item.ClassInfo, 'Price');
  Assert.IsNotNull(prop);
  Assert.AreEqual(100, prop.GetValue(item).AsInteger);
end;

procedure TestRTTIUtils.getPropertyObject;
var
  prop: TRttiProperty;
begin
  prop:=rttiUtils.GetProperty(item, 'Price');
  Assert.IsNotNull(prop);
  Assert.AreEqual(100, prop.GetValue(item).AsInteger);
end;

procedure TestRTTIUtils.getPropertyPath;
var
  prop: TRttiProperty;
begin
  prop:=rttiUtils.GetPropertyPath(item,'Tag');
  Assert.IsNotNull(prop);
  Assert.AreEqual(item.Tag, prop.GetValue(item).AsString);
  prop:=rttiUtils.GetPropertyPath(item, 'SubItem.Name');
  Assert.IsNotNull(prop);
end;

procedure TestRTTIUtils.getPropertyValueEx;
begin
  Assert.AreEqual(item.Tag, rttiUtils.GetPropertyValueEx(item, 'Tag').AsString);
end;

procedure TestRTTIUtils.getPropertyValueInstance;
begin
//  Assert.AreEqual(item.Tag, rttiUtils.GetPropertyValue(item, 'Tag'));
end;

procedure TestRTTIUtils.getPropertyValuePointer;
begin
//  Assert.AreEqual(item.Tag, rttiUtils.GetPropertyValue(item.ClassInfo, 'Tag'));
end;

procedure TestRTTIUtils.getTypePointer;
var
  cType: TRttiType;
begin
  cType:=rttiUtils.GetType(item.ClassInfo);
  Assert.Pass;
end;

procedure TestRTTIUtils.pathExists;
begin
  Assert.IsTrue(rttiUtils.PathExists(item, 'SubItem.Name'), 'Path - 1');
  Assert.IsFalse(rttiUtils.PathExists(item, 'SubItem.Surname'), 'Path 2');
end;

procedure TestRTTIUtils.propertyExists;
begin
  Assert.IsTrue(rttiUtils.PropertyExists(item.ClassInfo, 'Tag'));
end;

procedure TestRTTIUtils.setPathValue;
var
  price: integer;
  name: string;
begin
  price:=item.Price;
  rttiUtils.SetPathValue(item, 'Price', 200);
  Assert.AreEqual(200, item.Price, 'Path - 1');
  item.Price:=price;

  name:=item.SubItem.Name;
  rttiUtils.SetPathValue(item, 'SubItem.Name', 'Kour');
  Assert.AreEqual('Kour', item.SubItem.Name, 'Path - 2');
  item.SubItem.Name:=name;
end;

procedure TestRTTIUtils.setPropertyaValue;
var
  old: string;
begin
  old:=item.Tag;
  rttiUtils.SetPropertyValue(item, 'Tag', 'ABC');
  Assert.AreEqual('ABC', item.Tag);
  item.Tag:=old;
end;

procedure TestRTTIUtils.SetupFixture;
begin
  rttiUtils:=TRTTI.Create;

  item:=TItem.Create;
  item.Price:=100;
  item.Tag:='tag';
end;

procedure TestRTTIUtils.TearDownFicture;
begin
  rttiUtils.Free;
  item.Free;
end;

procedure TItem.changeTag(const aTag: string);
begin
  fTag:=aTag;
end;

constructor TItem.Create;
begin
  inherited;
  fSubItem:=TSubItem.Create;
end;

destructor TItem.Destroy;
begin
  fSubItem.Free;
  inherited;
end;

constructor TSubItem.Create;
begin
  inherited;
  fName:='John';
  fList:=TList<string>.Create;
end;

destructor TSubItem.Destroy;
begin
  fList.Free;
  inherited;
end;

initialization
  TDUnitX.RegisterTestFixture(TestRTTIUtils);

end.
