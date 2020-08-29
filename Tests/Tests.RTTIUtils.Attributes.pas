unit Tests.RTTIUtils.Attributes;

interface

uses
  DUnitX.TestFramework,
  Quick.RTTI.Utils;

type
  AttributeOne = class(TCustomAttribute)

  end;

  AttributeTwo = class(TCustomAttribute)

  end;

  AttributeThree = class(TCustomAttribute)

  end;

  AttributeParams = class(TCustomAttribute)
  private
    fValue: integer;
  public
    constructor Create(const aValue: integer);

    property Value: integer read fValue;
  end;


  TAttributeItem = class
  private
    fTag: Integer;
  public
    constructor Create;
    [AttributeOne]
    procedure changeBy10;

    [AttributeTwo]
    procedure changeByX (const aValue: integer);

    [AttributeParams (123)]
    procedure change123;

    [AttributeParams (1000)]
    procedure change1000;

    property Tag: Integer read fTag write fTag;
  end;

  [TestFixture]
  TTestRTTIUtilsAttributes = class
  private
    attrItem: TAttributeItem;
    rttiUtils: TRTTI;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFicture;

    [Test]
    procedure getAttributes;
    [Test]
    procedure attributeExists;
    [Test]
    procedure callMethod;
    [Test]
    [TestCase ('Value - 1', '123')]
    [TestCase ('Value - 2', '1000')]
    procedure callMethodAttributeWithArguments (const aValue: integer);
  end;

implementation

constructor TAttributeItem.Create;
begin
  inherited;
  fTag:=100;
end;

{ TAttributeItem }

procedure TAttributeItem.change1000;
begin
  fTag:=fTag + 1000;
end;

procedure TAttributeItem.change123;
begin
  fTag:=fTag + 123;
end;

procedure TAttributeItem.changeBy10;
begin
  fTag:=fTag + 10;
end;

procedure TAttributeItem.changeByX(const aValue: integer);
begin
  fTag:=fTag + aValue;
end;

{ TTestRTTIUtilsAttributes }

procedure TTestRTTIUtilsAttributes.attributeExists;
begin
  Assert.IsTrue(rttiUtils.AttributeExists(attrItem, AttributeOne), 'Exists - 1');
  Assert.IsFalse(rttiUtils.AttributeExists(attrItem, AttributeThree), 'Exists - 2');
end;

procedure TTestRTTIUtilsAttributes.callMethod;
var
  tag: integer;
begin
  tag:=attrItem.Tag;
  rttiUtils.CallMethod(attrItem, AttributeOne, []);
  Assert.AreEqual(tag + 10, attrItem.Tag, 'Call - 1');
  attrItem.Tag:=tag;

  rttiUtils.CallMethod(attrItem, AttributeTwo, [1000]);
  Assert.AreEqual(tag + 1000, attrItem.Tag, 'Call - 2');
  attrItem.Tag:=tag;
end;

procedure TTestRTTIUtilsAttributes.callMethodAttributeWithArguments(const
    aValue: integer);
var
  tag: integer;
  validator: TAttributeValidate;
begin
  tag:=attrItem.Tag;

  validator:=function (const aAttribute: TCustomAttribute): boolean
             begin
               result:=false;
               if aAttribute is AttributeParams then
                result:=(aAttribute as AttributeParams).Value = aValue;
             end;

  rttiUtils.CallMethod(attrItem, AttributeParams, validator, []);
  Assert.AreEqual(tag + aValue, attrItem.Tag, 'Call');
  attrItem.Tag:=tag;
end;

procedure TTestRTTIUtilsAttributes.getAttributes;
var
  list: TArray<TCustomAttribute>;
begin
  list:=rttiUtils.GetAttributes(attrItem);
  Assert.AreEqual(4, Length(list), ' Attr - 1');
  Assert.InheritsFrom(list[0].ClassType, AttributeOne, 'Attr - 2');
  Assert.InheritsFrom(list[1].ClassType, AttributeTwo, 'Attr - 3');
end;

procedure TTestRTTIUtilsAttributes.SetupFixture;
begin
  rttiUtils:=TRTTI.Create;
  attrItem:=TAttributeItem.Create;
end;

procedure TTestRTTIUtilsAttributes.TearDownFicture;
begin
  rttiUtils.Free;
  attrItem.Free;
end;

constructor AttributeParams.Create(const aValue: integer);
begin
  inherited Create;
  fValue:=aValue;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestRTTIUtilsAttributes);

end.
