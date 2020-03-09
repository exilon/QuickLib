program QuickValue;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  RTTI,
  Variants,
  System.TypInfo,
  Quick.Json.Serializer,
  Quick.Commons,
  Quick.Console,
  Quick.Value,
  Quick.Value.RTTI;

type
  TDynArrayOfVariant = array of TVarRecord;

  TMyArray = TArray<TObject>;

  PMyArray = ^TMyArray;

  TMyObject = class
  private
    fName : string;
    fAge : Integer;
  public
    property Name : string read fName write fName;
    property Age : Integer read fAge write fAge;
  end;

var
  flexvalue : TFlexValue;
  arr : TArray<string>;
  value : TValue;

function Test2(aValue : Pointer) : string;
var
  arr : TMyArray;
  parr : Pointer;
begin
  //if VarIsArray(aValue) then
  begin
    //parr := VarArrayLock(aValue);
    //SetLength(arr,VarArrayHighBound(aValue,1) + 1);
    parr := aValue;
    SetLength(arr,2);
    arr := PMyArray(@parr)^;
    var a := arr[0];
    var b := TMyObject(a).Name;
  end;
end;

var
  obj : TMyObject;
  arr2 : TArray<TMyObject>;
  vari : Variant;

begin
  try


    obj := TMyObject.Create;
    obj.Name := 'Joe';
    obj.Age := 30;

    arr2 := [obj];
    //vari := arr2;

    test2(arr2);

    arr := ['item1','item2','item3','item4'];
    flexvalue := arr;
    arr := TArray<string>(flexvalue.AsPointer);
    coutFmt('arr[1]=%s',[arr[1]],etInfo);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
