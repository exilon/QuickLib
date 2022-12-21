unit Quick.Serializer.Intf;

interface

uses
  SysUtils,
  {$IFNDEF FPC}
  rtti;
  {$ELSE}
  Rtti,
  rttiutils;
  {$ENDIF}

type
  TSerializerOptions = class
  private
    fUseEnumNames : Boolean;
    fUseJsonCaseSense : Boolean;
    fUseBase64Stream : Boolean;
    fUseNullStringsAsEmpty : Boolean;
    fUseGUIDWithBrackets: Boolean;
    fUseGUIDLowercase: Boolean;
  public
    property UseEnumNames : Boolean read fUseEnumNames write fUseEnumNames;
    property UseJsonCaseSense : Boolean read fUseJsonCaseSense write fUseJsonCaseSense;
    property UseBase64Stream : Boolean read fUseBase64Stream write fUseBase64Stream;
    property UseNullStringsAsEmpty : Boolean read fUseNullStringsAsEmpty write fUseNullStringsAsEmpty;
    property UseGUIDWithBrackets : Boolean read fUseGUIDWithBrackets write fUseGUIDWithBrackets;
    property UseGUIDLowercase : Boolean read fUseGUIDLowercase write fUseGUIDLowercase;
  end;

  ISerializer = interface
  ['{CA26F7AE-F1FE-41BE-9C23-723A687F60D1}']
    function JsonToObject(aType: TClass; const aJson: string): TObject; overload;
    function JsonToObject(aObject: TObject; const aJson: string): TObject; overload;
    function ObjectToJson(aObject : TObject; aIndent : Boolean = False): string;
    function ValueToJson(const aValue : TValue; aIndent : Boolean = False) : string;
    function Options : TSerializerOptions;
  end;

implementation

end.
