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
  ISerializer = interface
  ['{CA26F7AE-F1FE-41BE-9C23-723A687F60D1}']
    function JsonToObject(aType: TClass; const aJson: string): TObject; overload;
    function JsonToObject(aObject: TObject; const aJson: string): TObject; overload;
    function ObjectToJson(aObject : TObject; aIndent : Boolean = False): string;
    function ValueToJson(const aValue : TValue; aIndent : Boolean = False) : string;
  end;

implementation

end.
