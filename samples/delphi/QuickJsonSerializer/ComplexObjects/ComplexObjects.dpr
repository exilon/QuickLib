program ComplexObjects;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  System.SysUtils,
  Quick.Console,
  Quick.Json.Serializer;

type
  TUser = class
  private
    fName : string;
    fAge : Integer;
    fPhoto : TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Name : string read fName write fName;
    property Age : Integer read fAge write fAge;
    property Photo : TMemoryStream read fPhoto write fPhoto;
  end;

var
  json : string;
  user : TUser;
  photosize : Integer;
  serializer : TJsonSerializer;
{ TUser }

constructor TUser.Create;
begin
  fPhoto := TMemoryStream.Create;
end;

destructor TUser.Destroy;
begin
  fPhoto.Free;
  inherited;
end;

begin
  try
    user := TUser.Create;
    user.Name := 'John';
    user.Age := 30;
    user.Photo.LoadFromFile('.\photo.jpg');
    photosize := user.Photo.Size;
    cout('Photo Size: ' + photosize.ToString,ccWhite);

    serializer := TJsonSerializer.Create(TSerializeLevel.slPublicProperty,True);
    try
      //serializer.UseBase64Stream := False;
      //serialize
      cout('Serialize User:',ccYellow);
      json := serializer.ObjectToJson(user);
      //cout(json,ccWhite);
      user.Free;
      //deserialize
      cout('Deserialize User:',ccYellow);
      user := TUser.Create;
      serializer.JsonToObject(user,json);
      //check if deseralization is correct
      Assert(user.Name = 'John','Name serialize error!');
      Assert(user.Age = 30,'Age serialize error!');
      Assert(user.Photo.Size = photosize,'Photo serialize error!');
      user.Photo.SaveToFile('.\photo_new.jpg');
      cout('Photo Size: ' + photosize.ToString,ccWhite);
      cout(user.Name,ccWhite);
    finally
      serializer.Free;
    end;
    user.Free;
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

