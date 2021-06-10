program ObjectToJsonStream;

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
  public
    property Name : string read fName write fName;
    property Age : Integer read fAge write fAge;
  end;

var
  ss : TStringStream;
  user : TUser;
  serializer : TJsonSerializer;
begin
  try
    user := TUser.Create;
    user.Name := 'John';
    user.Age := 30;
    ss := TStringStream.Create;
    try
      serializer := TJsonSerializer.Create(TSerializeLevel.slPublicProperty,True);
      try
        serializer.ObjectToJsonStream(user,ss);
        cout(ss.DataString,ccWhite);

        user.Name := 'Peter';
        serializer.JsonStreamToObject(user,ss);
        Assert(user.Name = 'John','Serializer not modified property!');
        cout(user.Name,ccWhite);
      finally
        serializer.Free;
      end;
    finally
      ss.Free;
    end;
    user.Free;
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
