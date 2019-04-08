program ManageFlexArray;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Arrays;

type
  TUser = class
  private
    fName : string;
  public
    property Name : string read fName write fName;
  end;

var
  xarThings : TFlexArray;
  user : TUser;

begin
  try
    xarThings.Add(10);
    xarThings.Add('Hello');
    user := TUser.Create;
    try
      user.Name := 'Joe';
      xarThings.Add(user);

      cout('Integer Item = %d',[xarThings[0].AsInteger],etInfo);
      cout('String Item = %s',[xarThings[1].AsString],etInfo);
      cout('Record Item = %s',[TUser(xarThings[2]).Name],etInfo);
    finally
      user.Free;
    end;
    cout('Press <Enter> to Exit',ccYellow);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
