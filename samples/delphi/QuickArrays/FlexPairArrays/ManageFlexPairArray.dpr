program ManageFlexPairArray;

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
  flexarray : TFlexPairArray;
  user : TUser;

begin
  try
    flexarray.Add('onenumber',10);
    flexarray.Add('other','Hello boy!');
    user := TUser.Create;
    try
      user.Name := 'Joe';
      flexarray.Add('myuser',user);

      cout('Integer Item = %d',[flexarray.GetValue('onenumber').AsInteger],etInfo);
      cout('String Item = %s',[flexarray.GetValue('other').AsString],etInfo);
      cout('Record Item = %s',[TUser(flexarray.GetValue('myuser')).Name],etInfo);
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
