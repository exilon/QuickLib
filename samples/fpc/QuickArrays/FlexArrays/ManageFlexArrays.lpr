program ManageFlexArrays;

{$MODE DELPHI}

uses
  SysUtils,
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
  flexarray : TFlexArray;
  user : TUser;

begin
  try
    flexarray.Add(10);
    flexarray.Add('Hello');
    user := TUser.Create;
    try
      user.Name := 'Joe';
      flexarray.Add(user);

      cout('Integer Item = %d',[flexarray[0].AsInteger],etInfo);
      cout('String Item = %s',[flexarray[1].AsString],etInfo);
      cout('Record Item = %s',[TUser(flexarray[2].AsObject).Name],etInfo);
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
