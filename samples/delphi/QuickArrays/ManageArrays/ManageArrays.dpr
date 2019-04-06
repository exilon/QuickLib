program ManageArrays;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Arrays;

type

  TUser = record
    Name : string;
    Age : Integer;
  end;

var
  userarray : TXArray<TUser>;
  user : TUser;
  normalarray : TArray<TUser>;

begin
  try
    ReportMemoryLeaksOnShutdown := True;

    user.Name := 'Joe';
    user.Age := 30;
    userarray.Add(user);
    user.Name := 'Peter';
    user.Age := 32;
    userarray.Add(user);
    user.Name := 'James';
    user.Age := 40;
    userarray.Add(user);

    if userarray.Contains(user) then cout('found user in array',etInfo);

    for user in userarray do
    begin
      coutFmt('User: %s',[user.Name],etInfo);
    end;

    normalarray := userarray;

    coutFmt('Copied array value 1: %s',[normalarray[1].Name],etInfo);

    cout('Press <Enter> to Exit',ccYellow);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
