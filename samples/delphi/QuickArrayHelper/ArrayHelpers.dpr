program ArrayHelpers;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Arrays.Helper;

var

  myarray : TArray<string>;

begin
  try
    ReportMemoryLeaksOnShutdown := True;

    myarray.Add('one');
    myarray.Add('two');
    myarray.Add('three');
    coutFmt('count: %d',[myarray.Count],etInfo);
    if myarray.Contains('two') then cout('found "two" in array',etInfo)
      else cout('not found',etInfo);

    coutFmt('"three" in position %d',[myarray.IndexOf('three')],etInfo);

    TArrayHelper<string>.Add(myarray,'Four');

    cout('Press <Enter> to Exit',ccYellow);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
