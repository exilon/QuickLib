program ArrayHelpers;

{$Mode delphi}

uses
  SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Arrays.Helper;

var

  myarray : TStringArray;

begin
  try
    myarray.Add('one');
    myarray.Add('two');
    myarray.Add('three');
    coutFmt('count: %d',[myarray.Count],etInfo);
    if myarray.Contains('two') then cout('found "two" in array',etInfo)
      else cout('not found',etInfo);

    coutFmt('"three" in position %d',[myarray.IndexOf('three')],etInfo);

    TArrayHelper<string>.Add(myarray,'four');

    cout('Press <Enter> to Exit',ccYellow);

    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
