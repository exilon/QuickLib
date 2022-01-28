program Url_Manipulation;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console;

const
  aurls : array[0..4] of string = ('https://mydomain.com',
                            'http://www.google.com/Test/other',
                            'www.google.com/test/other?query=1&other=2',
                            'http://127.0.0.1:80/onemoretest/',
                            'www.google.com');

var
  i : Integer;
  host : string;
  path : string;
  query : string;
  woquery : string;
begin
  try
    for i := Low(aurls) to High(aurls) do
    begin
      cout('URL="%s"',[aurls[i]],etWarning);
      host := UrlGetHost(aurls[i]);
      path := UrlGetPath(aurls[i]);
      query := UrlGetQuery(aurls[i]);
      woquery := UrlRemovequery(aurls[i]);
      cout('Host="%s"',[host],etInfo);
      cout('Path="%s"',[path],etInfo);
      cout('Query="%s"',[query],etInfo);
      cout('Without query="%s"',[woquery],etInfo);
      cout('------------',etWarning);
    end;
    ConsoleWaitForEnterKey;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
