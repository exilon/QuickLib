program ConsoleMenu;

{$APPTYPE CONSOLE}

{$MODE DELPHI}

{$R *.res}

uses
  SysUtils,
  Windows,
  Quick.Commons,
  Quick.Console;

type
  TTest = class
    class procedure Option1;
    class procedure Option2;
    class procedure Option3;
    class procedure Option4;
    class procedure Option5;
  end;

var
  conmenu : TConsoleMenu;
  Test : TTest;
  menuop : TConsoleMenuOption;
  i : Integer;

{ TTest }

class procedure TTest.Option1;
begin
  coutXY(10,10,'Option 1 pressed',etInfo);
end;

class procedure TTest.Option2;
begin
  coutXY(10,10,'Option 2 pressed',etInfo);
end;

class procedure TTest.Option3;
begin
  coutXY(10,10,'Option 3 pressed',etInfo);
end;

class procedure TTest.Option4;
begin
  coutXY(10,10,'Option 4 pressed',etInfo);
end;

class procedure TTest.Option5;
begin
  coutXY(10,10,'Option 5 pressed',etInfo);
end;

begin
  try
    conmenu := TConsoleMenu.Create;
    menuop.Caption := 'Option 1';
    menuop.Key := VK_F1;
    menuop.OnKeyPressed := Test.Option1;
    conmenu.AddMenu(menuop);
    conmenu.AddMenu('Option 2',VK_F2,Test.Option2);
    conmenu.AddMenu('Option 3',VK_F3,Test.Option3);

    conmenu.AddMenu('Option 4',VK_F4,Test.Option4);

    conmenu.AddMenu('Option 5',VK_F5,Test.Option5);

    for i := 0 to 30 do writeln('hola que tal');

    conmenu.WaitForKeys;
    conmenu.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
