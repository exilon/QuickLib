program QuickConsoleMenu;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

{$R *.res}

uses
  SysUtils,
  Windows,
  Quick.Commons,
  Quick.Console;

type
  TTest = record
    procedure Option1;
    procedure Option2;
    procedure Option3;
    procedure Option4;
    procedure Option5;
  end;

var
  conmenu : TConsoleMenu;
  Test : TTest;
  menuop : TConsoleMenuOption;
  i : Integer;

{ TTest }

procedure TTest.Option1;
begin
  coutXY(10,10,'Option 1 pressed',etInfo);
end;

procedure TTest.Option2;
begin
  coutXY(10,10,'Option 2 pressed',etInfo);
end;

procedure TTest.Option3;
begin
  coutXY(10,10,'Option 3 pressed',etInfo);
end;

procedure TTest.Option4;
begin
  coutXY(10,10,'Option 4 pressed',etInfo);
end;

procedure TTest.Option5;
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

    conmenu.AddMenu('Option 5',VK_F5,{$IFDEF FPC}Test.Option5);{$ELSE}procedure
                                 begin
                                   coutXY(10,10,'Option 5 pressed',etInfo);
                                 end);
                                 {$ENDIF}

    conmenu.WaitForKeys;
    conmenu.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
