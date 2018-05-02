program QuickConfigDemo;

uses
  Forms, Interfaces,
  uMain in 'uMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
