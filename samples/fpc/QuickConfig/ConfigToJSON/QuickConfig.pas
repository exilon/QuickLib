program QuickConfig;

uses
  Forms, Interfaces,
  uMain in 'uMain.pas' {MainForm};

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
