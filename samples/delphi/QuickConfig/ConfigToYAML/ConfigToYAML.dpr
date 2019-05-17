program ConfigToYAML;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  Quick.Config.YAML in '..\..\..\..\Quick.Config.YAML.pas',
  Quick.YAML.Serializer in '..\..\..\..\Quick.YAML.Serializer.pas',
  Quick.YAML in '..\..\..\..\Quick.YAML.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
