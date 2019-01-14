program RunScheduledTasks;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
