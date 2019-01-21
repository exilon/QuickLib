program ChronoCheck;

uses
  System.StartUpCopy,
  FMX.Forms,
  Chrono in 'Chrono.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
