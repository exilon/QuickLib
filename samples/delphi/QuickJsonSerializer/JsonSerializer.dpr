program JsonSerializer;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'C:\Users\Kike\Documents\Embarcadero\Studio\Projects\JsonUtilsTest\main.pas' {Form1},
  jsonit in 'C:\Users\Kike\Documents\Embarcadero\Studio\Projects\JsonUtilsTest\jsonit.pas',
  Quick.Json.Serializer in 'C:\Users\Kike\Documents\Embarcadero\Studio\Projects\JsonUtilsTest\Quick.Json.Serializer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
