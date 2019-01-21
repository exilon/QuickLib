unit Chrono;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Quick.Commons,
  Quick.Chrono, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Label1: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  crono : TChronometer;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Button1.Text = 'Start' then
  begin
    crono.Start;
    Button1.Text := 'Stop';
  end
  else
  begin
    crono.Stop;
    Button1.Text := 'Start';
    Label1.Text := crono.ElapsedTime(True);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  crono := TChronometer.Create(False);
end;

end.
