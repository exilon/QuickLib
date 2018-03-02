unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    btnChrono: TButton;
    lblElapsedTime: TLabel;
    lblElapsedTimeLong: TLabel;
    tiChrono: TTimer;
    Label3: TLabel;
    lblTimer: TLabel;
    lblShortFormat: TLabel;
    lblLongFormat: TLabel;
    cbPrecissionFormat: TComboBox;
    Label1: TLabel;
    procedure btnChronoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tiChronoTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  StartDate : TDateTime;

implementation

uses
  Quick.Chrono;

var
  crono : TChronometer;
  hola : string;

{$R *.dfm}

procedure TMainForm.btnChronoClick(Sender: TObject);
begin
  if btnChrono.Caption = 'Start' then
  begin
    crono.ReportFormatPrecission := TPrecissionFormat(cbPrecissionFormat.ItemIndex);
    StartDate := Now();
    tiChrono.Enabled := True;
    crono.Start;
    btnChrono.Caption := 'Stop';
  end
  else
  begin
    crono.Stop;
    tiChrono.Enabled := False;
    btnChrono.Caption := 'Start';
    lblElapsedTime.Caption := crono.ElapsedTime;
    lblElapsedTimeLong.Caption := crono.ElapsedTime(True);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  crono := TChronometer.Create(False);
end;

procedure TMainForm.tiChronoTimer(Sender: TObject);
var
  newtime : string;
begin
  tiChrono.Enabled := False;
  try
    newtime := FormatDateTime('hh:mm:ss',Now()-StartDate);
    if newtime <> lblTimer.Caption then lblTimer.Caption := newtime;
  finally
    tiChrono.Enabled := True;
  end;
end;

end.
