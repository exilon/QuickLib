unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Quick.Config, Vcl.StdCtrls;

type

  TWinPos = record
  public
    PosX : Integer;
    PosY : Integer;
  end;

  TMyConfig = class(TAppConfig)
  public
    LastFilename : string;
    WindowPos : TWinPos;
  end;

  TMainForm = class(TForm)
    meInfo: TMemo;
    btnLoad: TButton;
    btnSave: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  MyConfig : TMyConfig;

implementation

{$R *.dfm}

procedure TMainForm.btnLoadClick(Sender: TObject);
begin
  MyConfig.Load(True);
  meInfo.Lines.Text := MyConfig.AsJsonString;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  MyConfig.LastFilename := 'notes.txt';
  MyConfig.WindowPos.PosX := 200;
  MyConfig.WindowPos.PosX := 100;
  MyConfig.Save;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MyConfig := TMyConfig.Create('prueba.json');
end;

end.
