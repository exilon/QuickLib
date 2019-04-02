unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Quick.Commons, Quick.SysInfo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts;

type
  TMainForm = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Label2: TLabel;
    lblDeviceName: TLabel;
    Layout3: TLayout;
    Label3: TLabel;
    lblAppPath: TLabel;
    Layout4: TLayout;
    Label5: TLabel;
    lblOS: TLabel;
    Layout5: TLayout;
    Label7: TLabel;
    lblAppName: TLabel;
    Layout6: TLayout;
    Label4: TLabel;
    lblAppVersion: TLabel;
    Layout7: TLayout;
    Label1: TLabel;
    lblUserName: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  lblDeviceName.Text := SystemInfo.HostName;
  lblUserName.Text := SystemInfo.UserName;
  lblOS.Text := SystemInfo.OsVersion;
  lblAppPath.Text := SystemInfo.AppPath;
  lblAppName.Text := SystemInfo.AppName;
  lblAppVersion.Text := SystemInfo.AppVersion;
end;

end.
