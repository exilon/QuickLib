unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls, OAuth.GMail, Quick.OAuth, FMX.Objects;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    efClientID: TEdit;
    Label2: TLabel;
    efSecretID: TEdit;
    Label3: TLabel;
    efCallbackURL: TEdit;
    Label5: TLabel;
    btnAuthorise: TButton;
    lbAccessToken: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    btnRefresh: TButton;
    cbUseExisting: TCheckBox;
    Label4: TLabel;
    crcAuthorise: TCircle;
    crcRefresh: TCircle;
    cbMakeTokenExpire: TCheckBox;
    RoundRect1: TRoundRect;
    RoundRect2: TRoundRect;
    lbExpiration: TLabel;
    RoundRect3: TRoundRect;
    lbRefreshToken: TLabel;
    RoundRect4: TRoundRect;
    lbReloadedToken: TLabel;
    Label8: TLabel;
    procedure btnAuthoriseClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fGMail: TOAuthGMail;
    fRetrieved: TDateTime;

    procedure SaveCredentials (const aToken: TOAuthToken);
    procedure LoadCredentials (var aToken: TOAuthToken);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.DateUtils;

{$R *.fmx}

procedure TForm1.btnAuthoriseClick(Sender: TObject);
begin
  crcAuthorise.Fill.Color:=TAlphaColorRec.Red;
  FreeAndNil(fGMail);
  fGMail:=TOAuthGMail.Create(efClientID.Text, efSecretID.Text);
  fGMail.CallbackURL:=efCallbackURL.Text;
  fGMail.OnSaveToken:=SaveCredentials;
  fGMail.OnLoadToken:=LoadCredentials;
  try
    fGMail.Authorize(procedure (const aToken: TOAuthToken)
                     begin
                       crcAuthorise.Fill.Color:=TAlphaColorRec.Green;
                     end);
  except

  end;
end;

procedure TForm1.btnRefreshClick(Sender: TObject);
begin
  crcRefresh.Fill.Color:=TAlphaColorRec.Red;
  fGMail.RefreshToken(procedure (const aToken: TOAuthToken)
                      begin
                        crcRefresh.Fill.Color:=TAlphaColorRec.Green;
                        lbReloadedToken.Text:=fGMail.AccessToken;
                      end);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fGMail.Free;
end;

procedure TForm1.LoadCredentials(var aToken: TOAuthToken);
begin
  if cbUseExisting.IsChecked then
  begin
    aToken.AccessToken:=lbAccessToken.Text;
    aToken.AccessTokenExpiration:=lbExpiration.Text.ToInteger;
    aToken.RefreshToken:=lbRefreshToken.Text;
    aToken.RetrieveDateTime:=fRetrieved;
  end
  else
  begin
    aToken.AccessToken:='';
    aToken.AccessTokenExpiration:=0;
    aToken.RefreshToken:='';
    aToken.RetrieveDateTime:=IncMinute(Now, -100);
  end;
  if cbMakeTokenExpire.IsChecked then
    aToken.RetrieveDateTime:=IncMinute(Now, -100);
end;

procedure TForm1.SaveCredentials(const aToken: TOAuthToken);
begin
  lbAccessToken.Text:=aToken.AccessToken;
  lbExpiration.Text:=aToken.AccessTokenExpiration.ToString;
  lbRefreshToken.Text:=aToken.RefreshToken;
  fRetrieved:=aToken.RetrieveDateTime;
end;

end.
