unit ScopedAndOwned.MainForm;

{ The main screen. Like every form, it uses the application's scope (scope A): resolving the unit of
  work when it is created opens the application's connection, which every screen without a scope of
  its own shares. From here, screens are opened in scope A (the default) or with a scope of their own. }

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  ScopedAndOwned.Model;

type
  TMainForm = class(TForm)
    LabelIntro: TLabel;
    LabelScope: TLabel;
    CheckOwnScope: TCheckBox;
    CheckOwnedAudit: TCheckBox;
    ButtonOpen: TButton;
    ButtonDatabase: TButton;
    MemoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonDatabaseClick(Sender: TObject);
  private
    fConnection: IUnitOfWork; // the application's unit of work, from scope A
  public
    procedure Log(const aText: string);
  end;

var
  MainForm: TMainForm;

implementation

uses
  Quick.IOC,
  ScopedAndOwned.ApplicationScope,
  ScopedAndOwned.ScreenForm;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LabelIntro.Caption :=
    'The application''s scope (scope A) is created when the application starts and freed when it ends. ' +
    'Every screen uses it by default, so they share its unit of work, as this main screen does. A screen ' +
    'can be opened with a scope of its own, disconnected from scope A; and a service can ask for the ' +
    'audit repository in an IOwned, which gets a scope of its own too.';
  Log('Application started: scope A created');
  fConnection := ApplicationScope.Resolve<IUnitOfWork>;
  LabelScope.Caption := Format('Application''s scope (scope A): UnitOfWork #%d', [fConnection.Id]);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fConnection := nil;
  MainForm := nil; // the activity log stops writing here
end;

procedure TMainForm.Log(const aText: string);
begin
  MemoLog.Lines.Add(aText);
end;

procedure TMainForm.ButtonOpenClick(Sender: TObject);
var
  kind: TAuditKind;
begin
  if CheckOwnedAudit.Checked then kind := akOwned
    else kind := akSameTransaction;
  if CheckOwnScope.Checked then TScreenForm.CreateWithOwnScope(Application, kind).Show
    else TScreenForm.CreateFor(Application, kind).Show; // the application's scope, by default
end;

procedure TMainForm.ButtonDatabaseClick(Sender: TObject);
var
  database: IFakeDatabase;
  row: string;
begin
  database := GlobalContainer.Resolve<IFakeDatabase>; // a singleton: no scope needed
  Log('');
  Log('In the database (committed):');
  Log('  customers: ' + string.Join(', ', database.Rows('customers')));
  for row in database.Rows('audit') do Log('  audit: ' + row);
end;

end.
