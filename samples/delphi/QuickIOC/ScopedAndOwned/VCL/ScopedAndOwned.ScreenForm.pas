unit ScopedAndOwned.ScreenForm;

{ A screen that saves a customer. By default it uses the application's scope (scope A), as every form
  of the application does: its service, repositories and unit of work are the application's ones. It
  can be created with a scope of its own instead, disconnected from scope A: then its scoped instances
  are its own, and are released when the screen closes. }

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Quick.IOC,
  ScopedAndOwned.Model;

type
  TAuditKind = (akOwned, akSameTransaction);

  TScreenForm = class(TForm)
    LabelName: TLabel;
    EditName: TEdit;
    ButtonSave: TButton;
    CheckRollback: TCheckBox;
    LabelHint: TLabel;
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fNumber: Integer;
    fScope: TIocScope;
    fOwnsScope: Boolean;
    fService: ICustomerService;
    fLog: IActivityLog;
    procedure Setup(aKind: TAuditKind);
  public
    /// Uses aScope; by default (nil) the application's scope, scope A, as every form does.
    constructor CreateFor(aOwner: TComponent; aKind: TAuditKind; aScope: TIocScope = nil);
    /// Uses a new scope of its own, disconnected from the application's scope: its unit of work and
    /// repositories are its own, and are released when the screen closes.
    constructor CreateWithOwnScope(aOwner: TComponent; aKind: TAuditKind);
    destructor Destroy; override;
  end;

implementation

uses
  ScopedAndOwned.ApplicationScope;

{$R *.dfm}

var
  ScreenCount: Integer;

constructor TScreenForm.CreateFor(aOwner: TComponent; aKind: TAuditKind; aScope: TIocScope);
begin
  if aScope = nil then aScope := ApplicationScope;
  fScope := aScope;
  inherited Create(aOwner);
  Setup(aKind);
end;

constructor TScreenForm.CreateWithOwnScope(aOwner: TComponent; aKind: TAuditKind);
begin
  fScope := GlobalContainer.CreateScope;
  fOwnsScope := True;
  inherited Create(aOwner);
  Setup(aKind);
end;

procedure TScreenForm.Setup(aKind: TAuditKind);
const
  SCOPE_NAME: array[Boolean] of string = ('the application''s scope (scope A)', 'a scope of its own');
  CUSTOMER_GOES: array[Boolean] of string = (
    'The customer is saved in the unit of work of the application''s scope, the same one the main ' +
    'screen and every screen without a scope of its own use. ',
    'The customer is saved in the unit of work of this screen''s own scope, not the application''s. ');
  AUDIT_GOES: array[TAuditKind] of string = (
    'The audit entry goes to the unit of work of the IOwned''s scope, outside the transaction: it stays ' +
    'even after a rollback.',
    'The audit entry goes to the same unit of work, in the same transaction: a rollback discards both.');
begin
  Inc(ScreenCount);
  fNumber := ScreenCount;
  fLog := GlobalContainer.Resolve<IActivityLog>; // a singleton: no scope needed
  fLog.Write('');
  fLog.Write(Format('Screen #%d opened, in %s', [fNumber, SCOPE_NAME[fOwnsScope]]));
  LabelHint.Caption := CUSTOMER_GOES[fOwnsScope] + AUDIT_GOES[aKind];
  if aKind = akOwned then
  begin
    Caption := Format('Screen #%d: %s, audit in IOwned', [fNumber, SCOPE_NAME[fOwnsScope]]);
    // resolved in the screen's scope: the IOwned<IAuditRepository> it asks for opens another one
    fService := fScope.Resolve<IOwnedAuditService>;
  end
  else
  begin
    Caption := Format('Screen #%d: %s, audit in the same transaction', [fNumber, SCOPE_NAME[fOwnsScope]]);
    fService := fScope.Resolve<ISameTransactionAuditService>;
  end;
end;

destructor TScreenForm.Destroy;
begin
  if fLog <> nil then
  begin
    fLog.Write('');
    if fOwnsScope then fLog.Write(Format('Screen #%d closed: it releases its service and frees its own scope', [fNumber]))
      else fLog.Write(Format('Screen #%d closed: it releases its service; the application''s scope stays', [fNumber]));
  end;
  // released first: the service, and for akOwned its IOwned with the IOwned's scope
  fService := nil;
  // only a scope of its own is freed here: the application's scope lives as long as the application
  if fOwnsScope then fScope.Free;
  inherited;
end;

procedure TScreenForm.ButtonSaveClick(Sender: TObject);
begin
  fLog.Write('');
  fLog.Write(Format('Screen #%d: Save name=%s rollback=%s',
    [fNumber, EditName.Text, BoolToStr(CheckRollback.Checked, True)]));
  fLog.Write(fService.Save(EditName.Text, CheckRollback.Checked).ToText);
end;

procedure TScreenForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree; // the screen goes, with its service (and its own scope, if it has one)
end;

end.
