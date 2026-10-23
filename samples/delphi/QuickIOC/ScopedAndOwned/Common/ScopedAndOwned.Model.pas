unit ScopedAndOwned.Model;

{ The model shared by the Horse and the VCL samples.

  There is no database: TFakeDatabase keeps in memory what was committed, and TFakeUnitOfWork plays
  the part of a database connection with its own transaction. Every unit of work, repository and
  service gets a number when it is created, so the output shows which instance each one received.

  Two services save a customer and an audit entry with the SAME two repositories, both scoped:
  - TOwnedAuditService asks for the audit repository as IOwned<IAuditRepository>: it is built in a
    scope of its own, with a unit of work of its own, outside the service's transaction;
  - TSameTransactionAuditService asks for both repositories directly: everything comes from the scope that
    resolved the service, so both repositories share one unit of work and one transaction.
  Only the constructors are different. }

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Quick.IOC;

type
  /// Where the instances report what happens to them. Singleton; the host decides where the text
  /// goes (the console in the Horse sample, a memo in the VCL sample).
  IActivityLog = interface
  ['{0B6E2C44-7A1F-4C1D-9E35-5D8A2F6B1C90}']
    procedure Write(const aText: string);
  end;

  TActivityLog = class(TInterfacedObject, IActivityLog)
  private
    fWriter: TProc<string>;
  public
    constructor Create(const aWriter: TProc<string>);
    procedure Write(const aText: string);
  end;

  /// What was committed, in memory. Singleton: shared by every scope and every thread.
  IFakeDatabase = interface
  ['{4F1C8E27-3B6D-4A90-8C52-E7D1A9B3F046}']
    procedure Apply(const aTable, aRow: string);
    function Rows(const aTable: string): TArray<string>;
  end;

  TFakeDatabase = class(TInterfacedObject, IFakeDatabase)
  private
    fLock: TCriticalSection;
    fRows: TStringList; // "table=row"
  public
    constructor Create;
    destructor Destroy; override;
    procedure Apply(const aTable, aRow: string);
    function Rows(const aTable: string): TArray<string>;
  end;

  /// A database connection with its own transaction. Scoped: one per scope, released with it.
  IUnitOfWork = interface
  ['{9D3A7F18-2E4B-4C65-B0A9-61C8E5D2F7A3}']
    function Id: Integer;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    /// Inside a transaction the change waits for Commit; outside one it is applied at once
    /// (autocommit), as a database does.
    procedure Execute(const aTable, aRow: string);
  end;

  TFakeUnitOfWork = class(TInterfacedObject, IUnitOfWork)
  private
    fId: Integer;
    fDatabase: IFakeDatabase;
    fLog: IActivityLog;
    fInTransaction: Boolean;
    fPending: TStringList; // "table=row", waiting for Commit
  public
    constructor Create(aDatabase: IFakeDatabase; aLog: IActivityLog);
    destructor Destroy; override;
    function Id: Integer;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    procedure Execute(const aTable, aRow: string);
  end;

  ICustomerRepository = interface
  ['{2C7E5B91-8F3A-4D06-A4E2-B9D0C6F1E583}']
    function Id: Integer;
    function UnitOfWorkId: Integer;
    procedure Insert(const aName: string);
  end;

  IAuditRepository = interface
  ['{E6A1D3F8-5C2B-4E97-8B14-0F7C9A2D6E35}']
    function Id: Integer;
    function UnitOfWorkId: Integer;
    procedure Register(const aText: string);
  end;

  TCustomerRepository = class(TInterfacedObject, ICustomerRepository)
  private
    fId: Integer;
    fUnitOfWork: IUnitOfWork;
  public
    constructor Create(aUnitOfWork: IUnitOfWork);
    function Id: Integer;
    function UnitOfWorkId: Integer;
    procedure Insert(const aName: string);
  end;

  TAuditRepository = class(TInterfacedObject, IAuditRepository)
  private
    fId: Integer;
    fUnitOfWork: IUnitOfWork;
  public
    constructor Create(aUnitOfWork: IUnitOfWork);
    function Id: Integer;
    function UnitOfWorkId: Integer;
    procedure Register(const aText: string);
  end;

  /// What a save reports: which instances did the work, and on which unit of work.
  TSaveResult = record
    Service: string;
    RolledBack: Boolean;
    ServiceUnitOfWork: Integer;
    CustomerRepository: Integer;
    CustomerUnitOfWork: Integer;
    AuditRepository: Integer;
    AuditUnitOfWork: Integer;
    function ToText: string;
  end;

  /// Saves a customer and its audit entry in one transaction; with aRollback, the transaction is
  /// rolled back at the end instead of committed.
  ICustomerService = interface
  ['{7B2D9E40-1A6C-4F83-9D57-C3E8A0B4F162}']
    function Save(const aName: string; aRollback: Boolean): TSaveResult;
  end;

  /// The audit repository comes from an IOwned: its own scope, its own unit of work.
  IOwnedAuditService = interface(ICustomerService)
  ['{5A8F3C16-9E2D-4B70-A1C4-D6E9F2B83057}']
  end;

  /// Both repositories share the unit of work of the scope that resolved the service.
  ISameTransactionAuditService = interface(ICustomerService)
  ['{C3F6A9D2-4B1E-4087-95E3-2A7D8C0F6B14}']
  end;

  TOwnedAuditService = class(TInterfacedObject, IOwnedAuditService, ICustomerService)
  private
    fUnitOfWork: IUnitOfWork;
    fCustomers: ICustomerRepository;
    // the IOwned is kept, not only its Value: its scope lives while the IOwned is referenced
    fAudit: IOwned<IAuditRepository>;
  public
    constructor Create(aUnitOfWork: IUnitOfWork; aCustomers: ICustomerRepository;
      aAudit: IOwned<IAuditRepository>);
    function Save(const aName: string; aRollback: Boolean): TSaveResult;
  end;

  TSameTransactionAuditService = class(TInterfacedObject, ISameTransactionAuditService, ICustomerService)
  private
    fUnitOfWork: IUnitOfWork;
    fCustomers: ICustomerRepository;
    fAudit: IAuditRepository;
  public
    constructor Create(aUnitOfWork: IUnitOfWork; aCustomers: ICustomerRepository;
      aAudit: IAuditRepository);
    function Save(const aName: string; aRollback: Boolean): TSaveResult;
  end;

implementation

var
  //each class numbers its instances
  UnitOfWorkCount: Integer;
  CustomerRepositoryCount: Integer;
  AuditRepositoryCount: Integer;

{ TActivityLog }

constructor TActivityLog.Create(const aWriter: TProc<string>);
begin
  inherited Create;
  fWriter := aWriter;
end;

procedure TActivityLog.Write(const aText: string);
begin
  fWriter(aText);
end;

{ TFakeDatabase }

constructor TFakeDatabase.Create;
begin
  inherited Create;
  fLock := TCriticalSection.Create;
  fRows := TStringList.Create;
end;

destructor TFakeDatabase.Destroy;
begin
  fRows.Free;
  fLock.Free;
  inherited;
end;

procedure TFakeDatabase.Apply(const aTable, aRow: string);
begin
  fLock.Enter;
  try
    fRows.Add(aTable + '=' + aRow);
  finally
    fLock.Leave;
  end;
end;

function TFakeDatabase.Rows(const aTable: string): TArray<string>;
var
  i: Integer;
begin
  Result := nil;
  fLock.Enter;
  try
    for i := 0 to fRows.Count - 1 do
      if fRows.Names[i] = aTable then Result := Result + [fRows.ValueFromIndex[i]];
  finally
    fLock.Leave;
  end;
end;

{ TFakeUnitOfWork }

constructor TFakeUnitOfWork.Create(aDatabase: IFakeDatabase; aLog: IActivityLog);
begin
  inherited Create;
  fId := TInterlocked.Increment(UnitOfWorkCount);
  fDatabase := aDatabase;
  fLog := aLog;
  fPending := TStringList.Create;
  fLog.Write(Format('  UnitOfWork #%d opened (a new connection)', [fId]));
end;

destructor TFakeUnitOfWork.Destroy;
begin
  if fInTransaction then Rollback;
  fPending.Free;
  fLog.Write(Format('  UnitOfWork #%d closed: released by its scope', [fId]));
  inherited;
end;

function TFakeUnitOfWork.Id: Integer;
begin
  Result := fId;
end;

procedure TFakeUnitOfWork.StartTransaction;
begin
  fInTransaction := True;
end;

procedure TFakeUnitOfWork.Commit;
var
  i: Integer;
begin
  for i := 0 to fPending.Count - 1 do
    fDatabase.Apply(fPending.Names[i], fPending.ValueFromIndex[i]);
  fPending.Clear;
  fInTransaction := False;
  fLog.Write(Format('  UnitOfWork #%d: transaction committed', [fId]));
end;

procedure TFakeUnitOfWork.Rollback;
begin
  fPending.Clear;
  fInTransaction := False;
  fLog.Write(Format('  UnitOfWork #%d: transaction rolled back', [fId]));
end;

procedure TFakeUnitOfWork.Execute(const aTable, aRow: string);
begin
  if fInTransaction then fPending.Add(aTable + '=' + aRow)
    else fDatabase.Apply(aTable, aRow);
end;

{ TCustomerRepository }

constructor TCustomerRepository.Create(aUnitOfWork: IUnitOfWork);
begin
  inherited Create;
  fId := TInterlocked.Increment(CustomerRepositoryCount);
  fUnitOfWork := aUnitOfWork;
end;

function TCustomerRepository.Id: Integer;
begin
  Result := fId;
end;

function TCustomerRepository.UnitOfWorkId: Integer;
begin
  Result := fUnitOfWork.Id;
end;

procedure TCustomerRepository.Insert(const aName: string);
begin
  fUnitOfWork.Execute('customers', aName);
end;

{ TAuditRepository }

constructor TAuditRepository.Create(aUnitOfWork: IUnitOfWork);
begin
  inherited Create;
  fId := TInterlocked.Increment(AuditRepositoryCount);
  fUnitOfWork := aUnitOfWork;
end;

function TAuditRepository.Id: Integer;
begin
  Result := fId;
end;

function TAuditRepository.UnitOfWorkId: Integer;
begin
  Result := fUnitOfWork.Id;
end;

procedure TAuditRepository.Register(const aText: string);
begin
  fUnitOfWork.Execute('audit', aText);
end;

{ TSaveResult }

function TSaveResult.ToText: string;
const
  ENDING: array[Boolean] of string = ('committed', 'rolled back');
begin
  Result := Format('%s: transaction %s' + sLineBreak +
    '  service             -> UnitOfWork #%d' + sLineBreak +
    '  CustomerRepository #%d -> UnitOfWork #%d' + sLineBreak +
    '  AuditRepository #%d    -> UnitOfWork #%d',
    [Service, ENDING[RolledBack], ServiceUnitOfWork, CustomerRepository, CustomerUnitOfWork,
     AuditRepository, AuditUnitOfWork]);
end;

/// The body of both services: only where the audit repository comes from is different.
function SaveCustomer(const aService: string; aUnitOfWork: IUnitOfWork;
  aCustomers: ICustomerRepository; aAudit: IAuditRepository; const aName: string;
  aRollback: Boolean): TSaveResult;
begin
  if aName.Trim.IsEmpty then raise EArgumentException.Create('The name is required');
  Result.Service := aService;
  Result.RolledBack := aRollback;
  Result.ServiceUnitOfWork := aUnitOfWork.Id;
  Result.CustomerRepository := aCustomers.Id;
  Result.CustomerUnitOfWork := aCustomers.UnitOfWorkId;
  Result.AuditRepository := aAudit.Id;
  Result.AuditUnitOfWork := aAudit.UnitOfWorkId;
  aUnitOfWork.StartTransaction;
  try
    aCustomers.Insert(aName);
    aAudit.Register(Format('customer "%s" saved by %s', [aName, aService]));
    if aRollback then aUnitOfWork.Rollback
      else aUnitOfWork.Commit;
  except
    aUnitOfWork.Rollback;
    raise;
  end;
end;

{ TOwnedAuditService }

constructor TOwnedAuditService.Create(aUnitOfWork: IUnitOfWork; aCustomers: ICustomerRepository;
  aAudit: IOwned<IAuditRepository>);
begin
  inherited Create;
  fUnitOfWork := aUnitOfWork; // from the scope that resolved the service
  fCustomers := aCustomers;   // from the same scope: the same unit of work
  fAudit := aAudit;           // a scope of its own: another unit of work
end;

function TOwnedAuditService.Save(const aName: string; aRollback: Boolean): TSaveResult;
begin
  Result := SaveCustomer('OwnedAuditService', fUnitOfWork, fCustomers, fAudit.Value, aName, aRollback);
end;

{ TSameTransactionAuditService }

constructor TSameTransactionAuditService.Create(aUnitOfWork: IUnitOfWork; aCustomers: ICustomerRepository;
  aAudit: IAuditRepository);
begin
  inherited Create;
  fUnitOfWork := aUnitOfWork; // the three references point to the same unit of work
  fCustomers := aCustomers;
  fAudit := aAudit;
end;

function TSameTransactionAuditService.Save(const aName: string; aRollback: Boolean): TSaveResult;
begin
  Result := SaveCustomer('SameTransactionAuditService', fUnitOfWork, fCustomers, fAudit, aName, aRollback);
end;

end.
