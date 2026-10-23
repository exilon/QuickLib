unit ScopedAndOwned.Routes;

{ The routes of the Horse sample. Each one resolves what it needs from the scope of its request:

    POST /owned?name=Ann[&rollback=true]
      TOwnedAuditService: the audit entry has its own unit of work, outside the transaction
    POST /same-transaction?name=Bob[&rollback=true]
      TSameTransactionAuditService: one unit of work, one transaction for both entries
    GET  /database
      what was committed }

interface

procedure RegisterRoutes;

implementation

uses
  System.SysUtils,
  System.JSON,
  Horse,
  Quick.IOC,
  ScopedAndOwned.Model,
  ScopedAndOwned.ScopePerRequest;

function QueryValue(Req: THorseRequest; const aName: string): string;
begin
  if not Req.Query.TryGetValue(aName, Result) then Result := '';
end;

procedure Log(const aText: string);
begin
  GlobalContainer.Resolve<IActivityLog>.Write(aText); // a singleton: no scope needed
end;

procedure SendJSON(Res: THorseResponse; aJSON: TJSONValue; aStatus: Integer);
begin
  try
    Res.ContentType('application/json; charset=utf-8');
    Res.Send(aJSON.ToJSON).Status(aStatus);
  finally
    aJSON.Free;
  end;
end;

function SaveResultToJSON(const aResult: TSaveResult): TJSONObject;
var
  unitOfWork: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('service', aResult.Service);
  Result.AddPair('rolledBack', TJSONBool.Create(aResult.RolledBack));
  unitOfWork := TJSONObject.Create;
  unitOfWork.AddPair('service', TJSONNumber.Create(aResult.ServiceUnitOfWork));
  unitOfWork.AddPair('customerRepository', TJSONNumber.Create(aResult.CustomerUnitOfWork));
  unitOfWork.AddPair('auditRepository', TJSONNumber.Create(aResult.AuditUnitOfWork));
  Result.AddPair('unitOfWork', unitOfWork);
end;

/// The same for both save routes: saves and answers.
procedure Save(aService: ICustomerService; Req: THorseRequest; Res: THorseResponse);
var
  saved: TSaveResult;
begin
  try
    saved := aService.Save(QueryValue(Req, 'name'), SameText(QueryValue(Req, 'rollback'), 'true'));
    Log(saved.ToText);
    SendJSON(Res, SaveResultToJSON(saved), 201);
  except
    on E: EArgumentException do
      SendJSON(Res, TJSONObject.Create(TJSONPair.Create('error', E.Message)), 400);
  end;
end;

procedure SaveWithOwnedAudit(Req: THorseRequest; Res: THorseResponse);
var
  service: IOwnedAuditService;
begin
  Log('');
  Log(Format('POST %s name=%s rollback=%s', [Req.PathInfo, QueryValue(Req, 'name'), QueryValue(Req, 'rollback')]));
  // resolved in the request scope: its unit of work and customer repository are the request's,
  // and the IOwned<IAuditRepository> it asks for opens a scope of its own
  service := RequestScope(Req).Resolve<IOwnedAuditService>;
  Save(service, Req, Res);
  // releasing the service releases its IOwned, and with it the IOwned's scope
  service := nil;
end;

procedure SaveWithSameTransactionAudit(Req: THorseRequest; Res: THorseResponse);
var
  service: ISameTransactionAuditService;
begin
  Log('');
  Log(Format('POST %s name=%s rollback=%s', [Req.PathInfo, QueryValue(Req, 'name'), QueryValue(Req, 'rollback')]));
  // resolved in the request scope: both repositories share the request's unit of work
  service := RequestScope(Req).Resolve<ISameTransactionAuditService>;
  Save(service, Req, Res);
  service := nil;
end;

function RowsToJSON(const aRows: TArray<string>): TJSONArray;
var
  row: string;
begin
  Result := TJSONArray.Create;
  for row in aRows do Result.Add(row);
end;

procedure ShowDatabase(Req: THorseRequest; Res: THorseResponse);
var
  database: IFakeDatabase;
  json: TJSONObject;
begin
  database := GlobalContainer.Resolve<IFakeDatabase>; // a singleton: no scope needed
  json := TJSONObject.Create;
  json.AddPair('customers', RowsToJSON(database.Rows('customers')));
  json.AddPair('audit', RowsToJSON(database.Rows('audit')));
  SendJSON(Res, json, 200);
end;

procedure RegisterRoutes;
begin
  THorse.Post('/owned', SaveWithOwnedAudit);
  THorse.Post('/same-transaction', SaveWithSameTransactionAudit);
  THorse.Get('/database', ShowDatabase);
end;

end.
