unit ScopedAndOwned.ScopePerRequest;

{ Ties a Quick.IOC scope (TIocScope) to the lifetime of a Horse request.

  The middleware opens a scope when the request starts, keeps it in Req.Sessions, and frees it when
  the request ends, which releases the scoped instances created in it (the unit of work, the
  repositories). A route resolves what it needs from RequestScope(Req). }

interface

uses
  System.SysUtils,
  Horse,
  Horse.Session,
  Quick.IOC;

/// Horse middleware: one TIocScope per request. Register it with THorse.Use before the routes.
procedure ScopePerRequest(Req: THorseRequest; Res: THorseResponse; Next: TProc);

/// The scope the middleware opened for this request.
function RequestScope(Req: THorseRequest): TIocScope;

implementation

type
  TScopeSession = class(TSession)
  private
    fScope: TIocScope;
  end;

procedure ScopePerRequest(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  scope: TIocScope;
  session: TScopeSession;
begin
  scope := GlobalContainer.CreateScope;
  try
    session := TScopeSession.Create; // owned by Req.Sessions; the scope is owned by this middleware
    session.fScope := scope;
    Req.Sessions.SetSession(TScopeSession, session);
    try
      Next();
    finally
      session.fScope := nil;
    end;
  finally
    // Free releases every instance of the scope, and raises again the first exception raised by
    // their destructors. The response is already built at this point: report it, do not raise it
    try
      scope.Free;
    except
      on E: Exception do
        Writeln(ErrOutput, 'Error releasing the request scope: ', E.ClassName, ': ', E.Message);
    end;
  end;
end;

function RequestScope(Req: THorseRequest): TIocScope;
var
  session: TScopeSession;
begin
  if not Req.Sessions.TryGetSession<TScopeSession>(session) or (session.fScope = nil) then
    raise EInvalidOpException.Create('No request scope: call THorse.Use(ScopePerRequest) before the routes');
  Result := session.fScope;
end;

end.
