program ScopedAndOwnedHorse;

{ Horse sample of scoped services and owned instances (IOwned<T>): one scope per request.
  Needs Horse (https://github.com/HashLoad/horse): see README.md for where the project looks for it. }

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.SyncObjs,
  Horse,
  Quick.IOC,
  ScopedAndOwned.Model in '..\Common\ScopedAndOwned.Model.pas',
  ScopedAndOwned.Registration in '..\Common\ScopedAndOwned.Registration.pas',
  ScopedAndOwned.ScopePerRequest in 'ScopedAndOwned.ScopePerRequest.pas',
  ScopedAndOwned.Routes in 'ScopedAndOwned.Routes.pas';

const
  PORT = 9001;

var
  ConsoleLock: TCriticalSection;

begin
  ConsoleLock := TCriticalSection.Create;
  try
    try
      // requests run in several threads: one line at a time on the console
      RegisterServices(GlobalContainer, TActivityLog.Create(
        procedure(aText: string)
        begin
          ConsoleLock.Enter;
          try
            Writeln(aText);
          finally
            ConsoleLock.Leave;
          end;
        end));
      // creating a singleton is not thread-safe: create them now, before the server accepts
      // requests in several threads
      GlobalContainer.Build;

      // before the routes: opens and frees the scope of each request
      THorse.Use(ScopePerRequest);
      RegisterRoutes;

      THorse.Listen(PORT,
        procedure
        begin
          Writeln(Format('Listening on http://localhost:%d', [PORT]));
          Writeln('  POST /owned?name=Ann&rollback=true             audit entry in a unit of work of its own');
          Writeln('  POST /same-transaction?name=Bob&rollback=true  customer and audit entry in the same transaction');
          Writeln('  GET  /database                                 what was committed');
          Writeln;
        end);
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    ConsoleLock.Free;
  end;
end.
