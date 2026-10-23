unit ScopedAndOwned.ApplicationScope;

{ Scope A of the VCL sample: the application's scope. It is created when the application starts and
  freed when it ends, and every form uses it by default, so they all share its scoped instances: one
  unit of work, one connection for the whole application. A form can ask for a scope of its own
  instead (TScreenForm.CreateWithOwnScope), and a service can ask for a dependency in an IOwned, which
  gets a scope of its own too. }

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Quick.IOC;

/// Creates the application's scope. Call it once, before the first form is created.
procedure CreateApplicationScope;

/// The application's scope (scope A).
function ApplicationScope: TIocScope;

implementation

type
  //owned by Application and created before any form: Application frees its components in reverse
  //order of creation, so the scope is freed after every form, when no form uses it anymore
  TApplicationScopeHolder = class(TComponent)
  private
    fScope: TIocScope;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Holder: TApplicationScopeHolder;

constructor TApplicationScopeHolder.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fScope := GlobalContainer.CreateScope;
end;

destructor TApplicationScopeHolder.Destroy;
begin
  Holder := nil;
  // releases the application's scoped instances: its unit of work and repositories
  fScope.Free;
  inherited;
end;

procedure CreateApplicationScope;
begin
  if Holder <> nil then raise EInvalidOpException.Create('The application scope already exists');
  Holder := TApplicationScopeHolder.Create(Application);
end;

function ApplicationScope: TIocScope;
begin
  if Holder = nil then raise EInvalidOpException.Create('Call CreateApplicationScope before creating the forms');
  Result := Holder.fScope;
end;

end.
