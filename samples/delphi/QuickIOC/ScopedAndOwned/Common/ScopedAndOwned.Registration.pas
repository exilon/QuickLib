unit ScopedAndOwned.Registration;

{ The registrations shared by the Horse and the VCL samples. The host (the Horse server or the VCL
  application) decides where the activity log goes, and who opens the scopes: the Horse sample opens
  one per request, the VCL sample one for the whole application, which every form uses by default. }

interface

uses
  Quick.IOC,
  ScopedAndOwned.Model;

procedure RegisterServices(aContainer: TIocContainer; const aLog: IActivityLog);

implementation

procedure RegisterServices(aContainer: TIocContainer; const aLog: IActivityLog);
begin
  //a scoped service resolved outside a scope raises EIocScopeError (True is already the default)
  aContainer.ValidateScopes := True;

  // ---- SINGLETON: one instance for the whole application ----------------------------------
  aContainer.RegisterInstance<IActivityLog>(aLog).AsSingleton;
  aContainer.RegisterType<IFakeDatabase, TFakeDatabase>.AsSingleton;

  // ---- SCOPED: one instance per scope ------------------------------------------------------
  // The scope is the request's (Horse), the application's or a screen's own (VCL), or an IOwned's
  aContainer.RegisterType<IUnitOfWork, TFakeUnitOfWork>.AsScoped;
  aContainer.RegisterType<ICustomerRepository, TCustomerRepository>.AsScoped;
  // also registers IOwned<IAuditRepository> (AutoRegisterOwned, True by default)
  aContainer.RegisterType<IAuditRepository, TAuditRepository>.AsScoped;

  // ---- TRANSIENT: a new instance on every resolution ---------------------------------------
  aContainer.RegisterType<IOwnedAuditService, TOwnedAuditService>.AsTransient;
  aContainer.RegisterType<ISameTransactionAuditService, TSameTransactionAuditService>.AsTransient;
end;

end.
