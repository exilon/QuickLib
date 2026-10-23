program ScopedAndOwnedVcl;

{ VCL sample of scoped services and owned instances (IOwned<T>): one scope for the whole application,
  shared by every form by default; a form can have a scope of its own. }

uses
  Vcl.Forms,
  Quick.IOC,
  ScopedAndOwned.Model in '..\Common\ScopedAndOwned.Model.pas',
  ScopedAndOwned.Registration in '..\Common\ScopedAndOwned.Registration.pas',
  ScopedAndOwned.ApplicationScope in 'ScopedAndOwned.ApplicationScope.pas',
  ScopedAndOwned.MainForm in 'ScopedAndOwned.MainForm.pas' {MainForm},
  ScopedAndOwned.ScreenForm in 'ScopedAndOwned.ScreenForm.pas' {ScreenForm};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  // the activity log goes to the main screen while it exists
  RegisterServices(GlobalContainer, TActivityLog.Create(
    procedure(aText: string)
    begin
      if MainForm <> nil then MainForm.Log(aText);
    end));
  // scope A: created with the application, before any form; every form uses it by default
  CreateApplicationScope;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
