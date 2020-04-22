program TypedFactory;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.IOC;

type
  TUser = class
  private
    fName : string;
    fAge : Integer;
  public
    property Name : string read fName write fName;
    property Age : Integer read fAge write fAge;
  end;

  IUserFactory = interface(IInvokable)
  ['{CCDC635D-483E-44A9-8F29-6A19CE1652F3}']
    function New : TUser;
  end;

var
  container : TIocContainer;
  user : TUser;
  factory : IUserFactory;
begin
  try
    ReportMemoryLeaksOnShutdown := True;
    container := TIocContainer.Create;
    container.RegisterTypedFactory<IUserFactory,TUser>();
    factory := container.Resolve<IUserFactory>;
    user := factory.New;
    user.Name := 'John';
    user.Age := 30;
    coutFmt('User: %s / Age: %d',[user.Name, user.Age],etInfo);
    user.Free;
    ConsoleWaitForEnterKey;
    container.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
