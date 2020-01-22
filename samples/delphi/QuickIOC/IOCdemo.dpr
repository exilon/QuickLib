program IOCdemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.IOC;

type
  IMathService = interface
  ['{E8BEE282-3828-4F27-869D-C808296F8CA2}']
    procedure IncNumsOperations;
    function NumOperations : Integer;
  end;

  ISumService = interface(IMathService)
  ['{7580F5F6-0132-49F3-A22A-A2E93E53E8A7}']
    function Sum(a, b : Integer) : Integer;
  end;

  IMultService = interface(IMathService)
  ['{56772C4B-70AF-4BB7-9DFE-E2D67912DDC1}']
    function Mult(a, b : Integer) : Integer;
  end;

  TMathService = class(TInterfacedObject,IMathService)
  private
    fNumOperations : Integer;
  public
    constructor Create;
    procedure IncNumsOperations;
    function NumOperations : Integer;
  end;

  TSumService = class(TMathService,ISumService)
  public
    function Sum(a, b : Integer) : Integer;
  end;

  TMultService = class(TMathService,IMultService)
  public
    function Mult(a, b : Integer) : Integer;
  end;

  IBigService = interface
  ['{AE7E7617-02BD-48C9-A370-49566A563C38}']
    function GetNumOperations : Integer;
    function Sum(a, b : Integer) : Integer;
    function Mult(a, b : Integer) : Integer;
    function SumService : ISumService;
    function MultService : IMultService;
    property NumOperations : Integer read GetNumOperations;
  end;
  TBigService = class(TInterfacedObject,IBigService)
  private
    fSumService : ISumService;
    fMultService : IMultService;
    fNumOperations : Integer;
    function GetNumOperations : Integer;
    procedure IncNumOperations;
  public
    constructor Create(aSumService : ISumService; aMultService : IMultService);
    function Sum(a, b : Integer) : Integer;
    function Mult(a, b : Integer) : Integer;
    function SumService : ISumService;
    function MultService : IMultService;
    property NumOperations : Integer read GetNumOperations;
  end;

  TDivideService = class
  private
    fNumOperations : Integer;
    fRound : Boolean;
  public
    constructor Create(aRound : Boolean);
    property NumOperations : Integer read fNumOperations;
    function Divide(a,b : Integer) : Integer;
  end;

{ TSumService }

function TSumService.Sum(a, b: Integer): Integer;
begin
  Result := a + b;
  IncNumsOperations;
end;

{ TMultService }

function TMultService.Mult(a, b: Integer): Integer;
begin
  Result := a * b;
  IncNumsOperations;
end;

{ TBigService }

constructor TBigService.Create(aSumService: ISumService; aMultService: IMultService);
begin
  fSumService := aSumService;
  fMultService := aMultService;
end;

function TBigService.Sum(a, b: Integer): Integer;
begin
  Result := fSumService.Sum(a,b);
  IncNumOperations;
end;

function TBigService.SumService: ISumService;
begin
  Result := fSumService;
end;

function TBigService.GetNumOperations: Integer;
begin
  Result := fNumOperations;
end;

procedure TBigService.IncNumOperations;
begin
  Inc(fNumOperations);
end;

function TBigService.Mult(a, b: Integer): Integer;
begin
  Result := fMultService.Mult(a,b);
  IncNumOperations;
end;

function TBigService.MultService: IMultService;
begin
  Result := fMultService;
end;

var
  iocContainer : TIocContainer;
  sumservice : ISumService;
  multservice : IMultService;
  bigservice : IBigService;
  divideservice : TDivideService;
  res : Integer;
  i : Integer;
  times : Integer;
  numoperations : Integer;

{ TMathService }

constructor TMathService.Create;
begin
  fNumOperations := 0;
end;

procedure TMathService.IncNumsOperations;
begin
  Inc(fNumOperations);
end;

function TMathService.NumOperations: Integer;
begin
  Result := fNumOperations;
end;

{ TDivideService }

constructor TDivideService.Create(aRound : Boolean);
begin
  fNumOperations := 0;
  fRound := aRound;
end;

function TDivideService.Divide(a, b: Integer): Integer;
begin
  if fRound then Result := Round(a / b)
    else Result := Trunc(a / b);
  Inc(fNumOperations);
end;

begin
  try
    ReportMemoryLeaksOnShutdown := True;

    iocContainer := TIocContainer.Create;
    iocContainer.RegisterType<ISumService,TSumService>.AsSingleTon.DelegateTo(function : TSumService
                                                                              begin
                                                                                Result := TSumService.Create;
                                                                              end);
    iocContainer.RegisterType<IMultService,TMultService>.AsTransient;
    iocContainer.RegisterType<IBigService,TBigService>('one').AsSingleTon;
    iocContainer.RegisterType<IBigService,TBigService>('other').AsTransient;

    iocContainer.RegisterInstance<TDivideService>('one').AsSingleton.DelegateTo(function : TDivideService
                                                                                begin
                                                                                  Result := TDivideService.Create(True);
                                                                                end);
    iocContainer.RegisterInstance<TDivideService>('other').AsTransient.DelegateTo(function : TDivideService
                                                                                begin
                                                                                  Result := TDivideService.Create(True);
                                                                                end);

    times := 100;
    res := 0;

    //test1: class injection as singleton
    for i := 1 to times do
    begin
      sumservice := iocContainer.Resolve<ISumService>;
      res := sumservice.Sum(2,2);
    end;
    if sumservice.NumOperations = times then cout('Test1: Class injection as Singleton test ok',etSuccess)
      else cout('Test1: Class injection as Singleton test error',etError);
    cout('SumService.Sum = %d (calls: %d)',[res,sumservice.NumOperations],etInfo);

    //test2: class injection as transient
    for i := 1 to times do
    begin
      multservice := iocContainer.Resolve<IMultService>;
      res := multservice.Mult(2,4);
    end;
    if multservice.NumOperations = 1 then cout('Test2: Class injection as Transient test ok',etSuccess)
      else cout('Test2: Class injection as Transient test error',etError);
    cout('MultService.Mult = %d (calls: %d)',[res,multservice.NumOperations],etInfo);

    //test3: constructor injection as singleton
    for i := 1 to times do
    begin
      bigservice := iocContainer.Resolve<IBigService>('one');
      res := bigservice.Sum(2,2);
    end;
    if bigservice.NumOperations = times then cout('Test3: Constructor injection as Singleton test ok',etSuccess)
      else cout('Test3: Constructor injection as Singleton test error',etError);
    cout('BigService.Sum = %d (calls: %d to BigService / calls: %d to SumService (as singleton))',[res,bigservice.NumOperations,bigservice.sumservice.NumOperations],etInfo);

    //test4: constructor injection as transient
    for i := 1 to times do
    begin
      bigservice := iocContainer.Resolve<IBigService>('other');
      res := bigservice.Mult(2,4);
    end;
    if bigservice.NumOperations = 1 then cout('Test4: Constructor injection as Transient test ok',etSuccess)
      else cout('Test4: Constructor injection as Transient test error',etError);
    cout('BigService.Mult = %d (calls: %d to BigService / calls: %d to MultService (as transient))',[res,bigservice.NumOperations,bigservice.multservice.NumOperations],etInfo);

    //test5: class instance injection as singleton
    for i := 1 to times do
    begin
      divideservice := iocContainer.Resolve<TDivideService>('one');
      res := divideservice.Divide(100,2);
    end;
    if divideservice.NumOperations = times then cout('Test5: Class instance injection as Singleton test ok',etSuccess)
      else cout('Test5: Class instance injection as Singleton test error',etError);
    cout('DivideService.Divide = %d (calls: %d)',[res,divideservice.NumOperations],etInfo);

    //test6: class instance injection as transient
    for i := 1 to times do
    begin
      divideservice := iocContainer.Resolve<TDivideService>('other');
      res := divideservice.Divide(100,2);
      numoperations := divideservice.NumOperations;
      //transient instances must be manual free
      divideservice.Free;
    end;
    if numoperations = 1 then cout('Test6: Class instance injection as Transient test ok',etSuccess)
      else cout('Test6: Class instance injection as Transient test error',etError);
    cout('DivideService.Divide = %d (calls: %d)',[res,numoperations],etInfo);

    cout('Press <ENTER> to Exit',ccYellow);
    ConsoleWaitForEnterKey;

    iocContainer.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
