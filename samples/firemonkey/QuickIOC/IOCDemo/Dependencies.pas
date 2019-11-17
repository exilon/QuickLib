unit Dependencies;

interface

uses
  System.SysUtils,
  Quick.Commons,
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

implementation

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

end.
