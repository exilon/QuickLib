program AutoMapperObjects;

uses
  SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.JSONRecord,
  Quick.AutoMapper;

type

  TJob = class
  private
    fName : string;
    fDateFrom : TDateTime;
    fDateTo : TDateTime;
  published
    property Name : string read fName write fName;
    property DateFrom : TDateTime read fDateFrom write fDateFrom;
    property DateTo : TDateTime read fDateTo write fDateTo;
  end;

  TCarType = (ctOil, ctDiesel);

  TCar = class
  private
    fModel : string;
    fCarType : TCarType;
  published
    property Model : string read fModel write fModel;
    property CarType : TCarType read fCarType write fCarType;
  end;

  TArrayOfInteger = array of Integer;

  TUserBase = class(TJsonRecord)
  private
    fName : string;
    fAge : Integer;
    fCreationDate : TDateTime;
    fNumbers : TArrayOfInteger;
  published
    property Name : string read fName write fName;
    property Age : Integer read fAge write fAge;
    property CreationDate : TDateTime read fCreationDate write fCreationDate;
    property Numbers : TArrayOfInteger read fNumbers write fNumbers;
  end;

  TUser = class(TUserBase)
  private
    fId : Int64;
    fCash : Integer;
    fJob : TJob;
    fCar : TCar;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Id : Int64 read fId write fId;
    property Cash : Integer read fCash write fCash;
    property Job : TJob read fJob write fJob;
    property Car : TCar read fCar write fCar;
  end;

  TUser2 = class(TUserBase)
  private
    fIdUser : Int64;
    fJob : TJob;
    fMoney : Integer;
    fCar : TCar;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property IdUser : Int64 read fIdUser write fIdUser;
    property Money : Integer read fMoney write fMoney;
    property Job : TJob read fJob write fJob;
    property Car : TCar read fCar write fCar;
  end;

var
  User : TUser;
  User2 : TUser2;
  AutoMapper : specialize TAutoMapper<TUser,TUser2>;

{ TUser }

constructor TUser.Create;
begin
  fCar := TCar.Create;
  fJob := TJob.Create;
end;

destructor TUser.Destroy;
begin
  fCar.Free;
  fJob.Free;
  inherited;
end;

{ TUser2 }

constructor TUser2.Create;
begin
  fCar := TCar.Create;
  fJob := TJob.Create;
end;

destructor TUser2.Destroy;
begin
  fCar.Free;
  fJob.Free;
  inherited;
end;

begin
  try
    Console.LogVerbose := LOG_ALL;
    User := TUser.Create;
    User.Id := 17;
    User.CreationDate := Now();
    User.Name := 'John Miller';
    User.Age := 30;
    User.Numbers := [1,2,3,4,5];
    User.Cash := 3500;
    User.Job.Name := 'Designer';
    User.Job.DateFrom := IncMonth(Now(),-12);
    User.Job.DateTo := Now();
    User.Car.Model := 'Ferrari';
    User.Car.CarType := ctOil;
    //User2 := TMapper<TUser2>.Map(User);
    AutoMapper := specialize TAutoMapper<TUser,TUser2>.Create;
    try
      AutoMapper.CustomMapping.AddMap('Cash','Money');
      AutoMapper.CustomMapping.AddMap('Id','IdUser');
      User2 := AutoMapper.Map(User);
      //User2 := TUser2.Create;
      //User.MapTo(User2);
      //User2.MapFrom(User);
      //User2 := User.Map<TUser2>;
      //User2 := TUser2(User.Clone);
      //User2 := TMapper<TUserBase>.Clone(User) as TUser2;

      cout('COMPARE USER VS USER2',etTrace);
      cout('User.Id = %d / User2.IdUser = %d',[User.Id,User2.IdUser],etInfo);
      cout('User.CreationDate = %s / User2.CreationDate = %s',[DateTimeToStr(User.CreationDate),DateTimetoStr(User2.CreationDate)],etInfo);
      cout('User.Name = %s / User2.Name = %s',[User.Name,User2.Name],etInfo);
      cout('User.Age = %d / User2.Age = %d',[User.Age,User2.Age],etInfo);
      //cout('User.Numbers = %d / User2.Numbers = %d',[User.Numbers[1],User2.Numbers[1]],etInfo);
      cout('User.Cash = %d / User2.Money = %d',[User.Cash,User2.Money],etInfo);
      cout('User.Job.Name = %s / User2.Job.Name = %s',[User.Job.Name,User2.Job.Name],etInfo);
      cout('User.Job.DateFrom = %s / User2.Job.DateFrom = %s',[DateTimeToStr(User.Job.DateFrom),DateTimeToStr(User2.Job.DateFrom)],etInfo);
      cout('User.Car.Model = %s / User2.Car.Model = %s',[User.Car.Model,User2.Car.Model],etInfo);

      cout(' ',etInfo);
      cout('USER AS JSON RESULT',etTrace);
      cout('%s',[User.ToJson],etInfo);
      cout(' ',etInfo);
      cout('USER2 AS JSON RESULT',etTrace);
      cout('%s',[User2.ToJson],etInfo);

    finally
      AutoMapper.Free;
      User.Free;
      User2.Free;
    end;
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
