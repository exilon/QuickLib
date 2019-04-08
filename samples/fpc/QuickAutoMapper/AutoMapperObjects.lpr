program AutoMapperObjects;

{$mode delphi}

uses
  SysUtils,
  Generics.Collections,
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

  TAgentStatus = (stActive, stIdle, stFail);

  TCar = class
  private
    fModel : string;
    fCarType : TCarType;
  published
    property Model : string read fModel write fModel;
    property CarType : TCarType read fCarType write fCarType;
  end;

  TCarList = TObjectList<TCar>;

  TAgent = class
  private
    fName : string;
    fStatus : TAgentStatus;
  published
    property Name : string read fName write fName;
    property Status : TAgentStatus read fStatus write fStatus;
  end;

  TAgentList = TList<TAgent>;

  TArrayNumbers = array of Integer;

  TUserBase = class(TJsonRecord)
  private
    fName : string;
    fAge : Integer;
    fCreationDate : TDateTime;
    fNumbers : TArrayNumbers;
    fAgent : TAgent;
  published
    property Name : string read fName write fName;
    property Age : Integer read fAge write fAge;
    property CreationDate : TDateTime read fCreationDate write fCreationDate;
    property Numbers : TArrayNumbers read fNumbers write fNumbers;
    property Agent : TAgent read fAgent write fAgent;
  end;

  TPointsList = TList<Integer>;

  TUser = class(TUserBase)
  private
    fId : Int64;
    fCash : Integer;
    fJob : TJob;
    fCar : TCar;
    fCarList : TCarList;
    fPoints : TPointsList;
    fAgentList : TAgentList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Id : Int64 read fId write fId;
    property Cash : Integer read fCash write fCash;
    property Job : TJob read fJob write fJob;
    property Car : TCar read fCar write fCar;
    property CarList : TCarList read fCarList write fCarList;
    property Points : TPointsList read fPoints write fPoints;
    property AgentList : TAgentList read fAgentList write fAgentList;
  end;

  TUser2 = class(TUserBase)
  private
    fIdUser : Int64;
    fJob : TJob;
    fMoney : Integer;
    fCar : TCar;
    fCarList : TCarList;
    fPoints : TPointsList;
    fAgentList : TAgentList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property IdUser : Int64 read fIdUser write fIdUser;
    property Money : Integer read fMoney write fMoney;
    property Job : TJob read fJob write fJob;
    property Car : TCar read fCar write fCar;
    property CarList : TCarList read fCarList write fCarList;
    property Points : TPointsList read fPoints write fPoints;
    property AgentList : TAgentList read fAgentList write fAgentList;
  end;

  TUserMapping = class
    class procedure DoMapping(const aSrcObj : TObject; const aTargetName : string; out Value : TFlexValue);
    class Procedure DoAfterMapping(const aSrcObj : TUser; aTgtObj : TUser2);
  end;

var
  User : TUser;
  User2 : TUser2;
  AutoMapper : TAutoMapper<TUser,TUser2>;
  job : TJob;
  car : TCar;
  agent : TAgent;

{ TUser }

constructor TUser.Create;
begin
  fCar := TCar.Create;
  fJob := TJob.Create;
  fCarList := TCarList.Create(True);
  fPoints := TPointsList.Create;
  fAgent := TAgent.Create;
  fAgentList := TAgentList.Create;
end;

destructor TUser.Destroy;
begin
  fCar.Free;
  fJob.Free;
  fCarList.Free;
  fPoints.Free;
  fAgent.Free;
  fAgentList.Free;
  inherited;
end;

{ TUser2 }

constructor TUser2.Create;
begin
  fCar := TCar.Create;
  fJob := TJob.Create;
  fCarList := TCarList.Create(True);
  fPoints := TPointsList.Create;
  fAgent := TAgent.Create;
  fAgentList := TAgentList.Create;
end;

destructor TUser2.Destroy;
begin
  fCar.Free;
  fJob.Free;
  fCarList.Free;
  fPoints.Free;
  fAgent.Free;
  fAgentList.Free;
  inherited;
end;

class procedure TUserMapping.DoMapping(const aSrcObj : TObject; const aTargetName : string; out Value : TFlexValue);
begin
  if aTargetName = 'Money' then Value := TUser(aSrcObj).Cash * 2
  else if aTargetName = 'IdUser' then Value := TUser(aSrcObj).Id;
end;

class procedure TUserMapping.DoAfterMapping(const aSrcObj : TUser; aTgtObj : TUser2);
begin
 aTgtObj.Money := aSrcObj.Cash * 2;
 aTgtObj.IdUser := aSrcObj.Id;
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
    car := TCar.Create;
    car.Model := 'Ford';
    car.CarType := ctDiesel;
    User.CarList.Add(car);
    car := TCar.Create;
    car.Model := 'Nissan';
    car.CarType := ctDiesel;
    User.CarList.Add(car);
    User.Points.Add(77);
    User.Points.Add(100);
    User.Points.Add(30);
    agent := TAgent.Create;
    agent.Name := 'FirstAgent';
    agent.Status := TAgentStatus.stIdle;
    User.Agent.Name := 'John';
    User.Agent.Status := TAgentStatus.stIdle;
    User.AgentList.Add(agent);
    agent := TAgent.Create;
    agent.Name := 'SecondAgent';
    agent.Status := TAgentStatus.stFail;
    User.AgentList.Add(agent);
    //User2 := TMapper<TUser2>.Map(User);
    AutoMapper := TAutoMapper<TUser,TUser2>.Create;
    try
      //option1: you can define auto map different named properties
      AutoMapper.CustomMapping.AddMap('Cash','Money');
      AutoMapper.CustomMapping.AddMap('Id','IdUser');

      //option2: you can decide to modify each property manually or allow to auto someones
      AutoMapper.OnDoMapping := TUserMapping.DoMapping;

      //option3: you can modify some properties after automapping done
      AutoMapper.OnAfterMapping := TUserMapping.DoAfterMapping;

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
