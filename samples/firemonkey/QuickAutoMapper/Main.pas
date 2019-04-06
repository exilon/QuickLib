unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Quick.AutoMapper, Quick.JSONRecord, FMX.Controls.Presentation, Quick.Arrays,
  FMX.ScrollBox, FMX.Memo, System.Generics.Collections;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TJob = record
    Name : string;
    DateFrom : TDateTime;
    DateTo : TDateTime;
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

  TAgent = record
    Name : string;
    Status : TAgentStatus;
  end;

  TAgentList = TList<TAgent>;

  TUserBase = class(TJsonRecord)
  private
    fName : string;
    fAge : Integer;
    fCreationDate : TDateTime;
    fNumbers : TArray<Integer>;
    fAgent : TAgent;
  published
    property Name : string read fName write fName;
    property Age : Integer read fAge write fAge;
    property CreationDate : TDateTime read fCreationDate write fCreationDate;
    property Numbers : TArray<Integer> read fNumbers write fNumbers;
    property Agent : TAgent read fAgent write fAgent;
  end;

  TPointsList = TXArray<Integer>;

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

var
  User : TUser;
  User2 : TUser2;
  UserClone : TUser;
  job : TJob;
  AutoMapper : TAutoMapper<TUser,TUser2>;
  car : TCar;
  agent : TAgent;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TUser }

constructor TUser.Create;
begin
  fCar := TCar.Create;
  fCarList := TCarList.Create(True);
  //fPoints := TPointsList.Create;
  fAgentList := TAgentList.Create;
end;

destructor TUser.Destroy;
begin
  fCar.Free;
  fCarList.Free;
  //fPoints.Free;
  fAgentList.Free;
  inherited;
end;

{ TUser2 }

constructor TUser2.Create;
begin
  fCar := TCar.Create;
  fCarList := TCarList.Create(True);
  //fPoints := TPointsList.Create;
  fAgentList := TAgentList.Create;
end;

destructor TUser2.Destroy;
begin
  fCar.Free;
  fCarList.Free;
  //fPoints.Free;
  fAgentList.Free;
  inherited;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
User := TUser.Create;
    User.Id := 17;
    User.CreationDate := Now();
    User.Name := 'John Miller';
    User.Age := 30;
    User.Numbers := [1,2,3,4,5];
    User.Cash := 3500;
    job.Name := 'Designer';
    job.DateFrom := IncMonth(Now(),-12);
    job.DateTo := Now();
    User.Job := job;
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
    agent.Name := 'FirstAgent';
    agent.Status := TAgentStatus.stIdle;
    User.Agent := agent;
    User.AgentList.Add(agent);
    agent.Name := 'SecondAgent';
    agent.Status := TAgentStatus.stFail;
    User.AgentList.Add(agent);
    //User2 := TMapper<TUser2>.Map(User);
    AutoMapper := TAutoMapper<TUser,TUser2>.Create;
    try
      AutoMapper.CustomMapping.AddMap('Cash','Money');
      AutoMapper.CustomMapping.AddMap('Id','IdUser');
      User2 := AutoMapper.Map(User);
      //User2 := TUser2.Create;
      //User.MapTo(User2);
      //User2.MapFrom(User);
      //User2 := User.Map<TUser2>;
      //UserClone := User.Clone as TUser;
      //User2 := TUser2(User.Clone);
      //User2 := TMapper<TUserBase>.Clone(User) as TUser2;

      Memo1.Lines.Add('COMPARE USER VS USER2');
      Memo1.Lines.Add(Format('User.Id = %d / User2.IdUser = %d',[User.Id,User2.IdUser]));
      Memo1.Lines.Add(Format('User.CreationDate = %s / User2.CreationDate = %s',[DateTimeToStr(User.CreationDate),DateTimetoStr(User2.CreationDate)]));
      Memo1.Lines.Add(Format('User.Name = %s / User2.Name = %s',[User.Name,User2.Name]));
      Memo1.Lines.Add(Format('User.Age = %d / User2.Age = %d',[User.Age,User2.Age]));
      Memo1.Lines.Add(Format('User.Numbers = %d / User2.Numbers = %d',[User.Numbers[1],User2.Numbers[1]]));
      Memo1.Lines.Add(Format('User.Cash = %d / User2.Money = %d',[User.Cash,User2.Money]));
      Memo1.Lines.Add(Format('User.Job.Name = %s / User2.Job.Name = %s',[User.Job.Name,User2.Job.Name]));
      Memo1.Lines.Add(Format('User.Job.DateFrom = %s / User2.Job.DateFrom = %s',[DateTimeToStr(User.Job.DateFrom),DateTimeToStr(User2.Job.DateFrom)]));
      Memo1.Lines.Add(Format('User.Car.Model = %s / User2.Car.Model = %s',[User.Car.Model,User2.Car.Model]));

      Memo1.Lines.Add(' ');
      Memo1.Lines.Add('USER AS JSON RESULT');
      Memo1.Lines.Add(Format('%s',[User.ToJson(True)]));
      Memo1.Lines.Add(' ');
      Memo1.Lines.Add('USER2 AS JSON RESULT');
      Memo1.Lines.Add(Format('%s',[User2.ToJson(True)]));

    finally
      AutoMapper.Free;
      User.Free;
      User2.Free;
    end;
end;

end.
