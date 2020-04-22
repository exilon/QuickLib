unit main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  Quick.JsonRecord,
  Quick.Base64,
  Quick.Json.Serializer;

type

  TID = Int64;

  TContactType = (ctInternal, ctExternal);
  TMessageState = (msPending, msSent, msNotSent);

  TRecipientArray = array of TID;

  TRecipient = record
    ID : TID;
    RType : TContactType;
    Confirm : TMessageState;
  end;

  TGenre = (gnMale, gnFemale);

  TGroupType = (gtInternal, gtExternal);

  TDayOfWeek = (wdSunday, wdMonday, wdThuesday, wdWednesday, wdThursday, wdFriday, wdSaturday);

  TUserStatus = (usAtOffice, usAtHome, usOnVacation);

  TDays = set of TDayOfWeek;

const
  DEF_WORKDAYS : TDays = [wdMonday, wdThuesday, wdWednesday, wdThursday, wdFriday];
  DEF_WEEKEND : TDays = [wdSaturday, wdSunday];

type

  TDepartment = record
    Id : TID;
    Name : string;
  end;

  TContactIdArray = array of TID;

  TGroup = class
  private
    fGlobalID: TGUID;
    fId : TID;
    fGType : TGroupType;
  published
    property GlobalID: TGUID read fGlobalID write fGlobalID;
    property Id : TID read fId write fId;
    property GType : TGroupType read fGType write fGType;
  end;

  TOptions = class
  private
    fOption1 : Integer;
    fOption2 : string;
    fAllowGroups : TGroupType;
  published
    property Option1 : Integer read fOption1 write fOption1;
    property Option2 : string read fOption2 write fOption2;
    property AllowGroups : TGroupType read fAllowGroups write fAllowGroups;
  end;

  TConnectionInfo = record
    IP : string;
    ConnectionDate : TDateTime;
  end;

  TConnectionArray = array of TConnectionInfo;

  TGroupList = TObjectList<TGroup>;

  TWorkingTime = class
  private
    fName : string;
    fWorkDays : TDays;
    fFreeDays : TDays;
  published
    property Name : string read fName write fName;
    property WorkDays : TDays read fWorkDays write fWorkDays;
    property FreeDays : TDays read fFreeDays write fFreeDays;
  end;

  TLevelPrivilege = array of TID;

  TUser = class(TJsonRecord)
  private
    fId : TID;
    fName : string;
    fSurname : string;
    fAge : Integer;
    fAddress : string;
    fPath : string;
    fOptions : TOptions;
    fLastConnections : TConnectionArray;
    fMarried : Boolean;
    fWorkingTime : TWorkingTime;
    fGenre : TGenre;
    fDepartment : TDepartment;
    fBalance : Double;
    fHireDate : TDateTime;
    fLevelPrivilege : TLevelPrivilege;
    fObservations : string;
    fStatus : TUserStatus;
    fGroups : TGroupList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    [TCommentProperty('Is user Id')]
    property Id : TID read fId write fId;
    property Name : string read fName write fName;
    property Surname : string read fSurname write fSurname;
    property Age : Integer read fAge write fAge;
    [TCommentProperty('gnFemale or gnMale')]
    property Genre : TGenre read fGenre write fGenre;
    property Department : TDepartment read fDepartment write fDepartment;
    property Address : string read fAddress write fAddress;
    property Path : string read fPath write fPath;
    property Balance : Double read fBalance write fBalance;
    [TCustomNameProperty('IsMarried')]
    property Married : Boolean read fMarried write fMarried;
    property WorkingTime : TWorkingTime read fWorkingTime write fWorkingTime;
    property HireDate : TDateTime read fHireDate write fHireDate;
    [TCommentProperty('Possible values = usAtOffice, usAtHome or usOnVacation')]
    property Status : TUserStatus read fStatus write fStatus;
    property LastConnections : TConnectionArray read fLastConnections write fLastConnections;
    property Observations : string read fObservations write fObservations;
    property LevelPrivilege : TLevelPrivilege read fLevelPrivilege write fLevelPrivilege;
    property Options : TOptions read fOptions write fOptions;
    property Groups : TGroupList read fGroups write fGroups;
  end;

  TUserList = TObjectList<TUser>;


  TForm1 = class(TForm)
    Memo1: TMemo;
    btnToJson: TButton;
    btnFromJson: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnToJsonClick(Sender: TObject);
    procedure btnFromJsonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Serializer : TJsonSerializer;
  User : TUser;
  User2 : TUser;
  UserList : TUserList;

implementation

{$R *.fmx}

procedure TForm1.btnFromJsonClick(Sender: TObject);
begin
  if User2 <> nil then User2.Free;
  User2 := TUser.Create;
  User2.FromJson(Memo1.Text);
  //User2 := TUser.CreateFromJson(Memo1.Text);
  //User2.CreateFromJson(Memo1.Text);
  Memo1.Lines.Add('User2 as json:');
  Memo1.Lines.Add(User2.ToJson(True));
  Memo1.Lines.Add(Format('Groups.OwnedObjects=%s',[BoolToStr(User2.Groups.OwnsObjects,True)]));
  Memo1.Lines.Add(Format('Groups.Count=%d',[User2.Groups.Count]));
  Memo1.Lines.Add(Format('Groups.Capacity=%d',[User2.Groups.Capacity]));
  ShowMessage(Format('%s %s from %s',[User2.Name,User2.Surname,User2.Address]));
end;

procedure TForm1.btnToJsonClick(Sender: TObject);
begin
  Memo1.Text := User.ToJson(True);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(User) then User.Free;
  if Assigned(User2) then User2.Free;
  Serializer.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  lastcon : TConnectionInfo;
  group : TGroup;
  department : TDepartment;
  guid: TGUID;
begin
  serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
  user := TUser.Create;
  user.Id := 77;
  user.Name := 'Joe';
  user.Surname := 'Smith Valdés';
  user.Age := 30;
  user.Married := True;
  user.Address := 'Sunset st. 2 \b';
  User.Path := 'C:\documents\files';
  user.Options.Option1 := 1;
  user.Options.Option2 := 'good';
  user.Options.AllowGroups := gtExternal;
  user.Balance := 99.9;
  user.HireDate := Now();
  user.LevelPrivilege := [1,2,3,4];
  user.WorkingTime.Name:= 'WeekConfig';
  user.WorkingTime.WorkDays := DEF_WORKDAYS;
  user.WorkingTime.FreeDays := DEF_WEEKEND;
  user.Observations := 'Good aptitude';
  department.Id := 10;
  department.Name := 'IT';
  user.Department := department;
  user.Status := TUserStatus.usOnVacation;
  lastcon.IP := '127.0.0.1';
  lastcon.ConnectionDate := Now();
  User.LastConnections := [lastcon];
  lastcon.IP := '192.0.0.1';
  lastcon.ConnectionDate := Now();
  User.LastConnections := User.LastConnections + [lastcon];
  group := TGroup.Create;
  group.Id := 1;
  group.GType := gtInternal;
  CreateGUID(guid);
  group.GlobalID:=guid;
  user.Groups.Add(group);
  group := TGroup.Create;
  group.Id := 2;
  group.GType := gtExternal;
  CreateGUID(guid);
  group.GlobalID:=guid;
  user.Groups.Add(group);
 end;


{ TUser }

constructor TUser.Create;
begin
  fOptions := TOptions.Create;
  fWorkingTime := TWorkingTime.Create;
  fGroups := TGroupList.Create(True);
end;

destructor TUser.Destroy;
begin
  fOptions.Free;
  fWorkingTime.Free;
  fGroups.Free;
  inherited;
end;

end.
