unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, Quick.Json.Serializer,
  System.Generics.Collections;

type

  TID = Int64;

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
    fId : TID;
    fGType : TGroupType;
  public
    property Id : TID read fId write fId;
    property GType : TGroupType read fGType write fGType;
  end;

  TOptions = class
  private
    fOption1 : Integer;
    fOption2 : string;
    fAllowGroups : TGroupType;
  public
    property Option1 : Integer read fOption1 write fOption1;
    property Option2 : string read fOption2 write fOption2;
    property AllowGroups : TGroupType read fAllowGroups write fAllowGroups;
  end;

  TConnectionInfo = record
    IP : string;
    ConnectionDate : TDateTime;
    Contacts : TContactIdArray;
  end;

  TConnectionArray = array of TConnectionInfo;

  TGroupList = TObjectList<TGroup>;
  //TGroupList = class(TObjectList<TGroup>);

  TWorkingTime = class
  private
    fWorkDays : TDays;
    fFreeDays : TDays;
  public
    property WorkDays : TDays read fWorkDays write fWorkDays;
    property FreeDays : TDays read fFreeDays write fFreeDays;
  end;

  TLevelPrivilege = array of TID;

  TUser = class
  private
    fId : TID;
    fName : string;
    fSurname : string;
    fAge : Integer;
    fAddress : string;
    fOptions : TOptions;
    fLastConnections : TConnectionArray;
    fMarried : Boolean;
    fWorkingTime : TWorkingTime;
    [TCommentProperty('gnFemale or gnMale')]
    fGenre : TGenre;
    fHireDate : TDateTime;
    fLevelPrivilege : TLevelPrivilege;
    fObservations : string;
    fStatus : TUserStatus;
    fGroups : TGroupList;
  public
    constructor Create;
    destructor Destroy; override;
    [TCommentProperty('Is user Id')]
    property Id : TID read fId write fId;
    property Name : string read fName write fName;
    property Surname : string read fSurname write fSurname;
    property Age : Integer read fAge write fAge;
    property Address : string read fAddress write fAddress;
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
  serializer : TJsonSerializer;
  User : TUser;
  UserList : TUserList;

implementation

{$R *.fmx}

procedure TForm1.btnFromJsonClick(Sender: TObject);
var
  newuser : TUser;
begin
  newuser := TUser.Create;
  try
    newuser := serializer.JsonToObject(newuser,Memo1.Text) as TUser;
    //newuser.Groups := serializer.JsonToObject(newuser.Groups,Memo1.Text) as TGroupList;
    Memo1.Lines.Add('NewUser:');
    Memo1.Lines.Add(serializer.ObjectToJson(newuser));
    Memo1.Lines.Add('TGroup OwnsObjects: ' + BoolToStr(newuser.Groups.OwnsObjects,True));
    Memo1.Lines.Add('TGroup Capacity: ' + newuser.Groups.Capacity.ToString);
    Memo1.Lines.Add('TGroup Count: ' + newuser.Groups.Count.ToString);
    Memo1.Lines.Add(newuser.Groups[0].Id.ToString);
    //Memo1.Lines.Add(serializer.ObjectToJson(newuser.Groups));
  finally
    newuser.Free;
  end;
end;

procedure TForm1.btnToJsonClick(Sender: TObject);
begin
  Memo1.Text := serializer.ObjectToJson(User);
  btnFromJson.Enabled := True;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  User.Free;
  serializer.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  lastcon : TConnectionInfo;
  group : TGroup;
begin
  serializer := TJsonSerializer.Create(slPublicProperty);
  user := TUser.Create;
  user.Id := 77;
  user.Name := 'Joe';
  user.Surname := 'Smith';
  user.Age := 30;
  user.Married := True;
  user.Address := 'Sunset st. 2';
  user.Options.Option1 := 1;
  user.Options.Option2 := 'good';
  user.Options.AllowGroups := gtExternal;
  user.HireDate := Now();
  user.LevelPrivilege := [1,2,3,4];
  user.WorkingTime.WorkDays := DEF_WORKDAYS;
  user.WorkingTime.FreeDays := DEF_WEEKEND;
  user.Observations := 'Good aptitude';
  user.Status := TUserStatus.usOnVacation;
  lastcon.IP := '127.0.0.1';
  lastcon.ConnectionDate := Now();
  lastcon.Contacts := [1,2,3,4,5];
  User.LastConnections := [lastcon];
  lastcon.IP := '192.0.0.1';
  lastcon.ConnectionDate := Now();
  User.LastConnections := User.LastConnections + [lastcon];
  group := TGroup.Create;
  group.Id := 1;
  group.GType := gtInternal;
  user.Groups.Add(group);
  group := TGroup.Create;
  group.Id := 2;
  group.GType := gtExternal;
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
