unit main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Quick.Json.Serializer,
  Generics.Collections;

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
  published
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
    //[TCommentProperty('gnFemale or gnMale')]
    fGenre : TGenre;
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
    //[TCommentProperty('Is user Id')]
    property Id : TID read fId write fId;
    property Name : string read fName write fName;
    property Surname : string read fSurname write fSurname;
    property Age : Integer read fAge write fAge;
    property Address : string read fAddress write fAddress;
    property Balance : Double read fBalance write fBalance;
    //[TCustomNameProperty('IsMarried')]
    property Married : Boolean read fMarried write fMarried;
    property WorkingTime : TWorkingTime read fWorkingTime write fWorkingTime;
    property HireDate : TDateTime read fHireDate write fHireDate;
    //[TCommentProperty('Possible values = usAtOffice, usAtHome or usOnVacation')]
    property Status : TUserStatus read fStatus write fStatus;
    //property LastConnections : TConnectionArray read fLastConnections write fLastConnections;
    property Observations : string read fObservations write fObservations;
    property LevelPrivilege : TLevelPrivilege read fLevelPrivilege write fLevelPrivilege;
    property Options : TOptions read fOptions write fOptions;
    property Groups : TGroupList read fGroups write fGroups;
  end;

  TUserList = TObjectList<TUser>;

  { TForm1 }

  TForm1 = class(TForm)
    btnFromJson: TButton;
    btnToJson: TButton;
    Memo1: TMemo;
    procedure btnFromJsonClick(Sender: TObject);
    procedure btnToJsonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  serializer : TJsonSerializer;
  User : TUser;
  UserList : TUserList;


var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnToJsonClick(Sender: TObject);
begin
  Memo1.Text := serializer.ObjectToJson(User,True);
  btnFromJson.Enabled := True;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  User.Free;
  serializer.Free;
end;

procedure TForm1.btnFromJsonClick(Sender: TObject);
var
  newuser : TUser;
begin
  newuser := TUser.Create;
  try
    newuser := serializer.JsonToObject(newuser,Memo1.Text) as TUser;
    Memo1.Lines.Add('NewUser:');
    Memo1.Lines.Add(serializer.ObjectToJson(newuser));
  finally
    newuser.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  lastcon : TConnectionInfo;
  group : TGroup;
begin
  serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
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
  user.Balance := 99.9;
  user.HireDate := Now();
  user.LevelPrivilege := [1,2,3,4];
  user.WorkingTime.Name:= 'WeekConfig';
  user.WorkingTime.WorkDays := DEF_WORKDAYS;
  user.WorkingTime.FreeDays := DEF_WEEKEND;
  user.Observations := 'Good aptitude';
  user.Status := TUserStatus.usOnVacation;
  //lastcon.IP := '127.0.0.1';
  //lastcon.ConnectionDate := Now();
  //User.LastConnections := [lastcon];
  //lastcon.IP := '192.0.0.1';
  //lastcon.ConnectionDate := Now();
  //User.LastConnections := User.LastConnections + [lastcon];
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

