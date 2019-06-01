program LinqQuerySample;

{$mode delphi}

uses
  SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Chrono,
  Quick.Arrays,
  Quick.Linq,
  Quick.Expression;

const
  numusers = 0;
  UserNames : array[0..25] of string = ('Cliff','Alan','Anna','Phil','John','Michel','Jennifer','Peter','Brandon','Joe','Steve','Lorraine','Bill','Tom','Norma','Martin','Steffan','Wilma','Derek','Lewis','Paul',
                                 'Erik','Robert','Nicolas','Frederik','Rose');
  UserSurnames : array[0..25] of string = ('Gordon','Summer','Huan','Paterson','Johnson','Michelson','Smith','Peterson','Miller','McCarney','Roller','Gonzalez','Thomson','Muller','Jefferson','Volkov','Matheu','Morrison','Newman','Lover','Sunday',
                                    'Roberts','Landon','Yuri','Paris','Levis');
type
  TLoginInfo = class
  private
    fUserName : string;
    fUserPassword : string;
    fLocked : Boolean;
  published
    property UserName : string read fUserName write fUserName;
    property UserPassword : string read fUserPassword write fUserPassword;
    property Locked : Boolean read fLocked write fLocked;
  end;

  { TUser }

  TUser = class
  private
    fId : Int64;
    fName : string;
    fSurName : string;
    fAge : Integer;
    fLoginInfo : TLoginInfo;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Id : Int64 read fId write fId;
    property Name : string read fName write fName;
    property SurName : string read fSurName write fSurName;
    property Age : Integer read fAge write fAge;
    property LoginInfo : TLoginInfo read fLoginInfo write fLoginInfo;
  end;

var
  users : TXArray<TUser>;
  user : TUser;
  i : Integer;
  n : Integer;
  crono : TChronometer;

{ TUser }

constructor TUser.Create;
begin
  fLoginInfo := TLoginInfo.Create;
end;

destructor TUser.Destroy;
begin
  fLoginInfo.Free;
  inherited Destroy;
end;

begin
  try
    cout('Generating list...',etInfo);
    //generate first dummy entries
    for i := 1 to numusers - high(UserNames) do
    begin
      user := TUser.Create;
      user.Id := Random(999999999999999);
      user.Name := 'Name' + i.ToString;
      user.SurName := 'SurName' + i.ToString;
      user.Age := 18 + Random(20);
      users.Add(user);
    end;

    //generate real entries to search
    for i := 0 to high(UserNames) do
    begin
      user := TUser.Create;
      user.Id := Random(999999999999999);
      user.Name := UserNames[i];
      user.SurName := UserSurnames[i];
      user.Age := 18 + Random(20);
      user.LoginInfo.Username := UserNames[i];
      user.LoginInfo.UserPassword := RandomPassword(8,[pfIncludeNumbers,pfIncludeSigns]);
      user.LoginInfo.Locked := False;

      users.Add(user);
    end;

    crono := TChronometer.Create;

    //test search by normal iteration
    user := nil;
    crono.Start;
    for i := 0 to users.Count - 1 do
    begin
      //if (users[i].Name = 'Anus') or (users[i].SurName = 'Smith') then
      if users[i].Name = 'Peter' then
      begin
        crono.Stop;
        user := users[i];
        Break;
      end;
    end;
    if user <> nil then cout('Found by Iteration: %s %s at %d position in %s',[user.Name,user.SurName,i,crono.ElapsedTime],etSuccess)
      else cout('Not found by Iteration!',etError);


    //test search by Linq iteration
    crono.Start;
    user := TLinq<TUser>.From(users).Where('(Name = ?) OR (SurName = ?)',['Anus','Smith']).OrderBy('Name').SelectFirst;
    //user := TLinq<TUser>.From(users).Where('LoginInfo.UserName = ?',['Cliff']).SelectFirst;
    crono.Stop;
    if user <> nil then cout('Found by Linq: %s %s in %s',[user.Name,user.SurName,crono.ElapsedTime],etSuccess)
      else cout('Not found by Linq! (%s)',[crono.ElapsedTime],etError);

    //ConsoleWaitForEnterKey;
    cout('Multi results:',etInfo);

    //test search by normal iteration
    user := nil;
    n := 0;
    cout('Found by Iteration:',etInfo);
    for i := 0 to users.Count - 1 do
    begin
      //if ((users[i].Name = 'Anna') or (users[i].Age > 30)) or (users[i].SurName = 'Smith') then
      //if users[i].Age > 18 then
      if (users[i].Name = 'Anna') then
      begin
        Inc(n);
        user := users[i];
        cout('%d. %s %s',[n,user.Name,user.SurName],etSuccess)
      end;
    end;
    if user = nil then cout('Not found by Iteration!',etError);

    //test search by Linq iteration
    user := nil;
    n := 0;
    cout('Found by Linq:',etInfo);
    //TLinq.From<TUser>(users).Where('SurName Like ?',['p%']).Delete;

    TLinq<TUser>.From(users).Where('Name = ?',['Peter']).Update(['Name'],['Poter']);

    //for user in TLinq<TUser>.From(users).Where('(Name = ?) OR (SurName = ?) OR (SurName = ?)',['Peter','Smith','Huan']).Select do
    //for user in TLinq<TUser>.From(users).Where('(Name = ?) OR (Age > ?) OR (SurName = ?)',['Anna',30,'Smith']).Select do
    //for user in TLinq<TUser>.Fromusers).Where('Age > ?',[18]).Select do
    //for user in TLinq<TUser>.From(users).Where('SurName Like ?',['%son']).Select do
    //for user in TLinq<TUser>.From(users).Where('SurName Like ?',['p%']).Select do
    //for user in TLinq<TUser>.From(users).Where('1 = 1',[]).Select do
    for user in TLinq<TUser>.From(users).Where('(LoginInfo.UserName Like ?) OR (LoginInfo.UserName Like ?)',['p%','a%'])
                                         .OrderBy('Name')
                                         .Select do
    begin
      Inc(n);
      cout('%d. %s %s',[n,user.Name,user.SurName],etSuccess);
    end;
    if user = nil then cout('Not found by Linq!',etError);

    cout('Press a key to Exit',etInfo);
    Readln;
    crono.Free;
  except
    on E: Exception do
      cout('%s : %s',[E.ClassName,E.Message],etError);
  end;
end.
