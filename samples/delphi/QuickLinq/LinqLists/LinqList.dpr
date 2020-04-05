program LinqList;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  Quick.Commons,
  Quick.Console,
  Quick.Chrono,
  Quick.Lists,
  Quick.Linq;

type
  TLoginInfo = record
    Username : string;
    UserPassword : string;
    Locked : Boolean;
  end;

  TUser = class
  private
    fId : Int64;
    fName : string;
    fSurName : string;
    fAge : Integer;
    fLoginInfo : TLoginInfo;
  published
    property Id : Int64 read fId write fId;
    property Name : string read fName write fName;
    property SurName : string read fSurName write fSurName;
    property Age : Integer read fAge write fAge;
    property LoginInfo : TLoginInfo read fLoginInfo write fLoginInfo;
  end;


const
  numusers = 100000;
  UserNames : array of string = ['Cliff','Alan','Anna','Phil','John','Michel','Jennifer','Peter','Brandon','Joe','Steve','Lorraine','Bill','Tom','Norma','Martin','Steffan','Wilma','Derek','Lewis','Paul',
                                 'Erik','Robert','Nicolas','Frederik','Rose'];
  UserSurnames : array of string = ['Gordon','Summer','Huan','Paterson','Johnson','Michelson','Smith','Peterson','Miller','McCarney','Roller','Gonzalez','Thomson','Muller','Jefferson','Volkov','Matheu','Morrison','Newman','Lover','Sunday',
                                    'Roberts','Landon','Yuri','Paris','Levis'];


var
  users : TIndexedObjectList<TUser>;
  users2 : TSearchObjectList<TUser>;
  user : TUser;
  i : Integer;
  n : Integer;
  crono : TChronometer;
  login : TLoginInfo;

begin
  try
    ReportMemoryLeaksOnShutdown := True;

    users := TIndexedObjectList<TUser>.Create(True);
    users.Indexes.Add('Name','Name');
    users.Indexes.Add('Surname','fSurname',TClassField.cfField);
    users.Indexes.Add('id','Id');

    users2 := TSearchObjectList<TUser>.Create(False);

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
      users2.Add(user);
    end;

    //generate real entries to search
    for i := 0 to high(UserNames) do
    begin
      user := TUser.Create;
      user.Id := Random(999999999999999);
      user.Name := UserNames[i];
      user.SurName := UserSurnames[i];
      user.Age := 18 + Random(20);
      login.Username := user.Name;
      login.UserPassword := RandomPassword(8,[pfIncludeNumbers,pfIncludeSigns]);
      login.Locked := False;
      user.LoginInfo := login;

      users.Add(user);
      users2.Add(user);
    end;

    crono := TChronometer.Create;

    //test search by index
    crono.Start;
    user := users.Get('Name','Peter');
    crono.Stop;
    if user <> nil then cout('Found by Index: %s %s in %s',[user.Name,user.SurName,crono.ElapsedTime],etSuccess)
      else cout('Not found!',etError);

    //test search by normal iteration
    user := nil;
    crono.Start;
    for i := 0 to users.Count - 1 do
    begin
      //if (users[i].Name = 'Anus') or (users[i].SurName = 'Smith') then
      if users[i].Name = 'Peter' then
      begin
        user := users[i];
        crono.Stop;
        Break;
      end;
    end;
    if user <> nil then cout('Found by Iteration: %s %s at %d position in %s',[user.Name,user.SurName,i,crono.ElapsedTime],etSuccess)
      else cout('Not found by Iteration!',etError);


    //test search by Linq iteration
    crono.Start;
    //user := TLinq.From<TUser>(users2).Where('(Name = ?) OR (SurName = ?)',['Anus','Smith']).OrderBy('Name').SelectFirst;
    user := TLinq<TUser>.From(users2).Where('Name = ?',['Peter']).SelectFirst;
    crono.Stop;
    if user <> nil then cout('Found by Linq: %s %s in %s',[user.Name,user.SurName,crono.ElapsedTime],etSuccess)
      else cout('Not found by Linq! (%s)',[crono.ElapsedTime],etError);

    //test search by Linq iteration (predicate)
    crono.Start;
    //user := TLinq.From<TUser>(users2).Where('(Name = ?) OR (SurName = ?)',['Anus','Smith']).OrderBy('Name').SelectFirst;
    user := TLinq<TUser>.From(users2).Where(function(aUser : TUser) : Boolean
      begin
        Result := aUser.Name = 'Peter';
      end).SelectFirst;
    crono.Stop;
    if user <> nil then cout('Found by Linq (predicate): %s %s in %s',[user.Name,user.SurName,crono.ElapsedTime],etSuccess)
      else cout('Not found by Linq! (%s)',[crono.ElapsedTime],etError);

    //test search by embeded iteration
    crono.Start;
    user := users2.Get('Name','Peter');
    crono.Stop;
    if user <> nil then cout('Found by Search: %s %s in %s',[user.Name,user.SurName,crono.ElapsedTime],etSuccess)
      else cout('Not found!',etError);

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
    //TLinq<TUser>.From(users2).Where('Name Like ?',['p%']).Delete;

    TLinq<TUser>.From(users2).Where('Name = ?',['Peter']).Update(['Name'],['Poter']);

    //for user in TLinq<TUser>.From(users2).Where('(Name = ?) OR (SurName = ?) OR (SurName = ?)',['Peter','Smith','Huan']).Select do
    //for user in TLinq<TUser>.From(users2).Where('(Name = ?) OR (Age > ?) OR (SurName = ?)',['Anna',30,'Smith']).Select do
    //for user in TLinq<TUser>.From(users2).Where('Age > ?',[18]).Select do
    //for user in TLinq<TUser>.From(users2).Where('SurName Like ?',['%son']).Select do
    //for user in TLinq<TUser>.From(users2).Where('SurName Like ?',['p%']).Select do
    //for user in TLinq<TUser>.From(users2).Where('1 = 1',[]).Select do
    for user in TLinq<TUser>.From(users2).Where('(LoginInfo.UserName Like ?) OR (LoginInfo.UserName Like ?)',['p%','a%'])
                                         .OrderBy('Name')
                                         .Select do
    begin
      Inc(n);
      cout('Login.Username: %d. %s %s',[n,user.Name,user.SurName],etSuccess);
    end;
    if user = nil then cout('Not found by Linq!',etError);

    cout('Press a key to Exit',etInfo);
    Readln;
    users.Free;
    users2.Free;
    crono.Free;
  except
    on E: Exception do
      cout('%s : %s',[E.ClassName,E.Message],etError);
  end;
end.
