program IndexedList;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Chrono,
  Quick.Lists;

type
  TUser = class
  private
    fId : Int64;
    fName : string;
    fSurName : string;
    fAge : Integer;
  published
    property Id : Int64 read fId write fId;
    property Name : string read fName write fName;
    property SurName : string read fSurName write fSurName;
    property Age : Integer read fAge write fAge;
  end;


const
  numusers = 100000;
  UserNames : array of string = ['Cliff','Alan','Anna','Phil','John','Michel','Jennifer','Peter','Brandon','Joe','Steve','Lorraine','Bill','Tom'];
  UserSurnames : array of string = ['Gordon','Summer','Huan','Paterson','Johnson','Michelson','Smith','Peterson','Miller','McCarney','Roller','Gonzalez','Thomson','Muller'];


var
  users : TIndexedObjectList<TUser>;
  users2 : TSearchObjectList<TUser>;
  user : TUser;
  i : Integer;
  crono : TChronometer;

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
    crono.Start;
    for i := 0 to users.Count - 1 do
    begin
      if users[i].Name = 'Peter' then
      begin
        crono.Stop;
        cout('Found by Iteration: %s %s at %d position in %s',[user.Name,user.SurName,i,crono.ElapsedTime],etSuccess);
        Break;
      end;
    end;

    //test search by embeded iteration
    crono.Start;
    user := users2.Get('Name','Peter');
    crono.Stop;
    if user <> nil then cout('Found by Search: %s %s in %s',[user.Name,user.SurName,crono.ElapsedTime],etSuccess)
      else cout('Not found!',etError);

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
