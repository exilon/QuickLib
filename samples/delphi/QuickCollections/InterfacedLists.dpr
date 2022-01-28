program InterfacedLists;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  System.Generics.Collections,
  Quick.Collections,
  Quick.Linq;

type

  TUser = class
  private
    fName : string;
    fAge : Integer;
    fRoles : TArray<string>;
    fRoles2 : IList<string>;
  public
    constructor Create(const aName : string; aAge : Integer; aRoles : TArray<string>);
    property Name : string read fName write fName;
    property Age : Integer read fAge write fAge;
    property Roles : TArray<string> read fRoles write fRoles;
    property Roles2 : IList<string> read fRoles2 write fRoles2;
  end;

var
  List : IList<string>;

  ListObj : IObjectList<TUser>;

  myarray : TArray<string>;

  user : TUser;
  name : string;

{ TMyItem }

constructor TUser.Create(const aName : string; aAge : Integer; aRoles : TArray<string>);
begin
  fName := aName;
  fAge := aAge;
  fRoles := aRoles;
  fRoles2 := TXList<string>.Create;
  fRoles2.AddRange(aRoles);
end;

begin
  try
    ReportMemoryLeaksOnShutdown := True;

    //add values
    myarray := ['Joe','Mat','Lee'];
    //remove if starts with J
    myarray.Where('^J',True).Delete;
    //search for regex match
    cout('Search for regex match',ccYellow);
    for name in myarray.Where('e$',True).Select do
    begin
      cout('User %s ends with "e"',[name],etInfo);
    end;

    //add values to list
    List := TXList<string>.Create;
    List.Add('Joe');
    List.Add('Mat');
    List.Add('Lee');

    //get from index
    cout('User is %s',[List[2]],etInfo);

    //search for regex match
    cout('Search for regex match',ccYellow);
    for name in List.Where('^Ma',True).Select do
    begin
      cout('User %s starts with "Ma"',[name],etInfo);
    end;

    //add values to objectlist
    ListObj := TXObjectList<TUser>.Create;
    ListObj.Add(TUser.Create('Joe',22,['LocalAdmin']));
    ListObj.Add(TUser.Create('Mat',30,['SuperAdmin','DomainAdmin']));
    ListObj.Add(TUser.Create('Lee',40,['User']));

    //search for a object property
    cout('Search for a object property match with WhereClause',ccYellow);
    user := ListObj.Where('Name = ?',['Lee']).SelectFirst;
    if user <> nil then cout('%s is %d years old',[user.Name,user.Age],etInfo);

    //search with predicate
    cout('Search for a property match with Predicate',ccYellow);
    user := ListObj.Where(function(aUser : TUser) : Boolean
      begin
        Result := aUser.Name.StartsWith('J');
      end).SelectFirst;
    if user <> nil then cout('%s starts with J letter',[user.Name],etInfo);

    //search into a array property
    cout('Search into a array property',ccYellow);
    user := ListObj.Where('Roles2 CONTAINS ?',['SuperAdmin']).SelectFirst;
    if user <> nil then cout('%s is %s',[user.Name,CommaText(user.Roles)],etInfo);

    cout('List before remove Mat',ccYellow);
    for user in ListObj do
    begin
      cout('User "%s"',[user.Name],etInfo);
    end;
    ListObj.Where('Name = ?',['Mat']).Delete;
    cout('List after remove Mat',ccYellow);
    for user in ListObj do
    begin
      cout('User "%s"',[user.Name],etInfo);
    end;

    cout('Press ENTER to Exit',ccYellow);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
