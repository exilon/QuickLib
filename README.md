


**QuickLib**
--------

Small delphi/fpc library containing interesting and quick to implement functions, created to simplify application development and crossplatform support.
* NEW: Improved firemonkey android compatibility.
* NEW: JsonRecord
* NEW: AutoMapper
* NEW: JsonSerializer
* NEW: Improved Linux compatibility.
* NEW: Delphinus support

----------
**Quick.AppService:** Allow a console app to run as console mode or service mode with same code simplifying debug tasks.

```delphi
if not AppService.IsRunningAsService then
begin
    ...your code running as console
end
else
begin
    AppService.ServiceName := 'MyService';
    AppService.DisplayName := 'MyServicesvc';
    //you can pass an anonymous method to events
    AppService.OnStart := procedure
                          begin
                            ...your start code
	                      end;
    AppService.OnExecute := YourExecuteFunction;
    AppService.OnStop := YourStopFunction;
    AppService.CheckParams;
end;
```

**Quick.Azure/Amazon:** Simplifies blob iteraction with Azure and Amazon Cloud Storage.

```delphi
//connect to a Azure blobstorage
QuickAzure := TQuickAzure.Create(AzureAccountName,AzureAccountKey);

//download a blob file to a stream
done := QuickAzure.GetBlob('MyContainer','MyFile.jpg',ResponseInfo,MyStream);
    
//check if exists a folder
found := ExistFolder('MyContainer','/Public/Documents/Personal');
    
//list blobs starting with a pattern (recursively or not)
for azBlob in ListBlobs('MyContainer','/Public/Documents',Recursive,ResponseInfo) do
begin
    if azBlob.Size > 1000 then Showmessage(azBlob.Name);
end;
```


**Quick.Network:** CIDR and IP Range functions.

```delphi
//convert ip string to integer
IPv4ToInt('192.168.1.10');

//get first and last ip of a subnet scope
GetIpRange('192.168.100.0','255.255.255.0',LowIp,HighIP);
```

**Quick.Commons:** Functions frequently needed in the everyday of a developer.

```delphi
//coverts UTC time TDateTime to Local date time
UTCToLocalTime(MyUTCTime);
    
//generate a 10 char length random password with alfanumeric and signs.
RandomPassword(10,[pfIncludeNumbers,pfIncludeSigns]);

//Capitalize every word of a phrase
CapitalizeAll('the grey fox'); //returns "The Grey Fox"

//Simple TCounter and TTimeCounter for loops
counter := TCounter;
counter.Init(200);
timecounter : TTimeCounter;
timecounter.Init(10000);
while true do
begin
    Inc(n);
    {your procedural process here}
    //every 200 steps writes to console
    if counter.Check then writeln(Format('Processed %d entries',[n]));
    //every 10 seconds writes to console
    if timecounter.Check then writeln('Im working...'); 
end;
```

**Quick.Chrono:** Chronometer and Benchmark a piece of code is simple.

```delphi
//get elapsed time execution of a code part
Chrono := TChronometer.Create(False);
Chrono.Start;
...code you need benchmark
Chrono.Stop;

//shows elapsed time in LongTime format (2 hour(s) and 10 minute(s))
Showmessage(Chrono.TimeElapsed(True));

//shows elapsed time in ShortTime format (02:10:00)
Showmessage(Chrono.TimeElapsed(False));
//get benchmak info of a process
Chrono := TChronoBenchMark.Create;
Chrono.TotalProcess := 100000;
for i := 1 to 10000 do
begin
    {your process here}
    Chrono.CurrentProcess := i;
    //shows estimated time your process will take in x hour(s), x minute(s) x second(s) format
    writeln(Chrono.EstimatedTime(True));
    //shows speed: num items per second processed of your process
    writeln(Format('Items processed %d/sec',[Chrono.Speed]));
end;
writeln(Chrono.ElapsedTime(False)); //shows total time elapsed in 00:00:00 format
```

**Quick.Console:** Write log messages to console with colors and more...

```delphi
//define which level of output needed
Console.Verbose := LOG_DEBUG;

//writes line to console in red color
cout('Error x',etError); 

//writes formatted line in green color
coutFmt('Proccess %s finished',[ProccesName],etSuccess);

//writes integer
cout(12348);

//Connect a QuickLog and write to disk and screen with one line of code (with independent verbose levels)
MyQuickLog := TQuickLog.Create;
MyQuickLog.Verbose := LOG_ALL;
Console.Verbose := LOG_ONLYERRORS;
Console.Log := MyQuickLog;
```

**Quick.Log:** Log to disk or memory with verbose levels and daily or max space rotation.

```delphi
//write a header on start with info as running path, appname, debugmode, user, etc...
Log.ShowHeader := True;

//sets log with rotation at 20MB
Log.SetLog('.\mylog.log',False,20);

//write an error message
Log.Add('Error x',etError);

//write formatted error message
Log.Add('Error is %s',[ErrorStr],etError);
```

**Quick.Config:** Load/Save a config as json file or Windows Registry keys. Create a descend class from TAppConfig and add private variables will be loaded/saved.

```delphi
//create a class heritage
TMyConfig = class(TAppConfig)
private
    fName : string;
    fSurname : string;
    fStatus : Integer;
public
    property Name : string read fName write fName;
    property SurName : string read fSurname write fSurname;
    property Status : Integer read fStatus write fStatus;
end;
  
//create your config to json file
//Add Quick.Config.Json to your uses
AppConfigJson := TAppConfigJsonProvider<TMyConfig>.Create(MyConfig);
AppConfigJson.CreateIfNotExists := True;
AppConfigJson.Filename := 'Config.json';
MyConfig.Name := 'John';
MyConfig.Surname := 'Smith';
//load
AppConfigJson.Load(MyConfig);
//save
AppConfigJson.Save(MyConfig);
  
//create your config to Windows Registry
//Add Quick.Config.Registry to your uses
AppConfigReg := TAppConfigRegistryProvider<TMyConfig>.Create(MyConfig);
//Define Registry as HKEY_CURRENT_USER\Software\MyApp
AppConfigReg.HRoot := HKEY_CURRENT_USER; 
AppConfigReg.MainKey := 'MyApp';
MyConfig.Name := 'John';
MyConfig.Surname := 'Smith';
//load
AppConfigReg.Load(Config);
//save
AppConfigReg.Save(Config);
```

**Quick.FileMonitor:** Monitorizes a file for changes and throws events.

```delphi
FileMonitor.Filename := '.\myfile.txt';
//check file changes every 2 seconds
FileMonitor.Interval := 2000;
//watch for deleted or modified file events
FileMonitor.Notifies := [mnFileModified, mnFileDeleted)];
FileMonitor.OnFileChange := MyFileChangeFunction;
FileMonitor.Enabled := True;
```
	
**Quick.JsonUtils:** Utils for working with json objects.

```delphi
//When unit declared in uses, a TObject Helper allows all your objects to be loaded or saved to/from json string
MyObject.FromJson := jsonstring;
MyString := MyObject.ToJson;

//You can clone simple objects with clone function
MyObject1.Clone(MyObject2);
```

**Quick.SMTP:** Send email with two code lines.

```delphi
//Send email
SMTP := TSMTP.Create('mail.domain.com',25,False);
SMTP.SendMail('my@email.com','to@email.com','Email subject','My message body');

//You can define more advanced options
SMTP.SenderName := 'John';
SMTP.From := 'my@email.com';
SMTP.Recipient := 'one@email.com,two@email.com';
SMTP.Subject := 'Email subject';
SMTP.AddBodyFromFile := '.\body.html';
SMTP.CC := 'other@email.com';
SMTP.BCC := 'more@email.com';
SMTP.Attachments.Add('.\notes.txt');
SMTP.SendMail;
```

**Quick.Threads:** Thread safe classes.
- TThreadedQueueCS: Version of TThreadedQueue with Critical Section.
- TThreadObjectList: Thread safe Object List.
- TThreadedQueueList: Thread safe Queue List. Autogrow and with Critical Section.

**Quick.Process:** Manages windows processes.
```delphi
//kill explorer process
KillProcess('explorer.exe');
//determine if an application is running
if IsProcessRunning('explorer.exe') then Showmessage('Explorer is running!');
//get username who is running an exe
writeln('Explorer.exe open by: ' + GetProcessUser('explorer.exe');
//gets handle of a window with a 20 seconds timeout
if FindWindowTimeout('MainWindow',20) then writeln('Window detected');
```

**Quick.Process:** Manages windows services.
```delphi
//detect if a service is installed
if not ServiceIsPresent('localhost','MySvc') then raise Exception.Create('Service not installed!');
//Start a service
ServiceStart('localhost','MySvc');
//Uninstall a service
ServiceUninstall('MySvc');
```

**Quick.Format:** String format.

```delphi
//Format bytes to MB, GB, TB...
FormatBytes(50000) //shows 50KB
FormatBytes(90000000) //shows 90MB
```

**Quick.JsonSerializer:** Serializes an object from/to json text. You can define if public or published will be processed (only Delphi, fpc rtti only supports published properties)	

```delphi
json := '{"name":"Peter","age":30}';
serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
try
   serializer.JsonToObject(user,json);
finally
   serializer.Free;
end;
```

**Quick.AutoMapper:** Map fields from one class to another class. Allows custom mappings to match different fields.	

```delphi
//Map values from User1 to User2
TMapper<TUser2>.Map(User);

//Map custom mappings
AutoMapper := TAutoMapper<TUser,TUser2>.Create;
AutoMapper.CustomMapping.AddMap('Cash','Money');
AutoMapper.CustomMapping.AddMap('Id','IdUser');
User2 := AutoMapper.Map(User);
```

**Quick.JsonRecord:** Used as a DTO class, with json serialize and mapping functions included.	

```delphi
type
   TUser = class(TJsonRecord)
   private
      fName : string;
      fAge : Integer;
   published
      property Name : string read fName write fName;
      property Age : Integer read fAge write fAge;
   end;
var
   user, user2 : TUser;
begin
   user := TUser.Create;
   Writeln(user.ToJson);
   user.Mapto(User2);
   Writeln(user2.ToJson);
end;
```