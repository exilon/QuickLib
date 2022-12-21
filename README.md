![alt text](docs/QuickLib.png "QuickLib") 

QuickLib is a delphi/Firemonkey(Windows, Linux, Android, OSX & IOS) and fpc(Windows & Linux) library containing interesting and quick to implement functions, created to simplify application development and crossplatform support and improve productivity. Delphi XE8 - Delphi 11 Alexandria supported.

## Give it a star
Please "star" this project in GitHub! It costs nothing but helps to reference the code.
![alt text](docs/githubstartme.jpg "Give it a star")

## Support
If you find this project useful, please consider making a donation.

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=BKLKPNEYKSBKL)

**Areas of functionality:**
  
* **Mapping**: Map fields from a class to other class, copy objects, etc..
* **Config**: Use your config as an object and load/save from/to file (Json/Yaml) or Windows Registry.
* **Serialization**: Serialize objects to/from json/Yaml.
* **Scheduling**: Schedule tasks launching as independent threads with retry policies.
* **Threading**: Simplify run and control of multithread background tasks, Thread-safe Lists, queues, etc
* **Data**: Flexible data interchange and storage, allowing several input-output types.
* **Cloud**: Simplify cloud Azure/Amazon file management, send emails and more.
* **Querying**: Indexed Lists, Searchable Lists and Linq query system for generic lists and arrays.
* **Benchmark**: Time elapsed control and benchmark functions.
* **Filesystem**: Process and Services control, file modify monitors and helpers, etc...
* **FailControl**: Fail and Retry policies.
* **Caching:**: Cache string or objects to retrieve fast later.
* **Templating:** Simple string templating with dictionaries.
* **Debuging:** Utils to debug your code.
* **Parameters:** Work with commandline parameters.

**Main units description:**

* **Quick.Commons:** Functions frequently needed in the day to day of a developer. 
* **Quick.AppService:** Allow a console app to run as console mode or service mode with same code simplifying debug tasks.
* **Quick.Azure/Amazon:** Simplifies blob iteraction with Azure and Amazon Cloud Storage.
* **Quick.Network:** CIDR and IP Range functions.
* **Quick.Chrono:** Chronometer and Benchmark a piece of code is simple.
* **Quick.Console:** Write log messages to console with colors and more...
* **Quick.Log:** Log to disk or memory with verbose levels and daily or max space rotation.
* **Quick.Config:** Load/Save a config as Json or Yaml file or Windows Registry keys and manage it as an object.
* **Quick.FileMonitor:** Monitorizes a file for changes and throws events.
* **Quick.JsonUtils:** Utils for working with json objects.
* **Quick.SMTP:** Send email with two code lines.
* **Quick.Threads:** Thread safe classes, scheduling and backgrounds tasks with retry policies.
* **Quick.Process:** Manages windows processes.
* **Quick.Services:** Manages windows services.
* **Quick.Format:** String format.
* **Quick.RTTI.Utils:** Simplifies working with RTTI.
* **Quick.JsonSerializer:** Serializes an object from/to json text. You can define if public or published will be processed (only Delphi, fpc rtti only supports published properties)	
* **Quick.AutoMapper:** Map fields from one class to another class. Allows custom mappings to match different fields and custom mapping procedure to cast/convert fields manually.	
* **Quick.JsonRecord:** Used as a DTO class, with json serialize and mapping functions included.	
* **Quick.Lists:** Improved lists with indexing or search features.
* **Quick.Value** FlexValue stores any data type and allow pass to other class with integrated operators and autofrees.
* **Quick.Arrays:** Improved arrays.
* **Quick.YAML:** Yaml object structure.
* **Quick.YAML.Serializer:** Serialize/Deserialize object from/to Yaml.
* **Quick.Expression:** Evaluate object properties using expressions.
* **Quick.Linq:** Makes Linq queries to any TObjectList<T>, TList<T>, TArray<T> and TXArray<T>, performing Select by complex Where like SQL syntax, update and order over your list.
* **Quick.MemoryCache:** Caches objects/info with an expiration time, to avoid generate this info everytime is needed (database queries, hard to calculate info, etc).
* **Quick.Collections:** Collections improvements like IList and IObjectList with Linq inherited.
* **Quick.Pooling:** Creation of object pool to avoid external resource consum exhausts and overheads.
* **Quick.Template:** String template replacing with dictionary or delegate.
* **Quick.Debug.Utils:** Simple debugging and code benchmark utils.
* **Quick.Parameters:** Work with commandline parameters like a class.
* **Quick.Url.Utils:** Simple url manipulation
* **Quick.RegEx.Utils:** Commonly used RegEx comparison (email verification, password complexity, etc)
* **Quick.Conditions:** Pre and postcondition validations in fluent style.


**Updates:**

* NEW: RAD Studio 11 supported
* NEW: Condition checks
* NEW: Commonly used RegEx validations
* NEW: Url manipulation utils
* NEW: QuickParameters to work with commandline arguments like a class.
* NEW: HttpServer custom and dynamic error pages.
* NEW: Debug utils
* NEW: String Template
* NEW: RAD Studio 10.4 supported
* NEW: Collections: IList and IObjectList with linQ support.
* NEW: Pooling: ObjectPool.
* NEW: Options file settings with sections.
* NEW: MemoryCache with expiration & object compression.
* NEW: Now included on RAD Studio GetIt package manager.
* NEW: Background & Scheduled task with retry policies
* NEW: RunTask, FaultControl
* NEW: Linq over generic lists and arrays.
* NEW: QuickConfig YAML provider.
* NEW: YAML Object and Serializer
* NEW: AutoMapper customapping path namespaces style.
* NEW: FlexArray, FlexPair & FlexPairArray.
* NEW: AutoMapper mapping procedures (see documentation below)
* NEW: JsonSerializer improved
* NEW: TXArray: array like TList
* NEW: Delphi Linux compatibility
* NEW: QuickConfigJson reload if config file changed
* NEW: First version with OSX/IOS partial support
* NEW: Delphinus-Support

**Installation:**
----------
* **From package managers:**
1. Search "QuickLib" on Delphinus or GetIt package managers and click *Install*
* **From Github:**
1. Clone Github repository or download zip file and extract it.
2. Add QuickLib folder to your path libraries on Delphi IDE.

**Documentation:**
----------
**Quick.AppService:**
--
Allow a console app to run as console mode or service mode with same code simplifying debug tasks.

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

**Quick.Azure/Amazon:**
--
Simplifies blob iteraction with Azure and Amazon Cloud Storage.

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


**Quick.Network:**
--
CIDR and IP Range functions.

```delphi
//convert ip string to integer
IPv4ToInt('192.168.1.10');

//get first and last ip of a subnet scope
GetIpRange('192.168.100.0','255.255.255.0',LowIp,HighIP);
```

**Quick.Commons:**
--
Functions frequently needed in the everyday of a developer.

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

**Quick.Chrono:**
--
Chronometer and Benchmark a piece of code is simple.

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

**Quick.Console:**
--
Write log messages to console with colors and more...

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

**Quick.Log:**
--
Log to disk or memory with verbose levels and daily or max space rotation.

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

**Quick.Config:**
--
Load/Save a config as Json or Yaml file or Windows Registry keys. Create a descend class from TAppConfigJson, TAppConfigYaml or TAppConfigRegistry and added published properties will be loaded/saved. Files configs can be reloaded on detect files changes.

```delphi
//create a class heritage
TMyConfig = class(TAppConfigJson)
private
    fName : string;
    fSurname : string;
    fStatus : Integer;
published
    property Name : string read fName write fName;
    property SurName : string read fSurname write fSurname;
    property Status : Integer read fStatus write fStatus;
end;

//create your config to json file
//Add Quick.Config.Json to your uses
MyConfig := TMyConfig.Create('Config.json');
MyConfig.Provider.CreateIfNotExists := True;
MyConfig.Provider.ReloadIfFileModified := True;
MyConfig.Name := 'John';
MyConfig.Surname := 'Smith';
//load
MyConfig.Load;
//save
MyConfig.Save;
  
//create your config to Windows Registry
//Add Quick.Config.Registry to your uses
MyConfig := TMyConfig.Create;
//Define Registry as HKEY_CURRENT_USER\Software\MyApp
MyConfig.HRoot := HKEY_CURRENT_USER; 
MyConfig.MainKey := 'MyApp';
MyConfig.Name := 'John';
MyConfig.Surname := 'Smith';
//load
MyConfig.Load;
//save
MyConfig.Save;

//Create a custom Config with no default provider
TMyConfig = class(TAppConfig)
...your properties
end;

MyConfig := TMyConfig.Create(TAppConfigJsonProvider.Create('.\config.json');

```

**Quick.FileMonitor:**
--
Monitorizes a file for changes and throws events.

```delphi
FileMonitor.Filename := '.\myfile.txt';
//check file changes every 2 seconds
FileMonitor.Interval := 2000;
//watch for deleted or modified file events
FileMonitor.Notifies := [mnFileModified, mnFileDeleted)];
FileMonitor.OnFileChange := MyFileChangeFunction;
FileMonitor.Enabled := True;
```
	
**Quick.JsonUtils:**
--
Utils for working with json objects.

```delphi
//When unit declared in uses, a TObject Helper allows all your objects to be loaded or saved to/from json string
MyObject.FromJson := jsonstring;
MyString := MyObject.ToJson;

//You can clone simple objects with clone function
MyObject1.Clone(MyObject2);
```

**Quick.SMTP:**
--
Send email with two code lines.

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

**Quick.Threads:**
--
Thread safe classes.

**TThreadedQueueCS:** Version of TThreadedQueue with Critical Section.

**TThreadObjectList:** Thread safe Object List.

**TThreadedQueueList:** Thread safe Queue List. Autogrow and with Critical Section.

**TAnonymousThread:** Creates anonymous thread defining unchained Execute and OnTerminate methods. Use Execute_Sync and OnTerminate_Sync methods if code needs to update UI.
  - **Execute:** Specify code to execute on start.
  - **Execute_Sync:** Like Execute but runs code with syncronized thread method (avoids problems if your code updates UI).
  - **OnTerminate:** Specify code to execute when task finishes.
  - **OnTerminate_Sync:** Like OnTerminate but runs code with syncronized thread method (avoids problems if your code updates UI).
  - **Start:** Starts thread execution.
```delphi
//simple anonymousthread
TAnonymousThread.Execute(
      procedure
      var
        i : Integer;
      begin
        for i := 0 to 10 do cout('Working %d',[i],etTrace);
        cout('executed thread',etSuccess);
      end)
    .OnTerminate(
      procedure
      begin
        cout('terminated thread',etSuccess);
        cout('PRESS <ENTER> TO EXIT',etInfo);
      end)
    .Start;
```

**TRunTask:** Launch an autofree single task thread with fault & retry control policies. Params can be passed and created into code.
- *Define code to execute:*
  - **Execute:** Specify Task name, parameters to pass to anonymous method(If OwnedParams=true, task will free params on termination task) and method than will be executed. 
  - **Execute_Sync:** Like Execute but runs code with synchronize thread method (avoids problems if your code updates UI).
  - **SetParameter:** Defines values or objects needed by your task.
- *Define events to control:*
  - **OnInitialize:** Specify code to run before main execute task (this code only runs one time, OnExecute can be retried more than one time)
  - **OnRetry:** Specify code to run when execution fails and decide if needs to retry or cancel next retries.
  - **OnTerminate:** Specify code to execute when task finishes.
  - **OnTerminate_Sync:** Like OnTerminate but runs code with syncronized thread method (avoids problems if your code updates UI).
  - **OnException:** Specify code to execute when task generates an exception.
- *Define fail/retry policies:*
  - **RetryForever:** If execution fails, code will be retry forever until task executes ok.
  - **Retry:** If execution fails, code will be retry x times.
  - **WaitAndRetry:** If execution fails, code will be retry x times, and wait x millisecons before each retry. You can specify number of retries and wait time between retries.
  - **Run:** Starts task execution.
```delphi
  TRunTask.Execute(
      procedure(task : ITask)
      var
        stream : TStringStream;
        response : IHttpRequestResponse;
      begin
        stream := TStringStream.Create;
        try
          response := TJsonHttpClient(task['httpclient'].AsObject).Get(task['url']);
          task.Result := response.StatusCode;
          if response.StatusCode <> 200 then raise Exception.Create(response.StatusText);
        finally
          stream.Free;
        end;
      end)
    .SetParameter('httpclient',(TJsonHttpClient.Create),True)
    .SetParameter('url','https://mydomain.com/testfile')
    .WaitAndRetry(5,250,2)
    .OnRetry(
      procedure(task : ITask; aException : Exception; var vStopRetries : Boolean)
      begin
        //if error 404 don't try to retry request
        if task.Result = 404 then vStopRetries := True;
      end)
    .OnException(
      procedure(task : ITask; aException : Exception)
      begin
        coutFmt('Exception downloading (Error: %s / StatusCode: %d)...',[aException.Message,task.Result.AsInteger],etError);
      end)
    .OnTerminated(
      procedure(task : ITask)
      begin
        if task.Done then coutFmt('Download "%s" finished ok',[task['url'].AsString],etSuccess)
          else coutFmt('Download "%s" failed after %d retries',[task['url'].AsString,task.NumRetries],etError);
      end)
    .Run;
```

**TBackgroundsTasks:** Launch tasks in background allowing number of concurrent workers with fault and retry control policies. Use AddTask_Sync and OnTerminate_Sync methods if code needs to update UI.
- *Add a task to execute:*
  - **AddTask:** Specify Task name, parameters to pass to anonymous method(If OwnedParams=true, task will free params on expiration task) and method than will be executed. 
  - **AddTask_Sync:** Like AddTask but runs code with synchronize thread method (avoids problems if your code updates UI).
  - **SetParameter:** Defines values or objects needed by your task. Every parameter will be accesible into anomymous methods defines as task[<name>] or task.[index]
- *Define events to control:*
  - **OnInitialize:** Specify code to run before main execute task (this code only runs one time, OnExecute can be retried more than one time)
  - **OnRetry:** Specify code to run when execution fails and decide if needs to retry or cancel next retries.
  - **OnTerminate:** Specify code to execute when task finishes.
  - **OnTerminate_Sync:* Like OnTerminate but runs code with syncronized thread method (avoids problems if your code updates UI).
  - **OnException:** Specify code to execute when task generates an exception.
- *Define fail/retry policies:*
  - **RetryForever:** If execution fails, code will be retry forever until task executes ok.
  - **Retry:** If execution fails, code will be retry x times. Allow define array of milliseconds as wait time.
  - **WaitAndRetry:** If execution fails, code will be retry x times, and wait x millisecons before each retry. You can specify number of retries and wait time between retries.
- *Begin execution:*
  - **Start:** Starts tasks execution.
```delphi
    backgroundtasks := TBackgroundTasks.Create(10);
    for i := 1 to 100 do
    begin
      mytask := TMyTask.Create;
      mytask.Id := i;
      mytask.Name := 'Task' + i.ToString;
      backgroundtasks.AddTask([mytask],False,
                              procedure(task : ITask)
                              begin
                                cout('task %d started',[TMyTask(task.Param[0].AsObject).Id],etDebug);
                                TMyTask(task.Param[0].AsObject).DoJob;
                              end
							).WaitAndRetry([250,2000,10000])
                            ).OnException(
                              procedure(task : ITask; aException : Exception)
                              begin
                                cout('task %d failed (%s)',[TMyTask(task.Param[0].AsObject).Id,aException.Message],etError);
                              end
                            ).OnTerminated(
                              procedure(task : ITask)
                              begin
                                cout('task %d finished',[TMyTask(task.Param[0].AsObject).Id],etDebug);
                                TMyTask(task.Param[0].AsObject).Free;
                              end
                            ).Run;
    end;
    backgroundtasks.Start;
```
**TScheduledTasks:** Alternative to Timer. You can assign tasks with start time, repeat options and expiration date and fail and retry control policies. Use AddTask_Sync, OnTerminate_Sync and OnExpired_Sync if code needs to update UI.
You can assign anonymous methods to execute, exception, terminate and expiration events.
- *Add a task to execute:*
  - **AddTask:** Specify Task name, parameters to pass to anonymous method(If OwnedParams=true, task will free params on expiration task) and method than will be executed. 
  - **AddTask_Sync:** Like AddTask but runs code with synchronize thread method (avoids problems if your code updates UI).
  - **SetParameter:** Defines values or objects needed by your task. Every parameter will be accesible into anomymous methods defines as task[<name>] or task.[index]
- *Define events to control:*
  - **OnInitialize:** Specify code to run before main execute task (this code only runs one time, OnExecute can be retried more than one time)
  - **OnRetry:** Specify code to run when execution fails and decide if needs to retry or cancel next retries.
  - **OnTerminate:** Specify code to execute when task finishes.
  - **OnTerminate_Sync:** Like OnTerminate but runs code with syncronized thread method (avoids problems if your code updates UI).
  - **OnExpire:** Specify code to execute when task expiration reached or task was cancelled.
  - **OnExpire_Sync:** Like OnExpire but runs code with synchronized thread method (avoids problems if your code updates UI).
  - **OnException:** Specify code to execute when task generates an exception.
- *Define when to start task:*
  - **StartNow:** Start task immediately.
  - **StartAt:** Date and time to start task. 
  - **StartTodayAt:** Start task today at defined time.
  - **StartTomorrowAt:** Start task tomorrow at defined time.
  - **StartOnDayChange:** Start task when day changes.
  - **StartInMinutes:** Start task after x minutes.
  - **StartInSeconds:** Start task after x seconds.
- *Define if needs to repeat or not (if not defined a previous StartAt, StartOn, etc, task will be executed immediately):*
  - **RunOnce:** Task will be executed one time only. 
  - **RepeatEvery:** Can indicate repeat step over time and expiration date.
  - **RepeatEveryDay:** Repeat task every day at same hour.
  - **RepeatEveryWeek:** Repeat task every week at same hour.
- *Define fail/retry policies:*
  - **RetryForever:** If execution fails, code will be retry forever until task executes ok.
  - **Retry:** If execution fails, code will be retry x times.
  - **WaitAndRetry:** If execution fails, code will be retry x times, and wait x millisecons before each retry. You can specify number of retries and wait time between retries.
- *Start/Stop scheduler:*
  - **Start:** Starts scheduler.
  - **Stop:** Stops scheduler.
```delphi
myjob := TMyJob.Create;
myjob.Name := Format('Run at %s and repeat every 1 second until %s',[DateTimeToStr(ScheduledDate),DateTimeToStr(ExpirationDate)]);
scheduledtasks.AddTask('Task1',[myjob],True,
                            procedure(task : ITask)
                            begin
                              cout('task "%s" started',[TMyTask(task.Param[0]).Name],etDebug);
                              TMyJob(task.Param[0]).DoJob;
                            end
                          ).OnException(
                            procedure(task : ITask; aException : Exception)
                            begin
                              cout('task "%s" failed (%s)',[TMyJob(task.Param[0]).Name,aException.Message],etError);
                            end
                          ).OnTerminated(
                            procedure(task : ITask)
                            begin
                              cout('task "%s" finished',[TMyJob(task.Param[0]).Name],etDebug);
                            end
                          ).OnExpired(
                            procedure(task : ITask)
                            begin
                              cout('task "%s" expired',[TMyJob(task.Param[0]).Name],etWarning);
                            end
                          ).StartAt(ScheduledDate
                          ).RepeatEvery(1,TTimeMeasure.tmSeconds,ExpirationDate);
scheduledtasks.Start;
```

- **ITask:** Interface passed to every task event of TRunTask, TBackgroundTasks and TScheduledTasks.
  - **NumWorker:** Return number of worker assigned to execute task.
  - **Result:** Can store any value type (TFlexValue is like variant type)
  - **Param[name]:** Can store parameters passed to task or created dynamically into every anonymous methods passed to each event.
  - **Param[index]:** Can store parameters passed to task or created dynamically into every anonymous methods passed to each event.
  - **Done:** Return true is task is executed without errors.
  - **Failed:** Return true is task has failed.
  - **IdTask:** Task id defined.
  - **NumRetries:** Number of retries done.
  - **MaxRetries:** Number of maximum retries allowed before mark task as failed.
  - **LastException:** Return last exception of a failed task.
  - **CircuitBreaked:** Return true if max retries has been reached or user cancelled into OnRetry event.
  - **IsEnabled:** Return status of task.

**Quick.FaultControl:**
--
Manages fail and retry policies, defining max retries, wait time beetween retries and circuit break mecanism.

**Quick.Process:**
--
Manages windows processes.
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

**Quick.Services:**
--
Manages windows services.
```delphi
//detect if a service is installed
if not ServiceIsPresent('localhost','MySvc') then raise Exception.Create('Service not installed!');
//Start a service
ServiceStart('localhost','MySvc');
//Uninstall a service
ServiceUninstall('MySvc');
```

**Quick.Format:**
--
String format.

```delphi
//Format bytes to MB, GB, TB...
FormatBytes(50000) //shows 50KB
FormatBytes(90000000) //shows 90MB
```

**Quick.JsonSerializer:**
--
Serializes an object from/to json text. You can define if public or published will be processed (only Delphi, fpc rtti only supports published properties)	

```delphi
json := '{"name":"Peter","age":30}';
serializer := TJsonSerializer.Create(TSerializeLevel.slPublishedProperty);
try
   serializer.JsonToObject(user,json);
finally
   serializer.Free;
end;
```

**Quick.AutoMapper:**
--
Map fields from one class to another class. Allows custom mappings to match different fields and custom mapping procedure to cast/convert fields manually.	

```delphi
//Map values from User1 to User2
TMapper<TUser2>.Map(User);

//Map custom mappings
AutoMapper := TAutoMapper<TUser,TUser2>.Create;

//option1: you can define auto map different named properties
AutoMapper.CustomMapping.AddMap('Cash','Money');
AutoMapper.CustomMapping.AddMap('Id','IdUser');

//option2: you can decide to modify each property manually or allow to auto someones
AutoMapper.OnDoMapping := procedure(const aSrcObj : TUser; const aTargetName : string; out Value : TFlexValue)
                          begin
                            if aTargetName = 'Money' then Value := aSrcObj.Cash * 2
                              else if aTargetName = 'IdUser' then Value := aSrcObj.Id;
                          end;

//option3: you can modify some properties after automapping done
AutoMapper.OnAfterMapping := procedure(const aSrcObj : TUser; aTgtObj : TUser2)
                             begin
                               aTgtObj.Money := aSrcObj.Cash * 2;
                               aTgtObj.IdUser := aSrcObj.Id;
                             end;

User2 := AutoMapper.Map(User);
```

**Quick.JsonRecord:**
--
Used as a DTO class, with json serialize and mapping functions included.	

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
   //show as json string
   Writeln(user.ToJson);
   //mapping to other class
   user.Mapto(User2);
   Writeln(user2.ToJson);
   //load from file
   user.LoadFromFile('.\user.json');
   //save to file
   user2.SaveToFile('.\user2.json');
end;
```

**Quick.Lists:**
--
Improved lists with indexing or search features.
- **TIndexedObjectList:** Allows fast hashed searches by object properties or fields.
- **TSearchObjectList:** Allows iteration search by object properties or fields.
```delphi
var
   users : TIndexedObjectList<TUser>;
begin
   users := TIndexedObjectList<TUser>.Create(True);
   //create index by property "Name"
   users.Indexes.Add('Name','Name',TClassField.cfProperty);
   //create index by private field "Id"
   users.Indexes.Add('Id','fId',TClassField.cfField);
   //get user by "Name" index
   writeln(users.Get('Name','Peter').SurName);
end;
```

**Quick.Value**
--
FlexValue stores any data type and allow pass to other class with integrated operators and autofrees.
```delphi
var
  value : TFlexValue;
  str : string;
  num : Integer; 
begin
  value := 'hello';
  str := value;
  value := 123;
  num := value;
end;
```

**Quick.Arrays:**
--
Improved arrays.

**TXArray:** Array with methods like TList.
```delphi
var
   users : TXArray<TUser>;
begin
   users.Add(User);
   if users.Count:= TIndexedObjectList<TUser>.Create(True);
   //create index by property "Name"
   users.Indexes.Add('Name','Name',TClassField.cfProperty);
   //create index by private field "Id"
   users.Indexes.Add('Id','fId',TClassField.cfField);
   //get user by "Name" index
   writeln(users.Get('Name','Peter').SurName);
end;
```

**TFlexArray:** Array with methods like TList than can storage different value types into same array.
```delphi
var
  flexarray : TFlexArray;
begin
    flexarray.Add(10);
    flexarray.Add('Hello');
    user := TUser.Create;
    try
      user.Name := 'Joe';
      flexarray.Add(user);

      cout('Integer Item = %d',[flexarray[0].AsInteger],etInfo);
      cout('String Item = %s',[flexarray[1].AsString],etInfo);
      cout('Record Item = %s',[TUser(flexarray[2]).Name],etInfo);
    finally
      user.Free;
    end;
end;
```
**TFlexPairArray:** Array with methods like TList than can store different value types into same array, and search by item name.
```delphi
var
  flexarray : TFlexPairArray;
begin
    flexarray.Add('onenumber',10);
    flexarray.Add('other','Hello boy!');
    user := TUser.Create;
    try
      user.Name := 'Joe';
      flexarray.Add('myuser',user);

      cout('Integer Item = %d',[flexarray.GetValue('onenumber').AsInteger],etInfo);
      cout('String Item = %s',[flexarray.GetValue('other').AsString],etInfo);
      cout('Record Item = %s',[TUser(flexarray.GetValue('myuser')).Name],etInfo);
    finally
      user.Free;
    end;
end;
```

**Quick.YAML:**
--
Yaml object structure.

**TYamlObject:** A Yaml object is and array of YamlValue pairs.
```delphi
  //create Yaml object from yaml text
  yamlobj.ParseYamlValue(aYaml)
  //add a pair
  yamlobj.AddPair('Name','Mike');
  //display as yaml structure
  Writeln(yamlobj.ToYaml);
```
**TYamlArray:** Array of objects or scalars.
```delphi
  yamlarray.AddElement(TYamlPair.Create('Age',30));
  yamlobj.AddPair('myarray',yamlarray);
```
**TYamlPair:** Name-Value pair. Value can be object, array or scalar.
```delphi
  n := yamlobj.GetPair('Name').Value as TYamlInteger;
```

**Quick.YAML.Serializer:**
--
Serialize/Deserialize object from/to Yaml.
```delphi
  //Serialize
  text := YamlSerializer.ObjectToYaml(obj);
  //Deserialize
  YamlSerializer.YamlToObject(obj,yamltext);
```

**Quick.Expression:**
--
 Evaluate object properties or single values using expressions.
```delphi
  if TExpressionParser.Validate(user,('(Age > 30) AND (Dept.Name = "Financial")') then
  begin
    //do something
  end;

  if TExpressionParser.Validate(user,('(20 > 30) OR (5 > 3)') then
  begin
    //do something
  end;
```

**Quick.Linq:**
--
Makes Linq queries to any TObjectList<T>, TList<T>, TArray<T> and TXArray<T>, performing Select by complex Where like SQL syntax, update and order over your list. Where clauses uses namespaces to determine nested properties. LinQ can search for a element into a property array. 
Now includes and TArray<string> helper to add, remove and search with regular expressions into array.
- **From:** Array, XArray or TObjectList to use.
- **Where:** Expression to search. You can use a dots to define property path.
- **SelectAll:** Returns an array of objects matching where clause
- **SelectTop:** Returns top x objects matching where clause.
- **SelectFirst:** Returns first object matching where clause.
- **SelectLast:** Returns last object matching where clause.
- **OrderBy:** Define order of returned list.
- **Update:** Update fields of matching where clause.
- **Delete:** Delete objectes matching where clause.
- **Count:** Return number of elements matching where clause.
```delphi
  //Select multi conditional
  for user in TLinq<TUser>.From(userslist).Where('(Name = ?) OR (SurName = ?) OR (SurName = ?)',['Peter','Smith','Huan']).Select do
  begin
    //do something
  end;
  
  //Select like and update field
  TLinq<TUser>.From(userlist).Where('SurName Like ?',['%son']).SelectFirst.Name := 'Robert';
  
  //Select top and Order by field
  for user in TLinq<TUser>.From(userlist).Where('Age > ?',[18]).SelectTop(10).OrderBy('Name') do
  begin
    //do something
  end;
  
  //update fields by conditional
  TLinq<TUser>.From(userlist).Where('Name = ?',['Peter']).Update(['Name'],['Joe']);
  
  //count results
  numusers := TLinq<TUser>.From(userlist).Where('(Age > ?) AND (Age < ?)',[30,40]).Count;
```

**Quick.HTTPServer:**
--
TCustomHttpServer is a simple interfaced HttpServer with own HttpRequest and HttpResponse implementations to allow easy httpserver engine changes. 
You can enable custom error pages to return customized pages and dynamic error pages.
THttpServer is the IndyHttpServer implementation, but you can define your own.
```delphi
TMyHttpServer = class(THttpServer)
  public
    procedure ProcessRequest(aRequest: IHttpRequest; aResponse: IHttpResponse); override;
  end;

  procedure TMyHttpServer.ProcessRequest(aRequest: IHttpRequest; aResponse: IHttpResponse);
  begin
    aResponse.ContentText := 'Hello world!';
  end;
```

**Quick.MemoryCache:**
--
Caches objects or strings with an expiration time, to avoid generate this info everytime is needed (database queries, hard to calculate info, etc). TMemoryCache allows to cache objects and strings. Generic version TMemoryCache<T> allows to cache a defined type only.
- Create: Could be defined Purge interval and Serialization and Compression engines. By default serializes as Json and compress with gzip.
```delphi
 //create MemoryCache with 10 seconds purge interval
 cache := TMemoryCache.Create(10);

 //create MemoryCache for a type
 cache := TMemoryCache<TMyObj>.Create;
```
- Compression: Enables/Disables cache data compression.
- CachedObjects: Returns number of objects currently in cache.
- CacheSize: Returns size in bytes of all objects currently in cache. Real memory used depends of memory managers or architecture. This value is the real size of data bytes.
- PurgeInterval: Interval purge job tries to find expired objects to remove from cache (Default value 20 seconds).
- OnCacheFlushed: When cache is flushed.
- OnBeginPurgerJob: When PurgerJob starts.
- OnEndPurgerJob: When PurgerJob ends.
- Flush: Removes all cache objects.
- SetValue: Adds an object to cache. You can indicate expiration date or number of milliseconds to expire. If not defined cache will be infinity. MemoryCache can store objects or strings.
```delphi
//set string to cache without expiration
cache.SetValue('mystring','hello world');

//set string to cache with expiration to 10 seconds
cache.SetValue('mystring','this cache will expire in 10 seconds';

//set object to cache
cache.SetValue('Obj1',valueobj);
```
- TryGetValue: Tries to get and object from cache. Returns false if object doesn't exists or it's expired.
```delphi
//get string query result
cache.GetValue('Query12');

//get integer
cache.TryGetValue<Integer>('number',valueint);

//get object
cache.TryGetValue('Obj1',valueobj);
```

- RemoveValue: Removes an object from cache.

- **Cache Engine providers:**

- TCacheSerializerJSON: Uses JSON to serialize cache data.  
- TCacheCompressorGzip: Uses Gzip to compress cache data.
- TCacheCompressorLZO: Uses LZO to compress cache data.
   
```delphi 
 //create MemoryCache with 20 seconds purge interval and compression with LZO engine
 cache := TMemoryCache.Create(10,nil,TCacheCompressorLZO.Create);
 ```

 **Quick.IOC:**
 --
  Inversion of Control manager allows autocreate interfaced o instanced object or autoinject them in constructor classes, to avoid dependency.

 Create a container to manage dependency injection. 
 ```delphi
iocContainer := TIocContainer.Create;
```
**Register Types:**

You need to register types before you can inject them. A Type can be registered as Singleton, Transient.
**Singleton**: Life cycle will be one single instance for all injections, similar to a Global variable.
**Transient**: Life cycle will be one instance per each injection.
Register an interface type into container as transient:
```delphi
iocContainer.RegisterType<IMultService,TMultService>.AsTransient;
```
Register an interface type as singleton, delegating construction:
```delphi
iocContainer.RegisterType<ISumService,TSumService>.AsSingleTon.DelegateTo(
  function : TSumService
  begin
    Result := TSumService.Create;
  end
);
```
**Register Instances:**

Register a named instance object as transient, delegating construction:
```delphi
iocContainer.RegisterInstance<TDivideService>('one').AsTransient.DelegateTo(
  function : TDivideService
  begin
    Result := TDivideService.Create(True);
  end
);
```
**Register Options:**

Register IOptions (only singleton):
```delphi
 iocContainer.RegisterOptions<TMyOptions>(MyOptions);
```
**Resolve Types:**

AbtractFactory:
Creates a class trying to resolve all creation method parameter with dependency injection.
```delphi
MyClass := iocContainer.AbstractFactory<TMyBaseClass>(TMyClass);
```
Resolve an interface dependency:
```delphi
multservice := iocContainer.Resolve<IMultService>;
result := multservice.Mult(2,4);
```
**Resolve Instances:**

Resolve a named instance dependency:
```delphi
divideservice := iocContainer.Resolve<TDivideService>('other');
result := divideservice.Divide(100,2);
```
Interface instances will be freed automatically, but instance dependencies only will be freed if defined as singleton, transient instances will be destroyed by code.

**Quick.Options:**
 --
You define sections as classes and saves as single file settings. Works similar to dotnet Options. Options file can be in JSON or YAML format.

Define your option class inherited from TOptions and all published properties will be load/save.
Create options container, with JsonSerializer and reloading on change:
```delphi
Options := TOptionsContainer.Create('.\options.conf',TJsonOptionsSerializer.Create,True);
```
Add a section to your container options:
```delphi
Options.AddSection<TLoggingOptions>('Logging')
```
**Configure Options:**

You can define section name to save into file and delegate configuration default settings and validating values:
```delphi
Options.AddSection<TLoggingOptions>('Logging').ConfigureOptions(
  procedure(aOptions : TLoggingOptions)
  begin
    aOptions.Path := 'C:\';
  end
).ValidateOptions;
```
**Validate Options:**

Validate options allows verify if option settings are setted between defined ranges. This validation needs previously assigned custom attributes to properties in your TOptions class.
- **StringLength(max,messagestr):** Allows define max length in string properties, returning messagestr if length greater than max.
- **Range(min,max,messagestr):** Allows define a range of min and max values permitted, returning messagestr if value length outbounds margins.
```delphi
TLoggingOptions = class(TOptions)
  private
    fPath : string;
  published
    [Required, StringLength(255,'Path too long')]
    property Path : string read fPath write fPath;
    [Range(0.0,5.2)]
    property Level : Double read fLevel write fLevel;
  end;
```
**Use Options:**
To retrieve option section:
```delphi
LoggingOptions := Options.GetSection<TLoggingOptions>;
LoggginOptions.Path := 'C:\Path';
```
**Use IOptions:**
IOptions is a dependency injectable interface to TOptions. You can register with IocContainer.RegisterOptions<TOptions> to make injectable into constructor methods.
```delphi
UIOptions := Options.GetSectionInterface<TUIOptions>.Value;
UIOptions.WindowColor := clBlue;
```
**Load/Save Options:**

Load options from file settings:
```delphi
options.Load;
```
Save options to file settings:
```delphi
options.Save;
```
If you defined container creation with ReloadOnChanged parameter to true, every time file settings is changed, configuration will be reloaded. If you need to control when to reload, you can listen to the event:
```
Options.OnFileModified := procedure
  begin
    cout('Detected config file modification!',etWarning);
  end;
```

**Quick.Pooling:**
 --
 Define pool of connection, threads or any object you want to control to avoid resource consum like database connections, http clients, etc...

Create http client pool:
```delphi
 pool := TObjectPool<THTTPClient>.Create(5,5000,procedure(var aInstance : THTTPClient)
        begin
          aInstance := THTTPClient.Create;
          aInstante.UserAgent := 'MyAgent';
        end);
```
Get object from pool:

```delphi
httpcli := pool.Get.Item;
statuscode := httpcli.Get('https://www.mydomain.com').StatusCode;
```

**Quick.Collections:**
 --
Defines interfaced List and Objectlist with linQ support inherited.

- TXList<T> / IList<T>: Interfaced List allowing LinQ regEx search/remove/update.

```delphi
myarray := ['Joe','Mat','Lee'];
//search for regex match
cout('Search for regex match',ccYellow);
for name in myarray.Where('e$',True).Select do
begin
  cout('User %s ends with "e"',[name],etInfo);
end;
```

- TXObjectList<T> / IObjectList<T>: Interfaced ObjectList allowing LinQ predicate or expression search/remove/update.
Expression search:
```delphi
user := ListObj.Where('Profile.Name = ?',['Lee']).SelectFirst;
```
Expression search for item array:
```delphi
users := ListObj.Where('Roles CONTAINS ?',['SuperAdmin']).Select;
```
Predicate search:

```delphi
user := ListObj.Where(function(aUser : TUser) : Boolean
      begin
        Result := aUser.Name.StartsWith('J');
      end).SelectFirst;
    if user <> nil then cout('%s starts with J letter',[user.Name],etInfo);
```
See Quick.Linq section to view more functions allowed.

**Quick.Template:**
 --
String template replacing using a dictionary or delegate function. You can specify quoted token chars. 

Replace passing a dictionary:
```delphi
dict := TDictionary<string,string>.Create;
dict.Add('User','John');
dict.Add('Age','20');
dict.Add('SurName','Peterson');
mytemplate := 'User {{User}} {{SurName}} are {{Age}} years old.';
template := TStringTemplate.Create('{{','}}',dict);
Result := template.Replace(mytemplate);
```
Replace with delegate function:
```delphi
mytemplate := 'User {{User}} {{SurName}} are {{Age}} years old.';
template := TStringTemplate.Create('{{','}}',function(const aToken : string) : string
  begin
    if token = 'User' then Result := 'John'
    else if token = 'Surname' then Result := 'Peterson'
    else if token = 'Age' then Result := '20';
  end);
Result := template.Replace(mytemplate);
```

**Quick.Debug.Utils:**
 --
Debug utils to check performance and get enter and exit method checkpoint.Define with a Debug a compiler directive to only be active when your app is compiled in debug mode.
On console apps uses console out by default. You can pass a logger to output in:
```delphi
TDebugUtils.SetLogger(ilogger);
```
Trace a part of your code:
```delphi
function TCalculator.Subs(a, b: Int64): Int64;
begin
  {$IFDEF DEBUG}
  TDebugger.Trace(Self,Format('Substract %d - %d',[a,b]));
  {$ENDIF}
  Result := a - b;
  //simulate working for 200ms
  Sleep(200);
end;
//Returns:
//29-06-2020 23:31:41.391  [TRACE] TCalculator -> Substract 30 - 12
```
Calculate time to process from point to exit function:
```delphi
function TCalculator.Sum(a, b: Int64): Int64;
begin
  {$IFDEF DEBUG}
  TDebugger.TimeIt(Self,'Sum',Format('Sum %d + %d',[a,b]));
  {$ENDIF}
  Result := a + b;
  //simulate working for 1 seconds
  Sleep(1000);
end;
//Returns:
//29-06-2020 22:58:45.808  [CHRONO] TCalculator.Sum -> Sum 100 + 50 = 1,00s
```
Calculate time to process from point to point and exit function:
```delphi
function TCalculator.Divide(a, b: Int64): Double;
begin
  {$IFDEF DEBUG}
  var crono := TDebugger.TimeIt(Self,'Divide',Format('Divide %d / %d',[a,b]));
  {$ENDIF}
  Result := a / b;
  //simulate working for 500ms
  Sleep(500);
  {$IFDEF DEBUG}
  crono.BreakPoint('Only divide');
  {$ENDIF}
  //simulate working for 1 second
  Sleep(1000);
  {$IFDEF DEBUG}
  crono.BreakPoint('Only Sleep');
  {$ENDIF}
end;
//Returns:
//29-06-2020 23:25:46.223  [CHRONO] TCalculator.Divide -> First point = 500,18ms
//29-06-2020 23:25:47.224  [CHRONO] TCalculator.Divide -> Second point = 1,00s
//29-06-2020 23:25:47.225  [CHRONO] TCalculator.Divide -> Divide 10 / 2 = 1,50s
```
Get notification when enter and exit function, and times it:
```delphi
function TCalculator.Mult(a, b: Int64): Int64;
begin
  {$IFDEF DEBUG}
  TDebugger.Enter(Self,'Mult').TimeIt;
  {$ENDIF}
  Result := a * b;
  //simulate working for 2 seconds
  Sleep(2000);
end;
//Returns:
//29-06-2020 22:58:45.808  [ENTER] >> TCalculator.Mult
//29-06-2020 22:58:47.810  [EXIT] >> TCalculator.Mult in 2,00s
```

**Quick.Parameters:**
--
Working with commandline parameters will be easy using commandline extension.
Define a class inherited from TParameters or TServiceParameters (if working with QuickAppServices) with your possible arguments as published properties:
```delphi
uses
  Quick.Parameters;
type
  TCommand = (Copy, Move, Remove);
  TMyMode = (mdAdd, mdSelect, mdRemove);

  [CommandDescription('Simple console application example with Quick.Parameters')]
  TMyParameter = class(TParameters)
  private
    fCommand : TCommand;
    fHost : string;
    fPort : Integer;
    fRetries : Integer;
    fUseTCP : Boolean;
    fConfigFile: string;
    fSilent: Boolean;
    fMyMode: TMyMode;
    fLogErrorsConsole: Boolean;
    fLogErrors: Boolean;
    fShowReport: Boolean;
  published
    [ParamCommand(1)]
    [ParamRequired]
    [ParamHelp('Command action.','command-action')]
    property Command : TCommand read fCommand write fCommand;

    [ParamName('HostName'),ParamRequired]
    [ParamHelp('Define host to connect.','host')]
    property Host : string read fHost write fHost;

    [ParamName('Port','p')]
    [ParamValueIsNextParam]
    [ParamHelp('Define Port to connect (default 80)','port')]
    property Port : Integer read fPort write fPort;

    [ParamHelp('Number of max retries.')]
    property Retries : Integer read fRetries write fRetries;

    [ParamHelp('Path to config.','path')]
    [ParamName('Config-file')]
    property ConfigFile : String read fConfigFile write fConfigFile;

    [ParamHelp('Silent mode.')]
    property Silent : Boolean read fSilent write fSilent;

    [ParamHelp('Modes (mdAdd, mdSelect, mdRemove)')]
    property Mode : TMyMode read fMyMode write fMyMode;
  end;

```
Use param:
```delphi
params := TMyParameter.Create;
```
When you call your exe with --help you get documentation. If you need to check for a switch or value, you can do like this:
```delphi
if params.Port = 0 then ...
if params.Silent then ...
```
QuickParameters uses custom attributes to define special parameter conditions:

- **CommandDescription:** Defines text to describe your application in help documentation.

- **ParamCommand(number):** Defines static position into commandline for single parameters.

- **ParamName(name,alias):** Define a diferent name for parameter. Allows to use special characters not allowed for class properties (like file-name or config.file). Optional Alias argument defines an alternative (normally short name) parameter name.

- **ParamHelp(helptext,valuename):** Defines a commandline help text and value name in usage section.

- **ParamSwitchChar(sign):** Defines string or char to indicate switch or parameter. If not defined, a double dash (--) will be used by default.

- **ParamValueSeparator(sign):** Defines string or char to separate parameter name from value (filename=config.json). If not defined, equal sign (=) will be used by default.

- **ParamValueIsNextParam:** Defines a parameter with a value without value separator (filename c:\config.ini)

- **ParamRequired:** Defines a parameter as required. If param not found, an exception will be raised.

QuickParameter automatically checks for value types. If you define a parameter value as Integer, and pass an alfanumeric, an exception will be raised.

Help customization:
You can define your own color customization with ColorizeHelp. Enabled property will use custom colors, otherwise b/w will be used.
```delphi
Parameters.ColorizeHelp.Enabled := True;
Parameters.ColorizeHelp.CommandName := ccCyan;
Parameters.ColorizeHelp.CommandUsage := ccBlue;
```
When parameters detects help parameter, help documentation will be showed.

Parameters.ShowHelp: Shows help documentation, generated automatically:
```
Parameters v.1.0
Usage: Parameters <command-action> <--HostName=<host>> [--Port <port>] [--Retries=<value>]
                  [--Config-file=<path>] [--UseTCP] [--Silent] [--Mode=<value>]
                  [--ShowReport] [--Help]

Simple console application example with Quick.Parameters

Arguments:

    Command                  Command action.
  --HostName                 Define host to connect.
  --Port, -p                 Define Port to connect (default 80)
  --Retries                  Number of max retries.
  --Config-file              Path to config.
  --UseTCP                   Use TCP connection if present.
  --Silent                   Silent mode.
  --Mode                     Modes (mdAdd, mdSelect, mdRemove)
  --Help, -h                 Show this documentation
```

**Quick.Url.Utils:**
--
- **GetProtocol:** Get protocol from an url.
- **GetHost:** Get hostname from an url.
- **GetPath:** Get path from an url.
- **GetQuery:** Get Query part from an url.
- **RemoveProtocol:** Remove protocol from an url.
- **RemoveQuery:** Remove query part from an url.
- **EncodeUrl:** Encode path and query from and url.

**Quick.RegEx.Utils:**
--
Commonly used validations.
```delphi
```

**Quick.Conditions:**
--
Pre and postcondition validations in fluent style.
Condition.Requires evaluates a variable for conditions before do some operations.
Condition.Ensures evaluates a variable result for conditions after do some operations.
```delphi
    Condition.Requires(num, "num")
        .IsInRange(1,10,'value for num is out of range');   // throws custom error if not in range
        .IsNotGreater(50);   // throws ArgumentException if not equal to 128

    Condition.Requires(myobj, "myobj")
        .WithExceptionOnFailure(EMyException) //throws specific exception on failure
        .IsNotNull()          // throws ArgumentNullException if null
        .Evaluate(myobj.id > 10); // myobj.id must be greater than 10

    Condition.Requires(text, "text")
        .IsNotEmpty()          // throws ArgumentNullException if empty
        .StartsWith("<html>") // throws ArgumentException if not starts with <html>
        .EndsWith("</html>") // throws ArgumentException if not ends with </html>
        .IsNotLowerCase // thows ArgumentException if not lowercase
        .Evaluate(text.Contains("sometxt") or test.Contains('othertxt')); // throws ArgumentException if not evaluates
```

>Do you want to learn delphi or improve your skills? [learndelphi.org](https://learndelphi.org)
