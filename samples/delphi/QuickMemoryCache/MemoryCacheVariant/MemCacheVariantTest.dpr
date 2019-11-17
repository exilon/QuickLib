program MemCacheVariantTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Threads,
  Quick.Chrono,
  Quick.Format,
  Quick.MemoryCache;

type
  TMyObject = class
  private
    fTestStr : string;
    fTestInt : Integer;
  public
    property TestStr : string read fTestStr write fTestStr;
    property TestInt : Integer read fTestInt write fTestInt;
  end;

var
  cache : IMemoryCache;
  valuestr : string;
  valueobj : TMyObject;
  valueobj2 : TMyObject;
  backgroundtasks : TBackgroundTasks;
  i : Integer;
  n : Integer;
  chrono : TChronometer;
  dummystr : string;
  arr : TArray<string>;
  arrobj : TArray<TMyObject>;

begin

  ReportMemoryLeaksOnShutdown := True;

  backgroundtasks := TBackgroundTasks.Create(20,100);

  cache := TMemoryCache.Create(10);
  //cache := TMemoryCache.Create(10,nil,TCacheCompressorLZO.Create);
  cache.Compression := True;
  cache.OnEndPurgerJob := procedure(aRemovedEntries : Integer)
                          begin
                            cout(Format('Purger job finished (Removed: %s /CachedObjects: %s / CacheSize: %s)',[NumberToStr(aRemovedEntries),NumberToStr(cache.CachedObjects),FormatBytes(cache.CacheSize)]),ccMagenta);
                          end;

  cache.OnPurgeJobError := procedure(const aErrorMsg : string)
                           begin
                             coutFmt('Error flushing cache expireds (%s)',[aErrorMsg],etError);
                           end;
  //set string to cache
  cache.SetValue('one','one string');

  //set string to cache with expiration to 10 seconds
  cache.SetValue('other','another string',10);

  //set array of string
  cache.SetValue('myarray',['one','two','three','four']);

  //set object to cache
  valueobj := TMyObject.Create;
  try
    valueobj.TestStr := 'TestOk';
    valueobj.TestInt := 7;
    cache.SetValue('Obj1',valueobj);
  finally
    valueobj.Free;
  end;

  //set array of myobject
  valueobj := TMyObject.Create;
  try
    valueobj.TestStr := 'TestOk One';
    valueobj.TestInt := 7;
    arrobj := arrobj + [valueobj];
    cache.SetValue('arrayobj',TArray<TObject>(arrobj));
  finally
    valueobj.Free;
  end;
  arrobj := nil;

  //get string from cache
  //cache.TryGetValue<string>('one',valuestr);
  valuestr := cache.GetValue('one');
  coutFmt('Got Id(one) from cache: %s',[valuestr],etSuccess);

  //get other string from cache
  //cache.TryGetValue<string>('other',valueint);
  cache.TryGetValue('other',valuestr);
  coutFmt('Got Id(other) from cache: %s',[valuestr],etSuccess);

  //get object from cache
  valueobj2 := TMyObject.Create;
  try
    cache.TryGetValue('Obj1',valueobj2);
    coutFmt('Got Id(Obj1) from cache: (TestStr = %s / TestIn = %d)',[valueobj2.TestStr,valueobj2.TestInt],etSuccess);
  finally
    valueobj2.Free;
  end;

  //get array of string from cache
  cache.TryGetValue('myarray',arr);
  coutFmt('array of string[0] = %s',[arr[0]],etSuccess);

  //get array of myobject from cache
  cache.TryGetValue('arrayobj',TArray<TObject>(arrobj));
  coutFmt('array of MyObject[0].TestStr = %s',[arrobj[0].TestStr],etSuccess);

  chrono := TChronometer.Create(False);
  //concurrent read/writes
  dummystr := '';

  cout('Preparing data..',ccWhite);

  dummystr := RandomString(1024);
  n := 0;
  for i := 1 to 100000 do
  begin
    Inc(n);
    if n < 9 then
    begin
      backgroundtasks.AddTask(procedure(task : ITask)
                           var a : string;
                          begin
                            if not cache.TryGetValue('1',a) then
                            begin
                              coutFmt('Value %d not in cache or expired',[1],etWarning);
                              cache.SetValue('1',a,1000);
                            end;
                            //coutFmt('Get Id(1) from cache = %s',[a],etSuccess);
                          end
                  ).Run;
    end
    else
    begin
      n := 0;
      backgroundtasks.AddTask(procedure(task : ITask)
                           var a : string;
                          begin
                            a := Random(1000000).ToString;
                            cache.SetValue(a,dummystr + a,Random(5000));
                            //coutFmt('Set Id(1) to cache = %s',[a],etSuccess);
                          end
                  ).Run;
    end;
  end;
  backgroundtasks.AddTask(procedure(task : ITask)
                           var a : string;
                          begin
                            chrono.Stop;
                            coutFmt('Performance: %s Combined cache IO Read/Writes in %s',[NumberToStr(i),chrono.ElapsedTime],etWarning);
                            coutFmt('Cached Objects: %s / Cache Size: %s',[NumberToStr(cache.CachedObjects),FormatBytes(cache.CacheSize)],etInfo);
                            //cache.Flush;
                          end
                  ).Run;
  cout('Stress caching test launched',ccWhite);
  backgroundtasks.Start;
  chrono.Start;

  cout('Wait to see how cache is expiring every 10 seconds or press <ENTER> to Exit',ccYellow);
  ConsoleWaitForEnterKey;
  coutFmt('Cached Objects: %s / Cache Size: %s',[NumberToStr(cache.CachedObjects),FormatBytes(cache.CacheSize)],etInfo);
  //cache.Free;
  chrono.Free;
  backgroundtasks.Free;
end.
