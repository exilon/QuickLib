program MemCacheTest;

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
  cache : TMemoryCache<TMyObject>;
  valueobj : TMyObject;
  backgroundtasks : TBackgroundTasks;
  i : Integer;
  n : Integer;
  chrono : TChronometer;
  dummystr : string;

begin

  ReportMemoryLeaksOnShutdown := True;

  backgroundtasks := TBackgroundTasks.Create(20,100);

  cache := TMemoryCache<TMyObject>.Create(10);
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

  //set object to cache
  valueobj := TMyObject.Create;
  try
    valueobj.TestStr := 'TestOk';
    valueobj.TestInt := 7;
    cache.SetValue('obj1',valueobj);
  finally
    valueobj.Free;
  end;

  //get object from cache
  //valueobj := TMyObject.Create;
  try
    //cache.TryGetValue('Obj1',valueobj,TMyObject);
    cache.TryGetValue('obj1',valueobj);
    coutFmt('Got Id(obj1) from cache: (TestStr = %s / TestIn = %d)',[valueobj.TestStr,valueobj.TestInt],etSuccess);
  finally
    valueobj.Free;
  end;

  chrono := TChronometer.Create(False);
  //concurrent read/writes
  dummystr := '';

  cout('Preparing data..',ccWhite);

  //for i := 0 to 1024 do dummystr := dummystr + Chr(Random(255));
  dummystr := RandomString(1024);
  n := 0;
  for i := 1 to 100000 do
  begin
    Inc(n);
    if n < 9 then
    begin
      backgroundtasks.AddTask(procedure(task : ITask)
                           var obj : TMyObject;
                          begin
                            if not cache.TryGetValue('obj1',obj) then
                            begin
                              coutFmt('Value %d not in cache or expired',[1],etWarning);
                              obj := TMyObject.Create;
                              try
                                obj.TestStr := 'TestOk';
                                obj.TestInt := 7;
                                cache.SetValue('obj1',obj,1000);
                              finally
                                obj.Free;
                              end;
                            end;
                            //coutFmt('Get Id(1) from cache = %s',[obj.TestStr],etSuccess);
                          end
                  ).OnException(procedure(task : ITask; aException : Exception)
                          begin
                            cout(aException.Message,etError);
                          end
                  ).Run;
    end
    else
    begin
      n := 0;
      backgroundtasks.AddTask(procedure(task : ITask)
                           var
                            obj : TMyObject;
                          begin
                            obj := TMyObject.Create;
                            try
                              obj.TestStr := 'TestOk';
                              obj.TestInt := 7;
                              cache.SetValue(Random(1000000).ToString,obj,Random(5000));
                            finally
                              obj.Free;
                            end;
                            //coutFmt('Set Id(1) to cache = %s',[obj.TestStr],etSuccess);
                          end
                  ).OnException(procedure(task : ITask; aException : Exception)
                          begin
                            cout(aException.Message,etError);
                          end
                  ).Run;
    end;
  end;
  backgroundtasks.AddTask(procedure(task : ITask)
                          begin
                            chrono.Stop;
                            coutFmt('Performance: %s Combined cache IO Read/Writes in %s',[NumberToStr(i),chrono.ElapsedTime],etWarning);
                            coutFmt('Cached Objects: %s / Cache Size: %s',[NumberToStr(cache.CachedObjects),FormatBytes(cache.CacheSize)],etInfo);
                            cout('Wait to see how cache is expiring every 10 seconds or press <ENTER> to Exit',ccYellow);
                            //cache.Flush;
                          end
                  ).Run;
  cout('Stress caching test launched...',ccWhite);
  backgroundtasks.Start;
  chrono.Start;

  ConsoleWaitForEnterKey;
  coutFmt('Cached Objects: %s / Cache Size: %s',[NumberToStr(cache.CachedObjects),FormatBytes(cache.CacheSize)],etInfo);
  cache.Free;
  chrono.Free;
  backgroundtasks.Free;
end.
