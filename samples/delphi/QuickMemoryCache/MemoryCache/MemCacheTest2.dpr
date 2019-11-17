program MemCacheTest2;

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
  cache : TMemoryCache<string>;
  valuestr : string;
  valueint : Integer;
  valuefloat : Extended;
  valuebool : Boolean;
  valueobj : TMyObject;
  backgroundtasks : TBackgroundTasks;
  i : Integer;
  n : Integer;
  chrono : TChronometer;
  dummystr : string;
  arr : TArray<string>;

begin

  ReportMemoryLeaksOnShutdown := True;

  backgroundtasks := TBackgroundTasks.Create(20,100);

  cache := TMemoryCache<string>.Create(10);
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

  chrono := TChronometer.Create(False);
  //concurrent read/writes
  dummystr := '';

  cout('Preparing data..',ccWhite);

  //for i := 0 to 1024 do dummystr := dummystr + Chr(Random(255));
  cache.SetValue('obj1',dummystr,30000);
  dummystr := RandomString(1024);
  n := 0;
  for i := 1 to 100000 do
  begin
    Inc(n);
    if n < 9 then
    begin
      backgroundtasks.AddTask(procedure(task : ITask)
                          var
                            a : string;
                          begin
                            if not cache.TryGetValue('obj1',a) then
                            begin
                              coutFmt('Value %d not in cache or expired',[1],etWarning);
                              cache.SetValue('obj1',dummystr,1000);
                            end;
                            //coutFmt('Get Id(1) from cache = %s',[a],etSuccess);
                          end
                  ).Run;
    end
    else
    begin
      n := 0;
      backgroundtasks.AddTask(procedure(task : ITask)
                           var
                            a : string;
                          begin
                            a := Random(1000000).ToString;
                            cache.SetValue(a,dummystr,Random(5000));
                            //coutFmt('Set Id(1) to cache = %s',[a],etSuccess);
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
