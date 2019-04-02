program simplechrono;

uses
  SysUtils,
  DateUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Chrono;

var
  crono : TChronometer;
  starttime : TDateTime;
  ms : Int64;
begin
  try
    Console.LogVerbose := LOG_ALL;
    cout('Chrono Test',etInfo);
    crono := TChronometer.Create;
    crono.Start;
    starttime := Now();
    repeat
      ms := MillisecondsBetween(Now(),StartTime);
    until ms >= 4000;
    crono.Stop;
    cout('crono stopped!',etInfo);
    cout('Loop: %d Elapsed: %s',[ms,crono.ElapsedTime],etInfo);
    Readln;
  except
    on e : Exception do WriteLn(e.message);
  end;
end.

