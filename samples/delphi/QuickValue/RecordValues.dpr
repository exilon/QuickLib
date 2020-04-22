program RecordValues;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Value,
  Quick.Value.RTTI;

type
  TMyRecord = record
    Name : string;
    Age : Integer;
  end;

var
  rec : TMyRecord;
  rec2 : TMyRecord;
  fvalue : TFlexValue;

begin
  try
    rec.Name := 'John';
    rec.Age := 30;

    fvalue.FromRecord(rec);
    rec2 := fvalue.AsRecord<TMyRecord>;

    Writeln(rec2.Name + '=' + rec2.Age.ToString);
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
