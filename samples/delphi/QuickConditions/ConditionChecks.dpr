program ConditionChecks;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  System.SysUtils,
  Quick.Console,
  Quick.Conditions;

type
  EMyException = class(Exception);

var
  result : string;
  num : Int64;
  fnum : Double;
  obj : TStream;

begin
  try
    //text must start with h and end with o
    result := 'Hello';
    Condition.Requires(result,'result')
      .IsNotEmpty
      .StartsWith('h',True)
      .EndsWith('o');

    //text longer than 10 and contains check
    result := 'Text to check';
    Condition.Requires(result,'result')
      .IsNotEmpty
      .IsLongerThan(10)
      .Contains('check',True);

    //text must be shorter than 10
    result := 'Text';
    Condition.Requires(result,'result')
      .IsNotEmpty
      .IsShorterThan(10);

    //text must be not lowercase
    result := 'Text';
    Condition.Requires(result,'result')
      .IsNotEmpty
      .IsNotLowerCase;

    //num min 1 and max 10
    num := 10;
    Condition.Requires(num,'num')
      .IsInRange(1,10,'value for num is out of range');

    fnum := 7.3;
    Condition.Requires(fnum,'fnum')
      .IsGreaterThan(5)
      .IsLessOrEqual(8)
      .IsNotInRange(6,7);

    obj := TStringStream.Create;
    Condition.Requires(obj,'obj')
      .WithExceptionOnFailure(EMyException)
      .IsNotNull
      .IsOfType(TStream)
      .Evaluate(obj.Size = 0);
    obj.Free;

    Condition.Ensures(obj,'obj')
      .IsNotNull;


    cout('All conditions passed!',ccGreen);

    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
