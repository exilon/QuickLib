program DebugUtils;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Debug.Utils;

type
  TCalculator = class
  public
    function Sum(a, b : Int64) : Int64;
    function Subs(a, b: Int64): Int64;
    function Mult(a, b : Int64) : Int64;
    function Divide(a, b: Int64): Double;
  end;

var
  num : Int64;
  float : Double;
  calculator : TCalculator;

{ TCalculator }

function TCalculator.Sum(a, b: Int64): Int64;
begin
  {$IFDEF DEBUG}
  TDebugger.TimeIt(Self,'Sum',Format('Sum %d + %d',[a,b]));
  {$ENDIF}
  Result := a + b;
  //simulate working for 150ms
  Sleep(150);
end;

function TCalculator.Subs(a, b: Int64): Int64;
begin
  {$IFDEF DEBUG}
  TDebugger.Trace(Self,Format('Substract %d - %d',[a,b]));
  {$ENDIF}
  Result := a - b;
  //simulate working for 200ms
  Sleep(200);
end;

function TCalculator.Mult(a, b: Int64): Int64;
begin
  {$IFDEF DEBUG}
  TDebugger.Enter(Self,'Mult').TimeIt;
  {$ENDIF}
  Result := a * b;
  //simulate working for 300ms
  Sleep(300);
end;

function TCalculator.Divide(a, b: Int64): Double;
begin
  {$IFDEF DEBUG}
  var crono := TDebugger.TimeIt(Self,'Divide',Format('Divide %d / %d',[a,b]));
  {$ENDIF}
  Result := a / b;
  //simulate working for 500ms
  Sleep(500);
  {$IFDEF DEBUG}
  crono.BreakPoint('First point');
  {$ENDIF}
  //simulate working for 1 second
  Sleep(1000);
  {$IFDEF DEBUG}
  crono.BreakPoint('Second point');
  {$ENDIF}
end;

begin
  try
    calculator := TCalculator.Create;

    num := calculator.Sum(100,50);
    cout('Total is %d',[num],etInfo);

    num := calculator.Subs(30,12);
    cout('Total is %d',[num],etInfo);

    num := calculator.Mult(20,2);
    cout('Total is %d',[num],etInfo);

    float := calculator.Divide(10,2);
    cout('Total is %f',[float],etInfo);

    cout('Press <ENTER> to Exit',ccYellow);
    ConsoleWaitForEnterKey;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
