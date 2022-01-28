program ExpressionsTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Expression;

var
  listexpressions : TArray<string>;
begin
  try
    //true expressions
    listexpressions.Add('1 = 1');
    listexpressions.Add('1 < 2');
    listexpressions.Add('2 > 1');
    listexpressions.Add('(1 = 1) AND (2 = 2)');
    listexpressions.Add('(1 = 1) OR (2 = 3)');
    listexpressions.Add('(1 = 2) OR (2 = 2)');
    listexpressions.Add('(0.3 < 8) or ((5 < 4) and (2 = 1))');
    listexpressions.Add('(8 > 3) or ((5 > 4) and (2 = 1))');

    cout('True expressions',ccWhite);
    for var expression in listexpressions do
    begin
      if TExpressionParser.Validate(expression) then cout(expression,ccGreen) else cout(expression,ccRed);
    end;

    listexpressions := [];

    //false expressions
    listexpressions.Add('1 = 2');
    listexpressions.Add('2 < 1');
    listexpressions.Add('1 > 2');
    listexpressions.Add('(1 <> 1) AND (2 <> 2)');
    listexpressions.Add('(1 <> 1) OR (2 = 3)');
    listexpressions.Add('(1 = 2) OR (2 <> 2)');
    listexpressions.Add('(0.3 > 8) or ((5 > 4) and (2 <> 2))');
    listexpressions.Add('(8 < 3) or ((5 < 4) and (2 <> 1))');

    cout('False expressions',ccWhite);
    for var expression in listexpressions do
    begin
      if not TExpressionParser.Validate(expression) then cout(expression,ccGreen) else cout(expression,ccRed);
    end;

    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
