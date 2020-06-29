program Template1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  Quick.Commons,
  Quick.Console,
  Quick.Chrono,
  Quick.Template;

var
  crono : TChronometer;
  dict : TDictionary<string,string>;
  template : TStringTemplate;
  mytemplate : string;
  str : string;
  str2 : string;
  //i : Integer;

begin
  try
    mytemplate := 'User {{User}} {{SurName}} are {{Age}} years old.' + sLineBreak +
                  '{{SurName}} is {{Height}} tall and has {{Money}} euro(s) bank account and access level {{Level}}.' + sLineBreak +
                  '{{User}} is from {{State}} ({{Country}}).';
//    for i := 1 to 15 do
//    begin
//      mytemplate := mytemplate + mytemplate + sLineBreak;
//    end;

    cout('String Lenght = %s',[NumberToStr(mytemplate.Length)],etInfo);

    crono := TChronometer.Create(False);
    dict := TDictionary<string,string>.Create;
    dict.Add('User','John');
    dict.Add('Age','20');
    dict.Add('SurName','Peterson');
    dict.Add('Money','100.000');
    dict.Add('Country','Spain');
    dict.Add('State','Barcelona');
    dict.Add('Level','1');
    dict.Add('Height','175');

    //Test StrinTemplate
    template := TStringTemplate.Create('{{','}}',dict);

    cout('Test with StringTemplate',ccGreen);
    crono.Start;
    str := template.Replace(mytemplate);
    crono.Stop;
    cout('Time: %s',[crono.ElapsedTime(False)],etInfo);
    //cout(str,ccLightGreen);

    //Test standard StringReplace
    cout('Test with standard StringReplace',ccGreen);
    crono.Start;
    str2 := StringReplace(mytemplate,'{{User}}',dict['User'],[rfReplaceAll]);
    str2 := StringReplace(str2,'{{SurName}}',dict['SurName'],[rfReplaceAll]);
    str2 := StringReplace(str2,'{{Age}}',dict['Age'],[rfReplaceAll]);
    str2 := StringReplace(str2,'{{Money}}',dict['Money'],[rfReplaceAll]);
    str2 := StringReplace(str2,'{{Country}}',dict['Country'],[rfReplaceAll]);
    str2 := StringReplace(str2,'{{State}}',dict['State'],[rfReplaceAll]);
    str2 := StringReplace(str2,'{{Level}}',dict['Level'],[rfReplaceAll]);
    str2 := StringReplace(str2,'{{Height}}',dict['Height'],[rfReplaceAll]);
    crono.Stop;
    cout('Time: %s',[crono.ElapsedTime(False)],etInfo);
    cout(str,ccLightGreen);

    cout('Press <ENTER> to Exit',ccYellow);
    ConsoleWaitForEnterKey;

    dict.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
