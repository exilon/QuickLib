unit Quick.RegEx.Utils.Tests;

interface

uses
  TestFramework, System.SysUtils, Quick.RegEx.Utils, System.RegularExpressions;

type
  // Test methods for class TRegExUtils

  TestTRegExUtils = class(TTestCase)
  strict private
    FRegExUtils: TRegExUtils;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsNumber;
    procedure TestIsInteger;
    procedure TestIsFloat;
    procedure TestIsAlphanumeric;
    procedure TestIsValidEmail;
    procedure TestIsPasswordComplex;
    procedure TestIsValidUsername;
    procedure TestIsValidUrl;
    procedure TestIsValidIpv4;
    procedure TestIsValidIpv6;
    procedure TestIsValidDate_YYYYMMdd;
    procedure TestIsValidDate_ddMMYYY;
    procedure TestIsValidHtmlTag;
    procedure TestHasDuplicates;
    procedure TestIsValidPhoneNumber;
    procedure TestIsValidFilePath;
    procedure TestIsValidVisaCard;
    procedure TestIsValidMasterCard;
    procedure TestIsValidAmericanExpressCard;
    procedure TestIsValidPassport;
    procedure TestIsValidDNI_ES;
    procedure TestIsValidSSN_US;
  end;

implementation

procedure TestTRegExUtils.SetUp;
begin
  FRegExUtils := TRegExUtils.Create;
end;

procedure TestTRegExUtils.TearDown;
begin
  FRegExUtils.Free;
  FRegExUtils := nil;
end;

procedure TestTRegExUtils.TestIsNumber;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsNumber('100');
  CheckTrue(ReturnValue,'Value is Integer');

  ReturnValue := FRegExUtils.IsNumber('10.0');
  CheckTrue(ReturnValue,'Value is Float');

  //not valid cases
  ReturnValue := FRegExUtils.IsNumber('a100');
  CheckFalse(ReturnValue,'Value is alphanumeric');

  ReturnValue := FRegExUtils.IsNumber('abc');
  CheckFalse(ReturnValue,'Value is alpha');
end;

procedure TestTRegExUtils.TestIsInteger;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsInteger('100');
  CheckTrue(ReturnValue,'Value is Integer');

  ReturnValue := FRegExUtils.IsInteger('10.0');
  CheckFalse(ReturnValue,'Value is Float');

  //not valid cases
  ReturnValue := FRegExUtils.IsInteger('a100');
  CheckFalse(ReturnValue,'Value is alphanumeric');

  ReturnValue := FRegExUtils.IsInteger('abc');
  CheckFalse(ReturnValue,'Value is alpha');
end;

procedure TestTRegExUtils.TestIsFloat;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsFloat('100');
  CheckFalse(ReturnValue,'Value is Integer');

  ReturnValue := FRegExUtils.IsFloat('10.0');
  CheckTrue(ReturnValue,'Value is Float');

  //not valid cases
  ReturnValue := FRegExUtils.IsFloat('a100');
  CheckFalse(ReturnValue,'Value is alphanumeric');

  ReturnValue := FRegExUtils.IsFloat('abc');
  CheckFalse(ReturnValue,'Value is alpha');
end;

procedure TestTRegExUtils.TestIsAlphanumeric;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsAlphanumeric('a100');
  CheckTrue(ReturnValue,'Value is alphanumeric');

  ReturnValue := FRegExUtils.IsAlphanumeric('abc');
  CheckTrue(ReturnValue,'Value is alpha');

  ReturnValue := FRegExUtils.IsAlphanumeric('100');
  CheckTrue(ReturnValue,'Value is numeric');

  //not valid cases
  ReturnValue := FRegExUtils.IsAlphanumeric('10.0');
  CheckFalse(ReturnValue,'Value is Float');

  ReturnValue := FRegExUtils.IsAlphanumeric('ab$gig');
  CheckFalse(ReturnValue,'Value has simbols');
end;

procedure TestTRegExUtils.TestIsValidEmail;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidEmail('joe@doe.net');
  CheckTrue(ReturnValue,'Simple email');

  ReturnValue := FRegExUtils.IsValidEmail('joe.doe@gmail.com');
  CheckTrue(ReturnValue,'Email with dot in username');

  ReturnValue := FRegExUtils.IsValidEmail('joe_mail@doe.com');
  CheckTrue(ReturnValue,'Email with _');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidEmail('joedoe.com');
  CheckFalse(ReturnValue,'Email without @');

  ReturnValue := FRegExUtils.IsValidEmail('joe@doecom');
  CheckFalse(ReturnValue,'Email without dot');
end;

procedure TestTRegExUtils.TestIsPasswordComplex;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsPasswordComplex('Fkga$103');
  CheckTrue(ReturnValue,'Password with lower,upper,symbols and min length');

  ReturnValue := FRegExUtils.IsPasswordComplex('Fk%ga103abBBa');
  CheckTrue(ReturnValue,'Password with lower,upper,symbols and more length');

  //not valid cases
  ReturnValue := FRegExUtils.IsPasswordComplex('Fkgaa103abc');
  CheckFalse(ReturnValue,'Password without simbols');

  ReturnValue := FRegExUtils.IsPasswordComplex('fkga$103abc');
  CheckFalse(ReturnValue,'Password without uppercase');

  ReturnValue := FRegExUtils.IsPasswordComplex('Fkga$10');
  CheckFalse(ReturnValue,'Password too short');
end;

procedure TestTRegExUtils.TestIsValidUsername;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidUsername('usr',3,20);
  CheckTrue(ReturnValue,'Username with alfa chars and correct length');

  ReturnValue := FRegExUtils.IsValidUsername('user1',3,20);
  CheckTrue(ReturnValue,'Username with alfanumeric chars and correct length');

  ReturnValue := FRegExUtils.IsValidUsername('USER_9',3,20);
  CheckTrue(ReturnValue,'Username with uppercase and _ char and correct length');

  ReturnValue := FRegExUtils.IsValidUsername('Joe-Doe',3,20);
  CheckTrue(ReturnValue,'Username with - char and correct length');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidUsername('jd',3,20);
  CheckFalse(ReturnValue,'Username too short');

  ReturnValue := FRegExUtils.IsValidUsername('jdoe_username',3,5);
  CheckFalse(ReturnValue,'Username too large');

  ReturnValue := FRegExUtils.IsValidUsername('jdoe$user',3,20);
  CheckFalse(ReturnValue,'Username with symbols');
end;

procedure TestTRegExUtils.TestIsValidUrl;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidUrl('http://google.com',False);
  CheckTrue(ReturnValue,'Url with http');

  ReturnValue := FRegExUtils.IsValidUrl('https://www.google.com',True);
  CheckTrue(ReturnValue,'Url with https');

  ReturnValue := FRegExUtils.IsValidUrl('www.google.com/test.html',True);
  CheckTrue(ReturnValue,'Url optional protocol, without protocol');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidUrl('www.google.com\api\Test',True);
  CheckTrue(ReturnValue,'Url with backslash');

  ReturnValue := FRegExUtils.IsValidUrl('myurl',False);
  CheckFalse(ReturnValue,'Url required protocol, without protocol');

  ReturnValue := FRegExUtils.IsValidUrl('htt://www.google.com/test.html',False);
  CheckFalse(ReturnValue,'Url with incorrect protocol');

  ReturnValue := FRegExUtils.IsValidUrl('www.google.com/Test',False);
  CheckFalse(ReturnValue,'Url without protocol');
end;

procedure TestTRegExUtils.TestIsValidIpv4;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidIpv4('192.168.1.100');
  CheckTrue(ReturnValue,'Valid Ipv4');

  ReturnValue := FRegExUtils.IsValidIpv4('1.1.1.1');
  CheckTrue(ReturnValue,'Valid Ipv4');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidIpv4('aaaa');
  CheckFalse(ReturnValue,'Ipv4 alpha');

  ReturnValue := FRegExUtils.IsValidIpv4('1.1');
  CheckFalse(ReturnValue,'Ipv4 short 1');

  ReturnValue := FRegExUtils.IsValidIpv4('1.1.1');
  CheckFalse(ReturnValue,'Ipv4 short 2');

  ReturnValue := FRegExUtils.IsValidIpv4('300.1.1.1');
  CheckFalse(ReturnValue,'Ipv4 out-of-range 1');

  ReturnValue := FRegExUtils.IsValidIpv4('1.300.1.1');
  CheckFalse(ReturnValue,'Ipv4 out-of-range 2');

  ReturnValue := FRegExUtils.IsValidIpv4('1.1.300.1');
  CheckFalse(ReturnValue,'Ipv4 out-of-range 3');

  ReturnValue := FRegExUtils.IsValidIpv4('1.1.1.300');
  CheckFalse(ReturnValue,'Ipv4 out-of-range 4');
end;

procedure TestTRegExUtils.TestIsValidIpv6;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidIpv6('2001:db8:3333:4444:db8a:6666:7777:8888');
  CheckTrue(ReturnValue,'Ipv6 lowercase');

  ReturnValue := FRegExUtils.IsValidIpv6('2001:DB8:3333:4444:CCCC:DDDD:EEEE:FFFF');
  CheckTrue(ReturnValue,'Ipv6 uppercase');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidIpv6('1.1.1.1');
  CheckFalse(ReturnValue,'Ipv4 address');

  ReturnValue := FRegExUtils.IsValidIpv6('2001:db8:3333:4444:db8a:6666:7777');
  CheckFalse(ReturnValue,'Ipv6 short 1');

  ReturnValue := FRegExUtils.IsValidIpv6(':db8:3333:4444:db8a:6666:7777:88');
  CheckFalse(ReturnValue,'Ipv6 short 2');

  ReturnValue := FRegExUtils.IsValidIpv6('2001:db8:db8a:6666');
  CheckFalse(ReturnValue,'Ipv6 short 3');

  ReturnValue := FRegExUtils.IsValidIpv6('R1DB:db8:3333:4X44:db8a:6666:77S7:K988');
  CheckFalse(ReturnValue,'Ipv6 out-of-range');
end;

procedure TestTRegExUtils.TestIsValidDate_YYYYMMdd;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidDate_YYYYMMdd('2020-10-02');
  CheckTrue(ReturnValue,'Date format YYYY-MM-dd');

  ReturnValue := FRegExUtils.IsValidDate_YYYYMMdd('2020.10.02');
  CheckTrue(ReturnValue,'Date format YYYY.MM.dd');

  ReturnValue := FRegExUtils.IsValidDate_YYYYMMdd('2021/04/02');
  CheckTrue(ReturnValue,'Date format YYYY/MM/dd');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidDate_YYYYMMdd('02/04/2021');
  CheckFalse(ReturnValue,'Date format dd/MM/YYYY');

  ReturnValue := FRegExUtils.IsValidDate_YYYYMMdd('21-04-02');
  CheckFalse(ReturnValue,'Date format YY-MM-dd');

  ReturnValue := FRegExUtils.IsValidDate_YYYYMMdd('2021/1/2');
  CheckFalse(ReturnValue,'Date format YYYY/M/d');

  ReturnValue := FRegExUtils.IsValidDate_YYYYMMdd('21-04-02');
  CheckFalse(ReturnValue,'Date YY/MM/dd');

  ReturnValue := FRegExUtils.IsValidDate_YYYYMMdd('2020-14-31');
  CheckFalse(ReturnValue,'Date format YYYY-MM-dd out-of-range');
end;

procedure TestTRegExUtils.TestIsValidDate_ddMMYYY;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidDate_ddMMYYY('02-12-2020');
  CheckTrue(ReturnValue,'Date format dd-MM-YYYY');

  ReturnValue := FRegExUtils.IsValidDate_ddMMYYY('02.10.2020');
  CheckTrue(ReturnValue,'Date format dd.MM.YYYY');

  ReturnValue := FRegExUtils.IsValidDate_ddMMYYY('12/04/2019');
  CheckTrue(ReturnValue,'Date format dd/MM/YYYY');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidDate_ddMMYYY('2020-10-02');
  CheckFalse(ReturnValue,'Date format YYYY-MM-dd');

  ReturnValue := FRegExUtils.IsValidDate_ddMMYYY('2020.10.02');
  CheckFalse(ReturnValue,'Date format YYYY.MM.dd');

  ReturnValue := FRegExUtils.IsValidDate_ddMMYYY('2021/04/02');
  CheckFalse(ReturnValue,'Date format YYYY/MM/dd');
end;

procedure TestTRegExUtils.TestIsValidHtmlTag;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidHtmlTag('<html>Test</html>');
  CheckTrue(ReturnValue,'Html tag 1');

  ReturnValue := FRegExUtils.IsValidHtmlTag('<html>Test<our>test2</our></html>');
  CheckTrue(ReturnValue,'Html tag 2');

  ReturnValue := FRegExUtils.IsValidHtmlTag('<h2 class="test">');
  CheckTrue(ReturnValue,'Html tag 3');

  ReturnValue := FRegExUtils.IsValidHtmlTag('</a>');
  CheckTrue(ReturnValue,'Html tag 4');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidHtmlTag('Test html>');
  CheckFalse(ReturnValue,'Html tag not started');

  ReturnValue := FRegExUtils.IsValidHtmlTag('Test <<html');
  CheckFalse(ReturnValue,'Html tag not finished');

  ReturnValue := FRegExUtils.IsValidHtmlTag('html Test');
  CheckFalse(ReturnValue,'Html tag not found');
end;

procedure TestTRegExUtils.TestHasDuplicates;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.HasDuplicates('Test Test');
  CheckTrue(ReturnValue,'2 duplicates');

  ReturnValue := FRegExUtils.HasDuplicates('Test Test Test');
  CheckTrue(ReturnValue,'3 duplicates');

  ReturnValue := FRegExUtils.HasDuplicates('Test,Test,Test');
  CheckTrue(ReturnValue,'3 duplicates with commas');

  //not valid cases
  ReturnValue := FRegExUtils.HasDuplicates('Test');
  CheckFalse(ReturnValue,'Not duplicates');
end;

procedure TestTRegExUtils.TestIsValidPhoneNumber;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidPhoneNumber('956223355');
  CheckTrue(ReturnValue,'Local number 1');

  ReturnValue := FRegExUtils.IsValidPhoneNumber('956 331 155');
  CheckTrue(ReturnValue,'Local number 2');

  ReturnValue := FRegExUtils.IsValidPhoneNumber('+34 956223355');
  CheckTrue(ReturnValue,'International number 1');

  ReturnValue := FRegExUtils.IsValidPhoneNumber('(+34) 956 332 552');
  CheckTrue(ReturnValue,'International number 2');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidPhoneNumber('abc');
  CheckFalse(ReturnValue,'Phone without numbers');

  ReturnValue := FRegExUtils.IsValidPhoneNumber('34349gabb');
  CheckFalse(ReturnValue,'Phone with alphanumeric');
end;

procedure TestTRegExUtils.TestIsValidFilePath;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidFilePath('/directory/file.ext');
  CheckTrue(ReturnValue,'Linux path');

  ReturnValue := FRegExUtils.IsValidFilePath('\directory\fiile.ext');
  CheckTrue(ReturnValue,'Windows path 1');

  ReturnValue := FRegExUtils.IsValidFilePath('C:\directory\file.ext');
  CheckTrue(ReturnValue,'Windows path 2');

  ReturnValue := FRegExUtils.IsValidFilePath('google.com/test/file.ext');
  CheckTrue(ReturnValue,'Url path');

  ReturnValue := FRegExUtils.IsValidFilePath('//server/test/file.ext');
  CheckTrue(ReturnValue,'Network path');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidFilePath('file.ext');
  CheckFalse(ReturnValue,'File without path');

  ReturnValue := FRegExUtils.IsValidFilePath('file');
  CheckFalse(ReturnValue,'File without path and extension');

  ReturnValue := FRegExUtils.IsValidFilePath('/directory/file');
  CheckFalse(ReturnValue,'Linux path without extension');

  ReturnValue := FRegExUtils.IsValidFilePath('\directory\fiile');
  CheckFalse(ReturnValue,'Windows path without extension 1');

  ReturnValue := FRegExUtils.IsValidFilePath('C:\directory\file');
  CheckFalse(ReturnValue,'Windows path without extension 2');

  ReturnValue := FRegExUtils.IsValidFilePath('google.com/test/file');
  CheckFalse(ReturnValue,'Url path without extension');

  ReturnValue := FRegExUtils.IsValidFilePath('//server/test/file');
  CheckFalse(ReturnValue,'Network path without extension');
end;

procedure TestTRegExUtils.TestIsValidVisaCard;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidVisaCard('4012888888881881');
  CheckTrue(ReturnValue,'Visa card number');

  ReturnValue := FRegExUtils.IsValidVisaCard('5555555555554444');
  CheckFalse(ReturnValue,'Master card number');

  ReturnValue := FRegExUtils.IsValidVisaCard('378282246310005');
  CheckFalse(ReturnValue,'American Express card number');
end;

procedure TestTRegExUtils.TestIsValidMasterCard;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidMasterCard('5555555555554444');
  CheckTrue(ReturnValue,'Master card number');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidMasterCard('4012888888881881');
  CheckFalse(ReturnValue,'Visa card number');

  ReturnValue := FRegExUtils.IsValidMasterCard('378282246310005');
  CheckFalse(ReturnValue,'American Express card number');
end;

procedure TestTRegExUtils.TestIsValidAmericanExpressCard;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidAmericanExpressCard('378282246310005');
  CheckTrue(ReturnValue,'American Express card number');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidAmericanExpressCard('5555555555554444');
  CheckFalse(ReturnValue,'Master card number');

  ReturnValue := FRegExUtils.IsValidAmericanExpressCard('4012888888881881');
  CheckFalse(ReturnValue,'Visa card number');
end;

procedure TestTRegExUtils.TestIsValidPassport;
var
  ReturnValue: Boolean;
begin
  //
end;

procedure TestTRegExUtils.TestIsValidDNI_ES;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidDNI_ES('86207845G');
  CheckTrue(ReturnValue,'Valid DNI');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidDNI_ES('86207845');
  CheckFalse(ReturnValue,'DNI without letter');

  ReturnValue := FRegExUtils.IsValidDNI_ES('1111111G');
  CheckFalse(ReturnValue,'DNI incorrect');
end;

procedure TestTRegExUtils.TestIsValidSSN_US;
var
  ReturnValue: Boolean;
begin
  //valid cases
  ReturnValue := FRegExUtils.IsValidSSN_US('065-26-0974');
  CheckTrue(ReturnValue,'Valid SSN');

  //not valid cases
  ReturnValue := FRegExUtils.IsValidSSN_US('065-26974');
  CheckFalse(ReturnValue,'SSN incorrect');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTRegExUtils.Suite);
end.

