program SendEmail;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Console,
  Quick.SMTP;

var
  smtp : TSMTP;
begin
  try
    smtp := TSMTP.Create('mail.mydomain.com',25,True);
    smtp.Username := 'myemail@mydomain.com';
    smtp.Password := '1234';
    smtp.SendEmail('myemail@mydomain.com',
                   'Test email',
                   'A test message',
                   'other@mydomain.com',
                   'cc@mydomain.com',
                   'bc@mydomain.com',
                   'This is the body message');
    cout('Press <ENTER> to Exit',ccYellow);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
