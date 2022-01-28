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
    ReportMemoryLeaksOnShutdown := True;
    smtp := TSMTP.Create('mail.domain.com',25,False);
    try
      smtp.Username := 'test@domain.com';
      smtp.Password := '';
      smtp.Mail.AddAttachment('output2.png','d:\output.png');
      //smtp.Mail.AddAttachment('dell.gif','d:\dell.gif');
      //smtp.Mail.AddAttachment('config.json','d:\config.json');
      //smtp.Mail.Attachments.Add('d:\output.png');
      //smtp.Mail.Attachments.Add('d:\config.json');
      smtp.Mail.SenderName := 'Attachment Test';
      smtp.Mail.From := 'test@domain.com';
      smtp.Mail.Recipient := 'other@domain.com';
      smtp.Mail.Subject := 'test adjuntos';
      smtp.Mail.Body := 'Ver adjuntos';
      smtp.SendMail;
    finally
      smtp.Free;
    end;
    cout('Press <ENTER> to Exit',ccYellow);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
