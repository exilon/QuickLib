program HttpServerService;

uses
  SysUtils,
  Quick.Commons,
  Quick.Logger.Intf,
  Quick.Console,
  Quick.AppService,
  Quick.HttpServer,
  Quick.HttpServer.Request,
  Quick.HttpServer.Response,
  HttpServerService.Logger;

type
  TMyHttpServer = class(THttpServer)
  public
    procedure ProcessRequest(aRequest: IHttpRequest; aResponse: IHttpResponse); override;
  end;

  procedure TMyHttpServer.ProcessRequest(aRequest: IHttpRequest; aResponse: IHttpResponse);
  begin
    aResponse.ContentText := 'Hello world!';
  end;

var
  HttpServer : TMyHttpServer;
  Port : Integer;
  Logger : TQuickLogger;

begin
  try
    {$IFNDEF FPC}
    ReportMemoryLeaksOnShutdown := True;
    {$ENDIF}
    //run as console
    Logger := TQuickLogger.Create;
    Logger.Init;
    if not AppService.IsRunningAsService then
    begin
      //create server
      cout('Init server...',etInfo);
      if ParamCount > 0 then
      begin
        Integer.TryParse(ParamStr(1),Port)
      end;
      //start server
      if Port = 0 then Port := 8080;
      HttpServer := TMyHttpServer.Create('127.0.0.1',Port,False,Logger);
      try
        HttpServer.Start;
        //Wait for Exit
        cout(' ',ccWhite);
        cout('Press [Enter] to quit',ccYellow);
        ConsoleWaitForEnterKey;
      finally
        HttpServer.Free;
      end;
    end
    else //run as a service
    begin
      AppService.DisplayName := 'Remote Server';
      AppService.ServiceName := 'RemoteServerSvc';
      AppService.CanInstallWithOtherName := True;
      HttpServer := TMyHttpServer.Create('127.0.0.1',Port,False,Logger);
      AppService.OnStop := @HttpServer.Free;
      AppService.OnExecute := @HttpServer.Start;
      AppService.CheckParams;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
