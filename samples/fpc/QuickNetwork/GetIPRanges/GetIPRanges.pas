program GetIPRanges;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Quick.Commons,
  Quick.Network,
  Quick.Console;

var
  Ip,
  Mask : string;
  LowIP,
  HighIp : string;

begin
  try
    Console.LogVerbose := LOG_DEBUG;
    ip := '192.168.1.15';
    mask := '255.255.255.0';
    GetIpRange(Ip,Mask,LowIP,HighIp);
    coutFmt('IP: %s Mask: %s / Range: %s to %s',[Ip,Mask,LowIP,HighIp],etInfo);
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
