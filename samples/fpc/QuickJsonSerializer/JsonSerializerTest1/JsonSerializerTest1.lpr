program JsonSerializerTest1;

{$mode delphi}

uses
  SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Json.Serializer;

type
  THost = class
  private
    fName : string;
    fIP : string;
    fPort : Integer;
  published
    property Name : string read fName write fName;
    property IP : string read fIP write fIP;
    property Port : Integer read fPort write fPort;
  end;

  THostList = TArray<THost>;

  TConfig = class
  private
    fHosts : THostList;
    fDebugMode : Boolean;
    fLevel : Integer;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Hosts : THostList read fHosts write fHosts;
    property DebugMode : Boolean read fDebugMode write fDebugMode;
    property Level : Integer read fLevel write fLevel;
  end;

const
  jsonstring = '{"Hosts":[{"Name":"Host 1 año perfección","IP":"127.0.0.1","Port":80},{"Name":"Host 2","IP":"192.168.1.1","Port":443}],"DebugMode":true,"Level":1}';
  jsonstring2 = '{"Hosts":{"List":[{"Name":"Host 1","IP":"127.0.0.2","Port":80},{"Name":"Host 2","IP":"192.168.1.2","Port":443}]},"DebugMode":true,"Level":2}';

var
  config : TConfig;
  host : THost;
  serializer : TJsonSerializer;
  json : string;

{ TConfig }

constructor TConfig.Create;
begin
end;

destructor TConfig.Destroy;
var
  host : THost;
begin
  for host in fHosts do host.Free;
  inherited;
end;

begin
  try
    serializer := TJsonSerializer.Create(slPublishedProperty);
    try

      //created from object
      cout('Create from object',ccYellow);
      config := TConfig.Create;
      try
        host := THost.Create;
        host.Name := 'Host 1';
        host.IP := '127.0.0.1';
        host.Port := 80;
        config.DebugMode := True;
        config.Level := 1;
        config.Hosts := config.Hosts + [host];

        host := THost.Create;
        host.Name := 'Host 2';
        host.IP := '192.168.1.1';
        host.Port := 443;
        config.Hosts := config.Hosts + [host];

        json := serializer.ObjectToJson(config,True);
        cout(json,ccWhite);
        coutFmt('Count: %d',[High(config.Hosts) + 1],etInfo);
      finally
        config.Free;
      end;

      //from json string without list property
      cout('Create from jsonstring without "List" property',ccYellow);
      config := TConfig.Create;
      try
        serializer.JsonToObject(config,jsonstring);
        json := serializer.ObjectToJson(config,True);
        cout(json,ccWhite);
        coutFmt('Count: %d',[High(config.Hosts) + 1],etInfo);
      finally
        config.Free;
      end;

      //from json string with list property
      cout('Create from jsonstring with "List" property',ccYellow);
      config := TConfig.Create;
      try
        serializer.JsonToObject(config,jsonstring2);
        json := serializer.ObjectToJson(config,True);
        cout(json,ccWhite);
        coutFmt('Count: %d',[High(config.Hosts) + 1],etInfo);
      finally
        config.Free;
      end;
    finally
      serializer.Free;
    end;
    ConsoleWaitForEnterKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
