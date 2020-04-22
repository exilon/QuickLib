program JsonSerializerTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  Quick.Commons,
  Quick.Console,
  Quick.Json.Serializer;

type
  THost = class
  private
    fID: TGUID;
    fName : string;
    fIP : string;
    fPort : Integer;
  published
    property ID: TGUID read fID write fID;
    property Name : string read fName write fName;
    property IP : string read fIP write fIP;
    property Port : Integer read fPort write fPort;
  end;

  THostList = TObjectList<THost>;

  TConfig = class
  private
    fHosts : THostList;
    fDebugMode : Boolean;
    fLevel : Integer;
  published
    constructor Create;
    destructor Destroy; override;
    property Hosts : THostList read fHosts write fHosts;
    property DebugMode : Boolean read fDebugMode write fDebugMode;
    property Level : Integer read fLevel write fLevel;
  end;

const
  jsonstring = '{"Hosts":['+
                  '{"Name":"Host 1 año perfeción","IP":"127.0.0.1","Port":80, "ID":"{00FB3A62-F77D-4E71-9613-79E2E49D4562}"},'+
                  '{"Name":"Host 2","IP":"192.168.1.1","Port":443,"ID":"{EBEBBC33-71F2-404A-8C0E-903CFA268616}"}'+
                        '],'+
                '"DebugMode":true,"Level":1}';
  jsonstring2 = '{"Hosts":'+
                  '{"List":['+
                      '{"Name":"Host 1","IP":"127.0.0.2","Port":80, "ID":"{D52917AE-0A21-4B5B-945A-0F17FD158332}"},'+
                      '{"Name":"Host 2","IP":"192.168.1.2","Port":443, "ID":"{80E6467A-282C-437E-B66A-D704004A2C3F}"}'+
                          ']},'+
                   '"DebugMode":true,"Level":2'+
                   '}';

var
  config : TConfig;
  host : THost;
  serializer : TJsonSerializer;
  json : string;
  guid: TGUID;

{ TConfig }

constructor TConfig.Create;
begin
  fHosts := THostList.Create(True);
end;

destructor TConfig.Destroy;
begin
  fHosts.Free;
  inherited;
end;

begin
  try
    ReportMemoryLeaksOnShutdown := True;
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
        CreateGUID(guid);
        host.ID:=guid;
        config.DebugMode := True;
        config.Level := 1;
        config.Hosts.Add(host);

        host := THost.Create;
        host.Name := 'Host 2';
        host.IP := '192.168.1.1';
        host.Port := 443;
        CreateGUID(guid);
        host.ID:=guid;
        config.Hosts.Add(host);

        json := serializer.ObjectToJson(config,True);
        cout(json,ccWhite);
        coutFmt('Capacity: %d / Count: %d',[config.Hosts.Capacity,config.Hosts.Count],etInfo);
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
        coutFmt('Capacity: %d / Count: %d',[config.Hosts.Capacity,config.Hosts.Count],etInfo);
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
        coutFmt('Capacity: %d / Count: %d',[config.Hosts.Capacity,config.Hosts.Count],etInfo);
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
