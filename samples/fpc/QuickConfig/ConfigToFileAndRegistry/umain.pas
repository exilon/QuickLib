unit uMain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Quick.Config,
  //Quick.Config.Provider.Registry,
  Generics.Collections,
  Quick.Config.Provider.Json;

type

  TMyPriority = (msLow, msMed, msHigh);

  TWinPos = record
    PosX : Integer;
    PosY : Integer;
  end;

  TProcessType = record
    Id : Integer;
    Priority : TMyPriority;
    Redundant : Boolean;
  end;

  TWorker = class
    Name : string;
    Active : Boolean;
  end;

  TMyConfig2 = class(TAppConfig)
  private
    fhola : Integer;
  published
    property hola : Integer read fhola write fhola;
  end;

  TMyConfig = class(TAppConfig)
  private
    fTitle : string;
    fHidden : Boolean;
    fSessionName: string;
  public
    Sizes : array of Integer;
    LastFilename : string;
    WindowPos : TWinPos;
    History : array of TProcessType;
    Complex : TProcessType;
    ModifyDate : TDateTime;
    WorkList : TObjectList<TWorker>;
    constructor Create; override;
    destructor Destroy; override;
    procedure DefaultValues;
    property Title : string read fTitle write fTitle;
    property SessionName : string read fSessionName write fSessionName;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnSaveJson: TButton;
    btnLoadJson: TButton;
    meInfo: TMemo;
    procedure btnLoadJsonClick(Sender: TObject);
    procedure btnSaveJsonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure SetConfig(cConfig: TMyConfig);
    function TestConfig(cConfig1, cConfig2 : TMyConfig) : Boolean;

  end;

var
  Form1: TForm1;
  ConfigJson : TMyConfig;
  ConfigJson2 : TMyConfig2;
  ConfigReg : TMyConfig;
  ConfigTest : TMyConfig;
  AppConfigJson : TAppConfigJsonProvider<TMyConfig2>;
  //AppConfigReg : TAppConfigRegistryProvider<TMyConfig>;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnSaveJsonClick(Sender: TObject);
begin
  SetConfig(ConfigJson);
  AppConfigJson.Save(ConfigJson2);
  meInfo.Lines.Add('Saved Config in Json at ' + DateTimeToStr(ConfigJson.LastSaved));
end;

procedure TForm1.btnLoadJsonClick(Sender: TObject);
begin
  meInfo.Lines.Add('Load ConfigJson');
  AppConfigJson.Load(ConfigJson2);
  meInfo.Lines.Add(ConfigJson.ToJSON);
  if TestConfig(ConfigTest,ConfigJson) then meInfo.Lines.Add('Test passed successfully!');
end;

function  TForm1.TestConfig(cConfig1, cConfig2 : TMyConfig) : Boolean;
var
  i : Integer;
begin
  try
    Assert(cConfig1.LastFilename = cConfig2.LastFilename);
    for i := Low(cConfig1.Sizes) to High(cConfig1.Sizes) do
      Assert(cConfig1.Sizes[i] = cConfig2.Sizes[i]);
    Assert(cConfig1.WindowPos.PosX = cConfig2.WindowPos.PosX);
    Assert(cConfig1.WindowPos.PosX = cConfig2.WindowPos.PosX);
    Assert(cConfig1.Complex.Priority = cConfig2.Complex.Priority);
    Assert(cConfig1.Complex.Redundant  = cConfig2.Complex.Redundant);
    Assert(cConfig1.Title = cConfig2.Title);
    for i := 0 to cConfig1.WorkList.Count - 1 do
    begin
      Assert(cConfig1.WorkList[i].Name = cConfig2.WorkList[i].Name);
      Assert(cConfig1.WorkList[i].Active = cConfig2.WorkList[i].Active);
    end;
    for i := 0 to High(cConfig1.History) do
    begin
      Assert(cConfig1.History[i].Priority = cConfig2.History[i].Priority);
      Assert(cConfig1.History[i].Redundant = cConfig2.History[i].Redundant);
    end;
    Result := True;
  except
    ShowMessage('Configuration not has been saved previously or has a corruption problem');
  end;
end;


procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(AppConfigJson) then AppConfigJson.Free;
  //if Assigned(AppConfigReg) then AppConfigReg.Free;
  if Assigned(ConfigTest) then ConfigTest.Free;
  if Assigned(ConfigReg) then ConfigReg.Free;
  if Assigned(ConfigJson) then ConfigJson.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ConfigTest := TMyConfig.Create;
  SetConfig(ConfigTest);
  AppConfigJson := TAppConfigJsonProvider<TMyConfig2>.Create(ConfigJson2);
  AppConfigJson.CreateIfNotExists := True;
  AppConfigJson.Filename := 'Config.json';
  //AppConfigReg := TAppConfigRegistryProvider<TMyConfig>.Create(ConfigReg);
  //AppConfigReg.HRoot := HKEY_CURRENT_USER;
  //AppConfigReg.MainKey := '_AppConfig';
end;

procedure TForm1.SetConfig(cConfig: TMyConfig);
begin

end;

{ TMyConfig }

constructor TMyConfig.Create;
begin
  inherited;
  WorkList := TObjectList<TWorker>.Create(True);
  DefaultValues;
end;

procedure TMyConfig.DefaultValues;
begin
  fTitle := 'Default value';
end;

destructor TMyConfig.Destroy;
begin
  if Assigned(WorkList) then WorkList.Free;
  inherited;
end;

end.

