unit uMain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Quick.Config,
  Quick.Config.Provider.Registry,
  Generics.Collections,
  registry,
  Quick.Config.Provider.Json;

type

  TMyPriority = (msLow, msMed, msHigh);

  TWinPos = class
  private
    fPosX : Integer;
    fPosY : Integer;
  published
    property PosX : Integer read fPosX write fPosX;
    property PosY : Integer read fPosY write fPosY;
  end;

  TProcessType = class
  private
    fId : Integer;
    fPriority : TMyPriority;
    fRedundant : Boolean;
  published
    property Id : Integer read fId write fId;
    property Priority : TMyPriority read fPriority write fPriority;
    property Redundant : Boolean read fRedundant write fRedundant;
  end;

  TWorker = class
  private
    fName : string;
    fActive : Boolean;
  published
    property Name : string read fName write fName;
    property Active : Boolean read fActive write fActive;
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
    fSizes : TArray<Integer>;
    fLastFilename : string;
    fWindowPos : TWinPos;
    fHistory : TArray<TProcessType>;
    fComplex : TProcessType;
    fModifyDate : TDateTime;
    fWorkList : TObjectList<TWorker>;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DefaultValues;
    property Hidden : Boolean read fHidden write fHidden;
  published
    property Title : string read fTitle write fTitle;
    property SessionName : string read fSessionName write fSessionName;
    property Sizes : TArray<Integer> read fSizes write fSizes;
    property LastFilename : string read fLastFilename write fLastFilename;
    //property WindowPos : TWinPos read fWindowPos write fWindowPos;
    property History : TArray<TProcessType> read fHistory write fHistory;
    property Complex : TProcessType read fComplex write fComplex;
    property ModifyDate : TDateTime read fModifyDate write fModifyDate;
    //property WorkList : TObjectList<TWorker> read fWorkList write fWorkList;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnLoadRegistry: TButton;
    btnSaveJson: TButton;
    btnLoadJson: TButton;
    btnSaveRegistry: TButton;
    meInfo: TMemo;
    procedure btnLoadJsonClick(Sender: TObject);
    procedure btnLoadRegistryClick(Sender: TObject);
    procedure btnSaveJsonClick(Sender: TObject);
    procedure btnSaveRegistryClick(Sender: TObject);
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
  ConfigReg : TMyConfig;
  ConfigTest : TMyConfig;
  AppConfigJson : TAppConfigJsonProvider<TMyConfig>;
  AppConfigReg : TAppConfigRegistryProvider<TMyConfig>;

implementation

{$R *.lfm}

uses
  Quick.Json.Serializer;

{ TForm1 }

procedure TForm1.btnSaveJsonClick(Sender: TObject);
begin
  SetConfig(ConfigJson);
  AppConfigJson.Save(ConfigJson);
  meInfo.Lines.Add(ConfigJson.ToJson);
  meInfo.Lines.Add('Saved Config in Json at ' + DateTimeToStr(ConfigJson.LastSaved));
end;

procedure TForm1.btnSaveRegistryClick(Sender: TObject);
begin
  SetConfig(ConfigJson);
  AppConfigReg.Save(ConfigJson);
  meInfo.Lines.Add(ConfigJson.ToJson);
  meInfo.Lines.Add('Saved Config in Registry at ' + DateTimeToStr(ConfigJson.LastSaved));
end;

procedure TForm1.btnLoadJsonClick(Sender: TObject);
begin
  meInfo.Lines.Add('Load ConfigJson');
  ConfigJson := TMyConfig.Create;
  AppConfigJson.Load(ConfigJson);
  meInfo.Lines.Add(ConfigJson.ToJSON);
  if TestConfig(ConfigTest,ConfigJson) then meInfo.Lines.Add('Test passed successfully!');
end;

procedure TForm1.btnLoadRegistryClick(Sender: TObject);
begin
  meInfo.Lines.Add('Load ConfigRegistry');
  ConfigJson := TMyConfig.Create;
  AppConfigReg.Load(ConfigJson);
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
    //Assert(cConfig1.WindowPos.PosX = cConfig2.WindowPos.PosX);
    //Assert(cConfig1.WindowPos.PosX = cConfig2.WindowPos.PosX);
    Assert(cConfig1.Complex.Priority = cConfig2.Complex.Priority);
    Assert(cConfig1.Complex.Redundant  = cConfig2.Complex.Redundant);
    Assert(cConfig1.Title = cConfig2.Title);
    //for i := 0 to cConfig1.WorkList.Count - 1 do
    //begin
    //  Assert(cConfig1.WorkList[i].Name = cConfig2.WorkList[i].Name);
    //  Assert(cConfig1.WorkList[i].Active = cConfig2.WorkList[i].Active);
    //end;
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
  if Assigned(AppConfigReg) then AppConfigReg.Free;
  if Assigned(ConfigTest) then ConfigTest.Free;
  if Assigned(ConfigReg) then ConfigReg.Free;
  if Assigned(ConfigJson) then ConfigJson.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ConfigTest := TMyConfig.Create;
  SetConfig(ConfigTest);
  AppConfigJson := TAppConfigJsonProvider<TMyConfig>.Create(ConfigJson);
  AppConfigJson.CreateIfNotExists := True;
  AppConfigJson.Filename := '.\Config.json';
  ConfigJson := TMyConfig.Create;
  AppConfigReg := TAppConfigRegistryProvider<TMyConfig>.Create(ConfigReg);
  AppConfigReg.HRoot := HKEY_CURRENT_USER;
  AppConfigReg.MainKey := '_AppConfig';
end;

procedure TForm1.SetConfig(cConfig: TMyConfig);
var
  processtype : TProcessType;
begin
  cConfig.Title := 'hola';
  cConfig.Complex := TProcessType.Create;
  cConfig.Complex.Id := 1;
  cConfig.Complex.Redundant := True;
  cConfig.Complex.Priority := TMyPriority.msMed;
  //processtype := TProcessType.Create;
  //processtype.Id := 1;
  //processtype.Priority := msLow;
  //processtype.Redundant := True;
  //cConfig.History := [processtype];
  cConfig.ModifyDate := Now();
end;

{ TMyConfig }

constructor TMyConfig.Create;
begin
  inherited;
  //WorkList := TObjectList<TWorker>.Create(True);
  DefaultValues;
end;

procedure TMyConfig.DefaultValues;
begin
  fTitle := 'Default value';
end;

destructor TMyConfig.Destroy;
begin
  //if Assigned(WorkList) then WorkList.Free;
  inherited;
end;

end.

