unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  System.Generics.Collections,
  Quick.Commons,
  Quick.Config.YAML;

type

  TMyPriority = (msLow, msMed, msHigh);

  TWinPos = class
  private
    fPosX : Integer;
    fPosY : Integer;
    fFixed : Boolean;
  published
    property PosX : Integer read fPosX write fPosX;
    property PosY : Integer read fPosY write fPosY;
    property Fixed : Boolean read fFixed write fFixed;
  end;

  TProcessType = record
    Id : Integer;
    Priority : TMyPriority;
    Redundant : Boolean;
  end;

  TJob = class
  private
    fJobName : string;
    fTimeElapsed : Integer;
  published
    property JobName : string read fJobName write fJobName;
    property TimeElapsed : Integer read fTimeElapsed write fTimeElapsed;
  end;

  TWorker = class
  private
    fName : string;
    fJob : TJob;
    fLevels : TArray<Integer>;
    fActive : Boolean;
  published
    property Name : string read fName write fName;
    property Job : TJob read fJob write fJob;
    property Levels : TArray<Integer> read fLevels write fLevels;
    property Active : Boolean read fActive write fActive;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMyConfig = class(TAppConfigYAML)
  private
    fTitle : string;
    fHidden : Boolean;
    fSessionName: string;
    fSizes : TArray<Integer>;
    fMethods : TArray<string>;
    fLastFilename : string;
    fWindowPos : TWinPos;
    fHistory : TArray<TProcessType>;
    fComplex : TProcessType;
    fDefaultWorker : TWorker;
    fModifyDate : TDateTime;
    fWorkList : TObjectList<TWorker>;
  published
    [TCommentProperty('Sizes array is simple')]
    property Sizes : TArray<Integer> read fSizes write fSizes;
    property LastFilename : string read fLastFilename write fLastFilename;
    property Methods : TArray<string> read fMethods write fMethods;
    property WindowPos : TWinPos read fWindowPos write fWindowPos;
    [TCommentProperty('Array of records')]
    property History : TArray<TProcessType> read fHistory write fHistory;
    property Complex : TProcessType read fComplex write fComplex;
    property DefaultWorker : TWorker read fDefaultWorker write fDefaultWorker;
    property ModifyDate : TDateTime read fModifyDate write fModifyDate;
    property Title : string read fTitle write fTitle;
    property SessionName : string read fSessionName write fSessionName;
    [TCommentProperty('List of work tasks config')]
    property WorkList : TObjectList<TWorker> read fWorkList write fWorkList;
  public
    destructor Destroy; override;
    procedure Init; override;
    procedure DefaultValues; override;
  end;

  TMainForm = class(TForm)
    meInfo: TMemo;
    btnLoadFile: TButton;
    btnSaveFile: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSaveFileClick(Sender: TObject);
    procedure btnLoadFileClick(Sender: TObject);
    procedure SetConfig(cConfig: TMyConfig);
    function TestConfig(cConfig1, cConfig2 : TMyConfig) : Boolean;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OnFileModified;
  end;

var
  MainForm: TMainForm;
  ConfigTest : TMyConfig;
  ConfigYaml : TMyConfig;

implementation

{$R *.dfm}

procedure TMainForm.btnLoadFileClick(Sender: TObject);
var
  sl : TStringList;
  s : string;
begin
  meInfo.Lines.Add('Load ConfigReg');
  ConfigYaml.Load;
  meInfo.Lines.Add(ConfigYaml.ToYAML);
  if TestConfig(configtest,ConfigYaml) then meInfo.Lines.Add('Test passed successfully!');
 end;

procedure TMainForm.btnSaveFileClick(Sender: TObject);
begin
  ConfigYaml.Free;
  ConfigYaml := TMyConfig.Create('.\config.yml');
  SetConfig(ConfigYaml);
  ConfigYaml.Save;
  meInfo.Lines.Add('Saved Config in Yaml at ' + DateTimeToStr(ConfigYaml.LastSaved));
end;

procedure TMainForm.SetConfig(cConfig : TMyConfig);
var
  winpos : TWinpos;
  protype : TProcessType;
  i : Integer;
  worker : TWorker;
begin
  cConfig.LastFilename := 'library.txt';
  cConfig.Sizes := [23,11,554,12,34,29,77,30,48,59,773,221,98,3,22,983,122,231,433,12,31,987];
  cConfig.DefaultWorker.Levels := [10,12,14,18,20];
  cConfig.WindowPos.PosX := 640;
  cConfig.WindowPos.PosX := 480;
  cConfig.WindowPos.Fixed := True;
  cConfig.Methods := ['GET','POST','PUT','DELETE','HEAD'];
  protype.Id := 5;
  protype.Priority := msHigh;
  protype.Redundant := False;
  cConfig.Complex := protype;
  cConfig.DefaultWorker.Name := 'Process ' + i.ToString;
  cConfig.DefaultWorker.Job.JobName := 'Job ' + i.ToString;
  cConfig.DefaultWorker.Job.TimeElapsed := i * Random(1000);
  cConfig.DefaultWorker.Active := Boolean(Random(1));
  cConfig.Title := 'a fresh title';
  cConfig.SessionName := 'First Session';
  for I := 0 to 5 do
  begin
    worker := TWorker.Create;
    worker.Name := 'Process ' + i.ToString;
    worker.Levels := [10,12,14,18,20];
    worker.Job.JobName := 'Job ' + i.ToString;
    worker.Job.TimeElapsed := i * Random(1000);
    worker.Active := Boolean(Random(1));
    cConfig.WorkList.Add(worker);
  end;
  for i := 0 to 2 do
  begin
    protype.Id := i;
    protype.Priority := msLow;
    protype.Redundant := True;
    cConfig.History := cConfig.History + [protype];
  end;
  cConfig.ModifyDate := Now();
end;

function  TMainForm.TestConfig(cConfig1, cConfig2 : TMyConfig) : Boolean;
var
  i : Integer;
begin
  Result := False;
  try
    Assert(cConfig1.LastFilename = cConfig2.LastFilename);
    for i := Low(cConfig1.Sizes) to High(cConfig1.Sizes) do
      Assert(cConfig1.Sizes[i] = cConfig2.Sizes[i]);
    Assert(cConfig1.WindowPos.PosX = cConfig2.WindowPos.PosX);
    Assert(cConfig1.WindowPos.PosX = cConfig2.WindowPos.PosX);
    Assert(cConfig1.Complex.Priority = cConfig2.Complex.Priority);
    Assert(cConfig1.Complex.Redundant  = cConfig2.Complex.Redundant);
    Assert(cConfig1.Title = cConfig2.Title);
    Assert(cConfig1.WorkList.Count = cConfig2.WorkList.Count);
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

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(ConfigYaml) then ConfigYaml.Free;
  if Assigned(ConfigTest) then ConfigTest.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ConfigYaml := TMyConfig.Create('.\config.yml');
  ConfigYaml.Provider.OnFileModified := OnFileModified;
  ConfigYaml.Provider.ReloadIfFileChanged := False;
  //create config test to compare later
  ConfigTest := TMyConfig.Create('');
  SetConfig(ConfigTest);
end;

procedure TMainForm.OnFileModified;
begin
  meInfo.Lines.Add('Config file modified. Config will be reload');
end;

{ TMyConfig }

procedure TMyConfig.Init;
begin
  inherited;
  fWorkList := TObjectList<TWorker>.Create(True);
  fWindowPos := TWinPos.Create;
  fDefaultWorker := TWorker.Create;
  DefaultValues;
end;

procedure TMyConfig.DefaultValues;
begin
  inherited;
  fTitle := 'Default value';
end;

destructor TMyConfig.Destroy;
begin
  if Assigned(fWorkList) then fWorkList.Free;
  if Assigned(fDefaultWorker) then fDefaultWorker.Free;
  if Assigned(fWindowPos) then fWindowPos.Free;
  inherited;
end;

{ TWorker }

constructor TWorker.Create;
begin
  fJob := TJob.Create;
end;

destructor TWorker.Destroy;
begin
  fJob.Free;
  inherited;
end;

end.
