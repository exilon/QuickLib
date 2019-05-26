unit uMain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  {$IFDEF FPC}
  registry,
  {$ENDIF}
  Quick.Config.Yaml,
  Quick.Yaml,
  Generics.Collections;

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

  TMyConfig2 = class(TAppConfigYaml)
  private
    fhola : Integer;
  published
    property hola : Integer read fhola write fhola;
  end;

  TArraySizes = TArray<Integer>;
  TArrayHistory = array of TProcessType;

  TMyConfig = class(TAppConfigYaml)
  private
    fTitle : string;
    fHidden : Boolean;
    fSessionName: string;
    fSizes : TArraySizes;
    fLastFilename : string;
    fWindowPos : TWinPos;
    fHistory : TArrayHistory;
    fComplex : TProcessType;
    fModifyDate : TDateTime;
    //fWorkList : TObjectList<TWorker>;
  public
    procedure Init; override;
    destructor Destroy; override;
    procedure DefaultValues; override;
    property Hidden : Boolean read fHidden write fHidden;
  published
    property Title : string read fTitle write fTitle;
    property SessionName : string read fSessionName write fSessionName;
    property Sizes : TArraySizes read fSizes write fSizes;
    property LastFilename : string read fLastFilename write fLastFilename;
    property WindowPos : TWinPos read fWindowPos write fWindowPos;
    property History : TArrayHistory read fHistory write fHistory;
    property Complex : TProcessType read fComplex write fComplex;
    property ModifyDate : TDateTime read fModifyDate write fModifyDate;
    //property WorkList : TObjectList<TWorker> read fWorkList write fWorkList;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnSaveYaml: TButton;
    btnLoadYaml: TButton;
    Button1: TButton;
    meInfo: TMemo;
    procedure btnLoadYamlClick(Sender: TObject);
    procedure btnSaveYamlClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OnConfigFileModified;
  private

  public
    procedure SetConfig(cConfig: TMyConfig);
    function TestConfig(cConfig1, cConfig2 : TMyConfig) : Boolean;

  end;

var
  Form1: TForm1;
  ConfigYaml : TMyConfig;
  ConfigTest : TMyConfig;

implementation

{$R *.lfm}

uses
  Quick.Yaml.Serializer;

{ TForm1 }

procedure TForm1.btnSaveYamlClick(Sender: TObject);
begin
  SetConfig(ConfigYaml);
  ConfigYaml.Save;

  meInfo.Lines.Add(ConfigYaml.ToYaml);
  meInfo.Lines.Add('Saved Config in Yaml at ' + DateTimeToStr(ConfigYaml.LastSaved));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  yaml : TYamlObject;
  sl : TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile('.\Config.yml');
    yaml := TYamlObject.Create;
    yaml.ParseYaml(sl.Text);
    meInfo.Lines.Add(yaml.ToYaml);
  finally
    sl.Free;
  end;
end;

procedure TForm1.btnLoadYamlClick(Sender: TObject);
begin
  meInfo.Lines.Add('Load ConfigYaml');
  ConfigYaml.Load;
  meInfo.Lines.Add(ConfigYaml.ToYaml);
  if TestConfig(ConfigTest,ConfigYaml) then meInfo.Lines.Add('Test passed successfully!');
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
  if Assigned(ConfigTest) then ConfigTest.Free;
  if Assigned(ConfigYaml) then ConfigYaml.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ConfigTest := TMyConfig.Create('');
  SetConfig(ConfigTest);
  ConfigYaml := TMyConfig.Create('.\Config.yml');
  ConfigYaml.Provider.CreateIfNotExists := True;
  ConfigYaml.Provider.ReloadIfFileChanged := True;
  ConfigYaml.Provider.OnFileModified := OnConfigFileModified;
end;

procedure TForm1.OnConfigFileModified;
begin
  meInfo.Lines.Add('Config file modified');
end;

procedure TForm1.SetConfig(cConfig: TMyConfig);
var
  processtype : TProcessType;
begin
  cConfig.Title := 'hola';
  cConfig.SessionName := 'Session01';
  cConfig.LastFileName := 'C:\library.txt';
  cConfig.Sizes := [1,2,3,4,5,6,7];
  cConfig.Complex := TProcessType.Create;
  cConfig.Complex.Id := 1;
  cConfig.Complex.Redundant := True;
  cConfig.Complex.Priority := TMyPriority.msMed;
  cConfig.WindowPos := TWinPos.Create;
  cConfig.WindowPos.PosX := 100;
  cConfig.WindowPos.PosY := 200;
  processtype := TProcessType.Create;
  processtype.Id := 1;
  processtype.Priority := msLow;
  processtype.Redundant := True;
  cConfig.History := [processtype];
  cConfig.ModifyDate := Now();
end;

{ TMyConfig }

procedure TMyConfig.Init;
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

