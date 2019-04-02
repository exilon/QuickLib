unit uMain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  {$IFDEF FPC}
  registry,
  {$ENDIF}
  Quick.Config.Registry,
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

  TMyConfig2 = class(TAppConfigRegistry)
  private
    fhola : Integer;
  published
    property hola : Integer read fhola write fhola;
  end;

  TArraySizes = array of Integer;
  TArrayHistory = array of TProcessType;

  TMyConfig = class(TAppConfigRegistry)
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
  ConfigReg : TMyConfig;
  ConfigTest : TMyConfig;

implementation

{$R *.lfm}

uses
  Quick.Json.Serializer;

{ TForm1 }

procedure TForm1.btnSaveJsonClick(Sender: TObject);
begin
  SetConfig(ConfigReg);
  ConfigReg.Save;

  meInfo.Lines.Add(ConfigReg.ToJson);
  meInfo.Lines.Add('Saved Config in Json at ' + DateTimeToStr(ConfigReg.LastSaved));
end;

procedure TForm1.btnLoadJsonClick(Sender: TObject);
var
  NewConfig : TMyConfig;
begin
  meInfo.Lines.Add('Load ConfigJson');
  NewConfig := TMyConfig.Create(ConfigReg.Provider.HRoot,ConfigReg.Provider.MainKey);
  try
    NewConfig.Load;
    meInfo.Lines.Add(NewConfig.ToJSON);
    if TestConfig(ConfigTest,NewConfig) then meInfo.Lines.Add('Test passed successfully!');
  finally
    NewConfig.Free;
  end;
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
  if Assigned(ConfigReg) then ConfigReg.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ConfigTest := TMyConfig.Create;
  SetConfig(ConfigTest);
  ConfigReg := TMyConfig.Create(HKEY_CURRENT_USER,'_AppConfig2');
  ConfigReg.Provider.CreateIfNotExists := True;
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

