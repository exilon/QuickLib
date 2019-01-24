unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  DateUtils,
  Quick.Commons,
  Quick.Threads, FMX.StdCtrls, FMX.Layouts;

type

  TMyJob = class
  private
    fId : Integer;
    fName : string;
  public
    destructor Destroy; override;
    property Id : Integer read fId write fId;
    property Name : string read fName write fName;
    procedure DoJob;
  end;


  TfrmMain = class(TForm)
    meLog: TMemo;
    Layout1: TLayout;
    btnStart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    { Private declarations }
    ScheduledTasks : TScheduledTasks;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  ScheduledDate : TDateTime;
  ExpirationDate : TDateTime;

implementation

{$R *.fmx}


procedure Log(const msg : string; params : array of const);
begin
  frmMain.meLog.Lines.Add(Format(msg,params));
end;

destructor TMyJob.Destroy;
begin

  inherited;
end;

procedure TMyJob.DoJob;
var
  a, b, i : Integer;
begin
  log('[%s] task "%s" doing job...',[DateTimeToStr(Now()),fName]);
  //Sleep(Random(1000));
  a := Random(100);
  b := Random(5) + 1;

  i := a div b;
  log('task "%s" result %d / %d = %d',[fName,a,b,i]);
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  ScheduledTasks.Start;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  //if not ScheduledTasks.IsStarted then ScheduledTasks.Start;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ScheduledTasks.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  myjob : TMyJob;
begin
  ScheduledTasks := TScheduledTasks.Create;
  myjob := TMyJob.Create;
    myjob.Id := 1;
    myjob.Name := 'Run now and repeat every 1 second for 5 times';
    scheduledtasks.AddTask_Sync('Task1',[myjob,1],True,
                            procedure(task : ITask)
                            begin
                              Log('task "%s" started',[TMyJob(task.Param[0]).Name]);
                              TMyJob(task.Param[0]).DoJob;
                            end
                          ).OnException(
                            procedure(task : ITask; aException : Exception)
                            begin
                              Log('task "%s" failed (%s)',[TMyJob(task.Param[0]).Name,aException.Message]);
                            end
                          ).OnTerminated_Sync(
                            procedure(task : ITask)
                            begin
                              Log('task "%s" finished',[TMyJob(task.Param[0]).Name]);
                            end
                          ).OnExpired_Sync(
                            procedure(task : ITask)
                            begin
                              Log('task "%s" expired',[TMyJob(task.Param[0]).Name]);
                            end
                          ).RepeatEvery(1,TTimeMeasure.tmSeconds,5);

    myjob := TMyJob.Create;
    myjob.Id := 2;
    myjob.Name := 'Run now, repeat every 1 second forever';
    scheduledtasks.AddTask_Sync('Task2',[myjob,32,true,3.2,myjob.ClassType],True,
                            procedure(task : ITask)
                            begin
                              Log('task "%s" started with params(Int=%d / Bool=%s / Float=%s /Class=%s)',[TMyJob(task.Param[0]).Name,task.Param[1].AsInteger,task.Param[2].AsString,task.Param[3].AsString,task.Param[4].AsString]);
                              TMyJob(task.Param[0]).DoJob;
                            end
                          ).OnException(
                            procedure(task : ITask; aException : Exception)
                            begin
                              Log('task "%s" failed (%s)',[TMyJob(task.Param[0]).Name,aException.Message]);
                            end
                          ).OnTerminated_Sync(
                            procedure(task : ITask)
                            begin
                              Log('task "%s" finished',[TMyJob(task.Param[0]).Name]);
                            end
                          ).OnExpired_Sync(
                            procedure(task : ITask)
                            begin
                              Log('task "%s" expired',[TMyJob(task.Param[0]).Name]);
                            end
                          ).StartAt(Now()
                          ).RepeatEvery(1,TTimeMeasure.tmSeconds);

    ScheduledDate := IncSecond(Now(),5);
    ExpirationDate := IncSecond(ScheduledDate,10);

    myjob := TMyJob.Create;
    myjob.Id := 3;
    myjob.Name := Format('Run at %s and repeat every 1 second until %s',[DateTimeToStr(ScheduledDate),DateTimeToStr(ExpirationDate)]);


    scheduledtasks.AddTask_Sync('Task3',[myjob],True,
                            procedure(task : ITask)
                            begin
                              Log('task "%s" started',[TMyJob(task.Param[0]).Name]);
                              TMyJob(task.Param[0]).DoJob;
                            end
                          ).OnException(
                            procedure(task : ITask; aException : Exception)
                            begin
                              Log('task "%s" failed (%s)',[TMyJob(task.Param[0]).Name,aException.Message]);
                            end
                          ).OnTerminated_Sync(
                            procedure(task : ITask)
                            begin
                              Log('task "%s" finished',[TMyJob(task.Param[0]).Name]);
                            end
                          ).OnExpired_Sync(
                            procedure(task : ITask)
                            begin
                              Log('task "%s" expired',[TMyJob(task.Param[0]).Name]);
                            end
                          ).StartAt(ScheduledDate
                          ).RepeatEvery(1,TTimeMeasure.tmSeconds,ExpirationDate);

    Log('Running tasks in background!',[]);
end;

end.
