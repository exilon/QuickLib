unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo,
  Quick.IOC,
  Dependencies;

type
  TForm1 = class(TForm)
    meInfo: TMemo;
    Panel1: TPanel;
    btnCheckIOC: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCheckIOCClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  iocContainer : TIocContainer;
  sumservice : ISumService;
  multservice : IMultService;
  bigservice : IBigService;
  divideservice : TDivideService;
  executions : Integer = 0;

implementation

{$R *.fmx}

procedure TForm1.btnCheckIOCClick(Sender: TObject);
var
  res : Integer;
  i : Integer;
  times : Integer;
  numoperations : Integer;
begin
  times := 100;
  res := 0;
  Inc(executions);

  //test1: class injection as singleton
  for i := 1 to times do
  begin
    sumservice := iocContainer.Resolve<ISumService>;
    res := sumservice.Sum(2,2);
  end;
  if sumservice.NumOperations = times * executions + (times * (executions - 1)) then meInfo.Lines.Add('Test1: Class injection as Singleton test ok')
    else meInfo.Lines.Add('Test1: Class injection as Singleton test error');
  meInfo.Lines.Add(Format('SumService.Sum = %d (calls: %d)',[res,sumservice.NumOperations]));

  //test2: class injection as transient
  for i := 1 to times do
  begin
    multservice := iocContainer.Resolve<IMultService>;
    res := multservice.Mult(2,4);
  end;
  if multservice.NumOperations = 1 then meInfo.Lines.Add('Test2: Class injection as Transient test ok')
    else meInfo.Lines.Add('Test2: Class injection as Transient test error');
  meInfo.Lines.Add(Format('MultService.Mult = %d (calls: %d)',[res,multservice.NumOperations]));

  //test3: constructor injection as singleton
  for i := 1 to times do
  begin
    bigservice := iocContainer.Resolve<IBigService>('one');
    res := bigservice.Sum(2,2);
  end;
  if bigservice.NumOperations = times * executions then meInfo.Lines.Add('Test3: Constructor injection as Singleton test ok')
    else meInfo.Lines.Add('Test3: Constructor injection as Singleton test error');
  meInfo.Lines.Add(Format('BigService.Sum = %d (calls: %d to BigService / calls: %d to SumService (as singleton))',[res,bigservice.NumOperations,bigservice.sumservice.NumOperations]));

  //test4: constructor injection as transient
  for i := 1 to times do
  begin
    bigservice := iocContainer.Resolve<IBigService>('other');
    res := bigservice.Mult(2,4);
  end;
  if bigservice.NumOperations = 1 then meInfo.Lines.Add('Test4: Constructor injection as Transient test ok')
    else meInfo.Lines.Add('Test4: Constructor injection as Transient test error');
  meInfo.Lines.Add(Format('BigService.Mult = %d (calls: %d to BigService / calls: %d to MultService (as transient))',[res,bigservice.NumOperations,bigservice.multservice.NumOperations]));

  //test5: class instance injection as singleton
  for i := 1 to times do
  begin
    divideservice := iocContainer.Resolve<TDivideService>('one');
    res := divideservice.Divide(100,2);
  end;
  if divideservice.NumOperations = times * executions then meInfo.Lines.Add('Test5: Class instance injection as Singleton test ok')
    else meInfo.Lines.Add('Test5: Class instance injection as Singleton test error');
  meInfo.Lines.Add(Format('DivideService.Divide = %d (calls: %d)',[res,divideservice.NumOperations]));

  //test6: class instance injection as transient
  for i := 1 to times do
  begin
    divideservice := iocContainer.Resolve<TDivideService>('other');
    res := divideservice.Divide(100,2);
    numoperations := divideservice.NumOperations;
    //transient instances must be manual free
    divideservice.Free;
  end;
  if numoperations = 1 then meInfo.Lines.Add('Test6: Class instance injection as Transient test ok')
    else meInfo.Lines.Add('Test6: Class instance injection as Transient test error');
  meInfo.Lines.Add(Format('DivideService.Divide = %d (calls: %d)',[res,numoperations]));

  meInfo.Lines.Add('Test finished');
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  iocContainer.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  iocContainer := TIocContainer.Create;
    iocContainer.RegisterType<ISumService,TSumService>.AsSingleTon.DelegateTo(function : TSumService
                                                                              begin
                                                                                Result := TSumService.Create;
                                                                              end);
    iocContainer.RegisterType<IMultService,TMultService>.AsTransient;
    iocContainer.RegisterType<IBigService,TBigService>('one').AsSingleTon;
    iocContainer.RegisterType<IBigService,TBigService>('other').AsTransient;

    iocContainer.RegisterInstance<TDivideService>('one').AsSingleton.DelegateTo(function : TDivideService
                                                                                begin
                                                                                  Result := TDivideService.Create(True);
                                                                                end);
    iocContainer.RegisterInstance<TDivideService>('other').AsTransient.DelegateTo(function : TDivideService
                                                                                begin
                                                                                  Result := TDivideService.Create(True);
                                                                                end);
end;

end.
