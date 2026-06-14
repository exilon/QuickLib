program QuickLogDemo;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

{$R *.res}

uses
  Quick.Log, Quick.Commons;

begin
  Log := TQuickLog.Create;
  try
    Log.SetLog('.\QuickDemo.log', True, 50);
    Log.Add('QuickLogDemo Start', etTrace);

    Log.Add('Info', etInfo);
    Log.Add('Success', etSuccess);
    Log.Add('Warning', etWarning);
    Log.Add('Error', etError);
    Log.Add('Debug', etDebug);
    Log.Add('Done', etDone);
    Log.Add('Critical', etCritical);
    Log.Add('Exception', etException);

    Log.Add('QuickLogDemo End', etTrace);
  finally
    Log.Free;
  end;
end.
