unit Quick.SyncObjs.Linux.Compatibility;

{i$ QuickLib.inc}

interface

uses
  SyncObjs;

type

  TRTLCriticalSection = TCriticalSection;

  procedure EnterCriticalSection(CS : TCriticalSection);
  procedure LeaveCriticalSection(CS : TCriticalSection);
  procedure InitializeCriticalSection(var CS : TCriticalSection);
  procedure DeleteCriticalSection(CS : TCriticalSection);

implementation

procedure EnterCriticalSection(CS : TCriticalSection);
begin
  CS.Enter;
end;

procedure LeaveCriticalSection(CS : TCriticalSection);
begin
  CS.Leave;
end;

procedure InitializeCriticalSection(var CS : TCriticalSection);
begin
  CS := TCriticalSection.Create;
end;

procedure DeleteCriticalSection(CS : TCriticalSection);
begin
  CS.Free;
end;

end.
