unit Quick.OAuth.Utils;

interface

type
  TRequestMethod = (rmGET, rmPOST);

function EncodeURL (const aURL: string): string;
procedure OpenURL (const aURL: string);

function GetDomain (const aURL: string): string;
function GetPort (const aURL: string): integer;

function GetMethodFromRequest (const aRequest: string): TRequestMethod;
function GetCleanRequest (const aRequest: string): string;


implementation

uses
  {$IFDEF MSWINDOWS}
  WinApi.ShellAPI,
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils,
  System.Types, System.Classes, System.StrUtils;

{$I QuickLib.INC}

function EncodeURL (const aURL: string): string;
var
  bArray: TBytes;
  c: Char;
  b: byte;
begin
  result:='';
  bArray := TEncoding.UTF8.GetBytes(aURL);
  for b in bArray do
  begin
    c := Chr(b);
    case c of
      'A'..'Z',
      'a'..'z',
      '0'..'9',
           '-',
           '_',
           '.': result := result + c
    else
      result:= result + '%' + IntToHex(Ord(b),2);
    end;
  end;
end;

procedure OpenURL (const aURL: string);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0,PChar('open'),PChar(aURL),PChar(''),PChar(''), SW_NORMAL);
  {$ELSE}
    raise Exception.Create('OpenURL not implemented yet');
  {$ENDIF}
end;

function GetDomain (const aURL: string): string;
{$IFDEF DELPHIXE3_UP}
var
  parts: TStringDynArray;
begin
  result:=aURL;
  parts:=aURL.Split([':']);
  if Length(parts) > 1 then
    result:=parts[1].Replace('/', '');
{$ELSE}
begin
  raise Exception.Create('Not implemented yet');
{$ENDIF}
end;

function GetPort (const aURL: string): integer;
{$IFDEF DELPHIXE3_UP}
var
  parts: TStringDynArray;
begin
  result:=80;
  parts:=aURL.Split([':']);
  if Length(parts) > 1 then
    TryStrToInt(parts[High(parts)].Replace('/', ''), result);
{$ELSE}
begin
  raise Exception.Create('Not implemented yet');
{$ENDIF}
end;

function GetMethodFromRequest (const aRequest: string): TRequestMethod;
begin
  result:=rmGET;
  if aRequest.Trim = '' then
    Exit;
  case IndexStr(aRequest.Split([' '])[0].ToUpper, ['GET', 'POST']) of
    0: result:=rmGET;
    1: result:=rmPOST;
  end;
end;

function GetCleanRequest (const aRequest: string): string;
var
  parts: TStringDynArray;
begin
  result:=aRequest;
  if aRequest.Trim = '' then
    Exit;
  parts:=aRequest.Split([' ']);
  if Length(parts) > 1 then
    result:=parts[1];
end;

end.
