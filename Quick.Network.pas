{ ***************************************************************************

  Copyright (c) 2015-2018 Kike Pérez

  Unit        : Quick.Network
  Description : Network related functions
  Author      : Kike Pérez
  Version     : 1.4
  Created     : 11/07/2017
  Modified    : 07/04/2018

  This file is part of QuickLib: https://github.com/exilon/QuickLib

 ***************************************************************************

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

 *************************************************************************** }

unit Quick.Network;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  Math;

  function IntToIPv4(IPv4: LongWord): string;
  function IPv4ToInt(const IPv4: string) : LongWord;
  function IPv4ToIntReverse(const IPv4: string) : LongWord;
  procedure CIDRToRange(CIDR : string; out MinIP,MaxIP : string); overload;
  procedure CIDRToRange(CIDR : string; out MinIP,MaxIP : LongWord); overload;
  procedure GetIPRange(const cIP, cMask : string; out LowIP, HighIP : string);

implementation

function IntToIPv4(IPv4: LongWord): string;
var
  Retvar : string;
  iSeg,iShift,
  i, iMask : LongWord;
begin
  Retvar := '';
  iShift := 24;
  iMask := $FF000000;
  for i := 1 to 4 do
  begin
    iSeg := (IPv4 and iMask) shr iShift;
    Retvar := Retvar + IntToStr(iSeg);
    if i < 4 then Retvar := Retvar + '.';
    iMask := iMask shr 8;
    dec(iShift,8);
  end;
  Result := Retvar;
end;

function IPv4ToInt(const IPv4: string) : LongWord;
var
  S : TStrings;
begin
  S := TStringList.Create;
  try
    S.Delimiter := '.';
    S.DelimitedText := IPv4;
    if S.Count <> 4 then raise Exception.Create('Invalid IP4 Address string');
    Result := (StrToInt(S[0]) shl 24) + (StrToInt(S[1]) shl 16) + (StrToInt(S[2]) shl 8) + StrToInt(S[3]);
  finally
    S.Free;
  end;
end;

function IPv4ToIntReverse(const IPv4: string) : LongWord;
var
  S : TStrings;
begin
  S := TStringList.Create;
  try
    S.Delimiter := '.';
    S.DelimitedText := IPv4;
    if S.Count <> 4 then raise Exception.Create('Invalid IP4 Address string');
    Result := (StrToInt(S[3]) shl 24) + (StrToInt(S[2]) shl 16) + (StrToInt(S[1]) shl 8) + StrToInt(S[0]);
  finally
    S.Free;
  end;
end;

procedure CIDRToRange(CIDR : string; out MinIP,MaxIP : string);
var
  Mask : Integer;
begin
  Mask := StrToInt(Copy(CIDR,Pos('/',CIDR)+1,CIDR.Length));
  CIDR := Copy(CIDR,0,Pos('/',CIDR)-1);
  MinIP := IntToIPv4((IPv4ToInt(CIDR)) and ((-1 shl (32 - Mask))));
  MaxIP := IntToIPv4((IPv4ToInt(MinIP)) + Round(Power(2,(32 - Mask))) - 1);
end;

procedure CIDRToRange(CIDR : string; out MinIP,MaxIP : LongWord);
var
  Mask : Integer;
  aux : string;
begin
  Mask := StrToInt(Copy(CIDR,Pos('/',CIDR)+1,CIDR.Length));
  CIDR := Copy(CIDR,0,Pos('/',CIDR)-1);
  aux := IntToIPv4((IPv4ToInt(CIDR)) and ((-1 shl (32 - Mask))));
  MinIP := IPv4ToInt(aux);
  aux := IntToIPv4((IPv4ToInt(aux)) + Round(Power(2,(32 - Mask))) - 1);
  MaxIP := IPv4ToInt(aux);
end;

procedure GetIPRange(const cIP, cMask : string; out LowIP, HighIP : string);
begin
  try                      
    LowIP := IntToIPv4((IPv4ToInt(cIP) and (IPv4ToInt(cMask)) + 1));
    HighIP := IntToIPv4(IPv4ToInt(cIP) or not(IPv4ToInt(cMask))-1);
  except
    on E : Exception do raise Exception.Create(Format('GetIPRange error: %s',[e.Message]));
  end;
end;

end.
