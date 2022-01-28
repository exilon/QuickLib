{ ***************************************************************************

  Copyright (c) 2016-2017 Kike Pérez

  Unit        : Quick.Base64
  Description : Base64 functions
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 08/11/2017
  Modified    : 14/08/2018

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
unit Quick.Base64;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  {$IFDEF DELPHIXE7_UP}
  System.NetEncoding;
  {$ELSE}
  IdCoderMIME,
  IdGlobal;
  {$ENDIF}

function Base64Encode(const Input: string): string;
function Base64Decode(const Input: string): string;
{$IFDEF DELPHIXE7_UP}
function Base64DecodeFromBinary(const Input: string) : string;
function Base64DecodeToBytes(const Input: string) : TBytes;
{$ENDIF}

implementation

function Base64Encode(const Input: string): string;
begin
  {$IFDEF DELPHIXE7_UP}
  Result := TNetEncoding.Base64.Encode(Input);
  {$ELSE}
  Result := TIdEncoderMIME.EncodeString(Input,IndyTextEncoding_OSDefault);
  {$ENDIF}
end;

function Base64Decode(const Input: string): string;
begin
  {$IFDEF DELPHIXE7_UP}
  Result := TNetEncoding.Base64.Decode(Input);
  {$ELSE}
  Result := TIdDecoderMIME.DecodeString(Input,IndyTextEncoding_OSDefault);
  {$ENDIF}
end;

{$IFDEF DELPHIXE7_UP}
function Base64DecodeFromBinary(const Input: string) : string;
var
  b : TBytes;
begin
  b := TNetEncoding.Base64.DecodeStringToBytes(Input);
  Result := TEncoding.ANSI.GetString(b);
end;

function Base64DecodeToBytes(const Input: string) : TBytes;
begin
  Result := TNetEncoding.Base64.DecodeStringToBytes(Input);
end;
{$ENDIF}

end.

