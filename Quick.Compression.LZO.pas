{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.Compression.LZO
  Description : LZO Compressor
  Author      : Kike Pérez
  Version     : 1.8
  Created     : 15/09/2019
  Modified    : 22/09/2019

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

unit Quick.Compression.LZO;

{$i QuickLib.inc}

interface

uses
  SysUtils;

const

  {$IFDEF MSWINDOWS}
  LIBPATH = '.\lzo.dll';
  {$ELSE}
  LIBPATH = './lzo.so';
  {$ENDIF}

type

  TLZOCompressor = class
  private
    fWorkMemory : Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    function Version : string;
    function VersionDate : string;
    function Compress(const aUncompressed : string) : string;
    function Decompress(const aCompressed : string) : string;
  end;

  ELZOCompressor = class(Exception);

implementation

function __lzo_init3 : Integer; stdcall; external LIBPATH;
function lzo_version_string : PChar; stdcall; external LIBPATH;
function lzo_version_date : PChar; stdcall; external LIBPATH;
function lzo1x_1_compress(src : Pointer; src_len : LongWord; dest : Pointer; var dest_len : LongWord; wrkmem : Pointer) : Integer; stdcall; external LIBPATH;
function lzo1x_decompress(src : Pointer; src_len : LongWord; dest : Pointer; var dest_len : LongWord; wrkmem : Pointer) : Integer; stdcall; external LIBPATH;


{ TLZOCompressor }

function TLZOCompressor.Compress(const aUncompressed: string): string;
var
  srclen : Integer;
  outlen : LongWord;
  uncompressed : TBytes;
  compressed : TBytes;
  byteslen : array[1.. sizeof(integer)] of byte;
begin
  outlen := 0;
  try
    SetLength(compressed, Round(aUncompressed.Length + aUncompressed.Length / 64 + 16 + 3 + 4));
    uncompressed := TEncoding.UTF8.GetBytes(aUncompressed);
    lzo1x_1_compress(uncompressed, High(uncompressed)+1, compressed, outlen, fWorkMemory);
    Finalize(uncompressed);
    srclen := aUncompressed.Length;
    Move(srclen,byteslen[1], SizeOf(Integer));
    SetLength(compressed,outlen + 4);
    Move(byteslen,compressed[outlen],4);
    Result := TEncoding.ANSI.GetString(compressed,0,outlen + 4);
  except
    on E : Exception do raise ELZOCompressor.CreateFmt('LZO Compression error: %s',[e.Message]);
  end;
end;

constructor TLZOCompressor.Create;
begin
  if __lzo_init3 <> 0 then raise Exception.Create('Initialization LZO-Compressor failed');
  GetMem(fWorkMemory,16384 * 4);
end;

function TLZOCompressor.Decompress(const aCompressed: string): string;
var
  srclen : LongWord;
  dstlen : LongWord;
  uncompressed : TBytes;
  compressed : TBytes;
  i : Integer;
begin
  try
    compressed := TEncoding.ANSI.GetBytes(aCompressed);
    //get src length
    srclen := PLongInt(@compressed[High(compressed)-3])^;
    SetLength(uncompressed,srclen);
    dstlen := 0;
    lzo1x_decompress(compressed, High(compressed) - 4, uncompressed, dstlen, fWorkMemory);
    Result := TEncoding.UTF8.GetString(uncompressed,0,dstlen);
  except
    on E : Exception do raise ELZOCompressor.CreateFmt('LZO Descompression error: %s',[e.Message]);
  end;
end;

destructor TLZOCompressor.Destroy;
begin
  FreeMem(fWorkMemory);
  inherited;
end;

function TLZOCompressor.Version: string;
begin
  Result := string(lzo_version_string);
end;

function TLZOCompressor.VersionDate: string;
begin
  Result := string(lzo_version_date^);
end;

end.
