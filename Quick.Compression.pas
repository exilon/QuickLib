{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.Compression
  Description : Compression functions
  Author      : Kike Pérez
  Version     : 1.2
  Created     : 14/08/2018
  Modified    : 05/09/2019

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
unit Quick.Compression;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  ZLib,
  System.NetEncoding;

type

  TCompressionLevel = TZCompressionLevel;

  function CompressString_Deprecated(const aStr : string; aCompressionLevel : TCompressionLevel = zcDefault) : string;
  function DecompressString_Deprecated(const aStr: string) : string;
  function CompressString(const aStr : string; aCompressionLevel : TCompressionLevel = zcDefault) : string;
  function DecompressString(const aStr: string) : string;
  function CompressGZipString(const aStr : string; aCompressionLevel : TCompressionLevel = zcDefault) : string;
  function DecompressGZipString(const aStr: string) : string;
  function CompressAndEncodeString(const aStr: string; aCompressionLevel : TCompressionLevel = zcDefault): string;
  function DecodeAndDecompressString(const aStr: string): string;

implementation

function CompressString_Deprecated(const aStr : string; aCompressionLevel : TCompressionLevel = zcDefault) : string;
var
  strstream : TStringStream;
  zipstream : TStringStream;
begin
  strstream := TStringStream.Create(aStr,TEncoding.UTF8);
  try
    zipstream := TStringStream.Create('',TEncoding.ANSI);
    try
      ZCompressStream(strstream, zipstream, aCompressionLevel);
      zipstream.Position := 0;
      Result := zipstream.DataString;
    finally
      zipstream.Free;
    end;
  finally
    strstream.Free;
  end;
end;

function DecompressString_Deprecated(const aStr: string) : string;
var
  strstream : TStringStream;
  zipstream : TStringStream;
begin
  zipstream := TStringStream.Create(aStr,TEncoding.ANSI);
  try
    strstream := TStringStream.Create('',TEncoding.UTF8);
    try
      zipstream.Position := 0;
      ZDecompressStream(zipstream,strstream);
      strstream.Position := 0;
      Result := strstream.DataString;
    finally
      strstream.Free;
    end;
  finally
    zipstream.Free;
  end;
end;

function CompressString(const aStr : string; aCompressionLevel : TCompressionLevel = zcDefault) : string;
var
  instream : TStringStream;
  zipstream : TMemoryStream;
  outstream : TStringStream;
begin
  instream := TStringStream.Create(aStr,TEncoding.UTF8);
  try
    zipstream := TMemoryStream.Create;
    try
      ZCompressStream(instream,zipstream,aCompressionLevel);
      zipstream.Position := 0;
      outstream := TStringStream.Create('',TEncoding.UTF8);
      try
        outstream.CopyFrom(zipstream,0);
        Result := string(outstream.DataString);
      finally
        outstream.Free;
      end;
    finally
      zipstream.Free;
    end;
  finally
    instream.Free;
  end;
end;

function DecompressString(const aStr: string) : string;
var
  instream : TStringStream;
  zipstream : TMemoryStream;
  outstream : TStringStream;
begin
  instream := TStringStream.Create(aStr,TEncoding.UTF8);
  try
    zipstream := TStringStream.Create;
    try
      zipstream.CopyFrom(instream,0);
      outstream := TStringStream.Create('',TEncoding.UTF8);
      try
        ZDecompressStream(zipstream,outstream);
        outstream.Position := 0;
        Result := string(outstream.DataString);
      finally
        outstream.Free;
      end;
    finally
      zipstream.Free;
    end;
  finally
    instream.Free;
  end;
end;

function CompressGZipString(const aStr : string; aCompressionLevel : TCompressionLevel = zcDefault) : string;
var
  instream : TStringStream;
  outstream : TStringStream;
  zipstream : TZCompressionStream;
begin
  outstream := TStringStream.Create('',TEncoding.ANSI);
  try
    zipstream := TZCompressionStream.Create(outstream,System.ZLib.TZCompressionLevel(aCompressionLevel),15 + 16);
    try
      instream := TStringStream.Create(aStr,TEncoding.UTF8);
      try
        instream.Position := 0;
        zipstream.CopyFrom(instream,instream.Size);
      finally
        instream.Free;
      end;
    finally
      zipstream.Free;
    end;
    outstream.Position := 0;
    Result := outstream.DataString;
  finally
    outstream.Free;
  end;

end;

function DecompressGZipString(const aStr: string) : string;
var
  outstream : TStringStream;
  instream : TStringStream;
  unzipstream : TZDecompressionStream;
begin
  outstream := TStringStream.Create('',TEncoding.UTF8);
  try
    instream := TStringStream.Create(aStr,TEncoding.ANSI);
    try
      unzipstream := nil;
      try
        unzipstream := TZDecompressionStream.Create(instream,15 + 16);
        outstream.CopyFrom(unzipstream,0);
      finally
        unzipstream.Free;
      end;
    finally
      instream.Free;
    end;
    outstream.Position := 0;
    Result := outstream.DataString;
  finally
    outstream.Free;
  end;
end;

function CompressAndEncodeString(const aStr: string; aCompressionLevel : TCompressionLevel = zcDefault): string;
var
  utf8stream: TStringStream;
  zipstream: TMemoryStream;
  base64stream: TStringStream;
begin
  utf8stream := TStringStream.Create(aStr,TEncoding.UTF8);
  try
    zipstream := TMemoryStream.Create;
    try
      ZCompressStream(utf8stream,zipstream,TZCompressionLevel(aCompressionLevel));
      base64stream := TStringStream.Create('',TEncoding.ASCII);
      try
        zipstream.Position := 0;
        TNetEncoding.Base64.Encode(zipstream,base64stream);
        base64stream.Position := 0;
        Result := string(base64stream.DataString);
      finally
        base64stream.Free;
      end;
    finally
      zipstream.Free;
    end;
  finally
    utf8stream.Free;
  end;
end;

function DecodeAndDecompressString(const aStr: string): string;
var
  utf8stream: TStringStream;
  zipstream: TMemoryStream;
  base64stream: TStringStream;
begin
  base64stream := TStringStream.Create(aStr,TEncoding.ASCII);
  try
    zipstream := TMemoryStream.Create;
    try
      base64stream.Position := 0;
      TNetEncoding.Base64.Decode(base64stream,zipstream);
      zipstream.Position := 0;
      utf8stream := TStringStream.Create('',TEncoding.UTF8);
      try
        ZDecompressStream(zipstream,utf8stream);
        Result := string(utf8stream.DataString);
      finally
        utf8stream.Free;
      end;
    finally
      zipstream.Free;
    end;
  finally
    base64stream.Free;
  end;
end;

end.
