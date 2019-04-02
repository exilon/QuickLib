{ ***************************************************************************

  Copyright (c) 2016-2017 Kike Pérez

  Unit        : Quick.Compression
  Description : Compression functions
  Author      : Kike Pérez
  Version     : 1.1
  Created     : 14/08/2018
  Modified    : 20/08/2018

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
  System.SysUtils,
  System.ZLib;

type

  TCompressionLevel = TZCompressionLevel;

  function CompressString(const aStr : string; aLevel : TCompressionLevel = zcDefault) : string;
  function DecompressString(const aStr: string) : string;

implementation

function CompressString(const aStr : string; aLevel : TCompressionLevel = zcDefault) : string;
var
  strstream : TStringStream;
  zipstream : TStringStream;
begin
  strstream := TStringStream.Create(aStr,TEncoding.UTF8);
  try
    zipstream := TStringStream.Create('',TEncoding.ANSI);
    try
      ZCompressStream(strstream, zipstream, aLevel);
      zipstream.Position := 0;
      Result := zipstream.DataString;
    finally
      zipstream.Free;
    end;
  finally
    strstream.Free;
  end;
end;

function DecompressString(const aStr: string) : string;
var
  strstream : TStringStream;
  zipstream : TStringStream;
begin
  zipstream := TStringStream.Create(aStr,TEncoding.ANSI);
  try
    strstream := TStringStream.Create('',TEncoding.UTF8);
    try
      ZDecompressStream(zipstream, strstream);
      strstream.Position := 0;
      Result := strstream.DataString;
    finally
      strstream.Free;
    end;
  finally
    zipstream.Free;
  end;
end;

end.
