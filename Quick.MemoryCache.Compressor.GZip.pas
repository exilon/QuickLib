{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.MemoryCache.Compressor.GZip
  Description : Compress Cache data
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 14/07/2019
  Modified    : 15/09/2019

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

unit Quick.MemoryCache.Compressor.GZip;

{$i QuickLib.inc}

interface

uses
  Quick.Compression,
  Quick.MemoryCache.Types;

type

  TCacheCompressorGZip = class(TInterfacedObject,ICacheCompressor)
  public
    function Compress(const aValue : string) : string;
    function Decompress(const aValue : string) : string;
  end;

implementation

{ TCacheCompresorGZip }

function TCacheCompressorGZip.Compress(const aValue: string): string;
begin
  Result := CompressGZipString(aValue,TCompressionLevel.zcFastest);
end;

function TCacheCompressorGZip.Decompress(const aValue: string): string;
begin
  Result := DeCompressGZipString(aValue);
end;

end.
