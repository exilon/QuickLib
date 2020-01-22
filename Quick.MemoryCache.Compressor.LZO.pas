{ ***************************************************************************

  Copyright (c) 2016-2019 Kike P�rez

  Unit        : Quick.MemoryCache.Compressor.LZO
  Description : Compress Cache data
  Author      : Kike P�rez
  Version     : 1.0
  Created     : 15/07/2019
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

unit Quick.MemoryCache.Compressor.LZO;

{$i QuickLib.inc}

interface

uses
  Quick.MemoryCache.Types,
  Quick.Compression.LZO;

type

  TCacheCompressorLZO = class(TInterfacedObject,ICacheCompressor)
  public
    function Compress(const aValue : string) : string;
    function Decompress(const aValue : string) : string;
  end;

implementation

{ TCacheCompresorLZO }

function TCacheCompressorLZO.Compress(const aValue: string): string;
var
  lzo : TLZOCompressor;
begin
  lzo := TLZOCompressor.Create;
  try
    Result := lzo.Compress(aValue);
  finally
    lzo.Free;
  end;
end;

function TCacheCompressorLZO.Decompress(const aValue: string): string;
var
  lzo : TLZOCompressor;
begin
  lzo := TLZOCompressor.Create;
  try
    Result := lzo.Decompress(aValue);
  finally
    lzo.Free;
  end;
end;

end.
