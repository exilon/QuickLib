{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.MemoryCache.Types
  Description : Memory Cache Types
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

unit Quick.MemoryCache.Types;

{$i QuickLib.inc}

interface

uses
  RTTI;

type

  ICacheEntry = interface
  ['{3158454E-07D5-41A2-A0FA-D3917F6B58C1}']
    function GetCreationDate: TDateTime;
    function GetData: string;
    function GetExpiration: Cardinal;
    function GetExpirationDate: TDateTime;
    procedure SetCreationDate(const Value: TDateTime);
    procedure SetData(const Value: string);
    procedure SetExpirationDate(const Value: TDateTime);
    procedure SetExpiration(aMilliseconds : Cardinal);
    property CreationDate : TDateTime read GetCreationDate write SetCreationDate;
    property Expiration : Cardinal read GetExpiration write SetExpiration;
    property ExpirationDate : TDateTime read GetExpirationDate write SetExpirationDate;
    property Data : string read GetData write SetData;
    function Size : Integer;
    function IsExpired : Boolean;
  end;

  ICacheCompressor = interface
  ['{DE4FB31F-0A0B-49AF-88A6-41689C316DFE}']
    function Compress(const aValue : string) : string;
    function Decompress(const aValue : string) : string;
  end;

  ICacheSerializer = interface
  ['{F26B99AE-5080-4EDB-80CF-508E1A6F9EDE}']
    function Serialize(aObject : TObject) : string; overload;
    function Serialize(aArray : TArray<string>) : string; overload;
    function Serialize(aArray: TArray<TObject>): string; overload;
    procedure Deserialize(const aValue : string; aObject : TObject); overload;
    procedure Deserialize(const aValue : string; var aArray : TArray<string>); overload;
    procedure Deserialize(const aValue : string; var aArray: TArray<TObject>); overload;
  end;

implementation

end.
