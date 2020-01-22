{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.HttpServer.Intf.Cache
  Description : Http Server Cache Interface
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 12/10/2019
  Modified    : 27/10/2019

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

unit Quick.Cache.Intf;

{$i QuickLib.inc}

interface

type
  ICache = interface
  ['{CF5061E1-BEAC-4CCC-8469-5020968116B9}']
    function GetCompression: Boolean;
    procedure SetCompression(const Value: Boolean);
    function GetCachedObjects: Integer;
    function GetCacheSize: Integer;
    property Compression : Boolean read GetCompression write SetCompression;
    property CachedObjects : Integer read GetCachedObjects;
    property CacheSize : Integer read GetCacheSize;
    procedure SetValue(const aKey : string; aValue : TObject; aExpirationMilliseconds : Integer = 0); overload;
    procedure SetValue(const aKey : string; aValue : TObject; aExpirationDate : TDateTime); overload;
    procedure SetValue(const aKey, aValue : string; aExpirationMilliseconds : Integer = 0); overload;
    procedure SetValue(const aKey, aValue : string; aExpirationDate : TDateTime); overload;
    procedure SetValue(const aKey : string; aValue : TArray<string>; aExpirationMilliseconds : Integer = 0); overload;
    procedure SetValue(const aKey : string; aValue : TArray<string>; aExpirationDate : TDateTime); overload;
    procedure SetValue(const aKey : string; aValue : TArray<TObject>; aExpirationMilliseconds : Integer = 0); overload;
    procedure SetValue(const aKey : string; aValue : TArray<TObject>; aExpirationDate : TDateTime); overload;
    function GetValue(const aKey : string) : string; overload;
    function TryGetValue(const aKey : string; aValue : TObject) : Boolean; overload;
    function TryGetValue(const aKey : string; out aValue : string) : Boolean; overload;
    function TryGetValue(const aKey : string; out aValue : TArray<string>) : Boolean; overload;
    function TryGetValue(const aKey : string; out aValue : TArray<TObject>) : Boolean; overload;
    procedure RemoveValue(const aKey : string);
    procedure Flush;
  end;

  IMemoryCache = ICache;

implementation

end.
