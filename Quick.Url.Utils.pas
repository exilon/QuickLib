{ ***************************************************************************

  Copyright (c) 2016-2021 Kike Pérez

  Unit        : Quick.Url.Utils
  Description : Common Url utils
  Author      : Kike Pérez
  Version     : 2.0
  Created     : 17/03/2021
  Modified    : 17/03/2021

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

unit Quick.Url.Utils;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  {$IFDEF NOTUSEINDY}
  System.NetEncoding,
  {$ELSE}
  IdURI,
  {$ENDIF}
  Quick.Commons;

type
  TUrlUtils = class
    class function GetProtocol(const aUrl : string) : string;
    class function GetHost(const aUrl : string) : string;
    class function GetPath(const aUrl : string) : string;
    class function GetQuery(const aUrl : string) : string;
    class function RemoveProtocol(const aUrl : string) : string;
    class function RemoveQuery(const aUrl : string) : string;
    class function EncodeUrl(const aUrl : string) : string;
  end;

implementation

{ TUrlUtils }

class function TUrlUtils.EncodeUrl(const aUrl: string): string;
{$IFDEF NOTUSEINDY}
const
  RestUnsafeChars: TURLEncoding.TUnsafeChars = [Ord('"'), Ord(''''), Ord(':'), Ord(';'), Ord('<'), Ord('='), Ord('>'),
    Ord('@'), Ord('['), Ord(']'), Ord('^'), Ord('`'), Ord('{'), Ord('}'), Ord('|'), Ord('\'), Ord('?'), Ord('#'),
    Ord('&'), Ord('!'), Ord('$'), Ord('('), Ord(')'), Ord(','), Ord('~'), Ord(' '), Ord('*'), Ord('+')];
{$ENDIF}
var
  proto : string;
  path : string;
  query : string;
begin
  {$IFDEF NOTUSEINDY}
  Result := TNetEncoding.URL.Encode(aUrl);
  //Result := StringReplace(aUrl,' ','%20',[rfReplaceAll]);
  proto := UrlGetProtocol(aUrl);
  if not proto.IsEmpty then proto := proto + '://';

  path := UrlGetPath(aUrl);
  path := TNetEncoding.Url.EncodePath(path);


  query := UrlGetQuery(aUrl);
  query := TNetEncoding.Url.EncodeQuery(query);
  if not query.IsEmpty then query := '?' + query;

  Result := proto + UrlGetHost(aUrl) + path + query;
  {$ELSE}
  Result := TIdURI.URLEncode(aUrl);
  {$ENDIF}
end;

class function TUrlUtils.GetProtocol(const aUrl: string): string;
begin
  Result := UrlGetProtocol(aUrl);
end;

class function TUrlUtils.GetHost(const aUrl: string): string;
begin
  Result := UrlGetHost(aUrl);
end;

class function TUrlUtils.GetPath(const aUrl: string): string;
begin
  Result := UrlGetPath(aUrl);
end;

class function TUrlUtils.GetQuery(const aUrl: string): string;
begin
  Result := UrlGetQuery(aUrl);
end;

class function TUrlUtils.RemoveProtocol(const aUrl: string): string;
begin
  Result := UrlRemoveProtocol(aUrl);
end;

class function TUrlUtils.RemoveQuery(const aUrl: string): string;
begin
  Result := UrlRemoveQuery(aUrl);
end;

end.
