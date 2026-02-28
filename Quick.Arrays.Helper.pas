{ ***************************************************************************

  Copyright (c) 2016-2025 Kike Pérez

  Unit        : Quick.Arrays.Helper
  Description : Array helpers
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 24/03/2019
  Modified    : 10/05/2025

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

unit Quick.Arrays.Helper;

{$i QuickLib.inc}

interface

uses
  Generics.Defaults, System.SysUtils;

type

  TArrayHelper<T> = class
  public
    class function Count(var aArray : TArray<T>) : Integer;
    class procedure Add(var aArray : TArray<T>; aItem : T); static;
    class procedure Insert(var aArray : TArray<T>; aItem : T; aIndex : Integer); static;
    class procedure Remove(var aArray : TArray<T>; aIndex : Integer); static;
    class procedure Clear(var aArray : TArray<T>);
    class function Contains(var aArray : TArray<T>; aItem : T) : Boolean;
    class function IndexOf(var aArray : TArray<T>; aItem : T) : Integer;
  end;

  {$IFDEF FPC}
  TStringArray = TArray<string>;
  {$ENDIF}

  TStringArrayHelper = record Helper for {$IFNDEF FPC}TArray<string>{$ELSE}TStringArray{$ENDIF}
    function Count : Integer;
    procedure Add(const aValue : string);
    procedure Insert(const aValue : string; aIndex : Integer);
    procedure Remove(aIndex : Integer);
    procedure Clear;
    function Contains(const aItem : string) : Boolean;
    function IndexOf(const aItem : string) : Integer;
    procedure Sort;
  end;

  {$IFDEF FPC}
  TIntegerArray = TArray<Integer>;
  {$ENDIF}

  TIntegerArrayHelper = record Helper for {$IFNDEF FPC}TArray<Integer>{$ELSE}TIntegerArray{$ENDIF}
    function Count : Integer;
    procedure Add(aValue : Integer);
    procedure Insert(const aValue : Integer; aIndex : Integer);
    procedure Remove(aIndex : Integer);
    procedure Clear;
    function Contains(aItem : Integer) : Boolean;
    function IndexOf(aItem : Integer) : Integer;
    procedure Sort;
  end;


implementation


{  TArray  }

class function TArrayHelper<T>.Count(var aArray : TArray<T>) : Integer;
begin
  Result := High(aArray)+1;
end;

class procedure TArrayHelper<T>.Add(var aArray : TArray<T>; aItem : T);
begin
  SetLength(aArray, Length(aArray) + 1);
  aArray[High(aArray)] := aItem;
end;

class procedure TArrayHelper<T>.Remove(var aArray : TArray<T>; aIndex : Integer);
begin
  {$IFDEF DELPHIXE7_UP}
  System.Delete(aArray,aIndex,1);
  {$ELSE}
  TArrayUtil<T>.Delete(aArray,aIndex);
  {$ENDIF}
end;

class procedure TArrayHelper<T>.Insert(var aArray : TArray<T>; aItem : T; aIndex : Integer);
begin
  System.Insert(aItem,aArray,aIndex);
end;

class function TArrayHelper<T>.Contains(var aArray : TArray<T>; aItem : T) : Boolean;
var
  icomparer : IEqualityComparer<T>;
  i : Integer;
begin
  Result := False;
  icomparer := TEqualityComparer<T>.Default;
  for i := Low(aArray) to High(aArray) do
  begin
    if icomparer.Equals(aArray[i],aItem) then Exit(True);
  end;
end;

class function TArrayHelper<T>.IndexOf(var aArray : TArray<T>; aItem : T) : Integer;
var
  icomparer : IEqualityComparer<T>;
  i : Integer;
begin
  icomparer := TEqualityComparer<T>.Default;
  for i := Low(aArray) to High(aArray) do
  begin
    if icomparer.Equals(aArray[i],aItem) then Exit(i);
  end;
  Result := -1;
end;

class procedure TArrayHelper<T>.Clear(var aArray : TArray<T>);
begin
  SetLength(aArray, 0);
end;

{  TStringArrayHelper  }

function TStringArrayHelper.Count : Integer;
begin
  Result := TArrayHelper<string>.Count(Self);
end;

procedure TStringArrayHelper.Add(const aValue : string);
begin
  TArrayHelper<string>.Add(Self,aValue);
end;

procedure TStringArrayHelper.Insert(const aValue : string; aIndex : Integer);
begin
  TArrayHelper<string>.Insert(Self,aValue,aIndex);
end;

procedure TStringArrayHelper.Remove(aIndex : Integer);
begin
  TArrayHelper<string>.Remove(Self,aIndex);
end;

function TStringArrayHelper.Contains(const aItem : string) : Boolean;
begin
  Result := TArrayHelper<string>.Contains(Self,aItem);
end;

function TStringArrayHelper.IndexOf(const aItem : string) : Integer;
begin
  Result := TArrayHelper<string>.IndexOf(Self,aItem);
end;

procedure TStringArrayHelper.Clear;
begin
  TArrayHelper<string>.Clear(Self);
end;

procedure TStringArrayHelper.Sort;
var
  i, j : Integer;
  tmp: string;
begin
  for i := Low(Self) to High(Self) - 1 do
    for j := i + 1 to High(Self) do
      if Self[i] > Self[j] then
      begin
        tmp := Self[i];
        Self[i] := Self[j];
        Self[j] := tmp;
      end;
end;

{  TIntegerArrayHelper  }

function TIntegerArrayHelper.Count : Integer;
begin
  Result := TArrayHelper<Integer>.Count(Self);
end;

procedure TIntegerArrayHelper.Add(aValue : Integer);
begin
  TArrayHelper<Integer>.Add(Self,aValue);
end;

procedure TIntegerArrayHelper.Insert(const aValue : Integer; aIndex : Integer);
begin
  TArrayHelper<Integer>.Insert(Self,aValue,aIndex);
end;

procedure TIntegerArrayHelper.Remove(aIndex : Integer);
begin
  TArrayHelper<Integer>.Remove(Self,aIndex);
end;

procedure TIntegerArrayHelper.Sort;
var
  i, j : Integer;
  tmp: Integer;
begin
  for i := Low(Self) to High(Self) - 1 do
    for j := i + 1 to High(Self) do
      if Self[i] > Self[j] then
      begin
        tmp := Self[i];
        Self[i] := Self[j];
        Self[j] := tmp;
      end;
end;

function TIntegerArrayHelper.Contains(aItem : Integer) : Boolean;
begin
  Result := TArrayHelper<Integer>.Contains(Self,aItem);
end;

function TIntegerArrayHelper.IndexOf(aItem : Integer) : Integer;
begin
  Result := TArrayHelper<Integer>.IndexOf(Self,aItem);
end;

procedure TIntegerArrayHelper.Clear;
begin
  TArrayHelper<Integer>.Clear(Self);
end;

end.
