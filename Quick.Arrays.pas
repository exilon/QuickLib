{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.Arrays
  Description : Multifuntional Arrays
  Author      : Kike Pérez
  Version     : 1.2
  Created     : 24/03/2019
  Modified    : 16/10/2019

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

unit Quick.Arrays;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  Generics.Defaults,
  Generics.Collections,
  Quick.Value;

type

  TXArray<T> = record
  type
    TEnumerator = class(Generics.Collections.TEnumerator<T>)
      private
        fArray : ^TArray<T>;
        fIndex : Integer;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(var aArray: TArray<T>);
      end;
  private type
    arrayofT = array of T;
    ParrayofT = ^arrayofT;
  private
    fArray : TArray<T>;
    function GetItem(Index : Integer) : T;
    procedure SetItem(Index : Integer; const aValue : T);
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure SetCapacity(const Value: Integer);
    function GetPArray: ParrayofT;
  public
    function GetEnumerator: TEnumerator<T>;
    property AsArray : TArray<T> read fArray;
    property Items[Index : Integer] : T read GetItem write SetItem; default;
    property Count : Integer read GetCount;
    property Capacity : Integer read GetCapacity write SetCapacity;
    function Add(aValue : T) : Integer;
    procedure Insert(aItem : T; aIndex : Integer);
    procedure Delete(aIndex : Integer);
    procedure Remove(aItem : T);
    function Contains(aItem : T) : Boolean;
    function IndexOf(aItem : T) : Integer;
    property PArray : ParrayofT read GetPArray;
    class operator Implicit(const Value : TxArray<T>) : TArray<T>;
    class operator Implicit(const Value : TArray<T>) : TxArray<T>;
  end;

  TPair = record
    Name : string;
    Value : string;
    constructor Create(const aName, aValue : string);
  end;

  TPairArray = TArray<TPair>;

  PPairArray = ^TPairArray;

  TPairXArray = TXArray<TPair>;

  TFlexArray = TXArray<TFlexValue>;

  TFlexPairArray = TArray<TFlexPair>;

  TPairArrayHelper = record helper for TPairArray
  public
    function GetValue(const aName : string) : string;
    function GetPair(const aName : string) : TPair;
    function Add(aPair : TPair) : Integer; overload;
    function Add(const aName, aValue : string) : Integer; overload;
    procedure AddOrUpdate(const aName, aValue : string);
    function Exists(const aName : string) : Boolean;
    function Remove(const aName : string) : Boolean;
    function Count : Integer;
    property Items[const aName : string] : string read GetValue write AddOrUpdate;
  end;

  TFlexPairArrayHelper = record helper for TFlexPairArray
  public
    function GetValue(const aName : string) : TFlexValue;
    function GetPair(const aName : string) : TFlexPair;
    function Add(aFlexPair : TFlexPair) : Integer; overload;
    function Add(const aName: string; aValue : TFlexValue): Integer; overload;
    procedure AddOrUpdate(const aName : string; aValue : TFlexValue);
    function Exists(const aName : string) : Boolean;
    function Remove(const aName : string) : Boolean;
    function Count : Integer;
    property Items[const aName : string] : TFlexValue read GetValue write AddOrUpdate;
  end;

implementation


{  TxArray  }

function TxArray<T>.GetItem(Index : Integer) : T;
begin
  Result := fArray[Index];
end;

function TXArray<T>.GetPArray: ParrayofT;
begin
  Pointer(Result) := fArray;
end;

procedure TXArray<T>.SetCapacity(const Value: Integer);
begin
  if Value = High(fArray) then Exit;

  if Value < 0 then SetLength(fArray,1)
    else SetLength(fArray, Value);
end;

procedure TxArray<T>.SetItem(Index : Integer; const aValue : T);
begin
  fArray[Index] := aValue;
end;

function TXArray<T>.GetCapacity: Integer;
begin
  Result := High(fArray) + 1;
end;

function TXArray<T>.GetCount: Integer;
begin
  Result := High(fArray) + 1;
end;

function TxArray<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := TEnumerator.Create(fArray);
end;

function TxArray<T>.Add(aValue : T) : Integer;
begin
  SetLength(fArray, Length(fArray) + 1);
  fArray[High(fArray)] := aValue;
  Result := High(fArray);
end;

procedure TxArray<T>.Delete(aIndex : Integer);
begin
  System.Delete(fArray,aIndex,1);
end;

procedure TxArray<T>.Remove(aItem : T);
var
  nindex : Integer;
begin
  nindex := IndexOf(aItem);
  if nindex > -1 then System.Delete(fArray,nindex,1);
end;

procedure TxArray<T>.Insert(aItem : T; aIndex : Integer);
begin
  System.Insert(aItem,fArray,aIndex);
end;

function TxArray<T>.Contains(aItem : T) : Boolean;
var
  icomparer : IEqualityComparer<T>;
  i : Integer;
begin
  Result := False;
  icomparer := TEqualityComparer<T>.Default;
  for i := Low(fArray) to High(fArray) do
  begin
    if icomparer.Equals(fArray[i],aItem) then Exit(True);
  end;
end;

function TxArray<T>.IndexOf(aItem : T) : Integer;
var
  icomparer : IEqualityComparer<T>;
  i : Integer;
begin
  icomparer := TEqualityComparer<T>.Default;
  for i := Low(fArray) to High(fArray) do
  begin
    if icomparer.Equals(fArray[i],aItem) then Exit(i);
  end;
  Result := -1;
end;

class operator TxArray<T>.Implicit(const Value : TxArray<T>) : TArray<T>;
begin
  Result := Value.fArray;
end;

class operator TXArray<T>.Implicit(const Value: TArray<T>): TxArray<T>;
begin
  Result.fArray := Value;
end;


{ TXArray<T>.TEnumerator }

constructor TXArray<T>.TEnumerator.Create(var aArray: TArray<T>);
begin
  fIndex := -1;
  fArray := @aArray;
end;

function TXArray<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := TArray<T>(fArray^)[fIndex];
end;

function TXArray<T>.TEnumerator.DoMoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex < High(TArray<T>(fArray^))+1;
end;

{ TFlexPairArrayHelper }

function TFlexPairArrayHelper.Add(const aName: string; aValue : TFlexValue): Integer;
begin
  SetLength(Self,Length(Self)+1);
  Self[High(Self)].Name := aName;
  Self[High(Self)].Value := aValue;
  Result := High(Self);
end;

function TFlexPairArrayHelper.Count: Integer;
begin
  Result := High(Self) + 1;
end;

function TFlexPairArrayHelper.Add(aFlexPair: TFlexPair): Integer;
begin
  SetLength(Self,Length(Self)+1);
  Self[High(Self)] := aFlexPair;
  Result := High(Self);
end;

function TFlexPairArrayHelper.Exists(const aName: string): Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i].Name,aName) = 0 then Exit(True)
  end;
end;

function TFlexPairArrayHelper.GetPair(const aName: string): TFlexPair;
var
  i : Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i].Name,aName) = 0 then Exit(Self[i]);
  end;
end;

function TFlexPairArrayHelper.GetValue(const aName: string): TFlexValue;
var
  i : Integer;
begin
  Result.Clear;
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i].Name,aName) = 0 then Exit(Self[i].Value);
  end;
end;

function TFlexPairArrayHelper.Remove(const aName: string): Boolean;
var
  i : Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i].Name,aName) = 0 then
    begin
      System.Delete(Self,i,1);
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure TFlexPairArrayHelper.AddOrUpdate(const aName : string; aValue : TFlexValue);
var
  i : Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i].Name,aName) = 0 then
    begin
      Self[i].Value := aValue;
      Exit;
    end;
  end;
  //if not exists add it
  Self.Add(aName,aValue);
end;

{ TPair }

constructor TPair.Create(const aName, aValue: string);
begin
  Name := aName;
  Value := aValue;
end;

{ TPairArrayHelper }

function TPairArrayHelper.Add(aPair: TPair): Integer;
begin
  SetLength(Self,Length(Self)+1);
  Self[High(Self)] := aPair;
  Result := High(Self);
end;

function TPairArrayHelper.Add(const aName, aValue: string): Integer;
begin
  SetLength(Self,Length(Self)+1);
  Self[High(Self)].Name := aName;
  Self[High(Self)].Value := aValue;
  Result := High(Self);
end;

procedure TPairArrayHelper.AddOrUpdate(const aName, aValue: string);
var
  i : Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i].Name,aName) = 0 then
    begin
      Self[i].Value := aValue;
      Exit;
    end;
  end;
  //if not exists add it
  Self.Add(aName,aValue);
end;

function TPairArrayHelper.Count: Integer;
begin
  Result := High(Self) + 1;
end;

function TPairArrayHelper.Exists(const aName: string): Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i].Name,aName) = 0 then Exit(True)
  end;
end;

function TPairArrayHelper.GetPair(const aName: string): TPair;
var
  i : Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i].Name,aName) = 0 then Exit(Self[i]);
  end;
end;

function TPairArrayHelper.GetValue(const aName: string): string;
var
  i : Integer;
begin
  Result := '';
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i].Name,aName) = 0 then Exit(Self[i].Value);
  end;
end;

function TPairArrayHelper.Remove(const aName: string): Boolean;
var
  i : Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i].Name,aName) = 0 then
    begin
      System.Delete(Self,i,1);
      Exit(True);
    end;
  end;
  Result := False;
end;

end.
