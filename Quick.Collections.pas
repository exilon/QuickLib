{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.Collections
  Description : Generic Collections
  Author      : Kike Pérez
  Version     : 1.2
  Created     : 07/03/2020
  Modified    : 20/03/2020

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

unit Quick.Collections;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  TypInfo,
  System.Types,
  System.Generics.Defaults,
  System.Generics.Collections,
  Quick.Linq;

type

  IListBase<T> = interface
  ['{9A9B2DB9-14E4-49DD-A628-F84F50539F41}']
    function GetList: TArray<T>;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer); overload;
    function GetCount : Integer;
    procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
    function GetEnumerator : TEnumerator<T>;
    function Add(const Value: T): Integer;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload;
    procedure AddRange(const Collection: TEnumerable<T>); overload;
    procedure Insert(Index: Integer; const Value: T);
    procedure InsertRange(Index: Integer; const Values: array of T; Count: Integer); overload;
    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: TEnumerable<T>); overload;
    function Remove(const Value: T): Integer;
    function RemoveItem(const Value: T; Direction: TDirection): Integer;
    procedure Delete(Index: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    function ExtractItem(const Value: T; Direction: TDirection): T;
    function Extract(const Value: T): T;
    function ExtractAt(Index: Integer): T;
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    function First: T;
    function Last: T;
    procedure Clear;
    function Contains(const Value: T): Boolean;
    function IndexOf(const Value: T): Integer;
    function IndexOfItem(const Value: T; Direction: TDirection): Integer;
    function LastIndexOf(const Value: T): Integer;
    procedure Reverse;
    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function BinarySearch(const Item: T; out Index: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean; overload;
    procedure TrimExcess;
    function ToArray: TArray<T>;
    procedure FromList(const aList : TList<T>);
    function ToList : TList<T>;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property List: TArray<T> read GetList;
    function Any : Boolean; overload;
  end;

  IList<T> = interface(IListBase<T>)
  ['{78952BD5-7D15-42BB-ADCB-2F835DF879A0}']
    function Any(const aMatchString : string; aUseRegEx : Boolean) : Boolean; overload;
    function Where(const aMatchString : string; aUseRegEx : Boolean) : ILinqArray<T>; overload;
    function Where(const aWhereClause : string; aWhereValues : array of const) : ILinqQuery<T>; overload;
    function Where(const aWhereClause: string): ILinqQuery<T>; overload;
    {$IFNDEF FPC}
    function Where(aPredicate : TPredicate<T>) : ILinqQuery<T>; overload;
    {$ENDIF}
  end;

  IObjectList<T : class> = interface(IListBase<T>)
  ['{7380847B-9F94-4FB8-8B73-DC8ACAFF1729}']
    function Any(const aWhereClause : string; aValues : array of const) : Boolean; overload;
    function Where(const aWhereClause : string; aWhereValues : array of const) : ILinqQuery<T>; overload;
    function Where(const aWhereClause: string): ILinqQuery<T>; overload;
    function Where(aPredicate : TPredicate<T>): ILinqQuery<T>; overload;
  end;

  TXList<T> = class(TInterfacedObject,IList<T>)
  private type
    arrayofT = array of T;
  private
    fList : TList<T>;
    function GetList: TArray<T>;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer); overload;
    function GetCount : Integer;
    procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
    function GetEnumerator : TEnumerator<T>;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const Value: T): Integer; inline;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload; inline;
    procedure AddRange(const Collection: TEnumerable<T>); overload;
    procedure Insert(Index: Integer; const Value: T); inline;
    procedure InsertRange(Index: Integer; const Values: array of T; Count: Integer); overload;
    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: TEnumerable<T>); overload;
    function Remove(const Value: T): Integer; inline;
    function RemoveItem(const Value: T; Direction: TDirection): Integer; inline;
    procedure Delete(Index: Integer); inline;
    procedure DeleteRange(AIndex, ACount: Integer); inline;
    function ExtractItem(const Value: T; Direction: TDirection): T; inline;
    function Extract(const Value: T): T; inline;
    function ExtractAt(Index: Integer): T; inline;
    procedure Exchange(Index1, Index2: Integer); inline;
    procedure Move(CurIndex, NewIndex: Integer); inline;
    function First: T; inline;
    function Last: T; inline;
    procedure Clear; inline;
    function Contains(const Value: T): Boolean; inline;
    function IndexOf(const Value: T): Integer; inline;
    function IndexOfItem(const Value: T; Direction: TDirection): Integer; inline;
    function LastIndexOf(const Value: T): Integer; inline;
    procedure Reverse; inline;
    procedure Sort; overload; inline;
    procedure Sort(const AComparer: IComparer<T>); overload; inline;
    function BinarySearch(const Item: T; out Index: Integer): Boolean; overload; inline;
    function BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean; overload; inline;
    procedure TrimExcess; inline;
    function ToArray: TArray<T>; inline;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property List: arrayofT read GetList;
    procedure FromList(const aList : TList<T>);
    function ToList : TList<T>;
    function Any : Boolean; overload; virtual;
    function Where(const aWhereClause : string; aWhereValues : array of const) : ILinqQuery<T>; overload;
    function Where(const aWhereClause: string): ILinqQuery<T>; overload;
    function Any(const aMatchString : string; aUseRegEx : Boolean) : Boolean; overload;
    function Where(const aMatchString : string; aUseRegEx : Boolean) : ILinqArray<T>; overload;
    {$IFNDEF FPC}
    function Where(aPredicate : TPredicate<T>) : ILinqQuery<T>; overload;
    {$ENDIF}
  end;

  TXObjectList<T : class> = class(TXList<T>,IObjectList<T>)
  private
    fOwnsObjects : Boolean;
    procedure InternalOnNotify(Sender: TObject; const Item: T; Action: TCollectionNotification);
  public
    constructor Create(aOwnedObjects : Boolean = True);
    destructor Destroy; override;
    function Any(const aWhereClause : string; aValues : array of const) : Boolean; overload;
    function Where(const aWhereClause : string; aWhereValues : array of const) : ILinqQuery<T>; overload;
    function Where(const aWhereClause: string): ILinqQuery<T>; overload;
    function Where(aPredicate : TPredicate<T>): ILinqQuery<T>; overload;
  end;

  ECollectionError = class(Exception);
  ECollectionNotSupported = class(Exception);

implementation



{ TXList<T> }

constructor TXList<T>.Create;
begin
  fList := TList<T>.Create;
end;

destructor TXList<T>.Destroy;
begin
  fList.Free;
  inherited;
end;

function TXList<T>.Add(const Value: T): Integer;
begin
  Result := fList.Add(Value);
end;

procedure TXList<T>.AddRange(const Values: array of T);
begin
  fList.AddRange(Values);
end;

procedure TXList<T>.AddRange(const Collection: IEnumerable<T>);
begin
  fList.AddRange(Collection);
end;

procedure TXList<T>.AddRange(const Collection: TEnumerable<T>);
begin
  fList.AddRange(Collection);
end;

function TXList<T>.Any(const aMatchString: string;
  aUseRegEx: Boolean): Boolean;
begin

end;

function TXList<T>.Any: Boolean;
begin
  Result := fList.Count > 0;
end;

function TXList<T>.BinarySearch(const Item: T; out Index: Integer): Boolean;
begin
  Result := fList.BinarySearch(Item,Index);
end;

function TXList<T>.BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean;
begin
  Result := fList.BinarySearch(Item,Index,AComparer);
end;

procedure TXList<T>.Clear;
begin
  fList.Clear;
end;

function TXList<T>.Contains(const Value: T): Boolean;
begin
  Result := fList.Contains(Value);
end;

procedure TXList<T>.Delete(Index: Integer);
begin
  fList.Delete(Index);
end;

procedure TXList<T>.DeleteRange(AIndex, ACount: Integer);
begin
  fList.DeleteRange(aIndex,aCount);
end;

procedure TXList<T>.Exchange(Index1, Index2: Integer);
begin
  fList.Exchange(Index1,Index2);
end;

function TXList<T>.Extract(const Value: T): T;
begin
  Result := fList.Extract(Value);
end;

function TXList<T>.ExtractAt(Index: Integer): T;
begin
  Result := fList.ExtractAt(Index);
end;

function TXList<T>.ExtractItem(const Value: T; Direction: TDirection): T;
begin
  Result := fList.ExtractItem(Value,Direction);
end;

function TXList<T>.First: T;
begin
  Result := fList.First;
end;

procedure TXList<T>.FromList(const aList: TList<T>);
var
  value : T;
begin
  for value in aList do fList.Add(value);
end;

function TXList<T>.GetCapacity: Integer;
begin
  Result := fList.Capacity;
end;

function TXList<T>.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TXList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := fList.GetEnumerator;
end;

function TXList<T>.GetItem(Index: Integer): T;
begin
  Result := fList.Items[Index];
end;

function TXList<T>.GetList: TArray<T>;
begin
  Result := fList.ToArray;
end;

function TXList<T>.IndexOf(const Value: T): Integer;
begin
  Result := fList.IndexOf(Value);
end;

function TXList<T>.IndexOfItem(const Value: T; Direction: TDirection): Integer;
begin
  Result := fList.IndexOfItem(Value,Direction);
end;

procedure TXList<T>.Insert(Index: Integer; const Value: T);
begin
  fList.Insert(Index,Value);
end;

procedure TXList<T>.InsertRange(Index: Integer; const Collection: IEnumerable<T>);
begin
  fList.InsertRange(Index,Collection);
end;

procedure TXList<T>.InsertRange(Index: Integer; const Collection: TEnumerable<T>);
begin
  fList.InsertRange(index,Collection);
end;

procedure TXList<T>.InsertRange(Index: Integer; const Values: array of T; Count: Integer);
begin
  fList.InsertRange(Index,Values,Count);
end;

procedure TXList<T>.InsertRange(Index: Integer; const Values: array of T);
begin
  fList.InsertRange(index,Values);
end;

function TXList<T>.Last: T;
begin
  Result := fList.Last;
end;

function TXList<T>.LastIndexOf(const Value: T): Integer;
begin
  Result := fList.LastIndexOf(Value);
end;

procedure TXList<T>.Move(CurIndex, NewIndex: Integer);
begin
  fList.Move(CurIndex,NewIndex);
end;

function TXList<T>.Remove(const Value: T): Integer;
begin
  Result := fList.Remove(Value);
end;

function TXList<T>.RemoveItem(const Value: T; Direction: TDirection): Integer;
begin
  Result := fList.RemoveItem(Value,Direction);
end;

procedure TXList<T>.Reverse;
begin
  fList.Reverse;
end;

procedure TXList<T>.SetCapacity(Value: Integer);
begin
  fList.Capacity := Value;
end;

procedure TXList<T>.SetCount(Value: Integer);
begin
  fList.Count := Value;
end;

procedure TXList<T>.SetItem(Index: Integer; const Value: T);
begin
  fList.Items[Index] := Value;
end;

procedure TXList<T>.Sort(const AComparer: IComparer<T>);
begin
  fList.Sort(AComparer);
end;

procedure TXList<T>.Sort;
begin
  fList.Sort;
end;

function TXList<T>.ToArray: TArray<T>;
begin
  Result := fList.ToArray;
end;

function TXList<T>.ToList: TList<T>;
var
  value : T;
begin
  Result := TList<T>.Create;
  for value in fList do Result.Add(value);    
end;

procedure TXList<T>.TrimExcess;
begin
  fList.TrimExcess;
end;

function TXList<T>.Where(const aMatchString: string; aUseRegEx: Boolean): ILinqArray<T>;
begin
  Result := TLinqArray<T>.Create(fList.ToArray);
  Result.Where(aMatchString, aUseRegEx);
end;

function TXList<T>.Where(const aWhereClause: string; aWhereValues: array of const): ILinqQuery<T>;
begin
  if PTypeInfo(typeInfo(T)).Kind <> tkClass then raise ECollectionNotSupported.Create('TXList<T>.Where only supports classes. Use MatchString overload method instead!');
  Result := TLinqQuery<TObject>.Create(TObjectList<TObject>(Self.fList)).Where(aWhereClause,aWhereValues) as ILinqQuery<T>;
end;

function TXList<T>.Where(const aWhereClause: string): ILinqQuery<T>;
begin
  if PTypeInfo(typeInfo(T)).Kind <> tkClass then raise ECollectionNotSupported.Create('TXList<T>.Where only supports classes. Use MatchString overload method instead!');
  Result := TLinqQuery<TObject>.Create(TObjectList<TObject>(Self.fList)).Where(aWhereClause) as ILinqQuery<T>;
end;

function TXList<T>.Where(aPredicate: TPredicate<T>): ILinqQuery<T>;
begin
  if PTypeInfo(typeInfo(T)).Kind <> tkClass then raise ECollectionNotSupported.Create('TXList<T>.Where only supports classes. Use MatchString overload method instead!');
  Result := TLinqQuery<TObject>.Create(TObjectList<TObject>(Self.fList)).Where(TPredicate<TObject>(aPredicate)) as ILinqQuery<T>;
end;

{ TXObjectList<T> }

function TXObjectList<T>.Any(const aWhereClause: string; aValues: array of const): Boolean;
var
  query : ILinqQuery<T>;
begin
  query := TLinqQuery<T>.Create(Self.fList);
  Result := query.Where(aWhereClause,aValues).Count > 0;
end;

constructor TXObjectList<T>.Create(aOwnedObjects: Boolean);
begin
  inherited Create;
  fOwnsObjects := aOwnedObjects;
  fList.OnNotify := InternalOnNotify;
end;

destructor TXObjectList<T>.Destroy;
begin

  inherited;
end;

procedure TXObjectList<T>.InternalOnNotify(Sender: TObject; const Item: T; Action: TCollectionNotification);
begin
  if (fOwnsObjects) and (Action = TCollectionNotification.cnRemoved) then
  begin
    if Assigned(Item) then Item.DisposeOf;
    //if PTypeInfo(typeInfo(T)).Kind = tkClass then
    //PObject(@Item).DisposeOf;
  end;
end;

function TXObjectList<T>.Where(const aWhereClause: string): ILinqQuery<T>;
begin
    Result := TLinqQuery<T>.Create(Self.fList).Where(aWhereClause);
end;

function TXObjectList<T>.Where(const aWhereClause: string; aWhereValues: array of const): ILinqQuery<T>;
begin
  Result := TLinqQuery<T>.Create(Self.fList).Where(aWhereClause,aWhereValues);
end;

function TXObjectList<T>.Where(aPredicate: TPredicate<T>): ILinqQuery<T>;
begin
  Result := TLinqQuery<T>.Create(Self.fList).Where(aPredicate);
end;

end.
