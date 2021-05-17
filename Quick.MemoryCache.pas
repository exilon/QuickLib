{ ***************************************************************************

  Copyright (c) 2016-2021 Kike Pérez

  Unit        : Quick.MemoryCache
  Description : Cache objects with expiration control
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 14/07/2019
  Modified    : 17/05/2021

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

unit Quick.MemoryCache;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.DateUtils,
  System.TypInfo,
  RTTI,
  Quick.Commons,
  Quick.Value,
  Quick.Threads,
  Quick.Cache.Intf,
  Quick.MemoryCache.Types,
  Quick.MemoryCache.Serializer.Json,
  Quick.MemoryCache.Compressor.GZip;

type

  TCacheFlushedEvent = reference to procedure(aRemovedEntries : Integer);
  TBeginPurgerJobEvent = reference to procedure;
  TEndPurgerJobEvent = reference to procedure(aPurgedEntries : Integer);
  TPurgerJobErrorEvent = reference to procedure(const aErrorMsg : string);

  IMemoryCache<T> = interface
  ['{57927AD7-C993-4C3C-B552-43A39F99E73A}']
    function GetCompression: Boolean;
    procedure SetCompression(const Value: Boolean);
    function GetCachedObjects: Integer;
    function GetCacheSize: Integer;
    procedure SetOnBeginPurgerJob(const Value: TBeginPurgerJobEvent);
    procedure SetOnCacheFlushed(const Value: TCacheFlushedEvent);
    procedure SetOnEndPurgerJob(const Value: TEndPurgerJobEvent);
    procedure SetOnPurgerJobError(const Value: TPurgerJobErrorEvent);
    property Compression : Boolean read GetCompression write SetCompression;
    property CachedObjects : Integer read GetCachedObjects;
    property CacheSize : Integer read GetCacheSize;
    property OnCacheFlushed : TCacheFlushedEvent write SetOnCacheFlushed;
    property OnBeginPurgerJob : TBeginPurgerJobEvent write SetOnBeginPurgerJob;
    property OnEndPurgerJob : TEndPurgerJobEvent write SetOnEndPurgerJob;
    property OnPurgeJobError : TPurgerJobErrorEvent write SetOnPurgerJobError;
    procedure SetValue(const aKey : string; aValue : T; aExpirationMillisecons : Integer = 0); overload;
    procedure SetValue(const aKey : string; aValue : T; aExpirationDate : TDateTime); overload;
    function GetValue(const aKey : string) : T;
    function TryGetValue(const aKey : string; out aValue : T) : Boolean;
    procedure RemoveValue(const aKey : string);
    procedure Refresh(const aKey: string; aExpirationMilliseconds : Integer);
    procedure Flush;
  end;


  IMemoryCache = interface(ICache)
  ['{F109AE78-43D7-4983-B8ED-52A41533EEED}']
    function GetCompression: Boolean;
    procedure SetCompression(const Value: Boolean);
    function GetCachedObjects: Integer;
    function GetCacheSize: Integer;
    procedure SetOnBeginPurgerJob(const Value: TBeginPurgerJobEvent);
    procedure SetOnCacheFlushed(const Value: TCacheFlushedEvent);
    procedure SetOnEndPurgerJob(const Value: TEndPurgerJobEvent);
    procedure SetOnPurgerJobError(const Value: TPurgerJobErrorEvent);
    property Compression : Boolean read GetCompression write SetCompression;
    property CachedObjects : Integer read GetCachedObjects;
    property CacheSize : Integer read GetCacheSize;
    property OnCacheFlushed : TCacheFlushedEvent write SetOnCacheFlushed;
    property OnBeginPurgerJob : TBeginPurgerJobEvent write SetOnBeginPurgerJob;
    property OnEndPurgerJob : TEndPurgerJobEvent write SetOnEndPurgerJob;
    property OnPurgeJobError : TPurgerJobErrorEvent write SetOnPurgerJobError;
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
    procedure Refresh(const aKey: string; aExpirationMilliseconds : Integer);
    procedure Flush;
  end;

  TCacheEntry = class(TInterfacedObject,ICacheEntry)
  private
    fCreationDate : TDateTime;
    fExpiration : Cardinal;
    fExpirationDate : TDateTime;
    fCompression : Boolean;
    fCompressor : ICacheCompressor;
    fData : string;
    fIsCompressed : Boolean;
    function GetCreationDate: TDateTime;
    function GetData: string;
    function GetExpiration: Cardinal;
    procedure SetCreationDate(const Value: TDateTime);
    procedure SetData(const Value: string);
    procedure SetExpiration(aMilliseconds : Cardinal);
    function GetExpirationDate: TDateTime;
    procedure SetExpirationDate(const Value: TDateTime);
  public
    constructor Create(aCompression : Boolean; aCacheCompressor : ICacheCompressor);
    property CreationDate : TDateTime read GetCreationDate write SetCreationDate;
    property Expiration : Cardinal read GetExpiration write SetExpiration;
    property ExpirationDate : TDateTime read GetExpirationDate write SetExpirationDate;
    property Data : string read GetData write SetData;
    function Size : Integer;
    function IsExpired : Boolean;
  end;

  TMemoryCacheBase = class(TInterfacedObject)
  private
    fPurgerInterval : Integer;
    fMaxSize : Integer;
    fCachedObjects : Integer;
    fCacheSize : Integer;
    fCompression : Boolean;
    fLock : TMultiReadExclusiveWriteSynchronizer;
    fCacheJobs : TScheduledTasks;
    fOnCacheFlushed : TCacheFlushedEvent;
    fOnPurgerJobError : TPurgerJobErrorEvent;
    fOnBeginPurgerJob : TBeginPurgerJobEvent;
    fOnEndPurgerJob : TEndPurgerJobEvent;
    procedure CreatePurgerJobs;
    procedure RemoveExpiredCacheEntries; virtual;
    procedure SetPurgerInterval(const Value: Integer);
  protected
    fItems : TDictionary<string,ICacheEntry>;
    fSerializer : ICacheSerializer;
    fCompressor : ICacheCompressor;
    function GetCachedObjects: Integer;
    function GetCacheSize: Integer;
    procedure SetOnBeginPurgerJob(const Value: TBeginPurgerJobEvent);
    procedure SetOnCacheFlushed(const Value: TCacheFlushedEvent);
    procedure SetOnEndPurgerJob(const Value: TEndPurgerJobEvent);
    procedure SetOnPurgerJobError(const Value: TPurgerJobErrorEvent);
    function GetCompression: Boolean;
    procedure SetCompression(const Value: Boolean);
  public
    constructor Create(aPurgerInterval : Integer = 20; aCacheSerializer : ICacheSerializer = nil; aCacheCompressor : ICacheCompressor = nil); virtual;
    destructor Destroy; override;
    property MaxSize : Integer read fMaxSize write fMaxSize;
    property PurgerInterval : Integer read fPurgerInterval;
    property Compression : Boolean read GetCompression write SetCompression;
    property CachedObjects : Integer read GetCachedObjects;
    property CacheSize : Integer read GetCacheSize;
    property OnCacheFlushed : TCacheFlushedEvent read fOnCacheFlushed write SetOnCacheFlushed;
    property OnBeginPurgerJob : TBeginPurgerJobEvent read fOnBeginPurgerJob write SetOnBeginPurgerJob;
    property OnEndPurgerJob : TEndPurgerJobEvent read fOnEndPurgerJob write SetOnEndPurgerJob;
    property OnPurgeJobError : TPurgerJobErrorEvent read fOnPurgerJobError write SetOnPurgerJobError;
    procedure RemoveValue(const aKey : string); virtual;
    procedure Refresh(const aKey: string; aExpirationMilliseconds : Integer);
    procedure Flush; virtual;
  end;

  TMemoryCache<T> = class(TMemoryCacheBase,IMemoryCache<T>)
  private
    procedure SetValue(const aKey : string; aValue : T; aExpirationMilliseconds : Integer; aExpirationDate : TDateTime); overload;
  public
    constructor Create(aPurgerInterval : Integer = 20; aCacheSerializer : ICacheSerializer = nil; aCacheCompressor : ICacheCompressor = nil); override;
    procedure SetValue(const aKey : string; aValue : T; aExpirationMillisecons : Integer = 0); overload;
    procedure SetValue(const aKey : string; aValue : T; aExpirationDate : TDateTime); overload;
    function GetValue(const aKey : string) : T;
    function TryGetValue(const aKey : string; out oValue : T) : Boolean;
    procedure RemoveValue(const aKey : string); override;
  end;

  TMemoryCache = class(TMemoryCacheBase,IMemoryCache)
  private
    procedure SetValue(const aKey: string; aValue: TObject; aExpirationMilliseconds : Integer; aExpirationDate : TDateTime); overload;
    procedure SetValue(const aKey, aValue: string; aExpirationMilliseconds : Integer; aExpirationDate : TDateTime); overload;
  public
    procedure SetValue(const aKey, aValue : string; aExpirationMilliseconds : Integer = 0); overload;
    procedure SetValue(const aKey, aValue : string; aExpirationDate : TDateTime); overload;
    procedure SetValue(const aKey : string; aValue : TObject; aExpirationMilliseconds : Integer = 0); overload;
    procedure SetValue(const aKey : string; aValue : TObject; aExpirationDate : TDateTime); overload;
    procedure SetValue(const aKey : string; aValue : TArray<string>; aExpirationMilliseconds : Integer = 0); overload;
    procedure SetValue(const aKey : string; aValue : TArray<string>; aExpirationDate : TDateTime); overload;
    procedure SetValue(const aKey : string; aValue : TArray<TObject>; aExpirationMilliseconds : Integer = 0); overload;
    procedure SetValue(const aKey : string; aValue : TArray<TObject>; aExpirationDate : TDateTime); overload;
    function GetValue(const aKey : string) : string; overload;
    function TryGetValue(const aKey : string; out aValue : string) : Boolean; overload;
    function TryGetValue(const aKey : string; aValue : TObject) : Boolean; overload;
    function TryGetValue<T>(const aKey : string; out oValue : T) : Boolean; overload;
    function TryGetValue(const aKey : string; out aValue : TArray<string>) : Boolean; overload;
    function TryGetValue(const aKey : string; out aValue : TArray<TObject>) : Boolean; overload;
  end;

  EMemoryCacheConfigError = class(Exception);
  EMemoryCacheSetError = class(Exception);
  EMemoryCacheGetError = class(Exception);
  EMemoryCacheFlushError = class(Exception);


implementation

{ TMemoryCacheBase }

constructor TMemoryCacheBase.Create(aPurgerInterval : Integer = 20; aCacheSerializer : ICacheSerializer = nil; aCacheCompressor : ICacheCompressor = nil);
begin
  fCompression := True;
  SetPurgerInterval(aPurgerInterval);
  fCachedObjects := 0;
  fCacheSize := 0;
  fLock := TMultiReadExclusiveWriteSynchronizer.Create;
  if aCacheSerializer <> nil then fSerializer := aCacheSerializer
    else fSerializer := TCacheJsonSerializer.Create;
  if aCacheCompressor <> nil then fCompressor := aCacheCompressor
    else fCompressor := TCacheCompressorGZip.Create;

  fItems := TDictionary<string,ICacheEntry>.Create;
  fCacheJobs := TScheduledTasks.Create;
  CreatePurgerJobs;
  fCacheJobs.Start;
end;

procedure TMemoryCacheBase.CreatePurgerJobs;
begin
  fCacheJobs.AddTask('RemoveExpired',procedure (task : ITask)
                  begin
                    RemoveExpiredCacheEntries;
                  end
                  ).OnException(procedure(task : ITask; aException : Exception)
                  begin
                    if Assigned(fOnPurgerJobError) then fOnPurgerJobError(aException.Message);
                  end
                  ).StartInSeconds(fPurgerInterval).RepeatEvery(fPurgerInterval,TTimeMeasure.tmSeconds);
end;

destructor TMemoryCacheBase.Destroy;
begin
  fItems.Free;
  fLock.Free;
  fCacheJobs.Stop;
  fCacheJobs.Free;
  inherited;
end;

procedure TMemoryCacheBase.Flush;
begin
  fLock.BeginWrite;
  try
    fItems.Clear;
    if Assigned(fOnCacheFlushed) then fOnCacheFlushed(fCachedObjects);
    fCachedObjects := 0;
    fCacheSize := 0;
  finally
    fLock.EndWrite;
  end;
end;

procedure TMemoryCacheBase.Refresh(const aKey: string; aExpirationMilliseconds : Integer);
var
  cacheitem : ICacheEntry;
begin
  if fItems.TryGetValue(aKey,cacheitem) then
  begin
    cacheitem.CreationDate := Now;
    cacheitem.Expiration := aExpirationMilliseconds;
  end;
end;

procedure TMemoryCacheBase.RemoveExpiredCacheEntries;
var
  pair : TPair<string,ICacheEntry>;
  removedentries : Integer;
begin
  if Assigned(fOnBeginPurgerJob) then fOnBeginPurgerJob;
  removedentries := 0;
  fLock.BeginRead;
  try
    for pair in fItems do
    begin
      if pair.Value.IsExpired then
      begin
        fLock.BeginWrite;
        try
          //decrease cacheitem size to cachesize
          AtomicDecrement(fCacheSize,pair.Value.Size);
          //remove cacheitem from cache
          fItems.Remove(pair.Key);
          //decrease cachedobjects
          AtomicDecrement(fCachedObjects,1);
          Inc(removedentries);
        finally
          fLock.EndWrite;
        end;
      end;
    end;
  finally
    fLock.EndRead;
    if Assigned(fOnEndPurgerJob) then fOnEndPurgerJob(removedentries);
  end;
end;

procedure TMemoryCacheBase.RemoveValue(const aKey: string);
var
  cacheitem : ICacheEntry;
begin
  if fItems.TryGetValue(aKey,cacheitem) then
  begin
    //decrease cacheitem size to cachesize
    AtomicDecrement(fCacheSize,cacheitem.Size);
    //remove cacheitem from cache
    fItems.Remove(aKey);
    //decrease cachedobjects
    AtomicDecrement(fCachedObjects,1);
  end;
end;

function TMemoryCacheBase.GetCachedObjects: Integer;
begin
  Result := fCachedObjects;
end;

function TMemoryCacheBase.GetCacheSize: Integer;
begin
  Result := fCacheSize;
end;

function TMemoryCacheBase.GetCompression: Boolean;
begin
  Result := fCompression;
end;

procedure TMemoryCacheBase.SetCompression(const Value: Boolean);
begin
  fCompression := Value;
end;

procedure TMemoryCacheBase.SetOnBeginPurgerJob(const Value: TBeginPurgerJobEvent);
begin
  fOnBeginPurgerJob := Value;
end;

procedure TMemoryCacheBase.SetOnCacheFlushed(const Value: TCacheFlushedEvent);
begin
  fOnCacheFlushed := Value;
end;

procedure TMemoryCacheBase.SetOnEndPurgerJob(const Value: TEndPurgerJobEvent);
begin
  fOnEndPurgerJob := Value;
end;

procedure TMemoryCacheBase.SetOnPurgerJobError(const Value: TPurgerJobErrorEvent);
begin
  fOnPurgerJobError := Value;
end;

procedure TMemoryCacheBase.SetPurgerInterval(const Value: Integer);
begin
  if Value > 5 then
  begin
    fPurgerInterval := Value;
  end
  else raise EMemoryCacheConfigError.Create('Purger Interval must be greater than 5 seconds');
end;

{ TCacheItem }

constructor TCacheEntry.Create(aCompression : Boolean; aCacheCompressor : ICacheCompressor);
begin
  fIsCompressed := False;
  fCompression := aCompression;
  fCompressor := aCacheCompressor;
end;

function TCacheEntry.GetCreationDate: TDateTime;
begin
  Result := fCreationDate;
end;

function TCacheEntry.GetData: string;
begin
  if fIsCompressed then Result := fCompressor.Decompress(fData)
    else Result := fData;
end;

function TCacheEntry.GetExpiration: Cardinal;
begin
  Result := fExpiration;
end;

function TCacheEntry.GetExpirationDate: TDateTime;
begin
  Result := fExpirationDate;
end;

procedure TCacheEntry.SetCreationDate(const Value: TDateTime);
begin
  fCreationDate := Value;
end;

procedure TCacheEntry.SetExpiration(aMilliseconds: Cardinal);
begin
  fExpiration := aMilliseconds;
  fExpirationDate := IncMilliSecond(fCreationDate,fExpiration);
end;

procedure TCacheEntry.SetExpirationDate(const Value: TDateTime);
begin
  fExpiration := MilliSecondOf(Value);
  fExpirationDate := Value;
end;

function TCacheEntry.IsExpired: Boolean;
begin
  if fExpiration = 0 then Result := False
    else Result := Now() > fExpirationDate;
end;

procedure TCacheEntry.SetData(const Value: string);
begin
  fIsCompressed := False;
  //var a := value;
  //var b := value.Length;
  if fCompression then
  begin
    if ((Value.Length + 1) * 2) > 1024 then
    begin
      fData := fCompressor.Compress(Value);
      fIsCompressed := True;
    end
    else
    begin
      fData := Value;
    end;
  end
  else fData := Value;
end;

function TCacheEntry.Size: Integer;
begin
  //Result := (fData.Length + 1) * SizeOf(Char);
  Result := (fData.Length + 1) * StringElementSize(fData);
end;

{ TMemoryCache<T> }

constructor TMemoryCache<T>.Create(aPurgerInterval: Integer; aCacheSerializer: ICacheSerializer; aCacheCompressor: ICacheCompressor);
begin
  inherited Create(aPurgerInterval,aCacheSerializer,aCacheCompressor);
end;

function TMemoryCache<T>.GetValue(const aKey: string): T;
begin
  TryGetValue(aKey,Result);
end;

procedure TMemoryCache<T>.RemoveValue(const aKey: string);
begin
  inherited RemoveValue(aKey);
end;

procedure TMemoryCache<T>.SetValue(const aKey: string; aValue: T; aExpirationDate: TDateTime);
begin
  SetValue(aKey,aValue,0,aExpirationDate);
end;

procedure TMemoryCache<T>.SetValue(const aKey: string; aValue: T; aExpirationMillisecons: Integer);
begin
  SetValue(aKey,aValue,aExpirationMillisecons,0.0);
end;

procedure TMemoryCache<T>.SetValue(const aKey: string; aValue: T; aExpirationMilliseconds : Integer; aExpirationDate : TDateTime);
var
  serialized : string;
  cacheitem : TCacheEntry;
begin
  fLock.BeginWrite;
  try
    cacheitem := TCacheEntry.Create(fCompression,fCompressor);
    cacheitem.CreationDate := Now();
    cacheitem.Expiration := aExpirationMilliseconds;
    if aExpirationDate > 0.0 then cacheitem.ExpirationDate := aExpirationDate;
    //add object to cache
    case PTypeInfo(TypeInfo(T))^.Kind of
      tkClass, tkPointer :
        begin
          //object type need to be serialized
          cacheitem.Data := fSerializer.Serialize(PObject(@aValue)^);
        end;
      tkString, tkWideString, tkUString, tkChar, tkWideChar : cacheitem.Data := string((@aValue)^);
      {$IFNDEF NEXTGEN}
      tkAnsiString : cacheitem.Data := string(AnsiString((@aValue)^));
      {$ENDIF}
    else
      begin
        raise EMemoryCacheSetError.Create('Type not supported as cache');
      end;
    end;
    RemoveValue(aKey);
    fItems.Add(aKey,cacheitem);
    //add cacheitem size to cachesize
    AtomicIncrement(fCacheSize,cacheitem.Size);
    //increment cacheobjects
    AtomicIncrement(fCachedObjects,1);
  finally
    fLock.EndWrite;
  end;
end;

function TMemoryCache<T>.TryGetValue(const aKey: string; out oValue: T): Boolean;
var
  cacheitem : ICacheEntry;
  flexvalue : TFlexValue;
  obj : TObject;
begin
  fLock.BeginRead;
  try
    Result := fItems.TryGetValue(aKey,cacheitem);
    //check if cacheitem already expired
    if Result and cacheitem.IsExpired then Exit(False);
  finally
    fLock.EndRead;
  end;

  if Result then
  begin
    flexvalue.AsString := cacheitem.Data;
    case PTypeInfo(TypeInfo(T))^.Kind of
      tkInteger : oValue := TValue.From(flexvalue.AsInteger).AsType<T>;
      tkInt64 : oValue := TValue.From(flexvalue.AsInt64).AsType<T>;
      tkFloat :
        begin
          if TypeInfo(T) = TypeInfo(TDateTime) then oValue := TValue.From(flexvalue.AsDateTime).AsType<T>
            else oValue := TValue.From(flexvalue.AsExtended).AsType<T>;
        end;
      tkString,
      tkUString  : oValue := TValue.From(flexvalue.AsString).AsType<T>;
      {$IFDEF MSWINDOWS}
      tkAnsiString : oValue := TValue.From(flexvalue.AsAnsiString).AsType<T>;
      tkWideString : oValue := TValue.From(flexvalue.AsWideString).AsType<T>;
      {$ENDIF}
      tkEnumeration :
        begin
          if TypeInfo(T) = TypeInfo(Boolean) then oValue := TValue.From(flexvalue.AsBoolean).AsType<T>
            else oValue := TValue.From(flexvalue.AsInteger).AsType<T>;
        end;
      tkClass, tkPointer :
        begin
          obj := PTypeInfo(TypeInfo(T))^.TypeData.ClassType.Create;
          fSerializer.Deserialize(cacheitem.Data,obj);
          oValue := TValue.From(obj).AsType<T>;
          //oValue := T((@obj)^);
        end
      else raise EMemoryCacheGetError.Create('Error casting value from cache');
    end;
  end;
end;

{ TMemoryCache }

function TMemoryCache.GetValue(const aKey: string): string;
begin
  TryGetValue(aKey,Result);
end;

procedure TMemoryCache.SetValue(const aKey, aValue: string; aExpirationMilliseconds: Integer);
begin
  SetValue(aKey,aValue,aExpirationMilliseconds,0.0);
end;

procedure TMemoryCache.SetValue(const aKey, aValue: string; aExpirationDate: TDateTime);
begin
  SetValue(aKey,aValue,0,aExpirationDate);
end;

procedure TMemoryCache.SetValue(const aKey: string; aValue: TObject; aExpirationMilliseconds: Integer);
begin
  SetValue(aKey,aValue,aExpirationMilliseconds,0.0);
end;

procedure TMemoryCache.SetValue(const aKey: string; aValue: TObject; aExpirationDate: TDateTime);
begin
  SetValue(aKey,aValue,0,aExpirationDate);
end;

procedure TMemoryCache.SetValue(const aKey: string; aValue: TObject; aExpirationMilliseconds : Integer; aExpirationDate : TDateTime);
begin
  SetValue(aKey,fSerializer.Serialize(aValue),aExpirationMilliseconds,aExpirationDate);
end;

procedure TMemoryCache.SetValue(const aKey, aValue: string; aExpirationMilliseconds : Integer; aExpirationDate : TDateTime);
var
  cacheitem : TCacheEntry;
begin
  fLock.BeginWrite;
  try
    cacheitem := TCacheEntry.Create(fCompression,fCompressor);
    cacheitem.CreationDate := Now();
    cacheitem.Expiration := aExpirationMilliseconds;
    if aExpirationDate > 0.0 then cacheitem.ExpirationDate := aExpirationDate;
    //add object to cache
    cacheitem.Data := aValue;
    RemoveValue(aKey);
    fItems.Add(aKey,cacheitem);
    //add cacheitem size to cachesize
    AtomicIncrement(fCacheSize,cacheitem.Size);
    //increment cacheobjects
    AtomicIncrement(fCachedObjects,1);
  finally
    fLock.EndWrite;
  end;
end;

procedure TMemoryCache.SetValue(const aKey: string; aValue: TArray<string>; aExpirationDate: TDateTime);
begin
  SetValue(aKey,fSerializer.Serialize(aValue),0,aExpirationDate);
end;

procedure TMemoryCache.SetValue(const aKey: string; aValue: TArray<string>; aExpirationMilliseconds: Integer);
begin
  SetValue(aKey,fSerializer.Serialize(aValue),aExpirationMilliseconds,0.0);
end;

procedure TMemoryCache.SetValue(const aKey: string; aValue: TArray<TObject>; aExpirationDate: TDateTime);
begin
  SetValue(aKey,fSerializer.Serialize(aValue),0,aExpirationDate);
end;

procedure TMemoryCache.SetValue(const aKey: string; aValue: TArray<TObject>; aExpirationMilliseconds: Integer);
begin
  SetValue(aKey,fSerializer.Serialize(aValue),aExpirationMilliseconds,0.0);
end;

function TMemoryCache.TryGetValue(const aKey: string; aValue : TObject): Boolean;
var
  cacheitem : ICacheEntry;
begin
  fLock.BeginRead;
  try
    if aValue = nil then raise EMemoryCacheGetError.Create('Cannot passed a nil object as param');

    Result := fItems.TryGetValue(aKey,cacheitem);
    //check if cacheitem already expired
    if (not Result) or (cacheitem.IsExpired) then Exit(False);
  finally
    fLock.EndRead;
  end;
  fSerializer.Deserialize(cacheitem.Data,aValue);
end;

function TMemoryCache.TryGetValue(const aKey: string; out aValue: string): Boolean;
begin
  Result := TryGetValue<string>(aKey,aValue);
end;

function TMemoryCache.TryGetValue<T>(const aKey: string; out oValue: T): Boolean;
var
  cacheitem : ICacheEntry;
  flexvalue : TFlexValue;
  obj : TObject;
begin
  fLock.BeginRead;
  try
    Result := fItems.TryGetValue(aKey,cacheitem);
    //check if cacheitem already expired
    if Result and cacheitem.IsExpired then Exit(False);
  finally
    fLock.EndRead;
  end;

  if Result then
  begin
    flexvalue.AsString := cacheitem.Data;
    case PTypeInfo(TypeInfo(T))^.Kind of
      tkInteger : oValue := TValue.From(flexvalue.AsInteger).AsType<T>;
      tkInt64 : oValue := TValue.From(flexvalue.AsInt64).AsType<T>;
      tkFloat :
        begin
          if TypeInfo(T) = TypeInfo(TDateTime) then oValue := TValue.From(flexvalue.AsDateTime).AsType<T>
            else oValue := TValue.From(flexvalue.AsExtended).AsType<T>;
        end;
      tkString,
      tkUString  : oValue := TValue.From(flexvalue.AsString).AsType<T>;
      {$IFDEF MSWINDOWS}
      tkAnsiString : oValue := TValue.From(flexvalue.AsAnsiString).AsType<T>;
      tkWideString : oValue := TValue.From(flexvalue.AsWideString).AsType<T>;
      {$ENDIF}
      tkEnumeration :
        begin
          if TypeInfo(T) = TypeInfo(Boolean) then oValue := TValue.From(flexvalue.AsBoolean).AsType<T>
            else oValue := TValue.From(flexvalue.AsInteger).AsType<T>;
        end;
      tkClass, tkPointer :
        begin
          obj := PTypeInfo(TypeInfo(T))^.TypeData.ClassType.Create;
          fSerializer.Deserialize(flexvalue.AsString,obj);
          oValue := TValue.From(obj).AsType<T>;
        end;
      else raise EMemoryCacheGetError.Create('Error casting value from cache');
    end;
  end;
end;

function TMemoryCache.TryGetValue(const aKey: string; out aValue: TArray<string>): Boolean;
var
  cacheitem : ICacheEntry;
begin
  fLock.BeginRead;
  try
    Result := fItems.TryGetValue(aKey,cacheitem);
    //check if cacheitem already expired
    if Result and  cacheitem.IsExpired then Exit(False);
  finally
    fLock.EndRead;
  end;

  if Result then fSerializer.Deserialize(cacheitem.Data,aValue);
end;

function TMemoryCache.TryGetValue(const aKey: string; out aValue: TArray<TObject>): Boolean;
var
  cacheitem : ICacheEntry;
begin
  fLock.BeginRead;
  try
    Result := fItems.TryGetValue(aKey,cacheitem);
    //check if cacheitem already expired
    if Result and  cacheitem.IsExpired then Exit(False);
  finally
    fLock.EndRead;
  end;

  if Result then fSerializer.Deserialize(cacheitem.Data,aValue);
end;

end.
