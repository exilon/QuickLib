{ ***************************************************************************

  Copyright (c) 2015-2021 Kike Pérez

  Unit        : Quick.Amazon
  Description : Amazon object operations
  Author      : Kike Pérez
  Version     : 1.4
  Created     : 18/11/2016
  Modified    : 18/11/2021

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

unit Quick.Amazon;

{$i QuickLib.inc}

interface

uses
  Classes,
  System.SysUtils,
  System.Generics.Collections,
  IPPeerClient,
  Data.Cloud.CloudAPI,
  Data.Cloud.AmazonAPI,
  Quick.Commons;

const

  AWSRegionSet : array of string = [
    'eu-west-1',
    'eu-west-2',
    'eu-west-3',
    'eu-central-1',
    'us-east-1',
    'us-east-2',
    'us-west-1',
    'us-west-2',
    'ap-east-1',
    'ap-south-1',
    'ap-southeast-1',
    'ap-southeast-2',
    'ap-northeast-1',
    'ap-northeast-2',
    'ap-northeast-3',
    'ca-central-1',
    'sa-east-1',
    'us-east-1', // deprecated
    'eu-west-1', // deprecated
    'cn-north-1',
    'cn-northwest-1',
    'eu-north-1',
    'me-south-1'];

type

  TAmazonProtocol = (amHTTP,amHTTPS);
  TAmazonStorage = TAmazonStorageService;
  TAmazonACLAccess = TAmazonACLType;
  TAmazonRegion = Data.Cloud.AmazonAPI.TAmazonRegion;

  TAmazonObject = class
    Name : string;
    Modified : TDateTime;
    Size : Int64;
    IsDeleted : Boolean;
  end;

  TAmazonObjects = class(TObjectList<TAmazonObject>);

  TAmazonResponseInfo = record
    StatusCode : Integer;
    StatusMsg : string;
  end;

  TQuickAmazon = class
    private
      fconAmazon : TAmazonConnectionInfo;
      fAccountName : string;
      fAccountKey : string;
      fAWSRegion : TAmazonRegion;
      fAmazonProtocol : TAmazonProtocol;
      procedure SetAccountName(amAccountName : string);
      procedure SetAccountKey(amAccountKey : string);
      procedure SetAmazonProtocol(amProtocol : TAmazonProtocol);
      procedure SetAWSRegion(Value : TAmazonRegion);
      function FileToArray(cFilename : string) : TArray<Byte>;
      function ByteContent(DataStream: TStream): TBytes;
    public
      constructor Create; overload;
      constructor Create(amAccountName, amAccountKey : string); overload;
      destructor Destroy; override;
      property AccountName : string read fAccountName write SetAccountName;
      property AccountKey : string read fAccountKey write SetAccountKey;
      property AmazonProtocol : TAmazonProtocol read fAmazonProtocol write SetAmazonProtocol;
      property AWSRegion : TAmazonRegion read fAWSRegion write SetAWSRegion;
      function StorageURL(amBucket : string) : string;
      function PutObject(amBucket, cFilename, amObjectName : string; amACLType : TAmazonACLType; var amResponseInfo : TAmazonResponseInfo) : Boolean; overload;
      function PutObject(amBucket : string; cStream : TStream; amObjectName : string; amACLType : TAmazonACLType; var amResponseInfo : TAmazonResponseInfo) : Boolean; overload;
      function GetObject(amBucket, amObjectName, cFilenameTo : string; var amResponseInfo : TAmazonResponseInfo) : Boolean; overload;
      function GetObject(amBucket, amObjectName : string; var amResponseInfo : TAmazonResponseInfo) : TMemoryStream; overload;
      function ExistsObject(amBucket, amObjectName : string; amRegion : TAmazonRegion) : Boolean;
      function DeleteObject(amBucket,amObjectName : string; var amResponseInfo : TAmazonResponseInfo) : Boolean;
      function ListObjects(amBucket : string; amObjectsStartWith : string; amRegion : TAmazonRegion; var amResponseInfo : TAmazonResponseInfo) : TAmazonObjects;
      function ListObjectsNames(amBucket : string; amObjectsStartWith : string; amRegion : TAmazonRegion; var amResponseInfo : TAmazonResponseInfo) : TStrings;
      function ExistsBucket(amBucketName : string) : Boolean;
      function ListBuckets(var amResponseInfo : TAmazonResponseInfo) : TStrings;
      function CreateBucket(amBucket : string; amBucketRegion : TAmazonRegion; amACLType : TAmazonACLAccess; var amResponseInfo : TAmazonResponseInfo) : Boolean;
      function DeleteBucket(amBucket : string; amBucketRegion : TAmazonRegion; var amResponseInfo : TAmazonResponseInfo) : Boolean;
      {$IFNDEF DELPHISYDNEY_UP}
      class function GetAWSRegion(Region: TAmazonRegion): string; overload;
      {$ELSE}
      class function GetAWSRegion(const Region : string) : TAmazonRegion; overload;
      {$ENDIF}
  end;

implementation

constructor TQuickAmazon.Create;
begin
  inherited;
  fconAmazon := TAmazonConnectionInfo.Create(nil);
  fAmazonProtocol := amHTTP;
  fconAmazon.UseDefaultEndpoints := False;
end;

constructor TQuickAmazon.Create(amAccountName, amAccountKey : string);
begin
  Create;
  SetAccountName(amAccountName);
  SetAccountKey(amAccountKey);
end;

destructor TQuickAmazon.Destroy;
begin
  if Assigned(fconAmazon) then fconAmazon.Free;
  inherited;
end;

procedure TQuickAmazon.SetAWSRegion(Value : TAmazonRegion);
begin
  fAWSRegion := Value;

  //fconAmazon.StorageEndpoint := Format('s3-%s.amazonaws.com',[GetAWSRegion(Value)]);
  //fconAmazon.StorageEndpoint := Format('s3.%s.amazonaws.com',[GetAWSRegion(Value)]);
  {$IFDEF DELPHISYDNEY_UP}
  if not StrInArray(Value,AWSRegionSet) then raise Exception.CreateFmt('%s is not a valid region for AmazonS3!',[Value]);

  fconAmazon.Region := Value;
  {$ELSE}
  fconAmazon.StorageEndpoint := Format('s3.%s.amazonaws.com',[GetAWSRegion(Value)]);
  {$ENDIF}
end;

procedure TQuickAmazon.SetAccountName(amAccountName : string);
begin
  if fAccountName <> amAccountName  then
  begin
    fAccountName := amAccountName;
    fconAmazon.AccountName := amAccountName;
  end;
end;

procedure TQuickAmazon.SetAccountKey(amAccountKey : string);
begin
  if fAccountKey  <> amAccountKey   then
  begin
    fAccountKey  := amAccountKey ;
    fconAmazon.AccountKey  := amAccountKey;
  end;
end;

procedure TQuickAmazon.SetAmazonProtocol(amProtocol: TAmazonProtocol);
begin
  if fAmazonProtocol <> amProtocol then
  begin
    fAmazonProtocol := amProtocol;
    if amProtocol = amHTTP then fconAmazon.Protocol := 'http'
      else fconAmazon.Protocol := 'https';
  end;
end;

function TQuickAmazon.FileToArray(cFilename : string) : TArray<Byte>;
var
  fs : TFileStream;
  bs : TBytesStream;
begin
  fs := TFileStream.Create(cFilename, fmOpenRead);
  try
    Result := ByteContent(fs);
  finally
    fs.Free;
  end;
end;

function TQuickAmazon.ByteContent(DataStream: TStream): TBytes;
var
  Buffer: TBytes;
begin
  if not Assigned(DataStream) then Exit(nil);
  SetLength(Buffer, DataStream.Size);
  // the content may have been read
  DataStream.Position := 0;
  if DataStream.Size > 0 then
  DataStream.Read(Buffer[0], DataStream.Size);
  Result := Buffer;
end;

function GetResponseInfo(amResponseInfo : TCloudResponseInfo) : TAmazonResponseInfo;
begin
  Result.StatusCode := amResponseInfo.StatusCode;
  Result.StatusMsg := amResponseInfo.StatusMessage;
end;

function TQuickAmazon.StorageURL(amBucket : string) : string;
begin
  Result := fconAmazon.StorageURL(amBucket)
end;

function TQuickAmazon.PutObject(amBucket, cFilename, amObjectName : string; amACLType : TAmazonACLType; var amResponseInfo : TAmazonResponseInfo) : Boolean;
var
  AmazonS3 : TAmazonStorage;
  Content : TArray<Byte>;
  CloudResponseInfo : TCloudResponseInfo;
begin
  AmazonS3 := TAmazonStorage.Create(fconAmazon);
  if amBucket = '' then amBucket := '$root';
  try
    Content := FileToArray(cFilename);
    if amObjectName = '' then amObjectName := cFilename;
    if amObjectName.StartsWith('/') then amObjectName := Copy(amObjectName,2,Length(amObjectName));
    CloudResponseInfo := TCloudResponseInfo.Create;
    try
      Result := AmazonS3.UploadObject(amBucket,amObjectName,Content,False,nil,nil,amACLType,CloudResponseInfo{$IFDEF DELPHIRX11_UP},fAWSRegion{$ENDIF});
      amResponseInfo := GetResponseInfo(CloudResponseInfo);
    finally
      CloudResponseInfo.Free;
    end;
  finally
    AmazonS3.Free;
  end;
end;

function TQuickAmazon.PutObject(amBucket : string; cStream : TStream; amObjectName : string; amACLType : TAmazonACLType; var amResponseInfo : TAmazonResponseInfo) : Boolean;
var
  AmazonS3 : TAmazonStorage;
  Content : TBytes;
  CloudResponseInfo : TCloudResponseInfo;
begin
  amResponseInfo.StatusCode := 500;
  if amBucket = '' then amBucket := '$root';
  if amObjectName.StartsWith('/') then amObjectName := Copy(amObjectName,2,Length(amObjectName));
  try
    AmazonS3 := TAmazonStorage.Create(fconAmazon);
    try
      //AmazonS3.Timeout := fTimeout;
      CloudResponseInfo := TCloudResponseInfo.Create;
      try
        //CloudResponseInfo.Headers.AddPair();
        Content := ByteContent(cStream);
        Result := AmazonS3.UploadObject(amBucket,amObjectName,Content,False,nil,nil,amACLType,CloudResponseInfo{$IFDEF DELPHIRX11_UP},fAWSRegion{$ENDIF});
        amResponseInfo := GetResponseInfo(CloudResponseInfo);
      finally
        CloudResponseInfo.Free;
      end;
    finally
      AmazonS3.Free;
      SetLength(Content,0);
      Content := nil;
    end;
  except
      Result := False;
  end;
end;

function TQuickAmazon.GetObject(amBucket, amObjectName, cFilenameTo : string; var amResponseInfo : TAmazonResponseInfo) : Boolean;
var
  AmazonS3 : TAmazonStorage;
  fs : TFileStream;
  CloudResponseInfo : TCloudResponseInfo;
  //amParams : TAmazonGetObjectOptionals;
begin
  Result := False;
  if amBucket = '' then amBucket := '$root';
  if amObjectName.StartsWith('/') then amObjectName := Copy(amObjectName,2,Length(amObjectName));
  AmazonS3 := TAmazonStorage.Create(fconAmazon);
  try
    //AmazonS3.Timeout := fTimeout;
    CloudResponseInfo := TCloudResponseInfo.Create;
    if FileExists(cFilenameTo) then fs := TFileStream.Create(cFilenameTo,fmOpenWrite)
      else fs := TFileStream.Create(cFilenameTo,fmCreate);
    try
      try
        AmazonS3.GetObject(amBucket,amObjectName,fs,CloudResponseInfo{$IFDEF DELPHIRX11_UP},fAWSRegion{$ENDIF});
        amResponseInfo := GetResponseInfo(CloudResponseInfo);
        if amResponseInfo.StatusCode = 200 then Result := True;
      except
        Result := False;
      end;
    finally
      fs.Free;
      CloudResponseInfo.Free;
    end;
  finally
    AmazonS3.Free;
  end;
end;

function TQuickAmazon.GetObject(amBucket, amObjectName : string; var amResponseInfo : TAmazonResponseInfo) : TMemoryStream;
var
  AmazonS3 : TAmazonStorage;
  CloudResponseInfo : TCloudResponseInfo;
begin
  Result := TMemoryStream.Create;
  if amBucket = '' then amBucket := '$root';
  if amObjectName.StartsWith('/') then amObjectName := Copy(amObjectName,2,Length(amObjectName));
  AmazonS3 := TAmazonStorage.Create(fconAmazon);
  try
    //AmazonS3.Timeout := fTimeout;
    CloudResponseInfo := TCloudResponseInfo.Create;
    try
      try
        AmazonS3.GetObject(amBucket,amObjectName,Result,CloudResponseInfo{$IFDEF DELPHIRX11_UP},fAWSRegion{$ENDIF});
        amResponseInfo := GetResponseInfo(CloudResponseInfo);
      except
        Result := nil;
      end;
    finally
      CloudResponseInfo.Free;
    end;
  finally
    AmazonS3.Free;
  end;
end;

function TQuickAmazon.ExistsObject(amBucket, amObjectName : string; amRegion : TAmazonRegion) : Boolean;
var
  amObject : string;
  amObjects : TStrings;
  ResponseInfo : TAmazonResponseInfo;
begin
  Result := False;
  amObjects := ListObjectsNames(amBucket,amObjectName,amRegion,ResponseInfo);
  try
    if (ResponseInfo.StatusCode = 200) and (Assigned(amObjects)) then
    begin
      for amObject in amObjects do
      begin
        if amObject = amObjectName then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  finally
    amObjects.Free;
  end;
end;

function TQuickAmazon.DeleteObject(amBucket,amObjectName : string; var amResponseInfo : TAmazonResponseInfo) : Boolean;
var
  AmazonS3 : TAmazonStorage;
  CloudResponseInfo : TCloudResponseInfo;
begin
  if amBucket = '' then amBucket := '$root';
  if amObjectName.StartsWith('/') then amObjectName := Copy(amObjectName,2,Length(amObjectName));
  AmazonS3 := TAmazonStorage.Create(fconAmazon);
  try
    //AmazonS3.Timeout := fTimeout;
    CloudResponseInfo := TCloudResponseInfo.Create;
    try
      Result := AmazonS3.DeleteObject(amBucket,amObjectName,CloudResponseInfo);
      amResponseInfo := GetResponseInfo(CloudResponseInfo);
    finally
      CloudResponseInfo.Free;
    end;
  finally
    AmazonS3.Free;
  end;
end;

function TQuickAmazon.ListObjects(amBucket : string; amObjectsStartWith : string; amRegion : TAmazonRegion; var amResponseInfo : TAmazonResponseInfo) : TAmazonObjects;
var
  AmazonS3 : TAmazonStorage;
  amObject : TAmazonObject;
  i : Integer;
  amBucketResult : TAmazonBucketResult;
  CloudResponseInfo : TCloudResponseInfo;
  cNextMarker : string;
  amParams : TStrings;
begin
  Result := TAmazonObjects.Create(True);
  cNextMarker := '';
  if amBucket = '' then amBucket := '$root';
  AmazonS3 := TAmazonStorage.Create(fconAmazon);
  CloudResponseInfo := TCloudResponseInfo.Create;
  try
    //AmazonS3.Timeout := fTimeout;
    repeat
      amParams := TStringList.Create;
      try
        amParams.Values['prefix'] := amObjectsStartWith;
        if cNextMarker <> '' then amParams.Values['marker'] := cNextMarker;
        amBucketResult := AmazonS3.GetBucket(amBucket,amParams,CloudResponseInfo,amRegion);
        amResponseInfo := GetResponseInfo(CloudResponseInfo);
        if Assigned(amBucketResult) then
        begin
          try
            Result.Capacity := amBucketResult.Objects.Count;
            for i := 0 to amBucketResult.Objects.Count-1 do
            begin
              amObject := TAmazonObject.Create;
              amObject.Name := amBucketResult.Objects[i].Name;
              amObject.Modified := StrToDateTime(amBucketResult.Objects[i].LastModified);
              amObject.Size := amBucketResult.Objects[i].Size;
              amObject.IsDeleted := amBucketResult.Objects[i].IsDeleted;
              Result.Add(amObject);
            end;
          finally
            amBucketResult.Free;
          end;
          cNextMarker := amBucketResult.Marker;
        end;
      finally
        amParams.Free;
      end;
    until (cNextMarker = '') or (amResponseInfo.StatusCode <> 200);
  finally
    AmazonS3.Free;
    CloudResponseInfo.Free;
  end;
end;

function TQuickAmazon.ListObjectsNames(amBucket : string; amObjectsStartWith : string; amRegion : TAmazonRegion; var amResponseInfo : TAmazonResponseInfo) : TStrings;
var
  AmazonS3 : TAmazonStorage;
  i : Integer;
  amBucketResult : TAmazonBucketResult;
  CloudResponseInfo : TCloudResponseInfo;
  cNextMarker : string;
  amParams : TStrings;
begin
  Result := TStringList.Create;
  cNextMarker := '';
  if amBucket = '' then amBucket := '$root';
  AmazonS3 := TAmazonStorage.Create(fconAmazon);
  CloudResponseInfo := TCloudResponseInfo.Create;
  try
    //AmazonS3.Timeout := fTimeout;
    repeat
      amParams := TStringList.Create;
      try
        if amObjectsStartWith <> '' then amParams.Values['prefix'] := amObjectsStartWith;
        if cNextMarker <> '' then amParams.Values['marker'] := cNextMarker;
        amBucketResult := AmazonS3.GetBucket(amBucket,amParams,CloudResponseInfo,amRegion);
        amResponseInfo := GetResponseInfo(CloudResponseInfo);
        if Assigned(amBucketResult) then
        begin
          try
            Result.Capacity := amBucketResult.Objects.Count;
            for i := 0 to amBucketResult.Objects.Count-1 do Result.Add(amBucketResult.Objects[i].Name);
          finally
            amBucketResult.Free;
          end;
          cNextMarker := amBucketResult.Marker;
        end;
      finally
        amParams.Free;
      end;
    until (cNextMarker = '') or (amResponseInfo.StatusCode <> 200);
  finally
    AmazonS3.Free;
    CloudResponseInfo.Free;
  end;
end;

function TQuickAmazon.ExistsBucket(amBucketName : string) : Boolean;
var
  amBucket : string;
  amBuckets : TStrings;
  ResponseInfo : TAmazonResponseInfo;
begin
  Result := False;
  amBuckets := ListBuckets(ResponseInfo);
  try
    if (ResponseInfo.StatusCode = 200) and (Assigned(amBuckets)) then
    begin
      for amBucket in amBuckets do
      begin
        if amBucket = amBucketName then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  finally
    amBuckets.Free;
  end;
end;

function TQuickAmazon.ListBuckets(var amResponseInfo : TAmazonResponseInfo) : TStrings;
var
  AmazonS3 : TAmazonStorageService;
  CloudResponseInfo : TCloudResponseInfo;
  Buckets : TStrings;
  i : Integer;
begin
  AmazonS3 := TAmazonStorageService.Create(fconAmazon);
  Result := TStringList.Create;
  try
    //AmazonS3.Timeout := fTimeout;
    CloudResponseInfo := TCloudResponseInfo.Create;
    try
      Buckets := AmazonS3.ListBuckets(CloudResponseInfo);
      try
        Result.Capacity := Buckets.Count;
        for i := 0 to Buckets.Count -1 do
        begin
          Result.Add(Buckets.Names[i]);
        end;
        amResponseInfo := GetResponseInfo(CloudResponseInfo);
      finally
        Buckets.Free;
      end;
    finally
      CloudResponseInfo.Free;
    end;
  finally
    AmazonS3.Free;
  end;
end;

function TQuickAmazon.CreateBucket(amBucket : string; amBucketRegion : TAmazonRegion; amACLType : TAmazonACLAccess; var amResponseInfo : TAmazonResponseInfo) : Boolean;
var
  AmazonS3 : TAmazonStorageService;
  CloudResponseInfo : TCloudResponseInfo;
begin
  Result := False;
  if amBucket = '' then Exit;

  AmazonS3 := TAmazonStorageService.Create(fconAmazon);
  try
    CloudResponseInfo := TCloudResponseInfo.Create;
    try
      Result := AmazonS3.CreateBucket(amBucket,amACLType,amBucketRegion,CloudResponseInfo);
      amResponseInfo := GetResponseInfo(CloudResponseInfo);
    finally
      CloudResponseInfo.Free;
    end;
  finally
    AmazonS3.Free;
  end;
end;

function TQuickAmazon.DeleteBucket(amBucket : string; amBucketRegion : TAmazonRegion; var amResponseInfo : TAmazonResponseInfo) : Boolean;
var
  AmazonS3 : TAmazonStorageService;
  CloudResponseInfo : TCloudResponseInfo;
begin
  Result := False;
  if amBucket = '' then Exit;

  AmazonS3 := TAmazonStorageService.Create(fconAmazon);
  try
    CloudResponseInfo := TCloudResponseInfo.Create;
    try
      Result := AmazonS3.DeleteBucket(amBucket,CloudResponseInfo,amBucketRegion);
      amResponseInfo := GetResponseInfo(CloudResponseInfo);
    finally
      CloudResponseInfo.Free;
    end;
  finally
    AmazonS3.Free;
  end;
end;

{$IFNDEF DELPHISYDNEY_UP}
class function TQuickAmazon.GetAWSRegion(Region: TAmazonRegion): string;
begin
  Result := TAmazonStorageService.GetRegionString(Region);
end;
{$ELSE}
class function TQuickAmazon.GetAWSRegion(const Region : string) : TAmazonRegion;
begin
  Result := TAmazonStorageService.GetRegionFromString(Region);
end;
{$ENDIF}

end.
