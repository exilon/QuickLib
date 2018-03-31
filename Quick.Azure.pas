{ ***************************************************************************

  Copyright (c) 2015-2017 Kike Pérez

  Unit        : Quick.Azure
  Description : Azure blobs operations
  Author      : Kike Pérez
  Version     : 1.2
  Created     : 27/08/2015
  Modified    : 20/10/2017

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

unit Quick.Azure;

interface

uses
  Classes,
  System.SysUtils,
  System.Generics.Collections,
  IPPeerClient,
  Data.Cloud.CloudAPI,
  Data.Cloud.AzureAPI;

type

  TAzureProtocol = (azHTTP,azHTTPS);
  //TAzureBlob = Data.Cloud.AzureAPI.TAzureBlob;
  TBlobPublicAccess = Data.Cloud.AzureAPI.TBlobPublicAccess;

  TAzureResponseInfo = record
    StatusCode : Integer;
    StatusMsg : string;
  end;

  TAzureBlobObject = class
    Name : string;
    Size : Int64;
    LastModified : TDateTime;
  end;

  TBlobList = class (TObjectList<TAzureBlobObject>);


  TQuickAzure = class
    private
      fconAzure : TAzureConnectionInfo;
      fAccountName : string;
      fAccountKey : string;
      fAzureProtocol : TAzureProtocol;
      fTimeOut : Integer;
      procedure SetAccountName(azAccountName : string);
      procedure SetAccountKey(azAccountKey : string);
      procedure SetAzureProtocol(azProtocol : TAzureProtocol);
      function FileToArray(cFilename : string) : TArray<Byte>;
      function StreamToArray(cStream : TStream) : TArray<Byte>;
      function GMT2DateTime(const gmtdate : string):TDateTime;
    public
      constructor Create; overload;
      constructor Create(azAccountName, azAccountKey : string); overload;
      destructor Destroy; override;
      property AccountName : string read fAccountName write SetAccountName;
      property AccountKey : string read fAccountKey write SetAccountKey;
      property AzureProtocol : TAzureProtocol read fAzureProtocol write SetAzureProtocol;
      property TimeOut : Integer read fTimeOut write fTimeOut;
      function PutBlob(azContainer, cFilename, azBlobName : string; out azResponseInfo : TAzureResponseInfo) : Boolean; overload;
      function PutBlob(azContainer : string; cStream : TStream; azBlobName : string; out azResponseInfo : TAzureResponseInfo) : Boolean; overload;
      function GetBlob(azContainer, azBlobName, cFilenameTo : string; out azResponseInfo : TAzureResponseInfo) : Boolean; overload;
      function GetBlob(azContainer, azBlobName : string; out azResponseInfo : TAzureResponseInfo; out Stream : TMemoryStream) : Boolean; overload;
      function GetBlob(const azContainer, azBlobName : string; out azResponseInfo : TAzureResponseInfo) : TMemoryStream; overload;
      function CopyBlob(azSourceContainer, azSourceBlobName : string; azTargetContainer, azTargetBlobName : string; out azResponseInfo : TAzureResponseInfo) : Boolean;
      function RenameBlob(const azContainer, azSourceBlobName, azTargetBlobName : string; out azResponseInfo : TAzureResponseInfo) : Boolean;
      function ExistsObject(const azContainer, azBlobName : string) : Boolean;
      function ExistsFolder(azContainer : string; azFolderName : string) : Boolean;
      function DeleteBlob(azContainer,azBlobName : string; out azResponseInfo : TAzureResponseInfo) : Boolean;
      function ListBlobs(azContainer : string; azBlobsStartWith : string; Recursive : Boolean; out azResponseInfo : TAzureResponseInfo) : TBlobList;
      function ListBlobsNames(azContainer : string; azBlobsStartWith : string; Recursive : Boolean; out azResponseInfo : TAzureResponseInfo) : TStrings;
      function ExistsContainer(azContainer : string) : Boolean;
      function ListContainers(azContainersStartWith : string; azResponseInfo : TAzureResponseInfo) : TStrings;
      function CreateContainer(azContainer : string; azPublicAccess : TBlobPublicAccess; out azResponseInfo : TAzureResponseInfo) : Boolean;
      function DeleteContainer(azContainer : string; out azResponseInfo : TAzureResponseInfo) : Boolean;
  end;

implementation


constructor TQuickAzure.Create;
begin
  inherited;
  fconAzure := TAzureConnectionInfo.Create(nil);
  fAzureProtocol := azHTTP;
  fTimeOut := 30;
end;

constructor TQuickAzure.Create(azAccountName, azAccountKey : string);
begin
  Create;
  SetAccountName(azAccountName);
  SetAccountKey(azAccountKey);
end;

destructor TQuickAzure.Destroy;
begin
  if Assigned(fconAzure) then fconAzure.Free;
  inherited;
end;

procedure TQuickAzure.SetAccountName(azAccountName : string);
begin
  if fAccountName <> azAccountName  then
  begin
    fAccountName := azAccountName;
    fconAzure.AccountName := azAccountName;
  end;
end;

procedure TQuickAzure.SetAccountKey(azAccountKey : string);
begin
  if fAccountKey  <> azAccountKey   then
  begin
    fAccountKey  := azAccountKey ;
    fconAzure.AccountKey  := azAccountKey;
  end;
end;

procedure TQuickAzure.SetAzureProtocol(azProtocol: TAzureProtocol);
begin
  if fAzureProtocol <> azProtocol then
  begin
    fAzureProtocol := azProtocol;
    if azProtocol = azHTTP then fconAzure.Protocol := 'HTTP'
      else fconAzure.Protocol := 'HTTPS';
  end;
end;

function TQuickAzure.FileToArray(cFilename : string) : TArray<Byte>;
var
  fs : TFileStream;
  bs : TBytesStream;
begin
  fs := TFileStream.Create(cFilename, fmOpenRead);
  try
    bs := TBytesStream.Create(Result);
    try
      bs.LoadFromStream(fs);
      Result := bs.Bytes;
    finally
      bs.Free
    end;
  finally
    fs.Free;
  end;
end;

function TQuickAzure.StreamToArray(cStream : TStream) : TArray<Byte>;
var
  bs : TBytesStream;
begin
  bs := TBytesStream.Create(Result);
  try
    bs.LoadFromStream(cStream);
    Result := bs.Bytes;
  finally
    bs.Free
  end;
end;

function TQuickAzure.GMT2DateTime(const gmtdate : string):TDateTime;
  function GetMonthDig(Value : string):Integer;
  const
    aMonth : array[1..12] of string = ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
  var
    idx : Integer;
  begin
    Result := 0;
    for idx := 1 to 12 do
    begin
      if CompareText(Value,aMonth[idx]) = 0 then
      begin
        Result := idx;
        Break;
      end;
     end;
  end;
var
  i : Integer;
  Len : Integer;
  wDay, wMonth, wYear,
  wHour, wMinute, wSec : Word;
begin
  //GMT Format: 'Mon, 12 Jan 2014 16:20:35 GMT'
  Result := 0;
  Len := 0;
  if gmtdate = '' then Exit;

  try
    for i := 0 to Length(gmtdate) do
    begin
      if gmtdate[i] in ['0'..'9'] then
      begin
        Len := i;
        Break;
      end;
    end;

    //Day
    wDay := StrToIntDef(Copy(gmtdate,Len,2),0);
    if wDay = 0 then Exit;

    Inc(Len,3);

    //Month
    wMonth := GetMonthDig(Copy(gmtdate,Len,3));
    if wMonth = 0 then Exit;
    Inc(Len,4);

    //Year
    wYear := StrToIntDef(Copy(gmtdate,Len,4),0);
    if wYear = 0 then Exit;
    Inc(Len,5);

    //Hour
    wHour := StrToIntDef(Copy(gmtdate,Len,2),99);
    if wHour = 99 then Exit;
    Inc(Len,3);

    //Min
    wMinute := StrToIntDef(Copy(gmtdate,Len,2),99);
    if wMinute = 99 then Exit;
    Inc(Len,3);

    //Sec
    wSec := StrToIntDef(Copy(gmtdate,Len,2),99);
    if wSec = 99 then Exit;

    Result := EncodeDate(wYear,wMonth,wDay) + EncodeTime(wHour,wMinute,wSec,0);
  except
    Result := 0;
  end;
end;

function GetResponseInfo(ResponseInfo : TCloudResponseInfo) : TAzureResponseInfo;
begin
  Result.StatusCode := ResponseInfo.StatusCode;
  Result.StatusMsg := ResponseInfo.StatusMessage;
end;

function TQuickAzure.PutBlob(azContainer, cFilename, azBlobName : string; out azResponseInfo : TAzureResponseInfo) : Boolean;
var
  BlobService : TAzureBlobService;
  Content : TArray<Byte>;
  CloudResponseInfo : TCloudResponseInfo;
begin
  Result := False;
  BlobService := TAzureBlobService.Create(fconAzure);
  try
    if azContainer = '' then azContainer := '$root';
    CloudResponseInfo := TCloudResponseInfo.Create;
    try
      BlobService.Timeout := fTimeout;
      Content := FileToArray(cFilename);
      if azBlobName = '' then azBlobName := cFilename;
      if azBlobName.StartsWith('/') then azBlobName := Copy(azBlobName,2,Length(azBlobName));
      Result := BlobService.PutBlockBlob(azContainer,azBlobName,Content,EmptyStr,nil,nil,CloudResponseInfo);
      azResponseInfo := GetResponseInfo(CloudResponseInfo);
    finally
      CloudResponseInfo.Free;
    end;
  finally
    BlobService.Free;
  end;
end;

function TQuickAzure.PutBlob(azContainer : string; cStream : TStream; azBlobName : string; out azResponseInfo : TAzureResponseInfo) : Boolean;
var
  BlobService : TAzureBlobService;
  Content : TArray<Byte>;
  CloudResponseInfo : TCloudResponseInfo;
begin
  azResponseInfo.StatusCode := 500;
  if cStream.Size = 0 then
  begin
    azResponseInfo.StatusMsg := 'Stream is empty';
    Exit;
  end;

  if azContainer = '' then azContainer := '$root';
  if azBlobName.StartsWith('/') then azBlobName := Copy(azBlobName,2,Length(azBlobName));
  try
    BlobService := TAzureBlobService.Create(fconAzure);
    try
      BlobService.Timeout := fTimeout;
      CloudResponseInfo := TCloudResponseInfo.Create;
      try
        Content := StreamToArray(cStream);
        Result := BlobService.PutBlockBlob(azContainer,azBlobName,Content,EmptyStr,nil,nil,CloudResponseInfo);
        azResponseInfo := GetResponseInfo(CloudResponseInfo);
      finally
        CloudResponseInfo.Free;
      end;
    finally
      BlobService.Free;
    end;
  except
    on E : Exception do
    begin
      azResponseInfo.StatusCode := 500;
      azResponseInfo.StatusMsg := e.message;
      Result := False;
    end;
  end;
end;

function TQuickAzure.GetBlob(azContainer, azBlobName, cFilenameTo : string; out azResponseInfo : TAzureResponseInfo) : Boolean;
var
  BlobService : TAzureBlobService;
  fs : TFileStream;
  CloudResponseInfo : TCloudResponseInfo;
begin
  Result := False;
  if azContainer = '' then azContainer := '$root';
  if azBlobName.StartsWith('/') then azBlobName := Copy(azBlobName,2,Length(azBlobName));
  BlobService := TAzureBlobService.Create(fconAzure);
  try
    BlobService.Timeout := fTimeout;
    fs := TFileStream.Create(cFilenameTo,fmCreate);
    try
      try
        CloudResponseInfo := TCloudResponseInfo.Create;
        try
          Result := BlobService.GetBlob(azContainer,azBlobName,fs,EmptyStr,CloudResponseInfo);
          azResponseInfo := GetResponseInfo(CloudResponseInfo);
        finally
          CloudResponseInfo.Free;
        end;
      except
        Result := False;
      end;
    finally
      fs.Free;
    end;
  finally
    BlobService.Free;
  end;
end;

function TQuickAzure.GetBlob(azContainer, azBlobName : string; out azResponseInfo : TAzureResponseInfo; out Stream : TMemoryStream) : Boolean;
var
  BlobService : TAzureBlobService;
  CloudResponseInfo : TCloudResponseInfo;
begin
  Result := False;
  Stream := TMemoryStream.Create;
  if azContainer = '' then azContainer := '$root';
  if azBlobName.StartsWith('/') then azBlobName := Copy(azBlobName,2,Length(azBlobName));
  BlobService := TAzureBlobService.Create(fconAzure);
  try
    BlobService.Timeout := fTimeout;
    try
      CloudResponseInfo := TCloudResponseInfo.Create;
      try
        Result := BlobService.GetBlob(azContainer,azBlobName,Stream,EmptyStr,CloudResponseInfo);
        azResponseInfo := GetResponseInfo(CloudResponseInfo);
      finally
        CloudResponseInfo.Free;
      end;
    except
      Stream := nil;
    end;
  finally
    BlobService.Free;
  end;
end;

function TQuickAzure.GetBlob(const azContainer, azBlobName : string; out azResponseInfo : TAzureResponseInfo) : TMemoryStream;
begin
  GetBlob(azContainer,azBlobName,azResponseInfo,Result);
end;

function TQuickAzure.CopyBlob(azSourceContainer, azSourceBlobName : string; azTargetContainer, azTargetBlobName : string; out azResponseInfo : TAzureResponseInfo) : Boolean;
var
  BlobService : TAzureBlobService;
  CloudResponseInfo : TCloudResponseInfo;
begin
  Result := False;
  if azSourceContainer = '' then azSourceContainer := '$root';
  if azTargetContainer = '' then azTargetContainer := '$root';
  if azSourceBlobName.StartsWith('/') then azSourceBlobName := Copy(azSourceBlobName,2,Length(azSourceBlobName));
  if azTargetBlobName.StartsWith('/') then azTargetBlobName := Copy(azTargetBlobName,2,Length(azTargetBlobName));
  BlobService := TAzureBlobService.Create(fconAzure);
  try
    BlobService.Timeout := fTimeout;
    try
      CloudResponseInfo := TCloudResponseInfo.Create;
      try
        Result := BlobService.CopyBlob(azTargetContainer,azTargetBlobName,azSourceContainer,azSourceBlobName,'',nil,CloudResponseInfo);
        azResponseInfo := GetResponseInfo(CloudResponseInfo);
      finally
        CloudResponseInfo.Free;
      end;
    except
      on E : Exception do
      begin
        Result := False;
        azResponseInfo.StatusCode := 500;
        azResponseInfo.StatusMsg := e.message;
      end;
    end;
  finally
    BlobService.Free;
  end;
end;

function TQuickAzure.RenameBlob(const azContainer, azSourceBlobName, azTargetBlobName : string; out azResponseInfo : TAzureResponseInfo) : Boolean;
var
  ResponseInfo : TAzureResponseInfo;
begin
  Result := False;
  if CopyBlob(azContainer,azSourceBlobName,azContainer,azTargetBlobName,ResponseInfo) then
  begin
    Result := DeleteBlob(azContainer,azSourceBlobName,ResponseInfo);
  end;
end;

function TQuickAzure.ExistsObject(const azContainer, azBlobName : string) : Boolean;
var
  azBlob : string;
  azBlobs : TStrings;
  ResponseInfo : TAzureResponseInfo;
begin
  Result := False;
  azBlobs := ListBlobsNames(azContainer,azBlobName,True,ResponseInfo);
  try
    if (ResponseInfo.StatusCode = 200) and (Assigned(azBlobs)) then
    begin
      for azBlob in azBlobs do
      begin
        if azBlob = azBlobName then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  finally
    azBlobs.Free;
  end;
end;

function TQuickAzure.ExistsFolder(azContainer : string; azFolderName : string) : Boolean;
var
  BlobService : TAzureBlobService;
  azBlob : TAzureBlob;
  azBlobList : TList<TAzureBlob>;
  CloudResponseInfo : TCloudResponseInfo;
  AzParams : TStrings;
  cNextMarker : string;
begin
  Result := False;
  if azContainer = '' then azContainer := '$root';
  BlobService := TAzureBlobService.Create(fconAzure);
  try
    BlobService.Timeout := fTimeout;
    AzParams := TStringList.Create;
    try
      if not azFolderName.EndsWith('/') then azFolderName := azFolderName + '/';      
      AzParams.Values['prefix'] := azFolderName;
      AzParams.Values['delimiter'] := '/';
      AzParams.Values['maxresults'] := '1';
      cNextMarker := '';
      CloudResponseInfo := TCloudResponseInfo.Create;
      try
        azBlobList := BlobService.ListBlobs(azContainer,cNextMarker,AzParams,CloudResponseInfo);
        try
          if (Assigned(azBlobList)) and (azBlobList.Count > 0) and (CloudResponseInfo.StatusCode = 200) then Result := True;
        finally
          //frees azbloblist objects
          for azBlob in azBlobList do azBlob.Free;
          azBlobList.Free;
        end;
      finally
        CloudResponseInfo.Free;
      end;
    finally
      AzParams.Free;
    end;
  finally
    BlobService.Free;
  end;
end;

function TQuickAzure.DeleteBlob(azContainer,azBlobName : string; out azResponseInfo : TAzureResponseInfo) : Boolean;
var
  BlobService : TAzureBlobService;
  CloudResponseInfo : TCloudResponseInfo;
begin
  Result := False;
  if azContainer = '' then azContainer := '$root';
  if azBlobName.StartsWith('/') then azBlobName := Copy(azBlobName,2,Length(azBlobName));
  BlobService := TAzureBlobService.Create(fconAzure);
  try
    BlobService.Timeout := fTimeout;
    CloudResponseInfo := TCloudResponseInfo.Create;
    try
      Result := BlobService.DeleteBlob(azContainer,azBlobName,False,EmptyStr,CloudResponseInfo);
      azResponseInfo := GetResponseInfo(CloudResponseInfo);
    finally
      CloudResponseInfo.Free;
    end;
  finally
    BlobService.Free;
  end;
end;

function TQuickAzure.ListBlobs(azContainer : string; azBlobsStartWith : string; Recursive : Boolean; out azResponseInfo : TAzureResponseInfo) : TBlobList;
var
  BlobService : TAzureBlobService;
  azBlob : TAzureBlob;
  azBlobList : TList<TAzureBlob>;
  Blob : TAzureBlobObject;
  CloudResponseInfo : TCloudResponseInfo;
  cNextMarker : string;
  AzParams : TStrings;
begin
  Result := TBlobList.Create(True);
  cNextMarker := '';
  if azContainer = '' then azContainer := '$root';
  BlobService := TAzureBlobService.Create(fconAzure);
  try
    BlobService.Timeout := fTimeout;
    repeat
      AzParams := TStringList.Create;
      try
        AzParams.Values['prefix'] := azBlobsStartWith;
        if not Recursive then AzParams.Values['delimiter'] := '/';
        if cNextMarker <> '' then AzParams.Values['marker'] := cNextMarker;
        CloudResponseInfo := TCloudResponseInfo.Create;
        try
          azBlobList := BlobService.ListBlobs(azContainer,cNextMarker,AzParams,CloudResponseInfo);
          azResponseInfo := GetResponseInfo(CloudResponseInfo);
          if Assigned(azBlobList) then
          begin
            try
              for azBlob in azBlobList do
              begin
                Blob := TAzureBlobObject.Create;
                Blob.Name := azBlob.Name;
                Blob.Size := StrToInt64Def(azBlob.Properties.Values['Content-Length'],0);
                Blob.LastModified := GMT2DateTime(azBlob.Properties.Values['Last-Modified']);
                Result.Add(Blob);
              end;
            finally
              //frees azbloblist objects
              for azBlob in azBlobList do azBlob.Free;
              azBlobList.Free;
            end;
          end;
        finally
          CloudResponseInfo.Free;
        end;
      finally
        FreeAndNil(AzParams);
      end;
    until (cNextMarker = '') or (azResponseInfo.StatusCode <> 200);
  finally
    BlobService.Free;
  end;
end;

function TQuickAzure.ListBlobsNames(azContainer : string; azBlobsStartWith : string; Recursive : Boolean; out azResponseInfo : TAzureResponseInfo) : TStrings;
var
  BlobService : TAzureBlobService;
  azBlob : TAzureBlob;
  azBlobList : TList<TAzureBlob>;
  CloudResponseInfo : TCloudResponseInfo;
  cNextMarker : string;
  AzParams : TStrings;
begin
  Result := TStringList.Create;
  cNextMarker := '';
  if azContainer = '' then azContainer := '$root';
  BlobService := TAzureBlobService.Create(fconAzure);
  CloudResponseInfo := TCloudResponseInfo.Create;
  try
    BlobService.Timeout := fTimeout;
    repeat
      AzParams := TStringList.Create;
      try
        AzParams.Values['prefix'] := azBlobsStartWith;
        if not Recursive then AzParams.Values['delimiter'] := '/';
        if cNextMarker <> '' then AzParams.Values['marker'] := cNextMarker;

        azBlobList := BlobService.ListBlobs(azContainer,cNextMarker,AzParams,CloudResponseInfo);
        azResponseInfo := GetResponseInfo(CloudResponseInfo);
        if Assigned(azBlobList) then
        begin
          Result.BeginUpdate;
          try
            for azBlob in azBlobList do Result.Add(azBlob.Name);
          finally
            Result.EndUpdate;
            //frees bloblist objects
            for azBlob in azBlobList do azBlob.Free;
            azBlobList.Free;
          end;
        end;
      finally
        AzParams.Free;
      end;
    until (cNextMarker = '') or (azResponseInfo.StatusCode <> 200);
  finally
    BlobService.Free;
    CloudResponseInfo.Free;
  end;
end;

function TQuickAzure.ExistsContainer(azContainer : string) : Boolean;
var
  Container : string;
  Containers : TStrings;
  ResponseInfo : TAzureResponseInfo;
begin
  Result := False;
  Containers := ListContainers(azContainer,ResponseInfo);
  try
    if (ResponseInfo.StatusCode = 200) and (Assigned(Containers)) then
    begin
      for Container in Containers do
      begin
        if Container = azContainer then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  finally
    Containers.Free;
  end;
end;

function TQuickAzure.ListContainers(azContainersStartWith : string; azResponseInfo : TAzureResponseInfo) : TStrings;
var
  BlobService : TAzureBlobService;
  CloudResponseInfo : TCloudResponseInfo;
  cNextMarker : string;
  AzParams : TStrings;
  AzContainer : TAzureContainer;
  AzContainers : TList<TAzureContainer>;
begin
  Result := TStringList.Create;
  cNextMarker := '';
  BlobService := TAzureBlobService.Create(fconAzure);
  CloudResponseInfo := TCloudResponseInfo.Create;
  try
    BlobService.Timeout := fTimeout;
    repeat
      AzParams := TStringList.Create;
      try
        if azContainersStartWith <> '' then AzParams.Values['prefix'] := azContainersStartWith;
        if cNextMarker <> '' then AzParams.Values['marker'] := cNextMarker;
        AzContainers := BlobService.ListContainers(cNextMarker,AzParams,CloudResponseInfo);
        try
          azResponseInfo := GetResponseInfo(CloudResponseInfo);
          if (azResponseInfo.StatusCode = 200) and (Assigned(AzContainer)) then
          begin
            for AzContainer in AzContainers do
            begin
              Result.Add(AzContainer.Name);
            end;
          end;
        finally
          if Assigned(AzContainer) then
          begin
            //frees ContainerList objects
            for AzContainer in AzContainers do AzContainer.Free;
            AzContainers.Free;
          end;
        end;
      finally
        AzParams.Free;
      end;
    until (cNextMarker = '') or (azResponseInfo.StatusCode <> 200);
  finally
    BlobService.Free;
    CloudResponseInfo.Free;
  end;
end;

function TQuickAzure.CreateContainer(azContainer : string; azPublicAccess : TBlobPublicAccess; out azResponseInfo : TAzureResponseInfo) : Boolean;
var
  BlobService : TAzureBlobService;
  CloudResponseInfo : TCloudResponseInfo;
begin
  Result := False;
  if azContainer = '' then Exit;

  BlobService := TAzureBlobService.Create(fconAzure);
  try
    BlobService.Timeout := fTimeout;
    CloudResponseInfo := TCloudResponseInfo.Create;
    Result := BlobService.CreateContainer(azContainer,nil,azPublicAccess,CloudResponseInfo);
    azResponseInfo := GetResponseInfo(CloudResponseInfo);
  finally
    BlobService.Free;
    CloudResponseInfo.Free;
  end;
end;

function TQuickAzure.DeleteContainer(azContainer : string; out azResponseInfo : TAzureResponseInfo) : Boolean;
var
  BlobService : TAzureBlobService;
  CloudResponseInfo : TCloudResponseInfo;
begin
  Result := False;
  if azContainer = '' then Exit;

  BlobService := TAzureBlobService.Create(fconAzure);
  try
    BlobService.Timeout := fTimeout;
    CloudResponseInfo := TCloudResponseInfo.Create;
    Result := BlobService.DeleteContainer(azContainer,CloudResponseInfo);
    azResponseInfo := GetResponseInfo(CloudResponseInfo);
  finally
    BlobService.Free;
    CloudResponseInfo.Free;
  end;
end;

end.
