{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.CloudStorage.Provider.Amazon
  Description : CloudStorage Amazon provider
  Author      : Kike Pérez
  Version     : 1.8
  Created     : 14/10/2018
  Modified    : 07/10/2019

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

unit Quick.CloudStorage.Provider.Amazon;

{$i QuickLib.inc}

interface

uses
  Classes,
  System.SysUtils,
  Quick.Commons,
  Quick.CloudStorage,
  Quick.Amazon;

type

  TCloudStorageAmazonProvider = class(TCloudStorageProvider)
  private
    fAmazon : TQuickAmazon;
    fAmazonID : string;
    fAmazonKey: string;
    fAmazonRegion : string;
    fSecure : Boolean;
    fCurrentBuckeet: string;
    procedure SetSecure(const Value: Boolean);
  public
    constructor Create; overload;
    constructor Create(const aAccountID, aAccountKey, aAWSRegion : string); overload;
    destructor Destroy; override;
    property Secure : Boolean read fSecure write SetSecure;
    procedure OpenDir(const aPath : string); override;
    function GetFile(const aPath: string; out stream : TStream) : Boolean; override;
  end;

implementation

{ TCloudExplorerProvider }

constructor TCloudStorageAmazonProvider.Create;
begin
  fAmazon := TQuickAmazon.Create;
  fAmazon.AmazonProtocol := amHTTPS;
end;

constructor TCloudStorageAmazonProvider.Create(const aAccountID, aAccountKey, aAWSRegion : string);
begin
  Create;
  fAmazonID := aAccountID;
  fAmazonKey := aAccountKey;
  fAmazonRegion := aAWSRegion;
  fAmazon.AccountName := fAmazonID;
  fAmazon.AccountKey := fAmazonKey;
  fAmazon.AWSRegion := TQuickAmazon.GetAWSRegion(fAmazonRegion);
end;

destructor TCloudStorageAmazonProvider.Destroy;
begin
  if Assigned(fAmazon) then fAmazon.Free;
  inherited;
end;

function TCloudStorageAmazonProvider.GetFile(const aPath: string; out stream : TStream) : Boolean;
begin

end;

procedure TCloudStorageAmazonProvider.OpenDir(const aPath : string);
var
  lista : TAmazonObjects;
  Blob : TAmazonObject;
  i : Integer;
  azurefilter : string;
  DirItem : TCloudItem;
  respinfo : TAmazonResponseInfo;
begin
  if aPath = '..' then
  begin
    CurrentPath := RemoveLastPathSegment(CurrentPath);
  end
  else
  begin
    if CurrentPath = '' then CurrentPath := aPath
      else CurrentPath := CurrentPath + aPath;
  end;
  if Assigned(OnBeginReadDir) then OnBeginReadDir(CurrentPath);
  if CurrentPath.StartsWith('/') then CurrentPath := Copy(CurrentPath,2,CurrentPath.Length);
  if (not CurrentPath.IsEmpty) and (not CurrentPath.EndsWith('/')) then CurrentPath := CurrentPath + '/';

  Status := stRetrieving;
  lista := fAmazon.ListObjects(RootFolder,CurrentPath,fAmazon.AWSRegion,respinfo);
  try
    if CurrentPath <> '' then
    begin
      if Assigned(OnGetListItem) then
      begin
        DirItem := TCloudItem.Create;
        try
          DirItem.Name := '..';
          DirItem.IsDir := True;
          DirItem.Date := 0;
          OnGetListItem(DirItem);
        finally
          DirItem.Free;
        end;
      end;
    end;

    if respinfo.StatusCode = 200 then
    begin
      for Blob in lista do
      begin
        DirItem := TCloudItem.Create;
        try
          if Blob.Name.StartsWith(CurrentPath) then Blob.Name := StringReplace(Blob.Name,CurrentPath,'',[]);
          if Blob.Name.Contains('/') then
          begin
            DirItem.IsDir := True;
            DirItem.Name := Copy(Blob.Name,1,Blob.Name.IndexOf('/'));
          end
          else
          begin
            DirItem.IsDir := False;
            DirItem.Name := Blob.Name;
            DirItem.Size := Blob.Size;
            DirItem.Date := Blob.Modified;
          end;
          if Assigned(OnGetListItem) then OnGetListItem(DirItem);
        finally
          DirItem.Free;
        end;
      end;
      Status := stDone;
    end
    else Status := stFailed;
  finally
    lista.Free;
    ResponseInfo.Get(respinfo.StatusCode,respinfo.StatusMsg);
  end;
end;

procedure TCloudStorageAmazonProvider.SetSecure(const Value: Boolean);
begin
  fSecure := Value;
  if Value then fAmazon.AmazonProtocol := TAmazonProtocol.amHTTPS
    else fAmazon.AmazonProtocol := TAmazonProtocol.amHTTP;
end;

end.
