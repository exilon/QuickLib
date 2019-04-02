unit Quick.CloudStorage.Provider.Azure;

interface

uses
  Classes,
  System.SysUtils,
  System.Generics.Collections,
  Quick.Commons,
  Quick.CloudStorage,
  IPPeerClient,
  Data.Cloud.CloudAPI,
  Data.Cloud.AzureAPI;

type

  TCloudStorageAzureProvider = class(TCloudStorageProvider)
  private
    fAzureConnection : TAzureConnectionInfo;
    fAzureID : string;
    fAzureKey : string;
    procedure SetSecure(aValue: Boolean); override;
    function ListContainers(azContainersStartWith : string; azResponseInfo : TResponseInfo) : TStrings;
  public
    constructor Create; overload; override;
    constructor Create(const aAccountName, aAccountKey : string); overload;
    destructor Destroy; override;
    function GetRootFolders : TStrings; override;
    procedure OpenDir(const aPath : string); override;
    function GetFile(const aPath: string; out stream : TStream) : Boolean; override;
    function GetURL(const aPath : string) : string; override;
  end;

implementation

{ TCloudExplorerProvider }

constructor TCloudStorageAzureProvider.Create;
begin
  fAzureConnection := TAzureConnectionInfo.Create(nil);
end;

constructor TCloudStorageAzureProvider.Create(const aAccountName, aAccountKey : string);
begin
  inherited Create;
  Create;
  fAzureID := aAccountName;
  fAzureKey := aAccountKey;
  fAzureConnection.AccountName := aAccountName;
  fAzureConnection.AccountKey := aAccountKey;
end;

destructor TCloudStorageAzureProvider.Destroy;
begin
  if Assigned(fAzureConnection) then fAzureConnection.Free;
  inherited;
end;

function TCloudStorageAzureProvider.GetFile(const aPath: string; out stream : TStream) : Boolean;
var
  BlobService : TAzureBlobService;
  CloudResponseInfo : TCloudResponseInfo;
begin
  BlobService := TAzureBlobService.Create(fAzureConnection);
  try
    CloudResponseInfo := TCloudResponseInfo.Create;
    try
      Result := BlobService.GetBlob(RootFolder,aPath,stream,'',CloudResponseInfo);
      if not Result then raise Exception.CreateFmt('Cloud error %d : %s',[CloudResponseInfo.StatusCode,CloudResponseInfo.StatusMessage]);
    finally
      CloudResponseInfo.Free;
    end;
  finally
    BlobService.Free;
  end;
end;

function TCloudStorageAzureProvider.GetRootFolders: TStrings;
var
  respinfo : TResponseInfo;
begin
  Result := ListContainers('',respinfo);
end;

function TCloudStorageAzureProvider.GetURL(const aPath: string): string;
begin
  Result := Format('https://%s.blob.core.windows.net/%s/%s',[fAzureConnection.AccountName,RootFolder,aPath]);
end;

function TCloudStorageAzureProvider.ListContainers(azContainersStartWith : string; azResponseInfo : TResponseInfo) : TStrings;
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
  BlobService := TAzureBlobService.Create(fAzureConnection);
  CloudResponseInfo := TCloudResponseInfo.Create;
  try
    BlobService.Timeout := Timeout;
    repeat
      AzParams := TStringList.Create;
      try
        if azContainersStartWith <> '' then AzParams.Values['prefix'] := azContainersStartWith;
        if cNextMarker <> '' then AzParams.Values['marker'] := cNextMarker;
        AzContainers := BlobService.ListContainers(cNextMarker,AzParams,CloudResponseInfo);
        try
          azResponseInfo.Get(CloudResponseInfo);
          if (azResponseInfo.StatusCode = 200) and (Assigned(AzContainers)) then
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

procedure TCloudStorageAzureProvider.OpenDir(const aPath: string);
var
  BlobService : TAzureBlobService;
  azBlob : TAzureBlob;
  azBlobList : TList<TAzureBlob>;
  DirItem : TCloudItem;
  CloudResponseInfo : TCloudResponseInfo;
  cNextMarker : string;
  AzParams : TStrings;
  azResponseInfo : TResponseInfo;
  azContainer : string;
begin
  Status := stSearching;
  cNextMarker := '';
  if aPath = '..' then
  begin
    CurrentPath := RemoveLastPathSegment(CurrentPath);
  end
  else
  begin
    if (CurrentPath = '') or (aPath.StartsWith('/')) then CurrentPath := aPath
      else CurrentPath := CurrentPath + aPath;
  end;
  if Assigned(OnBeginReadDir) then OnBeginReadDir(CurrentPath);
  if CurrentPath.StartsWith('/') then CurrentPath := Copy(CurrentPath,2,CurrentPath.Length);
  if (not CurrentPath.IsEmpty) and (not CurrentPath.EndsWith('/')) then CurrentPath := CurrentPath + '/';

  azContainer := RootFolder;
  if azContainer = '' then azContainer := '$root';
  BlobService := TAzureBlobService.Create(fAzureConnection);
  try
    BlobService.Timeout := Timeout;
    Status := stRetrieving;
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
    repeat
      if not (Status in [stSearching,stRetrieving]) then Exit;
      AzParams := TStringList.Create;
      try
        if fCancelOperation then
        begin
          fCancelOperation := False;
          Exit;
        end;
        AzParams.Values['prefix'] := CurrentPath;
        //if not Recursive then
        AzParams.Values['delimiter'] := '/';
        AzParams.Values['maxresults'] := '100';
        if cNextMarker <> '' then AzParams.Values['marker'] := cNextMarker;
        CloudResponseInfo := TCloudResponseInfo.Create;
        try
          azBlobList := BlobService.ListBlobs(azContainer,cNextMarker,AzParams,CloudResponseInfo);
          azResponseInfo.Get(CloudResponseInfo);
          if azResponseInfo.StatusCode = 200 then
          begin
            try
              for azBlob in azBlobList do
              begin
                if not (Status in [stSearching,stRetrieving]) then Exit;
                if fCancelOperation then
                begin
                  fCancelOperation := False;
                  Exit;
                end;
                DirItem := TCloudItem.Create;
                try
                  DirItem.Name := azBlob.Name;
                  if DirItem.Name.StartsWith(CurrentPath) then DirItem.Name := StringReplace(DirItem.Name,CurrentPath,'',[]);
                  if DirItem.Name.Contains('/') then
                  begin
                    DirItem.IsDir := True;
                    DirItem.Name := Copy(DirItem.Name,1,DirItem.Name.IndexOf('/'));
                  end
                  else
                  begin
                    DirItem.IsDir := False;
                    DirItem.Size := StrToInt64Def(azBlob.Properties.Values['Content-Length'],0);
                    DirItem.Date := GMT2DateTime(azBlob.Properties.Values['Last-Modified']);
                  end;
                  if Assigned(OnGetListItem) then OnGetListItem(DirItem);
                finally
                  DirItem.Free;
                end;
                azBlob.Free;
              end;
            finally
              //frees azbloblist objects
              //for azBlob in azBlobList do azBlob.Free;
              azBlobList.Free;
            end;
          end
          else
          begin
            Status := stFailed;
            Exit;
          end;
        finally
          CloudResponseInfo.Free;
        end;
      finally
        FreeAndNil(AzParams);
      end;
      if Assigned(OnRefreshReadDir) then OnRefreshReadDir(CurrentPath);
    until (cNextMarker = '') or (azResponseInfo.StatusCode <> 200);
    Status := stDone;
  finally
    BlobService.Free;
    if Assigned(OnEndReadDir) then OnEndReadDir(CurrentPath);
  end;
end;

{procedure TCloudStorageAzureProvider.OpenDir(const aPath : string);
var
  lista : TBlobList;
  Blob : TAzureBlobObject;
  i : Integer;
  azurefilter : string;
  DirItem : TCloudItem;
  respinfo : TAzureResponseInfo;
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
  lista := fAzure.ListBlobs(RootFolder,CurrentPath,False,respinfo);
  try
    if Assigned(lista) then
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
            DirItem.Date := Blob.LastModified;
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
end;}

procedure TCloudStorageAzureProvider.SetSecure(aValue: Boolean);
begin
  inherited;
  if aValue then fAzureConnection.Protocol := 'HTTPS'
    else fAzureConnection.Protocol := 'HTTP';
end;

end.
