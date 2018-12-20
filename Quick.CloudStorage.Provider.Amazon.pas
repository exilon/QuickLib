unit Quick.CloudStorage.Provider.Amazon;

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
