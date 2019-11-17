{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.CloudStorage
  Description : CloudStorage
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

unit Quick.CloudStorage;

{$i QuickLib.inc}

interface

uses
  Classes,
  System.SysUtils,
  System.Generics.Collections,
  Data.Cloud.CloudAPI;

type

  TCloudActionStatus = (stNone, stSearching, stRetrieving, stDone, stFailed);

  TCloudProtocol = (cpHTTP,cpHTTPS);

  TResponseInfo = record
    StatusCode : Integer;
    StatusMsg : string;
    procedure Get(aStatusCode : Integer; const aStatusMsg : string); overload;
    procedure Get(aCloudResponseInfo : TCloudResponseInfo); overload;
  end;

  TCloudItem = class
  private
    fName : string;
    fIsDir : Boolean;
    fSize : Int64;
    fDate : TDateTime;
  public
    property Name : string read fName write fName;
    property IsDir : Boolean read fIsDir write fIsDir;
    property Size : Int64 read fSize write fSize;
    property Date : TDateTime read fDate write fDate;
  end;

  TCloudItemList = TObjectList<TCloudItem>;

  TReadDirEvent = procedure(const aDir : string) of object;
  TGetListItemEvent = procedure(aItem : TCloudItem) of object;
  TChangeStatusEvent = procedure(aStatus : TCloudActionStatus) of object;

  ICloudStorage = interface
  ['{5F36CD88-405F-45C1-89E0-9114146CA8D9}']
  function GetName : string;
  function GetRootFolders : TStrings;
  procedure OpenDir(const aPath : string);
  function GetFile(const aSourcePath: string; out stream : TStream) : Boolean; overload;
  function GetFile(const aSourcePath, aTargetLocalFile : string) : Boolean; overload;
  function GetURL(const aPath : string) : string;
  end;

  TCloudPermissions = class
  private
    fCanList : Boolean;
    fCanRead : Boolean;
    fCanWrite : Boolean;
    fCanDelete : Boolean;
  public
    property CanList : Boolean read fCanList write fCanList;
    property CanRead : Boolean read fCanRead write fCanRead;
    property CanWrite : Boolean read fCanWrite write fCanWrite;
    property CanDelete : Boolean read fCanDelete write fCanDelete;
  end;

  TCloudStorageProvider = class(TInterfacedObject,ICloudStorage)
  private
    fName : string;
    fResponseInfo : TResponseInfo;
    fCurrentPath : string;
    fOnGetListItem : TGetListItemEvent;
    fOnBeginReadDir : TReadDirEvent;
    fOnRefresReadDir : TReadDirEvent;
    fOnEndReadDir : TReadDirEvent;
    fOnChangeStatus : TChangeStatusEvent;
    fStatus: TCloudActionStatus;
    fRootFolder : string;
    fTimeout : Integer;
    fSecure : Boolean;
    fPermissions : TCloudPermissions;
    procedure SetStatus(aStatus : TCloudActionStatus);
  protected
    fCancelOperation : Boolean;
    procedure SetSecure(aValue : Boolean); virtual;
    function GMT2DateTime(const gmtdate : string):TDateTime;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Name : string read fName write fName;
    property ResponseInfo : TResponseInfo read fResponseInfo write fResponseInfo;
    property Timeout : Integer read fTimeout write fTimeout;
    property CurrentPath : string read fCurrentPath write fCurrentPath;
    property RootFolder : string read fRootFolder write fRootFolder;
    property OnBeginReadDir : TReadDirEvent read fOnBeginReadDir write fOnBeginReadDir;
    property OnRefreshReadDir : TReadDirEvent read fOnRefresReadDir write fOnRefresReadDir;
    property OnEndReadDir : TReadDirEvent read fOnEndReadDir write fOnEndReadDir;
    property OnGetListItem : TGetListItemEvent read fOnGetListItem write fOnGetListItem;
    property Status : TCloudActionStatus read fStatus write SetStatus;
    property Secure : Boolean read fSecure write SetSecure;
    property OnChangeStatus : TChangeStatusEvent read fOnChangeStatus write fOnChangeStatus;
    property Permissions : TCloudPermissions read fPermissions write fPermissions;
    class function GetStatusStr(aStatus : TCloudActionStatus) : string;
    function GetName : string;
    function GetRootFolders : TStrings; virtual; abstract;
    procedure OpenDir(const aPath : string); virtual; abstract;
    function GetFile(const aPath: string; out stream : TStream) : Boolean; overload; virtual; abstract;
    function GetFile(const aSourcePath, aTargetLocalFile : string) : Boolean; overload; virtual;
    function GetURL(const aPath : string) : string; virtual; abstract;
  end;

implementation

const
  CloudActionStatusStr : array of string = ['','Searching...','Retrieving...','Done','Failed'];

constructor TCloudStorageProvider.Create;
begin
  fCancelOperation := False;
  fPermissions := TCloudPermissions.Create;
  fTimeout := 30;
  fSecure := True;
  fPermissions.CanList := True;
  fPermissions.CanRead := True;
  fPermissions.CanWrite := True;
  fPermissions.CanDelete := True;
end;

destructor TCloudStorageProvider.Destroy;
begin
  if Assigned(fPermissions) then fPermissions.Free;
  inherited;
end;

function TCloudStorageProvider.GetFile(const aSourcePath, aTargetLocalFile: string): Boolean;
var
  stream : TStream;
begin
  stream := TFileStream.Create(aTargetLocalFile,fmCreate);
  try
    Result := GetFile(aSourcePath,stream);
  finally
    stream.Free;
  end;
end;

function TCloudStorageProvider.GetName: string;
begin
  Result := fName;
end;

class function TCloudStorageProvider.GetStatusStr(aStatus: TCloudActionStatus): string;
begin
  Result := CloudActionStatusStr[Integer(aStatus)];
end;

function TCloudStorageProvider.GMT2DateTime(const gmtdate: string): TDateTime;
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

procedure TCloudStorageProvider.SetSecure(aValue: Boolean);
begin
  fSecure := aValue;
end;

procedure TCloudStorageProvider.SetStatus(aStatus: TCloudActionStatus);
begin
  fStatus := aStatus;
  if Assigned(fOnChangeStatus) then fOnChangeStatus(aStatus);
end;


{ TResponseInfo }

procedure TResponseInfo.Get(aStatusCode: Integer; const aStatusMsg: string);
begin
  Self.StatusCode := aStatusCode;
  Self.StatusMsg := aStatusMsg;
end;

procedure TResponseInfo.Get(aCloudResponseInfo : TCloudResponseInfo);
begin
  Self.StatusCode := aCloudResponseInfo.StatusCode;
  Self.StatusMsg := aCloudResponseInfo.StatusMessage;
end;

end.
