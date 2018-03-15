{ ***************************************************************************

  Copyright (c) 2016-2018 Kike Pérez

  Unit        : Quick.Files
  Description : Files functions
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 09/03/2018
  Modified    : 15/03/2018

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

unit Quick.Files;

interface

uses
  Classes,
  System.SysUtils,
  Winapi.Windows;

type

  TTextFileOperation = (tfOpenRead,tfOpenOverwrite,tfOpenAppend);

	TTextStreamFile = class
    private
      fReadStream : TStreamReader;
      fWriteStream : TStreamWriter;
      function GetEOF : Boolean;
    public
      constructor Create(const aFileName : string; aOpenMode : TTextFileOperation);
      destructor Destroy; override;
      function ReadLn: string; overload;
      function ReadLn(out Data: string): Boolean; overload;
      procedure WriteLn (const Data : string);
      procedure Close;
      property EOF: Boolean read GetEOF;
  end;

  function CreateDummyFile(const aFilename : string; const aSize : Int64) : Boolean;
  procedure SplitFile(const aFileName : string; aSplitByteSize : Int64);
  procedure MergeFiles(const aFirstSplitFileName, aOutFileName : string); overload;
  procedure MergeFiles(aFilenames : array of string; const aOutFileName : string); overload;
  function IsFileInUse(const aFileName : string) : Boolean;
  procedure FileReplaceText(const aFileName, aSearchText, AReplaceText : string);
  function FileSearchText(const aFileName, SearchText: string; caseSensitive : Boolean): Longint;
  function GetLastAccessTime(const aFileName: string): TDateTime;
  function GetCreationTime(const aFilename : string): TDateTime;
  function GetLastModificationTime(const aFileName : string): TDateTime;

implementation

{ TTextStreamFile }

constructor TTextStreamFile.Create(const aFileName : string; aOpenMode : TTextFileOperation);
var
  Append : Boolean;
begin
  if aOpenMode = tfOpenRead then fReadStream := TStreamReader.Create(aFileName,True)
  else
  begin
    if aOpenMode = tfOpenAppend then Append := True
      else Append := False;
    fWriteStream := TStreamWriter.Create(aFileName,Append);
  end;
end;

destructor TTextStreamFile.Destroy;
begin
   if Assigned(fReadStream) then fReadStream.Free;
   if Assigned(fWriteStream) then fWriteStream.Free;
   inherited Destroy;
end;

function TTextStreamFile.ReadLn(out Data: string): Boolean;
begin
   Data := fReadStream.ReadLine;
   Result := Data <> '';
end;

function TTextStreamFile.ReadLn: string;
begin
   Result := fReadStream.ReadLine;
end;

procedure TTextStreamFile.WriteLn (const Data : string);
begin
  fWriteStream.WriteLine(Data);
end;

function TTextStreamFile.GetEOF : Boolean;
begin
  Result := fReadStream.EndOfStream;
end;

procedure TTextStreamFile.Close;
begin
  if Assigned(fReadStream) then fReadStream.Close;
  if Assigned(fWriteStream) then fWriteStream.Close;
end;

{other functions}

function CreateDummyFile(const aFilename : string; const aSize : Int64 ) : Boolean;
var
  fs : TFileStream;
  i : Integer;
  buf : string;
Begin
  Result := False;
  fs := TFileStream.Create(aFilename,fmCreate);
  buf := 'A';
  try
    fs.Seek(0, soBeginning);
    for i := 0 to aSize do fs.Write(buf[1], Length(buf));
  finally
    fs.Free;
  end;
  Result := FileExists(aFilename);
End;

procedure SplitFile(const aFileName : string; aSplitByteSize : Int64);
var
  fs, ss: TFileStream;
  cnt : integer;
  splitname: string;
begin
   fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite) ;
   try
     for cnt := 1 to Trunc(fs.Size / aSplitByteSize) + 1 do
     begin
       splitname := ChangeFileExt(aFileName, Format('%s%.3d', ['.',cnt])) ;
       ss := TFileStream.Create(splitname, fmCreate or fmShareExclusive) ;
       try
         if fs.Size - fs.Position < aSplitByteSize then
           aSplitByteSize := fs.Size - fs.Position;
         ss.CopyFrom(fs, aSplitByteSize) ;
       finally
         ss.Free;
       end;
     end;
   finally
     fs.Free;
   end;
end;

procedure MergeFiles(const aFirstSplitFileName, aOutFileName : string);
var
  fs, ss: TFileStream;
  cnt: integer;
  splitfilename : string;
begin
   cnt := 1;
   splitfilename := aFirstSplitFileName;
   fs := TFileStream.Create(aOutFileName, fmCreate or fmShareExclusive) ;
   try
     while FileExists(splitfilename) do
     begin
       ss := TFileStream.Create(splitfilename, fmOpenRead or fmShareDenyWrite) ;
       try
         fs.CopyFrom(ss, 0) ;
       finally
         ss.Free;
       end;
       Inc(cnt) ;
       splitfilename := ChangeFileExt(aFirstSplitFileName, Format('%s%.3d', ['.',cnt])) ;
     end;
   finally
     fs.Free;
   end;
end;

procedure MergeFiles(aFilenames : array of string; const aOutFileName : string);
var
  filename : string;
  fs,
  ss : TFileStream;
begin
   fs := TFileStream.Create(aOutFileName,fmCreate or fmShareExclusive) ;
   try
     for filename in aFilenames do
     begin
       if not FileExists(filename) then raise Exception.CreateFmt('Merge file %s not found!',[filename]);
       ss := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite) ;
       try
         fs.CopyFrom(ss,0);
       finally
         ss.Free;
       end;
     end;
   finally
     fs.Free;
   end;
end;

function IsFileInUse(const aFileName : string) : Boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(aFileName) then Exit;
  try
   HFileRes := CreateFile(PChar(aFileName),
    GENERIC_READ or GENERIC_WRITE
    ,0
    ,nil
    ,OPEN_EXISTING
    ,FILE_ATTRIBUTE_NORMAL
    ,0);

   Result := (HFileRes = INVALID_HANDLE_VALUE);

   if not(Result) then begin
     CloseHandle(HFileRes);
   end;
  except
    Result := True;
  end;
end;

procedure FileReplaceText(const aFileName, aSearchText, AReplaceText : string);
var
  fs: TFileStream;
  S: string;
begin
  fs := TFileStream.Create(aFileName, fmOpenread or fmShareDenyNone);
  try
    SetLength(S, fs.Size);
    fs.ReadBuffer(S[1], fs.Size);
  finally
    fs.Free;
  end;
  S  := StringReplace(S, aSearchText, AReplaceText, [rfReplaceAll, rfIgnoreCase]);
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    fs.WriteBuffer(S[1], Length(S));
  finally
    fs.Free;
  end;
end;

function FileSearchText(const aFileName, SearchText: string; caseSensitive : Boolean): Longint;
const
  BufferSize = $8001;
var
  {$IF CompilerVersion < 20}
    pBuf, pEnd, pScan, pPos: PChar;
  {$ELSE}
    pBuf, pEnd, pScan, pPos: PAnsiChar;
  {$ENDIF}
  filesize: LongInt;
  bytesRemaining: LongInt;
  bytesToRead: Integer;
  F: file;
  {$IF CompilerVersion < 20}
    SearchFor: PChar;
  {$ELSE}
    SearchFor: PAnsiChar;
  {$ENDIF}
  oldMode: Word;
begin
  Result := -1;
  if (Length(SearchText) = 0) or (Length(aFileName) = 0) then Exit;
  SearchFor := nil;
  pBuf      := nil;
  AssignFile(F, aFileName);
  oldMode  := FileMode;
  FileMode := 0;
  Reset(F, 1);
  FileMode := oldMode;
  try
    {$IF CompilerVersion < 20}
      SearchFor := StrAlloc(Length(SearchText) + 1);
    {$ELSE}
      SearchFor := PAnsiChar(StrAlloc(Length(SearchText) + 1));
    {$ENDIF}
    StrPCopy(SearchFor, SearchText);
    if not caseSensitive then AnsiUpper(SearchFor);
    GetMem(pBuf, BufferSize);
    filesize := System.Filesize(F);
    bytesRemaining := filesize;
    pPos := nil;
    while bytesRemaining > 0 do
    begin
      if bytesRemaining >= BufferSize then bytesToRead := Pred(BufferSize)
        else bytesToRead := bytesRemaining;
      BlockRead(F, pBuf^, bytesToRead, bytesToRead);
      pEnd := @pBuf[bytesToRead];
      pEnd^ := #0;
      pScan := pBuf;
      while pScan < pEnd do
      begin
        if not caseSensitive then AnsiUpper(pScan);
        pPos := StrPos(pScan, SearchFor);
        if pPos <> nil then
        begin
          Result := FileSize - bytesRemaining +
            Longint(pPos) - Longint(pBuf);
          Break;
        end;
        pScan := StrEnd(pScan);
        Inc(pScan);
      end;
      if pPos <> nil then Break;
      bytesRemaining := bytesRemaining - bytesToRead;
      if bytesRemaining > 0 then
      begin
        Seek(F, FilePos(F) - Length(SearchText));
        bytesRemaining := bytesRemaining + Length(SearchText);
      end;
    end;
  finally
    CloseFile(F);
    if SearchFor <> nil then StrDispose(SearchFor);
    if pBuf <> nil then FreeMem(pBuf, BufferSize);
  end;
end;

function GetLastAccessTime(const aFileName: string): TDateTime;
var
  ffd: TWin32FindData;
  dft: DWORD;
  lft: TFileTime;
  h:   THandle;
begin
  h := FindFirstFile(PChar(aFileName), ffd);
  if (INVALID_HANDLE_VALUE <> h) then
  begin
    FindClose(h);
    FileTimeToLocalFileTime(ffd.ftLastAccessTime, lft);
    FileTimeToDosDateTime(lft, LongRec(dft).Hi, LongRec(dft).Lo);
    Result := FileDateToDateTime(dft);
  end;
end;

function GetCreationTime(const aFilename : string): TDateTime;
var
  ffd: TWin32FindData;
  dft: DWORD;
  lft: TFileTime;
  h:   THandle;
begin
  h := FindFirstFile(PChar(aFilename), ffd);
  if (INVALID_HANDLE_VALUE <> h) then
  begin
    FindClose(h);
    FileTimeToLocalFileTime(ffd.ftCreationTime, lft);
    FileTimeToDosDateTime(lft, LongRec(dft).Hi, LongRec(dft).Lo);
    Result := FileDateToDateTime(dft);
  end;
end;

function GetLastModificationTime(const aFileName : string): TDateTime;
var
  ffd: TWin32FindData;
  dft: DWORD;
  lft: TFileTime;
  h:   THandle;
begin
  h := FindFirstFile(PChar(aFilename), ffd);
  if (INVALID_HANDLE_VALUE <> h) then
  begin
    FindClose(h);
    FileTimeToLocalFileTime(ffd.ftLastWriteTime, lft);
    FileTimeToDosDateTime(lft, LongRec(dft).Hi, LongRec(dft).Lo);
    Result := FileDateToDateTime(dft);
  end;
end;

end.
