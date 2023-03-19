{ ***************************************************************************

  Copyright (c) 2016-2019 Kike Pérez

  Unit        : Quick.Files
  Description : Files functions
  Author      : Kike Pérez
  Version     : 1.5
  Created     : 09/03/2018
  Modified    : 16/11/2020

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

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF FPC}
  strutils,
    {$IFDEF LINUX}
    baseunix,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Base,
  Posix.SysStat,
  Posix.Utime,
  {$ENDIF}
  DateUtils;

{$IFDEF FPC}
resourcestring

  SPathTooLong = 'The specified path is too long';
  SPathNotFound = 'The specified path was not found';
  SPathFormatNotSupported = 'The path format is not supported';
  SDirectoryNotEmpty = 'The specified directory is not empty';
  SDirectoryAlreadyExists = 'The specified directory already exists';
  SDirectoryInvalid = 'The specified directory name is invalid';
  SSourceDirIsDestDir = 'The source directory is the same as the destination directory';
  SSourceFileIsDestFile = 'The source file is the same as the destination file';
  SPathToFileNeeded = 'The path must specify a file';
  SSameRootDrive = 'The source and destination paths must contain the same root drive';
  SDriveNotFound = 'The drive cannot be found';
  SFileNotFound = 'The specified file was not found';
  SFileAlreadyExists = 'The specified file already exists';
  SInvalidCharsInPath = 'Invalid characters in path';
  SInvalidCharsInFileName = 'Invalid characters in file name';
{$ENDIF}

type


  {$IFNDEF FPC}
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
    {$IF Defined(MACOS) OR Defined(NEXTGEN) OR Defined(DELPHILINUX)}
    TFileTime = LongInt;
    {$ENDIF}
  {$ELSE}
    {$IFDEF LINUX}
    TFILETIME = LongInt;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC}
  EStreamError = class(Exception);
  EFileStreamError = class(EStreamError)
    constructor Create(ResStringRec: PResStringRec; const FileName: string);
  end;

  TPathPrefixType = (pptNoPrefix, pptExtended, pptExtendedUNC);

  { TPath }

  TPath = class
  private
    const
      FCCurrentDir: string = '.';
      FCParentDir: string = '..';
      FCExtendedPrefix: string = '\\?\';
      FCExtendedUNCPrefix: string = '\\?\UNC\';
    class procedure CheckPathLength(const Path: string; const MaxLength: Integer);
  public
    class function GetFileNameWithoutExtension(const FileName : string) : string;
    class function GetDirectoryName(const FileName : string) : string;
    class function GetExtension(const Path : string) : string;
    class function ChangeExtension(const Path, NewExtension : string) : string;
    class function GetFileName(const aPath : string) : string;
    class function EndsWithDelimiter(const aPath : string) : Boolean;
    class function Combine(const aPath1, aPath2 : string) : string;
  end;

  TDirectory = class
  public
    class function Exists(const Path: string; FollowLink: Boolean = True): Boolean;
    class function GetDirectories(const Path : string) : TArray<string>;
  end;

  TFile = class
  public
    class function Exists(const Path : string) : Boolean;
    class function IsInUse(const Path : string) : Boolean;
    class function GetSize(const Path : string) : Int64;
    class function Create(const Path: string; const BufferSize: Integer): TFileStream; overload;
    class function Create(const Path: string): TFileStream; overload;
    class function GetExtension(const Path : string) : string;
    class function GetCreationTime(const Path : string): TDateTime;
    class function GetLastAccessTime(const Path : string): TDateTime;
    class function GetLastWriteTime(const Path : string): TDateTime;
    class procedure SetCreationTime(const Path: string; const CreationTime: TDateTime);
    class procedure SetLastAccessTime(const Path: string; const LastAccessTime: TDateTime);
    class procedure SetLastWriteTime(const Path: string; const LastWriteTime: TDateTime);
    class function IsReadOnly(const Path : string) : Boolean;
    class function Delete(const Path : string) : Boolean;
    class function Move(const SourceFileName, DestFileName: string) : Boolean;
  end;

  TTextWriter = class
  public
    procedure Close; virtual; abstract;
    procedure Flush; virtual; abstract;
    procedure Write(Value: Boolean); overload; virtual; abstract;
    procedure Write(Value: Char); overload; virtual; abstract;
    procedure Write(Value: Double); overload; virtual; abstract;
    procedure Write(Value: Integer); overload; virtual; abstract;
    procedure Write(Value: Int64); overload; virtual; abstract;
    procedure Write(Value: TObject); overload; virtual; abstract;
    procedure Write(Value: Single); overload; virtual; abstract;
    procedure Write(const Value: string); overload; virtual; abstract;
    procedure Write(const aFormat: string; Args: array of const); overload; virtual; abstract;
    procedure WriteLine; overload; virtual; abstract;
    procedure WriteLine(Value: Boolean); overload; virtual; abstract;
    procedure WriteLine(Value: Char); overload; virtual; abstract;
    procedure WriteLine(Value: Double); overload; virtual; abstract;
    procedure WriteLine(Value: Integer); overload; virtual; abstract;
    procedure WriteLine(Value: Int64); overload; virtual; abstract;
    procedure WriteLine(Value: TObject); overload; virtual; abstract;
    procedure WriteLine(Value: Single); overload; virtual; abstract;
    procedure WriteLine(const Value: string); overload; virtual; abstract;
    procedure WriteLine(const aFormat: string; Args: array of const); overload; virtual; abstract;
  end;

  TStreamWriter = class(TTextWriter)
  private
    FStream: TStream;
    FEncoding: TEncoding;
    FNewLine: string;
    FAutoFlush: Boolean;
    FOwnsStream: Boolean;
    FBufferIndex: Integer;
    FBuffer: TBytes;
    procedure WriteBytes(Bytes: TBytes);
  public
    constructor Create(Stream: TStream); overload;
    constructor Create(Stream: TStream; Encoding: TEncoding; BufferSize: Integer = 4096); overload;
    constructor Create(const Filename: string; Append: Boolean = False); overload;
    constructor Create(const Filename: string; Append: Boolean; Encoding: TEncoding; BufferSize: Integer = 4096); overload;
    destructor Destroy; override;
    procedure Close; override;
    procedure Flush; override;
    procedure OwnStream; inline;
    procedure Write(Value: Boolean); override;
    procedure Write(Value: Char); override;
    procedure Write(Value: Double); override;
    procedure Write(Value: Integer); override;
    procedure Write(Value: Int64); override;
    procedure Write(Value: TObject); override;
    procedure Write(Value: Single); override;
    procedure Write(const Value: string); override;
    procedure Write(const aFormat: string; Args: array of const); override;
    procedure WriteLine; override;
    procedure WriteLine(Value: Boolean); override;
    procedure WriteLine(Value: Char); override;
    procedure WriteLine(Value: Double); override;
    procedure WriteLine(Value: Integer); override;
    procedure WriteLine(Value: Int64); override;
    procedure WriteLine(Value: TObject); override;
    procedure WriteLine(Value: Single); override;
    procedure WriteLine(const Value: string); override;
    procedure WriteLine(const aFormat: string; Args: array of const); override;
    property AutoFlush: Boolean read FAutoFlush write FAutoFlush;
    property NewLine: string read FNewLine write FNewLine;
    property Encoding: TEncoding read FEncoding;
    property BaseStream: TStream read FStream;
  end;
  {$ENDIF FPC}

  TDirItem = record
  private
    fName : string;
    fIsDirectory : Boolean;
    fSize : Int64;
    fCreationDate : TDateTime;
    fLastModified : TDateTime;
  public
    property Name : string read fName write fName;
    property IsDirectory : Boolean read fIsDirectory write fIsDirectory;
    property Size : Int64 read fSize write fSize;
    property CreationDate : TDateTime read fCreationDate write fCreationDate;
    property LastModified : TDateTime read fLastModified write fLastModified;
  end;

  {$IFNDEF FPC}
  TDirItemAddProc = reference to procedure(const diritem : TDirItem);
  {$ELSE}
  TDirItemAddProc = procedure(const diritem : TDirItem);
  {$ENDIF}

  function CreateDummyFile(const aFilename : string; const aSize : Int64) : Boolean;
  procedure SplitFile(const aFileName : string; aSplitByteSize : Int64);
  procedure MergeFiles(const aFirstSplitFileName, aOutFileName : string); overload;
  procedure MergeFiles(aFilenames : array of string; const aOutFileName : string); overload;
  {$IFNDEF NEXTGEN}
  function IsFileInUse(const aFileName : string) : Boolean;
  {$ENDIF}
  procedure FileReplaceText(const aFileName, aSearchText, AReplaceText : string);
  {$IFNDEF NEXTGEN}
  function FileSearchText(const aFileName, SearchText: string; caseSensitive : Boolean): Longint;
  {$ENDIF}
  function GetCreationTime(const aFilename : string): TDateTime;
  function GetLastAccessTime(const aFileName: string): TDateTime;
  function GetLastWriteTime(const aFileName : string): TDateTime;
  {$IFDEF FPC}
  function FindDelimiter(const Delimiters, S: string; StartIdx: Integer = 1): Integer;
  {$ENDIF}
  function ConvertDateTimeToFileTime(const DateTime: TDateTime; const UseLocalTimeZone: Boolean): TFileTime;
  {$IFDEF MSWINDOWS}
  function ConvertFileTimeToDateTime(const FileTime : TFileTime; const UseLocalTimeZone : Boolean) : TDateTime;
  {$ENDIF}
  procedure SetDateTimeInfo(const Path: string; const CreationTime, LastAccessTime, LastWriteTime: PDateTime; const UseLocalTimeZone: Boolean);
  function GetFiles(const Path : string; Recursive : Boolean) : TArray<TDirItem>; overload;
  procedure GetFiles(const Path : string; aAddToList : TDirItemAddProc; Recursive : Boolean); overload;
  function GetDirectories(const Path : string; Recursive : Boolean) : TArray<TDirItem>;
  function GetFilesAndDirectories(const Path : string; Recursive : Boolean) : TArray<TDirItem>; overload;
  procedure GetFilesAndDirectories(const Path : string; aAddToList : TDirItemAddProc; Recursive : Boolean); overload;

implementation

{ TTextStreamFile }

{$IFNDEF FPC}
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

{$ENDIF NFPC}

{$IFDEF FPC}

{ EFileStreamError }

constructor EFileStreamError.Create(ResStringRec: PResStringRec;
  const FileName: string);
begin
  {$IFNDEF LINUX}
  inherited CreateResFmt(ResStringRec, [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);
  {$ELSE}
  inherited CreateResFmt(ResStringRec, [ExpandFileName(FileName), SysErrorMessage(errno)]);
  {$ENDIF}
end;

{ TPath }

class function TPath.GetFileNameWithoutExtension(const FileName: string
  ): string;
var
  fname : string;
begin
  fname := ExtractFileName(FileName);
  Result := Copy(fname, 1, Length(fname) - Length(ExtractFileExt(fname)));
end;

class function TPath.ChangeExtension(const Path, NewExtension : string) : string;
var
  dot : string;
begin
  if NewExtension.Contains('.') then dot := ''
    else dot := '.';
  Result := TPath.GetFileNameWithoutExtension(Path) + dot + NewExtension;
end;

class function TPath.GetFileName(const aPath: string): string;
begin
  Result := ExtractFileName(aPath);
end;

class function TPath.GetDirectoryName(const FileName : string) : string;
begin
  Result := ExtractFileDir(Filename);
end;

class procedure TPath.CheckPathLength(const Path: string; const MaxLength: Integer);
begin
{$IFDEF MSWINDOWS}
  if (Length(Path) >= MaxLength) then
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  if (Length(UTF8Encode(Path)) >= MaxLength) then
{$ENDIF POSIX}
    raise EPathTooLongException.CreateRes(@SPathTooLong);
end;

class function TPath.GetExtension(const Path : string) : string;
begin
  Result := ExtractFileExt(Path);
end;

class function TPath.EndsWithDelimiter(const aPath : string) : Boolean;
var
  c : Char;
begin
  if aPath = '' then Exit(False);
  c := aPath[High(aPath)];
  Result := (c = '\') or (c = '/');
end;

class function TPath.Combine(const aPath1, aPath2 : string) : string;
var
  delim : string;
begin
  delim := '';
  if aPath1.Contains('/') then delim := '/'
     else if aPath1.Contains('\') then delim := '\';
  if delim = '' then
  begin
    {$IFDEF LINUX}
    delim := '/';
    {$ELSE}
    delim := '\';
    {$ENDIF}
  end;
  if EndsWithDelimiter(aPath1) then
  begin
    if EndsWithDelimiter(aPath2) then Result := aPath1 + Copy(aPath2,2,aPath2.Length)
      else Result := aPath1 + aPath2;
  end
  else
  begin
    if EndsWithDelimiter(aPath2) then Result := aPath1 + aPath2
      else Result := aPath1 + delim + aPath2;
  end;
end;

{ TDirectory }

class function TDirectory.Exists(const Path: string; FollowLink: Boolean = True): Boolean;
begin
  Result := DirectoryExists(Path);
end;

class function TDirectory.GetDirectories(const Path : string) : TArray<string>;
var
  rec : TSearchRec;
begin
  if FindFirst(TPath.Combine(Path,'*'),faAnyFile and faDirectory,rec) = 0 then
  repeat
    if ((rec.Attr and faDirectory) = faDirectory) and (rec.Name <> '.') and (rec.Name <> '..') then
    begin
      Result := Result + [rec.Name];
    end;
  until FindNext(rec) <> 0;
  SysUtils.FindClose(rec);
end;

{ TFile }

class function TFile.Exists(const Path : string) : Boolean;
begin
  Result := FileExists(Path);
end;

class procedure TFile.SetCreationTime(const Path: string; const CreationTime: TDateTime);
begin
  SetDateTimeInfo(Path,@CreationTime,nil,nil,True);
end;

class procedure TFile.SetLastAccessTime(const Path: string; const LastAccessTime: TDateTime);
begin
  SetDateTimeInfo(Path,nil,@LastAccessTime,nil,True);
end;

class procedure TFile.SetLastWriteTime(const Path: string; const LastWriteTime: TDateTime);
begin
  SetDateTimeInfo(Path,nil,nil,@LastWriteTime,True);
end;

class function TFile.IsReadOnly(const Path : string) : Boolean;
begin
  Result := FileIsReadOnly(Path);
end;

class function TFile.Delete(const Path : string) : Boolean;
begin
  Result := DeleteFile(PChar(Path));
end;

class function TFile.Move(const SourceFileName, DestFileName: string) : Boolean;
begin
  {$IFNDEF LINUX}
  Result := MoveFile(PChar(SourceFileName),PChar(DestFileName));
  {$ELSE}
  Result := RenameFile(PChar(SourceFileName),PChar(DestFileName));
  {$ENDIF}
end;

{$IFNDEF NEXTGEN}
class function TFile.IsInUse(const Path : string) : Boolean;
begin
  Result := IsFileInUse(Path);
end;
{$ENDIF}

class function TFile.GetSize(const Path : string) : Int64;
var
  f : File of Byte;
begin
  Assign(f,Path);
  try
    Reset (f);
    Result := FileSize(f);
  finally
    CloseFile(f);
  end;
end;

class function TFile.GetExtension(const Path : string) : string;
begin
  Result := ExtractFileExt(Path);
end;

class function TFile.GetCreationTime(const Path : string) : TDateTime;
begin
  Result := Quick.Files.GetCreationTime(Path);
end;

class function TFile.GetLastAccessTime(const Path : string) : TDateTime;
begin
  Result := Quick.Files.GetLastAccessTime(Path);
end;

class function TFile.GetLastWriteTime(const Path : string) : TDateTime;
begin
  Result := Quick.Files.GetLastWriteTime(Path);
end;

class function TFile.Create(const Path: string; const BufferSize: Integer): TFileStream;
begin
  try
    Result := TFileStream.Create(Path,fmCreate);
  except
    on E: EFileStreamError do
      raise EInOutError.Create(E.Message);
  end;
end;

class function TFile.Create(const Path: string): TFileStream;
begin
  Result := Create(Path, 0);
end;

{ TStreamWriter }

procedure TStreamWriter.Close;
begin
  Flush;
  if FOwnsStream  then
    FreeAndNil(FStream);
end;

constructor TStreamWriter.Create(Stream: TStream);
begin
  inherited Create;
  FOwnsStream := False;
  FStream := Stream;
  FEncoding := TEncoding.UTF8;
  SetLength(FBuffer, 1024);
  FBufferIndex := 0;
  FNewLine := sLineBreak;
  FAutoFlush := True;
end;

constructor TStreamWriter.Create(Stream: TStream; Encoding: TEncoding; BufferSize: Integer);
begin
  inherited Create;
  FOwnsStream := False;
  FStream := Stream;
  FEncoding := Encoding;
  if BufferSize >= 128 then
    SetLength(FBuffer, BufferSize)
  else
    SetLength(FBuffer, 128);
  FBufferIndex := 0;
  FNewLine := sLineBreak;
  FAutoFlush := True;
  if Stream.Position = 0 then
    WriteBytes(FEncoding.GetPreamble);
end;

constructor TStreamWriter.Create(const Filename: string; Append: Boolean);
begin
  if (not FileExists(Filename)) or (not Append) then
    FStream := TFileStream.Create(Filename, fmCreate)
  else
  begin
    FStream := TFileStream.Create(Filename, fmOpenWrite);
    FStream.Seek(0, soEnd);
  end;
  Create(FStream);
  FOwnsStream := True;
end;

constructor TStreamWriter.Create(const Filename: string; Append: Boolean;
  Encoding: TEncoding; BufferSize: Integer);
begin
  if (not FileExists(Filename)) or (not Append) then
    FStream := TFileStream.Create(Filename, fmCreate)
  else
  begin
    FStream := TFileStream.Create(Filename, fmOpenWrite);
    FStream.Seek(0, soEnd);
  end;
  Create(FStream, Encoding, BufferSize);
  FOwnsStream := True;
end;

destructor TStreamWriter.Destroy;
begin
  Close;
  SetLength(FBuffer, 0);
  inherited;
end;

procedure TStreamWriter.Flush;
begin
  if FBufferIndex = 0 then
    Exit;
  if FStream = nil then
    Exit;

  FStream.Write(FBuffer[0], FBufferIndex);
  FBufferIndex := 0;
end;

procedure TStreamWriter.OwnStream;
begin
  FOwnsStream := True;
end;

procedure TStreamWriter.Write(const Value: string);
begin
  WriteBytes(FEncoding.GetBytes(Value));
end;

procedure TStreamWriter.WriteBytes(Bytes: TBytes);
var
  ByteIndex: Integer;
  WriteLen: Integer;
begin
  ByteIndex := 0;

  while ByteIndex < Length(Bytes) do
  begin
    WriteLen := Length(Bytes) - ByteIndex;
    if WriteLen > Length(FBuffer) - FBufferIndex then
      WriteLen := Length(FBuffer) - FBufferIndex;

    Move(Bytes[ByteIndex], FBuffer[FBufferIndex], WriteLen);

    Inc(FBufferIndex, WriteLen);
    Inc(ByteIndex, WriteLen);

    if FBufferIndex >= Length(FBuffer) then
      Flush;
  end;

  if FAutoFlush then
    Flush;
end;

procedure TStreamWriter.Write(const aFormat: string; Args: array of const);
begin
  WriteBytes(FEncoding.GetBytes(Format(aFormat, Args)));
end;

procedure TStreamWriter.Write(Value: Single);
begin
  WriteBytes(FEncoding.GetBytes(FloatToStr(Value)));
end;

procedure TStreamWriter.Write(Value: Double);
begin
  WriteBytes(FEncoding.GetBytes(FloatToStr(Value)));
end;

procedure TStreamWriter.Write(Value: Integer);
begin
  WriteBytes(FEncoding.GetBytes(IntToStr(Value)));
end;

procedure TStreamWriter.Write(Value: Char);
begin
  WriteBytes(FEncoding.GetBytes(Value));
end;

procedure TStreamWriter.Write(Value: TObject);
begin
  WriteBytes(FEncoding.GetBytes(Value.ToString));
end;

procedure TStreamWriter.Write(Value: Int64);
begin
  WriteBytes(FEncoding.GetBytes(IntToStr(Value)));
end;

procedure TStreamWriter.Write(Value: Boolean);
begin
  WriteBytes(FEncoding.GetBytes(BoolToStr(Value, True)));
end;

procedure TStreamWriter.WriteLine(Value: Double);
begin
  WriteBytes(FEncoding.GetBytes(FloatToStr(Value) + FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Integer);
begin
  WriteBytes(FEncoding.GetBytes(IntToStr(Value) + FNewLine));
end;

procedure TStreamWriter.WriteLine;
begin
  WriteBytes(FEncoding.GetBytes(FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Boolean);
begin
  WriteBytes(FEncoding.GetBytes(BoolToStr(Value, True) + FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Char);
begin
  WriteBytes(FEncoding.GetBytes(Value));
  WriteBytes(FEncoding.GetBytes(FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Int64);
begin
  WriteBytes(FEncoding.GetBytes(IntToStr(Value) + FNewLine));
end;

procedure TStreamWriter.WriteLine(const aFormat: string; Args: array of const);
begin
  WriteBytes(FEncoding.GetBytes(Format(aFormat, Args) + FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: TObject);
begin
  WriteBytes(FEncoding.GetBytes(Value.ToString + FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Single);
begin
  WriteBytes(FEncoding.GetBytes(FloatToStr(Value) + FNewLine));
end;

procedure TStreamWriter.WriteLine(const Value: string);
begin
  WriteBytes(FEncoding.GetBytes(Value + FNewLine));
end;

{$ENDIF FPC}

{other functions}

function CreateDummyFile(const aFilename : string; const aSize : Int64 ) : Boolean;
var
  fs : TFileStream;
  i : Integer;
  buf : string;
Begin
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
{$IF NOT Defined(LINUX) AND NOT Defined(MACOS) AND NOT Defined(ANDROID)}
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
{$ELSE}
var
  fs : TFileStream;
begin
  try
    fs := TFileStream.Create(aFileName, fmOpenReadWrite, fmShareExclusive);
    Result := True;
    fs.Free;
  except
    Result := False;
  end;

end;
{$ENDIF}

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

{$IFNDEF NEXTGEN}
function FileSearchText(const aFileName, SearchText: string; caseSensitive : Boolean): Longint;
const
  BufferSize = $8001;
var
  pBuf, pEnd, pScan, pPos: PAnsiChar;
  filesize: LongInt;
  bytesRemaining: LongInt;
  bytesToRead: Integer;
  F: file;
  SearchFor: PAnsiChar;
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
    {$IFDEF FPC}
    SearchFor := PChar(StrAlloc(Length(SearchText) + 1));
    {$ELSE}
    {$IFDEF DELPHI2010_UP}
    SearchFor := PAnsiChar(StrAlloc(Length(SearchText) + 1));
    {$ELSE}
      SearchFor := StrAlloc(Length(SearchText) + 1);
    {$ENDIF}
    {$ENDIF FPC}
    StrPCopy(SearchFor, SearchText);
    {$IFDEF FPC}
    if not caseSensitive then UpperCase(SearchFor);
    {$ELSE}
    if not caseSensitive then AnsiUpperCase(SearchFor);
    {$ENDIF}
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
        {$IFDEF FPC}
        if not caseSensitive then UpperCase(pScan);
        {$ELSE}
        if not caseSensitive then AnsiUpperCase(pScan);
        {$ENDIF}
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
{$ENDIF}

{$IFDEF MSWINDOWS}
function GetLastAccessTime(const aFileName: string): TDateTime;
var
  ffd: TWin32FindData;
  dft: DWORD;
  lft: TFileTime;
  h:   THandle;
begin
  Result := 0;
  {$IFDEF FPC}
  h := FindFirstFile(PAnsiChar(aFileName), ffd);
  {$ELSE}
  h := FindFirstFile(PChar(aFileName), ffd);
  {$ENDIF}
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
  Result := 0;
  {$IFDEF FPC}
  h := FindFirstFile(PAnsiChar(aFileName), ffd);
  {$ELSE}
  h := FindFirstFile(PChar(aFileName), ffd);
  {$ENDIF}
  if (INVALID_HANDLE_VALUE <> h) then
  begin
    FindClose(h);
    FileTimeToLocalFileTime(ffd.ftCreationTime, lft);
    FileTimeToDosDateTime(lft, LongRec(dft).Hi, LongRec(dft).Lo);
    Result := FileDateToDateTime(dft);
  end;
end;

function GetLastWriteTime(const aFileName : string): TDateTime;
var
  ffd: TWin32FindData;
  dft: DWORD;
  lft: TFileTime;
  h:   THandle;
begin
  Result := 0;
  {$IFDEF FPC}
  h := FindFirstFile(PAnsiChar(aFileName), ffd);
  {$ELSE}
  h := FindFirstFile(PChar(aFileName), ffd);
  {$ENDIF}
  if (INVALID_HANDLE_VALUE <> h) then
  begin
    FindClose(h);
    FileTimeToLocalFileTime(ffd.ftLastWriteTime, lft);
    FileTimeToDosDateTime(lft, LongRec(dft).Hi, LongRec(dft).Lo);
    Result := FileDateToDateTime(dft);
  end;
end;
{$ELSE}
  {$IFDEF FPC} //FPC Linux
  function GetLastAccessTime(const aFileName: string): TDateTime;
  var
    info : stat;
  begin
    Result := 0;
    if fpstat(aFileName,info) <> 0 then
    begin
      Result := info.st_atime;
    end;
  end;

  function GetCreationTime(const aFilename : string): TDateTime;
  var
    info : stat;
  begin
    Result := 0;
    if fpstat(aFileName,info) <> 0 then
    begin
      Result := info.st_ctime;
    end;
  end;

  function GetLastWriteTime(const aFileName : string): TDateTime;
  var
    info : stat;
  begin
    Result := 0;
    if fpstat(aFileName,info) <> 0 then
    begin
      Result := info.st_mtime;
    end;
  end;
  {$ELSE} //Delphi Nextgen & Linux
  function GetLastAccessTime(const aFileName: string): TDateTime;
  var
    info : TDateTimeInfoRec;
  begin
    if FileGetDateTimeInfo(aFileName,info,True) then Result := info.LastAccessTime
      else Result := 0.0;
  end;

  function GetCreationTime(const aFilename : string): TDateTime;
   var
    info : TDateTimeInfoRec;
  begin
    if FileGetDateTimeInfo(aFileName,info,True) then Result := info.CreationTime
      else Result := 0.0;
  end;

  function GetLastWriteTime(const aFileName : string): TDateTime;
   var
    info : TDateTimeInfoRec;
  begin
    if FileGetDateTimeInfo(aFileName,info,True) then Result := info.TimeStamp
      else Result := 0.0;
  end;
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
function FindDelimiter(const Delimiters, S: string; StartIdx: Integer = 1): Integer;
var
  Stop: Boolean;
  Len: Integer;
begin
  Result := 0;

  Len := S.Length;
  Stop := False;
  while (not Stop) and (StartIdx <= Len) do
    if IsDelimiter(Delimiters, S, StartIdx) then
    begin
      Result := StartIdx;
      Stop := True;
    end
    else
      Inc(StartIdx);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function ConvertDateTimeToFileTime(const DateTime: TDateTime; const UseLocalTimeZone: Boolean): TFileTime;
var
  LFileTime: TFileTime;
  SysTime: TSystemTime;
begin
  Result.dwLowDateTime := 0;
  Result.dwLowDateTime := 0;
  DecodeDateTime(DateTime, SysTime.wYear, SysTime.wMonth, SysTime.wDay,
    SysTime.wHour, SysTime.wMinute, SysTime.wSecond, SysTime.wMilliseconds);

  if SystemTimeToFileTime(SysTime, LFileTime) then
    if UseLocalTimeZone then
      LocalFileTimeToFileTime(LFileTime, Result)
    else
      Result := LFileTime;
end;
function ConvertFileTimeToDateTime(const FileTime : TFileTime; const UseLocalTimeZone : Boolean) : TDateTime;
var
  dft: DWORD;
  lft: TFileTime;
begin
  FileTimeToLocalFileTime(FileTime, lft);
  FileTimeToDosDateTime(lft, LongRec(dft).Hi, LongRec(dft).Lo);
  Result := FileDateToDateTime(dft);
end;


{$ENDIF}
{$If Defined(FPC) AND Defined(LINUX)}
function ConvertDateTimeToFileTime(const DateTime: TDateTime; const UseLocalTimeZone: Boolean): TFileTime;
begin
  { Use the time zone if necessary }
  if not UseLocalTimeZone then
    Result := DateTimeToFileDate(DateTime)
  else
    Result := DateTimeToFileDate(DateTime);
end;
{$ENDIF}
{$IFDEF POSIX}
function ConvertDateTimeToFileTime(const DateTime: TDateTime; const UseLocalTimeZone: Boolean): TFileTime;
begin
  { Use the time zone if necessary }
  if not UseLocalTimeZone then
    Result := DateTimeToFileDate(TTimeZone.Local.ToLocalTime(DateTime))
  else
    Result := DateTimeToFileDate(DateTime);
end;
{$ENDIF}

procedure SetDateTimeInfo(const Path: string; const CreationTime, LastAccessTime, LastWriteTime: PDateTime; const UseLocalTimeZone: Boolean);
{$IFDEF MSWINDOWS}
var
  LFileHnd: THandle;
  LFileAttr: Cardinal;
  LFileCreationTime: PFileTime;
  LFileLastAccessTime: PFileTime;
  LFileLastWriteTime: PFileTime;
begin
  // establish what date-times must be set to the directory
  LFileHnd := 0;
  LFileCreationTime := nil;
  LFileLastAccessTime := nil;
  LFileLastWriteTime := nil;

  try
    try
      if Assigned(CreationTime) then
      begin
        New(LFileCreationTime);
        LFileCreationTime^ := ConvertDateTimeToFileTime(CreationTime^, UseLocalTimeZone);
      end;
      if Assigned(LastAccessTime) then
      begin
        New(LFileLastAccessTime);
        LFileLastAccessTime^ := ConvertDateTimeToFileTime(LastAccessTime^, UseLocalTimeZone);
      end;
      if Assigned(LastWriteTime) then
      begin
        New(LFileLastWriteTime);
        LFileLastWriteTime^ := ConvertDateTimeToFileTime(LastWriteTime^, UseLocalTimeZone);
      end;

      // determine if Path points to a directory or a file
      SetLastError(ERROR_SUCCESS);
      LFileAttr := FileGetAttr(Path);
      if LFileAttr and faDirectory <> 0 then
        LFileAttr := FILE_FLAG_BACKUP_SEMANTICS
      else
        LFileAttr := FILE_ATTRIBUTE_NORMAL;

      // set the new date-times to the directory or file
      LFileHnd := CreateFile(PChar(Path), GENERIC_WRITE, FILE_SHARE_WRITE, nil,
        OPEN_EXISTING, LFileAttr, 0);

      if LFileHnd <> INVALID_HANDLE_VALUE then
        SetFileTime(LFileHnd, LFileCreationTime, LFileLastAccessTime, LFileLastWriteTime);
    except
      on E: EConvertError do
        raise EArgumentOutOfRangeException.Create(E.Message);
    end;
  finally
    CloseHandle(LFileHnd);
    SetLastError(ERROR_SUCCESS);

    Dispose(LFileCreationTime);
    Dispose(LFileLastAccessTime);
    Dispose(LFileLastWriteTime);
  end;
end;
{$ENDIF}
{$IFDEF POSIX}
var
  LFileName: Pointer;
  LStatBuf: _stat;
  LBuf: utimbuf;
  ErrCode: Integer;
  M: TMarshaller;
begin
  { Do nothing if no date/time passed. Ignore CreationTime. Unixes do not support creation times for files. }
  if (LastAccessTime = nil) and (LastWriteTime = nil) then
    Exit;

  LFileName := M.AsAnsi(Path, CP_UTF8).ToPointer;

  { Obtain the file times. lstat may fail }
  if ((LastAccessTime = nil) or (LastWriteTime = nil)) then
  begin
    ErrCode := stat(LFileName, LStatBuf);

    { Fail if we can't access the file properly }
    if ErrCode <> 0 then
      Exit; // Fail here prematurely. Do not chnage file times if we failed to fetch the old ones.
  end;

  try
    { Preserve of set the new value }
    if LastAccessTime <> nil then
      LBuf.actime := ConvertDateTimeToFileTime(LastAccessTime^, UseLocalTimeZone)
    else
      LBuf.actime := LStatBuf.st_atime;

    { Preserve of set the new value }
    if LastWriteTime <> nil then
      LBuf.modtime := ConvertDateTimeToFileTime(LastWriteTime^, UseLocalTimeZone)
    else
      LBuf.modtime := LStatBuf.st_mtime;

    { Call utime to set the file times }
    utime(LFileName, LBuf);
  except
    on E: EConvertError do // May rise in ConvertDateTimeToFileTime
      raise EArgumentOutOfRangeException.Create(E.Message);
  end;
end;
{$ENDIF}
{$if Defined(FPC) AND Defined(LINUX)}
begin
end;
{$ENDIF}

function GetFiles(const Path : string; Recursive : Boolean) : TArray<TDirItem>;
var
  rec : TSearchRec;
  diritem : TDirItem;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Path) + '*', faAnyFile, rec) = 0 then
  try
    repeat
      if (rec.Attr and faDirectory) <> faDirectory then
      begin
        diritem.Name := rec.Name;
        diritem.IsDirectory := False;
        diritem.Size := rec.Size;
        {$IFNDEF LINUX}
        diritem.CreationDate := ConvertFileTimeToDateTime(rec.FindData.ftCreationTime,True);
        diritem.LastModified := ConvertFileTimeToDateTime(rec.FindData.ftLastWriteTime,True);
        {$ELSE}
        diritem.CreationDate := FileDateToDateTime(rec.Time);
        {$ENDIF}
        Result := Result + [diritem];
      end
      else
      begin
        if Recursive then Result := Result + GetFiles(IncludeTrailingPathDelimiter(Path) + diritem.Name,Recursive);
      end;
    until FindNext(rec) <> 0;
  finally
    SysUtils.FindClose(rec);
  end;
end;

procedure GetFiles(const Path : string; aAddToList : TDirItemAddProc; Recursive : Boolean);
var
  rec : TSearchRec;
  diritem : TDirItem;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Path) + '*', faAnyFile, rec) = 0 then
  try
    repeat
      if (rec.Attr and faDirectory) <> faDirectory then
      begin
        diritem.Name := rec.Name;
        diritem.IsDirectory := False;
        diritem.Size := rec.Size;
       {$IFNDEF LINUX}
        diritem.CreationDate := ConvertFileTimeToDateTime(rec.FindData.ftCreationTime,True);
        diritem.LastModified := ConvertFileTimeToDateTime(rec.FindData.ftLastWriteTime,True);
        {$ELSE}
        diritem.CreationDate := FileDateToDateTime(rec.Time);
        {$ENDIF}
        aAddToList(diritem);
      end
      else
      begin
        if Recursive then GetFiles(IncludeTrailingPathDelimiter(Path) + diritem.Name,aAddToList,Recursive);
      end;
    until FindNext(rec) <> 0;
  finally
    SysUtils.FindClose(rec);
  end;
end;

function GetDirectories(const Path : string; Recursive : Boolean) : TArray<TDirItem>;
var
  rec : TSearchRec;
  diritem : TDirItem;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Path) + '*', faAnyFile, rec) = 0 then
  try
    repeat
      if ((rec.Attr and faDirectory) = faDirectory) and (rec.Name <> '.') and (rec.Name <> '..') then
      begin
        diritem.Name := rec.Name;
        diritem.IsDirectory := True;
        diritem.Size := rec.Size;
        {$IFNDEF LINUX}
        diritem.CreationDate := ConvertFileTimeToDateTime(rec.FindData.ftCreationTime,True);
        diritem.LastModified := ConvertFileTimeToDateTime(rec.FindData.ftLastWriteTime,True);
        {$ELSE}
        diritem.CreationDate := FileDateToDateTime(rec.Time);
        {$ENDIF}
        Result := Result + [diritem];
        if Recursive then Result := Result + GetFiles(IncludeTrailingPathDelimiter(Path) + diritem.Name,Recursive);
      end;
    until FindNext(rec) <> 0;
  finally
    SysUtils.FindClose(rec);
  end;
end;

function GetFilesAndDirectories(const Path : string; Recursive : Boolean) : TArray<TDirItem>;
var
  rec : TSearchRec;
  diritem : TDirItem;
  dirpath : string;
  wildcard : string;
begin
  if Path.Contains('*') then
  begin
    dirpath := ExtractFilePath(Path);
    wildcard := ExtractFileName(Path);
  end
  else
  begin
    dirpath := Path;
    wildcard := '*';
  end;
  dirpath := IncludeTrailingPathDelimiter(dirpath);


  if FindFirst(dirpath + wildcard, faAnyFile, rec) = 0 then
  try
    repeat
      if (rec.Attr and faDirectory) <> faDirectory then
      begin
        diritem.Name := rec.Name;
        diritem.IsDirectory := False;
        diritem.Size := rec.Size;
        {$IFNDEF LINUX}
        diritem.CreationDate := ConvertFileTimeToDateTime(rec.FindData.ftCreationTime,True);
        diritem.LastModified := ConvertFileTimeToDateTime(rec.FindData.ftLastWriteTime,True);
        {$ELSE}
        diritem.CreationDate := FileDateToDateTime(rec.Time);
        {$ENDIF}
        Result := Result + [diritem];
      end
      else if (rec.Name <> '.') and (rec.Name <> '..') then
      begin
        diritem.Name := rec.Name;
        diritem.IsDirectory := True;
        diritem.Size := rec.Size;
        {$IFNDEF LINUX}
        diritem.CreationDate := ConvertFileTimeToDateTime(rec.FindData.ftCreationTime,True);
        diritem.LastModified := ConvertFileTimeToDateTime(rec.FindData.ftLastWriteTime,True);
        {$ELSE}
        diritem.CreationDate := FileDateToDateTime(rec.Time);
        {$ENDIF}
        Result := Result + [diritem];
        if Recursive then Result := Result + GetFilesAndDirectories(dirpath + diritem.Name,Recursive);
      end;
    until FindNext(rec) <> 0;
  finally
    SysUtils.FindClose(rec);
  end;
end;

procedure GetFilesAndDirectories(const Path : string; aAddToList : TDirItemAddProc; Recursive : Boolean);
var
  rec : TSearchRec;
  diritem : TDirItem;
  dirpath : string;
  wildcard : string;
begin
  if not Assigned(aAddToList) then raise Exception.Create('GetFilesAndDirecties: AddToList cannot be nil!');

  if Path.Contains('*') then
  begin
    dirpath := ExtractFilePath(Path);
    wildcard := ExtractFileName(Path);
  end
  else
  begin
    dirpath := Path;
    wildcard := '*';
  end;
  dirpath := IncludeTrailingPathDelimiter(dirpath);


  if FindFirst(dirpath + wildcard, faAnyFile, rec) = 0 then
  try
    repeat
      if (rec.Attr and faDirectory) <> faDirectory then
      begin
        diritem.Name := rec.Name;
        diritem.IsDirectory := False;
        diritem.Size := rec.Size;
        {$IFNDEF LINUX}
        diritem.CreationDate := ConvertFileTimeToDateTime(rec.FindData.ftCreationTime,True);
        diritem.LastModified := ConvertFileTimeToDateTime(rec.FindData.ftLastWriteTime,True);
        {$ELSE}
        diritem.CreationDate := FileDateToDateTime(rec.Time);
        {$ENDIF}
        aAddToList(diritem);
      end
      else if (rec.Name <> '.') and (rec.Name <> '..') then
      begin
        diritem.Name := rec.Name;
        diritem.IsDirectory := True;
        diritem.Size := rec.Size;
        {$IFNDEF LINUX}
        diritem.CreationDate := ConvertFileTimeToDateTime(rec.FindData.ftCreationTime,True);
        diritem.LastModified := ConvertFileTimeToDateTime(rec.FindData.ftLastWriteTime,True);
        {$ELSE}
        diritem.CreationDate := FileDateToDateTime(rec.Time);
        {$ENDIF}
        aAddToList(diritem);
        if Recursive then GetFilesAndDirectories(dirpath + diritem.Name,aAddToList,Recursive);
      end;
    until FindNext(rec) <> 0;
  finally
    SysUtils.FindClose(rec);
  end;
end;

end.
