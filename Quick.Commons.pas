{ ***************************************************************************

  Copyright (c) 2016-2017 Kike Pérez

  Unit        : Quick.Commons
  Description : Common functions
  Author      : Kike Pérez
  Version     : 1.2
  Created     : 14/07/2017
  Modified    : 11/11/2017

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

unit Quick.Commons;

interface
  uses
    Classes,
    System.SysUtils,
    System.Types,
    {$IFDEF MSWINDOWS}
      Windows,
      Winapi.ShlObj,
      System.Win.Registry,
    {$ENDIF MSWINDOWS}
    System.IOUtils,
    System.DateUtils;

type

 TEventType = (etInfo, etSuccess, etWarning, etError, etDebug, etTrace);
 TLogVerbose = set of TEventType;

const
  LOG_ONLYERRORS = [etInfo,etError];
  LOG_ERRORSANDWARNINGS = [etInfo,etWarning,etError];
  LOG_TRACE = [etInfo,etError,etWarning,etTrace];
  LOG_ALL = [etInfo,etSuccess,etWarning,etError,etTrace];
  LOG_DEBUG = [etInfo,etSuccess,etWarning,etError,etDebug];
  EventStr : array of string = ['INFO','SUCC','WARN','ERROR','DEBUG','TRACE'];

type
  TPasswordComplexity = set of (pfIncludeNumbers,pfIncludeSigns);

  {$IFDEF MSWINDOWS}
  TEnvironmentPath = record
    EXEPATH : string;
    WINDOWS : string;
    SYSTEM : string;
    PROGRAMFILES : string;
    COMMONFILES : string;
    HOMEDRIVE : string;
    TEMP : string;
    USERPROFILE : string;
    INSTDRIVE : string;
    DESKTOP : string;
    STARTMENU : string;
    DESKTOP_ALLUSERS : string;
    STARTMENU_ALLUSERS : string;
    STARTUP : string;
    APPDATA : String;
    PROGRAMDATA : string;
    ALLUSERSPROFILE : string;
  end;
  {$ENDIF MSWINDOWS}

  TFileHelper = record helper for TFile
    class function IsInUse(const FileName : string) : Boolean; static;
    class function GetSize(const FileName: String): Int64; static;
  end;

  TDirectoryHelper = record helper for TDirectory
    class function GetSize(const Path: String): Int64; static;
  end;

  TTextFileOperation = (tfOpenRead,tfOpenOverwrite,tfOpenAppend);

	TTextStreamFile = class
    private
      fReadStream : TStreamReader;
      fWriteStream : TStreamWriter;
      function GetEOF : Boolean;
    public
      constructor Create(const FileName : string; OpenMode : TTextFileOperation);
      destructor Destroy; override;
      function ReadLn: string; overload;
      function ReadLn(out Data: string): Boolean; overload;
      procedure WriteLn (const Data : string);
      procedure Close;
      property EOF: Boolean read GetEOF;
  end;

  //generates a random password with complexity options
  function RandomPassword(const PasswordLength : Integer; Complexity : TPasswordComplexity = [pfIncludeNumbers,pfIncludeSigns]) : string;
  //extracts file extension from a filename
  function ExtractFileNameWithoutExt(const FileName: String): String;
  //converts a Unix path to Windows path
  function UnixToWindowsPath(const UnixPath: string): string;
  //converts a Windows path to Unix path
  function WindowsToUnixPath(const WindowsPath: string): string;
  {$IFDEF MSWINDOWS}
  //get typical environment paths as temp, desktop, etc
  procedure GetEnvironmentPaths;
  function GetSpecialFolderPath(folderID : Integer) : string;
  //checks if running on a 64bit OS
  function Is64bitOS : Boolean;
  //checks if is a console app
  function IsConsole : Boolean;
  //checks if compiled in debug mode
  function IsDebug : Boolean;
  //checks if running as a service
  function IsService : Boolean;
  //gets number of seconds without user interaction (mouse, keyboard)
  function SecondsIdle: DWord;
  //frees process memory not needed
  procedure FreeUnusedMem;
  //changes screen resolution
  function SetScreenResolution(Width, Height: integer): Longint;
  {$ENDIF MSWINDOWS}
  //returns last day of current month
  function LastDayCurrentMonth: TDateTime;
  //checks if two datetimes are in same day
  function IsSameDay(cBefore, cNow : TDateTime) : Boolean;
  //returns n times a char
  function FillStr(const C : Char; const Count : Byte) : string;
  //returns a number leading zero
  function Zeroes(const Number, Len : Int64) : string;
  //converts a number to thousand delimeter string
  function NumberToStr(const Number : Int64) : string;
  //returns n spaces
  function Spaces(const Count : Integer) : string;
  //returns current date as a string
  function NowStr : string;
  //returns a new GUID as string
  function NewGuidStr : string;
  //compare a string with a wildcard pattern (? or *)
  function IsLike(cText, Pattern: string) : Boolean;
  //Upper case for first letter
  function Capitalize(s: string): string;
  function CapitalizeWords(s: string): string;
  //returns current logged user
  function GetLoggedUserName : string;
  //returns computer name
  function GetComputerName : string;
  //Changes incorrect delims in path
  function NormalizePathDelim(const cPath : string; const Delim : Char) : string;
  //Removes last segment of a path
  function RemoveLastPathSegment(cDir : string) : string;
  //finds swith in commandline params
  function ParamFindSwitch(const Switch : string) : Boolean;
  //gets value for a switch if exists
  function ParamGetSwitch(const Switch : string; var cvalue : string) : Boolean;
  //returns app version (major & minor)
  function GetAppVersionStr: string;
  //returns app version full (major, minor, release & compiled)
  function GetAppVersionFullStr: string;
  //UTC DateTime to Local DateTime
  function UTCToLocalTime(GMTTime: TDateTime): TDateTime;
  //Local DateTime to UTC DateTime
  function LocalTimeToUTC(LocalTime : TDateTime): TDateTime;
  //count number of digits of a Integer
  function CountDigits(anInt: Cardinal): Cardinal; inline;

var
  {$IFDEF MSWINDOWS}
  path : TEnvironmentPath;
  {$ENDIF MSWINDOWS}

implementation

{TFileHelper}

class function TFileHelper.IsInUse(const FileName : string) : Boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  try
   HFileRes := CreateFile(PChar(FileName)
    ,GENERIC_READ or GENERIC_WRITE
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

class function TFileHelper.GetSize(const FileName: String): Int64;
  var
    info: TWin32FileAttributeData;
begin
  Result := -1;
  if not GetFileAttributesEx(PWideChar(FileName), GetFileExInfoStandard, @info) then Exit;
  Result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
end;

{TDirectoryHelper}

class function TDirectoryHelper.GetSize(const Path: String): Int64;
var
  filename : string;
begin
  Result := -1;
  for filename in TDirectory.GetFiles(Path) do
  begin
    Result := Result + TFile.GetSize(filename);
  end;
end;

{TTextStreamFile}

constructor TTextStreamFile.Create(const FileName : string; OpenMode : TTextFileOperation);
var
  Append : Boolean;
begin
  if OpenMode = tfOpenRead then fReadStream := TStreamReader.Create(FileName,True)
  else
  begin
    if OpenMode = tfOpenAppend then Append := True
      else Append := False;
    fWriteStream := TStreamWriter.Create(FileName,Append);
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

function RandomPassword(const PasswordLength : Integer; Complexity : TPasswordComplexity = [pfIncludeNumbers,pfIncludeSigns]) : string;
const
  PassAlpha  = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  PassSigns = '@!&$';
  PassNumbers = '1234567890';
var
  MinNumbers,
  MinSigns : Integer;
  NumNumbers,
  NumSigns : Integer;
begin
  Result := '';
  Randomize;
  //fill all alfa
  repeat
    Result := Result + PassAlpha[Random(Length(PassAlpha))+1];
  until (Length(Result) = PasswordLength);
  //checks if need include numbers
  if pfIncludeNumbers in Complexity then
  begin
    MinNumbers := Round(PasswordLength / 10 * 2);
    NumNumbers := 0;
    if MinNumbers = 0 then MinNumbers := 1;
    repeat
      Result[Random(PasswordLength)+1] := PassNumbers[Random(Length(PassNumbers))+1];
      Inc(NumNumbers);
    until NumNumbers = MinNumbers;
  end;
  //checks if need include signs
  if pfIncludeNumbers in Complexity then
  begin
    MinSigns := Round(PasswordLength / 10 * 1);
    NumSigns := 0;
    if MinSigns = 0 then MinSigns := 1;
    repeat
      Result[Random(PasswordLength)+1] := PassSigns[Random(Length(PassSigns))+1];
      Inc(NumSigns);
    until NumSigns = MinSigns;
  end;
end;

function ExtractFileNameWithoutExt(const FileName: String): String;
begin
  Result := TPath.GetFileNameWithoutExtension(FileName);
end;

function UnixToWindowsPath(const UnixPath: string): string;
begin
  Result:=StringReplace(UnixPath, '/', '\',[rfReplaceAll, rfIgnoreCase]);
end;

function WindowsToUnixPath(const WindowsPath: string): string;
begin
  Result:=StringReplace(WindowsPath, '\', '/',[rfReplaceAll, rfIgnoreCase]);
end;

{$IFDEF MSWINDOWS}
procedure GetEnvironmentPaths;
begin
  //gets path
  path.EXEPATH := TPath.GetDirectoryName(ParamStr(0));
  path.WINDOWS := GetEnvironmentVariable('windir');
  path.PROGRAMFILES := GetEnvironmentVariable('ProgramFiles');
  path.COMMONFILES := GetEnvironmentVariable('CommonProgramFiles(x86)');
  path.HOMEDRIVE := GetEnvironmentVariable('SystemDrive');
  path.USERPROFILE := GetEnvironmentVariable('USERPROFILE');
  path.PROGRAMDATA := GetEnvironmentVariable('ProgramData');
  path.ALLUSERSPROFILE := GetEnvironmentVariable('AllUsersProfile');
  path.INSTDRIVE := path.HOMEDRIVE;
  path.TEMP := GetEnvironmentVariable('TEMP');
  path.SYSTEM := GetSpecialFolderPath(CSIDL_SYSTEM);
  path.APPDATA:=GetSpecialFolderPath(CSIDL_APPDATA);
  //these paths fail if user is SYSTEM
  try
    path.DESKTOP := GetSpecialFolderPath(CSIDL_DESKTOP);
    path.DESKTOP_ALLUSERS := GetSpecialFolderPath(CSIDL_COMMON_DESKTOPDIRECTORY);
    path.STARTMENU:=GetSpecialFolderPath(CSIDL_PROGRAMS);
    path.STARTMENU_ALLUSERS:=GetSpecialFolderPath(CSIDL_COMMON_PROGRAMS);
    path.STARTMENU_ALLUSERS := path.STARTMENU;
    path.STARTUP:=GetSpecialFolderPath(CSIDL_STARTUP);
  except
    //
  end;
end;

function GetSpecialFolderPath(folderID : Integer) : string;
var
  ppidl: PItemIdList;
begin
  SHGetSpecialFolderLocation(0, folderID, ppidl);
  SetLength(Result, MAX_PATH);
  if not SHGetPathFromIDList(ppidl, PChar(Result)) then
  begin
    raise exception.create(Format('GetSpecialFolderPath: Invalid PIPL (%d)',[folderID]));
  end;
  SetLength(Result, lStrLen(PChar(Result)));
end;

function Is64bitOS : Boolean;
begin
  {$IFDEF WIN64}
    Result := True;
  {$ELSE}
    Result := False;
  {$ENDIF WIN64}
end;

function IsConsole: Boolean;
begin
  {$IFDEF CONSOLE}
    Result := True;
  {$ELSE}
    Result := False;
  {$ENDIF CONSOLE}
end;

function IsDebug: Boolean;
begin
  {$IFDEF DEBUG}
    Result := True;
  {$ELSE}
    Result := False;
  {$ENDIF DEBUG}
end;

function IsService : Boolean;
begin
  //only working with my Quick.AppService unit
  try
    Result := (IsConsole) and (GetStdHandle(STD_OUTPUT_HANDLE) = 0);
  except
    Result := False;
  end;
end;

function SecondsIdle: DWord;
var
   liInfo: TLastInputInfo;
begin
   liInfo.cbSize := SizeOf(TLastInputInfo) ;
   GetLastInputInfo(liInfo) ;
   Result := (GetTickCount - liInfo.dwTime) DIV 1000;
end;

procedure FreeUnusedMem;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF);
end;

function SetScreenResolution(Width, Height: integer): Longint;
var
  DeviceMode: TDeviceMode;
begin
  with DeviceMode do
  begin
    dmSize := SizeOf(TDeviceMode);
    dmPelsWidth := Width;
    dmPelsHeight := Height;
    dmFields := DM_PELSWIDTH or DM_PELSHEIGHT;
  end;
  Result := ChangeDisplaySettings(DeviceMode, CDS_UPDATEREGISTRY);
end;
{$ENDIF MSWINDOWS}

function LastDayCurrentMonth: TDateTime;
begin
  Result := EncodeDate(YearOf(Now),MonthOf(Now), DaysInMonth(Now));
end;

function IsSameDay(cBefore, cNow : TDateTime) : Boolean;
begin
  Result := DateTimeInRange(cNow,StartOfTheDay(cBefore),EndOfTheDay(cNow),True);
end;

function FillStr(const C : Char; const Count : Byte) : string;
var
    i   : Byte;
begin
  Result := '';
  for i := 1 to Count do Result := Result + C;
end;

function Zeroes(const Number, Len : Int64) : string;
begin
  if Len > Length(IntToStr(Number)) then Result := FillStr('0',Len - Length(IntToStr(Number))) + IntToStr(Number)
    else Result := IntToStr(Number);
end;

function NumberToStr(const Number : Int64) : string;
begin
  try
    Result := FormatFloat('0,',Number);
  except
    Result := '#Error';
  end;
end;

function Spaces(const Count : Integer) : string;
begin
  Result := FillStr(' ',Count);
end;

function NowStr : string;
begin
  Result := DateTimeToStr(Now());
end;

function NewGuidStr : string;
var
  guid : TGUID;
begin
  guid.NewGuid;
  Result := guid.ToString
  //GUIDToString(guid);
end;

function IsLike(cText, Pattern: string) : Boolean;
var
  i, n : Integer;
  match : Boolean;
  wildcard : Boolean;
  CurrentPattern : Char;
  aux : string;
begin
  Result := False;
  wildcard := False;
  cText := LowerCase(cText);
  Pattern := LowerCase(Pattern);
  match := False;

  if (Pattern.Length > cText.Length) or (Pattern = '') then Exit;
  if Pattern = '*' then
  begin
    Result := True;
    Exit;
  end;

  for i := 1 to cText.Length do
  begin
    CurrentPattern := Pattern[i];
    if CurrentPattern = '*' then wildcard := True;

    if wildcard then
    begin
      aux := Copy(Pattern,i+1,Pattern.Length);
      n := Pos(Copy(Pattern,i+1,Pattern.Length),cText);
      if (n > i) or (Pattern.Length = i) then
      begin
        Result := True;
        Exit;
      end;
    end
    else
    begin
      if (cText[i] = CurrentPattern) or (CurrentPattern = '?') then match := True
        else match := False;
    end;
  end;
  Result := match;
end;

function Capitalize(s: string): string;
begin
  Result := '';
  if s.Length = 0 then Exit;
  s := LowerCase(s,loUserLocale);
  Result := UpperCase(s[1],loUserLocale) + Trim(Copy(s, 2, s.Length));
end;

function CapitalizeWords(s: string): string;
var
  cword : string;
begin
  Result := '';
  if s.Length = 0 then Exit;
  s := LowerCase(s,loUserLocale);
  for cword in s.Split([' ']) do
  begin
    if Result = '' then Result := Capitalize(cword)
      else Result := Result + ' ' + Capitalize(cword);
  end;
end;

function GetLoggedUserName : string;
const
  cnMaxUserNameLen = 254;
var
  sUserName     : string;
  dwUserNameLen : DWord;
begin
  dwUserNameLen := cnMaxUserNameLen-1;
  SetLength( sUserName, cnMaxUserNameLen );
  GetUserName(PChar( sUserName ),dwUserNameLen );
  SetLength( sUserName, dwUserNameLen );
  Result := sUserName;
end;

function GetComputerName : string;
var
  dwLength: dword;
begin
  dwLength := 253;
  SetLength(Result, dwLength+1);
  if not Windows.GetComputerName(pchar(result), dwLength) then Result := 'Not detected!';
  Result := pchar(result);
end;

function NormalizePathDelim(const cPath : string; const Delim : Char) : string;
begin
  if Delim = '\' then Result := StringReplace(cPath,'/',Delim,[rfReplaceAll])
    else Result := StringReplace(cPath,'\',Delim,[rfReplaceAll]);
end;

function RemoveLastPathSegment(cDir : string) : string;
var
  posi : Integer;
  delim : Char;
  EndsWithDelim : Boolean;
begin
  if cDir.Contains('\') then delim := '\'
    else if cDir.Contains('/') then delim := '/'
      else
      begin
        Result := '';
        Exit;
      end;
  NormalizePathDelim(cDir,delim);

  if cDir.EndsWith(delim) then
  begin
    cDir := Copy(cDir,1,cDir.Length-1);
    EndsWithDelim := True;
  end
  else EndsWithDelim := False;

  if cDir.CountChar(delim) > 1 then posi := cDir.LastDelimiter(delim)
    else posi := Pos(delim,cDir)-1;
  if posi = cDir.Length then posi := 0;
  Result := Copy(cDir,1,posi);
  if (Result <> '') and (EndsWithDelim) then Result := Result + delim;
end;

function ParamFindSwitch(const Switch : string) : Boolean;
begin
  Result := FindCmdLineSwitch(Switch,['-', '/'],True);
end;

function ParamGetSwitch(const Switch : string; var cvalue : string) : Boolean;
begin
  Result := FindCmdLineSwitch(Switch,cvalue,True,[clstValueAppended]);
end;

function GetAppVersionStr: string;
var
  Rec: LongRec;
  ver : Cardinal;
begin
  ver := GetFileVersion(ParamStr(0));
  if ver <> Cardinal(-1) then
  begin
    Rec := LongRec(ver);
    Result := Format('%d.%d', [Rec.Hi, Rec.Lo]);
  end
  else Result := '';
end;

function GetAppVersionFullStr: string;
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  Result := '';
  Exe := ParamStr(0);
  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  if Size = 0 then
  begin
    //RaiseLastOSError;
    //no version info in file
    Exit;
  end;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  if (LongRec(FixedPtr.dwFileVersionLS).Hi = 0) and (LongRec(FixedPtr.dwFileVersionLS).Lo = 0) then
  begin
    Result := Format('%d.%d',
    [LongRec(FixedPtr.dwFileVersionMS).Hi,   //major
     LongRec(FixedPtr.dwFileVersionMS).Lo]); //minor
  end
  else if (LongRec(FixedPtr.dwFileVersionLS).Lo = 0) then
  begin
    Result := Format('%d.%d.%d',
    [LongRec(FixedPtr.dwFileVersionMS).Hi,   //major
     LongRec(FixedPtr.dwFileVersionMS).Lo,   //minor
     LongRec(FixedPtr.dwFileVersionLS).Hi]); //release
  end
  else
  begin
    Result := Format('%d.%d.%d.%d',
    [LongRec(FixedPtr.dwFileVersionMS).Hi,   //major
     LongRec(FixedPtr.dwFileVersionMS).Lo,   //minor
     LongRec(FixedPtr.dwFileVersionLS).Hi,   //release
     LongRec(FixedPtr.dwFileVersionLS).Lo]); //build
  end;
end;

function UTCToLocalTime(GMTTime: TDateTime): TDateTime;
begin
  Result :=  TTimeZone.Local.ToLocalTime(GMTTime);
end;

function LocalTimeToUTC(LocalTime : TDateTime): TDateTime;
begin
  Result := TTimeZone.Local.ToUniversalTime(LocalTime);
end;

function CountDigits(anInt: Cardinal): Cardinal; inline;
var
  cmp: Cardinal;
begin
  cmp := 10;
  Result := 1;
  while (Result < 10) and (cmp <= anInt) do
  begin
    cmp := cmp*10;
    Inc(Result);
  end;
end;

initialization
  try
    GetEnvironmentPaths;
  except
    on E : Exception do if not IsService then raise Exception.Create(E.Message);
  end;

end.
