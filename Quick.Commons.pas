{ ***************************************************************************

  Copyright (c) 2016-2018 Kike Pérez

  Unit        : Quick.Commons
  Description : Common functions
  Author      : Kike Pérez
  Version     : 1.4
  Created     : 14/07/2017
  Modified    : 29/03/2018

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

{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

unit Quick.Commons;

interface
  uses
    Classes,
    {$IFnDEF FPC}
    System.SysUtils,
    System.Types,
    {$ELSE}
    SysUtils,
    Types,
    {$ENDIF}
    {$IFDEF MSWINDOWS}
      Windows,
      {$IFnDEF FPC}
        Winapi.ShlObj,
        System.Win.Registry,
      {$ELSE}
      jwawinuser,
      ShlObj,
      Registry,
      {$ENDIF}
    {$ENDIF MSWINDOWS}
    {$IFnDEF FPC}
    System.IOUtils,
    System.DateUtils;
    {$ELSE}
      FileUtil,
      dateutils;
    {$ENDIF}

type

 TLogEventType = (etInfo, etSuccess, etWarning, etError, etDebug, etTrace);
 TLogVerbose = set of TLogEventType;

const
  LOG_ONLYERRORS = [etInfo,etError];
  LOG_ERRORSANDWARNINGS = [etInfo,etWarning,etError];
  LOG_TRACE = [etInfo,etError,etWarning,etTrace];
  LOG_ALL = [etInfo,etSuccess,etWarning,etError,etTrace];
  LOG_DEBUG = [etInfo,etSuccess,etWarning,etError,etDebug];
  {$IFDEF CompilerVersion}
  {$IF CompilerVersion > 27}
  EventStr : array of string = ['INFO','SUCC','WARN','ERROR','DEBUG','TRACE'];
  {$ENDIF}
  {$ELSE}
  EventStr : array[0..5] of string = ('INFO','SUCC','WARN','ERROR','DEBUG','TRACE');
  {$ENDIF}
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
  {$IFNDEF FPC}
  TFileHelper = record helper for TFile
    {$IFDEF MSWINDOWS}
    class function IsInUse(const FileName : string) : Boolean; static;
    {$ENDIF}
    class function GetSize(const FileName: String): Int64; static;
  end;

  TDirectoryHelper = record helper for TDirectory
    class function GetSize(const Path: String): Int64; static;
  end;
  {$ELSE}
    TFileHelper = class
      {$IFDEF MSWINDOWS}
      class function IsInUse(const FileName : string) : Boolean; static;
      {$ENDIF}
      class function GetSize(const FileName: String): Int64; static;
    end;

    TDirectoryHelper = class
      class function GetSize(const Path: String): Int64; static;
    end;
  {$ENDIF}

  TCounter = record
  private
    fMaxValue : Integer;
    fCurrentValue : Integer;
  public
    property MaxValue : Integer read fMaxValue;
    procedure Init(aMaxValue : Integer);
    function Count : Integer;
    function CountIs(aValue : Integer) : Boolean;
    function Check : Boolean;
    procedure Reset;
  end;

  TTimeCounter = record
  private
    fCurrentTime : TDateTime;
    fDoneEvery : Integer;
  public
    property DoneEvery : Integer read fDoneEvery;
    procedure Init(MillisecondsToReach : Integer);
    function Check : Boolean;
    procedure Reset;
  end;

  EEnvironmentPath = class(Exception);
  EShellError = class(Exception);

  //generates a random password with complexity options
  function RandomPassword(const PasswordLength : Integer; Complexity : TPasswordComplexity = [pfIncludeNumbers,pfIncludeSigns]) : string;
  //extracts file extension from a filename
  function ExtractFileNameWithoutExt(const FileName: string): string;
  //converts a Unix path to Windows path
  function UnixToWindowsPath(const UnixPath: string): string;
  //converts a Windows path to Unix path
  function WindowsToUnixPath(const WindowsPath: string): string;
  //corrects malformed urls
  function CorrectURLPath(cUrl : string) : string;
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
  {$IFDEF MSWINDOWS}
  //returns current logged user
  function GetLoggedUserName : string;
  //returns computer name
  function GetComputerName : string;
  {$ENDIF}
  //Changes incorrect delims in path
  function NormalizePathDelim(const cPath : string; const Delim : Char) : string;
  //Removes last segment of a path
  function RemoveLastPathSegment(cDir : string) : string;
  //finds swith in commandline params
  function ParamFindSwitch(const Switch : string) : Boolean;
  //gets value for a switch if exists
  function ParamGetSwitch(const Switch : string; var cvalue : string) : Boolean;
  {$IFDEF MSWINDOWS}
  //returns app version (major & minor)
  function GetAppVersionStr: string;
  //returns app version full (major, minor, release & compiled)
  function GetAppVersionFullStr: string;
  {$ENDIF}
  //UTC DateTime to Local DateTime
  function UTCToLocalTime(GMTTime: TDateTime): TDateTime;
  //Local DateTime to UTC DateTime
  function LocalTimeToUTC(LocalTime : TDateTime): TDateTime;
  //count number of digits of a Integer
  function CountDigits(anInt: Cardinal): Cardinal; inline;
  //save stream to file
  procedure SaveStreamToFile(stream : TStream; const filename : string);
  {$IFDEF MSWINDOWS}
  //process messages on console applications
  procedure ProcessMessages;
  //get last error message
  function GetLastOSError : String;
  {$ENDIF}
  {$IFDEF FPC}
  //implement our delphi time range functions becasue FPC hasn't yet
  function DateTimeInRange(ADateTime: TDateTime; AStartDateTime, AEndDateTime: TDateTime; aInclusive: Boolean = True): Boolean;
  function DateInRange(ADate: TDate; AStartDate, AEndDate: TDate; AInclusive: Boolean = True): Boolean;
  function TimeInRange(ATime: TTime; AStartTime, AEndTime: TTime; AInclusive: Boolean = True): Boolean;
  {$ENDIF}

{$IFDEF MSWINDOWS}
var
  path : TEnvironmentPath;
{$ENDIF}

implementation

{TFileHelper}

{$IFDEF MSWINDOWS}
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
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TFileHelper.GetSize(const FileName: String): Int64;
var
  info: TWin32FileAttributeData;
begin
  Result := -1;
  {$IFNDEF FPC}
  if not GetFileAttributesEx(PWideChar(FileName), GetFileExInfoStandard, @info) then Exit;
  {$ELSE}
  if not GetFileAttributesEx(PAnsiChar(FileName), GetFileExInfoStandard, @info) then Exit;
  {$ENDIF}
  Result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
end;
{$ELSE}
class function TFileHelper.GetSize(const FileName: String): Int64;
var
  sr : TSearchRec;
begin
  if FindFirst(fileName, faAnyFile, sr ) = 0 then Result := sr.Size
    else Result := -1;
end;
{$ENDIF}

{TDirectoryHelper}


{$IFNDEF FPC}
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
{$ELSE}
class function TDirectoryHelper.GetSize(const Path: String): Int64;
var
  filename : string;
begin
  Result := -1;
  for filename in FindAllFiles(Path) do
  begin
    Result := Result + TFileHelper.GetSize(filename);
  end;
end;
{$ENDIF}

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

function ExtractFileNameWithoutExt(const FileName: string): string;
begin
  {$IFNDEF FPC}
  Result := TPath.GetFileNameWithoutExtension(FileName);
  {$ELSE}
  Result := ExtractFilePath(Filename);
  {$ENDIF}
end;

function UnixToWindowsPath(const UnixPath: string): string;
begin
  Result := StringReplace(UnixPath, '/', '\',[rfReplaceAll, rfIgnoreCase]);
end;

function WindowsToUnixPath(const WindowsPath: string): string;
begin
  Result := StringReplace(WindowsPath, '\', '/',[rfReplaceAll, rfIgnoreCase]);
end;

function CorrectURLPath(cUrl : string) : string;
var
  nurl : string;
begin
  nurl := WindowsToUnixPath(cUrl);
  nurl := StringReplace(nurl,'//','/',[rfReplaceAll]);
  Result := StringReplace(nurl,' ','%20',[rfReplaceAll]);
  //TNetEncoding.Url.Encode()
end;

{$IFDEF MSWINDOWS}
procedure GetEnvironmentPaths;
begin
  //gets path
  {$IFNDEF FPC}
  path.EXEPATH := TPath.GetDirectoryName(ParamStr(0));
  path.WINDOWS := GetEnvironmentVariable('windir');
  path.PROGRAMFILES := GetEnvironmentVariable('ProgramFiles');
  path.COMMONFILES := GetEnvironmentVariable('CommonProgramFiles(x86)');
  path.HOMEDRIVE := GetEnvironmentVariable('SystemDrive');
  path.USERPROFILE := GetEnvironmentVariable('USERPROFILE');
  path.PROGRAMDATA := GetEnvironmentVariable('ProgramData');
  path.ALLUSERSPROFILE := GetEnvironmentVariable('AllUsersProfile');
  path.TEMP := GetEnvironmentVariable('TEMP');
  {$ELSE}
  path.EXEPATH:=ExtractFileDir(ParamStr(0));
  path.WINDOWS := Sysutils.GetEnvironmentVariable('windir');
  path.PROGRAMFILES := Sysutils.GetEnvironmentVariable('ProgramFiles');
  path.COMMONFILES := Sysutils.GetEnvironmentVariable('CommonProgramFiles(x86)');
  path.HOMEDRIVE := Sysutils.GetEnvironmentVariable('SystemDrive');
  path.USERPROFILE := Sysutils.GetEnvironmentVariable('USERPROFILE');
  path.PROGRAMDATA := Sysutils.GetEnvironmentVariable('ProgramData');
  path.ALLUSERSPROFILE := Sysutils.GetEnvironmentVariable('AllUsersProfile');
  path.TEMP := Sysutils.GetEnvironmentVariable('TEMP');
  {$ENDIF}
  path.INSTDRIVE := path.HOMEDRIVE;
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
    raise EShellError.create(Format('GetSpecialFolderPath: Invalid PIPL (%d)',[folderID]));
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
  {$IFNDEF FPC}
  Result := ChangeDisplaySettings(DeviceMode, CDS_UPDATEREGISTRY);
  {$ELSE}
  Result := Windows.ChangeDisplaySettings(DeviceMode, CDS_UPDATEREGISTRY);
  {$ENDIF}
end;
{$ENDIF MSWINDOWS}

function LastDayCurrentMonth: TDateTime;
begin
  Result := EncodeDate(YearOf(Now),MonthOf(Now), DaysInMonth(Now));
end;

function IsSameDay(cBefore, cNow : TDateTime) : Boolean;
begin
  //Test: Result := MinutesBetween(cBefore,cNow) < 1;
  Result := DateTimeInRange(cNow,StartOfTheDay(cBefore),EndOfTheDay(cBefore),True);
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
  {$IFNDEF FPC}
  s := LowerCase(s,loUserLocale);
  Result := UpperCase(s[1],loUserLocale) + Trim(Copy(s, 2, s.Length));
  {$ELSE}
  s := LowerCase(s);
  Result := UpperCase(s[1]) + Trim(Copy(s, 2, s.Length));
  {$ENDIF}
end;

function CapitalizeWords(s: string): string;
var
  cword : string;
begin
  Result := '';
  if s.Length = 0 then Exit;
  {$IFNDEF FPC}
  s := LowerCase(s,loUserLocale);
  {$ELSE}
  s := LowerCase(s);
  {$ENDIF}
  for cword in s.Split([' ']) do
  begin
    if Result = '' then Result := Capitalize(cword)
      else Result := Result + ' ' + Capitalize(cword);
  end;
end;

{$IFDEF MSWINDOWS}
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
{$ENDIF}

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
  {$IFNDEF FPC}
  Result := FindCmdLineSwitch(Switch,cvalue,True,[clstValueAppended]);
  {$ELSE}
  //Not result value in FPC :(
  Result := FindCmdLineSwitch(Switch);
  {$ENDIF}
end;


{$IFDEF MSWINDOWS}
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
{$ENDIF}

function UTCToLocalTime(GMTTime: TDateTime): TDateTime;
begin
  {$IFNDEF FPC}
  Result :=  TTimeZone.Local.ToLocalTime(GMTTime);
  {$ELSE}
  //TODO
  raise ENotImplemented.Create('Not implemented yet');
  {$ENDIF}
end;

function LocalTimeToUTC(LocalTime : TDateTime): TDateTime;
begin
  {$IFNDEF FPC}
  Result := TTimeZone.Local.ToUniversalTime(LocalTime);
  {$ELSE}
  //TODO
  raise ENotImplemented.Create('Not implemented yet');  ;
  {$ENDIF}
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

procedure SaveStreamToFile(stream : TStream; const filename : string);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(filename,fmCreate);
  try
    stream.Seek(0,soBeginning);
    fs.CopyFrom(stream,stream.Size);
  finally
    fs.Free;
  end;
end;

{ TCounter }

procedure TCounter.Init(aMaxValue : Integer);
begin
  fMaxValue := aMaxValue;
  fCurrentValue := 0;
end;

function TCounter.Count : Integer;
begin
  Result := fCurrentValue;
end;

function TCounter.CountIs(aValue : Integer) : Boolean;
begin
  Result := fCurrentValue = aValue;
end;

function TCounter.Check : Boolean;
begin
  if fCurrentValue = fMaxValue then
  begin
    Result := True;
    Reset;
  end
  else
  begin
    Result := False;
    Inc(fCurrentValue);
  end;
end;

procedure TCounter.Reset;
begin
  fCurrentValue := fMaxValue;
end;

{ TimeCounter }

procedure TTimeCounter.Init(MillisecondsToReach : Integer);
begin
  fDoneEvery := MillisecondsToReach;
end;

function TTimeCounter.Check : Boolean;
begin
  if MilliSecondsBetween(fCurrentTime,Now) > fDoneEvery then
  begin
    fCurrentTime := Now();
    Result := True;
  end
  else Result := False;
end;

procedure TTimeCounter.Reset;
begin
  fCurrentTime := Now();
end;

{$IFDEF MSWINDOWS}
procedure ProcessMessages;
var
  {$IFNDEF FPC}
  Msg: TMsg;
begin
  while integer(PeekMessage(Msg, 0, 0, 0, PM_REMOVE)) <> 0 do
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
  {$ELSE}
  Msg: Windows.TMsg;
begin
  while integer(Windows.PeekMessage(Msg, 0, 0, 0, PM_REMOVE)) <> 0 do
  begin
    Windows.TranslateMessage(Msg);
    Windows.DispatchMessage(Msg);
  end;
  {$ENDIF}
end;

function GetLastOSError: String;
begin
  Result := SysErrorMessage(Windows.GetLastError);
end;

{$ENDIF}

{$IFDEF FPC}
function DateTimeInRange(ADateTime: TDateTime; AStartDateTime, AEndDateTime: TDateTime; aInclusive: Boolean = True): Boolean;
begin
  if aInclusive then
    Result := (AStartDateTime <= ADateTime) and (ADateTime <= AEndDateTime)
  else
    Result := (AStartDateTime < ADateTime) and (ADateTime < AEndDateTime);
end;

function TimeInRange(ATime: TTime; AStartTime, AEndTime: TTime; AInclusive: Boolean = True): Boolean;
var
  LTime, LStartTime, LEndTime: TTime;
begin
  LTime := TimeOf(ATime);
  LStartTime := TimeOf(AStartTime);
  LEndTime := TimeOF(AEndTime);

  if LEndTime < LStartTime then
    if AInclusive then
      Result := (LStartTime <= LTime) or (LTime <= LEndTime)
    else
      Result := (LStartTime < LTime) or (LTime < LEndTime)
  else
    if AInclusive then
      Result := (LStartTime <= LTime) and (LTime <= LEndTime)
    else
      Result := (LStartTime < LTime) and (LTime < LEndTime);
end;

function DateInRange(ADate: TDate; AStartDate, AEndDate: TDate; AInclusive: Boolean = True): Boolean;
begin
  if AInclusive then
    Result := (DateOf(AStartDate) <= DateOf(ADate)) and (DateOf(ADate) <= DateOf(AEndDate))
 else
    Result := (DateOf(AStartDate) < DateOf(ADate)) and (DateOf(ADate) < DateOf(AEndDate));
end;

{$ENDIF}

initialization
{$IFDEF MSWINDOWS}
  try
    GetEnvironmentPaths;
  except
    on E : Exception do
    begin
      if not IsService then
      begin
        if IsConsole then Writeln(Format('[WARN] GetEnvironmentPaths: %s',[E.Message]))
          else raise EEnvironmentPath.Create(Format('Get environment path error: %s',[E.Message]));
      end;
    end;
  end;
{$ENDIF}

end.
