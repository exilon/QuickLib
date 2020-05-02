{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.Commons
  Description : Common functions
  Author      : Kike Pérez
  Version     : 1.9
  Created     : 14/07/2017
  Modified    : 02/05/2020

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

{$i QuickLib.inc}

interface

  uses
    Classes,
    SysUtils,
    Types,
    {$IFDEF MSWINDOWS}
      Windows,
      ShlObj,
    {$ENDIF MSWINDOWS}
    {$IFDEF FPC}
    Quick.Files,
      {$IFDEF LINUX}
      FileInfo,
      {$ENDIF}
    {$ELSE}
    IOUtils,
    {$ENDIF}
    {$IFDEF ANDROID}
    Androidapi.JNI.Os,
    Androidapi.Helpers,
    Androidapi.JNI.JavaTypes,
    Androidapi.JNI.GraphicsContentViewText,
    {$ENDIF}
    {$IFDEF IOS}
    iOSapi.UIKit,
    Posix.SysSysctl,
    Posix.StdDef,
    iOSapi.Foundation,
    Macapi.ObjectiveC,
    Macapi.Helpers,
    {$ENDIF}
    {$IFDEF OSX}
    Macapi.Foundation,
    Macapi.Helpers,
    FMX.Helpers.Mac,
    Macapi.ObjectiveC,
    {$ENDIF}
    {$IFDEF POSIX}
    Posix.Unistd,
    {$ENDIF}
    DateUtils;

type

 TLogEventType = (etInfo, etSuccess, etWarning, etError, etDebug, etTrace);
 TLogVerbose = set of TLogEventType;

const
  LOG_ONLYERRORS = [etInfo,etError];
  LOG_ERRORSANDWARNINGS = [etInfo,etWarning,etError];
  LOG_TRACE = [etInfo,etError,etWarning,etTrace];
  LOG_ALL = [etInfo,etSuccess,etWarning,etError,etTrace];
  LOG_DEBUG = [etInfo,etSuccess,etWarning,etError,etDebug];
  {$IFDEF DELPHIXE7_UP}
  EventStr : array of string = ['INFO','SUCC','WARN','ERROR','DEBUG','TRACE'];
  {$ELSE}
  EventStr : array[0..5] of string = ('INFO','SUCC','WARN','ERROR','DEBUG','TRACE');
  {$ENDIF}
type
  TPasswordComplexity = set of (pfIncludeNumbers,pfIncludeSigns);

  TEnvironmentPath = record
    EXEPATH : string;
    {$IFDEF MSWINDOWS}
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
    {$ENDIF MSWINDOWS}
  end;

  {$IFNDEF FPC}
  TFileHelper = record helper for TFile
    {$IF DEFINED(MSWINDOWS) OR DEFINED(DELPHILINUX)}
    class function IsInUse(const FileName : string) : Boolean; static;
    {$ENDIF}
    class function GetSize(const FileName: String): Int64; static;
  end;

  TDirectoryHelper = record helper for TDirectory
    class function GetSize(const Path: String): Int64; static;
  end;
  {$ENDIF}

  {$IFDEF FPC}
    {$IFDEF LINUX}
    UINT = cardinal;
    {$ENDIF}
  PLASTINPUTINFO = ^LASTINPUTINFO;
  tagLASTINPUTINFO = record
    cbSize: UINT;
    dwTime: DWORD;
  end;
  LASTINPUTINFO = tagLASTINPUTINFO;
  TLastInputInfo = LASTINPUTINFO;

  type
  TCmdLineSwitchType = (clstValueNextParam, clstValueAppended);
  TCmdLineSwitchTypes = set of TCmdLineSwitchType;
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

  {$IFNDEF FPC}

  {$IFNDEF DELPHIXE7_UP}
  TArrayUtil<T> = class
    class procedure Delete(var aArray : TArray<T>; aIndex : Integer);
  end;
  {$ENDIF}

  TArrayOfStringHelper = record helper for TArray<string>
  public
    function Any : Boolean; overload;
    function Any(const aValue : string) : Boolean; overload;
    function Add(const aValue : string) : Integer;
    function AddIfNotExists(const aValue : string; aCaseSense : Boolean = False) : Integer;
    function Remove(const aValue : string) : Boolean;
    function Exists(const aValue : string) : Boolean;
    function Count : Integer;
  end;
  TDelegate<T> = reference to procedure(Value : T);
  {$ENDIF}

  TPairItem = record
    Name : string;
    Value : string;
    constructor Create(const aName, aValue : string);
  end;

  TPairList = class
  type
    TPairEnumerator = class
      private
        fArray : ^TArray<TPairItem>;
        fIndex : Integer;
        function GetCurrent: TPairItem;
      public
        constructor Create(var aArray: TArray<TPairItem>);
        property Current : TPairItem read GetCurrent;
        function MoveNext: Boolean;
      end;
  private
    fItems : TArray<TPairItem>;
  public
    function GetEnumerator : TPairEnumerator;
    function GetValue(const aName : string) : string;
    function GetPair(const aName : string) : TPairItem;
    function Add(aPair : TPairItem) : Integer; overload;
    function Add(const aName, aValue : string) : Integer; overload;
    procedure AddOrUpdate(const aName, aValue : string);
    function Exists(const aName : string) : Boolean;
    function Remove(const aName : string) : Boolean;
    function Count : Integer;
    property Items[const aName : string] : string read GetValue write AddOrUpdate;
    function ToArray : TArray<TPairItem>;
    procedure FromArray(aValue : TArray<TPairItem>);
    procedure Clear;
  end;

  EEnvironmentPath = class(Exception);
  EShellError = class(Exception);

  //generates a random password with complexity options
  function RandomPassword(const PasswordLength : Integer; Complexity : TPasswordComplexity = [pfIncludeNumbers,pfIncludeSigns]) : string;
  //generates a random string
  function RandomString(const aLength: Integer) : string;
  //extracts file extension from a filename
  function ExtractFileNameWithoutExt(const FileName: string): string;
  //converts a Unix path to Windows path
  function UnixToWindowsPath(const UnixPath: string): string;
  //converts a Windows path to Unix path
  function WindowsToUnixPath(const WindowsPath: string): string;
  //corrects malformed urls
  function CorrectURLPath(cUrl : string) : string;
  //get typical environment paths as temp, desktop, etc
  procedure GetEnvironmentPaths;
  {$IFDEF MSWINDOWS}
  function GetSpecialFolderPath(folderID : Integer) : string;
  //checks if running on a 64bit OS
  function Is64bitOS : Boolean;
  //checks if is a console app
  function IsConsole : Boolean;
  function HasConsoleOutput : Boolean;
  //checks if compiled in debug mode
  {$ENDIF}
  function IsDebug : Boolean;
  {$IFDEF MSWINDOWS}
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
  {$IFDEF FPC}
  function DateTimeInRange(ADateTime: TDateTime; AStartDateTime, AEndDateTime: TDateTime; aInclusive: Boolean = True): Boolean;
  {$ENDIF}
  //checks if two datetimes are in same day
  function IsSameDay(cBefore, cNow : TDateTime) : Boolean;
  //change Time of a DateTime
  function ChangeTimeOfADay(aDate : TDateTime; aHour, aMinute, aSecond : Word; aMilliSecond : Word = 0) : TDateTime;
  //change Date of a DateTime
  function ChangeDateOfADay(aDate : TDateTime; aYear, aMonth, aDay : Word) : TDateTime;
  //returns n times a char
  function FillStr(const C : Char; const Count : Integer) : string;
  //checks if string exists in array of string
  function StrInArray(const aValue : string; const aInArray : array of string) : Boolean;
  //checks if integer exists in array of integer
  function IntInArray(const aValue : Integer; const aInArray : array of Integer) : Boolean;
  //check if array is empty
  function IsEmptyArray(aArray : TArray<string>) : Boolean; overload;
  function IsEmptyArray(aArray : TArray<Integer>) : Boolean; overload;
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
  //returns path delimiter if found
  function GetPathDelimiter(const aPath : string) : string;
  //returns first segment of a path
  function GetFirstPathSegment(const aPath : string) : string;
  //returns last segment of a path
  function GetLastPathSegment(const aPath : string) : string;
  //finds swith in commandline params
  function ParamFindSwitch(const Switch : string) : Boolean;
  //gets value for a switch if exists
  function ParamGetSwitch(const Switch : string; var cvalue : string) : Boolean;
  //returns app name (filename based)
  function GetAppName : string;
  //returns app version (major & minor)
  function GetAppVersionStr: string;
  //returns app version full (major, minor, release & compiled)
  function GetAppVersionFullStr: string;
  //convert UTC DateTime to Local DateTime
  function UTCToLocalTime(GMTTime: TDateTime): TDateTime;
  //convert Local DateTime to UTC DateTime
  function LocalTimeToUTC(LocalTime : TDateTime): TDateTime;
  //convert DateTime to GTM Time string
  function DateTimeToGMT(aDate : TDateTime) : string;
  //convert GMT Time string to DateTime
  function GMTToDateTime(aDate : string) : TDateTime;
  //convert DateTime to Json Date format
  function DateTimeToJsonDate(aDateTime : TDateTime) : string;
  //convert Json Date format to DateTime
  function JsonDateToDateTime(const aJsonDate : string) : TDateTime;
  //count number of digits of a Integer
  function CountDigits(anInt: Cardinal): Cardinal; inline;
  //count times a string is present in other string
  function CountStr(const aFindStr, aSourceStr : string) : Integer;
  //save stream to file
  procedure SaveStreamToFile(aStream : TStream; const aFilename : string);
  //save stream to string
  function StreamToString(aStream : TStream) : string;
  function StreamToString2(const aStream: TStream; const aEncoding: TEncoding): string;
  //save string to stream
  procedure StringToStream(const aStr : string; aStream : TStream);
  procedure StringToStream2(const aStr : string; aStream : TStream);
  //returns a real comma separated text from stringlist
  function CommaText(aList : TStringList) : string; overload;
  //returns a real comma separated text from array of string
  function CommaText(aArray : TArray<string>) : string; overload;
  //returns a string CRLF from array of string
  function ArrayToString(aArray : TArray<string>) : string;
  //converts TStrings to array
  function StringsToArray(aStrings : TStrings) : TArray<string>;
  {$IFDEF MSWINDOWS}
  //process messages on console applications
  procedure ProcessMessages;
  //get last error message
  function GetLastOSError : String;
  {$ENDIF}
  {$IF DEFINED(FPC) AND DEFINED(MSWINDOWS)}
  function GetLastInputInfo(var plii: TLastInputInfo): BOOL;stdcall; external 'user32' name 'GetLastInputInfo';
  {$ENDIF}
  function RemoveLastChar(const aText : string) : string;
  function DateTimeToSQL(aDateTime : TDateTime) : string;
  function IsInteger(const aValue : string) : Boolean;
  //extract a substring and deletes from source string
  function ExtractStr(var vSource : string; aIndex : Integer; aCount : Integer) : string;
  //get first string between string delimiters
  function GetSubString(const aSource, aFirstDelimiter, aLastDelimiter : string) : string;
  //get double quoted or dequoted string
  function DbQuotedStr(const str : string): string;
  function UnDbQuotedStr(const str: string) : string;
  //get simple quoted or dequoted string
  function SpQuotedStr(const str : string): string;
  function UnSpQuotedStr(const str : string): string;
  //ternary operator
  function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : string) : string; overload;
  function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : Integer) : Integer; overload;
  function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : Extended) : Extended; overload;
  function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : TObject) : TObject; overload;

var
  path : TEnvironmentPath;

implementation

{TFileHelper}

{$IFNDEF FPC}
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
{$IFDEF DELPHILINUX}
class function TFileHelper.IsInUse(const FileName : string) : Boolean;
var
  fs : TFileStream;
begin
  try
    fs := TFileStream.Create(FileName, fmOpenReadWrite, fmShareExclusive);
    Result := True;
    fs.Free;
  except
    Result := False;
  end;

end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TFileHelper.GetSize(const FileName: String): Int64;
var
  info: TWin32FileAttributeData;
begin
  Result := -1;
  if not GetFileAttributesEx(PWideChar(FileName), GetFileExInfoStandard, @info) then Exit;
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

function RandomString(const aLength: Integer) : string;
const
  chars : string = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';
var
  i : Integer;
  clong : Integer;
begin
  clong := High(chars);
  SetLength(Result, aLength);
  for i := 1 to aLength do
  begin
    Result[i] := chars[Random(clong) + 1];
  end;
end;

function ExtractFileNameWithoutExt(const FileName: string): string;
begin
  Result := TPath.GetFileNameWithoutExtension(FileName);
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

procedure GetEnvironmentPaths;
begin
  //gets path
  path.EXEPATH := TPath.GetDirectoryName(ParamStr(0));
  {$IFDEF MSWINDOWS}
  path.WINDOWS := SysUtils.GetEnvironmentVariable('windir');
  path.PROGRAMFILES := SysUtils.GetEnvironmentVariable('ProgramFiles');
  path.COMMONFILES := SysUtils.GetEnvironmentVariable('CommonProgramFiles(x86)');
  path.HOMEDRIVE := SysUtils.GetEnvironmentVariable('SystemDrive');
  path.USERPROFILE := SysUtils.GetEnvironmentVariable('USERPROFILE');
  path.PROGRAMDATA := SysUtils.GetEnvironmentVariable('ProgramData');
  path.ALLUSERSPROFILE := SysUtils.GetEnvironmentVariable('AllUsersProfile');
  path.INSTDRIVE := path.HOMEDRIVE;
  path.TEMP := SysUtils.GetEnvironmentVariable('TEMP');
  //these paths fail if user is SYSTEM
  try
    path.SYSTEM := GetSpecialFolderPath(CSIDL_SYSTEM);
    path.APPDATA := GetSpecialFolderPath(CSIDL_APPDATA);
    path.DESKTOP := GetSpecialFolderPath(CSIDL_DESKTOP);
    path.DESKTOP_ALLUSERS := GetSpecialFolderPath(CSIDL_COMMON_DESKTOPDIRECTORY);
    path.STARTMENU:=GetSpecialFolderPath(CSIDL_PROGRAMS);
    path.STARTMENU_ALLUSERS:=GetSpecialFolderPath(CSIDL_COMMON_PROGRAMS);
    path.STARTMENU_ALLUSERS := path.STARTMENU;
    path.STARTUP:=GetSpecialFolderPath(CSIDL_STARTUP);
  except
    //
  end;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
function GetSpecialFolderPath(folderID : Integer) : string;
var
  ppidl: PItemIdList;
begin
  SHGetSpecialFolderLocation(0, folderID, ppidl);
  SetLength(Result, MAX_PATH);
  if not SHGetPathFromIDList(ppidl,{$IFDEF FPC}PAnsiChar(Result){$ELSE}PChar(Result){$ENDIF}) then
  begin
    raise EShellError.create(Format('GetSpecialFolderPath: Invalid PIPL (%d)',[folderID]));
  end;
  SetLength(Result, lStrLen({$IFDEF FPC}PAnsiChar(Result){$ELSE}PChar(Result){$ENDIF}));
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
{$ENDIF}

function HasConsoleOutput : Boolean;
{$IFDEF MSWINDOWS}
  var
    stout : THandle;
  begin
    try
      stout := GetStdHandle(Std_Output_Handle);
      Win32Check(stout <> Invalid_Handle_Value);
      Result := stout <> 0;
    except
      Result := False;
    end;
  end;
{$ELSE}
  begin
    Result := IsConsole;
  end;
{$ENDIF}

function IsDebug: Boolean;
begin
  {$IFDEF DEBUG}
    Result := True;
  {$ELSE}
    Result := False;
  {$ENDIF DEBUG}
end;

{$IFDEF MSWINDOWS}
function IsService : Boolean;
begin
  //only working with my Quick.AppService unit
  try
    Result := (IsConsole) and (not HasConsoleOutput);
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

{$IFDEF FPC}
function DateTimeInRange(ADateTime: TDateTime; AStartDateTime, AEndDateTime: TDateTime; aInclusive: Boolean = True): Boolean;
begin
  if aInclusive then
    Result := (AStartDateTime <= ADateTime) and (ADateTime <= AEndDateTime)
  else
    Result := (AStartDateTime < ADateTime) and (ADateTime < AEndDateTime);
end;
{$ENDIF}

function IsSameDay(cBefore, cNow : TDateTime) : Boolean;
begin
  //Test: Result := MinutesBetween(cBefore,cNow) < 1;
  Result := DateTimeInRange(cNow,StartOfTheDay(cBefore),EndOfTheDay(cBefore),True);
end;

function ChangeTimeOfADay(aDate : TDateTime; aHour, aMinute, aSecond : Word; aMilliSecond : Word = 0) : TDateTime;
var
  y, m, d : Word;
begin
  DecodeDate(aDate,y,m,d);
  Result := EncodeDateTime(y,m,d,aHour,aMinute,aSecond,aMilliSecond);
end;

function ChangeDateOfADay(aDate : TDateTime; aYear, aMonth, aDay : Word) : TDateTime;
var
  h, m, s, ms : Word;
begin
  DecodeTime(aDate,h,m,s,ms);
  Result := EncodeDateTime(aYear,aMonth,aDay,h,m,s,0);
end;

function FillStr(const C : Char; const Count : Integer) : string;
var
  i   : Integer;
begin
  Result := '';
  for i := 1 to Count do Result := Result + C;
end;


function StrInArray(const aValue : string; const aInArray : array of string) : Boolean;
var
  s : string;
begin
  for s in aInArray do
  begin
    if s = aValue then Exit(True);
  end;
  Result := False;
end;

function IntInArray(const aValue : Integer; const aInArray : array of Integer) : Boolean;
var
  i : Integer;
begin
  for i in aInArray do
  begin
    if i = aValue then Exit(True);
  end;
  Result := False;
end;

function IsEmptyArray(aArray : TArray<string>) : Boolean;
begin
  Result := Length(aArray) = 0;
end;

function IsEmptyArray(aArray : TArray<Integer>) : Boolean;
begin
  Result := Length(aArray) = 0;
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
{$IFDEF MSWINDOWS}
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
{$ELSE}
  {$IF DEFINED(FPC) AND DEFINED(LINUX)}
  begin
    Result := GetEnvironmentVariable('USERNAME');
  end;
  {$ELSE}
  var
    {$IFNDEF NEXTGEN}
    plogin : PAnsiChar;
    {$ELSE}
    plogin : MarshaledAString;
    {$ENDIF}
  begin
    {$IFDEF POSIX}
    try
      plogin := getlogin;
      Result := Copy(plogin,1,Length(Trim(plogin)));
    except
      Result := 'N/A';
    end;
    {$ELSE}
    Result := 'N/A';
    {$ENDIF}
    //raise ENotImplemented.Create('Not Android GetLoggedUserName implemented!');
  end;
  {$ENDIF}
{$ENDIF}

{$IFDEF IOS}
function GetDeviceModel : String;
var
  size : size_t;
  buffer : array of Byte;
begin
  sysctlbyname('hw.machine',nil,@size,nil,0);
  if size > 0 then
  begin
    SetLength(buffer, size);
    sysctlbyname('hw.machine',@buffer[0],@size,nil,0);
    Result := UTF8ToString(MarshaledAString(buffer));
  end
  else Result := EmptyStr;
end;
{$ENDIF}

function GetComputerName : string;
{$IFDEF MSWINDOWS}
  var
    dwLength: dword;
  begin
    dwLength := 253;
    SetLength(Result, dwLength+1);
    if not Windows.GetComputerName(pchar(result), dwLength) then Result := 'Not detected!';
    Result := pchar(result);
  end;
{$ELSE}
  {$IF DEFINED(FPC) AND DEFINED(LINUX)}
  begin
    Result := GetEnvironmentVariable('COMPUTERNAME');
  end;
  {$ELSE} //Android gets model name
    {$IFDEF NEXTGEN}
    begin
      {$IFDEF ANDROID}
      Result := JStringToString(TJBuild.JavaClass.MODEL);
      {$ELSE} //IOS
      Result := GetDeviceModel;
      {$ENDIF}
    end;
    {$ELSE}
      {$IFDEF DELPHILINUX}
      var
        phost : PAnsiChar;
      begin
        try
          if gethostname(phost,_SC_HOST_NAME_MAX) = 0 then
          begin
            {$IFDEF DEBUG}
            Result := Copy(Trim(phost),1,Length(Trim(phost)));
            {$ELSE}
            Result := Copy(phost,1,Length(phost));
            {$ENDIF}
          end
          else Result := 'N/A.';
        except
          Result := 'N/A';
        end;
      end;
      {$ELSE} //OSX
      begin
        Result := NSStrToStr(TNSHost.Wrap(TNSHost.OCClass.currentHost).localizedName);
      end;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
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

function GetPathDelimiter(const aPath : string) : string;
begin
  if aPath.Contains('/') then Result := '/'
    else if aPath.Contains('\') then Result := '\'
    else Result := '';
end;

function GetFirstPathSegment(const aPath : string) : string;
var
  delimiter : string;
  spath : string;
begin
  delimiter := GetPathDelimiter(aPath);
  if delimiter.IsEmpty then Exit(aPath);
  if aPath.StartsWith(delimiter) then spath := Copy(aPath,2,aPath.Length)
    else spath := aPath;
  Result := Copy(spath,0,spath.IndexOf(delimiter));
end;

function GetLastPathSegment(const aPath : string) : string;
var
  delimiter : string;
  spath : string;
begin
  delimiter := GetPathDelimiter(aPath);
  if delimiter.IsEmpty then Exit(aPath);
  if aPath.EndsWith(delimiter) then spath := Copy(aPath,0,aPath.Length - 1)
    else spath := aPath;
  Result := spath.Substring(spath.LastDelimiter(delimiter)+1);
end;

function ParamFindSwitch(const Switch : string) : Boolean;
begin
  Result := FindCmdLineSwitch(Switch,['-', '/'],True);
end;

{$IFDEF FPC}
function FindCmdLineSwitch(const Switch: string; var Value: string; IgnoreCase: Boolean = True;
  const SwitchTypes: TCmdLineSwitchTypes = [clstValueNextParam, clstValueAppended]): Boolean; overload;
type
  TCompareProc = function(const S1, S2: string): Boolean;
var
  Param: string;
  I, ValueOfs,
  SwitchLen, ParamLen: Integer;
  SameSwitch: TCompareProc;
begin
  Result := False;
  Value := '';
  if IgnoreCase then
    SameSwitch := SameText else
    SameSwitch := SameStr;
  SwitchLen := Switch.Length;

  for I := 1 to ParamCount do
  begin
    Param := ParamStr(I);
    if CharInSet(Param.Chars[0], SwitchChars) and SameSwitch(Param.SubString(1,SwitchLen), Switch) then
    begin
      ParamLen := Param.Length;
      // Look for an appended value if the param is longer than the switch
      if (ParamLen > SwitchLen + 1) then
      begin
        // If not looking for appended value switches then this is not a matching switch
        if not (clstValueAppended in SwitchTypes) then
          Continue;
        ValueOfs := SwitchLen + 1;
        if Param.Chars[ValueOfs] = ':' then
          Inc(ValueOfs);
        Value := Param.SubString(ValueOfs, MaxInt);
      end
      // If the next param is not a switch, then treat it as the value
      else if (clstValueNextParam in SwitchTypes) and (I < ParamCount) and
              not CharInSet(ParamStr(I+1).Chars[0], SwitchChars) then
        Value := ParamStr(I+1);
      Result := True;
      Break;
    end;
  end;
end;
{$ENDIF}

function ParamGetSwitch(const Switch : string; var cvalue : string) : Boolean;
begin
  Result := FindCmdLineSwitch(Switch,cvalue,True,[clstValueAppended]);
end;


function GetAppName : string;
begin
  Result := ExtractFilenameWithoutExt(ParamStr(0));
end;

function GetAppVersionStr: string;
{$IFDEF MSWINDOWS}
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
{$ELSE}
  {$IF DEFINED(FPC) AND DEFINED(LINUX)}
  var
    version : TProgramVersion;
  begin
    if GetProgramVersion(version) then Result := Format('%d.%d', [version.Major, version.Minor])
      else Result := '';
  end;
  {$ELSE}
    {$IFDEF NEXTGEN}
      {$IFDEF ANDROID}
      var
        PkgInfo : JPackageInfo;
      begin
        PkgInfo := SharedActivity.getPackageManager.getPackageInfo(SharedActivity.getPackageName,0);
        Result := IntToStr(PkgInfo.VersionCode);
      end;
      {$ELSE} //IOS
      var
        AppKey: Pointer;
        AppBundle: NSBundle;
        BuildStr : NSString;
      begin
        try
          AppKey := (StrToNSStr('CFBundleVersion') as ILocalObject).GetObjectID;
          AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
          BuildStr := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(AppKey));
          Result := UTF8ToString(BuildStr.UTF8String);
        except
          Result := '';
        end;
      end;
      {$ENDIF}
    {$ELSE} //OSX
      {$IFDEF OSX}
      var
        AppKey: Pointer;
        AppBundle: NSBundle;
        BuildStr : NSString;
      begin
        try
          AppKey := (StrToNSStr('CFBundleVersion') as ILocalObject).GetObjectID;

          AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
          BuildStr := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(AppKey));
          Result := UTF8ToString(BuildStr.UTF8String);

        except
          Result := '';
        end;
      end;
      {$ELSE}
        begin
          Result := '';
        end;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

function GetAppVersionFullStr: string;
{$IFDEF MSWINDOWS}
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
{$ELSE}
  {$IF DEFINED(FPC) AND DEFINED(LINUX)}
  var
    version : TProgramVersion;
  begin
    if GetProgramVersion(version) then Result := Format('%d.%d.%d.%d', [version.Major, version.Minor, version.Revision, version.Build])
      else Result := '';
  end;
  {$ELSE}
    {$IFDEF NEXTGEN}
      {$IFDEF ANDROID}
      var
        PkgInfo : JPackageInfo;
      begin
        PkgInfo := SharedActivity.getPackageManager.getPackageInfo(SharedActivity.getPackageName,0);
        Result := JStringToString(PkgInfo.versionName);
      end;
      {$ELSE} //IOS
      var
        AppKey: Pointer;
        AppBundle: NSBundle;
        BuildStr : NSString;
      begin
        AppKey := (StrToNSStr('CFBundleVersion') as ILocalObject).GetObjectID;
        AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
        BuildStr := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(AppKey));
        Result := UTF8ToString(BuildStr.UTF8String);
      end;
      {$ENDIF}
    {$ELSE}
      {$IFDEF OSX}
      var
        AppKey: Pointer;
        AppBundle: NSBundle;
        BuildStr : NSString;
      begin
        AppKey := (StrToNSStr('CFBundleVersion') as ILocalObject).GetObjectID;
        AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
        BuildStr := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(AppKey));
        Result := UTF8ToString(BuildStr.UTF8String);
      end;
      {$ELSE}
        begin
          Result := '';
        end;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

function UTCToLocalTime(GMTTime: TDateTime): TDateTime;
begin
  {$IFDEF FPC}
  Result := LocalTimeToUniversal(GMTTime);
  {$ELSE}
  Result :=  TTimeZone.Local.ToLocalTime(GMTTime);
  {$ENDIF}
end;

function LocalTimeToUTC(LocalTime : TDateTime): TDateTime;
begin
  {$IFDEF FPC}
  Result := UniversalTimeToLocal(Localtime);
  {$ELSE}
  Result := TTimeZone.Local.ToUniversalTime(LocalTime);
  {$ENDIF}
end;

function DateTimeToGMT(aDate : TDateTime) : string;
var
  FmtSettings : TFormatSettings;
begin
  FmtSettings.DateSeparator := '-';
  FmtSettings.TimeSeparator := ':';
  FmtSettings.ShortDateFormat := 'YYYY-MM-DD"T"HH:NN:SS.ZZZ" GMT"';
  Result := DateTimeToStr(aDate,FmtSettings).Trim;
end;

function GMTToDateTime(aDate : string) : TDateTime;
var
  FmtSettings : TFormatSettings;
begin
  FmtSettings.DateSeparator := '-';
  FmtSettings.TimeSeparator := ':';
  FmtSettings.ShortDateFormat := 'YYYY-MM-DD"T"HH:NN:SS.ZZZ" GMT"';
  Result := StrToDateTime(aDate,FmtSettings);
end;

function DateTimeToJsonDate(aDateTime : TDateTime) : string;
{$IFNDEF DELPHIXE7_UP}
var
  FmtSettings : TFormatSettings;
{$ENDIF}
begin
  {$IFDEF DELPHIXE7_UP}
  Result := DateToISO8601(aDateTime);
  {$ELSE}
  FmtSettings.DateSeparator := '-';
  FmtSettings.TimeSeparator := ':';
  FmtSettings.ShortDateFormat := 'YYYY-MM-DD"T"HH:NN:SS.ZZZ"Z"';
  Result := DateTimeToStr(aDateTime,FmtSettings).Trim;
  {$ENDIF}
end;

function JsonDateToDateTime(const aJsonDate : string) : TDateTime;
{$IFNDEF DELPHIXE7_UP}
var
  FmtSettings : TFormatSettings;
{$ENDIF}
{$IFDEF FPC}
var
  jdate : string;
{$ENDIF}
begin
  {$IFDEF DELPHIXE7_UP}
  Result := ISO8601ToDate(aJsonDate);
  {$ELSE}
  FmtSettings.DateSeparator := '-';
  FmtSettings.TimeSeparator := ':';
  FmtSettings.ShortDateFormat := 'YYYY-MM-DD"T"HH:NN:SS.ZZZ"Z"';
    {$IFDEF FPC}
    jdate := StringReplace(aJsondate,'T',' ',[rfIgnoreCase]);
    jdate := Copy(jdate,1,Pos('.',jdate)-1);
    Result := StrToDateTime(jdate,FmtSettings);
    {$ELSE}
    Result := StrToDateTime(aJsonDate,FmtSettings);
    {$ENDIF}
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

function CountStr(const aFindStr, aSourceStr : string) : Integer;
var
  i : Integer;
  found : Integer;
  findstr : string;
  mainstr : string;
begin
  findstr := aFindStr.ToLower;
  mainstr := aSourceStr.ToLower;
  Result := 0;
  i := 0;
  while i < mainstr.Length do
  begin
    found := Pos(findstr,mainstr,i);
    if found > 0 then
    begin
      i := found;
      Inc(Result);
    end
    else Break;
  end;
end;

procedure SaveStreamToFile(aStream : TStream; const aFileName : string);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(aFileName,fmCreate);
  try
    aStream.Seek(0,soBeginning);
    fs.CopyFrom(aStream,aStream.Size);
  finally
    fs.Free;
  end;
end;

function StreamToString(aStream : TStream) : string;
var
  ss : TStringStream;
begin
  aStream.Position := 0;
  if aStream = nil then Exit;
  if aStream is TMemoryStream then
  begin
    SetString(Result, PChar(TMemoryStream(aStream).Memory), TMemoryStream(aStream).Size div SizeOf(Char));
  end
  else if aStream is TStringStream then
  begin
    Result := TStringStream(aStream).DataString;
  end
  else
  begin
    ss := TStringStream.Create;
    try
      aStream.Seek(0,soBeginning);
      ss.CopyFrom(aStream,aStream.Size);
      Result := ss.DataString;
    finally
      ss.Free;
    end;
  end;
end;

function StreamToString2(const aStream: TStream; const aEncoding: TEncoding): string;
var
  sbytes: TBytes;
begin
  aStream.Position := 0;
  SetLength(sbytes, aStream.Size);
  aStream.ReadBuffer(sbytes,aStream.Size);
  Result := aEncoding.GetString(sbytes);
end;

procedure StringToStream(const aStr : string; aStream : TStream);
begin
  aStream.Seek(0,soBeginning);
  aStream.WriteBuffer(Pointer(aStr)^,aStr.Length * SizeOf(Char));
end;

procedure StringToStream2(const aStr : string; aStream : TStream);
var
  stream : TStringStream;
begin
  stream := TStringStream.Create(aStr,TEncoding.UTF8);
  try
    aStream.CopyFrom(stream,stream.Size);
  finally
    stream.Free;
  end;
end;


function CommaText(aList : TStringList) : string;
var
  value : string;
  sb : TStringBuilder;
begin
  if aList.Text = '' then Exit;
  sb := TStringBuilder.Create;
  try
    for value in aList do
    begin
      sb.Append(value);
      sb.Append(',');
    end;
    if sb.Length > 1 then Result := sb.ToString(0, sb.Length - 1);
  finally
    sb.Free;
  end;
end;

function CommaText(aArray : TArray<string>) : string;
var
  value : string;
  sb : TStringBuilder;
begin
  if High(aArray) < 0 then Exit;
  sb := TStringBuilder.Create;
  try
    for value in aArray do
    begin
      sb.Append(value);
      sb.Append(',');
    end;
    if sb.Length > 1 then Result := sb.ToString(0, sb.Length - 1);
  finally
    sb.Free;
  end;
end;

function ArrayToString(aArray : TArray<string>) : string;
var
  value : string;
  sb : TStringBuilder;
begin
  if High(aArray) < 0 then Exit;
  sb := TStringBuilder.Create;
  try
    for value in aArray do
    begin
      sb.Append(value);
      sb.Append(#10#13);
    end;
  finally
    sb.Free;
  end;
end;

function StringsToArray(aStrings : TStrings) : TArray<string>;
var
  i : Integer;
begin
  if aStrings.Count = 0 then Exit;
  SetLength(Result,aStrings.Count);
  for i := 0 to aStrings.Count - 1 do
  begin
    Result[i] := aStrings[i];
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

{ TArrayOfStringHelper}

{$IFNDEF FPC}
function TArrayOfStringHelper.Any : Boolean;
begin
  Result := High(Self) >= 0;
end;

function TArrayOfStringHelper.Any(const aValue : string) : Boolean;
begin
  Result := Exists(aValue);
end;

function TArrayOfStringHelper.Add(const aValue : string) : Integer;
begin
  SetLength(Self,Length(Self)+1);
  Self[High(Self)] := aValue;
  Result := High(Self);
end;

function TArrayOfStringHelper.AddIfNotExists(const aValue : string; aCaseSense : Boolean = False) : Integer;
var
  i : Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if aCaseSense then
    begin
      if Self[i] = aValue then Exit(i);
    end
    else
    begin
      if CompareText(Self[i],aValue) = 0 then Exit(i)
    end;
  end;
  //if not exists add it
  Result := Self.Add(aValue);
end;

function TArrayOfStringHelper.Remove(const aValue : string) : Boolean;
var
  i : Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if CompareText(Self[i],aValue) = 0 then
    begin
      {$IFDEF DELPHIXE7_UP}
      System.Delete(Self,i,1);
      {$ELSE}
      TArrayUtil<string>.Delete(Self,i);
      {$ENDIF}
      Exit(True);
    end;
  end;
  Result := False;
end;

function TArrayOfStringHelper.Exists(const aValue : string) : Boolean;
var
  value : string;
begin
  Result := False;
  for value in Self do
  begin
    if CompareText(value,aValue) = 0 then Exit(True)
  end;
end;

function TArrayOfStringHelper.Count : Integer;
begin
  Result := High(Self) + 1;
end;
{$ENDIF}

{ TPairItem }

constructor TPairItem.Create(const aName, aValue: string);
begin
  Name := aName;
  Value := aValue;
end;

{ TPairList }

function TPairList.GetEnumerator : TPairEnumerator;
begin
  Result := TPairEnumerator.Create(fItems);
end;

function TPairList.Add(aPair: TPairItem): Integer;
begin
  SetLength(fItems,Length(fItems)+1);
  fItems[High(fItems)] := aPair;
  Result := High(fItems);
end;

function TPairList.Add(const aName, aValue: string): Integer;
begin
  SetLength(fItems,Length(fItems)+1);
  fItems[High(fItems)].Name := aName;
  fItems[High(fItems)].Value := aValue;
  Result := High(fItems);
end;

procedure TPairList.AddOrUpdate(const aName, aValue: string);
var
  i : Integer;
begin
  for i := Low(fItems) to High(fItems) do
  begin
    if CompareText(fItems[i].Name,aName) = 0 then
    begin
      fItems[i].Value := aValue;
      Exit;
    end;
  end;
  //if not exists add it
  Self.Add(aName,aValue);
end;

function TPairList.Count: Integer;
begin
  Result := High(fItems) + 1;
end;

function TPairList.Exists(const aName: string): Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := Low(fItems) to High(fItems) do
  begin
    if CompareText(fItems[i].Name,aName) = 0 then Exit(True)
  end;
end;

function TPairList.GetPair(const aName: string): TPairItem;
var
  i : Integer;
begin
  for i := Low(fItems) to High(fItems) do
  begin
    if CompareText(fItems[i].Name,aName) = 0 then Exit(fItems[i]);
  end;
end;

function TPairList.GetValue(const aName: string): string;
var
  i : Integer;
begin
  Result := '';
  for i := Low(fItems) to High(fItems) do
  begin
    if CompareText(fItems[i].Name,aName) = 0 then Exit(fItems[i].Value);
  end;
end;

function TPairList.Remove(const aName: string): Boolean;
var
  i : Integer;
begin
  for i := Low(fItems) to High(fItems) do
  begin
    if CompareText(fItems[i].Name,aName) = 0 then
    begin
      {$IF Defined(DELPHIXE7_UP) OR Defined(FPC)}
      System.Delete(fItems,i,1);
      {$ELSE}
      TArrayUtil<TPairItem>.Delete(fItems,i);
      {$ENDIF}
      Exit(True);
    end;
  end;
  Result := False;
end;

function TPairList.ToArray : TArray<TPairItem>;
begin
  Result := fItems;
end;

procedure TPairList.FromArray(aValue : TArray<TPairItem>);
begin
  fItems := aValue;
end;

procedure TPairList.Clear;
begin
  SetLength(fItems,0);
end;

{ TPairList.TPairEnumerator}

constructor TPairList.TPairEnumerator.Create(var aArray: TArray<TPairItem>);
begin
  fIndex := -1;
  fArray := @aArray;
end;

function TPairList.TPairEnumerator.GetCurrent : TPairItem;
begin
  Result := TArray<TPairItem>(fArray^)[fIndex];
end;

function TPairList.TPairEnumerator.MoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex < High(TArray<TPairItem>(fArray^))+1;
end;

{$IFDEF MSWINDOWS}
procedure ProcessMessages;
var
  Msg: TMsg;
begin
  while integer(PeekMessage(Msg, 0, 0, 0, PM_REMOVE)) <> 0 do
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

function GetLastOSError: String;
begin
  Result := SysErrorMessage(Windows.GetLastError);
end;
{$ENDIF}

function RemoveLastChar(const aText : string) : string;
begin
  Result := aText.Remove(aText.Length - 1);
end;

function DateTimeToSQL(aDateTime : TDateTime) : string;
begin
  Result := FormatDateTime('YYYYMMDD hh:mm:ss',aDateTime);
end;

function IsInteger(const aValue : string) : Boolean;
var
  i : Integer;
begin
  Result := TryStrToInt(aValue,i);
end;

function ExtractStr(var vSource : string; aIndex : Integer; aCount : Integer) : string;
begin
  if aIndex > vSource.Length then Exit('');

  Result := Copy(vSource,aIndex,aCount);
  Delete(vSource,aIndex,aCount);
end;

function GetSubString(const aSource, aFirstDelimiter, aLastDelimiter : string) : string;
var
  i : Integer;
begin
  i := Pos(aFirstDelimiter,aSource);
  if i > -1 then Result := Copy(aSource, i + aFirstDelimiter.Length, Pos(aLastDelimiter, aSource, i + aFirstDelimiter.Length) - i - aFirstDelimiter.Length)
    else Result := '';
end;

function DbQuotedStr(const str : string): string;
var
  i : Integer;
begin
  Result := str;
  for i := Result.Length - 1 downto 0 do
  begin
    if Result.Chars[i] = '"' then Result := Result.Insert(i, '"');
  end;
  Result := '"' + Result + '"';
end;

function UnDbQuotedStr(const str: string) : string;
begin
  Result := Trim(str);
  if not Result.IsEmpty then
  begin
    if Result.StartsWith('"') then Result := Copy(Result, 2, Result.Length - 2);
  end;
end;

function SpQuotedStr(const str : string): string;
begin
  Result := '''' + str + '''';
end;

function UnSpQuotedStr(const str: string) : string;
begin
  Result := Trim(str);
  if not Result.IsEmpty then
  begin
    if Result.StartsWith('''') then Result := Copy(Result, 2, Result.Length - 2);
  end;
end;

function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : string) : string;
begin
  if aCondition then Result := aIfIsTrue else Result := aIfIsFalse;
end;

function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : Integer) : Integer;
begin
  if aCondition then Result := aIfIsTrue else Result := aIfIsFalse;
end;

function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : Extended) : Extended;
begin
  if aCondition then Result := aIfIsTrue else Result := aIfIsFalse;
end;

function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : TObject) : TObject;
begin
  if aCondition then Result := aIfIsTrue else Result := aIfIsFalse;
end;

{$IFNDEF FPC}
  {$IFNDEF DELPHIXE7_UP}
  class procedure TArrayUtil<T>.Delete(var aArray : TArray<T>; aIndex : Integer);
  var
    n : Integer;
    len : Integer;
  begin
    len := Length(aArray);
    if (len > 0) and (aIndex < len) then
    begin
      for n := aIndex + 1 to len - 1 do aArray[n - 1] := aArray[n];
      SetLength(aArray, len - 1);
    end;
  end;
  {$ENDIF}
{$ENDIF}

{$IFNDEF NEXTGEN}
initialization
  try
    GetEnvironmentPaths;
  except
    {$IFDEF SHOW_ENVIRONMENTPATH_ERRORS}
    on E : Exception do
    begin
      if not IsService then
      begin
        if HasConsoleOutput then Writeln(Format('[WARN] GetEnvironmentPaths: %s',[E.Message]))
          else MessageBox(0,PWideChar(Format('Get environment path error: %s',[E.Message])),'GetEnvironmentPaths',MB_ICONEXCLAMATION);
      end;
    end;
    {$ENDIF}
  end;
{$ENDIF}

end.


