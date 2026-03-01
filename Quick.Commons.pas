{ ***************************************************************************

  Copyright (c) 2016-2025 Kike Pérez

  Unit        : Quick.Commons
  Description : Common functions
  Author      : Kike Pérez
  Version     : 2.0
  Created     : 14/07/2017
  Modified    : 01/03/2026

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
      ActiveX,
      ShlObj,
    {$ENDIF MSWINDOWS}
    {$IFDEF FPC}
    Quick.Files,
      {$IFDEF LINUX}
      FileInfo,
      BaseUnix,
      {$ENDIF}
    {$ELSE}
    IOUtils,
    {$ENDIF}
    {$IFDEF ANDROID}
    Androidapi.JNI.Os,
    Androidapi.Helpers,
    Androidapi.JNI.JavaTypes,
    Androidapi.JNI.GraphicsContentViewText,
    {$IFDEF DELPHIRX103_UP}
      Androidapi.JNI.App,
    {$ENDIF}
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
    Posix.Pwd,
    {$ENDIF}
    DateUtils;

type
  /// <summary>
  /// Defines the different types of log events available in the logging system.
  /// </summary>
  TLogEventType = (etInfo, etSuccess, etWarning, etError, etDebug, etDone, etTrace, etCritical, etException);
  
  /// <summary>
  /// Set of TLogEventType used to define which log event types should be processed.
  /// </summary>
  TLogVerbose = set of TLogEventType;

const
  /// <summary>Log verbosity level that only captures informational and error events.</summary>
  LOG_ONLYERRORS = [etInfo,etError];
  /// <summary>Log verbosity level that captures informational, warning, and error events.</summary>
  LOG_ERRORSANDWARNINGS = [etInfo,etWarning,etError];
  /// <summary>Log verbosity level that captures informational, error, warning, and trace events.</summary>
  LOG_TRACE = [etInfo,etError,etWarning,etTrace];
  /// <summary>Log verbosity level that captures all event types.</summary>
  LOG_ALL = [etInfo, etSuccess, etWarning, etError, etDebug, etDone, etTrace, etCritical, etException];
  /// <summary>Log verbosity level that captures debug-related events.</summary>
  LOG_DEBUG = [etInfo,etSuccess,etWarning,etError,etDebug];
  {$IFDEF DELPHIXE7_UP}
  /// <summary>Array of string representations for each TLogEventType.</summary>
  EventStr : array of string = ['INFO','SUCC','WARN','ERROR','DEBUG','DONE','TRACE','CRITICAL','EXCEPTION'];
  {$ELSE}
  EventStr : array[0..8] of string = ('INFO','SUCC','WARN','ERROR','DEBUG','DONE','TRACE','CRITICAL','EXCEPTION');
  {$ENDIF}
  /// <summary>Carriage Return + Line Feed constant for line breaks.</summary>
  CRLF = #13#10;
type
  /// <summary>
  /// Defines password complexity requirements.
  /// </summary>
  /// <remarks>
  /// pfIncludeNumbers: Include numeric digits in the password
  /// pfIncludeSigns: Include special characters in the password
  /// </remarks>
  TPasswordComplexity = set of (pfIncludeNumbers,pfIncludeSigns);

  /// <summary>
  /// Record containing paths to common environment and system directories.
  /// </summary>
  /// <remarks>
  /// Provides easy access to frequently used system paths such as temp directories,
  /// desktop, program files, etc. Automatically populated at initialization.
  /// </remarks>
  TEnvironmentPath = record
    /// <summary>Path to the executable directory.</summary>
    EXEPATH : string;
    {$IFDEF MSWINDOWS}
    /// <summary>Windows system directory path.</summary>
    WINDOWS : string;
    /// <summary>Windows system32 directory path.</summary>
    SYSTEM : string;
    /// <summary>Program Files directory path.</summary>
    PROGRAMFILES : string;
    /// <summary>Common Files directory path.</summary>
    COMMONFILES : string;
    /// <summary>System drive (typically C:).</summary>
    HOMEDRIVE : string;
    /// <summary>Temporary files directory path.</summary>
    TEMP : string;
    /// <summary>User profile directory path.</summary>
    USERPROFILE : string;
    /// <summary>Installation drive.</summary>
    INSTDRIVE : string;
    /// <summary>Current user's desktop directory path.</summary>
    DESKTOP : string;
    /// <summary>Current user's start menu directory path.</summary>
    STARTMENU : string;
    /// <summary>All users desktop directory path.</summary>
    DESKTOP_ALLUSERS : string;
    /// <summary>All users start menu directory path.</summary>
    STARTMENU_ALLUSERS : string;
    /// <summary>Startup directory path.</summary>
    STARTUP : string;
    /// <summary>Application data directory path.</summary>
    APPDATA : String;
    /// <summary>ProgramData directory path.</summary>
    PROGRAMDATA : string;
    /// <summary>All users profile directory path.</summary>
    ALLUSERSPROFILE : string;
    {$ENDIF MSWINDOWS}
  end;

  {$IFNDEF FPC}
  /// <summary>
  /// Helper record for TFile class providing additional file operations.
  /// </summary>
  TFileHelper = record helper for TFile
    {$IF DEFINED(MSWINDOWS) OR DEFINED(DELPHILINUX)}
    /// <summary>
    /// Checks if a file is currently in use by another process.
    /// </summary>
    /// <param name="FileName">Full path to the file to check.</param>
    /// <returns>True if the file is in use, False otherwise.</returns>
    class function IsInUse(const FileName : string) : Boolean; static;
    {$ENDIF}
    /// <summary>
    /// Gets the size of a file in bytes.
    /// </summary>
    /// <param name="FileName">Full path to the file.</param>
    /// <returns>File size in bytes, or -1 if the file doesn't exist.</returns>
    class function GetSize(const FileName: String): Int64; static;
  end;

  /// <summary>
  /// Helper record for TDirectory class providing additional directory operations.
  /// </summary>
  TDirectoryHelper = record helper for TDirectory
    /// <summary>
    /// Calculates the total size of all files in a directory.
    /// </summary>
    /// <param name="Path">Full path to the directory.</param>
    /// <returns>Total size in bytes of all files in the directory.</returns>
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

  /// <summary>
  /// Counter record that tracks iterations up to a maximum value.
  /// </summary>
  /// <remarks>
  /// Provides a simple mechanism to count up to a specific value and check when
  /// the maximum is reached. Automatically resets after reaching the maximum.
  /// </remarks>
  TCounter = record
  private
    fMaxValue : Integer;
    fCurrentValue : Integer;
  public
    /// <summary>Gets the maximum value this counter can reach.</summary>
    property MaxValue : Integer read fMaxValue;
    /// <summary>
    /// Initializes the counter with a maximum value.
    /// </summary>
    /// <param name="aMaxValue">Maximum count value.</param>
    procedure Init(aMaxValue : Integer);
    /// <summary>
    /// Returns the current count value.
    /// </summary>
    /// <returns>Current count value.</returns>
    function Count : Integer;
    /// <summary>
    /// Checks if current count equals a specific value.
    /// </summary>
    /// <param name="aValue">Value to compare against.</param>
    /// <returns>True if current count equals aValue.</returns>
    function CountIs(aValue : Integer) : Boolean;
    /// <summary>
    /// Increments the counter and checks if maximum is reached.
    /// </summary>
    /// <returns>True if maximum value is reached (counter resets), False otherwise.</returns>
    function Check : Boolean;
    /// <summary>
    /// Resets the counter to zero.
    /// </summary>
    procedure Reset;
  end;

  /// <summary>
  /// Time-based counter that tracks elapsed time in milliseconds.
  /// </summary>
  /// <remarks>
  /// Useful for implementing time-based events or throttling operations.
  /// Checks if a specific time interval has elapsed since last reset.
  /// </remarks>
  TTimeCounter = record
  private
    fCurrentTime : TDateTime;
    fDoneEvery : Integer;
  public
    /// <summary>Gets the time interval in milliseconds.</summary>
    property DoneEvery : Integer read fDoneEvery;
    /// <summary>
    /// Initializes the time counter with a millisecond interval.
    /// </summary>
    /// <param name="MillisecondsToReach">Time interval in milliseconds.</param>
    procedure Init(MillisecondsToReach : Integer);
    /// <summary>
    /// Checks if the time interval has elapsed.
    /// </summary>
    /// <returns>True if the interval has passed (counter resets), False otherwise.</returns>
    function Check : Boolean;
    /// <summary>
    /// Resets the counter to current time.
    /// </summary>
    procedure Reset;
  end;

  {$IFNDEF FPC}

  {$IFNDEF DELPHIXE7_UP}
  TArrayUtil<T> = class
    class procedure Delete(var aArray : TArray<T>; aIndex : Integer);
  end;
  {$ENDIF}

  /// <summary>
  /// Helper record extending TArray<string> with utility methods.
  /// </summary>
  TArrayOfStringHelper = record helper for TArray<string>
  public
    /// <summary>Checks if the array has any elements.</summary>
    /// <returns>True if array is not empty.</returns>
    function Any : Boolean; overload;
    /// <summary>Checks if the array contains a specific value.</summary>
    /// <param name="aValue">Value to search for.</param>
    /// <returns>True if value exists in array.</returns>
    function Any(const aValue : string) : Boolean; overload;
    /// <summary>Adds a value to the array.</summary>
    /// <param name="aValue">Value to add.</param>
    /// <returns>Index of the added element.</returns>
    function Add(const aValue : string) : Integer;
    /// <summary>Adds a value only if it doesn't already exist.</summary>
    /// <param name="aValue">Value to add.</param>
    /// <param name="aCaseSense">Whether comparison is case-sensitive.</param>
    /// <returns>Index of the element (existing or newly added).</returns>
    function AddIfNotExists(const aValue : string; aCaseSense : Boolean = False) : Integer;
    /// <summary>Removes a value from the array.</summary>
    /// <param name="aValue">Value to remove.</param>
    /// <returns>True if value was found and removed.</returns>
    function Remove(const aValue : string) : Boolean;
    /// <summary>Checks if a value exists in the array.</summary>
    /// <param name="aValue">Value to search for.</param>
    /// <returns>True if value exists.</returns>
    function Exists(const aValue : string) : Boolean;
    /// <summary>Gets the number of elements in the array.</summary>
    /// <returns>Number of elements.</returns>
    function Count : Integer;
  end;
  TDelegate<T> = reference to procedure(Value : T);
  {$ENDIF}

  /// <summary>
  /// Record representing a name-value pair.
  /// </summary>
  TPairItem = record
    /// <summary>Name/key of the pair.</summary>
    Name : string;
    /// <summary>Value associated with the name.</summary>
    Value : string;
    /// <summary>
    /// Creates a new pair item with specified name and value.
    /// </summary>
    /// <param name="aName">Name/key of the pair.</param>
    /// <param name="aValue">Value of the pair.</param>
    constructor Create(const aName, aValue : string);
  end;

  /// <summary>
  /// List class for managing name-value pairs.
  /// </summary>
  /// <remarks>
  /// Provides dictionary-like functionality using a dynamic array of TPairItem.
  /// Supports enumeration and indexed access by name.
  /// </remarks>
  TPairList = class
  type
    /// <summary>
    /// Enumerator for iterating over pair items.
    /// </summary>
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
    /// <summary>Gets an enumerator for the list.</summary>
    function GetEnumerator : TPairEnumerator;
    /// <summary>Gets the value for a given name.</summary>
    /// <param name="aName">Name to search for.</param>
    /// <returns>Value associated with the name, or empty string if not found.</returns>
    function GetValue(const aName : string) : string;
    /// <summary>Gets the pair item for a given name.</summary>
    /// <param name="aName">Name to search for.</param>
    /// <returns>TPairItem with the specified name.</returns>
    function GetPair(const aName : string) : TPairItem;
    /// <summary>Adds a pair item to the list.</summary>
    /// <param name="aPair">Pair item to add.</param>
    /// <returns>Index of the added item.</returns>
    function Add(aPair : TPairItem) : Integer; overload;
    /// <summary>Adds a name-value pair to the list.</summary>
    /// <param name="aName">Name/key of the pair.</param>
    /// <param name="aValue">Value of the pair.</param>
    /// <returns>Index of the added item.</returns>
    function Add(const aName, aValue : string) : Integer; overload;
    /// <summary>Adds or updates a name-value pair.</summary>
    /// <param name="aName">Name/key of the pair.</param>
    /// <param name="aValue">Value to set.</param>
    procedure AddOrUpdate(const aName, aValue : string);
    /// <summary>Checks if a name exists in the list.</summary>
    /// <param name="aName">Name to search for.</param>
    /// <returns>True if name exists.</returns>
    function Exists(const aName : string) : Boolean;
    /// <summary>Removes a pair by name.</summary>
    /// <param name="aName">Name of the pair to remove.</param>
    /// <returns>True if pair was found and removed.</returns>
    function Remove(const aName : string) : Boolean;
    /// <summary>Gets the number of pairs in the list.</summary>
    /// <returns>Number of pairs.</returns>
    function Count : Integer;
    /// <summary>Default indexed property for accessing values by name.</summary>
    property Items[const aName : string] : string read GetValue write AddOrUpdate;
    /// <summary>Converts the list to an array of pairs.</summary>
    /// <returns>Array containing all pair items.</returns>
    function ToArray : TArray<TPairItem>;
    /// <summary>Populates the list from an array of pairs.</summary>
    /// <param name="aValue">Array of pair items to load.</param>
    procedure FromArray(aValue : TArray<TPairItem>);
    /// <summary>Removes all pairs from the list.</summary>
    procedure Clear;
  end;

  {$IFDEF DELPHIXE7_UP}
  /// <summary>
  /// Helper record extending TDateTime with utility methods.
  /// </summary>
  TDateTimeHelper = record helper for TDateTime
  public
    /// <summary>Converts the datetime to SQL format string (YYYY-MM-DD hh:mm:ss).</summary>
    function ToSQLString : string;
    /// <summary>Sets the datetime to current time.</summary>
    procedure FromNow;
    /// <summary>Converts UTC time to local time.</summary>
    /// <param name="aUTCTime">UTC datetime value.</param>
    procedure FromUTC(const aUTCTime : TDateTime);
    /// <summary>Increments the datetime by specified number of days.</summary>
    /// <param name="aValue">Number of days to increment (default 1).</param>
    function IncDay(const aValue : Cardinal = 1) : TDateTime;
    /// <summary>Decrements the datetime by specified number of days.</summary>
    /// <param name="aValue">Number of days to decrement (default 1).</param>
    function DecDay(const aValue : Cardinal = 1) : TDateTime;
    /// <summary>Increments the datetime by specified number of months.</summary>
    /// <param name="aValue">Number of months to increment (default 1).</param>
    function IncMonth(const aValue : Cardinal = 1) : TDateTime;
    /// <summary>Decrements the datetime by specified number of months.</summary>
    /// <param name="aValue">Number of months to decrement (default 1).</param>
    function DecMonth(const aValue : Cardinal = 1) : TDateTime;
    /// <summary>Increments the datetime by specified number of years.</summary>
    /// <param name="aValue">Number of years to increment (default 1).</param>
    function IncYear(const aValue : Cardinal = 1) : TDateTime;
    /// <summary>Decrements the datetime by specified number of years.</summary>
    /// <param name="aValue">Number of years to decrement (default 1).</param>
    function DecYear(const aValue : Cardinal = 1) : TDateTime;
    /// <summary>Checks if this datetime equals another datetime.</summary>
    function IsEqualTo(const aDateTime : TDateTime) : Boolean;
    /// <summary>Checks if this datetime is after another datetime.</summary>
    function IsAfter(const aDateTime : TDateTime) : Boolean;
    /// <summary>Checks if this datetime is before another datetime.</summary>
    function IsBefore(const aDateTime : TDateTime) : Boolean;
    /// <summary>Checks if this datetime is on the same day as another datetime.</summary>
    function IsSameDay(const aDateTime : TDateTime) : Boolean;
    /// <summary>Checks if this datetime has the same time as another time.</summary>
    function IsSameTime(const aTime : TTime) : Boolean;
    /// <summary>Gets the day of the week (1-7).</summary>
    function DayOfTheWeek : Word;
    /// <summary>Converts datetime to JSON/ISO 8601 format string.</summary>
    function ToJsonFormat : string;
    /// <summary>Converts datetime to GMT format string.</summary>
    function ToGMTFormat: string;
    /// <summary>Converts datetime to TimeStamp.</summary>
    function ToTimeStamp : TTimeStamp;
    /// <summary>Converts local datetime to UTC.</summary>
    function ToUTC : TDateTime;
    /// <summary>Converts datetime to milliseconds.</summary>
    function ToMilliseconds : Int64;
    /// <summary>Converts datetime to string.</summary>
    function ToString : string;
    /// <summary>Extracts the date part of the datetime.</summary>
    function Date : TDate;
    /// <summary>Extracts the time part of the datetime.</summary>
    function Time : TTime;
    /// <summary>Checks if time is in AM (ante meridiem).</summary>
    function IsAM : Boolean;
    /// <summary>Checks if time is in PM (post meridiem).</summary>
    function IsPM : Boolean;
  end;

  /// <summary>
  /// Helper record extending TDate with utility methods.
  /// </summary>
  TDateHelper = record helper for TDate
  public
    /// <summary>Converts date to string.</summary>
    function ToString : string;
  end;

  /// <summary>
  /// Helper record extending TTime with utility methods.
  /// </summary>
  TTimeHelper = record helper for TTime
  public
    /// <summary>Converts time to string.</summary>
    function ToString : string;
  end;
  {$ENDIF}

  /// <summary>Exception raised when environment path operations fail.</summary>
  EEnvironmentPath = class(Exception);
  /// <summary>Exception raised when shell operations fail.</summary>
  EShellError = class(Exception);

  /// <summary>
  /// Generates a random password with specified complexity requirements.
  /// </summary>
  /// <param name="PasswordLength">Desired length of the password.</param>
  /// <param name="Complexity">Set of complexity requirements (numbers, signs).</param>
  /// <returns>Randomly generated password string.</returns>
  function RandomPassword(const PasswordLength : Integer; Complexity : TPasswordComplexity = [pfIncludeNumbers,pfIncludeSigns]) : string;
  
  /// <summary>
  /// Generates a random alphanumeric string.
  /// </summary>
  /// <param name="aLength">Length of the string to generate.</param>
  /// <returns>Random string containing letters and numbers.</returns>
  function RandomString(const aLength: Integer) : string;
  
  /// <summary>
  /// Extracts filename without extension from a full path.
  /// </summary>
  /// <param name="FileName">Full path or filename.</param>
  /// <returns>Filename without extension.</returns>
  function ExtractFileNameWithoutExt(const FileName: string): string;
  
  /// <summary>
  /// Converts Unix-style path (/) to Windows-style path (\).
  /// </summary>
  /// <param name="UnixPath">Path with Unix separators.</param>
  /// <returns>Path with Windows separators.</returns>
  function UnixToWindowsPath(const UnixPath: string): string;
  
  /// <summary>
  /// Converts Windows-style path (\) to Unix-style path (/).
  /// </summary>
  /// <param name="WindowsPath">Path with Windows separators.</param>
  /// <returns>Path with Unix separators.</returns>
  function WindowsToUnixPath(const WindowsPath: string): string;
  
  /// <summary>
  /// Corrects malformed URLs by fixing slashes and encoding spaces.
  /// </summary>
  /// <param name="cUrl">URL to correct.</param>
  /// <returns>Corrected URL.</returns>
  function CorrectURLPath(const cUrl : string) : string;
  
  /// <summary>Extracts the protocol from a URL (e.g., 'http', 'https').</summary>
  function UrlGetProtocol(const aUrl : string) : string;
  
  /// <summary>Extracts the host from a URL.</summary>
  function UrlGetHost(const aUrl : string) : string;
  
  /// <summary>Extracts the path from a URL.</summary>
  function UrlGetPath(const aUrl : string) : string;
  
  /// <summary>Extracts the query string from a URL.</summary>
  function UrlGetQuery(const aUrl : string) : string;
  
  /// <summary>Removes the protocol part from a URL.</summary>
  function UrlRemoveProtocol(const aUrl : string) : string;
  
  /// <summary>Removes the query string part from a URL.</summary>
  function UrlRemoveQuery(const aUrl : string) : string;
  
  /// <summary>Performs simple URL encoding (spaces to %20).</summary>
  function UrlSimpleEncode(const aUrl : string) : string;
  
  /// <summary>
  /// Populates the global 'path' variable with common environment paths.
  /// </summary>
  /// <remarks>
  /// Automatically called during unit initialization. Fills paths like TEMP, DESKTOP, etc.
  /// </remarks>
  procedure GetEnvironmentPaths;
  
  {$IFDEF MSWINDOWS}
  /// <summary>
  /// Gets a special folder path by its CSIDL identifier.
  /// </summary>
  /// <param name="folderID">CSIDL folder identifier (e.g., CSIDL_DESKTOP).</param>
  /// <returns>Full path to the special folder.</returns>
  function GetSpecialFolderPath(folderID : Integer) : string;
  
  /// <summary>Checks if the operating system is 64-bit.</summary>
  /// <returns>True if running on 64-bit OS.</returns>
  function Is64bitOS : Boolean;
  
  /// <summary>Checks if the application is compiled as a console application.</summary>
  /// <returns>True if console application.</returns>
  function IsConsole : Boolean;
  
  /// <summary>Checks if the application has console output available.</summary>
  /// <returns>True if console output is available.</returns>
  function HasConsoleOutput : Boolean;
  {$ENDIF}
  
  /// <summary>Checks if the application is compiled in debug mode.</summary>
  /// <returns>True if compiled with DEBUG directive.</returns>
  function IsDebug : Boolean;
  
  {$IFDEF MSWINDOWS}
  /// <summary>Checks if the application is running as a Windows service.</summary>
  /// <returns>True if running as a service.</returns>
  function IsService : Boolean;
  
  /// <summary>
  /// Gets the number of seconds since last user input (keyboard or mouse).
  /// </summary>
  /// <returns>Seconds of idle time.</returns>
  function SecondsIdle: DWord;
  
  /// <summary>
  /// Frees unused process memory to reduce memory footprint.
  /// </summary>
  procedure FreeUnusedMem;
  
  /// <summary>
  /// Changes the screen resolution.
  /// </summary>
  /// <param name="Width">Desired screen width in pixels.</param>
  /// <param name="Height">Desired screen height in pixels.</param>
  /// <returns>Result code from ChangeDisplaySettings.</returns>
  function SetScreenResolution(Width, Height: integer): Longint;
  {$ENDIF MSWINDOWS}
  
  /// <summary>Returns the last day of the current month.</summary>
  /// <returns>TDateTime representing the last day of current month.</returns>
  function LastDayCurrentMonth: TDateTime;
  
  {$IFDEF FPC}
  /// <summary>
  /// Checks if a datetime is within a specified range.
  /// </summary>
  /// <param name="aInclusive">If true, range endpoints are included.</param>
  function DateTimeInRange(ADateTime: TDateTime; AStartDateTime, AEndDateTime: TDateTime; aInclusive: Boolean = True): Boolean;
  {$ENDIF}
  
  /// <summary>
  /// Checks if two datetimes are on the same day.
  /// </summary>
  /// <param name="cBefore">First datetime to compare.</param>
  /// <param name="cNow">Second datetime to compare.</param>
  /// <returns>True if both datetimes are on the same day.</returns>
  function IsSameDay(cBefore, cNow : TDateTime) : Boolean;
  
  /// <summary>
  /// Changes the time portion of a datetime value.
  /// </summary>
  /// <param name="aDate">Original datetime.</param>
  /// <param name="aHour">New hour value.</param>
  /// <param name="aMinute">New minute value.</param>
  /// <param name="aSecond">New second value.</param>
  /// <param name="aMilliSecond">New millisecond value (default 0).</param>
  /// <returns>DateTime with modified time.</returns>
  function ChangeTimeOfADay(aDate : TDateTime; aHour, aMinute, aSecond : Word; aMilliSecond : Word = 0) : TDateTime;
  
  /// <summary>
  /// Changes the date portion of a datetime value.
  /// </summary>
  /// <param name="aDate">Original datetime.</param>
  /// <param name="aYear">New year value.</param>
  /// <param name="aMonth">New month value.</param>
  /// <param name="aDay">New day value.</param>
  /// <returns>DateTime with modified date.</returns>
  function ChangeDateOfADay(aDate : TDateTime; aYear, aMonth, aDay : Word) : TDateTime;
  
  /// <summary>
  /// Creates a string by repeating a character.
  /// </summary>
  /// <param name="C">Character to repeat.</param>
  /// <param name="Count">Number of repetitions.</param>
  /// <returns>String containing the repeated character.</returns>
  function FillStr(const C : Char; const Count : Integer) : string;
  
  /// <summary>
  /// Creates a string by repeating a string value.
  /// </summary>
  /// <param name="value">String to repeat.</param>
  /// <param name="Count">Number of repetitions.</param>
  /// <returns>String containing the repeated value.</returns>
  function FillStrEx(const value : string; const Count : Integer) : string;
  
  /// <summary>
  /// Checks if a string exists in an array of strings.
  /// </summary>
  /// <param name="aValue">Value to search for.</param>
  /// <param name="aInArray">Array to search in.</param>
  /// <param name="aCaseSensitive">Whether comparison is case-sensitive (default True).</param>
  /// <returns>True if value is found in array.</returns>
  function StrInArray(const aValue : string; const aInArray : array of string; aCaseSensitive : Boolean = True) : Boolean;
  
  /// <summary>
  /// Checks if an integer exists in an array of integers.
  /// </summary>
  /// <param name="aValue">Value to search for.</param>
  /// <param name="aInArray">Array to search in.</param>
  /// <returns>True if value is found in array.</returns>
  function IntInArray(const aValue : Integer; const aInArray : array of Integer) : Boolean;
  
  /// <summary>Checks if a string array is empty.</summary>
  function IsEmptyArray(aArray : TArray<string>) : Boolean; overload;
  
  /// <summary>Checks if an integer array is empty.</summary>
  function IsEmptyArray(aArray : TArray<Integer>) : Boolean; overload;
  
  /// <summary>
  /// Returns a number as a string with leading zeros.
  /// </summary>
  /// <param name="Number">Number to format.</param>
  /// <param name="Len">Desired minimum length.</param>
  /// <returns>String with leading zeros.</returns>
  function Zeroes(const Number, Len : Int64) : string;
  
  /// <summary>
  /// Converts a number to a string with thousand delimiters.
  /// </summary>
  /// <param name="Number">Number to format.</param>
  /// <returns>Formatted number string (e.g., "1,000,000").</returns>
  function NumberToStr(const Number : Int64) : string;
  
  /// <summary>
  /// Creates a string of spaces.
  /// </summary>
  /// <param name="Count">Number of spaces.</param>
  /// <returns>String containing spaces.</returns>
  function Spaces(const Count : Integer) : string;
  
  /// <summary>Returns the current date and time as a string.</summary>
  function NowStr : string;
  
  /// <summary>
  /// Generates a new GUID and returns it as a string.
  /// </summary>
  /// <returns>GUID string in format {XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}.</returns>
  function NewGuidStr : string;
  
  /// <summary>
  /// Compares a string with a wildcard pattern.
  /// </summary>
  /// <param name="cText">Text to match.</param>
  /// <param name="Pattern">Pattern with wildcards (* for multiple chars, ? for single char).</param>
  /// <returns>True if text matches pattern.</returns>
  function IsLike(cText, Pattern: string) : Boolean;
  
  /// <summary>
  /// Capitalizes the first letter of a string.
  /// </summary>
  /// <param name="s">String to capitalize.</param>
  /// <returns>String with first letter uppercase.</returns>
  function Capitalize(s: string): string;
  
  /// <summary>
  /// Capitalizes the first letter of each word in a string.
  /// </summary>
  /// <param name="s">String to capitalize.</param>
  /// <returns>String with each word capitalized.</returns>
  function CapitalizeWords(s: string): string;
  
  /// <summary>Gets the currently logged-in username.</summary>
  /// <returns>Username string.</returns>
  function GetLoggedUserName : string;
  
  /// <summary>Gets the computer name.</summary>
  /// <returns>Computer name string.</returns>
  function GetComputerName : string;
  
  {$IFDEF MSWINDOWS}
  /// <summary>Checks if the session is a remote desktop connection.</summary>
  /// <returns>True if remote desktop session.</returns>
  function IsRemoteSession : Boolean;
  {$ENDIF}
  
  /// <summary>
  /// Extracts domain and username from a login string.
  /// </summary>
  /// <param name="aUser">User string (formats: DOMAIN\User or User@domain.com).</param>
  /// <param name="oDomain">Output parameter for domain.</param>
  /// <param name="oUser">Output parameter for username.</param>
  /// <returns>True if domain was extracted, False otherwise.</returns>
  function ExtractDomainAndUser(const aUser : string; out oDomain, oUser : string) : Boolean;
  
  /// <summary>
  /// Normalizes path delimiters to a specific character.
  /// </summary>
  /// <param name="cPath">Path to normalize.</param>
  /// <param name="Delim">Desired delimiter character (\ or /).</param>
  /// <returns>Path with normalized delimiters.</returns>
  function NormalizePathDelim(const cPath : string; const Delim : Char) : string;
  
  /// <summary>
  /// Combines two paths with a specified delimiter.
  /// </summary>
  /// <param name="aFirstPath">First path segment.</param>
  /// <param name="aSecondPath">Second path segment.</param>
  /// <param name="aDelim">Path delimiter to use.</param>
  /// <returns>Combined path.</returns>
  function CombinePaths(const aFirstPath, aSecondPath: string; aDelim : Char): string;
  
  /// <summary>
  /// Removes the first segment from a path.
  /// </summary>
  /// <param name="cdir">Path to process.</param>
  /// <returns>Path without first segment.</returns>
  function RemoveFirstPathSegment(const cdir : string) : string;
  
  /// <summary>
  /// Removes the last segment from a path.
  /// </summary>
  /// <param name="cDir">Path to process.</param>
  /// <returns>Path without last segment.</returns>
  function RemoveLastPathSegment(const cDir : string) : string;
  
  /// <summary>
  /// Detects and returns the path delimiter used in a path.
  /// </summary>
  /// <param name="aPath">Path to analyze.</param>
  /// <returns>Path delimiter (\ or /) or empty string if none found.</returns>
  function GetPathDelimiter(const aPath : string) : string;
  
  /// <summary>
  /// Extracts the first segment from a path.
  /// </summary>
  /// <param name="aPath">Path to process.</param>
  /// <returns>First path segment.</returns>
  function GetFirstPathSegment(const aPath : string) : string;
  
  /// <summary>
  /// Extracts the last segment from a path.
  /// </summary>
  /// <param name="aPath">Path to process.</param>
  /// <returns>Last path segment.</returns>
  function GetLastPathSegment(const aPath : string) : string;
  
  /// <summary>
  /// Checks if a command-line switch exists.
  /// </summary>
  /// <param name="Switch">Switch name to find (without - or /).</param>
  /// <returns>True if switch was found in command line parameters.</returns>
  function ParamFindSwitch(const Switch : string) : Boolean;
  
  /// <summary>
  /// Gets the value of a command-line switch.
  /// </summary>
  /// <param name="Switch">Switch name (without - or /).</param>
  /// <param name="cvalue">Output parameter for switch value.</param>
  /// <returns>True if switch was found and value retrieved.</returns>
  function ParamGetSwitch(const Switch : string; var cvalue : string) : Boolean;
  
  /// <summary>
  /// Gets the application name based on the executable filename.
  /// </summary>
  /// <returns>Application name without extension.</returns>
  function GetAppName : string;
  
  /// <summary>
  /// Gets the application version string (major.minor).
  /// </summary>  
  /// <returns>Version string (e.g., "1.0").</returns>
  function GetAppVersionStr: string;
  
  /// <summary>
  /// Gets the full application version string (major.minor.release.build).
  /// </summary>
  /// <returns>Full version string (e.g., "1.0.0.0").</returns>
  function GetAppVersionFullStr: string;
  
  /// <summary>
  /// Converts UTC datetime to local datetime.
  /// </summary>
  /// <param name="GMTTime">UTC datetime value.</param>
  /// <returns>Local datetime value.</returns>
  function UTCToLocalTime(GMTTime: TDateTime): TDateTime;
  
  /// <summary>
  /// Converts local datetime to UTC datetime.
  /// </summary>
  /// <param name="LocalTime">Local datetime value.</param>
  /// <returns>UTC datetime value.</returns>
  function LocalTimeToUTC(LocalTime : TDateTime): TDateTime;
  
  /// <summary>
  /// Converts datetime to GMT format string.
  /// </summary>
  /// <param name="aDate">DateTime to convert.</param>
  /// <returns>GMT formatted string.</returns>
  function DateTimeToGMT(aDate : TDateTime) : string;
  
  /// <summary>
  /// Converts GMT format string to datetime.
  /// </summary>
  /// <param name="aDate">GMT formatted string.</param>
  /// <returns>TDateTime value.</returns>
  function GMTToDateTime(aDate : string) : TDateTime;
  
  /// <summary>
  /// Converts datetime to JSON/ISO 8601 date format.
  /// </summary>
  /// <param name="aDateTime">DateTime to convert.</param>
  /// <returns>ISO 8601 formatted string.</returns>
  function DateTimeToJsonDate(aDateTime : TDateTime) : string;
  
  /// <summary>
  /// Converts JSON/ISO 8601 date format to datetime.
  /// </summary>
  /// <param name="aJsonDate">ISO 8601 formatted string.</param>
  /// <returns>TDateTime value.</returns>
  function JsonDateToDateTime(const aJsonDate : string) : TDateTime;
  
  /// <summary>
  /// Counts the number of digits in an integer.
  /// </summary>
  /// <param name="anInt">Integer to analyze.</param>
  /// <returns>Number of digits.</returns>
  function CountDigits(anInt: Cardinal): Cardinal; inline;
  
  /// <summary>
  /// Counts occurrences of a substring in a string.
  /// </summary>
  /// <param name="aFindStr">Substring to find.</param>
  /// <param name="aSourceStr">String to search in.</param>
  /// <returns>Number of occurrences.</returns>
  function CountStr(const aFindStr, aSourceStr : string) : Integer;
  
  /// <summary>
  /// Saves a stream to a file.
  /// </summary>
  /// <param name="aStream">Stream to save.</param>
  /// <param name="aFilename">Target filename.</param>
  procedure SaveStreamToFile(aStream : TStream; const aFilename : string);
  
  /// <summary>
  /// Converts a stream to a string using specified encoding.
  /// </summary>
  /// <param name="aStream">Stream to convert.</param>
  /// <param name="aEncoding">Text encoding to use.</param>
  /// <returns>String content.</returns>
  function StreamToString(const aStream: TStream; const aEncoding: TEncoding): string;
  
  /// <summary>
  /// Converts a stream to a string (auto-detecting type).
  /// </summary>
  /// <param name="aStream">Stream to convert.</param>
  /// <returns>String content.</returns>
  function StreamToStringEx(aStream : TStream) : string;
  
  /// <summary>
  /// Writes a string to a stream using specified encoding.
  /// </summary>
  /// <param name="aStr">String to write.</param>
  /// <param name="aStream">Target stream.</param>
  /// <param name="aEncoding">Text encoding to use.</param>
  procedure StringToStream(const aStr : string; aStream : TStream; const aEncoding: TEncoding);
  
  /// <summary>
  /// Writes a string to a stream directly.
  /// </summary>
  /// <param name="aStr">String to write.</param>
  /// <param name="aStream">Target stream.</param>
  procedure StringToStreamEx(const aStr : string; aStream : TStream);
  
  /// <summary>
  /// Converts a TStringList to a comma-separated string.
  /// </summary>
  /// <param name="aList">StringList to convert.</param>
  /// <returns>Comma-separated string.</returns>
  function CommaText(aList : TStringList) : string; overload;
  
  /// <summary>
  /// Converts an array of strings to a comma-separated string.
  /// </summary>
  /// <param name="aArray">Array to convert.</param>
  /// <returns>Comma-separated string.</returns>
  function CommaText(aArray : TArray<string>) : string; overload;
  
  /// <summary>
  /// Converts an array of strings to a CRLF-separated string.
  /// </summary>
  /// <param name="aArray">Array to convert.</param>
  /// <returns>CRLF-separated string.</returns>
  function ArrayToString(aArray : TArray<string>) : string; overload;
  
  /// <summary>
  /// Converts an array of strings to a string with custom separator.
  /// </summary>
  /// <param name="aArray">Array to convert.</param>
  /// <param name="aSeparator">Separator string.</param>
  /// <returns>Separated string.</returns>
  function ArrayToString(aArray : TArray<string>; aSeparator : string) : string; overload;
  
  /// <summary>
  /// Converts an array of integers to a CRLF-separated string.
  /// </summary>
  /// <param name="aArray">Array to convert.</param>
  /// <returns>CRLF-separated string.</returns>
  function ArrayToString(aArray : TArray<Integer>) : string; overload;
  
  /// <summary>
  /// Converts an array of integers to a string with custom separator.
  /// </summary>
  /// <param name="aArray">Array to convert.</param>
  /// <param name="aSeparator">Separator string.</param>
  /// <returns>Separated string.</returns>
  function ArrayToString(aArray : TArray<Integer>; aSeparator : string) : string; overload;
  
  /// <summary>
  /// Converts a TStrings to an array of strings.
  /// </summary>
  /// <param name="aStrings">TStrings to convert.</param>
  /// <returns>Array of strings.</returns>
  function StringsToArray(aStrings : TStrings) : TArray<string>; overload;
  
  /// <summary>
  /// Splits a comma or semicolon-separated string into an array.
  /// </summary>
  /// <param name="aString">String to split.</param>
  /// <returns>Array of strings.</returns>
  function StringsToArray(const aString : string) : TArray<string>; overload;
  {$IFDEF MSWINDOWS}
  /// <summary>
  /// Processes pending Windows messages in console applications.
  /// </summary>
  /// <remarks>
  /// Allows console applications to process Windows messages, similar to Application.ProcessMessages.
  /// </remarks>
  procedure ProcessMessages;
  
  /// <summary>
  /// Gets the last Windows error message.
  /// </summary>
  /// <returns>Formatted error message string.</returns>
  function GetLastOSError : String;
  {$ENDIF}
  
  {$IF DEFINED(FPC) AND DEFINED(MSWINDOWS)}
  function GetLastInputInfo(var plii: TLastInputInfo): BOOL;stdcall; external 'user32' name 'GetLastInputInfo';
  {$ENDIF}
  
  /// <summary>
  /// Removes the last character from a string.
  /// </summary>
  /// <param name="aText">String to process.</param>
  /// <returns>String without last character.</returns>
  function RemoveLastChar(const aText : string) : string;
  
  /// <summary>
  /// Converts a datetime to SQL format string (YYYY-MM-DD hh:mm:ss).
  /// </summary>
  /// <param name="aDateTime">DateTime to convert.</param>
  /// <returns>SQL formatted datetime string.</returns>
  function DateTimeToSQL(aDateTime : TDateTime) : string;
  
  /// <summary>
  /// Checks if a string represents a valid integer.
  /// </summary>
  /// <param name="aValue">String to check.</param>
  /// <returns>True if string is a valid integer.</returns>
  function IsInteger(const aValue : string) : Boolean;
  
  /// <summary>
  /// Checks if a string represents a valid floating-point number.
  /// </summary>
  /// <param name="aValue">String to check.</param>
  /// <returns>True if string is a valid float.</returns>
  function IsFloat(const aValue : string) : Boolean;
  
  /// <summary>
  /// Checks if a string represents a valid boolean value.
  /// </summary>
  /// <param name="aValue">String to check.</param>
  /// <returns>True if string is a valid boolean.</returns>
  function IsBoolean(const aValue : string) : Boolean;
  
  /// <summary>
  /// Extracts a substring from a string and removes it from the source.
  /// </summary>
  /// <param name="vSource">Source string (modified by this function).</param>
  /// <param name="aIndex">Starting index (1-based).</param>
  /// <param name="aCount">Number of characters to extract.</param>
  /// <returns>Extracted substring.</returns>
  function ExtractStr(var vSource : string; aIndex : Integer; aCount : Integer) : string;
  
  /// <summary>
  /// Extracts a substring between two delimiters.
  /// </summary>
  /// <param name="aSource">Source string.</param>
  /// <param name="aFirstDelimiter">Starting delimiter.</param>
  /// <param name="aLastDelimiter">Ending delimiter.</param>
  /// <returns>Substring between delimiters.</returns>
  function GetSubString(const aSource, aFirstDelimiter, aLastDelimiter : string) : string;
  
  /// <summary>
  /// Adds double quotes around a string, escaping existing quotes.
  /// </summary>
  /// <param name="str">String to quote.</param>
  /// <returns>Double-quoted string.</returns>
  function DbQuotedStr(const str : string): string;
  
  /// <summary>
  /// Removes double quotes from a quoted string.
  /// </summary>
  /// <param name="str">Quoted string.</param>
  /// <returns>Unquoted string.</returns>
  function UnDbQuotedStr(const str: string) : string;
  
  /// <summary>
  /// Adds single quotes around a string.
  /// </summary>
  /// <param name="str">String to quote.</param>
  /// <returns>Single-quoted string.</returns>
  function SpQuotedStr(const str : string): string;
  
  /// <summary>
  /// Removes single quotes from a quoted string.
  /// </summary>
  /// <param name="str">Quoted string.</param>
  /// <returns>Unquoted string.</returns>
  function UnSpQuotedStr(const str : string): string;
  
  /// <summary>
  /// Removes specified quote characters from a string.
  /// </summary>
  /// <param name="str">Quoted string.</param>
  /// <param name="aQuote">Quote character to remove.</param>
  /// <returns>Unquoted string.</returns>
  function UnQuotedStr(const str : string; const aQuote : Char) : string;
  
  /// <summary>
  /// Ternary operator for strings (if-then-else expression).
  /// </summary>
  /// <param name="aCondition">Condition to evaluate.</param>
  /// <param name="aIfIsTrue">Value if condition is true.</param>
  /// <param name="aIfIsFalse">Value if condition is false.</param>
  /// <returns>Selected value based on condition.</returns>
  function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : string) : string; overload;
  
  /// <summary>
  /// Ternary operator for integers (if-then-else expression).
  /// </summary>
  function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : Integer) : Integer; overload;
  
  /// <summary>
  /// Ternary operator for extended (float) values (if-then-else expression).
  /// </summary>
  function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : Extended) : Extended; overload;
  
  /// <summary>
  /// Ternary operator for objects (if-then-else expression).
  /// </summary>
  function Ifx(aCondition : Boolean; const aIfIsTrue, aIfIsFalse : TObject) : TObject; overload;

var
  /// <summary>
  /// Global variable containing commonly used environment and system paths.
  /// </summary>
  /// <remarks>
  /// Automatically populated during unit initialization by calling GetEnvironmentPaths.
  /// </remarks>
  path : TEnvironmentPath;
  
  /// <summary>
  /// Indicates if the application is running as a Quick.Service application.
  /// </summary>
  /// <remarks>
  /// Set to True when using the QuickService framework.
  /// </remarks>
  IsQuickServiceApp : Boolean;

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
  if pfIncludeSigns in Complexity then
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

function CorrectURLPath(const cUrl : string) : string;
var
  nurl : string;
begin
  nurl := WindowsToUnixPath(cUrl);
  nurl := StringReplace(nurl,'//','/',[rfReplaceAll]);
  Result := StringReplace(nurl,' ','%20',[rfReplaceAll]);
  //TNetEncoding.Url.Encode()
end;

function UrlGetProtocol(const aUrl : string) : string;
begin
  Result := aUrl.SubString(0,aUrl.IndexOf('://'));
end;

function UrlGetHost(const aUrl : string) : string;
var
  url : string;
  len : Integer;
begin
  url := UrlRemoveProtocol(aUrl);

  if url.Contains('/') then len := url.IndexOf('/')
    else len := url.Length;

  Result := url.SubString(0,len);
end;

function UrlGetPath(const aUrl : string) : string;
var
  url : string;
  len : Integer;
begin
  url := UrlRemoveProtocol(aUrl);
  if not url.Contains('/') then Exit('');
  len := url.IndexOf('?');
  if len < 0 then len := url.Length
    else len := url.IndexOf('?') - url.IndexOf('/');
  Result := url.Substring(url.IndexOf('/'),len);
end;

function UrlGetQuery(const aUrl : string) : string;
begin
  if not aUrl.Contains('?') then Exit('');

  Result := aUrl.Substring(aUrl.IndexOf('?')+1);
end;

function UrlRemoveProtocol(const aUrl : string) : string;
var
  pos : Integer;
begin
  pos := aUrl.IndexOf('://');
  if pos < 0 then pos := 0
    else pos := pos + 3;
  Result := aUrl.SubString(pos, aUrl.Length);
end;

function UrlRemoveQuery(const aUrl : string) : string;
begin
  if not aUrl.Contains('?') then Exit(aUrl);
  Result := aUrl.Substring(0,aUrl.IndexOf('?'));
end;

function UrlSimpleEncode(const aUrl : string) : string;
begin
  Result := StringReplace(aUrl,' ','%20',[rfReplaceAll]);
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
  shellMalloc: IMalloc;
  ppidl: PItemIdList;
begin
  ppidl := nil;
  try
    if SHGetMalloc(shellMalloc) = NOERROR then
    begin
      SHGetSpecialFolderLocation(0, folderID, ppidl);
      SetLength(Result, MAX_PATH);
      if not SHGetPathFromIDList(ppidl,{$IFDEF FPC}PAnsiChar(Result){$ELSE}PChar(Result){$ENDIF}) then
      begin
        raise EShellError.create(Format('GetSpecialFolderPath: Invalid PIPL (%d)',[folderID]));
      end;
      SetLength(Result, lStrLen({$IFDEF FPC}PAnsiChar(Result){$ELSE}PChar(Result){$ENDIF}));
    end;
  finally
    if ppidl <> nil then
      shellMalloc.Free(ppidl);
  end;
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
      {$WARN SYMBOL_PLATFORM OFF}
      //Allready checked that we are on a windows platform
        Win32Check(stout <> Invalid_Handle_Value);
      {$WARN SYMBOL_PLATFORM ON}
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

function FillStrEx(const value : string; const Count : Integer) : string;
var
  i   : Integer;
begin
  Result := '';
  for i := 1 to Count do Result := Result + value;
end;

function StrInArray(const aValue : string; const aInArray : array of string; aCaseSensitive : Boolean = True) : Boolean;
var
  s : string;
begin
  for s in aInArray do
  begin
    if aCaseSensitive then
    begin
      if s = aValue then Exit(True);
    end
    else
    begin
      if CompareText(aValue,s) = 0 then Exit(True);
    end;
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
{$IFNDEF DELPHIRX10_UP}
var
  guid : TGUID;
{$ENDIF}
begin
  {$IFDEF DELPHIRX10_UP}
  Result := TGUID.NewGuid.ToString;
  {$ELSE}
  guid.NewGuid;
  Result := guid.ToString
  {$ENDIF}
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
  var
    pw : PPasswordRecord;
  begin
    // Prefer environment variables (fast path) then fall back to /etc/passwd
    // via fpgetpwuid(), which works even in containers with no env vars set.
    Result := GetEnvironmentVariable('USER');
    if Result.IsEmpty then Result := GetEnvironmentVariable('LOGNAME');
    if Result.IsEmpty then Result := GetEnvironmentVariable('USERNAME');
    if Result.IsEmpty then
    begin
      pw := fpgetpwuid(fpgetuid);
      if pw <> nil then Result := string(pw^.pw_name);
    end;
  end;
  {$ELSE}
  var
    {$IFNDEF NEXTGEN}
    plogin : PAnsiChar;
    {$ELSE}
    plogin : MarshaledAString;
    {$ENDIF}
    {$IFDEF POSIX}
    pw : PPasswd;
    {$ENDIF}
  begin
    {$IFDEF POSIX}
    try
      plogin := getlogin;
      {$IFDEF NEXTGEN}
      Result := string(plogin);
      {$ELSE}
      Result := Copy(plogin,1,Length(Trim(plogin)));
      {$ENDIF}
      // getlogin returns empty in containers/CI (no controlling terminal).
      // Fall back to env vars, then to getpwuid() which reads /etc/passwd.
      if Result.IsEmpty then Result := GetEnvironmentVariable('USER');
      if Result.IsEmpty then Result := GetEnvironmentVariable('LOGNAME');
      if Result.IsEmpty then Result := GetEnvironmentVariable('USERNAME');
      if Result.IsEmpty then
      begin
        pw := getpwuid(getuid);
        if pw <> nil then Result := string(pw^.pw_name);
      end;
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
          phost := AllocMem(256);
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
          finally
            FreeMem(phost);
          end;
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

{$IFDEF MSWINDOWS}
function IsRemoteSession : Boolean;
const
  SM_REMOTECONTROL      = $2001;
  SM_REMOTESESSION      = $1000;
begin
  Result := (GetSystemMetrics(SM_REMOTESESSION) <> 0) or (GetSystemMetrics(SM_REMOTECONTROL) <> 0);
end;
{$ENDIF}

function ExtractDomainAndUser(const aUser : string; out oDomain, oUser : string) : Boolean;
begin
  //check if domain specified into username
  if aUser.Contains('\') then
  begin
    oDomain := Copy(aUser,Low(aUser),Pos('\',aUser)-1);
    oUser := Copy(aUser,Pos('\',aUser)+1,aUser.Length);
    Exit(True);
  end
  else if aUser.Contains('@') then
  begin
    oDomain := Copy(aUser,Pos('@',aUser)+1,aUser.Length);
    oUser := Copy(aUser,Low(aUser),Pos('@',aUser)-1);
    Exit(True);
  end;
  oDomain := '';
  oUser := aUser;
  Result := False;
end;

function NormalizePathDelim(const cPath : string; const Delim : Char) : string;
begin
  if Delim = '\' then Result := StringReplace(cPath,'/',Delim,[rfReplaceAll])
    else Result := StringReplace(cPath,'\',Delim,[rfReplaceAll]);
end;

function CombinePaths(const aFirstPath, aSecondPath: string; aDelim : Char): string;
var
  path1 : string;
  path2 : string;
begin
  path1 := NormalizePathDelim(aFirstPath,aDelim);
  path2 := NormalizePathDelim(aSecondPath,aDelim);
  if path1.EndsWith(aDelim) then
  begin
    if path2.StartsWith(aDelim) then Result := path1 + path2.Substring(1)
      else Result := path1 + path2;
  end
  else
  begin
     if path2.StartsWith(aDelim) then Result := path1 + path2
      else result := path1 + aDelim + path2;
  end;
end;

function RemoveFirstPathSegment(const cdir : string) : string;
var
  posi : Integer;
  delim : Char;
  dir : string;
  StartsWithDelim : Boolean;
begin
  if cDir.Contains('\') then delim := '\'
    else if cDir.Contains('/') then delim := '/'
      else
      begin
        Exit('');
      end;

  dir := NormalizePathDelim(cDir,delim);
  if dir.StartsWith(delim) then
  begin
    dir := Copy(dir,2,dir.Length);
    StartsWithDelim := True;
  end
  else StartsWithDelim := False;

  if dir.CountChar(delim) = 0 then Exit('')
    else posi := Pos(delim,dir)+1;
  Result := Copy(dir,posi,dir.Length);
  if (not Result.IsEmpty) and (StartsWithDelim) then Result := delim + Result;
end;

function RemoveLastPathSegment(const cDir : string) : string;
var
  posi : Integer;
  delim : Char;
  dir : string;
  EndsWithDelim : Boolean;
begin
  if cDir.Contains('\') then delim := '\'
    else if cDir.Contains('/') then delim := '/'
      else
      begin
        Exit('');
      end;
  dir := NormalizePathDelim(cDir,delim);

  if dir.EndsWith(delim) then
  begin
    dir := Copy(dir,1,dir.Length-1);
    EndsWithDelim := True;
  end
  else EndsWithDelim := False;

  if dir.CountChar(delim) > 1 then posi := dir.LastDelimiter(delim)
    else posi := Pos(delim,dir)-1;
  if posi = dir.Length then posi := 0;
  Result := Copy(dir,1,posi);
  if (not Result.IsEmpty) and (EndsWithDelim) then Result := Result + delim;
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
  if spath.Contains(delimiter) then Result := Copy(spath,0,spath.IndexOf(delimiter))
    else Result := spath;
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
        {$IFDEF DELPHIRX103_UP}
        PkgInfo := TAndroidHelper.Activity.getPackageManager.getPackageInfo(TAndroidHelper.Activity.getPackageName,0);
        {$ELSE}
        PkgInfo := SharedActivity.getPackageManager.getPackageInfo(SharedActivity.getPackageName,0);
        {$ENDIF}
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
        {$IFDEF DELPHIRX103_UP}
        PkgInfo := TAndroidHelper.Activity.getPackageManager.getPackageInfo(TAndroidHelper.Activity.getPackageName,0);
        {$ELSE}
        PkgInfo := SharedActivity.getPackageManager.getPackageInfo(SharedActivity.getPackageName,0);
        {$ENDIF}
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

function StreamToString(const aStream: TStream; const aEncoding: TEncoding): string;
var
  sbytes: TBytes;
begin
  aStream.Position := 0;
  SetLength(sbytes, aStream.Size);
  aStream.ReadBuffer(sbytes,aStream.Size);
  Result := aEncoding.GetString(sbytes);
end;

function StreamToStringEx(aStream : TStream) : string;
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

procedure StringToStream(const aStr : string; aStream : TStream; const aEncoding: TEncoding);
var
  stream : TStringStream;
begin
  stream := TStringStream.Create(aStr,aEncoding);
  try
    aStream.CopyFrom(stream,stream.Size);
  finally
    stream.Free;
  end;
end;

procedure StringToStreamEx(const aStr : string; aStream : TStream);
begin
  aStream.Seek(0,soBeginning);
  aStream.WriteBuffer(Pointer(aStr)^,aStr.Length * SizeOf(Char));
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
  Result := '';
  if High(aArray) < 0 then Exit;
  sb := TStringBuilder.Create;
  try
    for value in aArray do
    begin
      sb.Append(value);
      sb.Append(CRLF);
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function ArrayToString(aArray : TArray<string>; aSeparator : string) : string;
var
  value : string;
  sb : TStringBuilder;
  isfirst : Boolean;
begin
  Result := '';
  if High(aArray) < 0 then Exit;
  isfirst := True;
  sb := TStringBuilder.Create;
  try
    for value in aArray do
    begin
      if isfirst then isfirst := False
        else sb.Append(aSeparator);
      sb.Append(value);
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function ArrayToString(aArray : TArray<Integer>) : string;
var
  value : Integer;
  sb : TStringBuilder;
begin
  Result := '';
  if High(aArray) < 0 then Exit;
  sb := TStringBuilder.Create;
  try
    for value in aArray do
    begin
      sb.Append(value);
      sb.Append(CRLF);
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function ArrayToString(aArray : TArray<Integer>; aSeparator : string) : string;
var
  value : Integer;
  sb : TStringBuilder;
  isfirst : Boolean;
begin
  Result := '';
  if High(aArray) < 0 then Exit;
  isfirst := True;
  sb := TStringBuilder.Create;
  try
    for value in aArray do
    begin
      if isfirst then isfirst := False
        else sb.Append(aSeparator);
      sb.Append(value);
    end;
    Result := sb.ToString;
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

function StringsToArray(const aString : string) : TArray<string>;
var
  item : string;
begin
  for item in aString.Split([';',',']) do Result := Result + [item.Trim];
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
  Result := FormatDateTime('YYYY-MM-DD hh:mm:ss',aDateTime);
end;

function IsInteger(const aValue : string) : Boolean;
var
  i : Integer;
begin
  Result := TryStrToInt(aValue,i);
end;

function IsFloat(const aValue : string) : Boolean;
var
  e : Extended;
begin
  Result := TryStrToFloat(aValue,e);
end;

function IsBoolean(const aValue : string) : Boolean;
var
  b : Boolean;
begin
  Result := TryStrToBool(aValue,b);
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

function UnQuotedStr(const str : string; const aQuote : Char) : string;
begin
  if (str.Length > 0) and (str[Low(str)] = aQuote) and (str[High(str)] = aQuote) then Result := Copy(str, Low(str)+1, High(str) - 2)
    else Result := str;
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

{ TDateTimeHelper }

{$IFDEF DELPHIXE7_UP}
function TDateTimeHelper.ToSQLString : string;
begin
  Result := DateTimeToSQL(Self);
end;

procedure TDateTimeHelper.FromNow;
begin
  Self := Now;
end;

procedure TDateTimeHelper.FromUTC(const aUTCTime: TDateTime);
begin
  Self := UTCToLocalTime(aUTCTime);
end;

function TDateTimeHelper.IncDay(const aValue : Cardinal = 1) : TDateTime;
begin
  Result := System.DateUtils.IncDay(Self,aValue);
end;

function TDateTimeHelper.DecDay(const aValue : Cardinal = 1) : TDateTime;
begin
  Result := System.DateUtils.IncDay(Self,-aValue);
end;

function TDateTimeHelper.IncMonth(const aValue : Cardinal = 1) : TDateTime;
begin
  Result := SysUtils.IncMonth(Self,aValue);
end;

function TDateTimeHelper.DecMonth(const aValue : Cardinal = 1) : TDateTime;
begin
  Result := SysUtils.IncMonth(Self,-aValue);
end;

function TDateTimeHelper.IncYear(const aValue : Cardinal = 1) : TDateTime;
begin
  Result := System.DateUtils.IncYear(Self,aValue);
end;

function TDateTimeHelper.DecYear(const aValue : Cardinal = 1) : TDateTime;
begin
  Result := System.DateUtils.IncYear(Self,-aValue);
end;

function TDateTimeHelper.IsEqualTo(const aDateTime : TDateTime) : Boolean;
begin
  Result := Self = aDateTime;
end;

function TDateTimeHelper.IsAfter(const aDateTime : TDateTime) : Boolean;
begin
  Result := Self > aDateTime;
end;

function TDateTimeHelper.IsBefore(const aDateTime : TDateTime) : Boolean;
begin
  Result := Self < aDateTime;
end;

function TDateTimeHelper.IsSameDay(const aDateTime : TDateTime) : Boolean;
begin
  Result := System.DateUtils.SameDate(Self,aDateTime);
end;

function TDateTimeHelper.IsSameTime(const aTime : TTime) : Boolean;
begin
  Result := System.DateUtils.SameTime(Self,aTime);
end;

function TDateTimeHelper.DayOfTheWeek : Word;
begin
  Result := System.DateUtils.NthDayOfWeek(Self);
end;

function TDateTimeHelper.ToJsonFormat : string;
begin
  Result := DateTimeToJsonDate(Self);
end;

function TDateTimeHelper.ToGMTFormat : string;
begin
  Result := DateTimeToGMT(Self);
end;

function TDateTimeHelper.ToTimeStamp : TTimeStamp;
begin
  Result := DateTimeToTimeStamp(Self);
end;

function TDateTimeHelper.ToUTC : TDateTime;
begin
  Result := LocalTimeToUTC(Self);
end;

function TDateTimeHelper.ToMilliseconds : Int64;
begin
  {$IFDEF DELPHIRX104_ANDUP}
  Result := System.DateUtils.DateTimeToMilliseconds(Self);
  {$ELSE}
  Result := System.DateUtils.MilliSecondOf(Self);
  {$ENDIF}
end;

function TDateTimeHelper.ToString : string;
begin
  Result := DateTimeToStr(Self);
end;

function TDateTimeHelper.Date : TDate;
begin
  Result := System.DateUtils.DateOf(Self);
end;

function TDateTimeHelper.Time : TTime;
begin
  Result := System.DateUtils.TimeOf(Self);
end;

function TDateTimeHelper.IsAM : Boolean;
begin
  Result := System.DateUtils.IsAM(Self);
end;

function TDateTimeHelper.IsPM : Boolean;
begin
  Result := System.DateUtils.IsPM(Self);
end;

{ TDateHelper }
function TDateHelper.ToString : string;
begin
  Result := DateToStr(Self);
end;

{ TTimeHelper }
function TTimeHelper.ToString : string;
begin
  Result := TimeToStr(Self);
end;
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



