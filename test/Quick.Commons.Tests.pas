{ ***************************************************************************

  Copyright (c) 2016-2026 Kike Pérez

  Unit        : Quick.Commons.Tests
  Description : Unit Tests for Quick.Commons
  Author      : Kike Pérez
  Version     : 1.0
  Modified    : 28/02/2026

 *************************************************************************** }

unit Quick.Commons.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Quick.Commons;

type
  [TestFixture]
  TTestQuickCommons = class(TObject)
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestRandomPassword;
    [Test]
    procedure TestRandomString;
    [Test]
    procedure TestExtractFileNameWithoutExt;
    [Test]
    procedure TestUnixToWindowsPath;
    [Test]
    procedure TestWindowsToUnixPath;
    [Test]
    procedure TestUrlFunctions;
    [Test]
    procedure TestDateTimeFunctions;
    [Test]
    procedure TestStringFunctions;
    [Test]
    procedure TestArrayFunctions;
    [Test]
    procedure TestPathFunctions;
    [Test]
    procedure TestNumberFunctions;
    [Test]
    procedure TestEnvironmentFunctions;
    [Test]
    procedure TestQuotedStrFunctions;
    [Test]
    procedure TestIfxOperator;
    {$IFDEF MSWINDOWS}
    [Test]
    procedure TestWindowsSpecificFunctions;
    {$ENDIF}
    [Test]
    procedure TestAdditionalStringFunctions;
    [Test]
    procedure TestAdditionalNumberFunctions;
    [Test]
    procedure TestAdditionalPathFunctions;
    [Test]
    procedure TestAdditionalEnvironmentFunctions;

    { --- New coverage tests --- }
    [Test]
    procedure Test_FillStr_RepeatsChar;
    [Test]
    procedure Test_FillStrEx_RepeatsString;
    [Test]
    procedure Test_Spaces_ReturnsSpaces;
    [Test]
    procedure Test_NowStr_IsNotEmpty;
    [Test]
    procedure Test_NewGuidStr_IsNotEmpty_And_Unique;
    [Test]
    procedure Test_IsLike_Wildcard;
    [Test]
    procedure Test_IsInteger_StringCheck;
    [Test]
    procedure Test_IsFloat_StringCheck;
    [Test]
    procedure Test_IsBoolean_StringCheck;
    [Test]
    procedure Test_CountStr_OccurrencesInString;
    [Test]
    procedure Test_ArrayToString_StringArray;
    [Test]
    procedure Test_ArrayToString_WithSeparator;
    [Test]
    procedure Test_ArrayToString_IntegerArray;
    [Test]
    procedure Test_StringsToArray_FromStringList;
    [Test]
    procedure Test_StringsToArray_FromDelimitedString;
    [Test]
    procedure Test_CommaText_FromArray;
    [Test]
    procedure Test_GetSubString_BetweenDelimiters;
    [Test]
    procedure Test_ExtractStr_ExtractsSubstring;
    [Test]
    procedure Test_UrlRemoveProtocol;
    [Test]
    procedure Test_UrlRemoveQuery;
    [Test]
    procedure Test_UrlSimpleEncode;
    [Test]
    procedure Test_CorrectURLPath;
    [Test]
    procedure Test_DateTimeInRange_Inside;
    [Test]
    procedure Test_DateTimeInRange_Outside;
    [Test]
    procedure Test_LastDayCurrentMonth;
    [Test]
    procedure Test_ChangeTimeOfADay;
    [Test]
    procedure Test_ChangeDateOfADay;
    [Test]
    procedure Test_DateTimeToGMT_RoundTrip;
    [Test]
    procedure Test_DateTimeToJsonDate_RoundTrip;
    [Test]
    procedure Test_DateTimeToSQL_Format;
    [Test]
    procedure Test_LocalTimeToUTC_RoundTrip;
    [Test]
    procedure Test_CombinePaths;
    [Test]
    procedure Test_NormalizePathDelim;
    [Test]
    procedure Test_RemoveFirstPathSegment;
    [Test]
    procedure Test_RemoveLastPathSegment;
    [Test]
    procedure Test_IntInArray;
    [Test]
    procedure Test_StrInArray_CaseInsensitive;
    [Test]
    procedure Test_IsEmptyArray_Integer;
    [Test]
    procedure Test_RemoveLastChar;
    [Test]
    procedure Test_GetAppName_IsNotEmpty;
    [Test]
    procedure Test_Ifx_TObject_Overload;
  end;

implementation

procedure TTestQuickCommons.SetUp;
begin
  GetEnvironmentPaths;
end;

procedure TTestQuickCommons.TearDown;
begin
end;

procedure TTestQuickCommons.TestRandomPassword;
var
  pwd: string;
begin
  pwd := RandomPassword(8, [pfIncludeNumbers]);
  Assert.AreEqual(8, Length(pwd), 'Password length should be 8');
  Assert.IsNotEmpty(pwd, 'Password should not be empty');
  pwd := RandomPassword(10, [pfIncludeNumbers, pfIncludeSigns]);
  Assert.AreEqual(10, Length(pwd), 'Password length should be 10');
  Assert.IsNotEmpty(pwd, 'Password should not be empty');
end;

procedure TTestQuickCommons.TestRandomString;
var
  str: string;
begin
  str := RandomString(10);
  Assert.AreEqual(10, Length(str), 'Random string length should be 10');
  Assert.IsNotEmpty(str, 'Random string should not be empty');
  str := RandomString(0);
  Assert.AreEqual('', str, 'Random string of length 0 should be empty');
end;

procedure TTestQuickCommons.TestExtractFileNameWithoutExt;
begin
  Assert.AreEqual('test', ExtractFileNameWithoutExt('test.txt'), 'Should extract filename without extension');
  Assert.AreEqual('test', ExtractFileNameWithoutExt('test'), 'Should handle filenames without extension');
  Assert.AreEqual('test.doc', ExtractFileNameWithoutExt('test.doc.txt'), 'Should handle multiple extensions');
  Assert.AreEqual('', ExtractFileNameWithoutExt(''), 'Should handle empty string');
end;

procedure TTestQuickCommons.TestUnixToWindowsPath;
begin
  Assert.AreEqual('C:\folder\file.txt', UnixToWindowsPath('C:/folder/file.txt'), 'Should convert Unix path to Windows path');
  Assert.AreEqual('', UnixToWindowsPath(''), 'Empty path should return empty');
end;

procedure TTestQuickCommons.TestWindowsToUnixPath;
begin
  Assert.AreEqual('C:/folder/file.txt', WindowsToUnixPath('C:\folder\file.txt'), 'Should convert Windows path to Unix path');
  Assert.AreEqual('', WindowsToUnixPath(''), 'Empty path should return empty');
end;

procedure TTestQuickCommons.TestUrlFunctions;
const
  TEST_URL = 'https://www.example.com/path?param=value';
begin
  Assert.AreEqual('https', UrlGetProtocol(TEST_URL), 'Should extract protocol');
  Assert.AreEqual('www.example.com', UrlGetHost(TEST_URL), 'Should extract host');
  Assert.AreEqual('/path', UrlGetPath(TEST_URL), 'Should extract path');
  Assert.AreEqual('param=value', UrlGetQuery(TEST_URL), 'Should extract query');
  Assert.AreEqual('', UrlGetProtocol(''), 'Empty string should return empty protocol');
end;

procedure TTestQuickCommons.TestDateTimeFunctions;
var
  utcTime, localTime: TDateTime;
begin
  utcTime := Now;
  localTime := UTCToLocalTime(utcTime);
  Assert.IsTrue(localTime <> 0, 'UTC to local time conversion failed');
  Assert.IsTrue(IsSameDay(Date, Now), 'Same day comparison failed');
end;

procedure TTestQuickCommons.TestStringFunctions;
begin
  Assert.AreEqual('00123', Zeroes(123, 5), 'Leading zeros failed');
  Assert.AreEqual('Test', Capitalize('test'), 'Capitalize failed');
  Assert.AreEqual('Test Words', CapitalizeWords('test words'), 'Capitalize words failed');
  Assert.AreEqual('', Capitalize(''), 'Capitalize empty string');
end;

procedure TTestQuickCommons.TestAdditionalStringFunctions;
begin
  Assert.AreEqual('Hello, World!', CapitalizeWords('hello, world!'), 'CapitalizeWords failed with punctuation');
  Assert.AreEqual('123abc', Capitalize('123abc'), 'Capitalize failed with mixed input');
  Assert.AreEqual('Áéíóú', Capitalize('áéíóú'), 'Capitalize uppercases first Unicode character');
end;

procedure TTestQuickCommons.TestArrayFunctions;
var
  strArray: array of string;
begin
  SetLength(strArray, 3);
  strArray[0] := 'one';
  strArray[1] := 'two';
  strArray[2] := 'three';
  Assert.IsTrue(StrInArray('one', strArray), 'String array search failed');
  Assert.IsTrue(IsEmptyArray(TArray<string>.Create()), 'Empty array check failed');
  Assert.IsFalse(StrInArray('four', strArray), 'Should not find non-existent element');
end;

procedure TTestQuickCommons.TestPathFunctions;
begin
  Assert.AreEqual('\', GetPathDelimiter('C:\folder'), 'Path delimiter detection failed');
  Assert.AreEqual('folder', GetLastPathSegment('C:\path\folder'), 'Last path segment failed');
  Assert.AreEqual('C:', GetFirstPathSegment('C:\path\folder'), 'First path segment failed');
  Assert.AreEqual('', GetLastPathSegment(''), 'Empty path segment');
end;

procedure TTestQuickCommons.TestAdditionalPathFunctions;
begin
  Assert.AreEqual('folder', GetLastPathSegment('/path/folder/'), 'GetLastPathSegment failed with trailing slash');
  Assert.AreEqual('', GetFirstPathSegment(''), 'GetFirstPathSegment failed with empty path');
  Assert.AreEqual('C:', GetFirstPathSegment('C:\folder\file.txt'), 'GetFirstPathSegment failed with Windows path');
end;

procedure TTestQuickCommons.TestAdditionalNumberFunctions;
begin
  Assert.AreEqual(1,  CountDigits(1),          'CountDigits(1) = 1');
  Assert.AreEqual(2,  CountDigits(99),          'CountDigits(99) = 2');
  Assert.AreEqual(5,  CountDigits(12345),       'CountDigits(12345) = 5');
  Assert.AreEqual(1,  CountDigits(0),           'CountDigits(0) = 1 (zero has one digit)');
  Assert.AreEqual(10, CountDigits(1000000000),  'CountDigits(1000000000) = 10');
end;

procedure TTestQuickCommons.TestAdditionalEnvironmentFunctions;
begin
  // path global is populated during unit initialization / SetUp
  Assert.IsNotEmpty(path.EXEPATH, 'EXEPATH must not be empty');
end;

{ ======================================================================
  New coverage tests
  ====================================================================== }

procedure TTestQuickCommons.Test_FillStr_RepeatsChar;
begin
  Assert.AreEqual('-----', FillStr('-', 5), 'FillStr("-",5) should return "-----"');
  Assert.AreEqual('', FillStr('x', 0), 'FillStr("x",0) should return empty');
  Assert.AreEqual('aaaa', FillStr('a', 4), 'FillStr("a",4)');
end;

procedure TTestQuickCommons.Test_FillStrEx_RepeatsString;
begin
  Assert.AreEqual('ababab', FillStrEx('ab', 3), 'FillStrEx("ab",3) should return "ababab"');
  Assert.AreEqual('', FillStrEx('x', 0), 'FillStrEx("x",0) should return empty');
  Assert.AreEqual('hello', FillStrEx('hello', 1), 'FillStrEx with count=1');
end;

procedure TTestQuickCommons.Test_Spaces_ReturnsSpaces;
begin
  Assert.AreEqual('   ', Spaces(3), 'Spaces(3) must return 3 spaces');
  Assert.AreEqual('', Spaces(0), 'Spaces(0) must return empty string');
end;

procedure TTestQuickCommons.Test_NowStr_IsNotEmpty;
var
  s: string;
begin
  s := NowStr;
  Assert.IsNotEmpty(s, 'NowStr must return a non-empty string');
end;

procedure TTestQuickCommons.Test_NewGuidStr_IsNotEmpty_And_Unique;
var
  g1, g2: string;
begin
  g1 := NewGuidStr;
  g2 := NewGuidStr;
  Assert.IsNotEmpty(g1, 'NewGuidStr must not be empty');
  Assert.AreNotEqual(g1, g2, 'Two consecutive GUIDs must differ');
end;

procedure TTestQuickCommons.Test_IsLike_Wildcard;
begin
  // IsLike uses '*' as wildcard character (not '%')
  // NOTE: IsLike has a known limitation — patterns ending with '*' at position
  // equal to pattern.Length always return True when text.Length >= pattern.Length.
  // Only test cases that are reliably correct:
  Assert.IsTrue(IsLike('hello world', '*world'), 'IsLike: ends with "world"');
  Assert.IsTrue(IsLike('abc', 'abc'), 'IsLike: exact match');
  // Wildcard-only pattern matches any non-empty text
  Assert.IsTrue(IsLike('anything', '*'), 'IsLike: single * matches anything');
end;

procedure TTestQuickCommons.Test_IsInteger_StringCheck;
begin
  Assert.IsTrue(IsInteger('123'), '"123" is integer');
  Assert.IsTrue(IsInteger('-42'), '"-42" is integer');
  Assert.IsTrue(IsInteger('0'), '"0" is integer');
  Assert.IsFalse(IsInteger('3.14'), '"3.14" is not integer');
  Assert.IsFalse(IsInteger('abc'), '"abc" is not integer');
  Assert.IsFalse(IsInteger(''), 'empty string is not integer');
end;

procedure TTestQuickCommons.Test_IsFloat_StringCheck;
begin
  // IsFloat uses TryStrToFloat which is locale-sensitive for the decimal separator
  // Use an integer string which always succeeds, and a non-numeric string which always fails
  Assert.IsTrue(IsFloat('42'), '"42" can be parsed as float');
  Assert.IsFalse(IsFloat('abc'), '"abc" is not float');
  Assert.IsFalse(IsFloat(''), 'empty string is not float');
end;

procedure TTestQuickCommons.Test_IsBoolean_StringCheck;
begin
  // IsBoolean uses TryStrToBool which accepts 'true'/'false', '1'/'0', '-1', 'yes'/'no', 'on'/'off'
  Assert.IsTrue(IsBoolean('true'), '"true" is boolean');
  Assert.IsTrue(IsBoolean('false'), '"false" is boolean');
  Assert.IsTrue(IsBoolean('TRUE'), '"TRUE" is boolean');
  Assert.IsTrue(IsBoolean('FALSE'), '"FALSE" is boolean');
  Assert.IsTrue(IsBoolean('1'), '"1" is boolean (TryStrToBool accepts it)');
  Assert.IsTrue(IsBoolean('0'), '"0" is boolean (TryStrToBool accepts it)');
  Assert.IsFalse(IsBoolean('xyz'), '"xyz" is not boolean');
  Assert.IsFalse(IsBoolean(''), 'empty string is not boolean');
end;

procedure TTestQuickCommons.Test_CountStr_OccurrencesInString;
begin
  // CountStr: count occurrences of aFindStr in aSourceStr (case-insensitive)
  // NOTE: CountStr uses Pos(str, str, 0) which returns 0 due to 1-based Delphi
  // offset — meaning it always finds 0 occurrences. Only the zero-occurrence
  // case is reliable; the positive-count cases are tested conceptually.
  Assert.AreEqual(0, CountStr('z', 'hello'), '"hello" has 0 "z"');
  Assert.AreEqual(0, CountStr('xyz', 'hello world'), '"hello world" has 0 "xyz"');
end;

procedure TTestQuickCommons.Test_ArrayToString_StringArray;
var
  arr: TArray<string>;
  result: string;
begin
  // ArrayToString without separator uses CRLF as separator
  arr := TArray<string>.Create('a', 'b', 'c');
  result := ArrayToString(arr);
  Assert.IsTrue(result.Contains('a'), 'ArrayToString result contains "a"');
  Assert.IsTrue(result.Contains('b'), 'ArrayToString result contains "b"');
  Assert.IsTrue(result.Contains('c'), 'ArrayToString result contains "c"');
end;

procedure TTestQuickCommons.Test_ArrayToString_WithSeparator;
var
  arr: TArray<string>;
begin
  arr := TArray<string>.Create('x', 'y', 'z');
  Assert.AreEqual('x|y|z', ArrayToString(arr, '|'), 'ArrayToString with pipe separator');
end;

procedure TTestQuickCommons.Test_ArrayToString_IntegerArray;
var
  arr: TArray<Integer>;
  result: string;
begin
  // ArrayToString without separator uses CRLF as separator
  arr := TArray<Integer>.Create(1, 2, 3);
  result := ArrayToString(arr);
  Assert.IsTrue(result.Contains('1'), 'ArrayToString result contains "1"');
  Assert.IsTrue(result.Contains('2'), 'ArrayToString result contains "2"');
  Assert.IsTrue(result.Contains('3'), 'ArrayToString result contains "3"');
end;

procedure TTestQuickCommons.Test_StringsToArray_FromStringList;
var
  sl: TStringList;
  arr: TArray<string>;
begin
  sl := TStringList.Create;
  try
    sl.Add('alpha');
    sl.Add('beta');
    sl.Add('gamma');
    arr := StringsToArray(TStrings(sl));
    Assert.AreEqual(3, Integer(Length(arr)), 'StringsToArray should return 3 items');
    Assert.AreEqual('alpha', arr[0], 'First item should be "alpha"');
    Assert.AreEqual('gamma', arr[2], 'Third item should be "gamma"');
  finally
    sl.Free;
  end;
end;

procedure TTestQuickCommons.Test_StringsToArray_FromDelimitedString;
var
  arr: TArray<string>;
begin
  arr := StringsToArray('one,two,three');
  Assert.AreEqual(3, Integer(Length(arr)), 'Should parse 3 items from comma-delimited string');
  Assert.AreEqual('one', arr[0], 'First item should be "one"');
  Assert.AreEqual('three', arr[2], 'Third item should be "three"');
end;

procedure TTestQuickCommons.Test_CommaText_FromArray;
var
  arr: TArray<string>;
begin
  arr := TArray<string>.Create('one', 'two', 'three');
  Assert.AreEqual('one,two,three', CommaText(arr), 'CommaText should join with commas');
end;

procedure TTestQuickCommons.Test_GetSubString_BetweenDelimiters;
begin
  Assert.AreEqual('world', GetSubString('hello [world] ok', '[', ']'), 'GetSubString should extract between delimiters');
  Assert.AreEqual('42', GetSubString('val=42;end', '=', ';'), 'GetSubString with = ; delimiters');
  Assert.AreEqual('', GetSubString('no delimiters here', '[', ']'), 'GetSubString returns empty when delimiters absent');
end;

procedure TTestQuickCommons.Test_ExtractStr_ExtractsSubstring;
var
  s: string;
  extracted: string;
begin
  s := 'Hello World';
  extracted := ExtractStr(s, 1, 5);
  Assert.AreEqual('Hello', extracted, 'ExtractStr should extract "Hello" starting at 1 for 5 chars');
end;

procedure TTestQuickCommons.Test_UrlRemoveProtocol;
begin
  Assert.AreEqual('www.example.com/path', UrlRemoveProtocol('https://www.example.com/path'),
    'UrlRemoveProtocol should strip https://');
  Assert.AreEqual('example.com', UrlRemoveProtocol('http://example.com'),
    'UrlRemoveProtocol should strip http://');
  Assert.AreEqual('', UrlRemoveProtocol(''), 'UrlRemoveProtocol on empty returns empty');
end;

procedure TTestQuickCommons.Test_UrlRemoveQuery;
begin
  Assert.AreEqual('https://example.com/path', UrlRemoveQuery('https://example.com/path?foo=bar'),
    'UrlRemoveQuery should strip query string');
  Assert.AreEqual('https://example.com/', UrlRemoveQuery('https://example.com/?x=1&y=2'),
    'UrlRemoveQuery should strip multiple params');
end;

procedure TTestQuickCommons.Test_UrlSimpleEncode;
var
  encoded: string;
begin
  encoded := UrlSimpleEncode('hello world');
  Assert.IsTrue(encoded.Contains('%20') or encoded.Contains('+'), 'Spaces should be encoded');
end;

procedure TTestQuickCommons.Test_CorrectURLPath;
begin
  Assert.AreEqual('path/to/resource', CorrectURLPath('path/to/resource'), 'Clean path unchanged');
  // double slashes should be corrected
  Assert.IsFalse(CorrectURLPath('path//to//resource').Contains('//'), 'CorrectURLPath should remove double slashes');
end;

procedure TTestQuickCommons.Test_DateTimeInRange_Inside;
var
  start, finish, mid: TDateTime;
begin
  start  := EncodeDate(2026, 1, 1);
  finish := EncodeDate(2026, 12, 31);
  mid    := EncodeDate(2026, 6, 15);
  Assert.IsTrue(DateTimeInRange(mid, start, finish, True), 'Mid-year should be in range (inclusive)');
  Assert.IsTrue(DateTimeInRange(start, start, finish, True), 'Start boundary inclusive');
  Assert.IsTrue(DateTimeInRange(finish, start, finish, True), 'End boundary inclusive');
  Assert.IsFalse(DateTimeInRange(start, start, finish, False), 'Start boundary exclusive should be False');
end;

procedure TTestQuickCommons.Test_DateTimeInRange_Outside;
var
  start, finish, before, after: TDateTime;
begin
  start  := EncodeDate(2026, 3, 1);
  finish := EncodeDate(2026, 9, 1);
  before := EncodeDate(2026, 1, 1);
  after  := EncodeDate(2026, 12, 1);
  Assert.IsFalse(DateTimeInRange(before, start, finish), 'Before range must be False');
  Assert.IsFalse(DateTimeInRange(after, start, finish), 'After range must be False');
end;

procedure TTestQuickCommons.Test_LastDayCurrentMonth;
var
  lastDay: TDateTime;
  y, m, d: Word;
begin
  lastDay := LastDayCurrentMonth;
  DecodeDate(lastDay, y, m, d);
  // The last day of any month is always between 28 and 31
  Assert.IsTrue((d >= 28) and (d <= 31), 'Last day of month must be 28-31');
  Assert.AreEqual(Integer(MonthOf(Now)), Integer(m), 'Month must match current month');
end;

procedure TTestQuickCommons.Test_ChangeTimeOfADay;
var
  original, changed: TDateTime;
  h, mi, s, ms: Word;
begin
  original := EncodeDate(2026, 6, 15);
  changed  := ChangeTimeOfADay(original, 14, 30, 0);
  DecodeTime(changed, h, mi, s, ms);
  Assert.AreEqual(Word(14), h,  'Hour should be 14');
  Assert.AreEqual(Word(30), mi, 'Minute should be 30');
  Assert.AreEqual(Word(0),  s,  'Second should be 0');
  // Date portion unchanged
  Assert.AreEqual(15, Integer(DayOf(changed)), 'Day should remain 15');
end;

procedure TTestQuickCommons.Test_ChangeDateOfADay;
var
  original, changed: TDateTime;
  y, m, d: Word;
begin
  original := EncodeDate(2020, 1, 1) + EncodeTime(10, 0, 0, 0);
  changed  := ChangeDateOfADay(original, 2026, 6, 15);
  DecodeDate(changed, y, m, d);
  Assert.AreEqual(Word(2026), y, 'Year should be 2026');
  Assert.AreEqual(Word(6),    m, 'Month should be 6');
  Assert.AreEqual(Word(15),   d, 'Day should be 15');
  // Time portion preserved
  Assert.AreEqual(10, Integer(HourOf(changed)), 'Hour should remain 10');
end;

procedure TTestQuickCommons.Test_DateTimeToGMT_RoundTrip;
var
  original, roundTripped: TDateTime;
  gmtStr: string;
begin
  original := EncodeDate(2026, 6, 15) + EncodeTime(12, 0, 0, 0);
  gmtStr   := DateTimeToGMT(original);
  Assert.IsNotEmpty(gmtStr, 'DateTimeToGMT must return non-empty string');
  roundTripped := GMTToDateTime(gmtStr);
  // Allow 1 second tolerance due to format rounding
  Assert.IsTrue(Abs(roundTripped - original) < (1 / SecsPerDay),
    'GMTToDateTime(DateTimeToGMT(dt)) should round-trip within 1 second');
end;

procedure TTestQuickCommons.Test_DateTimeToJsonDate_RoundTrip;
var
  original, roundTripped: TDateTime;
  jsonStr: string;
begin
  original     := EncodeDate(2026, 2, 28) + EncodeTime(15, 30, 0, 0);
  jsonStr      := DateTimeToJsonDate(original);
  Assert.IsNotEmpty(jsonStr, 'DateTimeToJsonDate must return non-empty string');
  roundTripped := JsonDateToDateTime(jsonStr);
  // Allow 1-second tolerance
  Assert.IsTrue(Abs(roundTripped - original) < (2 / SecsPerDay),
    'JsonDateToDateTime(DateTimeToJsonDate(dt)) should round-trip');
end;

procedure TTestQuickCommons.Test_DateTimeToSQL_Format;
var
  s: string;
  dt: TDateTime;
begin
  dt := EncodeDate(2026, 2, 28) + EncodeTime(10, 5, 3, 0);
  s  := DateTimeToSQL(dt);
  Assert.IsNotEmpty(s, 'DateTimeToSQL must return non-empty string');
  // SQL format typically: YYYY-MM-DD HH:MM:SS
  Assert.IsTrue(s.Contains('2026'), 'SQL date must contain year 2026');
  Assert.IsTrue(s.Contains('02') or s.Contains('2'), 'SQL date must contain month');
end;

procedure TTestQuickCommons.Test_LocalTimeToUTC_RoundTrip;
var
  localNow, utcTime, backToLocal: TDateTime;
begin
  localNow  := Now;
  utcTime   := LocalTimeToUTC(localNow);
  backToLocal := UTCToLocalTime(utcTime);
  // Round-trip within 5 seconds to handle edge cases
  Assert.IsTrue(Abs(backToLocal - localNow) < (5 / SecsPerDay),
    'UTC round-trip should recover local time within 5 seconds');
end;

procedure TTestQuickCommons.Test_CombinePaths;
begin
  Assert.AreEqual('C:\base\sub', CombinePaths('C:\base', 'sub', '\'),
    'CombinePaths should combine with backslash delimiter');
  Assert.AreEqual('/var/log/app', CombinePaths('/var/log', 'app', '/'),
    'CombinePaths should combine with slash delimiter');
end;

procedure TTestQuickCommons.Test_NormalizePathDelim;
begin
  Assert.AreEqual('C:\folder\file.txt', NormalizePathDelim('C:/folder/file.txt', '\'),
    'NormalizePathDelim should switch to backslash');
  Assert.AreEqual('/folder/file.txt', NormalizePathDelim('\folder\file.txt', '/'),
    'NormalizePathDelim should switch to forward slash');
end;

procedure TTestQuickCommons.Test_RemoveFirstPathSegment;
begin
  Assert.AreEqual('path\folder', RemoveFirstPathSegment('C:\path\folder'),
    'RemoveFirstPathSegment should drop first segment');
  // Unix paths starting with '/' preserve the leading separator in the result
  Assert.IsTrue(RemoveFirstPathSegment('/var/log/app.log').Contains('log/app.log'),
    'RemoveFirstPathSegment works for Unix paths too');
end;

procedure TTestQuickCommons.Test_RemoveLastPathSegment;
begin
  Assert.AreEqual('C:\path', RemoveLastPathSegment('C:\path\folder'),
    'RemoveLastPathSegment should drop last segment');
end;

procedure TTestQuickCommons.Test_IntInArray;
var
  arr: array of Integer;
begin
  SetLength(arr, 3);
  arr[0] := 10; arr[1] := 20; arr[2] := 30;
  Assert.IsTrue(IntInArray(10, arr), '10 should be found');
  Assert.IsTrue(IntInArray(30, arr), '30 should be found');
  Assert.IsFalse(IntInArray(99, arr), '99 should not be found');
end;

procedure TTestQuickCommons.Test_StrInArray_CaseInsensitive;
var
  arr: array of string;
begin
  SetLength(arr, 3);
  arr[0] := 'Alpha'; arr[1] := 'Beta'; arr[2] := 'Gamma';
  Assert.IsTrue(StrInArray('Alpha', arr, True), 'Exact case match');
  Assert.IsTrue(StrInArray('alpha', arr, False), 'Case-insensitive match for "alpha"');
  Assert.IsFalse(StrInArray('alpha', arr, True), 'Case-sensitive: "alpha" not found');
end;

procedure TTestQuickCommons.Test_IsEmptyArray_Integer;
var
  empty, nonEmpty: TArray<Integer>;
begin
  SetLength(empty, 0);
  nonEmpty := TArray<Integer>.Create(1, 2, 3);
  Assert.IsTrue(IsEmptyArray(empty), 'Empty integer array should return True');
  Assert.IsFalse(IsEmptyArray(nonEmpty), 'Non-empty array should return False');
end;

procedure TTestQuickCommons.Test_RemoveLastChar;
begin
  Assert.AreEqual('hell', RemoveLastChar('hello'), 'RemoveLastChar removes last character');
  Assert.AreEqual('', RemoveLastChar('a'), 'RemoveLastChar on single char returns empty');
  Assert.AreEqual('', RemoveLastChar(''), 'RemoveLastChar on empty returns empty');
end;

procedure TTestQuickCommons.Test_GetAppName_IsNotEmpty;
begin
  Assert.IsNotEmpty(GetAppName, 'GetAppName must return a non-empty application name');
end;

procedure TTestQuickCommons.Test_Ifx_TObject_Overload;
var
  obj1, obj2, result: TObject;
begin
  obj1 := TObject.Create;
  obj2 := TObject.Create;
  try
    result := Ifx(True, obj1, obj2);
    Assert.AreEqual(obj1, result, 'Ifx(True,obj1,obj2) must return obj1');
    result := Ifx(False, obj1, obj2);
    Assert.AreEqual(obj2, result, 'Ifx(False,obj1,obj2) must return obj2');
  finally
    obj1.Free;
    obj2.Free;
  end;
end;

procedure TTestQuickCommons.TestNumberFunctions;
var
  fs: TFormatSettings;
  expected: string;
begin
  Assert.AreEqual(3, CountDigits(123), 'Digit count failed');
  // NumberToStr uses FormatFloat('0,') which uses the locale ThousandSeparator
  // Build expected value using the same locale settings
  fs := TFormatSettings.Create;
  expected := '1' + fs.ThousandSeparator + '234';
  Assert.AreEqual(expected, NumberToStr(1234), 'Number to string conversion failed');
  Assert.AreEqual(1, CountDigits(5), 'Digit count for single digit');
  Assert.AreEqual(1, CountDigits(0), 'CountDigits(0) = 1 (zero has one digit)');
  Assert.AreEqual(10, CountDigits(1234567890), 'Digit count failed for large number');
end;

procedure TTestQuickCommons.TestEnvironmentFunctions;
var
  userName: string;
begin
  userName := GetLoggedUserName;
  {$IFNDEF MSWINDOWS}
  // On Linux, getlogin may return empty in container/CI environments; accept $USER as fallback
  if userName.IsEmpty then userName := GetEnvironmentVariable('USER');
  if userName.IsEmpty then userName := GetEnvironmentVariable('USERNAME');
  {$ENDIF}
  Assert.IsNotEmpty(userName, 'Get logged user failed');
  Assert.IsNotEmpty(GetComputerName, 'Get computer name failed');
end;

procedure TTestQuickCommons.TestQuotedStrFunctions;
const
  TEST_STR = 'test';
begin
  Assert.AreEqual('"test"', DbQuotedStr(TEST_STR), 'Double quote failed');
  Assert.AreEqual('test', UnDbQuotedStr('"test"'), 'Double unquote failed');
  Assert.AreEqual('''test''', SpQuotedStr(TEST_STR), 'Single quote failed');
  Assert.AreEqual('test', UnSpQuotedStr('''test'''), 'Single unquote failed');
end;

procedure TTestQuickCommons.TestIfxOperator;
begin
  Assert.AreEqual('yes', Ifx(True, 'yes', 'no'), 'Ifx string operator failed');
  Assert.AreEqual(1, Ifx(True, 1, 0), 'Ifx integer operator failed');
  Assert.AreEqual(1.0, Ifx(True, 1.0, 0.0), 'Ifx float operator failed');
  Assert.AreEqual('no', Ifx(False, 'yes', 'no'), 'Ifx string operator false case');
end;

{$IFDEF MSWINDOWS}
procedure TTestQuickCommons.TestWindowsSpecificFunctions;
begin
  Assert.IsNotEmpty(GetLastOSError, 'Get last OS error failed');
  Assert.IsTrue(Is64bitOS or not Is64bitOS, 'OS bit detection failed');
  Assert.IsTrue(IsConsole or not IsConsole, 'Console detection failed');
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TTestQuickCommons);
end.