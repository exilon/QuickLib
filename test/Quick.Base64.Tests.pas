unit Quick.Base64.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Quick.Base64;

type
  [TestFixture]
  TTestQuickBase64 = class(TObject)
  public
    [Test]
    procedure Test_Base64Encode_SimpleString;
    [Test]
    procedure Test_Base64Encode_SpecialChars;
    [Test]
    procedure Test_Base64Encode_EmptyString;
    [Test]
    procedure Test_Base64Decode_ValidInput;
    [Test]
    procedure Test_Base64Decode_EmptyString;
    {$IFDEF DELPHIXE7_UP}
    [Test]
    procedure Test_Base64DecodeFromBinary;
    [Test]
    procedure Test_Base64DecodeToBytes;
    {$ENDIF}
  end;

implementation

procedure TTestQuickBase64.Test_Base64Encode_SimpleString;
const
  INPUT_TEXT = 'Hello World';
  EXPECTED_OUTPUT = 'SGVsbG8gV29ybGQ=';
begin
  Assert.AreEqual(EXPECTED_OUTPUT, Base64Encode(INPUT_TEXT), 'Base64 encoding of simple string failed');
end;

procedure TTestQuickBase64.Test_Base64Encode_SpecialChars;
const
  INPUT_TEXT = 'Test@123!#$%';
  EXPECTED_OUTPUT = 'VGVzdEAxMjMhIyQl';
begin
  Assert.AreEqual(EXPECTED_OUTPUT, Base64Encode(INPUT_TEXT), 'Base64 encoding of special characters failed');
end;

procedure TTestQuickBase64.Test_Base64Encode_EmptyString;
begin
  Assert.AreEqual('', Base64Encode(''), 'Base64 encoding of empty string should return empty string');
end;

procedure TTestQuickBase64.Test_Base64Decode_ValidInput;
const
  ENCODED_TEXT = 'SGVsbG8gV29ybGQ=';
  EXPECTED_OUTPUT = 'Hello World';
begin
  Assert.AreEqual(EXPECTED_OUTPUT, Base64Decode(ENCODED_TEXT), 'Base64 decoding failed');
end;

procedure TTestQuickBase64.Test_Base64Decode_EmptyString;
begin
  Assert.AreEqual('', Base64Decode(''), 'Base64 decoding of empty string should return empty string');
end;

{$IFDEF DELPHIXE7_UP}
procedure TTestQuickBase64.Test_Base64DecodeFromBinary;
const
  BINARY_DATA = 'AAECAwQFBgcICQ==';  // Represents bytes 0 through 9
var
  Result: string;
begin
  Result := Base64DecodeFromBinary(BINARY_DATA);
  Assert.IsNotEmpty(Result, 'Binary decode result should not be empty');
end;

procedure TTestQuickBase64.Test_Base64DecodeToBytes;
const
  ENCODED_TEXT = 'SGVsbG8=';  // "Hello" in Base64
var
  Bytes: TBytes;
begin
  Bytes := Base64DecodeToBytes(ENCODED_TEXT);
  Assert.AreEqual(5, Length(Bytes), 'Decoded bytes length should match original text length');
  Assert.AreEqual(72, Bytes[0], 'First byte should match ASCII value of H');
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TTestQuickBase64);
end.