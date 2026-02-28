unit Quick.Compression.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  ZLib,
  System.NetEncoding,
  Quick.Compression;

type
  [TestFixture]
  TQuickCompressionTests = class(TObject)
  private
    const
      TEST_STRING = 'This is a test string that will be compressed and decompressed';
      SPECIAL_CHARS = 'Special characters! áéíóú ñÑ @#$%^&*()';
      LONG_TEXT = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. ' +
                 'Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ' +
                 'Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ' +
                 'nisi ut aliquip ex ea commodo consequat.';
  published
    [Test]
    procedure Test_CompressString_Basic;
    [Test]
    procedure Test_DecompressString_Basic;
    [Test]
    procedure Test_CompressString_EmptyString;
    [Test]
    procedure Test_CompressString_SpecialChars;
    [Test]
    procedure Test_CompressString_LongText;
    [Test]
    procedure Test_CompressGZipString_Basic;
    [Test]
    procedure Test_DecompressGZipString_Basic;
    [Test]
    procedure Test_CompressAndEncodeString_Basic;
    [Test]
    procedure Test_DecodeAndDecompressString_Basic;
    [Test]
    procedure Test_CompressionLevels;
    [Test]
    procedure Test_Deprecated_Methods;
  end;

implementation

procedure TQuickCompressionTests.Test_CompressString_Basic;
var
  compressed, decompressed: string;
begin
  compressed := CompressString(TEST_STRING);
  decompressed := DecompressString(compressed);
  Assert.AreEqual(TEST_STRING, decompressed, 'Basic compression/decompression failed');
  Assert.IsTrue(Length(compressed) < Length(TEST_STRING), 'Compressed text should be shorter');
end;

procedure TQuickCompressionTests.Test_DecompressString_Basic;
var
  compressed: string;
begin
  compressed := CompressString(TEST_STRING);
  Assert.AreEqual(TEST_STRING, DecompressString(compressed), 'Decompression failed');
end;

procedure TQuickCompressionTests.Test_CompressString_EmptyString;
begin
  Assert.AreEqual('', CompressString(''), 'Compression of empty string should return empty string');
  Assert.AreEqual('', DecompressString(''), 'Decompression of empty string should return empty string');
end;

procedure TQuickCompressionTests.Test_CompressString_SpecialChars;
var
  compressed, decompressed: string;
begin
  compressed := CompressString(SPECIAL_CHARS);
  decompressed := DecompressString(compressed);
  Assert.AreEqual(SPECIAL_CHARS, decompressed, 'Compression with special characters failed');
end;

procedure TQuickCompressionTests.Test_CompressString_LongText;
var
  compressed, decompressed: string;
begin
  compressed := CompressString(LONG_TEXT);
  decompressed := DecompressString(compressed);
  Assert.AreEqual(LONG_TEXT, decompressed, 'Compression of long text failed');
end;

procedure TQuickCompressionTests.Test_CompressGZipString_Basic;
var
  compressed, decompressed: string;
begin
  compressed := CompressGZipString(TEST_STRING);
  decompressed := DecompressGZipString(compressed);
  Assert.AreEqual(TEST_STRING, decompressed, 'GZip compression/decompression failed');
end;

procedure TQuickCompressionTests.Test_DecompressGZipString_Basic;
var
  compressed: string;
begin
  compressed := CompressGZipString(TEST_STRING);
  Assert.AreEqual(TEST_STRING, DecompressGZipString(compressed), 'GZip decompression failed');
end;

procedure TQuickCompressionTests.Test_CompressAndEncodeString_Basic;
var
  compressed, decompressed: string;
begin
  compressed := CompressAndEncodeString(TEST_STRING);
  decompressed := DecodeAndDecompressString(compressed);
  Assert.AreEqual(TEST_STRING, decompressed, 'Compression/encoding failed');
end;

procedure TQuickCompressionTests.Test_DecodeAndDecompressString_Basic;
var
  compressed: string;
begin
  compressed := CompressAndEncodeString(TEST_STRING);
  Assert.AreEqual(TEST_STRING, DecodeAndDecompressString(compressed), 'Decoding/decompression failed');
end;

procedure TQuickCompressionTests.Test_CompressionLevels;
var
  compressedDefault, compressedMax: string;
begin
  compressedDefault := CompressString(LONG_TEXT, zcDefault);
  compressedMax := CompressString(LONG_TEXT, zcMax);
  Assert.IsTrue(Length(compressedMax) <= Length(compressedDefault), 
    'Maximum compression should be as effective or more than default');
end;

procedure TQuickCompressionTests.Test_Deprecated_Methods;
var
  compressed, decompressed: string;
begin
  compressed := CompressString_Deprecated(TEST_STRING);
  decompressed := DecompressString_Deprecated(compressed);
  Assert.AreEqual(TEST_STRING, decompressed, 'Deprecated methods should still work');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickCompressionTests);
end.