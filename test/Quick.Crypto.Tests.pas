unit Quick.Crypto.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Quick.Crypto;

type
  [TestFixture]
  TQuickCryptoTests = class(TObject)
  private
    const
      TEST_PASSWORD = 'MySecretPassword123';
      TEST_TEXT = 'Hello World! This is a test message.';
      EMPTY_TEXT = '';
  published
    [Test]
    procedure Test_AES128_Encrypt_ValidInput;
    [Test]
    procedure Test_AES128_Decrypt_ValidInput;
    [Test]
    procedure Test_AES128_EmptyString;
    [Test]
    procedure Test_AES128_EmptyPassword;
    [Test]
    procedure Test_AES128_LongText;
    [Test]
    procedure Test_AES128_SpecialCharacters;
    [Test]
    procedure Test_AES128_EncryptDecrypt_Cycle;
    [Test]
    procedure Test_AES128_UnicodeCharacters;
  end;

implementation

procedure TQuickCryptoTests.Test_AES128_Encrypt_ValidInput;
var
  encrypted: string;
begin
  encrypted := AES128_Encrypt(TEST_TEXT, TEST_PASSWORD);
  Assert.AreNotEqual(TEST_TEXT, encrypted, 'Encrypted text should not be the same as the original');
  Assert.IsTrue(encrypted <> '', 'Encrypted text should not be empty');
end;

procedure TQuickCryptoTests.Test_AES128_Decrypt_ValidInput;
var
  encrypted, decrypted: string;
begin
  encrypted := AES128_Encrypt(TEST_TEXT, TEST_PASSWORD);
  decrypted := AES128_Decrypt(encrypted, TEST_PASSWORD);
  Assert.AreEqual(TEST_TEXT, decrypted, 'Decrypted text should be the same as the original');
end;

procedure TQuickCryptoTests.Test_AES128_EmptyString;
var
  encrypted, decrypted: string;
begin
  encrypted := AES128_Encrypt(EMPTY_TEXT, TEST_PASSWORD);
  Assert.AreEqual(EMPTY_TEXT, encrypted, 'Encryption of empty text should return empty text');
  
  decrypted := AES128_Decrypt(encrypted, TEST_PASSWORD);
  Assert.AreEqual(EMPTY_TEXT, decrypted, 'Decryption of empty text should return empty text');
end;

procedure TQuickCryptoTests.Test_AES128_EmptyPassword;
begin
  Assert.WillRaise(
    procedure
    begin
      AES128_Encrypt(TEST_TEXT, '');
    end,
    EAssertionFailed,
    'Encryption with empty password should raise an exception'
  );
end;

procedure TQuickCryptoTests.Test_AES128_LongText;
const
  LONG_TEXT = 'This is a very long text that will be used to test the encryption ' +
              'of long strings and ensure that the process works correctly ' +
              'even with texts that exceed 256 characters. We need to verify ' +
              'that encryption and decryption work smoothly.';
var
  encrypted, decrypted: string;
begin
  encrypted := AES128_Encrypt(LONG_TEXT, TEST_PASSWORD);
  decrypted := AES128_Decrypt(encrypted, TEST_PASSWORD);
  Assert.AreEqual(LONG_TEXT, decrypted, 'Long text should decrypt correctly');
end;

procedure TQuickCryptoTests.Test_AES128_SpecialCharacters;
const
  SPECIAL_CHARS = '!@#$%^&*()_+-=[]{}|;:,.<>?`~';
var
  encrypted, decrypted: string;
begin
  encrypted := AES128_Encrypt(SPECIAL_CHARS, TEST_PASSWORD);
  decrypted := AES128_Decrypt(encrypted, TEST_PASSWORD);
  Assert.AreEqual(SPECIAL_CHARS, decrypted, 'Special characters should be handled correctly');
end;

procedure TQuickCryptoTests.Test_AES128_EncryptDecrypt_Cycle;
var
  i: Integer;
  encrypted, decrypted, currentText: string;
begin
  currentText := TEST_TEXT;
  for i := 1 to 5 do
  begin
    encrypted := AES128_Encrypt(currentText, TEST_PASSWORD);
    decrypted := AES128_Decrypt(encrypted, TEST_PASSWORD);
    Assert.AreEqual(currentText, decrypted, Format('Cycle %d: Text should remain the same', [i]));
    currentText := encrypted;
  end;
end;

procedure TQuickCryptoTests.Test_AES128_UnicodeCharacters;
const
  UNICODE_TEXT = 'Hello World! 你好世界 привет мир ⭐🌟';
var
  encrypted, decrypted: string;
begin
  encrypted := AES128_Encrypt(UNICODE_TEXT, TEST_PASSWORD);
  decrypted := AES128_Decrypt(encrypted, TEST_PASSWORD);
  Assert.AreEqual(UNICODE_TEXT, decrypted, 'Unicode characters should be handled correctly');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickCryptoTests);
end.