{ ***************************************************************************

  Copyright (c) 2016-2026 Kike Pérez

  Unit        : Quick.Format.Tests
  Description : Unit Tests for Quick.Format
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 27/02/2026
  Modified    : 27/02/2026

 *************************************************************************** }

unit Quick.Format.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Quick.Format;

type
  [TestFixture]
  TQuickFormatTests = class(TObject)
  public
    { NumberToStr }
    [Test]
    procedure Test_NumberToStr_Zero;
    [Test]
    procedure Test_NumberToStr_SmallPositive;
    [Test]
    procedure Test_NumberToStr_Thousands;
    [Test]
    procedure Test_NumberToStr_Millions;
    [Test]
    procedure Test_NumberToStr_Negative;
    [Test]
    procedure Test_NumberToStr_MaxInt64;

    { FormatBytes - bytes range (no decimals) }
    [Test]
    procedure Test_FormatBytes_Zero;
    [Test]
    procedure Test_FormatBytes_OneByteNoSpace;
    [Test]
    procedure Test_FormatBytes_OneByteWithSpace;
    [Test]
    procedure Test_FormatBytes_ExactlyOneKB;
    [Test]
    procedure Test_FormatBytes_ExactlyOneMB;
    [Test]
    procedure Test_FormatBytes_ExactlyOneGB;
    [Test]
    procedure Test_FormatBytes_PartialMB_HasDecimals;
    [Test]
    procedure Test_FormatBytes_SpacedTrue_ContainsSpace;
    [Test]
    procedure Test_FormatBytes_SpacedFalse_NoSpace;
    [Test]
    procedure Test_FormatBytes_KBUnit;
    [Test]
    procedure Test_FormatBytes_MBUnit;
    [Test]
    procedure Test_FormatBytes_GBUnit;
  end;

implementation

{ TQuickFormatTests }

{ --- NumberToStr --- }

procedure TQuickFormatTests.Test_NumberToStr_Zero;
begin
  Assert.AreEqual('0', NumberToStr(0), 'Zero should format as "0"');
end;

procedure TQuickFormatTests.Test_NumberToStr_SmallPositive;
begin
  Assert.AreEqual('999', NumberToStr(999), 'Values below 1000 have no separator');
end;

procedure TQuickFormatTests.Test_NumberToStr_Thousands;
var
  s: string;
begin
  s := NumberToStr(1000);
  // FormatFloat('0,', ...) produces locale-dependent thousands separator.
  // We verify that the digits 1 and 0s are present and it is not an error string.
  Assert.IsFalse(s.StartsWith('#'), 'Result should not be an error string');
  Assert.IsTrue(s.Contains('1'), 'Result must contain digit 1');
end;

procedure TQuickFormatTests.Test_NumberToStr_Millions;
var
  s: string;
begin
  s := NumberToStr(1000000);
  Assert.IsFalse(s.StartsWith('#'), 'Result should not be an error string');
  Assert.IsTrue(s.Contains('1'), 'Result must contain digit 1');
end;

procedure TQuickFormatTests.Test_NumberToStr_Negative;
var
  s: string;
begin
  s := NumberToStr(-5000);
  Assert.IsFalse(s.StartsWith('#'), 'Negative values should not produce an error');
end;

procedure TQuickFormatTests.Test_NumberToStr_MaxInt64;
var
  s: string;
begin
  s := NumberToStr(High(Int64));
  Assert.IsFalse(s.StartsWith('#'), 'MaxInt64 should not produce an error');
end;

{ --- FormatBytes --- }

procedure TQuickFormatTests.Test_FormatBytes_Zero;
var
  s: string;
begin
  s := FormatBytes(0);
  Assert.IsTrue(s.Contains('Byte') or s.Contains('0'),
    'Zero bytes should show "Byte(s)" or "0"');
end;

procedure TQuickFormatTests.Test_FormatBytes_OneByteNoSpace;
var
  s: string;
begin
  s := FormatBytes(1, False);
  Assert.IsTrue(s.Contains('Byte'), 'Single byte should use "Byte(s)" unit');
  Assert.IsFalse(s.Contains(' '), 'Spaced=False must not insert a space before unit');
end;

procedure TQuickFormatTests.Test_FormatBytes_OneByteWithSpace;
var
  s: string;
begin
  s := FormatBytes(1, True);
  Assert.IsTrue(s.Contains('Byte'), 'Single byte should use "Byte(s)" unit');
  Assert.IsTrue(s.Contains(' '), 'Spaced=True must insert a space before unit');
end;

procedure TQuickFormatTests.Test_FormatBytes_ExactlyOneKB;
var
  s: string;
begin
  // FormatBytes uses strict '>': 1025 > 1024 is True, so i advances to 1 (KB)
  s := FormatBytes(1025, False);
  Assert.IsTrue(s.Contains('KB'), '1025 bytes should display as KB');
end;

procedure TQuickFormatTests.Test_FormatBytes_ExactlyOneMB;
var
  s: string;
begin
  // 1024*1024+1 > 1024*1024, so i advances to 2 (MB)
  s := FormatBytes(Int64(1024) * 1024 + 1, False);
  Assert.IsTrue(s.Contains('MB'), '1 MB + 1 byte should display as MB');
end;

procedure TQuickFormatTests.Test_FormatBytes_ExactlyOneGB;
var
  s: string;
begin
  // 1024^3 + 1 > 1024^3, so i advances to 3 (GB)
  s := FormatBytes(Int64(1024) * 1024 * 1024 + 1, False);
  Assert.IsTrue(s.Contains('GB'), '1 GB + 1 byte should display as GB');
end;

procedure TQuickFormatTests.Test_FormatBytes_PartialMB_HasDecimals;
var
  s: string;
begin
  // 1.5 MB — should include a decimal separator (i=2 → '%.2f' branch)
  s := FormatBytes(Int64(1536) * 1024, False);
  Assert.IsTrue(s.Contains('MB'), '1.5 MB should display as MB');
  // Locale-independent: result length > 4 means there is at least one digit after separator
  Assert.IsTrue(Length(s) > 4, '1.5 MB result should contain decimal digits');
end;

procedure TQuickFormatTests.Test_FormatBytes_SpacedTrue_ContainsSpace;
var
  s: string;
begin
  s := FormatBytes(1024 * 1024, True);
  Assert.IsTrue(s.Contains(' '), 'Spaced=True should place a space between value and unit');
end;

procedure TQuickFormatTests.Test_FormatBytes_SpacedFalse_NoSpace;
var
  s: string;
begin
  s := FormatBytes(1024 * 1024, False);
  Assert.IsFalse(s.Contains(' '), 'Spaced=False should not place a space between value and unit');
end;

procedure TQuickFormatTests.Test_FormatBytes_KBUnit;
var
  s: string;
begin
  s := FormatBytes(2048, False);
  Assert.IsTrue(s.Contains('KB'), '2 KB should display as KB');
end;

procedure TQuickFormatTests.Test_FormatBytes_MBUnit;
var
  s: string;
begin
  s := FormatBytes(Int64(2) * 1024 * 1024, False);
  Assert.IsTrue(s.Contains('MB'), '2 MB should display as MB');
end;

procedure TQuickFormatTests.Test_FormatBytes_GBUnit;
var
  s: string;
begin
  s := FormatBytes(Int64(2) * 1024 * 1024 * 1024, False);
  Assert.IsTrue(s.Contains('GB'), '2 GB should display as GB');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickFormatTests);

end.
