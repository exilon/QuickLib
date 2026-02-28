{ ***************************************************************************
  Copyright (c) 2015-2026 Kike Pérez
  Unit        : Quick.Streams.Tests
  Description : Quick.Streams unit tests
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 27/02/2026
  Modified    : 27/02/2026
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

unit Quick.Streams.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Threading,
  Quick.Streams;

type
  [TestFixture]
  TQuickStreamsTests = class(TObject)
  public
    [Test]
    procedure Test_Create_DefaultTimeouts;
    [Test]
    procedure Test_Write_Then_Read_SameData;
    [Test]
    procedure Test_Write_Returns_ByteCount;
    [Test]
    procedure Test_Read_Returns_ByteCount;
    [Test]
    procedure Test_Seek_FromBeginning;
    [Test]
    procedure Test_Seek_FromEnd;
    [Test]
    procedure Test_Seek_FromCurrent;
    [Test]
    procedure Test_WriteRead_Threaded;
    [Test]
    procedure Test_ReadTimeout_RaisesException;
  end;

implementation

const
  TEST_CAPACITY = 1024;

// ── helpers ────────────────────────────────────────────────────────────────

procedure WriteAndReadHelper(out readBuf : TBytes);
var
  ps : TProxyStream;
  writeBuf : TBytes;
  n : Longint;
begin
  SetLength(writeBuf, 4);
  writeBuf[0] := 1; writeBuf[1] := 2; writeBuf[2] := 3; writeBuf[3] := 4;
  SetLength(readBuf, 4);
  ps := TProxyStream.Create(TEST_CAPACITY);
  try
    n := ps.Write(writeBuf[0], 4);
    Assert.AreEqual(4, Integer(n));
    n := ps.Read(readBuf[0], 4);
    Assert.AreEqual(4, Integer(n));
  finally
    ps.Free;
  end;
end;

// ── tests ──────────────────────────────────────────────────────────────────

procedure TQuickStreamsTests.Test_Create_DefaultTimeouts;
var
  ps : TProxyStream;
begin
  ps := TProxyStream.Create(512);
  try
    Assert.IsTrue(ps.ReadTimeout > 0, 'Default ReadTimeout should be > 0');
    Assert.IsTrue(ps.WriteTimeout > 0, 'Default WriteTimeout should be > 0');
  finally
    ps.Free;
  end;
end;

procedure TQuickStreamsTests.Test_Write_Then_Read_SameData;
var
  ps       : TProxyStream;
  wBuf, rBuf : TBytes;
begin
  SetLength(wBuf, 4);
  wBuf[0] := 10; wBuf[1] := 20; wBuf[2] := 30; wBuf[3] := 40;
  SetLength(rBuf, 4);

  ps := TProxyStream.Create(TEST_CAPACITY);
  try
    ps.Write(wBuf[0], 4);
    ps.Read(rBuf[0], 4);
    Assert.AreEqual(Integer(wBuf[0]), Integer(rBuf[0]), 'Byte 0 should match');
    Assert.AreEqual(Integer(wBuf[1]), Integer(rBuf[1]), 'Byte 1 should match');
    Assert.AreEqual(Integer(wBuf[2]), Integer(rBuf[2]), 'Byte 2 should match');
    Assert.AreEqual(Integer(wBuf[3]), Integer(rBuf[3]), 'Byte 3 should match');
  finally
    ps.Free;
  end;
end;

procedure TQuickStreamsTests.Test_Write_Returns_ByteCount;
var
  ps   : TProxyStream;
  wBuf : TBytes;
  n    : Longint;
begin
  SetLength(wBuf, 8);
  ps := TProxyStream.Create(TEST_CAPACITY);
  try
    n := ps.Write(wBuf[0], 8);
    Assert.AreEqual(8, Integer(n), 'Write should return the number of bytes written');
  finally
    ps.Free;
  end;
end;

procedure TQuickStreamsTests.Test_Read_Returns_ByteCount;
var
  ps       : TProxyStream;
  wBuf, rBuf : TBytes;
  n        : Longint;
begin
  SetLength(wBuf, 6);
  SetLength(rBuf, 6);
  ps := TProxyStream.Create(TEST_CAPACITY);
  try
    ps.Write(wBuf[0], 6);
    n := ps.Read(rBuf[0], 6);
    Assert.AreEqual(6, Integer(n), 'Read should return the number of bytes read');
  finally
    ps.Free;
  end;
end;

procedure TQuickStreamsTests.Test_Seek_FromBeginning;
var
  ps  : TProxyStream;
  pos : Int64;
begin
  ps := TProxyStream.Create(TEST_CAPACITY);
  try
    pos := ps.Seek(100, soBeginning);
    Assert.AreEqual(Int64(100), pos, 'Seek from beginning should return the offset');
  finally
    ps.Free;
  end;
end;

procedure TQuickStreamsTests.Test_Seek_FromEnd;
var
  ps  : TProxyStream;
  pos : Int64;
begin
  ps := TProxyStream.Create(TEST_CAPACITY);
  try
    pos := ps.Seek(0, soEnd);
    Assert.AreEqual(Int64(TEST_CAPACITY), pos, 'Seek from end with offset 0 should return stream size');
  finally
    ps.Free;
  end;
end;

procedure TQuickStreamsTests.Test_Seek_FromCurrent;
var
  ps  : TProxyStream;
  pos : Int64;
begin
  ps := TProxyStream.Create(TEST_CAPACITY);
  try
    // Internal fPosition starts at 0
    pos := ps.Seek(50, soCurrent);
    Assert.AreEqual(Int64(50), pos, 'Seek from current should add offset to position');
  finally
    ps.Free;
  end;
end;

procedure TQuickStreamsTests.Test_WriteRead_Threaded;
var
  ps      : TProxyStream;
  wBuf, rBuf : TBytes;
  reader  : ITask;
  readOk  : Boolean;
begin
  // TProxyStream synchronises reads and writes via lightweight events.
  // Write in the current thread, read in a background task.
  SetLength(wBuf, 4);
  wBuf[0] := 7; wBuf[1] := 8; wBuf[2] := 9; wBuf[3] := 10;
  SetLength(rBuf, 4);
  readOk := False;

  ps := TProxyStream.Create(TEST_CAPACITY);
  try
    reader := TTask.Run(procedure
    begin
      ps.Read(rBuf[0], 4);
      readOk := True;
    end);

    // Small delay then write
    Sleep(50);
    ps.Write(wBuf[0], 4);

    reader.Wait(5000);

    Assert.IsTrue(readOk, 'Threaded read should complete after write');
    Assert.AreEqual(Integer(wBuf[0]), Integer(rBuf[0]), 'Byte 0 should match in threaded scenario');
  finally
    ps.Free;
  end;
end;

procedure TQuickStreamsTests.Test_ReadTimeout_RaisesException;
var
  ps   : TProxyStream;
  rBuf : TBytes;
begin
  SetLength(rBuf, 4);
  ps := TProxyStream.Create(TEST_CAPACITY);
  try
    ps.ReadTimeout := 200; // very short timeout - no writer will signal
    Assert.WillRaise(
      procedure begin ps.Read(rBuf[0], 4); end,
      Exception,
      'Read should raise when timeout expires with no data');
  finally
    ps.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickStreamsTests);

end.
