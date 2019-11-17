program CompressionTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Quick.Commons,
  Quick.Console,
  Quick.Compression.LZO;

const
  str = 'This is a test compression with LZO algorithm. Repeated words are compressed' +
        'Test: Compression, Compression, Compression, Compression, Compression';
var
  lzo : TLZOCompressor;
  compStr : string;
  decompStr : string;

begin
  try
    ReportMemoryLeaksOnShutdown := True;
    lzo := TLZOCompressor.Create;
    coutFmt('Original: %s',[str],etInfo);
    coutFmt('Original size: %d',[str.Length],etInfo);
    compStr := lzo.Compress(str);
    coutFmt('Compressed: %s',[compStr],etInfo);
    coutFmt('Compressed size: %d',[compStr.Length],etInfo);
    decompStr := lzo.Decompress(compStr);
    coutFmt('Decompressed: %s',[decompStr],etSuccess);
    cout('Uncompressed size: %d',[decompStr.Length],etInfo);
    ConsoleWaitForEnterKey;
    lzo.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
