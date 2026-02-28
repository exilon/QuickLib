unit Quick.Template.Tests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  Quick.Template;

type
  [TestFixture]
  TQuickTemplateTests = class(TObject)
  public
    [Test]
    procedure Test_Replace_WithDictionary_BasicTokens;
    [Test]
    procedure Test_Replace_WithDictionary_MultipleTokens;
    [Test]
    procedure Test_Replace_WithDictionary_TokenNotFound_ReturnsTokenWithMark;
    [Test]
    procedure Test_Replace_WithDictionary_EmptyTemplate;
    [Test]
    procedure Test_Replace_WithDelegate_BasicToken;
    [Test]
    procedure Test_Replace_WithDelegate_MultipleTokens;
    [Test]
    procedure Test_Replace_WithDelegate_UnknownToken;
    [Test]
    procedure Test_Replace_CustomQuoteChars;
    [Test]
    procedure Test_Replace_NoTokensInTemplate;
    [Test]
    procedure Test_Replace_ConsecutiveTokens;
    [Test]
    procedure Test_Constructor_NilDictionary_Raises;
    [Test]
    procedure Test_Constructor_EmptyQuoteBegin_Raises;
    [Test]
    procedure Test_Constructor_EmptyQuoteEnd_Raises;
    [Test]
    procedure Test_Constructor_NilDelegate_Raises;
  end;

implementation

procedure TQuickTemplateTests.Test_Replace_WithDictionary_BasicTokens;
var
  dict: TDictionary<string, string>;
  tmpl: TStringTemplate;
begin
  dict := TDictionary<string, string>.Create;
  try
    dict.Add('Name', 'John');
    dict.Add('Age', '30');
    tmpl := TStringTemplate.Create('{{', '}}', dict);
    try
      Assert.AreEqual('Hello John, you are 30 years old.', tmpl.Replace('Hello {{Name}}, you are {{Age}} years old.'));
    finally
      tmpl.Free;
    end;
  finally
    dict.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Replace_WithDictionary_MultipleTokens;
var
  dict: TDictionary<string, string>;
  tmpl: TStringTemplate;
  result: string;
begin
  dict := TDictionary<string, string>.Create;
  try
    dict.Add('First', 'Quick');
    dict.Add('Second', 'Lib');
    dict.Add('Third', 'Delphi');
    tmpl := TStringTemplate.Create('{{', '}}', dict);
    try
      result := tmpl.Replace('{{First}}{{Second}} for {{Third}}');
      Assert.AreEqual('QuickLib for Delphi', result, 'Multiple tokens should all be replaced');
    finally
      tmpl.Free;
    end;
  finally
    dict.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Replace_WithDictionary_TokenNotFound_ReturnsTokenWithMark;
var
  dict: TDictionary<string, string>;
  tmpl: TStringTemplate;
  result: string;
begin
  dict := TDictionary<string, string>.Create;
  try
    dict.Add('Known', 'value');
    tmpl := TStringTemplate.Create('{{', '}}', dict);
    try
      result := tmpl.Replace('{{Known}} and {{Unknown}}');
      Assert.IsTrue(result.Contains('{{Unknown?}}'), 'Unknown token should be returned with ? marker');
    finally
      tmpl.Free;
    end;
  finally
    dict.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Replace_WithDictionary_EmptyTemplate;
var
  dict: TDictionary<string, string>;
  tmpl: TStringTemplate;
begin
  dict := TDictionary<string, string>.Create;
  try
    dict.Add('Key', 'val');
    tmpl := TStringTemplate.Create('{{', '}}', dict);
    try
      Assert.AreEqual('', tmpl.Replace(''), 'Empty template should produce empty result');
    finally
      tmpl.Free;
    end;
  finally
    dict.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Replace_WithDelegate_BasicToken;
var
  tmpl: TStringTemplate;
begin
  tmpl := TStringTemplate.Create('{{', '}}',
    function(const aToken: string): string
    begin
      if aToken = 'Greeting' then Result := 'Hello World'
      else Result := '';
    end);
  try
    Assert.AreEqual('Hello World!', tmpl.Replace('{{Greeting}}!'));
  finally
    tmpl.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Replace_WithDelegate_MultipleTokens;
var
  tmpl: TStringTemplate;
  result: string;
begin
  tmpl := TStringTemplate.Create('{{', '}}',
    function(const aToken: string): string
    begin
      if aToken = 'User' then Result := 'Alice'
      else if aToken = 'Role' then Result := 'Admin'
      else Result := '';
    end);
  try
    result := tmpl.Replace('User {{User}} has role {{Role}}.');
    Assert.AreEqual('User Alice has role Admin.', result, 'Delegate should replace all tokens');
  finally
    tmpl.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Replace_WithDelegate_UnknownToken;
var
  tmpl: TStringTemplate;
  result: string;
begin
  tmpl := TStringTemplate.Create('{{', '}}',
    function(const aToken: string): string
    begin
      Result := ''; // always returns empty → triggers ? marker
    end);
  try
    result := tmpl.Replace('Value: {{Missing}}');
    Assert.IsTrue(result.Contains('{{Missing?}}'), 'Unknown token should use ? marker');
  finally
    tmpl.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Replace_CustomQuoteChars;
var
  dict: TDictionary<string, string>;
  tmpl: TStringTemplate;
begin
  dict := TDictionary<string, string>.Create;
  try
    dict.Add('Color', 'Blue');
    tmpl := TStringTemplate.Create('<%', '%>', dict);
    try
      Assert.AreEqual('My favorite color is Blue.', tmpl.Replace('My favorite color is <%Color%>.'));
    finally
      tmpl.Free;
    end;
  finally
    dict.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Replace_NoTokensInTemplate;
var
  dict: TDictionary<string, string>;
  tmpl: TStringTemplate;
begin
  dict := TDictionary<string, string>.Create;
  try
    dict.Add('Key', 'val');
    tmpl := TStringTemplate.Create('{{', '}}', dict);
    try
      Assert.AreEqual('No tokens here.', tmpl.Replace('No tokens here.'));
    finally
      tmpl.Free;
    end;
  finally
    dict.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Replace_ConsecutiveTokens;
var
  dict: TDictionary<string, string>;
  tmpl: TStringTemplate;
begin
  dict := TDictionary<string, string>.Create;
  try
    dict.Add('A', 'Hello');
    dict.Add('B', 'World');
    tmpl := TStringTemplate.Create('{{', '}}', dict);
    try
      Assert.AreEqual('HelloWorld', tmpl.Replace('{{A}}{{B}}'), 'Consecutive tokens should be replaced without gap');
    finally
      tmpl.Free;
    end;
  finally
    dict.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Constructor_NilDictionary_Raises;
begin
  Assert.WillRaise(
    procedure
    var tmpl: TStringTemplate;
    begin
      tmpl := TStringTemplate.Create('{{', '}}', TDictionary<string,string>(nil));
      tmpl.Free;
    end,
    EStringTemplateError,
    'Nil dictionary should raise EStringTemplateError'
  );
end;

procedure TQuickTemplateTests.Test_Constructor_EmptyQuoteBegin_Raises;
var
  dict: TDictionary<string, string>;
begin
  dict := TDictionary<string, string>.Create;
  try
    Assert.WillRaise(
      procedure
      var tmpl: TStringTemplate;
      begin
        tmpl := TStringTemplate.Create('', '}}', dict);
        tmpl.Free;
      end,
      EStringTemplateError,
      'Empty QuoteBegin should raise EStringTemplateError'
    );
  finally
    dict.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Constructor_EmptyQuoteEnd_Raises;
var
  dict: TDictionary<string, string>;
begin
  dict := TDictionary<string, string>.Create;
  try
    Assert.WillRaise(
      procedure
      var tmpl: TStringTemplate;
      begin
        tmpl := TStringTemplate.Create('{{', '', dict);
        tmpl.Free;
      end,
      EStringTemplateError,
      'Empty QuoteEnd should raise EStringTemplateError'
    );
  finally
    dict.Free;
  end;
end;

procedure TQuickTemplateTests.Test_Constructor_NilDelegate_Raises;
begin
  Assert.WillRaise(
    procedure
    var tmpl: TStringTemplate;
    begin
      tmpl := TStringTemplate.Create('{{', '}}', TTokenFunc(nil));
      tmpl.Free;
    end,
    EStringTemplateError,
    'Nil delegate should raise EStringTemplateError'
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickTemplateTests);
end.
