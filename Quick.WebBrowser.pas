{ ***************************************************************************

  Copyright (c) 2014-2017 Kike Pérez

  Unit        : Quick.WebBrowser
  Description : Web browser functions
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 10/02/2014
  Modified    : 03/11/2016

  This file is part of QuickLib: https://github.com/exilon/QuickLib

  Uses code parts of: Thomas Stutz

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

unit Quick.WebBrowser;

interface

  uses
    Classes,
    Forms,
    System.SysUtils,
    SHDocVw,
    MSHTML,
    ActiveX,
    Vcl.Graphics,
    System.Variants,
    Winapi.WinInet;

  procedure WB_SetBorderColor(Sender: TObject; BorderColor: String);
  procedure WB_SetBorderStyle(Sender: TObject; BorderStyle: String);
  procedure WB_Set3DBorderStyle(Sender: TObject; bValue: Boolean);
  procedure WB_SetDessignMode(Sender : TObject; bEnabled : Boolean);
  procedure WB_SetFontColor(Sender : TObject; aColor : TColor);
  procedure WB_SetFontBold(Sender : TObject; bEnabled : Boolean);
  procedure WB_SetFontItalic(Sender : TObject; bEnabled : Boolean);
  procedure WB_SetFontUnderline(Sender : TObject; bEnabled : Boolean);
  procedure WB_SetFontFace(Sender : TObject; cFontName : string);
  procedure WB_SetFontSize(Sender : TObject; nFontSize : Integer);
  procedure WB_InsertImage(Sender : TObject);
  procedure WBLoadHTML(const WebBrowser: TWebBrowser; HTMLCode: string) ;
  function GetHTML(const wbBrowser : TWebBrowser) : string;
  function GetHTML2(const wbBrowser : TWebBrowser) : string;
  function GetPlainText(const Html: string): string;
  function GetWebBrowserHTML(const WebBrowser: TWebBrowser): String;
  procedure DeleteIECacheAll;
  procedure DeleteIECache(filenameWildcard : string);

implementation

procedure WB_SetBorderColor(Sender: TObject; BorderColor: String);
{
  BorderColor: Can be specified in HTML pages in two ways.
               1) by using a color name (red, green, gold, firebrick, ...)
               2) or by using numbers to denote an RGB color value. (#9400D3, #00CED1,...)

  See: http://msdn.microsoft.com/library/default.asp?url=/workshop/author/dhtml/reference/properties/borderstyle.asp
}

var
  Document : IHTMLDocument2;
  Element : IHTMLElement;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  if Assigned(Document) then
  begin
    Element := Document.Body;
    if Element <> nil then
    begin
      Element.Style.BorderColor := BorderColor;
    end;
  end;
end;

procedure WB_SetBorderStyle(Sender: TObject; BorderStyle: String);
{
  BorderStyle values:

  'none'         No border is drawn
  'dotted'       Border is a dotted line. (as of IE 5.5)
  'dashed'       Border is a dashed line. (as of IE 5.5)
  'solid'        Border is a solid line.
  'double'       Border is a double line
  'groove'       3-D groove is drawn //Está se ve perfecto en Windows 7 y Windows 8
  'ridge'        3-D ridge is drawn
  'inset'        3-D inset is drawn
  'window-inset' Border is the same as inset, but is surrounded by an additional single line
  'outset'       3-D outset is drawn

  See: http://msdn.microsoft.com/library/default.asp?url=/workshop/author/dhtml/reference/properties/borderstyle.asp
}

var
  Document : IHTMLDocument2;
  Element : IHTMLElement;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  if Assigned(Document) then
  begin
    Element := Document.Body;
    if Element <> nil then
    begin
      Element.Style.BorderStyle := BorderStyle;
    end;
  end;
end;

procedure WB_Set3DBorderStyle(Sender: TObject; bValue: Boolean);
{
  bValue: True: Show a 3D border style
          False: Show no border
}
var
  Document : IHTMLDocument2;
  Element : IHTMLElement;
  StrBorderStyle: string;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  if Assigned(Document) then
  begin
    Element := Document.Body;
    if Element <> nil then
    begin
      case BValue of
        False: StrBorderStyle := 'none';
        True: StrBorderStyle := '';
      end;
      Element.Style.BorderStyle := StrBorderStyle;
    end;
  end;
end;

procedure WB_SetDessignMode(Sender : TObject; bEnabled : Boolean);
begin
  ((Sender as TWebBrowser).Document as IHTMLDocument2).designMode := 'On';
end;

procedure WB_SetFontColor(Sender : TObject; aColor : TColor);
var
  Document : IHTMLDocument2;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  Document.execCommand('ForeColor',True,AColor);
end;

procedure WB_SetFontBold(Sender : TObject; bEnabled : Boolean);
var
  Document : IHTMLDocument2;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  Document.execCommand('Bold',False,bEnabled);
end;

procedure WB_SetFontItalic(Sender : TObject; bEnabled : Boolean);
var
  Document : IHTMLDocument2;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  Document.execCommand('Italic',False,bEnabled);
end;

procedure WB_SetFontUnderline(Sender : TObject; bEnabled : Boolean);
var
  Document : IHTMLDocument2;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  Document.execCommand('Underline',False,bEnabled);
end;

procedure WB_SetFontFace(Sender : TObject; cFontName : string);
var
  Document : IHTMLDocument2;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  Document.execCommand('FontName',False,cFontName);
end;

procedure WB_SetFontSize(Sender : TObject; nFontSize : Integer);
var
  Document : IHTMLDocument2;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  Document.execCommand('FontSize',False,nFontSize);
end;

procedure WB_InsertImage(Sender : TObject);
var
  Document : IHTMLDocument2;
begin
  Document := TWebBrowser(Sender).Document as IHTMLDocument2;
  Document.execCommand('InsertImage',True,0);
end;

procedure WBLoadHTML(const WebBrowser: TWebBrowser; HTMLCode: string) ;
var
   sl: TStringList;
   ms: TMemoryStream;
begin
   WebBrowser.Navigate('about:blank') ;
   while WebBrowser.ReadyState < READYSTATE_INTERACTIVE do
    Application.ProcessMessages;

   if Assigned(WebBrowser.Document) then
   begin
     sl := TStringList.Create;
     try
       ms := TMemoryStream.Create;
       try
         sl.Text := HTMLCode;
         sl.SaveToStream(ms) ;
         ms.Seek(0, 0) ;
         (WebBrowser.Document as IPersistStreamInit).Load(TStreamAdapter.Create(ms)) ;
       finally
         ms.Free;
       end;
     finally
       sl.Free;
     end;
   end;
end;

function GetHTML(const wbBrowser : TWebBrowser) : string;
var
    iall : IHTMLElement;
begin
  (wbBrowser.Document as IHTMLDocument2).designMode := 'Off';
  Result := (wbBrowser.Document as IHTMLDocument2).body.toString;
  exit;
  if Assigned(wbBrowser.Document) then
   begin
     iall := (wbBrowser.Document as IHTMLDocument2).body;

     while iall.parentElement <> nil do
     begin
       iall := iall.parentElement;
     end;
     Result := iall.outerHTML;
   end;
end;

function GetHTML2(const wbBrowser : TWebBrowser) : string;
var
    Doc: IHTMLDocument2;
    BodyElement: IHTMLElement;
begin
    Assert(Assigned(wbBrowser.Document));

    if wbBrowser.Document.QueryInterface(IHTMLDocument2, Doc) = S_OK then begin
        BodyElement := Doc.body;
        if Assigned(BodyElement) then
        begin
            result := '<html>' + BodyElement.outerHTML + '</html>';
        end;
    end;
end;

function GetWebBrowserHTML(const WebBrowser: TWebBrowser): String;
var
  LStream: TStringStream;
  Stream : IStream;
  LPersistStreamInit : IPersistStreamInit;
begin
  if not Assigned(WebBrowser.Document) then exit;
  LStream := TStringStream.Create('');
  try
    LPersistStreamInit := WebBrowser.Document as IPersistStreamInit;
    Stream := TStreamAdapter.Create(LStream,soReference);
    LPersistStreamInit.Save(Stream,true);
    result := LStream.DataString;
  finally
    LStream.Free();
  end;
end;

function GetPlainText(const Html: string): string;
var
DummyWebBrowser: TWebBrowser;
Document       : IHtmlDocument2;
DummyVar       : Variant;
begin
   Result := '';
   DummyWebBrowser := TWebBrowser.Create(nil);
   try
     //open an blank page to create a IHtmlDocument2 instance
     DummyWebBrowser.Navigate('about:blank');
     Document := DummyWebBrowser.Document as IHtmlDocument2;
     if (Assigned(Document)) then //Check the Document
     begin
       DummyVar      := VarArrayCreate([0, 0], varVariant); //Create a variant array to write the html code to the  IHtmlDocument2
       DummyVar[0]   := Html; //assign the html code to the variant array
       Document.Write(PSafeArray(TVarData(DummyVar).VArray)); //set the html in the document
       Document.Close;
       Result :=(Document.body as IHTMLBodyElement).createTextRange.text;//get the plain text
     end;
   finally
     DummyWebBrowser.Free;
   end;
end;

procedure DeleteIECacheAll;
var
  lpEntryInfo: PInternetCacheEntryInfo;
  hCacheDir: LongWord;
  dwEntrySize: LongWord;
begin
  dwEntrySize := 0;
  FindFirstUrlCacheEntry(nil, TInternetCacheEntryInfo(nil^), dwEntrySize);
  GetMem(lpEntryInfo, dwEntrySize);
  if dwEntrySize > 0 then lpEntryInfo^.dwStructSize := dwEntrySize;
  hCacheDir := FindFirstUrlCacheEntry(nil, lpEntryInfo^, dwEntrySize);
  if hCacheDir <> 0 then
  begin
    repeat
      DeleteUrlCacheEntry(lpEntryInfo^.lpszSourceUrlName);
      FreeMem(lpEntryInfo, dwEntrySize);
      dwEntrySize := 0;
      FindNextUrlCacheEntry(hCacheDir, TInternetCacheEntryInfo(nil^), dwEntrySize);
      GetMem(lpEntryInfo, dwEntrySize);
      if dwEntrySize > 0 then lpEntryInfo^.dwStructSize := dwEntrySize;
    until not FindNextUrlCacheEntry(hCacheDir, lpEntryInfo^, dwEntrySize);
  end;
  FreeMem(lpEntryInfo, dwEntrySize);
  FindCloseUrlCache(hCacheDir);
end;

//DeleteIECache('?M=P');
procedure DeleteIECache(filenameWildcard : string);
var
   lpEntryInfo: PInternetCacheEntryInfo;
   hCacheDir: LongWord;
   dwEntrySize: LongWord;
begin
   dwEntrySize := 0;
   FindFirstUrlCacheEntry(nil, TInternetCacheEntryInfo(nil^), dwEntrySize) ;
   GetMem(lpEntryInfo, dwEntrySize) ;
   if dwEntrySize > 0 then lpEntryInfo^.dwStructSize := dwEntrySize;
   hCacheDir := FindFirstUrlCacheEntry(nil, lpEntryInfo^, dwEntrySize) ;
   if hCacheDir <> 0 then
   begin
     repeat
       if Pos(filenameWildcard, lpEntryInfo^.lpszSourceUrlName) > 0 then begin
         DeleteUrlCacheEntry(lpEntryInfo^.lpszSourceUrlName) ;
       end;
       FreeMem(lpEntryInfo, dwEntrySize) ;
       dwEntrySize := 0;
       FindNextUrlCacheEntry(hCacheDir, TInternetCacheEntryInfo(nil^), dwEntrySize) ;
       GetMem(lpEntryInfo, dwEntrySize) ;
       if dwEntrySize > 0 then lpEntryInfo^.dwStructSize := dwEntrySize;
     until not FindNextUrlCacheEntry(hCacheDir, lpEntryInfo^, dwEntrySize) ;
   end;
   FreeMem(lpEntryInfo, dwEntrySize) ;
   FindCloseUrlCache(hCacheDir) ;
end;


end.
