{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.HttpServer.Types
  Description : Http Server Types
  Author      : Kike Pérez
  Version     : 1.8
  Created     : 30/08/2019
  Modified    : 26/03/2020

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

unit Quick.HttpServer.Types;

{$i QuickLib.inc}

interface

uses
  SysUtils,
  Generics.Collections;

type

  EControlledException = class(Exception)
  private
    fCallerClass : TClass;
  public
    constructor Create(aCaller : TObject; const aMessage : string);
    property CallerClass : TClass read fCallerClass write fCallerClass;
  end;

  TController = class(TInterfacedObject);

  TControllerClass = class of TController;

  TMethodVerb = (mUNKNOWN, mGET, mHEAD, mPOST, mPUT, mDELETE, mOPTIONS, mTRACE, mPATCH);

  TMethodVerbs = set of TMethodVerb;

  THttpStatusCode = (Accepted  = 202,
                     Ambiguous = 300,
                     BadGateway = 502,
                     BadRequest = 400,
                     Conflict = 409,
                     Continue = 100,
                     Created = 201,
                     ExpectationFailed = 417,
                     Forbidden = 403,
                     Found = 302,
                     GatewayTimeout = 504,
                     Gone = 410,
                     HttpVersionNotSupported = 505,
                     InternalServerError = 500,
                     LengthRequired = 411,
                     MethodNotAllowed = 405,
                     Moved = 301,
                     MovedPermanently = 301,
                     MultipleChoices = 300,
                     NoContent = 204,
                     NonAuthoritativeInformation = 203,
                     NotAcceptable = 406,
                     NotFound = 404,
                     NotImplemented = 501,
                     NotModified = 304,
                     OK = 200,
                     PartialContent = 206,
                     PaymentRequired = 402,
                     PreconditionFailed = 412,
                     ProxyAuthenticationRequired = 407,
                     Redirect = 302,
                     RedirectKeepVerb = 307,
                     RedirectMethod = 303,
                     RequestedRangeNotSatisfiable = 416,
                     RequestEntityTooLarge = 413,
                     RequestTimeout = 408,
                     RequestUriTooLong = 414,
                     ResetContent = 205,
                     SeeOther = 303,
                     ServiceUnavailable = 503,
                     SwitchingProtocols = 101,
                     TemporaryRedirect = 307,
                     Unauthorized = 401,
                     UnsupportedMediaType = 415,
                     Unused = 306,
                     UpgradeRequired = 426,
                     UseProxy = 305);

  TMIMETypes = class
  private
    fMIMEList : TDictionary<string,string>;
    procedure FillMIME;
  public
    constructor Create;
    destructor Destroy; override;
    function GetExtensionMIMEType(const aExtension : string) : string;
    function GetFileMIMEType(const aFilename : string) : string;
    procedure AddMIME(const aExtension, aMIMEType : string);
  end;

var
  MIMETypes : TMIMETypes;

const
  MethodVerbStr: array[0..Ord(High(TMethodVerb))] of string = ('UNKNOWN','HEAD','GET','POST','PUT','DELETE','OPTIONS','TRACE','PATCH');

implementation

procedure TMIMETypes.AddMIME(const aExtension, aMIMEType: string);
begin
  if not fMIMEList.ContainsKey(aExtension.ToLower) then fMIMEList.Add(aExtension.ToLower,aMIMEType.ToLower);
end;

constructor TMIMETypes.Create;
begin
  fMIMEList := TDictionary<string,string>.Create(375);
  FillMIME;
end;

destructor TMIMETypes.Destroy;
begin
  fMIMEList.Free;
  inherited;
end;

procedure TMIMETypes.FillMIME;
begin
  { Animation }
  fMIMEList.Add('.nml','animation/narrative');

  { Audio }
  fMIMEList.Add('.aac','audio/mp4');
  fMIMEList.Add('.aif','audio/x-aiff');
  fMIMEList.Add('.aifc','audio/x-aiff');
  fMIMEList.Add('.aiff','audio/x-aiff');
  fMIMEList.Add('.au','audio/basic');
  fMIMEList.Add('.gsm','audio/x-gsm');
  fMIMEList.Add('.kar','audio/midi');
  fMIMEList.Add('.m3u','audio/mpegurl');
  fMIMEList.Add('.m4a','audio/x-mpg');
  fMIMEList.Add('.mid','audio/midi');
  fMIMEList.Add('.midi','audio/midi');
  fMIMEList.Add('.mpega','audio/x-mpg');
  fMIMEList.Add('.mp2','audio/x-mpg');
  fMIMEList.Add('.mp3','audio/x-mpg');
  fMIMEList.Add('.mpga','audio/x-mpg');
  fMIMEList.Add('.pls','audio/x-scpls');
  fMIMEList.Add('.qcp','audio/vnd.qcelp');
  fMIMEList.Add('.ra','audio/x-realaudio');
  fMIMEList.Add('.ram','audio/x-pn-realaudio');
  fMIMEList.Add('.rm','audio/x-pn-realaudio');
  fMIMEList.Add('.sd2','audio/x-sd2');
  fMIMEList.Add('.sid','audio/prs.sid');
  fMIMEList.Add('.snd','audio/basic');
  fMIMEList.Add('.wav','audio/x-wav');
  fMIMEList.Add('.wax','audio/x-ms-wax');
  fMIMEList.Add('.wma','audio/x-ms-wma');
  fMIMEList.Add('.mjf','audio/x-vnd.AudioExplosion.MjuiceMediaFile');

  { Image }
  fMIMEList.Add('.art','image/x-jg');
  fMIMEList.Add('.bmp','image/bmp');
  fMIMEList.Add('.cdr','image/x-coreldraw');
  fMIMEList.Add('.cdt','image/x-coreldrawtemplate');
  fMIMEList.Add('.cpt','image/x-corelphotopaint');
  fMIMEList.Add('.djv','image/vnd.djvu');
  fMIMEList.Add('.djvu','image/vnd.djvu');
  fMIMEList.Add('.gif','image/gif');
  fMIMEList.Add('.ief','image/ief');
  fMIMEList.Add('.ico','image/x-icon');
  fMIMEList.Add('.jng','image/x-jng');
  fMIMEList.Add('.jpg','image/jpeg');
  fMIMEList.Add('.jpeg','image/jpeg');
  fMIMEList.Add('.jpe','image/jpeg');
  fMIMEList.Add('.pat','image/x-coreldrawpattern');
  fMIMEList.Add('.pcx','image/pcx');
  fMIMEList.Add('.pbm','image/x-portable-bitmap');
  fMIMEList.Add('.pgm','image/x-portable-graymap');
  fMIMEList.Add('.pict','image/x-pict');
  fMIMEList.Add('.png','image/x-png');
  fMIMEList.Add('.pnm','image/x-portable-anymap');
  fMIMEList.Add('.pntg','image/x-macpaint');
  fMIMEList.Add('.ppm','image/x-portable-pixmap');
  fMIMEList.Add('.psd','image/x-psd');
  fMIMEList.Add('.qtif','image/x-quicktime');
  fMIMEList.Add('.ras','image/x-cmu-raster');
  fMIMEList.Add('.rf','image/vnd.rn-realflash');
  fMIMEList.Add('.rgb','image/x-rgb');
  fMIMEList.Add('.rp','image/vnd.rn-realpix');
  fMIMEList.Add('.sgi','image/x-sgi');
  fMIMEList.Add('.svg','image/svg+xml');
  fMIMEList.Add('.svgz','image/svg+xml');
  fMIMEList.Add('.targa','image/x-targa');
  fMIMEList.Add('.tif','image/x-tiff');
  fMIMEList.Add('.webp','image/webp');
  fMIMEList.Add('.xbm','image/xbm');
  fMIMEList.Add('.xpm','image/x-xpixmap');
  fMIMEList.Add('.xwd','image/x-xwindowdump');

  { Text }
  fMIMEList.Add('.323','text/h323');
  fMIMEList.Add('.xml','text/xml');
  fMIMEList.Add('.uls','text/iuls');
  fMIMEList.Add('.txt','text/plain');
  fMIMEList.Add('.rtx','text/richtext');
  fMIMEList.Add('.wsc','text/scriptlet');
  fMIMEList.Add('.rt','text/vnd.rn-realtext');
  fMIMEList.Add('.htt','text/webviewhtml');
  fMIMEList.Add('.htc','text/x-component');
  fMIMEList.Add('.vcf','text/x-vcard');

  { Video }
  fMIMEList.Add('.asf','video/x-ms-asf');
  fMIMEList.Add('.asx','video/x-ms-asf');
  fMIMEList.Add('.avi','video/x-msvideo');
  fMIMEList.Add('.dl','video/dl');
  fMIMEList.Add('.dv','video/dv');
  fMIMEList.Add('.flc','video/flc');
  fMIMEList.Add('.fli','video/fli');
  fMIMEList.Add('.gl','video/gl');
  fMIMEList.Add('.lsf','video/x-la-asf');
  fMIMEList.Add('.lsx','video/x-la-asf');
  fMIMEList.Add('.mng','video/x-mng');
  fMIMEList.Add('.mp4','video/mpeg');
  fMIMEList.Add('.mpeg','video/x-mpeg2a');
  fMIMEList.Add('.mpa','video/mpeg');
  fMIMEList.Add('.mpe','video/mpeg');
  fMIMEList.Add('.mpg','video/mpeg');
  fMIMEList.Add('.ogv','video/ogg');
  fMIMEList.Add('.moov','video/quicktime');
  fMIMEList.Add('.mov','video/quicktime');
  fMIMEList.Add('.mxu','video/vnd.mpegurl');
  fMIMEList.Add('.qt','video/quicktime');
  fMIMEList.Add('.qtc','video/x-qtc');
  fMIMEList.Add('.rv','video/vnd.rn-realvideo');
  fMIMEList.Add('.ivf','video/x-ivf');
  fMIMEList.Add('.webm','video/webm');
  fMIMEList.Add('.wm','video/x-ms-wm');
  fMIMEList.Add('.wmp','video/x-ms-wmp');
  fMIMEList.Add('.wmv','video/x-ms-wmv');
  fMIMEList.Add('.wmx','video/x-ms-wmx');
  fMIMEList.Add('.wvx','video/x-ms-wvx');
  fMIMEList.Add('.rms','video/vnd.rn-realvideo-secure');
  fMIMEList.Add('.movie','video/x-sgi-movie');

  { Application }
  fMIMEList.Add('.7z','application/x-7z-compressed');
  fMIMEList.Add('.a','application/x-archive');
  fMIMEList.Add('.aab','application/x-authorware-bin');
  fMIMEList.Add('.aam','application/x-authorware-map');
  fMIMEList.Add('.aas','application/x-authorware-seg');
  fMIMEList.Add('.abw','application/x-abiword');
  fMIMEList.Add('.ace','application/x-ace-compressed');
  fMIMEList.Add('.ai','application/postscript');
  fMIMEList.Add('.alz','application/x-alz-compressed');
  fMIMEList.Add('.ani','application/x-navi-animation');
  fMIMEList.Add('.arj','application/x-arj');
  fMIMEList.Add('.bat','application/x-msdos-program');
  fMIMEList.Add('.bcpio','application/x-bcpio');
  fMIMEList.Add('.boz','application/x-bzip2');
  fMIMEList.Add('.bz','application/x-bzip');
  fMIMEList.Add('.bz2','application/x-bzip2');
  fMIMEList.Add('.cab','application/vnd.ms-cab-compressed');
  fMIMEList.Add('.cat','application/vnd.ms-pki.seccat');
  fMIMEList.Add('.ccn','application/x-cnc');
  fMIMEList.Add('.cco','application/x-cocoa');
  fMIMEList.Add('.cdf','application/x-cdf');
  fMIMEList.Add('.cer','application/x-x509-ca-cert');
  fMIMEList.Add('.chm','application/vnd.ms-htmlhelp');
  fMIMEList.Add('.chrt','application/vnd.kde.kchart');
  fMIMEList.Add('.cil','application/vnd.ms-artgalry');
  fMIMEList.Add('.class','application/java-vm');
  fMIMEList.Add('.com','application/x-msdos-program');
  fMIMEList.Add('.clp','application/x-msclip');
  fMIMEList.Add('.cpio','application/x-cpio');
  fMIMEList.Add('.cqk','application/x-calquick');
  fMIMEList.Add('.crd','application/x-mscardfile');
  fMIMEList.Add('.crl','application/pkix-crl');
  fMIMEList.Add('.csh','application/x-csh');
  fMIMEList.Add('.dar','application/x-dar');
  fMIMEList.Add('.dbf','application/x-dbase');
  fMIMEList.Add('.dcr','application/x-director');
  fMIMEList.Add('.deb','application/x-debian-package');
  fMIMEList.Add('.dir','application/x-director');
  fMIMEList.Add('.dist','vnd.apple.installer+xml');
  fMIMEList.Add('.distz','vnd.apple.installer+xml');
  fMIMEList.Add('.dll','application/x-msdos-program');
  fMIMEList.Add('.dmg','application/x-apple-diskimage');
  fMIMEList.Add('.doc','application/msword');
  fMIMEList.Add('.dot','application/msword');
  fMIMEList.Add('.dvi','application/x-dvi');
  fMIMEList.Add('.dxr','application/x-director');
  fMIMEList.Add('.ebk','application/x-expandedbook');
  fMIMEList.Add('.eps','application/postscript');
  fMIMEList.Add('.evy','application/envoy');
  fMIMEList.Add('.exe','application/x-msdos-program');
  fMIMEList.Add('.fdf','application/vnd.fdf');
  fMIMEList.Add('.fif','application/fractals');
  fMIMEList.Add('.flm','application/vnd.kde.kivio');
  fMIMEList.Add('.fml','application/x-file-mirror-list');
  fMIMEList.Add('.gzip','application/x-gzip');
  fMIMEList.Add('.gnumeric','application/x-gnumeric');
  fMIMEList.Add('.gtar','application/x-gtar');
  fMIMEList.Add('.gz','application/x-gzip');
  fMIMEList.Add('.hdf','application/x-hdf');
  fMIMEList.Add('.hlp','application/winhlp');
  fMIMEList.Add('.hpf','application/x-icq-hpf');
  fMIMEList.Add('.hqx','application/mac-binhex40');
  fMIMEList.Add('.hta','application/hta');
  fMIMEList.Add('.ims','application/vnd.ms-ims');
  fMIMEList.Add('.ins','application/x-internet-signup');
  fMIMEList.Add('.iii','application/x-iphone');
  fMIMEList.Add('.iso','application/x-iso9660-image');
  fMIMEList.Add('.jar','application/java-archive');
  fMIMEList.Add('.karbon','application/vnd.kde.karbon');
  fMIMEList.Add('.kfo','application/vnd.kde.kformula');
  fMIMEList.Add('.kon','application/vnd.kde.kontour');
  fMIMEList.Add('.kpr','application/vnd.kde.kpresenter');
  fMIMEList.Add('.kpt','application/vnd.kde.kpresenter');
  fMIMEList.Add('.kwd','application/vnd.kde.kword');
  fMIMEList.Add('.kwt','application/vnd.kde.kword');
  fMIMEList.Add('.latex','application/x-latex');
  fMIMEList.Add('.lha','application/x-lzh');
  fMIMEList.Add('.lcc','application/fastman');
  fMIMEList.Add('.lrm','application/vnd.ms-lrm');
  fMIMEList.Add('.lz','application/x-lzip');
  fMIMEList.Add('.lzh','application/x-lzh');
  fMIMEList.Add('.lzma','application/x-lzma');
  fMIMEList.Add('.lzo','application/x-lzop');
  fMIMEList.Add('.lzx','application/x-lzx');
  fMIMEList.Add('.m13','application/x-msmediaview');
  fMIMEList.Add('.m14','application/x-msmediaview');
  fMIMEList.Add('.mpp','application/vnd.ms-project');
  fMIMEList.Add('.mvb','application/x-msmediaview');
  fMIMEList.Add('.man','application/x-troff-man');
  fMIMEList.Add('.mdb','application/x-msaccess');
  fMIMEList.Add('.me','application/x-troff-me');
  fMIMEList.Add('.ms','application/x-troff-ms');
  fMIMEList.Add('.msi','application/x-msi');
  fMIMEList.Add('.mpkg','vnd.apple.installer+xml');
  fMIMEList.Add('.mny','application/x-msmoney');
  fMIMEList.Add('.nix','application/x-mix-transfer');
  fMIMEList.Add('.o','application/x-object');
  fMIMEList.Add('.oda','application/oda');
  fMIMEList.Add('.odb','application/vnd.oasis.opendocument.database');
  fMIMEList.Add('.odc','application/vnd.oasis.opendocument.chart');
  fMIMEList.Add('.odf','application/vnd.oasis.opendocument.formula');
  fMIMEList.Add('.odg','application/vnd.oasis.opendocument.graphics');
  fMIMEList.Add('.odi','application/vnd.oasis.opendocument.image');
  fMIMEList.Add('.odm','application/vnd.oasis.opendocument.text-master');
  fMIMEList.Add('.odp','application/vnd.oasis.opendocument.presentation');
  fMIMEList.Add('.ods','application/vnd.oasis.opendocument.spreadsheet');
  fMIMEList.Add('.ogg','application/ogg');
  fMIMEList.Add('.odt','application/vnd.oasis.opendocument.text');
  fMIMEList.Add('.otg','application/vnd.oasis.opendocument.graphics-template');
  fMIMEList.Add('.oth','application/vnd.oasis.opendocument.text-web');
  fMIMEList.Add('.otp','application/vnd.oasis.opendocument.presentation-template');
  fMIMEList.Add('.ots','application/vnd.oasis.opendocument.spreadsheet-template');
  fMIMEList.Add('.ott','application/vnd.oasis.opendocument.text-template');
  fMIMEList.Add('.p10','application/pkcs10');
  fMIMEList.Add('.p12','application/x-pkcs12');
  fMIMEList.Add('.p7b','application/x-pkcs7-certificates');
  fMIMEList.Add('.p7m','application/pkcs7-mime');
  fMIMEList.Add('.p7r','application/x-pkcs7-certreqresp');
  fMIMEList.Add('.p7s','application/pkcs7-signature');
  fMIMEList.Add('.package','application/vnd.autopackage');
  fMIMEList.Add('.pfr','application/font-tdpfr');
  fMIMEList.Add('.pkg','vnd.apple.installer+xml');
  fMIMEList.Add('.pdf','application/pdf');
  fMIMEList.Add('.pko','application/vnd.ms-pki.pko');
  fMIMEList.Add('.pl','application/x-perl');
  fMIMEList.Add('.pnq','application/x-icq-pnq');
  fMIMEList.Add('.pot','application/mspowerpoint');
  fMIMEList.Add('.pps','application/mspowerpoint');
  fMIMEList.Add('.ppt','application/mspowerpoint');
  fMIMEList.Add('.ppz','application/mspowerpoint');
  fMIMEList.Add('.ps','application/postscript');
  fMIMEList.Add('.pub','application/x-mspublisher');
  fMIMEList.Add('.qpw','application/x-quattropro');
  fMIMEList.Add('.qtl','application/x-quicktimeplayer');
  fMIMEList.Add('.rar','application/rar');
  fMIMEList.Add('.rjs','application/vnd.rn-realsystem-rjs');
  fMIMEList.Add('.rmf','application/vnd.rmf');
  fMIMEList.Add('.rmp','application/vnd.rn-rn_music_package');
  fMIMEList.Add('.rmx','application/vnd.rn-realsystem-rmx');
  fMIMEList.Add('.rnx','application/vnd.rn-realplayer');
  fMIMEList.Add('.rpm','application/x-redhat-package-manager');
  fMIMEList.Add('.rsml','application/vnd.rn-rsml');
  fMIMEList.Add('.rtsp','application/x-rtsp');
  fMIMEList.Add('.scm','application/x-icq-scm');
  fMIMEList.Add('.ser','application/java-serialized-object');
  fMIMEList.Add('.scd','application/x-msschedule');
  fMIMEList.Add('.sda','application/vnd.stardivision.draw');
  fMIMEList.Add('.sdc','application/vnd.stardivision.calc');
  fMIMEList.Add('.sdd','application/vnd.stardivision.impress');
  fMIMEList.Add('.sdp','application/x-sdp');
  fMIMEList.Add('.setpay','application/set-payment-initiation');
  fMIMEList.Add('.setreg','application/set-registration-initiation');
  fMIMEList.Add('.sh','application/x-sh');
  fMIMEList.Add('.shar','application/x-shar');
  fMIMEList.Add('.shw','application/presentations');
  fMIMEList.Add('.sit','application/x-stuffit');
  fMIMEList.Add('.sitx','application/x-stuffitx');
  fMIMEList.Add('.skd','application/x-koan');
  fMIMEList.Add('.skm','application/x-koan');
  fMIMEList.Add('.skp','application/x-koan');
  fMIMEList.Add('.skt','application/x-koan');
  fMIMEList.Add('.smf','application/vnd.stardivision.math');
  fMIMEList.Add('.smi','application/smil');
  fMIMEList.Add('.smil','application/smil');
  fMIMEList.Add('.spl','application/futuresplash');
  fMIMEList.Add('.ssm','application/streamingmedia');
  fMIMEList.Add('.sst','application/vnd.ms-pki.certstore');
  fMIMEList.Add('.stc','application/vnd.sun.xml.calc.template');
  fMIMEList.Add('.std','application/vnd.sun.xml.draw.template');
  fMIMEList.Add('.sti','application/vnd.sun.xml.impress.template');
  fMIMEList.Add('.stl','application/vnd.ms-pki.stl');
  fMIMEList.Add('.stw','application/vnd.sun.xml.writer.template');
  fMIMEList.Add('.svi','application/softvision');
  fMIMEList.Add('.sv4cpio','application/x-sv4cpio');
  fMIMEList.Add('.sv4crc','application/x-sv4crc');
  fMIMEList.Add('.swf','application/x-shockwave-flash');
  fMIMEList.Add('.swf1','application/x-shockwave-flash');
  fMIMEList.Add('.sxc','application/vnd.sun.xml.calc');
  fMIMEList.Add('.sxi','application/vnd.sun.xml.impress');
  fMIMEList.Add('.sxm','application/vnd.sun.xml.math');
  fMIMEList.Add('.sxw','application/vnd.sun.xml.writer');
  fMIMEList.Add('.sxg','application/vnd.sun.xml.writer.global');
  fMIMEList.Add('.t','application/x-troff');
  fMIMEList.Add('.tar','application/x-tar');
  fMIMEList.Add('.tcl','application/x-tcl');
  fMIMEList.Add('.tex','application/x-tex');
  fMIMEList.Add('.texi','application/x-texinfo');
  fMIMEList.Add('.texinfo','application/x-texinfo');
  fMIMEList.Add('.tbz','application/x-bzip-compressed-tar');
  fMIMEList.Add('.tbz2','application/x-bzip-compressed-tar');
  fMIMEList.Add('.tgz','application/x-compressed-tar');
  fMIMEList.Add('.tlz','application/x-lzma-compressed-tar');
  fMIMEList.Add('.tr','application/x-troff');
  fMIMEList.Add('.trm','application/x-msterminal');
  fMIMEList.Add('.troff','application/x-troff');
  fMIMEList.Add('.tsp','application/dsptype');
  fMIMEList.Add('.torrent','application/x-bittorrent');
  fMIMEList.Add('.ttz','application/t-time');
  fMIMEList.Add('.txz','application/x-xz-compressed-tar');
  fMIMEList.Add('.udeb','application/x-debian-package');
  fMIMEList.Add('.uin','application/x-icq');
  fMIMEList.Add('.urls','application/x-url-list');
  fMIMEList.Add('.ustar','application/x-ustar');
  fMIMEList.Add('.vcd','application/x-cdlink');
  fMIMEList.Add('.vor','application/vnd.stardivision.writer');
  fMIMEList.Add('.vsl','application/x-cnet-vsl');
  fMIMEList.Add('.wcm','application/vnd.ms-works');
  fMIMEList.Add('.wb1','application/x-quattropro');
  fMIMEList.Add('.wb2','application/x-quattropro');
  fMIMEList.Add('.wb3','application/x-quattropro');
  fMIMEList.Add('.wdb','application/vnd.ms-works');
  fMIMEList.Add('.wks','application/vnd.ms-works');
  fMIMEList.Add('.wmd','application/x-ms-wmd');
  fMIMEList.Add('.wms','application/x-ms-wms');
  fMIMEList.Add('.wmz','application/x-ms-wmz');
  fMIMEList.Add('.wp5','application/wordperfect5.1');
  fMIMEList.Add('.wpd','application/wordperfect');
  fMIMEList.Add('.wpl','application/vnd.ms-wpl');
  fMIMEList.Add('.wps','application/vnd.ms-works');
  fMIMEList.Add('.wri','application/x-mswrite');
  fMIMEList.Add('.xfdf','application/vnd.adobe.xfdf');
  fMIMEList.Add('.xls','application/x-msexcel');
  fMIMEList.Add('.xlb','application/x-msexcel');
  fMIMEList.Add('.xpi','application/x-xpinstall');
  fMIMEList.Add('.xps','application/vnd.ms-xpsdocument');
  fMIMEList.Add('.xsd','application/vnd.sun.xml.draw');
  fMIMEList.Add('.xul','application/vnd.mozilla.xul+xml');
  fMIMEList.Add('.z','application/x-compress');
  fMIMEList.Add('.zoo','application/x-zoo');
  fMIMEList.Add('.zip','application/x-zip-compressed');

  { WAP }
  fMIMEList.Add('.wbmp','image/vnd.wap.wbmp');
  fMIMEList.Add('.wml','text/vnd.wap.wml');
  fMIMEList.Add('.wmlc','application/vnd.wap.wmlc');
  fMIMEList.Add('.wmls','text/vnd.wap.wmlscript');
  fMIMEList.Add('.wmlsc','application/vnd.wap.wmlscriptc');

  { Non-web text}
  fMIMEList.Add('.asm','text/x-asm');
  fMIMEList.Add('.p','text/x-pascal');
  fMIMEList.Add('.pas','text/x-pascal');
  fMIMEList.Add('.cs','text/x-csharp');
  fMIMEList.Add('.c','text/x-csrc');
  fMIMEList.Add('.c++','text/x-c++src');
  fMIMEList.Add('.cpp','text/x-c++src');
  fMIMEList.Add('.cxx','text/x-c++src');
  fMIMEList.Add('.cc','text/x-c++src');
  fMIMEList.Add('.h','text/x-chdr');
  fMIMEList.Add('.h++','text/x-c++hdr');
  fMIMEList.Add('.hpp','text/x-c++hdr');
  fMIMEList.Add('.hxx','text/x-c++hdr');
  fMIMEList.Add('.hh','text/x-c++hdr');
  fMIMEList.Add('.java','text/x-java');

  { WEB }
  fMIMEList.Add('.css','text/css');
  fMIMEList.Add('.js','text/javascript');
  fMIMEList.Add('.htm','text/html');
  fMIMEList.Add('.html','text/html');
  fMIMEList.Add('.xhtml','application/xhtml+xml');
  fMIMEList.Add('.xht','application/xhtml+xml');
  fMIMEList.Add('.rdf','application/rdf+xml');
  fMIMEList.Add('.rss','application/rss+xml');
  fMIMEList.Add('.ls','text/javascript');
  fMIMEList.Add('.mocha','text/javascript');
  fMIMEList.Add('.shtml','server-parsed-html');
  fMIMEList.Add('.sgm','text/sgml');
  fMIMEList.Add('.sgml','text/sgml');

  { Message }
  fMIMEList.Add('.mht','message/rfc822');
end;

function TMIMETypes.GetExtensionMIMEType(const aExtension: string): string;
begin
  if not fMIMEList.TryGetValue(aExtension,Result) then Result := 'text/html';
end;

function TMIMETypes.GetFileMIMEType(const aFilename: string): string;
var
  fname : string;
begin
  fname := ExtractFileExt(aFilename);
  //remove queries
  if fname.Contains('?') then fname := Copy(fname,1,fname.IndexOf('?'));
  if not fMIMEList.TryGetValue(fname,Result) then Result := 'text/html';
end;

{ EControlledException }

constructor EControlledException.Create(aCaller: TObject; const aMessage: string);
begin
  inherited Create(aMessage);
  if aCaller <> nil then fCallerClass := aCaller.ClassType;
end;

initialization
  MIMETypes := TMIMETypes.Create;

finalization
  MIMETypes.Free;

end.
