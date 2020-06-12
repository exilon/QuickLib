unit Quick.OAuth;

interface

uses
  Quick.HttpClient,
  Quick.HttpServer.Request,
  Quick.HttpServer.Response,
  Quick.HttpServer,
  Quick.Threads,
  Quick.OAuth.Utils,
  SysUtils;

type
  TOAuthToken = class
  private
    fAccessTokenExpiration: integer;
    fAccessToken: string;
    fRefreshToken: string;
    fRetrieveDateTime: TDateTime;
  public
    property AccessToken: string read fAccessToken write fAccessToken;
    property AccessTokenExpiration: integer read fAccessTokenExpiration write
        fAccessTokenExpiration;
    property RefreshToken: string read fRefreshToken write fRefreshToken;
    property RetrieveDateTime: TDateTime read fRetrieveDateTime write
        fRetrieveDateTime;
  end;

  TOnSaveToken = procedure (const aToken: TOAuthToken) of object;
  TOnLoadToken = procedure (var aToken: TOAuthToken) of object;
  TOnAuthorizationCompleted = reference to procedure (const aToken: TOAuthToken);
  TOnRefreshCompleted = reference to procedure (const aToken: TOAuthToken);

  {$M+}
  TOAuthBase = class
  private
    fToken: TOAuthToken;
    fOnAuthorizationCompleted: TOnAuthorizationCompleted;
    fOnRefreshCompleted: TOnRefreshCompleted;

    fAccessTokenParam: string;
    fAuthCodeParam: string;
    fAuthErrorParam: string;
    fExpirationParam: string;
    fRefreshTokenParam: string;

    fCallbackURL: string;
    fServer: THttpServer;
    fClient: TJsonHttpClient;

    fOnSaveToken: TOnSaveToken;
    fOnLoadToken: TOnLoadToken;

    procedure ExchangeAuthForAccessToken (const aAuthToken: string);
    procedure RefreshAccessToken (const aRefreshToken: string);
    function GetAccessToken: string;
    function IsTokenValid: boolean;
  protected
    // Abstract
    function CreateAuthorizationRequest: string; virtual; abstract;
    function CreateAuthToAccessRequest (const aAuthToken: string): string; virtual; abstract;
    function CreateRefreshRequest (const aRefreshToken: string): string; virtual; abstract;

    // Available
    function CreateAuthorizationHTMLPage (const aAuthorised: boolean): string; virtual;
    procedure OnProcessRequest(aRequest: IHttpRequest; aResponse: IHttpResponse); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    // Methods
    procedure Authorize(const aOnAuthorizationCompleted: TOnAuthorizationCompleted = nil);
    procedure RefreshToken (const aOnRefreshCompleted: TOnRefreshCompleted = nil);
    // Properties
    property AccessTokenParam: string read fAccessTokenParam write
        fAccessTokenParam;
    property AuthCodeParam: string read fAuthCodeParam write fAuthCodeParam;
    property AuthErrorParam: string read fAuthErrorParam write fAuthErrorParam;
    property ExpirationParam: string read fExpirationParam write fExpirationParam;
    property RefreshTokenParam: string read fRefreshTokenParam write
        fRefreshTokenParam;

    property CallbackURL: string read fCallbackURL write fCallbackURL;
    property AccessToken: string read GetAccessToken;
  published
    // Events
    property OnSaveToken: TOnSaveToken read fOnSaveToken write fOnSaveToken;
    property OnLoadToken: TOnLoadToken read fOnLoadToken write fOnLoadToken;
  end;
  {$M-}

  EOAuthException = class (Exception);

implementation

uses
 System.JSON, System.DateUtils, System.Types;

{$I QuickLib.INC}

constructor TOAuthBase.Create;
begin
  inherited Create;
  fClient:=TJsonHttpClient.Create;
  {$IFNDEF DELPHIRX101_UP}
  fServer:=nil;
  {$ENDIF}
  fToken:=TOAuthToken.Create;
  fToken.AccessToken:='';
  fToken.AccessTokenExpiration:=0;
  fToken.RefreshToken:='';
  fToken.RetrieveDateTime:=EncodeDateTime(1900, 1, 1, 23, 59, 00, 00);
end;

function TOAuthBase.CreateAuthorizationHTMLPage(
  const aAuthorised: boolean): string;
begin
  if aAuthorised then
    result:='Access Authorised! You can now close this page and return to the application'
  else
    result:='Access Denied!';
end;

destructor TOAuthBase.Destroy;
begin
  fClient.Free;
  fServer.Free;
  fToken.Free;
  inherited;
end;

procedure TOAuthBase.ExchangeAuthForAccessToken(const aAuthToken: string);
var
  resp: IHttpRequestResponse;
  accToken: string;
  refrToken: string;
  expiry: integer;
begin
  fToken.AccessToken:='';
  fToken.AccessTokenExpiration:=0;
  try
    case GetMethodFromRequest(CreateAuthToAccessRequest(aAuthToken)) of
      rmGET: resp:=fClient.Get(GetCleanRequest(CreateAuthToAccessRequest(aAuthToken)));
      rmPOST: resp:=fClient.Post(GetCleanRequest(CreateAuthToAccessRequest(aAuthToken)), '');
    end;
    if (assigned(resp)) and (resp.StatusCode = 200) then
    begin
      if Assigned(resp.Response) then
      begin
        if resp.Response.TryGetValue(AccessTokenParam, accToken) then
          fToken.AccessToken:=accToken;
        if resp.Response.TryGetValue(ExpirationParam, expiry) then
          fToken.AccessTokenExpiration:=expiry;
        if resp.Response.TryGetValue(RefreshTokenParam, refrToken) then
          fToken.RefreshToken:=refrToken;
        fToken.RetrieveDateTime:=Now;
        if Assigned(fOnSaveToken) then
          fOnSaveToken(fToken);
        if Assigned(fOnAuthorizationCompleted) then
          fOnAuthorizationCompleted(fToken);
      end;
    end
    else
      raise EOAuthException.Create('Something went wrong. Please try again');
  except
    raise EOAuthException.Create('Something went wrong. Please try again');
  end;
end;

procedure TOAuthBase.OnProcessRequest(aRequest: IHttpRequest;
  aResponse: IHttpResponse);
begin
  fToken.AccessToken:='';
  fToken.AccessTokenExpiration:=0;
  if aRequest.UnparsedParams.Contains(AuthErrorParam) then
    aResponse.ContentText:= CreateAuthorizationHTMLPage(false)
  else
  if aRequest.UnparsedParams.Contains(AuthCodeParam) then
  begin
    ExchangeAuthForAccessToken(aRequest.Query[AuthCodeParam].AsString);
    aResponse.ContentText:= CreateAuthorizationHTMLPage(true);
  end;
end;

procedure TOAuthBase.RefreshAccessToken(const aRefreshToken: string);
var
  resp: IHttpRequestResponse;
  accToken: string;
  expiry: integer;
begin
  try
    case GetMethodFromRequest(CreateRefreshRequest(aRefreshToken)) of
      rmGET: resp:=fClient.Get(GetCleanRequest(CreateRefreshRequest(aRefreshToken)));
      rmPOST: resp:=fClient.Post(GetCleanRequest(CreateRefreshRequest(aRefreshToken)), '');
    end;
    if (assigned(resp)) and (resp.StatusCode = 200) then
    begin
      if Assigned(resp.Response) then
      begin
        if resp.Response.TryGetValue(AccessTokenParam, accToken) then
          fToken.AccessToken:=accToken;
        if resp.Response.TryGetValue(ExpirationParam, expiry) then
          fToken.AccessTokenExpiration:=expiry;
        fToken.RetrieveDateTime:=Now;
        if Assigned(fOnSaveToken) then
          fOnSaveToken(fToken);
      end;
    end
    else
      raise EOAuthException.Create('Something went wrong. Please try again');
  except
    raise EOAuthException.Create('Something went wrong. Please try again');
  end;
end;

procedure TOAuthBase.RefreshToken(const aOnRefreshCompleted:
    TOnRefreshCompleted = nil);
begin
  fOnRefreshCompleted:=aOnRefreshCompleted;
  if Assigned(fOnLoadToken) then
    fOnLoadToken(fToken);
  if fToken.AccessToken = '' then
    Authorize(TOnAuthorizationCompleted(fOnRefreshCompleted))
  else
  begin
    GetAccessToken;
    if Assigned(fOnRefreshCompleted) then
      fOnRefreshCompleted(fToken);
  end;
end;

procedure TOAuthBase.Authorize(const aOnAuthorizationCompleted:
    TOnAuthorizationCompleted = nil);
begin
  fOnAuthorizationCompleted:=aOnAuthorizationCompleted;
  if Assigned(fOnLoadToken) then
    fOnLoadToken(fToken);

  if IsTokenValid then
    Exit;

  fClient:=TJsonHttpClient.Create;
  if Assigned(fServer) then
    fServer.Stop;
  FreeAndNil(fServer);

  fServer:=THttpServer.Create(GetDomain(fCallbackURL), GetPort(fCallbackURL), false, nil);
  try
    fServer.OnNewRequest:=OnProcessRequest;
    fServer.Start;

    OpenURL(GetCleanRequest(CreateAuthorizationRequest));
  except
    fServer.Stop;
    FreeAndNil(fServer);
  end;
end;

function TOAuthBase.GetAccessToken: string;
begin
  result:='';
  if IsTokenValid then
    result:=fToken.AccessToken
  else
    RefreshAccessToken(fToken.RefreshToken);
end;

function TOAuthBase.IsTokenValid: boolean;
var
  expDate: TDateTime;
begin
  expDate:=IncSecond(fToken.RetrieveDateTime, fToken.AccessTokenExpiration);
  result:= CompareDateTime(expDate, Now) = GreaterThanValue;
end;

end.
