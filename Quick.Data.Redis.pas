{ ***************************************************************************

  Copyright (c) 2015-2022 Kike Pérez

  Unit        : Quick.Data.Redis
  Description : Redis client
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 22/02/2020
  Modified    : 07/03/2022

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

unit Quick.Data.Redis;

{$i QuickLib.inc}

interface

uses
  {$IFDEF DEBUG_REDIS}
  Quick.Debug.Utils,
  {$ENDIF}
  System.SysUtils,
  System.DateUtils,
  IdTCPClient,
  IdGlobal,
  Quick.Commons;

type
  IRedisResponse = interface
  ['{21EF7ABF-E678-4F18-AE56-8A7C6B817AE3}']
    function GetIsDone: Boolean;
    function GetResponse: string;
    procedure SetIsDone(const Value: Boolean);
    procedure SetResponse(const Value: string);
    property IsDone : Boolean read GetIsDone write SetIsDone;
    property Response : string read GetResponse write SetResponse;
  end;
  TRedisResponse = class(TInterfacedObject,IRedisResponse)
  private
    fIsDone : Boolean;
    fResponse : string;
    function GetIsDone: Boolean;
    function GetResponse: string;
    procedure SetIsDone(const Value: Boolean);
    procedure SetResponse(const Value: string);
  public
    constructor Create;
    property IsDone : Boolean read GetIsDone write SetIsDone;
    property Response : string read GetResponse write SetResponse;
  end;

  TRedisSortedItem = record
    Value : string;
    Score : Int64;
  end;

  IRedisCommand = interface
  ['{13A978D1-C689-403F-8623-3489E4DEE060}']
    function AddArgument(const aValue : string) : IRedisCommand; overload;
    function AddArgument(const aValue : Int64) : IRedisCommand; overload;
    function AddArgument(const aValue : Extended) : IRedisCommand; overload;
    function ToCommand : string;
  end;

  TRedisCommand = class(TInterfacedObject,IRedisCommand)
  private
    fCommand : string;
    fArguments : array of string;
  public
    constructor Create(const aCommand : string);
    function AddArgument(const aValue : string) : IRedisCommand; overload;
    function AddArgument(const aValue : Int64) : IRedisCommand; overload;
    function AddArgument(const aValue : Extended) : IRedisCommand; overload;
    function ToCommand : string;
  end;

  TRedisClient = class
  private
    fTCPClient : TIdTCPClient;
    fHost : string;
    fPort : Integer;
    fDataBaseNumber : Integer;
    fMaxSize : Int64;
    fPassword : string;
    fConnectionTimeout : Integer;
    fReadTimeout : Integer;
    fConnected : Boolean;
    fRaiseErrorIfCommandFails : Boolean;
    procedure SetConnectionTimeout(const Value: Integer);
    procedure SetReadTimeout(const Value: Integer);
    function Command(const aCommand : string; const aArguments : string) : IRedisResponse; overload;
    function Command(const aCommand, aArgumentsFormat : string; aValues : array of const) : IRedisResponse; overload;
    function Command(const aCommand : string) : IRedisResponse; overload;
    function EscapeString(const json: string) : string;
    //function BulkString(const aValue : string) : string;
  public
    constructor Create;
    destructor Destroy; override;
    property Host : string read fHost write fHost;
    property Port : Integer read fPort write fPort;
    property DataBaseNumber : Integer read fDataBaseNumber write fDataBaseNumber;
    property MaxSize : Int64 read fMaxSize write fMaxSize;
    property Password : string read fPassword write fPassword;
    property ConnectionTimeout : Integer read fConnectionTimeout write SetConnectionTimeout;
    property ReadTimeout : Integer read fReadTimeout write SetReadTimeout;
    property RaiseErrorIfCommandFails : Boolean read fRaiseErrorIfCommandFails write fRaiseErrorIfCommandFails;
    property Connected : Boolean read fConnected;
    function RedisSELECT(dbIndex : Integer) : Boolean;
    function RedisSET(const aKey, aValue : string; aTTLMs : Integer = -1) : Boolean;
    function RedisGET(const aKey : string; out oValue : string) : Boolean;
    function RedisDEL(const aKey : string) : Boolean;
    function RedisRPUSH(const aKey, aValue : string) : Boolean;
    function RedisLPUSH(const aKey, aValue : string) : Boolean;
    function RedisRPOP(const aKey : string; out oValue : string) : Boolean;
    function RedisBRPOP(const aKey: string; out oValue: string; aWaitTimeoutSecs : Integer): Boolean;
    function RedisLPOP(const aKey : string; out oValue : string) : Boolean;
    function RedisBLPOP(const aKey: string; out oValue: string; aWaitTimeoutSecs : Integer): Boolean;
    function RedisBRPOPLPUSH(const aKey, aKeyToMove: string; out oValue: string; aWaitTimeoutSecs : Integer): Boolean;
    function RedisLTRIM(const aKey : string; aFirstElement, aMaxSize : Int64) : Boolean;
    function RedisEXPIRE(const aKey : string; aTTLMs : Integer) : Boolean; overload;
    function RedisEXPIRE(const aKey : string; aExpireDate : TDateTime) : Boolean; overload;
    function RedisLINDEX(const aKey: string; aIndex: Integer; out oValue : string): Boolean;
    function RedisLREM(const aKey, aValue: string; aNumOccurrences: Integer): Boolean;
    function RedisZADD(const aKey, aValue : string; aScore : Int64) : Boolean;
    function RedisZREM(const aKey, aValue : string) : Boolean;
    function RedisZRANGE(const aKey : string; aStartPosition, aEndPosition : Int64) : TArray<string>;
    function RedisZRANGEBYSCORE(const aKey : string; aMinScore, aMaxScore : Int64) : TArray<TRedisSortedItem>;
    function RedisLLEN(const aKey : string): Integer;
    function RedisTTL(const aKey, aValue : string): Integer;
    function RedisAUTH(const aPassword : string) : Boolean;
    function RedisPING : Boolean;
    function RedisQUIT : Boolean;
    procedure Connect;
    procedure Disconnect;
  end;

  ERedisConnectionError = class(Exception);
  ERedisAuthError = class(Exception);
  ERedisCommandError = class(Exception);

implementation

const

  DEF_REDIS_PORT = 6379;
  DEF_CONNECTIONTIMEOUT = 30000;
  DEF_READTIMETOUT = 10000;


{ TRedisResponse }

constructor TRedisResponse.Create;
begin
  fIsDone := False;
  fResponse := '';
end;

function TRedisResponse.GetIsDone: Boolean;
begin
  Result := fIsDone;
end;

function TRedisResponse.GetResponse: string;
begin
  Result := fResponse;
end;

procedure TRedisResponse.SetIsDone(const Value: Boolean);
begin
  fIsDone := Value;
end;

procedure TRedisResponse.SetResponse(const Value: string);
begin
  fResponse := Value;
end;

{ TRedisClient }

constructor TRedisClient.Create;
begin
  inherited;
  fConnected := False;
  fHost := 'localhost';
  fPort := DEF_REDIS_PORT;
  fDataBaseNumber := 0;
  fMaxSize := 0;
  fPassword := '';
  fConnectionTimeout := DEF_CONNECTIONTIMEOUT;
  fReadTimeout := DEF_READTIMETOUT;
  fRaiseErrorIfCommandFails := False;
  fTCPClient := TIdTCPClient.Create;
end;

destructor TRedisClient.Destroy;
begin
  try
    try
      Disconnect;
    finally
      fTCPClient.Free;
    end;
  except
    //avoid closing errors
  end;
  inherited;
end;

procedure TRedisClient.Disconnect;
begin
  if fTCPClient.Connected then
  begin
    RedisQUIT;
    fTCPClient.IOHandler.InputBuffer.Clear;
    fTCPClient.IOHandler.WriteBufferFlush;
    if fTCPClient.Connected then fTCPClient.Disconnect(False);
  end;
  fConnected := False;
end;

procedure TRedisClient.Connect;
begin
  try
    //connect password and database
    if not fTCPClient.Connected then
    begin
      fTCPClient.Host := fHost;
      fTCPClient.Port := fPort;
      fTCPClient.ConnectTimeout := fConnectionTimeout;
      fTCPClient.ReadTimeout := fConnectionTimeout;
      fTCPClient.Connect;
      if not fTCPClient.Connected then raise ERedisConnectionError.Create('Can''t connect to Redis Server!');
    end;
    fTCPClient.Socket.Binding.SetKeepAliveValues(True,5000,1000);
    if fPassword <> '' then
    begin
      if not RedisAUTH(fPassword) then raise  ERedisAuthError.Create('Redis authentication error!');
    end;
    if fDataBaseNumber > 0 then
    begin
      if not RedisSELECT(fDataBaseNumber) then raise ERedisConnectionError.CreateFmt('Can''t select Redis Database "%d"',[fDataBaseNumber]);
    end;
    fTCPClient.IOHandler.MaxLineLength := MaxInt;
    fConnected := True;
  except
    on E : Exception do raise ERedisConnectionError.CreateFmt('Can''t connect to Redis service %s:%d (%s)',[Self.Host,Self.Port,e.Message]);
  end;
end;

function TRedisClient.EscapeString(const json: string): string;
begin
  Result := StringReplace(json,'\','\\',[rfReplaceAll]);
  Result := StringReplace(Result,'"','\"',[rfReplaceAll]);
  Result := StringReplace(Result,#13,'\r',[rfReplaceAll]);
  Result := StringReplace(Result,#10,'\n',[rfReplaceAll]);
  //Result := StringReplace(Result,'/','\/"',[rfReplaceAll]);
end;

//function TRedisClient.BulkString(const aValue : string) : string;
//begin
//  Result := Format('$%d%s%s%s',[aValue.Length,CRLF,aValue,CRLF]);
//end;

procedure TRedisClient.SetConnectionTimeout(const Value: Integer);
begin
  if fConnectionTimeout <> Value then
  begin
    fConnectionTimeout := Value;
    if Assigned(fTCPClient) then fTCPClient.ConnectTimeout := fConnectionTimeout;
  end;
end;

procedure TRedisClient.SetReadTimeout(const Value: Integer);
begin
  if fReadTimeout <> Value then
  begin
    fReadTimeout := Value;
    if Assigned(fTCPClient) then fTCPClient.ConnectTimeout := fReadTimeout;
  end;
end;

function TRedisClient.Command(const aCommand, aArgumentsFormat : string; aValues : array of const) : IRedisResponse;
begin
  Result := Command(aCommand,Format(aArgumentsFormat,aValues));
end;

function TRedisclient.Command(const aCommand : string; const aArguments : string) : IRedisResponse;
begin
  Result := Command(aCommand + ' ' + aArguments + CRLF);
end;

function TRedisClient.Command(const aCommand : string) : IRedisResponse;
  function TrimResponse(const aResponse : string) : string;
  begin
    Result := Copy(aResponse,Low(aResponse) + 1, aResponse.Length);
  end;
var
  res : string;
begin
  Result := TRedisResponse.Create;
  try
    if not fTCPClient.Connected then Connect;
    //Writeln('*'+ (aArguments.CountChar('$') + 1).ToString + CRLF + BulkString(aCommand) + aArguments);
    fTCPClient.IOHandler.Write(aCommand);
    if fTCPClient.IOHandler.CheckForDataOnSource(fReadTimeout) then
    begin
      res := fTCPClient.IOHandler.ReadLn;
      {$IFDEF DEBUG_REDIS}
      TDebugger.Trace(Self,Format('Command "%s"',[res]));
      {$ENDIF}
      if not res.IsEmpty then
      case res[Low(res)] of
        '+' :
          begin
            if res.Contains('+OK') then
            begin
              Result.IsDone := True;
            end
            else Result.Response := TrimResponse(res);
          end;
        '-' : Result.Response := TrimResponse(res);
        ':' :
          begin
            Result.Response := TrimResponse(res);
            Result.IsDone := Result.Response.ToInteger > -1;
          end;
        '$' :
          begin
            Result.Response := TrimResponse(res);
            if IsInteger(Result.Response) then
            begin
              if Result.Response.ToInteger > -1 then Result.IsDone := True;
            end
            else Result.IsDone := True;
          end;
        '*' :
          begin
            Result.Response := TrimResponse(res);
            Result.IsDone := True;
          end;
        else Result.Response := TrimResponse(res);
      end;
    end;
    if (fRaiseErrorIfCommandFails) and (not Result.IsDone) then raise ERedisCommandError.CreateFmt('command fail (%s)',[Result.Response]);
  except
    on E : Exception do raise ERedisCommandError.CreateFmt('Redis error: %s [%s...]',[e.message,aCommand.Substring(0,20)]);
  end;
end;

function TRedisClient.RedisRPUSH(const aKey, aValue : string) : Boolean;
begin
  Result := Command('RPUSH','%s "%s"',[aKey,EscapeString(aValue)]).IsDone;
end;

function TRedisClient.RedisSELECT(dbIndex: Integer): Boolean;
begin
  Result := Command('SELECT',dbIndex.ToString).IsDone;
end;

function TRedisClient.RedisSET(const aKey, aValue: string; aTTLMs: Integer = -1): Boolean;
var
  rediscmd : IRedisCommand;
begin
  rediscmd := TRedisCommand.Create('SET')
               .AddArgument(aKey)
               .AddArgument(aValue)
               .AddArgument('PX')
               .AddArgument(aTTLMs);
  Result := Command(rediscmd.ToCommand).IsDone;
end;

function TRedisClient.RedisRPOP(const aKey: string; out oValue: string): Boolean;
var
  rediscmd : IRedisCommand;
begin
  rediscmd := TRedisCommand.Create('RPOP')
               .AddArgument(aKey);
  if Command(rediscmd.ToCommand).IsDone then
  begin
    oValue := fTCPClient.IOHandler.ReadLn;
    Result := True;
  end
  else Result := False;
end;

function TRedisClient.RedisBRPOP(const aKey: string; out oValue: string; aWaitTimeoutSecs : Integer): Boolean;
var
  rediscmd : IRedisCommand;
  response : IRedisResponse;
begin
  Result := False;
  rediscmd := TRedisCommand.Create('BRPOP')
               .AddArgument(aKey)
               .AddArgument(aWaitTimeoutSecs);
  response := Command(rediscmd.ToCommand);
  if response.IsDone then
  begin
    //if response.Response = '-1' then Exit;
    fTCPClient.IOHandler.ReadLn; //$int
    fTCPClient.IOHandler.ReadLn; //key
    fTCPClient.IOHandler.ReadLn; //$int
    oValue := fTCPClient.IOHandler.ReadLn; //value
    if not oValue.IsEmpty then Result := True;
  end
  else
  begin
    if not response.Response.IsEmpty then ERedisCommandError.CreateFmt('BRPOP Error: %s',[response.Response]);
  end;
end;

function TRedisClient.RedisBRPOPLPUSH(const aKey, aKeyToMove: string; out oValue: string; aWaitTimeoutSecs: Integer): Boolean;
var
  rediscmd : IRedisCommand;
  response : IRedisResponse;
begin
  Result := False;
  rediscmd := TRedisCommand.Create('BRPOPLPUSH')
               .AddArgument(aKey)
               .AddArgument(aKeyToMove)
               .AddArgument(aWaitTimeoutSecs);
  response := Command(rediscmd.ToCommand);
  if response.IsDone then
  begin
    oValue := fTCPClient.IOHandler.ReadLn; //value
    if not oValue.IsEmpty then Result := True;
  end
  else raise ERedisCommandError.CreateFmt('BRPOPLPUSH Error: %s',[response.Response]);
end;

function TRedisClient.RedisDEL(const aKey: string): Boolean;
var
  rediscmd : IRedisCommand;
begin
  rediscmd := TRedisCommand.Create('DEL')
               .AddArgument(aKey);
  Result := Command(rediscmd.ToCommand).IsDone;
end;

function TRedisClient.RedisLLEN(const aKey : string): Integer;
var
  rediscmd : IRedisCommand;
  response : IRedisResponse;
begin
  Result := 0;
  rediscmd := TRedisCommand.Create('LLEN')
               .AddArgument(aKey);
  response := Command(rediscmd.ToCommand);
  if response.IsDone then
  begin
    Result := response.Response.ToInteger;
  end;
end;

function TRedisClient.RedisTTL(const aKey, aValue : string): Integer;
var
  rediscmd : IRedisCommand;
  response : IRedisResponse;
begin
  Result := 0;
  rediscmd := TRedisCommand.Create('TTL')
               .AddArgument(aKey)
               .AddArgument(aValue);
  response := Command(rediscmd.ToCommand);
  if response.IsDone then
  begin
    Result := response.Response.ToInteger;
  end;
end;

function TRedisClient.RedisZADD(const aKey, aValue: string; aScore: Int64): Boolean;
var
  rediscmd : IRedisCommand;
  response : IRedisResponse;
begin
  rediscmd := TRedisCommand.Create('ZADD')
               .AddArgument(aKey)
               .AddArgument(aScore)
               .AddArgument(aValue);
  response := Command(rediscmd.ToCommand);
  if response.IsDone then
  begin
    Result := response.Response.ToInteger = 1;
  end
  else raise ERedisCommandError.CreateFmt('ZADD %s',[response.Response]);
end;

function TRedisClient.RedisZRANGE(const aKey: string; aStartPosition, aEndPosition: Int64): TArray<string>;
var
  rediscmd : IRedisCommand;
  response : IRedisResponse;
  value : string;
  i : Integer;
begin
  Result := [];
  rediscmd := TRedisCommand.Create('ZRANGE')
               .AddArgument(aKey)
               .AddArgument(aStartPosition)
               .AddArgument(aEndPosition);
  response := Command(rediscmd.ToCommand);
  if response.IsDone then
  begin
    for i := 1 to (response.Response.ToInteger) do
    begin
      fTCPClient.IOHandler.ReadLn; //$int
      value := fTCPClient.IOHandler.ReadLn; //value
      Result := Result + [value];
    end;
  end
  else raise ERedisCommandError.CreateFmt('ZRANGE Error: %s',[response.Response]);
end;

function TRedisClient.RedisZRANGEBYSCORE(const aKey: string; aMinScore, aMaxScore: Int64): TArray<TRedisSortedItem>;
var
  rediscmd : IRedisCommand;
  response : IRedisResponse;
  item : TRedisSortedItem;
  i : Integer;
  value : string;
  score : string;
begin
  Result := [];
  rediscmd := TRedisCommand.Create('ZRANGEBYSCORE')
               .AddArgument(aKey)
               .AddArgument(aMinScore)
               .AddArgument(aMaxScore)
               .AddArgument('WITHSCORES');
  response := Command(rediscmd.ToCommand);
  if response.IsDone then
  begin
    for i := 1 to (response.Response.ToInteger Div 2) do
    begin
      fTCPClient.IOHandler.ReadLn; //$int
      value := fTCPClient.IOHandler.ReadLn; //value
      fTCPClient.IOHandler.ReadLn; //$int
      score := fTCPClient.IOHandler.ReadLn; //score
      item.Value := value;
      item.Score := score.ToInt64;
      Result := Result + [item];
    end;
  end
  else raise ERedisCommandError.CreateFmt('ZRANGE Error: %s',[response.Response]);
end;

function TRedisClient.RedisZREM(const aKey, aValue: string): Boolean;
var
  rediscmd : IRedisCommand;
  response : IRedisResponse;
begin
  Result := False;
  rediscmd := TRedisCommand.Create('ZREM')
               .AddArgument(aKey)
               .AddArgument(aValue);
  response := Command(rediscmd.ToCommand);
  if response.IsDone then
  begin
    Result := response.Response.ToInteger = 1;
  end;
end;

function TRedisClient.RedisLPOP(const aKey: string; out oValue: string): Boolean;
var
  rediscmd : IRedisCommand;
begin
  Result := False;
  rediscmd := TRedisCommand.Create('LPOP')
               .AddArgument(aKey);
  if Command(rediscmd.ToCommand).IsDone then
  begin
    oValue := fTCPClient.IOHandler.ReadLn;
    Result := True;
  end;
end;

function TRedisClient.RedisBLPOP(const aKey: string; out oValue: string; aWaitTimeoutSecs : Integer): Boolean;
var
  rediscmd : IRedisCommand;
  response : IRedisResponse;
begin
  rediscmd := TRedisCommand.Create('BLPOP')
               .AddArgument(aKey)
               .AddArgument(aWaitTimeoutSecs);
  response := Command(rediscmd.ToCommand);
  if response.IsDone then
  begin
    fTCPClient.IOHandler.ReadLn; //$int
    fTCPClient.IOHandler.ReadLn; //key
    fTCPClient.IOHandler.ReadLn; //$int
    oValue := fTCPClient.IOHandler.ReadLn; //value
    Result := True;
  end
  else raise ERedisCommandError.CreateFmt('BLPOP Error: %s',[response.Response]);
end;

function TRedisClient.RedisLPUSH(const aKey, aValue : string) : Boolean;
var
  rediscmd : IRedisCommand;
begin
  rediscmd := TRedisCommand.Create('LPUSH')
               .AddArgument(aKey)
               .AddArgument(aValue);
  Result := Command(rediscmd.ToCommand).IsDone;
end;

function TRedisClient.RedisLREM(const aKey, aValue: string; aNumOccurrences: Integer): Boolean;
var
  rediscmd : IRedisCommand;
begin
  rediscmd := TRedisCommand.Create('LREM')
               .AddArgument(aKey)
               .AddArgument(aNumOccurrences * -1)
               .AddArgument(aValue);
  Result := Command(rediscmd.ToCommand).IsDone;
end;

function TRedisClient.RedisLTRIM(const aKey : string; aFirstElement, aMaxSize : Int64) : Boolean;
var
  rediscmd : IRedisCommand;
begin
  rediscmd := TRedisCommand.Create('LTRIM')
               .AddArgument(aKey)
               .AddArgument(aFirstElement)
               .AddArgument(aMaxSize);
  Result := Command(rediscmd.ToCommand).IsDone;
end;

function TRedisClient.RedisAUTH(const aPassword : string) : Boolean;
begin
  Result := Command('AUTH',fPassword).IsDone;
end;

function TRedisClient.RedisEXPIRE(const aKey: string; aExpireDate: TDateTime): Boolean;
begin
  Result := RedisEXPIRE(aKey,MilliSecondsBetween(Now(),aExpireDate));
end;

function TRedisClient.RedisEXPIRE(const aKey: string; aTTLMs: Integer): Boolean;
begin
  Result := Command('PEXPIRE','%s %d',[aKey,aTTLMs]).IsDone;
end;

function TRedisClient.RedisLINDEX(const aKey: string; aIndex: Integer; out oValue : string): Boolean;
var
  response : IRedisResponse;
begin
  Result := False;
  response := Command('LINDEX','%s %d',[aKey,aIndex]);
  if response.IsDone then
  begin
    oValue := response.response;
    Result := True;
  end;
end;

function TRedisClient.RedisGET(const aKey: string; out oValue: string): Boolean;
var
  rediscmd : IRedisCommand;
begin
  Result := False;
  rediscmd := TRedisCommand.Create('GET')
               .AddArgument(aKey);
  if Command(rediscmd.ToCommand).IsDone then
  begin
    oValue := fTCPClient.IOHandler.ReadLn;
    Result := True;
  end;
end;

function TRedisClient.RedisPING : Boolean;
begin
  Result := False;
  if Command('PING'+ CRLF).IsDone then
  begin
    Result := fTCPClient.IOHandler.ReadLn = 'PONG';
  end;
end;

function TRedisClient.RedisQUIT : Boolean;
begin
  try
    Result := Command('QUIT' + CRLF).IsDone;
  except
    Result := False;
  end;
end;

{ TRedisCommand }

constructor TRedisCommand.Create(const aCommand: string);
begin
  fCommand := aCommand;
  fArguments := fArguments + [fCommand];
end;

function TRedisCommand.AddArgument(const aValue: string) : IRedisCommand;
begin
  Result := Self;
  fArguments := fArguments + [aValue];
end;

function TRedisCommand.AddArgument(const aValue: Extended): IRedisCommand;
begin
  Result := Self;
  fArguments := fArguments + [aValue.ToString];
end;

function TRedisCommand.AddArgument(const aValue: Int64): IRedisCommand;
begin
  Result := Self;
  fArguments := fArguments + [aValue.ToString];
end;

function TRedisCommand.ToCommand: string;
var
  arg : string;
begin
  Result := '*' + (High(fArguments) + 1).ToString + CRLF;
  for arg in fArguments do
  begin
    Result := Result + '$' + arg.Length.ToString + CRLF + arg + CRLF;
  end;
end;

end.
