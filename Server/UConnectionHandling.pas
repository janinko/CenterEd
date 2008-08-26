(*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License, Version 1.0 only
 * (the "License").  You may not use this file except in compliance
 * with the License.
 *
 * You can obtain a copy of the license at
 * http://www.opensource.org/licenses/cddl1.php.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at
 * http://www.opensource.org/licenses/cddl1.php.  If applicable,
 * add the following below this CDDL HEADER, with the fields enclosed
 * by brackets "[]" replaced with your own identifying * information:
 *      Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 *
 *
 *      Portions Copyright 2008 Andreas Schneider
 *)
unit UConnectionHandling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UPacket, UPacketHandlers, UConfig, UAccount, UNetState,
  UEnhancedMemoryStream, UEnums;
  
type

  { TProtocolVersion }

  TProtocolVersionPacket = class(TPacket)
    constructor Create(AVersion: Cardinal);
  end;

  { TLoginResponsePacket }

  TLoginResponsePacket = class(TPacket)
    constructor Create(AState: TLoginState; AAccount: TAccount = nil);
  end;
  
  { TServerStatePacket }

  TServerStatePacket = class(TPacket)
    constructor Create(AState: TServerState; AMessage: string = '');
  end;

procedure OnConnectionHandlerPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnLoginRequestPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnQuitPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);

var
  ConnectionPacketHandlers: array[0..$FF] of TPacketHandler;

implementation

uses
  md5, UCEDServer, UClientHandling, UPackets;

procedure OnConnectionHandlerPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
var
  packetHandler: TPacketHandler;
begin
  packetHandler := ConnectionPacketHandlers[ABuffer.ReadByte];
  if packetHandler <> nil then
    packetHandler.Process(ABuffer, ANetState);
end;

procedure OnLoginRequestPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  username, passwordHash: string;
  account: TAccount;
  netState: TNetState;
  invalid: Boolean;
begin
  username := ABuffer.ReadStringNull;
  passwordHash := MD5Print(MD5String(ABuffer.ReadStringNull));
  account := Config.Accounts.Find(username);
  if account <> nil then
  begin
    if account.AccessLevel > alNone then
    begin
      if account.PasswordHash = passwordHash then
      begin
        invalid := False;
        CEDServerInstance.TCPServer.IterReset;
        if CEDServerInstance.TCPServer.Iterator <> nil then
        begin
          repeat
            netState := TNetState(CEDServerInstance.TCPServer.Iterator.UserData);
            if (netState <> nil) and (netState.Account = account) then
            begin
              CEDServerInstance.SendPacket(ANetState, TLoginResponsePacket.Create(lsAlreadyLoggedIn));
              CEDServerInstance.Disconnect(ANetState.Socket);
              invalid := True;
              Break;
            end;
          until not CEDServerInstance.TCPServer.IterNext;
        end;

        if not invalid then
        begin
          Writeln(TimeStamp, 'Login (', username, '): ', ANetState.Socket.PeerAddress);
          ANetState.Account := account;
          CEDServerInstance.SendPacket(ANetState, TLoginResponsePacket.Create(lsOK, account));
          CEDServerInstance.SendPacket(ANetState, TCompressedPacket.Create(
            TClientListPacket.Create(ANetState)));
          CEDServerInstance.SendPacket(nil, TClientConnectedPacket.Create(username));
          CEDServerInstance.SendPacket(ANetState, TSetClientPosPacket.Create(account.LastPos));
        end;
      end else
      begin
        Writeln(TimeStamp, 'Invalid password for ', username);
        CEDServerInstance.SendPacket(ANetState, TLoginResponsePacket.Create(lsInvalidPassword));
        CEDServerInstance.Disconnect(ANetState.Socket);
      end;
    end else
    begin
      Writeln(TimeStamp, 'Access denied for ', username);
      CEDServerInstance.SendPacket(ANetState, TLoginResponsePacket.Create(lsNoAccess));
      CEDServerInstance.Disconnect(ANetState.Socket);
    end;
  end else
  begin
    Writeln(TimeStamp, 'Invalid account specified: ', ANetState.Socket.PeerAddress);
    CEDServerInstance.SendPacket(ANetState, TLoginResponsePacket.Create(lsInvalidUser));
    CEDServerInstance.Disconnect(ANetState.Socket);
  end;
end;

procedure OnQuitPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
begin
  CEDServerInstance.Disconnect(ANetState.Socket);
end;

{ TProtocolVersionPacket }

constructor TProtocolVersionPacket.Create(AVersion: Cardinal);
begin
  inherited Create($02, 0);
  FStream.WriteByte($01);
  FStream.WriteCardinal(AVersion);
end;

{ TLoginResponsePacket }

constructor TLoginResponsePacket.Create(AState: TLoginState;
  AAccount: TAccount = nil);
begin
  inherited Create($02, 0);
  FStream.WriteByte($03);
  FStream.WriteByte(Byte(AState));
  if AState = lsOK then
  begin
    FStream.WriteByte(Byte(AAccount.AccessLevel));
    FStream.WriteWord(Config.Map.Width);
    FStream.WriteWord(Config.Map.Height);
    WriteAccountRestrictions(FStream, AAccount);
  end;
end;

{ TServerStatePacket }

constructor TServerStatePacket.Create(AState: TServerState; AMessage: string = '');
begin
  inherited Create($02, 0);
  FStream.WriteByte($04);
  FStream.WriteByte(Byte(AState));
  if AState = ssOther then
    FStream.WriteStringNull(AMessage);
end;

{$WARNINGS OFF}
var
  i: Integer;

initialization
  for i := 0 to $FF do
    ConnectionPacketHandlers[i] := nil;
  ConnectionPacketHandlers[$03] := TPacketHandler.Create(0, @OnLoginRequestPacket);
  ConnectionPacketHandlers[$05] := TPacketHandler.Create(0, @OnQuitPacket);
finalization
  for i := 0 to $FF do
    if ConnectionPacketHandlers[i] <> nil then
      ConnectionPacketHandlers[i].Free;
{$WARNINGS ON}

end.

