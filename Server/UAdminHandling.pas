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
unit UAdminHandling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UPacket, UPacketHandlers, UConfig, UAccount, UNetState,
  UEnhancedMemoryStream, UEnums, lNet;
  
type

  { TModifyUserResponsePacket }

  TModifyUserResponsePacket = class(TPacket)
    constructor Create(AStatus: TModifyUserStatus; AAccount: TAccount);
  end;
  
  { TDeleteUserResponsePacket }

  TDeleteUserResponsePacket = class(TPacket)
    constructor Create(AStatus: TDeleteUserStatus; AUsername: string);
  end;
  
  { TUserListPacket }

  TUserListPacket = class(TPacket)
    constructor Create;
  end;
  
procedure OnAdminHandlerPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnFlushPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnQuitPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnModifyUserPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnDeleteUserPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnListUsersPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);

var
  AdminPacketHandlers: array[0..$FF] of TPacketHandler;

implementation

uses
  md5, UCEDServer, UPackets, UClientHandling;

procedure OnAdminHandlerPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
var
  packetHandler: TPacketHandler;
begin
  if not ValidateAccess(ANetState, alAdministrator) then Exit;
  packetHandler := AdminPacketHandlers[ABuffer.ReadByte];
  if packetHandler <> nil then
    packetHandler.Process(ABuffer, ANetState);
end;

procedure OnFlushPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
begin
  CEDServerInstance.Landscape.Flush;
  Config.Flush;
end;

procedure OnQuitPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
begin
  CEDServerInstance.Quit := True;
end;

procedure OnModifyUserPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  account: TAccount;
  username, password: string;
  accessLevel: TAccessLevel;
  netState: TNetState;
begin
  username := ABuffer.ReadStringNull;
  password := ABuffer.ReadStringNull;
  accessLevel := TAccessLevel(ABuffer.ReadByte);
  account := Config.Accounts.Find(username);
  if account <> nil then
  begin
    if password <> '' then
      account.PasswordHash := MD5Print(MD5String(password));
    if account.AccessLevel <> accessLevel then
    begin
      account.AccessLevel := accessLevel;
      CEDServerInstance.TCPServer.IterReset;
      while CEDServerInstance.TCPServer.IterNext do
      begin
        netState := TNetState(CEDServerInstance.TCPServer.Iterator.UserData);
        if (netState <> nil) and (netState.Account = account) then
        begin
          CEDServerInstance.SendPacket(netState, TAccessLevelChangedPacket.Create(accessLevel));
        end;
      end;
    end;
    CEDServerInstance.SendPacket(ANetState, TModifyUserResponsePacket.Create(muModified, account));
  end else
  begin
    account := TAccount.Create(Config.Accounts, username,
      MD5Print(MD5String(password)), accessLevel);
    if (username = '') or (Pos('=', username) > 0) then
    begin
      CEDServerInstance.SendPacket(ANetState, TModifyUserResponsePacket.Create(muInvalidUsername, account));
      account.Free;
      Exit;
    end;
    Config.Accounts.Add(account);
    Config.Invalidate;
    CEDServerInstance.SendPacket(ANetState, TModifyUserResponsePacket.Create(muAdded, account));
  end;
end;

procedure OnDeleteUserPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  account: TAccount;
  username: string;
  netState: TNetState;
begin
  username := ABuffer.ReadStringNull;
  account := Config.Accounts.Find(username);
  if (account <> nil) and (account <> ANetState.Account) then
  begin
    CEDServerInstance.TCPServer.IterReset;
    while CEDServerInstance.TCPServer.IterNext do
    begin
      netState := TNetState(CEDServerInstance.TCPServer.Iterator.UserData);
      if (netState <> nil) and (netState.Account = account) then
      begin
        CEDServerInstance.Disconnect(CEDServerInstance.TCPServer.Iterator);
        netState.Account := nil;
      end;
    end;
    Config.Accounts.Remove(account);
    Config.Invalidate;
    CEDServerInstance.SendPacket(ANetState, TDeleteUserResponsePacket.Create(duDeleted, username));
  end else
    CEDServerInstance.SendPacket(ANetState, TDeleteUserResponsePacket.Create(duNotFound, username));
end;

procedure OnListUsersPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
begin
  CEDServerInstance.SendPacket(ANetState, TCompressedPacket.Create(TUserListPacket.Create));
end;

{ TModifyUserResponsePacket }

constructor TModifyUserResponsePacket.Create(AStatus: TModifyUserStatus; AAccount: TAccount);
begin
  inherited Create($03, 0);
  FStream.WriteByte($05);
  FStream.WriteByte(Byte(AStatus));
  FStream.WriteStringNull(AAccount.Name);
  FStream.WriteByte(Byte(AAccount.AccessLevel));
end;

{ TDeleteUserResponsePacket }

constructor TDeleteUserResponsePacket.Create(AStatus: TDeleteUserStatus; AUsername: string);
begin
  inherited Create($03, 0);
  FStream.WriteByte($06);
  FStream.WriteByte(Byte(AStatus));
  FStream.WriteStringNull(AUsername);
end;

{ TUserListPacket }

constructor TUserListPacket.Create;
var
  i: Integer;
  account: TAccount;
begin
  inherited Create($03, 0);
  FStream.WriteByte($07);
  FStream.WriteWord(Config.Accounts.Count);
  for i := 0 to Config.Accounts.Count - 1 do
  begin
    account := TAccount(Config.Accounts.Items[i]);
    FStream.WriteStringNull(account.Name);
    FStream.WriteByte(Byte(account.AccessLevel));
  end;
end;

{$WARNINGS OFF}
var
  i: Integer;

initialization
  for i := 0 to $FF do
    AdminPacketHandlers[i] := nil;
  AdminPacketHandlers[$01] := TPacketHandler.Create(0, @OnFlushPacket);
  AdminPacketHandlers[$02] := TPacketHandler.Create(0, @OnQuitPacket);
  AdminPacketHandlers[$05] := TPacketHandler.Create(0, @OnModifyUserPacket);
  AdminPacketHandlers[$06] := TPacketHandler.Create(0, @OnDeleteUserPacket);
  AdminPacketHandlers[$07] := TPacketHandler.Create(0, @OnListUsersPacket);
finalization
  for i := 0 to $FF do
    if AdminPacketHandlers[i] <> nil then
      AdminPacketHandlers[i].Free;
{$WARNINGS ON}

end.

