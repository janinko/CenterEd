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
  Classes, SysUtils, math, UPacket, UPacketHandlers, UConfig, UAccount,
  UNetState, UEnhancedMemoryStream, UEnums, URegions;

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

  { TModifyRegionResponsePacket }

  TModifyRegionResponsePacket = class(TPacket)
    constructor Create(AStatus: TModifyRegionStatus; ARegion: TRegion);
  end;

  { TDeleteRegionResponsePacket }

  TDeleteRegionResponsePacket = class(TPacket)
    constructor Create(AStatus: TDeleteRegionStatus; ARegionName: string);
  end;

  { TUserRegionsPacket }

  TRegionListPacket = class(TPacket)
    constructor Create;
  end;

procedure OnAdminHandlerPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnFlushPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnQuitPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnModifyUserPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnDeleteUserPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnListUsersPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnModifyRegionPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnDeleteRegionPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
procedure OnListRegionsPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);


var
  AdminPacketHandlers: array[0..$FF] of TPacketHandler;

implementation

uses
  md5, UCEDServer, UPackets, UClientHandling;

procedure AdminBroadcast(AAccessLevel: TAccessLevel; APacket: TPacket);
var
  netState: TNetState;
begin
  CEDServerInstance.TCPServer.IterReset;
  while CEDServerInstance.TCPServer.IterNext do
  begin
    netState := TNetState(CEDServerInstance.TCPServer.Iterator.UserData);
    if (netState <> nil) and (netState.Account.AccessLevel >= AAccessLevel) then
      CEDServerInstance.SendPacket(netState, APacket, False);
  end;
  APacket.Free;
end;

procedure OnAdminHandlerPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
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
  regions: TStringList;
  i, regionCount: Integer;
begin
  username := ABuffer.ReadStringNull;
  password := ABuffer.ReadStringNull;
  accessLevel := TAccessLevel(ABuffer.ReadByte);

  regionCount := ABuffer.ReadByte;

  account := Config.Accounts.Find(username);
  if account <> nil then
  begin
    if password <> '' then
      account.PasswordHash := MD5Print(MD5String(password));

    account.Regions.Clear;
    for i := 0 to regionCount - 1 do
      account.Regions.Add(ABuffer.ReadStringNull);
    account.Invalidate;

    if account.AccessLevel <> accessLevel then
    begin
      account.AccessLevel := accessLevel;
      CEDServerInstance.TCPServer.IterReset;
      while CEDServerInstance.TCPServer.IterNext do
      begin
        netState := TNetState(CEDServerInstance.TCPServer.Iterator.UserData);
        if (netState <> nil) and (netState.Account = account) then
        begin
          CEDServerInstance.SendPacket(netState,
            TAccessLevelChangedPacket.Create(accessLevel));
        end;
      end;
    end;
    CEDServerInstance.SendPacket(ANetState,
      TModifyUserResponsePacket.Create(muModified, account));
  end else
  begin
    if username = '' then
    begin
      CEDServerInstance.SendPacket(ANetState,
        TModifyUserResponsePacket.Create(muInvalidUsername, account));
      Exit;
    end else
    begin
      regions := TStringList.Create;
      for i := 0 to regionCount - 1 do
        regions.Add(ABuffer.ReadStringNull);

      account := TAccount.Create(Config.Accounts, username,
        MD5Print(MD5String(password)), accessLevel, regions);

      Config.Accounts.Add(account);
      Config.Accounts.Invalidate;
      CEDServerInstance.SendPacket(ANetState,
        TModifyUserResponsePacket.Create(muAdded, account));
    end;
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
    CEDServerInstance.SendPacket(ANetState,
      TDeleteUserResponsePacket.Create(duDeleted, username));
  end else
    CEDServerInstance.SendPacket(ANetState,
      TDeleteUserResponsePacket.Create(duNotFound, username));
end;

procedure OnListUsersPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
begin
  CEDServerInstance.SendPacket(ANetState,
    TCompressedPacket.Create(TUserListPacket.Create));
end;

procedure OnModifyRegionPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  regionName: string;
  region: TRegion;
  status: TModifyRegionStatus;
  i, areaCount: Integer;
  x1, y1, x2, y2: Word;
begin
  regionName := ABuffer.ReadStringNull;

  region := Config.Regions.Find(regionName);
  if region = nil then
  begin
    region := TRegion.Create(Config.Regions, regionName);
    Config.Regions.Add(region);
    status := mrAdded;
  end else
  begin
    region.Areas.Clear;
    status := mrModified;
  end;

  areaCount := ABuffer.ReadByte;
  for i := 0 to areaCount - 1 do
  begin
    x1 := ABuffer.ReadWord;
    y1 := ABuffer.ReadWord;
    x2 := ABuffer.ReadWord;
    y2 := ABuffer.ReadWord;
    region.Areas.Add(Min(x1, x2), Min(y1, y2),
                     Max(x1, x2), Max(y1, y2));
  end;

  Config.Regions.Invalidate;

  AdminBroadcast(alAdministrator,
    TModifyRegionResponsePacket.Create(status, region));
end;

procedure OnDeleteRegionPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  regionName: string;
  regions: TRegionList;
  i: Integer;
  status: TDeleteRegionStatus;
begin
  regionName := ABuffer.ReadStringNull;
  i := 0;
  status := drNotFound;
  regions := Config.Regions;
  while (i < regions.Count) and (status = drNotFound) do
  begin
    if TRegion(regions[i]).Name = regionName then
    begin
      regions.Delete(i);
      regions.Invalidate;
      status := drDeleted;
    end else
      inc(i);
  end;

  AdminBroadcast(alAdministrator,
    TDeleteRegionResponsePacket.Create(status, regionName));
end;

procedure OnListRegionsPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
begin
  CEDServerInstance.SendPacket(ANetState,
    TCompressedPacket.Create(TRegionListPacket.Create));
end;

{ TModifyUserResponsePacket }

constructor TModifyUserResponsePacket.Create(AStatus: TModifyUserStatus;
  AAccount: TAccount);
var
  i: Integer;
begin
  inherited Create($03, 0);
  FStream.WriteByte($05);
  FStream.WriteByte(Byte(AStatus));
  FStream.WriteStringNull(AAccount.Name);
  if (AStatus = muAdded) or (AStatus = muModified) then
  begin
    FStream.WriteByte(Byte(AAccount.AccessLevel));
    FStream.WriteByte(AAccount.Regions.Count);
    for i := 0 to AAccount.Regions.Count - 1 do
      FStream.WriteStringNull(AAccount.Regions[i]);
  end;
  {TODO : check for client side modifications!}
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
  i, j: Integer;
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
    FStream.WriteByte(account.Regions.Count);
    for j := 0 to account.Regions.Count - 1 do
      FStream.WriteStringNull(account.Regions[j]);
  end;
end;

{ TModifyRegionResponsePacket }

constructor TModifyRegionResponsePacket.Create(AStatus: TModifyRegionStatus;
  ARegion: TRegion);
var
  i, areaCount: Integer;
begin
  inherited Create($03, 0);
  FStream.WriteByte($08);
  FStream.WriteByte(Byte(AStatus));
  FStream.WriteStringNull(ARegion.Name);
  if (AStatus = mrAdded) or (AStatus = mrModified) then
  begin
    areaCount := ARegion.Areas.Count;
    FStream.WriteByte(areaCount);
    for i := 0 to areaCount - 1 do
      with ARegion.Areas.Rects[i] do
      begin
        FStream.WriteWord(Left);
        FStream.WriteWord(Top);
        FStream.WriteWord(Right);
        FStream.WriteWord(Bottom);
      end;
  end;
end;

{ TDeleteRegionResponsePacket }

constructor TDeleteRegionResponsePacket.Create(AStatus: TDeleteRegionStatus;
  ARegionName: string);
begin
  inherited Create($03, 0);
  FStream.WriteByte($09);
  FStream.WriteByte(Byte(AStatus));
  FStream.WriteStringNull(ARegionName);
end;

{ TRegionListPacket }

constructor TRegionListPacket.Create;
var
  i, j: Integer;
  region: TRegion;
begin
  inherited Create($03, 0);
  FStream.WriteByte($0A);
  FStream.WriteByte(Config.Regions.Count);
  for i := 0 to Config.Regions.Count - 1 do
  begin
    region := TRegion(Config.Regions.Items[i]);
    FStream.WriteStringNull(region.Name);
    FStream.WriteByte(region.Areas.Count);
    for j := 0 to region.Areas.Count - 1 do
      with region.Areas.Rects[j] do
      begin
        FStream.WriteWord(Left);
        FStream.WriteWord(Top);
        FStream.WriteWord(Right);
        FStream.WriteWord(Bottom);
      end;
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
  AdminPacketHandlers[$08] := TPacketHandler.Create(0, @OnModifyRegionPacket);
  AdminPacketHandlers[$09] := TPacketHandler.Create(0, @OnDeleteRegionPacket);
  AdminPacketHandlers[$0A] := TPacketHandler.Create(0, @OnListRegionsPacket);
finalization
  for i := 0 to $FF do
    if AdminPacketHandlers[i] <> nil then
      AdminPacketHandlers[i].Free;
{$WARNINGS ON}

end.

