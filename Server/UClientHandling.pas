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
unit UClientHandling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UPacket, UPacketHandlers, UConfig, UAccount, UNetState,
  UEnhancedMemoryStream, UEnums, Math;
  
type

  { TClientConnectedPacket }

  TClientConnectedPacket = class(TPacket)
    constructor Create(AUsername: string);
  end;
  
  { TClientDisconnectedPacket }

  TClientDisconnectedPacket = class(TPacket)
    constructor Create(AUsername: string);
  end;
  
  { TClientListPacket }

  TClientListPacket = class(TPacket)
    constructor Create(AAvoid: TNetState = nil);
  end;
  
  { TSetClientPosPacket }

  TSetClientPosPacket = class(TPacket)
    constructor Create(APos: TPoint);
  end;
  
  { TChatMessagePacket }

  TChatMessagePacket = class(TPacket)
    constructor Create(ASender, AMessage: string);
  end;
  
  { TAccessChangedPacket }

  TAccessChangedPacket = class(TPacket)
    constructor Create(AAccount: TAccount);
  end;

  { TPasswordChangeStatusPacket }

  TPasswordChangeStatusPacket = class(TPacket)
    constructor Create(AResult: TPasswordChangeStatus);
  end;

procedure OnClientHandlerPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
procedure OnUpdateClientPosPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
procedure OnChatMessagePacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
procedure OnGotoClientPosPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
procedure OnChangePasswordPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);

procedure WriteAccountRestrictions(AStream: TEnhancedMemoryStream;
  AAccount: TAccount);

var
  ClientPacketHandlers: array[0..$FF] of TPacketHandler;

implementation

uses
  UCEDServer, UPackets, URegions;

procedure OnClientHandlerPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  packetHandler: TPacketHandler;
begin
  if not ValidateAccess(ANetState, alView) then Exit;
  packetHandler := ClientPacketHandlers[ABuffer.ReadByte];
  if packetHandler <> nil then
    packetHandler.Process(ABuffer, ANetState);
end;

procedure OnUpdateClientPosPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  pos: TPoint;
begin
  pos.x := ABuffer.ReadWord;
  pos.y := ABuffer.ReadWord;
  ANetState.Account.LastPos := pos;
end;

procedure OnChatMessagePacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
begin
  CEDServerInstance.SendPacket(nil, TCompressedPacket.Create(
    TChatMessagePacket.Create(ANetState.Account.Name, ABuffer.ReadStringNull)));
end;

procedure OnGotoClientPosPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  account: TAccount;
begin
  account := Config.Accounts.Find(ABuffer.ReadStringNull);
  if account <> nil then
    CEDServerInstance.SendPacket(ANetState,
      TSetClientPosPacket.Create(account.LastPos));
end;

procedure OnChangePasswordPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  oldPwd, newPwd: String;
begin
  oldPwd := ABuffer.ReadStringNull;
  newPwd := ABuffer.ReadStringNull;
  if ANetState.Account.CheckPassword(oldPwd) then
  begin
    //Check if the passwords actually differ. Changing them isn't allowed
    //otherwise. Might be open for configuration, though.
    if oldPwd <> newPwd then
    begin
      //Just a simple restriction to disallow too easy passwords.
      //TODO: Configurable restrictions
      if Length(newPwd) >= 4 then
      begin
        //Everything fine, update the password and report success.
        ANetState.Account.UpdatePassword(newPwd);
        CEDServerInstance.SendPacket(ANetState,
          TPasswordChangeStatusPacket.Create(pcSuccess));
      end else
      begin
        CEDServerInstance.SendPacket(ANetState,
          TPasswordChangeStatusPacket.Create(pcNewPwInvalid));
      end;
    end else
    begin
      CEDServerInstance.SendPacket(ANetState,
        TPasswordChangeStatusPacket.Create(pcIdentical));
    end;
  end else
  begin
    CEDServerInstance.SendPacket(ANetState,
      TPasswordChangeStatusPacket.Create(pcOldPwInvalid));
  end;
end;

procedure WriteAccountRestrictions(AStream: TEnhancedMemoryStream;
  AAccount: TAccount);
var
  areaCount: Word;
  i, j, offset, newOffset: Integer;
  region: TRegion;
  area: TRect;
begin
  offset := AStream.Position;
  areaCount := 0;
  AStream.WriteWord(areaCount);

  if AAccount.AccessLevel >= alAdministrator then
    Exit; //Admins shouldn't have restrictions anyway

  for i := 0 to AAccount.Regions.Count - 1 do
  begin
    region := Config.Regions.Find(AAccount.Regions.Strings[i]);
    if region <> nil then
      for j := 0 to region.Areas.Count - 1 do
      begin
        area := region.Areas.Rects[j];
        AStream.WriteWord(area.Left);
        AStream.WriteWord(area.Top);
        AStream.WriteWord(area.Right);
        AStream.WriteWord(area.Bottom);
        Inc(areaCount);
      end;
  end;
  if areaCount > 0 then
  begin
    newOffset := AStream.Position;
    AStream.Position := offset;
    AStream.WriteWord(areaCount);
    AStream.Position := newOffset;
  end;
end;

{ TClientConnectedPacket }

constructor TClientConnectedPacket.Create(AUsername: string);
begin
  inherited Create($0C, 0);
  FStream.WriteByte($01);
  FStream.WriteStringNull(AUsername);
end;

{ TClientDisconnectedPacket }

constructor TClientDisconnectedPacket.Create(AUsername: string);
begin
  inherited Create($0C, 0);
  FStream.WriteByte($02);
  FStream.WriteStringNull(AUsername);
end;

{ TClientListPacket }

constructor TClientListPacket.Create(AAvoid: TNetState = nil);
var
  netState: TNetState;
begin
  inherited Create($0C, 0);
  FStream.WriteByte($03);
  CEDServerInstance.TCPServer.IterReset;
  if CEDServerInstance.TCPServer.Iterator <> nil then
  begin
    repeat
      netState := TNetState(CEDServerInstance.TCPServer.Iterator.UserData);
      if (netState <> nil) and (netState <> AAvoid) and
        (netState.Account <> nil) then
        FStream.WriteStringNull(netState.Account.Name);
    until not CEDServerInstance.TCPServer.IterNext;
  end;
end;

{ TSetClientPosPacket }

constructor TSetClientPosPacket.Create(APos: TPoint);
begin
  inherited Create($0C, 0);
  FStream.WriteByte($04);
  FStream.WriteWord(EnsureRange(APos.x, 0, CEDServerInstance.Landscape.CellWidth - 1));
  FStream.WriteWord(EnsureRange(APos.y, 0, CEDServerInstance.Landscape.CellHeight - 1));
end;

{ TChatMessagePacket }

constructor TChatMessagePacket.Create(ASender, AMessage: string);
begin
  inherited Create($0C, 0);
  FStream.WriteByte($05);
  FStream.WriteStringNull(ASender);
  FStream.WriteStringNull(AMessage);
end;

{ TAccessChangedPacket }

constructor TAccessChangedPacket.Create(AAccount: TAccount);
begin
  inherited Create($0C, 0);
  FStream.WriteByte($07);
  FStream.WriteByte(Byte(AAccount.AccessLevel));
  WriteAccountRestrictions(FStream, AAccount);
end;

{ TPasswordChangeStatusPacket }

constructor TPasswordChangeStatusPacket.Create(AResult: TPasswordChangeStatus);
begin
  inherited Create($0C, 0);
  FStream.WriteByte($08);
  FStream.WriteByte(Byte(AResult));
end;

{$WARNINGS OFF}
var
  i: Integer;

initialization
  for i := 0 to $FF do
    ClientPacketHandlers[i] := nil;
  ClientPacketHandlers[$04] := TPacketHandler.Create(0, @OnUpdateClientPosPacket);
  ClientPacketHandlers[$05] := TPacketHandler.Create(0, @OnChatMessagePacket);
  ClientPacketHandlers[$06] := TPacketHandler.Create(0, @OnGotoClientPosPacket);
  ClientPacketHandlers[$08] := TPacketHandler.Create(0, @OnChangePasswordPacket);
finalization
  for i := 0 to $FF do
    if ClientPacketHandlers[i] <> nil then
      ClientPacketHandlers[i].Free;
{$WARNINGS ON}

end.

