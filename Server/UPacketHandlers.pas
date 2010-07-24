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
 *      Portions Copyright 2007 Andreas Schneider
 *)
unit UPacketHandlers;

interface

uses
  Classes, SysUtils, dzlib, UConfig, UNetState, UEnhancedMemoryStream, UEnums,
  ULinkedList, URegions;

type
  TPacketProcessor = procedure(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
  TPacketProcessorMethod = procedure(ABuffer: TEnhancedMemoryStream; ANetState: TNetState) of object;

  { TPacketHandler }

  TPacketHandler = class(TObject)
    constructor Create(ALength: Cardinal; APacketProcessor: TPacketProcessor); overload;
    constructor Create(ALength: Cardinal; APacketProcessorMethod: TPacketProcessorMethod); overload;
    procedure Process(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
  protected
    FLength: Cardinal;
    FPacketProcessor: TPacketProcessor;
    FPacketProcessorMethod: TPacketProcessorMethod;
  published
    property PacketLength: Cardinal read FLength;
  end;

var
  PacketHandlers: array[0..$FF] of TPacketHandler;
  
function ValidateAccess(ANetState: TNetState; ALevel: TAccessLevel): Boolean; overload;
function ValidateAccess(ANetState: TNetState; ALevel: TAccessLevel; AX, AY: Cardinal): Boolean; overload;
procedure RegisterPacketHandler(AID: Byte; APacketHandler: TPacketHandler);

implementation

uses
  UCEDServer, UPackets, UConnectionHandling, UAdminHandling, UClientHandling;

function ValidateAccess(ANetState: TNetState; ALevel: TAccessLevel): Boolean;
begin
  Result := (ANetState.Account <> nil) and (ANetState.Account.AccessLevel >= ALevel);
end;

function ValidateAccess(ANetState: TNetState; ALevel: TAccessLevel; AX, AY: Cardinal): Boolean;
var
  i,j: Word;
  region: TRegion;
  rect: TRect;
begin
  if not ValidateAccess(ANetState, ALevel) then Exit(False);
  if (ANetState.Account.Regions.Count = 0) or
    (ANetState.Account.AccessLevel >= alAdministrator) then Exit(True); //no restrictions

  Result := False;
  for i := 0 to ANetState.Account.Regions.Count - 1 do
  begin
    region := Config.Regions.Find(ANetState.Account.Regions[i]);
    if region <> nil then
    begin
      for j := 0 to region.Areas.Count - 1 do
      begin
        rect := region.Areas.Rects[j];
        if (AX >= rect.Left) and
           (AX < rect.Right) and
           (AY >= rect.Top) and
           (AY < rect.Bottom) then
          Exit(True);
      end;
    end;
  end;
end;

procedure RegisterPacketHandler(AID: Byte; APacketHandler: TPacketHandler);
begin
  if Assigned(PacketHandlers[AID]) then FreeAndNil(PacketHandlers[AID]);
  PacketHandlers[AID] := APacketHandler;
end;

{ TPacketHandler }

constructor TPacketHandler.Create(ALength: Cardinal; APacketProcessor: TPacketProcessor);
begin
  inherited Create;
  FLength := ALength;
  FPacketProcessor := APacketProcessor;
  FPacketProcessorMethod := nil;
end;

constructor TPacketHandler.Create(ALength: Cardinal;
  APacketProcessorMethod: TPacketProcessorMethod);
begin
  inherited Create;
  FLength := ALength;
  FPacketProcessor := nil;
  FPacketProcessorMethod := APacketProcessorMethod;
end;

procedure TPacketHandler.Process(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
begin
  if Assigned(FPacketProcessor) then
    FPacketProcessor(ABuffer, ANetState)
  else if Assigned(FPacketProcessorMethod) then
    FPacketProcessorMethod(ABuffer, ANetState);
end;

procedure OnCompressedPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
var
  uncompStream: TEnhancedMemoryStream;
  uncompBuffer: TDecompressionStream;
  targetSize: Cardinal;
  packetID: Byte;
begin
  targetSize := ABuffer.ReadCardinal;
  uncompBuffer := TDecompressionStream.Create(ABuffer);
  uncompStream := TEnhancedMemoryStream.Create;
  try
    uncompStream.CopyFrom(uncompBuffer, targetSize);
    uncompStream.Position := 0;
    packetID := uncompStream.ReadByte;
    if PacketHandlers[packetID] <> nil then
    begin
      if PacketHandlers[PacketID].PacketLength = 0 then
        uncompStream.Position := uncompStream.Position + 4;
      uncompStream.Lock(uncompStream.Position, uncompStream.Size - uncompStream.Position);
      PacketHandlers[PacketID].Process(uncompStream, ANetState);
      uncompStream.Unlock;
    end else
    begin
      Writeln(TimeStamp, 'Dropping client due to unknown packet: ', ANetState.Socket.PeerAddress);
      ANetState.ReceiveQueue.Clear;
      CEDServerInstance.Disconnect(ANetState.Socket);
    end;
  finally
    if uncompBuffer <> nil then uncompBuffer.Free;
    if uncompStream <> nil then uncompStream.Free;
  end;
end;

procedure OnRequestBlocksPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
var
  coords: TBlockCoordsArray;
begin
  if not ValidateAccess(ANetState, alView) then Exit;
  SetLength(coords, (ABuffer.Size - ABuffer.Position) div SizeOf(TBlockCoords));
  ABuffer.Read(coords[0], Length(coords) * SizeOf(TBlockCoords));
  CEDServerInstance.SendPacket(ANetState, TCompressedPacket.Create(TBlockPacket.Create(coords, ANetState)));
end;

procedure OnFreeBlockPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
var
  x, y: Word;
  blockSubscriptions: TLinkedList;
begin
  if not ValidateAccess(ANetState, alView) then Exit;
  x := ABuffer.ReadWord;
  y := ABuffer.ReadWord;
  blockSubscriptions := CEDServerInstance.Landscape.BlockSubscriptions[X, Y];
  if blockSubscriptions <> nil then
  begin
    blockSubscriptions.Delete(ANetState);
    ANetState.Subscriptions.Remove(blockSubscriptions);
  end;
end;

procedure OnNoOpPacket(ABuffer: TEnhancedMemoryStream; ANetState: TNetState);
begin
  //no operation
end;

{$WARNINGS OFF}
var
  i: Integer;

initialization
  for i := 0 to $FF do
    PacketHandlers[i] := nil;
  PacketHandlers[$01] := TPacketHandler.Create(0, @OnCompressedPacket);
  PacketHandlers[$02] := TPacketHandler.Create(0, @OnConnectionHandlerPacket);
  PacketHandlers[$03] := TPacketHandler.Create(0, @OnAdminHandlerPacket);
  PacketHandlers[$04] := TPacketHandler.Create(0, @OnRequestBlocksPacket);
  PacketHandlers[$05] := TPacketHandler.Create(5, @OnFreeBlockPacket);
  //$06-$0B handled by landscape
  PacketHandlers[$0C] := TPacketHandler.Create(0, @OnClientHandlerPacket);
  //$0D handled by radarmap
  //$0E handled by landscape
  PacketHandlers[$FF] := TPacketHandler.Create(1, @OnNoOpPacket);
finalization
  for i := 0 to $FF do
    if PacketHandlers[i] <> nil then
      PacketHandlers[i].Free;
{$WARNINGS ON}
end.

