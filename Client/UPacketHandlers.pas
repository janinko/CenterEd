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
  SysUtils, dzlib, UEnhancedMemoryStream;

type
  TPacketProcessor = procedure(ABuffer: TEnhancedMemoryStream);
  TPacketProcessorMethod = procedure(ABuffer: TEnhancedMemoryStream) of object;

  { TPacketHandler }

  TPacketHandler = class(TObject)
    constructor Create(ALength: Cardinal; APacketProcessor: TPacketProcessor); overload;
    constructor Create(ALength: Cardinal; APacketProcessorMethod: TPacketProcessorMethod); overload;
    procedure Process(ABuffer: TEnhancedMemoryStream);
  protected
    FLength: Cardinal;
    FPacketProcessor: TPacketProcessor;
    FPacketProcessorMethod: TPacketProcessorMethod;
  published
    property PacketLength: Cardinal read FLength;
  end;

var
  PacketHandlers: array[0..$FF] of TPacketHandler;
  
procedure RegisterPacketHandler(AID: Byte; APacketHandler: TPacketHandler);

implementation

uses
  UAdminHandling;

procedure RegisterPacketHandler(AID: Byte; APacketHandler: TPacketHandler);
begin
  FreeAndNil(PacketHandlers[AID]);
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

procedure TPacketHandler.Process(ABuffer: TEnhancedMemoryStream);
begin
  if Assigned(FPacketProcessor) then
    FPacketProcessor(ABuffer)
  else if Assigned(FPacketProcessorMethod) then
    FPacketProcessorMethod(ABuffer);
end;

procedure OnCompressedPacket(ABuffer: TEnhancedMemoryStream);
var
  uncompStream: TEnhancedMemoryStream;
  uncompBuffer: TDecompressionStream;
  targetSize: Cardinal;
  packetID: Byte;
begin
  //writeln('compressed size: ', ABuffer.Size);
  targetSize := ABuffer.ReadCardinal;
  //writeln('uncompressed size: ', targetSize);
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
      PacketHandlers[PacketID].Process(uncompStream);
      uncompStream.Unlock;
    end else
    begin
      {Writeln('Dropping client due to unknown packet: ', ANetState.Socket.PeerAddress);
      ANetState.Socket.Disconnect;
      ANetState.ReceiveQueue.Clear;}
    end;
  finally
    if uncompBuffer <> nil then uncompBuffer.Free;
    if uncompStream <> nil then uncompStream.Free;
  end;
end;


{$WARNINGS OFF}
var
  i: Integer;

initialization
  for i := 0 to $FF do
    PacketHandlers[i] := nil;
  PacketHandlers[$01] := TPacketHandler.Create(0, @OnCompressedPacket);
  //$02 --> ConnectionHandling, done by TdmNetwork
  PacketHandlers[$03] := TPacketHandler.Create(0, @OnAdminHandlerPacket);;
  //$04 --> handled by TLandscape
  //$06-$0B --> handled by TLandscape
  //$0C --> ClientHandling, done by TfrmMain
  //$0D --> RadarMapHandling, done by TfrmRadarMap
  //$0E --> LargeScaleCommands, done by TfrmLargeScaleCommands
finalization
  for i := 0 to $FF do
    if PacketHandlers[i] <> nil then
      PacketHandlers[i].Free;
{$WARNINGS ON}
end.

