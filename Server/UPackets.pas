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
unit UPackets;

interface

uses
  Classes, dzlib, UEnhancedMemoryStream, UPacket, UMap, UStatics, ULinkedList,
  UNetState;

type
  TBlockCoords = packed record
    X: Word;
    Y: Word;
  end;
  TBlockCoordsArray = array of TBlockCoords;
  
  { TCompressedPacket }

  TCompressedPacket = class(TPacket)
    constructor Create(APacket: TPacket);
  end;
    
  { TSendBlocksPacket }

  TBlockPacket = class(TPacket)
    constructor Create(ACoords: TBlockCoordsArray; ANetState: TNetState);
  end;
  
  { TDrawMapPacket }

  TDrawMapPacket = class(TPacket)
    constructor Create(AMapCell: TMapCell);
  end;
  
  { TInsertStaticPacket }

  TInsertStaticPacket = class(TPacket)
    constructor Create(AStaticItem: TStaticItem);
  end;
  
  { TDeleteStaticPacket }

  TDeleteStaticPacket = class(TPacket)
    constructor Create(AStaticItem: TStaticItem);
  end;
  
  { TElevateStaticPacket }

  TElevateStaticPacket = class(TPacket)
    constructor Create(AStaticItem: TStaticItem; ANewZ: ShortInt);
  end;
  
  { TMoveStaticPacket }

  TMoveStaticPacket = class(TPacket)
    constructor Create(AStaticItem: TStaticItem; ANewX, ANewY: Word);
  end;
  
  { THueStaticPacket }

  THueStaticPacket = class(TPacket)
    constructor Create(AStaticItem: TStaticItem; ANewHue: Word);
  end;

implementation

uses
  UCEDServer;
  
{ TCompressedPacket }

constructor TCompressedPacket.Create(APacket: TPacket);
var
  compBuffer: TEnhancedMemoryStream;
  compStream: TCompressionStream;
  sourceStream: TEnhancedMemoryStream;
begin
  inherited Create($01, 0);
  sourceStream := APacket.Stream;
  compBuffer := TEnhancedMemoryStream.Create;
  compStream := TCompressionStream.Create(clMax, compBuffer);
  compStream.CopyFrom(sourceStream, 0);
  compStream.Free;
  FStream.WriteCardinal(sourceStream.Size);
  FStream.CopyFrom(compBuffer, 0);
  compBuffer.Free;
  APacket.Free;
end;

{ TBlockPacket }

constructor TBlockPacket.Create(ACoords: TBlockCoordsArray; ANetState: TNetState);
var
  i: Integer;
  mapBlock: TMapBlock;
  staticsBlock: TStaticBlock;
  subscriptions: TLinkedList;
begin
  inherited Create($04, 0);
  for i := Low(ACoords) to High(ACoords) do
  begin
    mapBlock := CEDServerInstance.Landscape.GetMapBlock(ACoords[i].X, ACoords[i].Y);
    if mapBlock = nil then Continue;
    mapBlock.GetSize;
    staticsBlock := CEDServerInstance.Landscape.GetStaticBlock(ACoords[i].X, ACoords[i].Y);
    if staticsBlock = nil then Continue;
    staticsBlock.GetSize;
    
    FStream.Write(ACoords[i], SizeOf(TBlockCoords));
    mapBlock.Write(FStream);
    FStream.WriteWord(staticsBlock.Items.Count);
    staticsBlock.Write(FStream);

    if ANetState <> nil then
    begin
      subscriptions := CEDServerInstance.Landscape.BlockSubscriptions[ACoords[i].X, ACoords[i].Y];
      subscriptions.Delete(ANetState);
      subscriptions.Add(Integer(ANetState), ANetState);
      if ANetState.Subscriptions.IndexOf(subscriptions) = -1 then
        ANetState.Subscriptions.Add(subscriptions);
    end;
  end;
end;

{ TDrawMapPacket }

constructor TDrawMapPacket.Create(AMapCell: TMapCell);
begin
  inherited Create($06, 8);
  FStream.WriteWord(AMapCell.X);
  FStream.WriteWord(AMapCell.Y);
  FStream.WriteShortInt(AMapCell.Altitude);
  FStream.WriteWord(AMapCell.TileID);
end;

{ TInsertStaticPacket }

constructor TInsertStaticPacket.Create(AStaticItem: TStaticItem);
begin
  inherited Create($07, 10);
  FStream.WriteWord(AStaticItem.X);
  FStream.WriteWord(AStaticItem.Y);
  FStream.WriteShortInt(AStaticItem.Z);
  FStream.WriteWord(AStaticItem.TileID);
  FStream.WriteWord(AStaticItem.Hue);
end;

{ TDeleteStaticPacket }

constructor TDeleteStaticPacket.Create(AStaticItem: TStaticItem);
begin
  inherited Create($08, 10);
  FStream.WriteWord(AStaticItem.X);
  FStream.WriteWord(AStaticItem.Y);
  FStream.WriteShortInt(AStaticItem.Z);
  FStream.WriteWord(AStaticItem.TileID);
  FStream.WriteWord(AStaticItem.Hue);
end;

{ TElevateStaticPacket }

constructor TElevateStaticPacket.Create(AStaticItem: TStaticItem; ANewZ: ShortInt);
begin
  inherited Create($09, 11);
  FStream.WriteWord(AStaticItem.X);
  FStream.WriteWord(AStaticItem.Y);
  FStream.WriteShortInt(AStaticItem.Z);
  FStream.WriteWord(AStaticItem.TileID);
  FStream.WriteWord(AStaticItem.Hue);
  FStream.WriteShortInt(ANewZ);
end;

{ TMoveStaticPacket }

constructor TMoveStaticPacket.Create(AStaticItem: TStaticItem; ANewX,
  ANewY: Word);
begin
  inherited Create($0A, 14);
  FStream.WriteWord(AStaticItem.X);
  FStream.WriteWord(AStaticItem.Y);
  FStream.WriteShortInt(AStaticItem.Z);
  FStream.WriteWord(AStaticItem.TileID);
  FStream.WriteWord(AStaticItem.Hue);
  FStream.WriteWord(ANewX);
  FStream.WriteWord(ANewY);
end;

{ THueStaticPacket }

constructor THueStaticPacket.Create(AStaticItem: TStaticItem; ANewHue: Word);
begin
  inherited Create($0B, 12);
  FStream.WriteWord(AStaticItem.X);
  FStream.WriteWord(AStaticItem.Y);
  FStream.WriteShortInt(AStaticItem.Z);
  FStream.WriteWord(AStaticItem.TileID);
  FStream.WriteWord(AStaticItem.Hue);
  FStream.WriteWord(ANewHue);
end;

end.

