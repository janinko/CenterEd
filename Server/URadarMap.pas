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
unit URadarMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UConfig, UNetState, UEnhancedMemoryStream, UEnums;
  
type

  TRadarColorArray = array of Word;

  { TRadarMap }

  TRadarMap = class(TObject)
    constructor Create(AMap, AStatics, AStaIdx: TStream; AWidth, AHeight: Word;
      ARadarCol: string);
    destructor Destroy; override;
  protected
    FWidth: Word;
    FHeight: Word;
    FRadarColors: TRadarColorArray;
    FRadarMap: TRadarColorArray;
    FPackets: TList;
    FPacketSize: Cardinal;
    procedure OnRadarHandlingPacket(ABuffer: TEnhancedMemoryStream;
      ANetState: TNetState);
  public
    procedure Update(AX, AY, ATileID: Word);
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

implementation

uses
  UPacket, UPackets, UPacketHandlers, UCEDServer, crc;

type
  TMulIndex = packed record
    Position: Cardinal;
    Size: Cardinal;
    Userdata: Cardinal;
  end;
  TMapCell = packed record
    TileID: Word;
    Altitude: ShortInt;
  end;
  TStaticItem = packed record
    TileID: Word;
    X, Y: Byte;
    Z: ShortInt;
    Hue: Word;
  end;
  
  { TRadarChecksumPacket }

  TRadarChecksumPacket = class(TPacket)
    constructor Create(ARadarMap: TRadarColorArray);
  end;
  
  { TRadarMapPacket }

  TRadarMapPacket = class(TPacket)
    constructor Create(ARadarMap: TRadarColorArray);
  end;
  
  { TUpdateRadarPacket }

  TUpdateRadarPacket = class(TPacket)
    constructor Create(AX, AY, AColor: Word);
  end;

{ TRadarChecksumPacket }

constructor TRadarChecksumPacket.Create(ARadarMap: TRadarColorArray);
var
  checksum: Cardinal;
begin
  inherited Create($0D, 0);
  FStream.WriteByte($01);
  checksum := crc32(0, nil, 0);
  checksum := crc32(checksum, @ARadarMap[0], Length(ARadarMap) * SizeOf(Word));
  FStream.WriteCardinal(checksum);
end;

{ TRadarMapPacket }

constructor TRadarMapPacket.Create(ARadarMap: TRadarColorArray);
begin
  inherited Create($0D, 0);
  FStream.WriteByte($02);
  FStream.Write(ARadarMap[0], Length(ARadarMap) * SizeOf(Word));
end;

{ TUpdateRadarPacket }

constructor TUpdateRadarPacket.Create(AX, AY, AColor: Word);
begin
  inherited Create($0D, 0);
  FStream.WriteByte($03);
  FStream.WriteWord(AX);
  FStream.WriteWord(AY);
  FStream.WriteWord(AColor);
end;

{ TRadarMap }

constructor TRadarMap.Create(AMap, AStatics, AStaIdx: TStream; AWidth,
  AHeight: Word; ARadarCol: string);
var
  radarcol: TFileStream;
  count, i, item, highestZ: Integer;
  staticsItems: array of TStaticItem;
  mapCell: TMapCell;
  index: TMulIndex;
begin
  radarcol := TFileStream.Create(ARadarCol, fmOpenRead);
  SetLength(FRadarColors, radarcol.Size div SizeOf(Word));
  radarcol.Read(FRadarColors[0], radarcol.Size);
  radarcol.Free;
  
  FWidth := AWidth;
  FHeight := AHeight;
  
  count := AWidth * AHeight;
  SetLength(FRadarMap, count);
  
  AMap.Position := 4;
  AStaIdx.Position := 0;

  for i := 0 to count - 1 do
  begin
    AMap.Read(mapCell, SizeOf(TMapCell));
    AMap.Seek(193, soFromCurrent);
    FRadarMap[i] := FRadarColors[mapCell.TileID];
    AStaIdx.Read(index, SizeOf(TMulIndex));
    if (index.Position < $FFFFFFFF) and (index.Size > 0) then
    begin
      AStatics.Position := index.Position;
      SetLength(staticsItems, index.Size div 7);
      AStatics.Read(staticsItems[0], index.Size);
      highestZ := mapCell.Altitude;
      for item := Low(staticsItems) to High(staticsItems) do
      begin
        if (staticsItems[item].X = 0) and (staticsItems[item].Y = 0) and
          (staticsItems[item].Z >= highestZ) then
        begin
          highestZ := staticsItems[item].Z;
          FRadarMap[i] := FRadarColors[staticsItems[item].TileID + $4000];
        end;
      end;
    end;
  end;
  
  FPackets := nil;
  
  RegisterPacketHandler($0D, TPacketHandler.Create(2, @OnRadarHandlingPacket));
  
  inherited Create;
end;

destructor TRadarMap.Destroy;
begin
  RegisterPacketHandler($0D, nil);
  inherited Destroy;
end;

procedure TRadarMap.OnRadarHandlingPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  subID: Byte;
begin
  if not ValidateAccess(ANetState, alView) then Exit;
  
  subID := ABuffer.ReadByte;
  case subID of
    $01: //request checksum
      begin
        CEDServerInstance.SendPacket(ANetState, TRadarChecksumPacket.Create(
          FRadarMap));
      end;
    $02: //request radarmap
      begin
        CEDServerInstance.SendPacket(ANetState, TCompressedPacket.Create(
          TRadarMapPacket.Create(FRadarMap)));
      end;
  end;
end;

procedure TRadarMap.Update(AX, AY, ATileID: Word);
var
  color: Word;
  block: Cardinal;
  packet: TPacket;
begin
  block := AX * FHeight + AY;
  color := FRadarColors[ATileID];
  if FRadarMap[block] <> color then
  begin
    FRadarMap[block] := color;
    packet := TUpdateRadarPacket.Create(AX, AY, color);
    if FPackets <> nil then
    begin
      FPackets.Add(packet);
      Inc(FPacketSize, packet.Stream.Size);
    end else
      CEDServerInstance.SendPacket(nil, packet);
  end;
end;

procedure TRadarMap.BeginUpdate;
begin
  if FPackets <> nil then Exit;
  FPackets := TList.Create;
  FPacketSize := 0;
end;

procedure TRadarMap.EndUpdate;
var
  completePacket: TPacket;
  i: Integer;
begin
  if FPackets = nil then Exit;
  completePacket := TCompressedPacket.Create(TRadarMapPacket.Create(FRadarMap));
  if completePacket.Stream.Size <= (FPacketSize div 4) * 5 then
  begin
    CEDServerInstance.SendPacket(nil, completePacket);
    for i := 0 to FPackets.Count - 1 do
      TPacket(FPackets.Items[i]).Free;
  end else
  begin
    for i := 0 to FPackets.Count - 1 do
      CEDServerInstance.SendPacket(nil, TPacket(FPackets.Items[i]));
    completePacket.Free;
  end;
  FreeAndNil(FPackets);
end;

end.

