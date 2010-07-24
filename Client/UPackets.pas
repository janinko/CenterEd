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
 *      Portions Copyright 2009 Andreas Schneider
 *)
unit UPackets;

interface

uses
  Classes, dzlib, UEnhancedMemoryStream, UPacket, UStatics;

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
  
  { TLoginRequestPacket }

  TLoginRequestPacket = class(TPacket)
    constructor Create(AUsername, APassword: string);
  end;
  
  { TQuitPacket }

  TQuitPacket = class(TPacket)
    constructor Create;
  end;
    
  { TRequestBlocksPacket }

  TRequestBlocksPacket = class(TPacket)
    constructor Create(ACoords: TBlockCoordsArray);
  end;
  
  { TFreeBlockPacket }

  TFreeBlockPacket = class(TPacket)
    constructor Create(AX, AY: Word);
  end;
  
  { TDrawMapPacket }

  TDrawMapPacket = class(TPacket)
    constructor Create(AX, AY: Word; AZ: ShortInt; ATileID: Word);
  end;

  { TStaticPacket }

  TStaticPacket = class(TPacket)
  protected
    procedure WriteStaticItem(AStaticItem: TStaticItem);
  end;
  
  { TInsertStaticPacket }

  TInsertStaticPacket = class(TPacket)
    constructor Create(AX, AY: Word; AZ: ShortInt; ATileID: Word; AHue: Word);
  end;
  
  { TDeleteStaticPacket }

  TDeleteStaticPacket = class(TStaticPacket)
    constructor Create(AStaticItem: TStaticItem);
  end;
  
  { TElevateStaticPacket }

  TElevateStaticPacket = class(TStaticPacket)
    constructor Create(AStaticItem: TStaticItem; ANewZ: ShortInt);
    constructor Create(AX, AY: Word; AZ: ShortInt; ATileID: Word; AHue: Word;
      ANewZ: Word);
  end;
  
  { TMoveStaticPacket }

  TMoveStaticPacket = class(TStaticPacket)
    constructor Create(AStaticItem: TStaticItem; ANewX, ANewY: Word);
    constructor Create(AX, AY: Word; AZ: ShortInt; ATileID: Word; AHue: Word;
      ANewX, ANewY: Word);
  end;
  
  { THueStaticPacket }

  THueStaticPacket = class(TStaticPacket)
    constructor Create(AStaticItem: TStaticItem; ANewHue: Word);
    constructor Create(AX, AY: Word; AZ: ShortInt; ATileID: Word; AHue: Word;
      ANewHue: Word);
  end;
  
  { TUpdateClientPosPacket }

  TUpdateClientPosPacket = class(TPacket)
    constructor Create(AX, AY: Word);
  end;
  
  { TChatMessagePacket }

  TChatMessagePacket = class(TPacket)
    constructor Create(AMessage: string);
  end;
  
  { TGotoClientPosPacket }

  TGotoClientPosPacket = class(TPacket)
    constructor Create(AUsername: string);
  end;
  
  { TRequestRadarChecksumPacket }

  TRequestRadarChecksumPacket = class(TPacket)
    constructor Create;
  end;
  
  { TRequestRadarMapPacket }

  TRequestRadarMapPacket = class(TPacket)
    constructor Create;
  end;
  
  { TNoOpPacket }

  TNoOpPacket = class(TPacket)
    constructor Create;
  end;

implementation

{ TCompressedPacket }

constructor TCompressedPacket.Create(APacket: TPacket);
var
  compBuffer: TEnhancedMemoryStream;
  compStream: TCompressionStream;
  sourceStream: TStream;
begin
  inherited Create($01, 0);
  compBuffer := TEnhancedMemoryStream.Create;
  compStream := TCompressionStream.Create(clMax, compBuffer);
  sourceStream := APacket.Stream;  
  compStream.CopyFrom(sourceStream, 0);
  compStream.Free;  
  FStream.WriteCardinal(sourceStream.Size);
  FStream.CopyFrom(compBuffer, 0);
  compBuffer.Free;
  APacket.Free;
end;

{ TLoginRequestPacket }

constructor TLoginRequestPacket.Create(AUsername, APassword: string);
begin
  inherited Create($02, 0);
  FStream.WriteByte($03);
  FStream.WriteStringNull(AUsername);
  FStream.WriteStringNull(APassword);
end;

{ TQuitPacket }

constructor TQuitPacket.Create;
begin
  inherited Create($02, 0);
  FStream.WriteByte($05);
end;

{ TRequestBlocksPacket }

constructor TRequestBlocksPacket.Create(ACoords: TBlockCoordsArray);
begin
  inherited Create($04, 0);
  FStream.Write(ACoords[0], Length(ACoords) * SizeOf(TBlockCoords));
end;

{ TFreeBlockPacket }

constructor TFreeBlockPacket.Create(AX, AY: Word);
begin
  inherited Create($05, 5);
  FStream.WriteWord(AX);
  FStream.WriteWord(AY);
end;

{ TDrawMapPacket }

constructor TDrawMapPacket.Create(AX, AY: Word; AZ: ShortInt; ATileID: Word);
begin
  inherited Create($06, 8);
  FStream.WriteWord(AX);
  FStream.WriteWord(AY);
  FStream.WriteShortInt(AZ);
  FStream.WriteWord(ATileID);
end;

{ TStaticPacket }

procedure TStaticPacket.WriteStaticItem(AStaticItem: TStaticItem);
begin
  FStream.WriteWord(AStaticItem.X);
  FStream.WriteWord(AStaticItem.Y);
  FStream.WriteShortInt(AStaticItem.Z);
  FStream.WriteWord(AStaticItem.TileID);
  FStream.WriteWord(AStaticItem.Hue);
end;

{ TInsertStaticPacket }

constructor TInsertStaticPacket.Create(AX, AY: Word; AZ: ShortInt;
  ATileID: Word; AHue: Word);
begin
  inherited Create($07, 10);
  FStream.WriteWord(AX);
  FStream.WriteWord(AY);
  FStream.WriteShortInt(AZ);
  FStream.WriteWord(ATileID);
  FStream.WriteWord(AHue);
end;

{ TDeleteStaticPacket }

constructor TDeleteStaticPacket.Create(AStaticItem: TStaticItem);
begin
  inherited Create($08, 10);
  WriteStaticItem(AStaticItem);
end;

{ TElevateStaticPacket }

constructor TElevateStaticPacket.Create(AStaticItem: TStaticItem; ANewZ: ShortInt);
begin
  inherited Create($09, 11);
  WriteStaticItem(AStaticItem);
  FStream.WriteShortInt(ANewZ);
end;

constructor TElevateStaticPacket.Create(AX, AY: Word; AZ: ShortInt;
  ATileID: Word; AHue: Word; ANewZ: Word);
begin
  inherited Create($09, 11);
  FStream.WriteWord(AX);
  FStream.WriteWord(AY);
  FStream.WriteShortInt(AZ);
  FStream.WriteWord(ATileID);
  FStream.WriteWord(AHue);
  FStream.WriteShortInt(ANewZ);
end;

{ TMoveStaticPacket }

constructor TMoveStaticPacket.Create(AStaticItem: TStaticItem; ANewX,
  ANewY: Word);
begin
  inherited Create($0A, 14);
  WriteStaticItem(AStaticItem);
  FStream.WriteWord(ANewX);
  FStream.WriteWord(ANewY);
end;

constructor TMoveStaticPacket.Create(AX, AY: Word; AZ: ShortInt; ATileID: Word;
  AHue: Word; ANewX, ANewY: Word);
begin
  inherited Create($0A, 14);
  FStream.WriteWord(AX);
  FStream.WriteWord(AY);
  FStream.WriteShortInt(AZ);
  FStream.WriteWord(ATileID);
  FStream.WriteWord(AHue);
  FStream.WriteWord(ANewX);
  FStream.WriteWord(ANewY);
end;

{ THueStaticPacket }

constructor THueStaticPacket.Create(AStaticItem: TStaticItem; ANewHue: Word);
begin
  inherited Create($0B, 12);
  WriteStaticItem(AStaticItem);
  FStream.WriteWord(ANewHue);
end;

constructor THueStaticPacket.Create(AX, AY: Word; AZ: ShortInt; ATileID: Word;
  AHue: Word; ANewHue: Word);
begin
  inherited Create($0B, 12);
  FStream.WriteWord(AX);
  FStream.WriteWord(AY);
  FStream.WriteShortInt(AZ);
  FStream.WriteWord(ATileID);
  FStream.WriteWord(AHue);
  FStream.WriteWord(ANewHue);
end;

{ TUpdateClientPosPacket }

constructor TUpdateClientPosPacket.Create(AX, AY: Word);
begin
  inherited Create($0C, 0);
  FStream.WriteByte($04);
  FStream.WriteWord(AX);
  FStream.WriteWord(AY);
end;

{ TChatMessagePacket }

constructor TChatMessagePacket.Create(AMessage: string);
begin
  inherited Create($0C, 0);
  FStream.WriteByte($05);
  FStream.WriteStringNull(AMessage);
end;

{ TGotoClientPosPacket }

constructor TGotoClientPosPacket.Create(AUsername: string);
begin
  inherited Create($0C, 0);
  FStream.WriteByte($06);
  FStream.WriteStringNull(AUsername);
end;

{ TRequestRadarChecksumPacket }

constructor TRequestRadarChecksumPacket.Create;
begin
  inherited Create($0D, 2);
  FStream.WriteByte($01);
end;

{ TRequestRadarMapPacket }

constructor TRequestRadarMapPacket.Create;
begin
  inherited Create($0D, 2);
  FStream.WriteByte($02);
end;

{ TNoOpPacket }

constructor TNoOpPacket.Create;
begin
  inherited Create($FF, 1);
end;

end.

