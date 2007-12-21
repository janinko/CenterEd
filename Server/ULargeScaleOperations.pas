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
unit ULargeScaleOperations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMap, UStatics, UEnhancedMemoryStream, math,
  ULandscape;
  
type

  TCopyMoveType = (cmCopy = 0, cmMove = 1);
  TSetAltitudeType = (saTerrain = 1, saRelative = 2);
  TStaticsPlacement = (spTerrain = 1, spTop = 2, spFix = 3);

  { TLargeScaleOperation }

  TLargeScaleOperation = class(TObject)
    constructor Init(AData: TEnhancedMemoryStream; ALandscape: TLandscape); virtual;
  protected
    FLandscape: TLandscape;
  public
    procedure Apply(AMapCell: TMapCell; AStatics: TList;
      AAdditionalAffectedBlocks: TBits); virtual; abstract;
  end;
  
  { TLSCopyMove }

  TLSCopyMove = class(TLargeScaleOperation)
    constructor Init(AData: TEnhancedMemoryStream; ALandscape: TLandscape); override;
  protected
    FType: TCopyMoveType;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FErase: Boolean;
  public
    property OffsetX: Integer read FOffsetX;
    property OffsetY: Integer read FOffsetY;
    procedure Apply(AMapCell: TMapCell; AStatics: TList;
      AAdditionalAffectedBlocks: TBits); override;
  end;
  
  { TLSSetAltitude }

  TLSSetAltitude = class(TLargeScaleOperation)
    constructor Init(AData: TEnhancedMemoryStream; ALandscape: TLandscape); override;
  protected
    FType: TSetAltitudeType;
    FMinZ: ShortInt;
    FMaxZ: ShortInt;
    FRelativeZ: ShortInt;
  public
    procedure Apply(AMapCell: TMapCell; AStatics: TList;
      AAdditionalAffectedBlocks: TBits); override;
  end;
  
  { TLSDrawTerrain }

  TLSDrawTerrain = class(TLargeScaleOperation)
    constructor Init(AData: TEnhancedMemoryStream; ALandscape: TLandscape); override;
  protected
    FTileIDs: array of Word;
  public
    procedure Apply(AMapCell: TMapCell; AStatics: TList;
      AAdditionalAffectedBlocks: TBits); override;
  end;
  
  { TLSDeleteStatics }

  TLSDeleteStatics = class(TLargeScaleOperation)
    constructor Init(AData: TEnhancedMemoryStream; ALandscape: TLandscape); override;
  protected
    FTileIDs: array of Word;
    FMinZ: ShortInt;
    FMaxZ: ShortInt;
  public
    procedure Apply(AMapCell: TMapCell; AStatics: TList;
      AAdditionalAffectedBlocks: TBits); override;
  end;
  
  { TLSInsertStatics }

  TLSInsertStatics = class(TLargeScaleOperation)
    constructor Init(AData: TEnhancedMemoryStream; ALandscape: TLandscape); override;
  protected
    FTileIDs: array of Word;
    FProbability: Byte;
    FPlacementType: TStaticsPlacement;
    FFixZ: ShortInt;
  public
    procedure Apply(AMapCell: TMapCell; AStatics: TList;
      AAdditionalAffectedBlocks: TBits); override;
  end;
  

implementation

uses
  UCEDServer, UTiledata;
  
{ TLargeScaleOperation }

constructor TLargeScaleOperation.Init(AData: TEnhancedMemoryStream;
  ALandscape: TLandscape);
begin
  inherited Create;
  FLandscape := ALandscape;
end;
  
{ TLSCopyMove }

constructor TLSCopyMove.Init(AData: TEnhancedMemoryStream;
  ALandscape: TLandscape);
begin
  inherited Init(AData, ALandscape);
  FType := TCopyMoveType(AData.ReadByte);
  FOffsetX := AData.ReadInteger;
  FOffsetY := AData.ReadInteger;
  FErase := AData.ReadBoolean;
end;

procedure TLSCopyMove.Apply(AMapCell: TMapCell; AStatics: TList;
  AAdditionalAffectedBlocks: TBits);
var
  x, y: Word;
  targetCell: TMapCell;
  targetStatics: TList;
  targetStaticsBlock: TSeperatedStaticBlock;
  i: Integer;
  staticItem: TStaticItem;
begin
  x := EnsureRange(AMapCell.X + FOffsetX, 0, FLandscape.CellWidth - 1);
  y := EnsureRange(AMapCell.Y + FOffsetY, 0, FLandscape.CellHeight - 1);
  //writeln('target: ', x, ',', y);
  targetCell := FLandscape.MapCell[x, y];
  targetStaticsBlock := FLandscape.GetStaticBlock(x div 8, y div 8);
  targetStatics := targetStaticsBlock.Cells[(y mod 8) * 8 + (x mod 8)];
  if FErase then
  begin
    for i := 0 to targetStatics.Count - 1 do
    begin
      TStaticItem(targetStatics.Items[i]).Delete;
    end;
    targetStatics.Clear;
  end;
  targetCell.TileID := AMapCell.TileID;
  targetCell.Z := AMapCell.Z;
  
  if FType = cmCopy then
  begin
    for i := 0 to AStatics.Count - 1 do
    begin
      staticItem := TStaticItem.Create(nil, nil, 0, 0);
      staticItem.X := x;
      staticItem.Y := y;
      staticItem.Z := TStaticItem(AStatics.Items[i]).Z;
      staticItem.TileID := TStaticItem(AStatics.Items[i]).TileID;
      staticItem.Hue := TStaticItem(AStatics.Items[i]).Hue;
      staticItem.Owner := targetStaticsBlock;
      targetStatics.Add(staticItem);
    end;
  end else
  begin
    {for i := 0 to AStatics.Count - 1 do}
    while AStatics.Count > 0 do
    begin
      targetStatics.Add(AStatics.Items[0]);
      TStaticItem(AStatics.Items[0]).UpdatePos(x, y, TStaticItem(AStatics.Items[0]).Z);
      TStaticItem(AStatics.Items[0]).Owner := targetStaticsBlock;
      AStatics.Delete(0);
    end;
    //AStatics.Clear;
  end;
  
  FLandscape.SortStaticsList(targetStatics);
  AAdditionalAffectedBlocks.Bits[(x div 8) * FLandscape.Height + (y div 8)] := True;
end;

{ TLSSetAltitude }

constructor TLSSetAltitude.Init(AData: TEnhancedMemoryStream;
  ALandscape: TLandscape);
begin
  inherited Init(AData, ALandscape);
  FType := TSetAltitudeType(AData.ReadByte);
  case FType of
    saTerrain:
      begin
        FMinZ := AData.ReadShortInt;
        FMaxZ := AData.ReadShortInt;
      end;
    saRelative:
      begin
        FRelativeZ := AData.ReadShortInt;
      end;
  end;
end;

procedure TLSSetAltitude.Apply(AMapCell: TMapCell; AStatics: TList;
  AAdditionalAffectedBlocks: TBits);
var
  i: Integer;
  newZ: ShortInt;
  diff: ShortInt;
  static: TStaticItem;
begin
  if FType = saTerrain then
  begin
    newZ := FMinZ + Random(FMaxZ - FMinZ + 1);
    diff := newZ - AMapCell.Z;
    AMapCell.Z := newZ;
  end else
  begin
    diff := FRelativeZ;
    AMapCell.Z := EnsureRange(AMapCell.Z + diff, -128, 127);
  end;
  
  for i := 0 to AStatics.Count - 1 do
  begin
    static := TStaticItem(AStatics.Items[i]);
    static.Z := EnsureRange(static.Z + diff, -128, 127);
  end;
end;

{ TLSDrawTerrain }

constructor TLSDrawTerrain.Init(AData: TEnhancedMemoryStream;
  ALandscape: TLandscape);
var
  count: Word;
begin
  inherited Init(AData, ALandscape);
  count := AData.ReadWord;
  SetLength(FTileIDs, count);
  AData.Read(FTileIDs[0], count * SizeOf(Word));
end;

procedure TLSDrawTerrain.Apply(AMapCell: TMapCell; AStatics: TList;
  AAdditionalAffectedBlocks: TBits);
begin
  if Length(FTileIDs) > 0 then
    AMapCell.TileID := FTileIDs[Random(Length(FTileIDs))];
end;

{ TLSDeleteStatics }

constructor TLSDeleteStatics.Init(AData: TEnhancedMemoryStream;
  ALandscape: TLandscape);
var
  count: Word;
begin
  inherited Init(AData, ALandscape);
  count := AData.ReadWord;
  SetLength(FTileIDs, count);
  AData.Read(FTileIDs[0], count * SizeOf(Word));
  FMinZ := AData.ReadShortInt;
  FMaxZ := AData.ReadShortInt;
end;

procedure TLSDeleteStatics.Apply(AMapCell: TMapCell; AStatics: TList;
  AAdditionalAffectedBlocks: TBits);
var
  i, j: Integer;
  static: TStaticItem;
begin
  i := 0;
  while i < AStatics.Count do
  begin
    static := TStaticItem(AStatics.Items[i]);
    if InRange(static.Z, FMinZ, FMaxZ) then
    begin
      if Length(FTileIDs) > 0 then
      begin
        for j := Low(FTileIDs) to High(FTileIDs) do
        begin
          if static.TileID = FTileIDs[j] - $4000 then
          begin
            AStatics.Delete(i);
            static.Delete;
            Dec(i);
            Break;
          end;
        end;
        Inc(i);
      end else
      begin
        AStatics.Delete(i);
        static.Delete;
      end;
    end else
      Inc(i);
  end;
end;

{ TLSInsertStatics }

constructor TLSInsertStatics.Init(AData: TEnhancedMemoryStream;
  ALandscape: TLandscape);
var
  count: Word;
begin
  inherited Init(AData, ALandscape);
  count := AData.ReadWord;
  SetLength(FTileIDs, count);
  AData.Read(FTileIDs[0], count * SizeOf(Word));
  FProbability := AData.ReadByte;
  FPlacementType := TStaticsPlacement(AData.ReadByte);
  if FPlacementType = spFix then
    FFixZ := AData.ReadShortInt;
end;

procedure TLSInsertStatics.Apply(AMapCell: TMapCell; AStatics: TList;
  AAdditionalAffectedBlocks: TBits);
var
  staticItem, static: TStaticItem;
  topZ, staticTop: ShortInt;
  i: Integer;
begin
  if (Length(FTileIDs) = 0) or (Random(100) >= FProbability) then Exit;
  
  staticItem := TStaticItem.Create(nil, nil, 0, 0);
  staticItem.X := AMapCell.X;
  staticItem.Y := AMapCell.Y;
  staticItem.TileID := FTileIDs[Random(Length(FTileIDs))] - $4000;
  staticItem.Hue := 0;
  
  case FPlacementType of
    spTerrain:
      begin
        staticItem.Z := AMapCell.Z;
      end;
    spTop:
      begin
        topZ := AMapCell.Z;
        for i := 0 to AStatics.Count - 1 do
        begin
          static := TStaticItem(AStatics.Items[i]);
          staticTop := EnsureRange(static.Z + CEDServerInstance.Landscape.TiledataProvider.StaticTiles[static.TileID].Height, -128, 127);
          if staticTop > topZ then topZ := staticTop;
        end;
      end;
    spFix:
      begin
        staticItem.Z := FFixZ;
      end;
  end;
  
  AStatics.Add(staticItem);
  staticItem.Owner := CEDServerInstance.Landscape.GetStaticBlock(staticItem.X div 8,
    staticItem.Y div 8);
end;

end.

