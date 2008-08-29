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
unit ULandscape;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, math, contnrs, LCLIntf, GL, GLU, ImagingOpenGL,
  Imaging, ImagingClasses, ImagingTypes, ImagingUtility,
  UGenericIndex, UMap, UStatics, UArt, UTexture, UTiledata, UHue, UWorldItem,
  UMulBlock,
  UListSort, UVector, UEnhancedMemoryStream,
  UCacheManager, ULinkedList;

type
  TNormals = array[0..3] of TVector;
  PRadarBlock = ^TRadarBlock;
  TRadarBlock = array[0..7, 0..7] of Word;
  
  { TMaterial }
  
  TMaterial = class(TObject)
    constructor Create(AWidth, AHeight: Integer; AGraphic: TSingleImage);
    destructor Destroy; override;
  protected
    FWidth: Integer;
    FHeight: Integer;
    FRealWidth: Integer;
    FRealHeight: Integer;
    FTexture: GLuint;
    FGraphic: TSingleImage;
  public
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property RealWidth: Integer read FRealWidth;
    property RealHeight: Integer read FRealHeight;
    property Texture: GLuint read FTexture;
    property Graphic: TSingleImage read FGraphic;
    
    function HitTest(AX, AY: Integer): Boolean;
    procedure UpdateTexture;
  end;
  
  { TLandTextureManager }
  
  TLandTextureManager = class(TObject)
    constructor Create;
    destructor Destroy; override;
    function GetArtMaterial(ATileID: Word): TMaterial; overload;
    function GetArtMaterial(ATileID: Word; AHue: THue; APartialHue: Boolean): TMaterial; overload;
    function GetFlatLandMaterial(ATileID: Word): TMaterial;
    function GetTexMaterial(ATileID: Word): TMaterial;
  protected
    FArtCache: TCacheManager;
    FFlatLandArtCache: TCacheManager;
    FTexCache: TCacheManager;
  end;
  
  { TBlock }

  TBlock = class(TObject)
    constructor Create(AMap: TMapBlock; AStatics: TStaticBlock);
    destructor Destroy; override;
  protected
    FMapBlock: TMapBlock;
    FStaticBlock: TStaticBlock;
  public
    property Map: TMapBlock read FMapBlock;
    property Static: TStaticBlock read FStaticBlock;
  end;
  
  TLandscapeChangeEvent = procedure of object;
  TStaticFilter = function(AStatic: TStaticItem): Boolean of object;

  { TLandscape }

  TLandscape = class(TObject)
    constructor Create(AWidth, AHeight: Word);
    destructor Destroy; override;
  protected
    FWidth: Word;
    FHeight: Word;
    FCellWidth: Word;
    FCellHeight: Word;
    FBlockCache: TCacheManager;
    FOnChange: TLandscapeChangeEvent;
    FOpenRequests: array of Boolean;
    function Compare(left, right: TObject): Integer;
    function GetNormals(AX, AY: Word): TNormals;
    function GetMapCell(AX, AY: Word): TMapCell;
    function GetStaticList(AX, AY: Word): TList;
    function GetMapBlock(AX, AY: Word): TMapBlock;
    function GetStaticBlock(AX, AY: Word): TSeperatedStaticBlock;
    procedure UpdateStaticsPriority(AStaticItem: TStaticItem;
      APrioritySolver: Integer);
    procedure OnBlockChanged(ABlock: TMulBlock);
    procedure OnRemoveCachedObject(AObject: TObject);
    
    procedure OnBlocksPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnDrawMapPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnInsertStaticPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnDeleteStaticPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnElevateStaticPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnMoveStaticPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnHueStaticPacket(ABuffer: TEnhancedMemoryStream);
  public
    property Width: Word read FWidth;
    property Height: Word read FHeight;
    property CellWidth: Word read FCellWidth;
    property CellHeight: Word read FCellHeight;
    property MapCell[X, Y: Word]: TMapCell read GetMapCell;
    property StaticList[X, Y: Word]: TList read GetStaticList;
    property Normals[X, Y: Word]: TNormals read GetNormals;
    property OnChange: TLandscapeChangeEvent read FOnChange write FOnChange;

    function GetDrawList(AX, AY: Word; AMinZ, AMaxZ: ShortInt;
      AGhostTile: TWorldItem; AVirtualLayer: TStaticItem; AMap,
      AStatics: Boolean; ANoDraw:Boolean; AStaticsFilter: TStaticFilter): TList;
    function GetEffectiveAltitude(ATile: TMapCell): ShortInt;
    function GetLandAlt(AX, AY: Word; ADefault: ShortInt): ShortInt;

    procedure MoveStatic(AStatic: TStaticItem; AX, AY: Word);
    procedure PrepareBlocks(AX1, AY1, AX2, AY2: Word);
  end;
  PBlockInfo = ^TBlockInfo;
  TBlockInfo = record
    ScreenRect: TRect;
    Item: TWorldItem;
    Material: TMaterial;
    Next: PBlockInfo;
  end;

  { TTileList }

  TTileList = class(TObject)
    constructor Create; virtual;
    destructor Destroy; override;
  protected
    FFirst: PBlockInfo;
    FLastBlock: PBlockInfo;
  public
    procedure Clear; virtual;
    function Iterate(var ABlockInfo: PBlockInfo): Boolean; virtual;
    procedure Add(AItem: TWorldItem); virtual;
    procedure Delete(AItem: TWorldItem); virtual;
    property LastBlock: PBlockInfo read FLastBlock;
  end;

  { TScreenBuffer }

  TScreenBuffer = class(TTileList)
  public
    procedure OnTileRemoved(ATile: TMulBlock);
    procedure Clear; override;
    function Find(AScreenPosition: TPoint): PBlockInfo;
    procedure Store(AScreenRect: TRect; AItem: TWorldItem; AMaterial: TMaterial);
  end;
  
  TStaticInfo = packed record
    X: Word;
    Y: Word;
    Z: ShortInt;
    TileID: Word;
    Hue: Word;
  end;
  //operator=(AStaticItem: TStaticItem; AStaticInfo: TStaticInfo): Boolean;

implementation

uses
  UGameResources, UdmNetwork, UPackets, UPacketHandlers;

const
  mMap = 0;
  mStatics = 1;

function GetID(AX, AY: Word): Integer;
begin
  Result := ((AX and $7FFF) shl 15) or (AY and $7FFF);
end;

{operator=(AStaticItem: TStaticItem; AStaticInfo: TStaticInfo): Boolean;
begin
  Result := (AStaticItem.X = AStaticInfo.X) and
    (AStaticItem.Y = AStaticInfo.Y) and
    (AStaticItem.Z = AStaticInfo.Z) and
    (AStaticItem.TileID = AStaticInfo.TileID) and
    (AStaticItem.Hue = AStaticInfo.Hue);
end;}

{ TLandTextureManager }

constructor TLandTextureManager.Create;
begin
  inherited Create;
  FArtCache := TCacheManager.Create(1024);
  FFlatLandArtCache := TCacheManager.Create(128);
  FTexCache := TCacheManager.Create(128);
end;

destructor TLandTextureManager.Destroy;
begin
  if FArtCache <> nil then FreeAndNil(FArtCache);
  if FFlatLandArtCache <> nil then FreeAndNil(FFlatLandArtCache);
  if FTexCache <> nil then FreeAndNil(FTexCache);
  inherited Destroy;
end;

function TLandTextureManager.GetArtMaterial(ATileID: Word): TMaterial;
var
  artEntry: TArt;
begin
  if not FArtCache.QueryID(ATileID, TObject(Result)) then
  begin
    artEntry := TArt(ResMan.Art.Block[ATileID]);

    Result := TMaterial.Create(artEntry.Graphic.Width, artEntry.Graphic.Height,
      artEntry.Graphic);
    FArtCache.StoreID(ATileID, Result);

    artEntry.Free;
  end;
end;

function TLandTextureManager.GetArtMaterial(ATileID: Word; AHue: THue; APartialHue: Boolean): TMaterial;
var
  artEntry: TArt;
  id: Integer;
begin
  if AHue = nil then
  begin
    Result := GetArtMaterial(ATileID);
  end else
  begin
    id := ATileID or ((AHue.ID and $3FFF) shl 15) or (Byte(APartialHue) shl 29);
    if not FArtCache.QueryID(id, TObject(Result)) then
    begin
      artEntry := ResMan.Art.GetArt(ATileID, 0, AHue, APartialHue);

      Result := TMaterial.Create(artEntry.Graphic.Width, artEntry.Graphic.Height,
        artEntry.Graphic);
      FArtCache.StoreID(id, Result);

      artEntry.Free;
    end;
  end;
end;

function TLandTextureManager.GetFlatLandMaterial(ATileID: Word): TMaterial;
var
  artEntry: TArt;
begin
  if not FFlatLandArtCache.QueryID(ATileID, TObject(Result)) then
  begin
    artEntry := ResMan.Art.GetFlatLand(ATileID);

    Result := TMaterial.Create(artEntry.Graphic.Width, artEntry.Graphic.Height,
      artEntry.Graphic);
    FFlatLandArtCache.StoreID(ATileID, Result);

    artEntry.Free;
  end;
end;

function TLandTextureManager.GetTexMaterial(ATileID: Word): TMaterial;
var
  texEntry: TTexture;
  texID: Integer;
begin
  if not FTexCache.QueryID(ATileID, TObject(Result)) then
  begin
    texID := ResMan.Tiledata.LandTiles[ATileID].TextureID;
    if texID > 0 then
    begin
      texEntry := TTexture(ResMan.Texmaps.Block[texID]);

      Result := TMaterial.Create(texEntry.Graphic.Width, texEntry.Graphic.Height,
        texEntry.Graphic);
      FTexCache.StoreID(ATileID, Result);

      texEntry.Free;
    end else
      Result := nil;
  end;
end;

{ TBlock }

constructor TBlock.Create(AMap: TMapBlock; AStatics: TStaticBlock);
begin
  inherited Create;
  FMapBlock := AMap;
  FStaticBlock := AStatics;
end;

destructor TBlock.Destroy;
begin
  if FMapBlock <> nil then FreeAndNil(FMapBlock);
  if FStaticBlock <> nil then FreeAndNil(FStaticBlock);
  inherited Destroy;
end;

{ TLandscape }

constructor TLandscape.Create(AWidth, AHeight: Word);
var
  blockID: Integer;
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FCellWidth := FWidth * 8;
  FCellHeight := FHeight * 8;
  FBlockCache := TCacheManager.Create(256);
  FBlockCache.OnRemoveObject := @OnRemoveCachedObject;
  
  SetLength(FOpenRequests, FWidth * FHeight);
  for blockID := 0 to Length(FOpenRequests) - 1 do
    FOpenRequests[blockID] := False;
  
  RegisterPacketHandler($04, TPacketHandler.Create(0, @OnBlocksPacket));
  RegisterPacketHandler($06, TPacketHandler.Create(8, @OnDrawMapPacket));
  RegisterPacketHandler($07, TPacketHandler.Create(10, @OnInsertStaticPacket));
  RegisterPacketHandler($08, TPacketHandler.Create(10, @OnDeleteStaticPacket));
  RegisterPacketHandler($09, TPacketHandler.Create(11, @OnElevateStaticPacket));
  RegisterPacketHandler($0A, TPacketHandler.Create(14, @OnMoveStaticPacket));
  RegisterPacketHandler($0B, TPacketHandler.Create(12, @OnHueStaticPacket));
end;

destructor TLandscape.Destroy;
var
  i: Integer;
begin
  if FBlockCache <> nil then
  begin
    FBlockCache.OnRemoveObject := nil;
    FreeAndNil(FBlockCache);
  end;
  
  RegisterPacketHandler($04, nil);
  RegisterPacketHandler($06, nil);
  RegisterPacketHandler($07, nil);
  RegisterPacketHandler($08, nil);
  RegisterPacketHandler($09, nil);
  RegisterPacketHandler($0A, nil);
  RegisterPacketHandler($0B, nil);
  
  inherited Destroy;
end;

function TLandscape.GetMapCell(AX, AY: Word): TMapCell;
var
  block: TMapBlock;
begin
  Result := nil;
  if (AX >= 0) and (AX <= FCellWidth) and (AY >= 0) and (AY <= FCellHeight) then
  begin
    block := GetMapBlock(AX div 8, AY div 8);
    if block <> nil then
      Result := block.Cells[(AY mod 8) * 8 + AX mod 8];
  end;
end;

function TLandscape.GetLandAlt(AX, AY: Word; ADefault: ShortInt): ShortInt;
var
  cell: TMapCell;
begin
  cell := MapCell[AX, AY];
  if cell <> nil then
    Result := cell.Altitude
  else
    Result := ADefault;
end;

function TLandscape.GetStaticList(AX, AY: Word): TList;
var
  block: TSeperatedStaticBlock;
begin
  Result := nil;
  if (AX >= 0) and (AX <= FCellWidth) and (AY >= 0) and (AY <= FCellHeight) then
  begin
    block := GetStaticBlock(AX div 8, AY div 8);
    if block <> nil then
      Result := block.Cells[(AY mod 8) * 8 + AX mod 8];
  end;
end;

function TLandscape.Compare(left, right: TObject): Integer;
begin
  Result := TWorldItem(right).Priority - TWorldItem(left).Priority;
  if Result = 0 then
  begin
    if (left is TMapCell) and (right is TStaticItem) then
      Result := 1
    else if (left is TStaticItem) and (right is TMapCell) then
      Result := -1;
  end;

  if Result = 0 then
    Result := TWorldItem(right).PriorityBonus - TWorldItem(left).PriorityBonus;

  if Result = 0 then
    Result := TWorldItem(right).PrioritySolver - TWorldItem(left).PrioritySolver;
end;

function TLandscape.GetDrawList(AX, AY: Word; AMinZ, AMaxZ: ShortInt;
  AGhostTile: TWorldItem; AVirtualLayer: TStaticItem; AMap,
  AStatics: Boolean; ANoDraw:Boolean; AStaticsFilter: TStaticFilter): TList;
var
  landAlt: ShortInt;
  drawMapCell: TMapCell;
  drawStatics: TList;
  i: Integer;
begin
  Result := TList.Create;
  if AMap then
  begin
    landAlt := GetLandAlt(AX, AY, 0);
    if (landAlt >= AMinZ) and (landAlt <= AMaxZ) then
    begin
      drawMapCell := GetMapCell(AX, AY);
      if (drawMapCell <> nil) and (ANoDraw or (drawMapCell.TileID > 2)) then
      begin
        drawMapCell.Priority := GetEffectiveAltitude(drawMapCell);
        drawMapCell.PriorityBonus := 0;
        drawMapCell.PrioritySolver := 0;
        Result.Add(drawMapCell);
      end;
      
      if AGhostTile is TMapCell then
      begin
        AGhostTile.X := AX;
        AGhostTile.Y := AY;
        AGhostTile.Priority := GetEffectiveAltitude(TMapCell(AGhostTile));
        AGhostTile.PriorityBonus := 0;
        AGhostTile.PrioritySolver := 0;
        Result.Add(AGhostTile);
      end;
    end;
  end;

  if AStatics then
  begin
    drawStatics := GetStaticList(AX, AY);
    if drawStatics <> nil then
      for i := 0 to drawStatics.Count - 1 do
        if (TStaticItem(drawStatics[i]).Z >= AMinZ) and
           (TStaticItem(drawStatics[i]).Z <= AMaxZ) and
           ((AStaticsFilter = nil) or AStaticsFilter(TStaticItem(drawStatics[i]))) then
        begin
          UpdateStaticsPriority(TStaticItem(drawStatics[i]), i + 1);
          Result.Add(Pointer(drawStatics[i]));
        end;
        

    if AGhostTile is TStaticItem then
    begin
      UpdateStaticsPriority(TStaticItem(AGhostTile), MaxInt);
      Result.Add(AGhostTile);
    end;
  end;
  
  if AVirtualLayer <> nil then
  begin
    UpdateStaticsPriority(AVirtualLayer, MaxInt-1);
    Result.Add(AVirtualLayer);
  end;
  
  ListSort(Result, @Compare);
end;

function TLandscape.GetEffectiveAltitude(ATile: TMapCell): ShortInt;
var
  north, west, south, east: ShortInt;
begin
  north := ATile.Altitude;
  west := GetLandAlt(ATile.X, ATile.Y + 1, north);
  south := GetLandAlt(ATile.X + 1, ATile.Y + 1, north);
  east := GetLandAlt(ATile.X + 1, ATile.Y, north);

  if Abs(north - south) > Abs(west - east) then
    Result := (north + south) div 2
  else
    Result := (west + east) div 2;
end;

function TLandscape.GetNormals(AX, AY: Word): TNormals;
var
  cells: array[0..2, 0..2] of TNormals;
  north, west, south, east: TVector;
  i, j: Integer;

  function GetPlainNormals(X, Y: SmallInt): TNormals;
  var
    cell: TMapCell;
    north, west, south, east: ShortInt;
    u, v: TVector;
  begin
    cell := GetMapCell(X, Y);
    if Assigned(cell) then
    begin
      north := cell.Altitude;
      west := GetLandAlt(cell.X, cell.Y + 1, north);
      south := GetLandAlt(cell.X + 1, cell.Y + 1, north);
      east := GetLandAlt(cell.X + 1, cell.Y, north);
    end else
    begin
      north := 0;
      west := 0;
      east := 0;
      south := 0;
    end;

    if (north = west) and (west = east) and (north = south) then
    begin
      Result[0] := Vector(0, 0, 1);
      Result[1] := Vector(0, 0, 1);
      Result[2] := Vector(0, 0, 1);
      Result[3] := Vector(0, 0, 1);
    end else
    begin
      u := Vector(-22, 22, (north - east) * 4);
      v := Vector(-22, -22, (west - north) * 4);
      Result[0] := VectorNorm(VectorCross(u, v));

      u := Vector(22, 22, (east - south) * 4);
      v := Vector(-22, 22, (north - east) * 4);
      Result[1] := VectorNorm(VectorCross(u, v));

      u := Vector(22, -22, (south - west) * 4);
      v := Vector(22, 22, (east - south) * 4);
      Result[2] := VectorNorm(VectorCross(u, v));

      u := Vector(-22, -22, (west - north) * 4);
      v := Vector(22, -22, (south - west) * 4);
      Result[3] := VectorNorm(VectorCross(u, v));
    end;
  end;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      cells[i, j] := GetPlainNormals(AX - 1 + i, AY - 1 + j);

  north := cells[0, 0][2];
  west := cells[0, 1][1];
  east := cells[1, 0][3];
  south := cells[1, 1][0];
  Result[0] := VectorNorm(VectorAdd(VectorAdd(VectorAdd(north, west), east), south));

  north := cells[1, 0][2];
  west := cells[1, 1][1];
  east := cells[2, 0][3];
  south := cells[2, 1][0];
  Result[1] := VectorNorm(VectorAdd(VectorAdd(VectorAdd(north, west), east), south));

  north := cells[1, 1][2];
  west := cells[1, 2][1];
  east := cells[2, 1][3];
  south := cells[2, 2][0];
  Result[2] := VectorNorm(VectorAdd(VectorAdd(VectorAdd(north, west), east), south));

  north := cells[0, 1][2];
  west := cells[0, 2][1];
  east := cells[1, 1][3];
  south := cells[1, 2][0];
  Result[3] := VectorNorm(VectorAdd(VectorAdd(VectorAdd(north, west), east), south));
end;

procedure TLandscape.OnBlockChanged(ABlock: TMulBlock);
var
  block, old: TWorldBlock;
  mode: Byte;
  id, blockID: Integer;
begin
  {block := ABlock as TWorldBlock;
  if block <> nil then
  begin
    if block is TSeperatedStaticBlock then
      mode := mStatics
    else
      mode := mMap;
    id := GetID(block.X, block.Y, mode);
    blockID := (block.X * FHeight) + block.Y;
    if block.Changed or (block.RefCount > 0) then
    begin
      if FPersistentBlocks[blockID][mode] = nil then
      begin
        FPersistentBlocks[blockID][mode] := block;
        FBlockCache.DiscardID(id);
      end;
    end else
    begin
      FPersistentBlocks[blockID][mode] := nil;
      if not FBlockCache.QueryID(id, TObject(old)) then
        FBlockCache.StoreID(id, block);
    end;
  end;}
end;

procedure TLandscape.MoveStatic(AStatic: TStaticItem; AX, AY: Word);
var
  sourceBlock, targetBlock: TSeperatedStaticBlock;
  targetStaticList: TList;
  i: Integer;
begin
  if (AX >= 0) and (AX <= FCellWidth) and (AY >= 0) and (AY <= FCellHeight) then
  begin
    sourceBlock := AStatic.Owner as TSeperatedStaticBlock;
    targetBlock := GetStaticBlock(AX div 8, AY div 8);
    if (sourceBlock <> nil) and (targetBlock <> nil) then
    begin
      sourceBlock.Cells[(AStatic.Y mod 8) * 8 + AStatic.X mod 8].Remove(AStatic);
      targetStaticList := targetBlock.Cells[(AY mod 8) * 8 + AX mod 8];
      targetStaticList.Add(AStatic);
      for i := 0 to targetStaticList.Count - 1 do
        UpdateStaticsPriority(TStaticItem(targetStaticList.Items[i]), i);
      ListSort(targetStaticList, @Compare);
      AStatic.UpdatePos(AX, AY, AStatic.Z);
      AStatic.Owner := targetBlock;
    end;
  end;
end;

procedure TLandscape.PrepareBlocks(AX1, AY1, AX2, AY2: Word);
var
  x, y, i, mapID, staticID: Integer;
  coords: TBlockCoordsArray;
  obj: TObject;
begin
  AX1 := EnsureRange(AX1, 0, FWidth - 1);
  AY1 := EnsureRange(AY1, 0, FHeight - 1);
  AX2 := EnsureRange(AX2, 0, FWidth - 1);
  AY2 := EnsureRange(AY2, 0, FHeight - 1);

  SetLength(coords, 0);
  for x := AX1 to AX2 do
  begin
    for y := AY1 to AY2 do
    begin
      if (not FOpenRequests[y * FWidth + x]) and
         (not FBlockCache.QueryID(GetID(x, y), obj)) then
      begin
        SetLength(coords, Length(coords) + 1);
        i := High(coords);
        coords[i].X := x;
        coords[i].Y := y;
        FOpenRequests[y * FWidth + x] := True;
      end;
    end;
  end;
  if Length(coords) > 0 then
    dmNetwork.Send(TRequestBlocksPacket.Create(coords));
end;

function TLandscape.GetMapBlock(AX, AY: Word): TMapBlock;
var
  block: TBlock;
begin
  Result := nil;
  if (AX >= 0) and (AX < FWidth) and (AY >= 0) and (AY < FHeight) then
  begin
    if FBlockCache.QueryID(GetID(AX, AY), TObject(block)) then
      Result := block.Map;
  end;
end;

function TLandscape.GetStaticBlock(AX, AY: Word): TSeperatedStaticBlock;
var
  block: TBlock;
begin
  Result := nil;
  if (AX >= 0) and (AX < FWidth) and (AY >= 0) and (AY < FHeight) then
  begin
    if FBlockCache.QueryID(GetID(AX, AY), TObject(block)) then
      Result := TSeperatedStaticBlock(block.Static);
  end;
end;

procedure TLandscape.UpdateStaticsPriority(AStaticItem: TStaticItem;
  APrioritySolver: Integer);
var
  staticTileData: TStaticTileData;
begin
  staticTileData := ResMan.Tiledata.StaticTiles[AStaticItem.TileID];
  AStaticItem.PriorityBonus := 0;
  if not ((staticTileData.Flags and tdfBackground) = tdfBackground) then
    AStaticItem.PriorityBonus := AStaticItem.PriorityBonus + 1;
  if staticTileData.Height > 0 then
    AStaticItem.PriorityBonus := AStaticItem.PriorityBonus + 1;
  AStaticItem.Priority := AStaticItem.Z + AStaticItem.PriorityBonus;
  AStaticItem.PrioritySolver := APrioritySolver;
end;

procedure TLandscape.OnBlocksPacket(ABuffer: TEnhancedMemoryStream);
var
  index: TGenericIndex;
  map: TMapBlock;
  statics: TStaticBlock;
  coords: TBlockCoords;
  count: Word;
  id: Integer;
begin
  index := TGenericIndex.Create(nil);
  while ABuffer.Position < ABuffer.Size do
  begin
    ABuffer.Read(coords, SizeOf(TBlockCoords));
    id := GetID(coords.X, coords.Y);
  
    map := TMapBlock.Create(ABuffer, coords.X, coords.Y);
    count := ABuffer.ReadWord;
    if count > 0 then
      index.Lookup := ABuffer.Position
    else
      index.Lookup := $FFFFFFFF;
    index.Size := count * 7;
    statics := TSeperatedStaticBlock.Create(ABuffer, index, coords.X, coords.Y);

    FBlockCache.RemoveID(id);
    FBlockCache.StoreID(id, TBlock.Create(map, statics));
    
    FOpenRequests[coords.Y * FWidth + coords.X] := False;
  end;
  index.Free;
  if Assigned(FOnChange) then FOnChange;
end;

procedure TLandscape.OnDrawMapPacket(ABuffer: TEnhancedMemoryStream);
var
  x, y: Word;
  cell: TMapCell;
begin
  x := ABuffer.ReadWord;
  y := ABuffer.ReadWord;
  cell := GetMapCell(x, y);
  if cell <> nil then
  begin
    cell.Altitude := ABuffer.ReadShortInt;
    cell.TileID := ABuffer.ReadWord;
    if Assigned(FOnChange) then FOnChange;
  end;
end;

procedure TLandscape.OnInsertStaticPacket(ABuffer: TEnhancedMemoryStream);
var
  x, y: Word;
  block: TSeperatedStaticBlock;
  staticItem: TStaticItem;
  targetStaticList: TList;
  i: Integer;
begin
  x := ABuffer.ReadWord;
  y := ABuffer.ReadWord;
  block := GetStaticBlock(x div 8, y div 8);
  if block <> nil then
  begin
    staticItem := TStaticItem.Create(nil, nil, 0, 0);
    staticItem.X := x;
    staticItem.Y := y;
    staticItem.Z := ABuffer.ReadShortInt;
    staticItem.TileID := ABuffer.ReadWord;
    staticItem.Hue := ABuffer.ReadWord;
    targetStaticList := block.Cells[(y mod 8) * 8 + x mod 8];
    targetStaticList.Add(staticItem);
    for i := 0 to targetStaticList.Count - 1 do
      UpdateStaticsPriority(TStaticItem(targetStaticList.Items[i]), i);
    ListSort(targetStaticList, @Compare);
    staticItem.Owner := block;
    if Assigned(FOnChange) then FOnChange;
  end;
end;

procedure TLandscape.OnDeleteStaticPacket(ABuffer: TEnhancedMemoryStream);
var
  block: TSeperatedStaticBlock;
  i: Integer;
  statics: TList;
  staticInfo: TStaticInfo;
  staticItem: TStaticItem;
begin
  ABuffer.Read(staticInfo, SizeOf(TStaticInfo));
  block := GetStaticBlock(staticInfo.X div 8, staticInfo.Y div 8);
  if block <> nil then
  begin
    statics := block.Cells[(staticInfo.Y mod 8) * 8 + staticInfo.X mod 8];
    for i := 0 to statics.Count - 1 do
    begin
      staticItem := TStaticItem(statics.Items[i]);
      if (staticItem.Z = staticInfo.Z) and
         (staticItem.TileID = staticInfo.TileID) and
         (staticItem.Hue = staticInfo.Hue) then
      begin
        statics.Delete(i);
        staticItem.Delete;
        if Assigned(FOnChange) then FOnChange;
        Break;
      end;
    end;
  end;
end;

procedure TLandscape.OnElevateStaticPacket(ABuffer: TEnhancedMemoryStream);
var
  block: TSeperatedStaticBlock;
  i,j : Integer;
  statics: TList;
  staticInfo: TStaticInfo;
  staticItem: TStaticItem;
begin
  ABuffer.Read(staticInfo, SizeOf(TStaticInfo));
  block := GetStaticBlock(staticInfo.X div 8, staticInfo.Y div 8);
  if block <> nil then
  begin
    statics := block.Cells[(staticInfo.Y mod 8) * 8 + staticInfo.X mod 8];
    for i := 0 to statics.Count - 1 do
    begin
      staticItem := TStaticItem(statics.Items[i]);
      if (staticItem.Z = staticInfo.Z) and
         (staticItem.TileID = staticInfo.TileID) and
         (staticItem.Hue = staticInfo.Hue) then
      begin
        staticItem.Z := ABuffer.ReadShortInt;
        for j := 0 to statics.Count - 1 do
          UpdateStaticsPriority(TStaticItem(statics.Items[j]), j);
        ListSort(statics, @Compare);
        if Assigned(FOnChange) then FOnChange;
        Break;
      end;
    end;
  end;
end;

procedure TLandscape.OnMoveStaticPacket(ABuffer: TEnhancedMemoryStream);
var
  sourceBlock, targetBlock: TSeperatedStaticBlock;
  i: Integer;
  statics: TList;
  staticInfo: TStaticInfo;
  staticItem: TStaticItem;
  newX, newY: Word;
  item: PLinkedItem;
begin
  staticItem := nil;
  ABuffer.Read(staticInfo, SizeOf(TStaticInfo));
  newX := EnsureRange(ABuffer.ReadWord, 0, FCellWidth - 1);
  newY := EnsureRange(ABuffer.ReadWord, 0, FCellHeight - 1);

  sourceBlock := GetStaticBlock(staticInfo.X div 8, staticInfo.Y div 8);
  targetBlock := GetStaticBlock(newX div 8, newY div 8);
  if sourceBlock <> nil then
  begin
    statics := sourceBlock.Cells[(staticInfo.Y mod 8) * 8 + staticInfo.X mod 8];
    i := 0;
    while (i < statics.Count) and (staticItem = nil) do
    begin
      staticItem := TStaticItem(statics.Items[i]);
      if (staticItem.Z <> staticInfo.Z) or
         (staticItem.TileID <> staticInfo.TileID) or
         (staticItem.Hue <> staticInfo.Hue) then
      begin
        staticItem := nil;
      end;
      Inc(i);
    end;

    if staticItem <> nil then
    begin
      statics.Remove(staticItem);
      staticItem.Delete;
    end;
  end;
  
  if targetBlock <> nil then
  begin
    staticItem := TStaticItem.Create(nil, nil, 0, 0);
    staticItem.X := newX;
    staticItem.Y := newY;
    staticItem.Z := staticInfo.Z;
    staticItem.TileID := staticInfo.TileID;
    staticItem.Hue := staticInfo.Hue;
    statics := targetBlock.Cells[(newY mod 8) * 8 + newX mod 8];
    statics.Add(staticItem);
    for i := 0 to statics.Count - 1 do
      UpdateStaticsPriority(TStaticItem(statics.Items[i]), i);
    ListSort(statics, @Compare);
    staticItem.Owner := targetBlock;
  end;
  
  if Assigned(FOnChange) then FOnChange;
end;

procedure TLandscape.OnHueStaticPacket(ABuffer: TEnhancedMemoryStream);
var
  block: TSeperatedStaticBlock;
  i,j : Integer;
  statics: TList;
  staticInfo: TStaticInfo;
  staticItem: TStaticItem;
begin
  ABuffer.Read(staticInfo, SizeOf(TStaticInfo));
  block := GetStaticBlock(staticInfo.X div 8, staticInfo.Y div 8);
  if block <> nil then
  begin
    statics := block.Cells[(staticInfo.Y mod 8) * 8 + staticInfo.X mod 8];
    for i := 0 to statics.Count - 1 do
    begin
      staticItem := TStaticItem(statics.Items[i]);
      if (staticItem.Z = staticInfo.Z) and
         (staticItem.TileID = staticInfo.TileID) and
         (staticItem.Hue = staticInfo.Hue) then
      begin
        staticItem.Hue := ABuffer.ReadWord;
        if Assigned(FOnChange) then FOnChange;
        Break;
      end;
    end;
  end;
end;

procedure TLandscape.OnRemoveCachedObject(AObject: TObject);
var
  block: TBlock;
begin
  block := AObject as TBlock;
  if block <> nil then
    dmNetwork.Send(TFreeBlockPacket.Create(block.Map.X, block.Map.Y));
end;

{ TMaterial }

constructor TMaterial.Create(AWidth, AHeight: Integer;
  AGraphic: TSingleImage);
var
  caps: TGLTextureCaps;
begin
  inherited Create;
  FRealWidth := AWidth;
  FRealHeight := AHeight;
  GetGLTextureCaps(caps);
  if caps.NonPowerOfTwo then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
  end else
  begin
    if IsPow2(AWidth) then FWidth := AWidth else FWidth := NextPow2(AWidth);
    if IsPow2(AHeight) then FHeight := AHeight else FHeight := NextPow2(AHeight);
  end;
  FGraphic := TSingleImage.CreateFromParams(FWidth, FHeight, ifA8R8G8B8);
  AGraphic.CopyTo(0, 0, AWidth, AHeight, FGraphic, 0, 0);
  UpdateTexture;
end;

destructor TMaterial.Destroy;
begin
  if FGraphic <> nil then FreeAndNil(FGraphic);
  if FTexture <> 0 then glDeleteTextures(1, @FTexture);
  inherited Destroy;
end;

function TMaterial.HitTest(AX, AY: Integer): Boolean;
var
  pixel: TColor32Rec;
begin
  Result := False;
  //writeln(FGraphic.Width, ',', FGraphic.Height, ',', AX, ',', AY);
  if InRange(AX, 0, FGraphic.Width - 1) and
     InRange(AY, 0, FGraphic.Height - 1) then
  begin
    pixel := GetPixel32(FGraphic.ImageDataPointer^, AX, AY);
    if pixel.A > 0 then
      Result := True;
  end;
end;

procedure TMaterial.UpdateTexture;
begin
  if FTexture <> 0 then glDeleteTextures(1, @FTexture);

  FTexture := CreateGLTextureFromImage(FGraphic.ImageDataPointer^, 0, 0, False, ifUnknown, @FWidth, @FHeight);
  glBindTexture(GL_TEXTURE_2D, FTexture);
  {glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @FWidth);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @FHeight);}
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
end;

{ TTileList }

constructor TTileList.Create;
begin
  inherited Create;
  FFirst := nil;
  FLastBlock := nil;
end;

destructor TTileList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTileList.Clear;
var
  current, next: PBlockInfo;
begin
  current := FFirst;
  while current <> nil do
  begin
    next := current^.Next;
    Dispose(current);
    current := next;
  end;
  FFirst := nil;
  FLastBlock := nil;
end;

function TTileList.Iterate(var ABlockInfo: PBlockInfo): Boolean;
begin
  if ABlockInfo = nil then
    ABlockInfo := FFirst
  else
    ABlockInfo := ABlockInfo^.Next;
  Result := ABlockInfo <> nil;
end;

procedure TTileList.Add(AItem: TWorldItem);
var
  current: PBlockInfo;
begin
  New(current);
  current^.Item := AItem;
  current^.Next := nil;
  if FFirst = nil then FFirst := current;
  if FLastBlock <> nil then FLastBlock^.Next := current;
  FLastBlock := current;
end;

procedure TTileList.Delete(AItem: TWorldItem);
var
  current, last, next: PBlockInfo;
begin
  last := nil;
  current := FFirst;
  while current <> nil do
  begin
    if current^.Item = AItem then
    begin
      if FFirst = current then FFirst := current^.Next;
      if FLastBlock = current then FLastBlock := last;
      if last <> nil then last^.Next := current^.Next;
      Dispose(current);
      next := nil;
    end else
      next := current^.Next;
    last := current;
    current := next;
  end;
end;

{ TScreenBuffer }

procedure TScreenBuffer.OnTileRemoved(ATile: TMulBlock);
var
  currentItem, lastItem, nextItem: PBlockInfo;
begin
  lastItem := nil;
  currentItem := FFirst;
  while currentItem <> nil do
  begin
    if currentItem^.Item = ATile then
    begin
      if FFirst = currentItem then FFirst := currentItem^.Next;
      if FLastBlock = currentItem then FLastBlock := lastItem;
      if lastItem <> nil then lastItem^.Next := currentItem^.Next;
      Dispose(currentItem);
      nextItem := nil;
    end else
      nextItem := currentItem^.Next;
    lastItem := currentItem;
    currentItem := nextItem;
  end;
end;

procedure TScreenBuffer.Clear;
var
  current, next: PBlockInfo;
begin
  current := FFirst;
  while current <> nil do
  begin
    next := current^.Next;
    current^.Item.Locked := False;
    current^.Item.OnDestroy.UnregisterEvent(@OnTileRemoved);
    Dispose(current);
    current := next;
  end;
  FFirst := nil;
  FLastBlock := nil;
end;

function TScreenBuffer.Find(AScreenPosition: TPoint): PBlockInfo;
var
  current: PBlockInfo;
begin
  Result := nil;
  current := FFirst;
  while (current <> nil) and (Result = nil) do
  begin
    if PtInRect(current^.ScreenRect, AScreenPosition) and
       current^.Material.HitTest(AScreenPosition.x - current^.ScreenRect.Left,
                                 AScreenPosition.y - current^.ScreenRect.Top) then
    begin
      Result := current;
    end;
    current := current^.Next;
  end;
end;

procedure TScreenBuffer.Store(AScreenRect: TRect; AItem: TWorldItem;
  AMaterial: TMaterial);
var
  current: PBlockInfo;
begin
  New(current);
  AItem.Locked := True;
  AItem.OnDestroy.RegisterEvent(@OnTileRemoved);
  current^.ScreenRect := AScreenRect;
  current^.Item := AItem;
  current^.Material := AMaterial;
  current^.Next := FFirst;
  FFirst := current;
  if FLastBlock = nil then FLastBlock := current;
end;

end.

