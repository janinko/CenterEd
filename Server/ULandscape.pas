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
unit ULandscape;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, math, UGenericIndex, UMap, UStatics, UTiledata,
  UWorldItem, UMulBlock,
  UTileDataProvider, URadarMap,
  UCacheManager, ULinkedList, UBufferedStreams,
  UEnhancedMemoryStream, UPacketHandlers, UPackets, UNetState, UEnums;

type
  PRadarBlock = ^TRadarBlock;
  TRadarBlock = array[0..7, 0..7] of Word;
  TBlockSubscriptions = array of TLinkedList;

  { TSeperatedStaticBlock }

  TSeperatedStaticBlock = class(TStaticBlock)
    constructor Create(AData: TStream; AIndex: TGenericIndex; AX, AY: Word); overload;
    constructor Create(AData: TStream; AIndex: TGenericIndex); overload;
    destructor Destroy; override;
  protected
    FTiledataProvider: TTiledataProvider;
  public
    { Fields }
    Cells: array[0..63] of TStaticItemList;
    property TiledataProvider: TTiledataProvider read FTiledataProvider write FTiledataProvider;

    { Methods }
    function Clone: TSeperatedStaticBlock; override;
    function GetSize: Integer; override;
    procedure RebuildList;
  end;
  
  { TBlock }

  TBlock = class(TObject)
    constructor Create(AMap: TMapBlock; AStatics: TSeperatedStaticBlock);
    destructor Destroy; override;
  protected
    FMapBlock: TMapBlock;
    FStaticBlock: TSeperatedStaticBlock;
  public
    property Map: TMapBlock read FMapBlock;
    property Static: TSeperatedStaticBlock read FStaticBlock;
  end;

  TBlockCache = specialize TCacheManager<TBlock>;

  { TLandscape }

  TLandscape = class(TObject)
    constructor Create(AMap, AStatics, AStaIdx, ATiledata, ARadarCol: string;
      AWidth, AHeight: Word; var AValid: Boolean);
    constructor Create(AMap, AStatics, AStaIdx, ATiledata: TStream;
      ARadarCol: string; AWidth, AHeight: Word; var AValid: Boolean);
    destructor Destroy; override;
  protected
    FWidth: Word;
    FHeight: Word;
    FCellWidth: Word;
    FCellHeight: Word;
    FMap: TStream;
    FStatics: TStream;
    FStaIdx: TStream;
    FTiledata: TStream;
    FTiledataProvider: TTiledataProvider;
    FOwnsStreams: Boolean;
    FRadarMap: TRadarMap;
    FBlockCache: TBlockCache;
    FBlockSubscriptions: TBlockSubscriptions;
    procedure OnBlockChanged(ABlock: TMulBlock);
    procedure OnRemoveCachedObject(ABlock: TBlock);
    function GetMapCell(AX, AY: Word): TMapCell;
    function GetStaticList(AX, AY: Word): TStaticItemList;
    function GetBlockSubscriptions(AX, AY: Word): TLinkedList;

    procedure OnDrawMapPacket(ABuffer: TEnhancedMemoryStream;
      ANetState: TNetState);
    procedure OnInsertStaticPacket(ABuffer: TEnhancedMemoryStream;
      ANetState: TNetState);
    procedure OnDeleteStaticPacket(ABuffer: TEnhancedMemoryStream;
      ANetState: TNetState);
    procedure OnElevateStaticPacket(ABuffer: TEnhancedMemoryStream;
      ANetState: TNetState);
    procedure OnMoveStaticPacket(ABuffer: TEnhancedMemoryStream;
      ANetState: TNetState);
    procedure OnHueStaticPacket(ABuffer: TEnhancedMemoryStream;
      ANetState: TNetState);
    procedure OnLargeScaleCommandPacket(ABuffer: TEnhancedMemoryStream;
      ANetState: TNetState);
  public
    property Width: Word read FWidth;
    property Height: Word read FHeight;
    property CellWidth: Word read FCellWidth;
    property CellHeight: Word read FCellHeight;
    property MapCell[X, Y: Word]: TMapCell read GetMapCell;
    property StaticList[X, Y: Word]: TStaticItemList read GetStaticList;
    property BlockSubscriptions[X, Y: Word]: TLinkedList read GetBlockSubscriptions;
    property TiledataProvider: TTiledataProvider read FTiledataProvider;

    function GetMapBlock(AX, AY: Word): TMapBlock;
    function GetStaticBlock(AX, AY: Word): TSeperatedStaticBlock;
    function LoadBlock(AX, AY: Word): TBlock;

    procedure UpdateRadar(AX, AY: Word);
    function GetEffectiveAltitude(ATile: TMapCell): ShortInt;
    function GetLandAlt(AX, AY: Word; ADefault: ShortInt): ShortInt;
    procedure SortStaticsList(AStatics: TStaticItemList);

    procedure Flush;
    procedure SaveBlock(AWorldBlock: TWorldBlock);
    function Validate: Boolean;
  end;
  
  TStaticInfo = packed record
    X: Word;
    Y: Word;
    Z: ShortInt;
    TileID: Word;
    Hue: Word;
  end;
  TAreaInfo = packed record
    Left: Word;
    Top: Word;
    Right: Word;
    Bottom: Word;
  end;
  TWorldPoint = packed record
    X: Word;
    Y: Word;
  end;
  
function PointInArea(AArea: TAreaInfo; AX, AY: Word): Boolean; inline;

implementation

uses
  UCEDServer, UConnectionHandling, UConfig, ULargeScaleOperations;

function GetID(AX, AY: Word): Integer;
begin
  Result := ((AX and $7FFF) shl 15) or (AY and $7FFF);
end;

function PointInArea(AArea: TAreaInfo; AX, AY: Word): Boolean;
begin
  Result := InRange(AX, AArea.Left, AArea.Right) and
            InRange(AY, AArea.Top, AArea.Bottom);
end;

{ TSeperatedStaticBlock }

constructor TSeperatedStaticBlock.Create(AData: TStream; AIndex: TGenericIndex;
  AX, AY: Word);
var
  i: Integer;
  item: TStaticItem;
  block: TMemoryStream;
begin
  inherited Create;
  FItems := TStaticItemList.Create(False);

  FX := AX;
  FY := AY;

  for i := 0 to 63 do
    Cells[i] := TStaticItemList.Create(True);

  if (AData <> nil) and (AIndex.Lookup > 0) and (AIndex.Size > 0) then
  begin
    AData.Position := AIndex.Lookup;
    block := TMemoryStream.Create;
    block.CopyFrom(AData, AIndex.Size);
    block.Position := 0;
    for i := 1 to (AIndex.Size div 7) do
    begin
      item := TStaticItem.Create(Self, block, AX, AY);
      Cells[(item.Y mod 8) * 8 + (item.X mod 8)].Add(item);
    end;
    block.Free;
  end;
end;

constructor TSeperatedStaticBlock.Create(AData: TStream; AIndex: TGenericIndex);
begin
  Create(AData, AIndex, 0, 0);
end;

destructor TSeperatedStaticBlock.Destroy;
var
  i, j: Integer;
begin
  FreeAndNil(FItems);

  for i := 0 to 63 do
    FreeAndNil(Cells[i]);

  inherited Destroy;
end;

function TSeperatedStaticBlock.Clone: TSeperatedStaticBlock;
begin
  raise Exception.Create('TSeperatedStaticBlock.Clone is not implemented (yet).');
end;

function TSeperatedStaticBlock.GetSize: Integer;
begin
  RebuildList;
  Result := inherited GetSize;
end;

procedure TSeperatedStaticBlock.RebuildList;
var
  i, j, solver: Integer;
begin
  FItems.Clear;
  solver := 0;
  for i := 0 to 63 do
  begin
    if Cells[i] <> nil then
    begin
      for j := 0 to Cells[i].Count - 1 do
      begin
        FItems.Add(Cells[i].Items[j]);
        Cells[i].Items[j].UpdatePriorities(
          FTiledataProvider.StaticTiles[Cells[i].Items[j].TileID], solver);
        Inc(solver);
      end;
    end;
  end;
  Sort;
end;

{ TBlock }

constructor TBlock.Create(AMap: TMapBlock; AStatics: TSeperatedStaticBlock);
begin
  inherited Create;
  FMapBlock := AMap;
  FStaticBlock := AStatics;
end;

destructor TBlock.Destroy;
begin
  FreeAndNil(FMapBlock);
  FreeAndNil(FStaticBlock);
  inherited Destroy;
end;

{ TLandscape }

constructor TLandscape.Create(AMap, AStatics, AStaIdx, ATiledata,
  ARadarCol: string; AWidth, AHeight: Word; var AValid: Boolean);
var
  map, statics, staidx, tiledata: TStream;
begin
  Write(TimeStamp, 'Loading Map');
  map := TFileStream.Create(AMap, fmOpenReadWrite);
  Write(', Statics');
  statics := TFileStream.Create(AStatics, fmOpenReadWrite);
  Write(', StaIdx');
  staidx := TBufferedReader.Create(TFileStream.Create(AStaIdx, fmOpenReadWrite), True);
  Writeln(', Tiledata');
  tiledata := TFileStream.Create(ATiledata, fmOpenRead or fmShareDenyWrite);
  Create(map, statics, staidx, tiledata, ARadarCol, AWidth, AHeight, AValid);
  FOwnsStreams := True;
end;

constructor TLandscape.Create(AMap, AStatics, AStaIdx, ATiledata: TStream;
  ARadarCol: string; AWidth, AHeight: Word; var AValid: Boolean);
var
  blockID: Integer;
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FCellWidth := FWidth * 8;
  FCellHeight := FHeight * 8;
  FMap := AMap;
  FStatics := AStatics;
  FStaIdx := AStaIdx;
  FTiledata := ATiledata;
  FOwnsStreams := False;
  AValid := Validate;
  if AValid then
  begin
    Write(TimeStamp, 'Creating Cache');
    FBlockCache := TBlockCache.Create(256);
    FBlockCache.OnRemoveObject := @OnRemoveCachedObject;
    Write(', Tiledata');
    FTiledataProvider := TTiledataProvider.Create(ATiledata);
    Write(', Subscriptions');
    SetLength(FBlockSubscriptions, AWidth * AHeight);
    for blockID := 0 to AWidth * AHeight - 1 do
      FBlockSubscriptions[blockID] := TLinkedList.Create;

    Writeln(', RadarMap');
    FRadarMap := TRadarMap.Create(FMap, FStatics, FStaIdx, FWidth, FHeight,
      ARadarCol);

    RegisterPacketHandler($06, TPacketHandler.Create(8, @OnDrawMapPacket));
    RegisterPacketHandler($07, TPacketHandler.Create(10, @OnInsertStaticPacket));
    RegisterPacketHandler($08, TPacketHandler.Create(10, @OnDeleteStaticPacket));
    RegisterPacketHandler($09, TPacketHandler.Create(11, @OnElevateStaticPacket));
    RegisterPacketHandler($0A, TPacketHandler.Create(14, @OnMoveStaticPacket));
    RegisterPacketHandler($0B, TPacketHandler.Create(12, @OnHueStaticPacket));
    RegisterPacketHandler($0E, TPacketHandler.Create(0, @OnLargeScaleCommandPacket));
  end;
end;

destructor TLandscape.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FBlockSubscriptions) - 1 do
    FreeAndNil(FBlockSubscriptions[i]);
  FreeAndNil(FBlockCache);
  FreeAndNil(FTiledataProvider);
  FreeAndNil(FRadarMap);
  if FOwnsStreams then
  begin
    FreeAndNil(FMap);
    FreeAndNil(FStatics);
    FreeAndNil(FStaIdx);
    FreeAndNil(FTiledata);
  end;
  
  RegisterPacketHandler($06, nil);
  RegisterPacketHandler($07, nil);
  RegisterPacketHandler($08, nil);
  RegisterPacketHandler($09, nil);
  RegisterPacketHandler($0A, nil);
  RegisterPacketHandler($0B, nil);
  RegisterPacketHandler($0E, nil);
  
  inherited Destroy;
end;

function TLandscape.GetBlockSubscriptions(AX, AY: Word): TLinkedList;
begin
  if (AX >= 0) and (AX <= FWidth) and (AY >= 0) and (AY <= FHeight) then
    Result := FBlockSubscriptions[(AY * FWidth) + AX]
  else
    Result := nil;
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
begin
  if (AX >= 0) and (AX < FCellWidth) and (AY >= 0) and (AY < FCellHeight) then
    Result := MapCell[AX, AY].Altitude
  else
    Result := ADefault;
end;

function TLandscape.GetStaticList(AX, AY: Word): TStaticItemList;
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

procedure TLandscape.UpdateRadar(AX, AY: Word);
var
  mapTile: TMapCell;
  tile: TWorldItem;
  staticItems: TStaticItemList;
  tiles: TWorldItemList;
  i: Integer;
begin
  if (AX mod 8 = 0) and (AY mod 8 = 0) then
  begin
    staticItems := GetStaticList(AX, AY);
    if staticItems <> nil then
    begin
      tiles := TWorldItemList.Create(False);
      mapTile := GetMapCell(AX, AY);
      if mapTile <> nil then
      begin
        mapTile.Priority := GetEffectiveAltitude(mapTile);
        mapTile.PriorityBonus := 0;
        mapTile.PrioritySolver := 0;
        tiles.Add(mapTile);
      end;
      for i := 0 to staticItems.Count - 1 do
      begin
        staticItems[i].UpdatePriorities(
          FTiledataProvider.StaticTiles[staticItems[i].TileID],
          i + 1);
        tiles.Add(staticItems[i]);
      end;
      tiles.Sort(@CompareWorldItems);

      if tiles.Count > 0 then
      begin
        tile := tiles[tiles.Count - 1];
        if tile is TStaticItem then
          FRadarMap.Update(AX div 8, AY div 8, tile.TileID + $4000)
        else
          FRadarMap.Update(AX div 8, AY div 8, tile.TileID)
      end;

      tiles.Free;
    end;
  end;
end;

procedure TLandscape.SortStaticsList(AStatics: TStaticItemList);
var
  i: Integer;
begin
  for i := 0 to AStatics.Count - 1 do
    AStatics[i].UpdatePriorities(
      FTiledataProvider.StaticTiles[AStatics[i].TileID],
      i + 1);
  AStatics.Sort(@CompareStaticItems);
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

procedure TLandscape.OnBlockChanged(ABlock: TMulBlock);
begin
  // Do nothing for now
end;

procedure TLandscape.OnRemoveCachedObject(ABlock: TBlock);
begin
  if ABlock <> nil then
  begin
    if ABlock.Map.Changed then SaveBlock(ABlock.Map);
    if ABlock.Static.Changed then SaveBlock(ABlock.Static);
  end;
end;

function TLandscape.GetMapBlock(AX, AY: Word): TMapBlock;
var
  block: TBlock;
begin
  Result := nil;
  if (AX >= 0) and (AX < FWidth) and (AY >= 0) and (AY < FHeight) then
  begin
    if FBlockCache.QueryID(GetID(AX, AY), block) then
      Result := block.Map
    else
      Result := LoadBlock(AX, AY).Map;
  end;
end;

function TLandscape.GetStaticBlock(AX, AY: Word): TSeperatedStaticBlock;
var
  block: TBlock;
begin
  Result := nil;
  if (AX >= 0) and (AX < FWidth) and (AY >= 0) and (AY < FHeight) then
  begin
    if FBlockCache.QueryID(GetID(AX, AY), block) then
      Result := TSeperatedStaticBlock(block.Static)
    else
      Result := TSeperatedStaticBlock(LoadBlock(AX, AY).Static);
  end;
end;

function TLandscape.LoadBlock(AX, AY: Word): TBlock;
var
  map: TMapBlock;
  statics: TSeperatedStaticBlock;
  index: TGenericIndex;
begin
  FMap.Position := ((AX * FHeight) + AY) * 196;
  map := TMapBlock.Create(FMap, AX, AY);
  map.OnChanged := @OnBlockChanged;

  FStaIdx.Position := ((AX * FHeight) + AY) * 12;
  index := TGenericIndex.Create(FStaIdx);
  statics := TSeperatedStaticBlock.Create(FStatics, index, AX, AY);
  statics.OnChanged := @OnBlockChanged;
  statics.TiledataProvider := FTiledataProvider;
  index.Free;
  
  Result := TBlock.Create(map, statics);
  FBlockCache.StoreID(GetID(AX, AY), Result);
end;

//Intelligent write: replace if possible, otherwise extend

procedure TLandscape.Flush;
begin
  FBlockCache.Clear; //Clear writes modified blocks before removing them from the cache
end;

procedure TLandscape.SaveBlock(AWorldBlock: TWorldBlock);
var
  i, j, size: Integer;
  index: TGenericIndex;
begin
  if AWorldBlock is TMapBlock then
  begin
    FMap.Position := ((AWorldBlock.X * FHeight) + AWorldBlock.Y) * 196;
    AWorldBlock.Write(FMap);
    for i := 0 to 63 do
      TMapBlock(AWorldBlock).Cells[i].InitOriginalState;
    AWorldBlock.CleanUp;
  end else if AWorldBlock is TStaticBlock then
  begin
    FStaIdx.Position := ((AWorldBlock.X * FHeight) + AWorldBlock.Y) * 12;
    index := TGenericIndex.Create(FStaIdx);
    size := AWorldBlock.GetSize;
    if (size > index.Size) or (index.Lookup < 0) then
    begin
      FStatics.Position := FStatics.Size;
      index.Lookup := FStatics.Position;
    end;
    index.Size := size;
    if size = 0 then
      index.Lookup := -1
    else
    begin
      FStatics.Position := index.Lookup;
      AWorldBlock.Write(FStatics);
    end;
    FStaIdx.Seek(-12, soFromCurrent);
    index.Write(FStaIdx);
    index.Free;
    for i := 0 to 63 do
      for j := 0 to TSeperatedStaticBlock(AWorldBlock).Cells[i].Count - 1 do
        TStaticItem(TSeperatedStaticBlock(AWorldBlock).Cells[i].Items[j]).InitOriginalState;
    AWorldBlock.CleanUp;
  end;
end;

function TLandscape.Validate: Boolean;
var
  blocks: Integer;
begin
  blocks := FWidth * FHeight;
  FStaIdx.Seek(0, soFromEnd); //workaround for TBufferedStream
  Result := (FMap.Size = (blocks * 196)) and (FStaIdx.Position = (blocks * 12));
end;

procedure TLandscape.OnDrawMapPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  x, y: Word;
  cell: TMapCell;
  subscriptions: TLinkedList;
  subscriptionItem: PLinkedItem;
  packet: TDrawMapPacket;
begin
  x := ABuffer.ReadWord;
  y := ABuffer.ReadWord;

  if not ValidateAccess(ANetState, alNormal, x, y) then Exit;

  cell := GetMapCell(x, y);
  if cell <> nil then
  begin
    cell.Altitude := ABuffer.ReadShortInt;
    cell.TileID := ABuffer.ReadWord;
    
    packet := TDrawMapPacket.Create(cell);
    subscriptions := FBlockSubscriptions[(y div 8) * FWidth + (x div 8)];
    subscriptionItem := nil;
    while subscriptions.Iterate(subscriptionItem) do
      CEDServerInstance.SendPacket(TNetState(subscriptionItem^.Data), packet, False);
    packet.Free;
    
    UpdateRadar(x, y);
  end;
end;

procedure TLandscape.OnInsertStaticPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  x, y: Word;
  block: TSeperatedStaticBlock;
  staticItem: TStaticItem;
  targetStaticList: TStaticItemList;
  subscriptions: TLinkedList;
  subscriptionItem: PLinkedItem;
  packet: TInsertStaticPacket;
begin
  x := ABuffer.ReadWord;
  y := ABuffer.ReadWord;

  if not ValidateAccess(ANetState, alNormal, x, y) then Exit;

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
    SortStaticsList(targetStaticList);
    staticItem.Owner := block;
    
    packet := TInsertStaticPacket.Create(staticItem);
    subscriptions := FBlockSubscriptions[(y div 8) * FWidth + (x div 8)];
    subscriptionItem := nil;
    while subscriptions.Iterate(subscriptionItem) do
      CEDServerInstance.SendPacket(TNetState(subscriptionItem^.Data), packet, False);
    packet.Free;
    
    UpdateRadar(x, y);
  end;
end;

procedure TLandscape.OnDeleteStaticPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  block: TSeperatedStaticBlock;
  i: Integer;
  statics: TStaticItemList;
  staticInfo: TStaticInfo;
  staticItem: TStaticItem;
  subscriptions: TLinkedList;
  subscriptionItem: PLinkedItem;
  packet: TDeleteStaticPacket;
begin
  ABuffer.Read(staticInfo, SizeOf(TStaticInfo));

  if not ValidateAccess(ANetState, alNormal, staticInfo.X, staticInfo.Y) then Exit;

  block := GetStaticBlock(staticInfo.X div 8, staticInfo.Y div 8);
  if block <> nil then
  begin
    statics := block.Cells[(staticInfo.Y mod 8) * 8 + staticInfo.X mod 8];
    for i := 0 to statics.Count - 1 do
    begin
      staticItem := statics[i];
      if (staticItem.Z = staticInfo.Z) and
         (staticItem.TileID = staticInfo.TileID) and
         (staticItem.Hue = staticInfo.Hue) then
      begin
        packet := TDeleteStaticPacket.Create(staticItem);

        staticItem.Delete;
        statics.Delete(i);
        
        subscriptions := FBlockSubscriptions[(staticInfo.Y div 8) * FWidth +
          (staticInfo.X div 8)];
        subscriptionItem := nil;
        while subscriptions.Iterate(subscriptionItem) do
          CEDServerInstance.SendPacket(TNetState(subscriptionItem^.Data),
            packet, False);
        packet.Free;
        
        UpdateRadar(staticInfo.X, staticInfo.Y);
        
        Break;
      end;
    end;
  end;
end;

procedure TLandscape.OnElevateStaticPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  block: TSeperatedStaticBlock;
  i: Integer;
  statics: TStaticItemList;
  staticInfo: TStaticInfo;
  staticItem: TStaticItem;
  newZ: ShortInt;
  subscriptions: TLinkedList;
  subscriptionItem: PLinkedItem;
  packet: TElevateStaticPacket;
begin
  ABuffer.Read(staticInfo, SizeOf(TStaticInfo));

  if not ValidateAccess(ANetState, alNormal, staticInfo.X, staticInfo.Y) then Exit;

  block := GetStaticBlock(staticInfo.X div 8, staticInfo.Y div 8);
  if block <> nil then
  begin
    statics := block.Cells[(staticInfo.Y mod 8) * 8 + staticInfo.X mod 8];
    for i := 0 to statics.Count - 1 do
    begin
      staticItem := statics[i];
      if (staticItem.Z = staticInfo.Z) and
         (staticItem.TileID = staticInfo.TileID) and
         (staticItem.Hue = staticInfo.Hue) then
      begin
        newZ := ABuffer.ReadShortInt;
        packet := TElevateStaticPacket.Create(staticItem, newZ);

        staticItem.Z := newZ;
        SortStaticsList(statics);

        subscriptions := FBlockSubscriptions[(staticInfo.Y div 8) * FWidth +
          (staticInfo.X div 8)];
        subscriptionItem := nil;
        while subscriptions.Iterate(subscriptionItem) do
          CEDServerInstance.SendPacket(TNetState(subscriptionItem^.Data),
            packet, False);
        packet.Free;
        
        UpdateRadar(staticInfo.X, staticInfo.Y);

        Break;
      end;
    end;
  end;
end;

procedure TLandscape.OnMoveStaticPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  sourceBlock, targetBlock: TSeperatedStaticBlock;
  sourceSubscriptions, targetSubscriptions: TList;
  i: Integer;
  statics: TStaticItemList;
  staticInfo: TStaticInfo;
  staticItem: TStaticItem;
  newX, newY: Word;
  subscriptions: TLinkedList;
  subscriptionItem: PLinkedItem;
  insertPacket: TInsertStaticPacket;
  deletePacket: TDeleteStaticPacket;
  movePacket: TMoveStaticPacket;
begin
  staticItem := nil;
  ABuffer.Read(staticInfo, SizeOf(TStaticInfo));
  newX := EnsureRange(ABuffer.ReadWord, 0, FCellWidth - 1);
  newY := EnsureRange(ABuffer.ReadWord, 0, FCellHeight - 1);

  //Check, if both, source and target, are within a valid region
  if not ValidateAccess(ANetState, alNormal, staticInfo.X, staticInfo.Y) then Exit;
  if not ValidateAccess(ANetState, alNormal, newX, newY) then Exit;
  
  if (staticInfo.X = newX) and (staticInfo.Y = newY) then Exit;
  
  if ((abs(staticInfo.X - newX) > 8) or (abs(staticInfo.Y - newY) > 8)) and
     (not ValidateAccess(ANetState, alAdministrator)) then Exit;
  
  sourceBlock := GetStaticBlock(staticInfo.X div 8, staticInfo.Y div 8);
  targetBlock := GetStaticBlock(newX div 8, newY div 8);
  if (sourceBlock <> nil) and (targetBlock <> nil) then
  begin
    statics := sourceBlock.Cells[(staticInfo.Y mod 8) * 8 + staticInfo.X mod 8];
    i := 0;
    while (i < statics.Count) and (staticItem = nil) do
    begin
      staticItem := statics[i];
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
      deletePacket := TDeleteStaticPacket.Create(staticItem);
      movePacket := TMoveStaticPacket.Create(staticItem, newX, newY);

      i := statics.IndexOf(staticItem);
      statics[i] := nil;
      statics.Delete(i);

      statics := targetBlock.Cells[(newY mod 8) * 8 + newX mod 8];
      statics.Add(staticItem);
      staticItem.UpdatePos(newX, newY, staticItem.Z);
      staticItem.Owner := targetBlock;

      insertPacket := TInsertStaticPacket.Create(staticItem);

      SortStaticsList(statics);

      sourceSubscriptions := TList.Create;
      subscriptions := FBlockSubscriptions[(staticInfo.Y div 8) * FWidth + (staticInfo.X div 8)];
      subscriptionItem := nil;
      while subscriptions.Iterate(subscriptionItem) do
        sourceSubscriptions.Add(subscriptionItem^.Data);

      targetSubscriptions := TList.Create;
      subscriptions := FBlockSubscriptions[(newY div 8) * FWidth + (newX div 8)];
      subscriptionItem := nil;
      while subscriptions.Iterate(subscriptionItem) do
        targetSubscriptions.Add(subscriptionItem^.Data);

      for i := 0 to sourceSubscriptions.Count - 1 do
      begin
        if targetSubscriptions.IndexOf(sourceSubscriptions.Items[i]) > -1 then
          CEDServerInstance.SendPacket(TNetState(sourceSubscriptions.Items[i]), movePacket, False)
        else
          CEDServerInstance.SendPacket(TNetState(sourceSubscriptions.Items[i]), deletePacket, False);
      end;

      for i := 0 to targetSubscriptions.Count - 1 do
      begin
        if sourceSubscriptions.IndexOf(targetSubscriptions.Items[i]) = -1 then
          CEDServerInstance.SendPacket(TNetState(sourceSubscriptions.Items[i]), insertPacket, False);
      end;
      
      UpdateRadar(staticInfo.X, staticInfo.Y);
      UpdateRadar(newX, newY);

      insertPacket.Free;
      deletePacket.Free;
      movePacket.Free;
    end;
  end;
end;

procedure TLandscape.OnHueStaticPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  block: TSeperatedStaticBlock;
  i: Integer;
  statics: TStaticItemList;
  staticInfo: TStaticInfo;
  staticItem: TStaticItem;
  newHue: Word;
  subscriptions: TLinkedList;
  subscriptionItem: PLinkedItem;
  packet: THueStaticPacket;
begin
  ABuffer.Read(staticInfo, SizeOf(TStaticInfo));

  if not ValidateAccess(ANetState, alNormal, staticInfo.X, staticInfo.Y) then Exit;

  block := GetStaticBlock(staticInfo.X div 8, staticInfo.Y div 8);
  if block <> nil then
  begin
    statics := block.Cells[(staticInfo.Y mod 8) * 8 + staticInfo.X mod 8];
    for i := 0 to statics.Count - 1 do
    begin
      staticItem := statics[i];
      if (staticItem.Z = staticInfo.Z) and
         (staticItem.TileID = staticInfo.TileID) and
         (staticItem.Hue = staticInfo.Hue) then
      begin
        newHue := ABuffer.ReadWord;
        packet := THueStaticPacket.Create(staticItem, newHue);

        staticItem.Hue := newHue;

        subscriptions := FBlockSubscriptions[(staticInfo.Y div 8) * FWidth + (staticInfo.X div 8)];
        subscriptionItem := nil;
        while subscriptions.Iterate(subscriptionItem) do
          CEDServerInstance.SendPacket(TNetState(subscriptionItem^.Data), packet, False);
        packet.Free;

        Break;
      end;
    end;
  end;
end;

procedure TLandscape.OnLargeScaleCommandPacket(ABuffer: TEnhancedMemoryStream;
  ANetState: TNetState);
var
  areaInfo: array of TAreaInfo;
  areaCount: Byte;
  i: Integer;
  blockX, blockY, cellX, cellY, x, y: Word;
  realBlockX, realBlockY, realCellX, realCellY: Word;
  blockOffX, cellOffX, modX, blockOffY, cellOffY, modY: Integer;
  blockID, cellID: Cardinal;
  emptyBits: TBits;
  bitMask: array of TBits;
  mapTile: TMapCell;
  statics: TStaticItemList;
  operations: TList;
  clients: array of record
    NetState: TNetState;
    Blocks: TBlockCoordsArray;
  end;
  netState: TNetState;
  subscriptions: TLinkedList;
  subscriptionItem: PLinkedItem;
  cmOperation: TLSCopyMove;
  additionalAffectedBlocks: TBits;
begin
  if not ValidateAccess(ANetState, alAdministrator) then Exit;
  Writeln(TimeStamp, ANetState.Account.Name, ' begins large scale operation');
  CEDServerInstance.SendPacket(nil, TServerStatePacket.Create(ssOther,
    Format('%s is performing large scale operations ...', [ANetState.Account.Name])));

  //Bitmask
  emptyBits := TBits.Create(64);
  SetLength(bitMask, FWidth * FHeight);
  for i := Low(bitMask) to High(bitMask) do
    bitMask[i] := TBits.Create(64);
  //'additionalAffectedBlocks' is used to store whether a certain block was
  //touched during an operation which was designated to another block (for
  //example by moving items with an offset). This is (indirectly) merged later
  //on.
  additionalAffectedBlocks := TBits.Create(FWidth * FHeight);

  areaCount := ABuffer.ReadByte;
  SetLength(areaInfo, areaCount);
  for i := 0 to areaCount - 1 do
  begin
    areaInfo[i].Left := Max(ABuffer.ReadWord, 0);
    areaInfo[i].Top := Max(ABuffer.ReadWord, 0);
    areaInfo[i].Right := Min(ABuffer.ReadWord, FCellWidth - 1);
    areaInfo[i].Bottom := Min(ABuffer.ReadWord, FCellHeight - 1);
    for x := areaInfo[i].Left to areaInfo[i].Right do
      for y := areaInfo[i].Top to areaInfo[i].Bottom do
      begin
        blockID := (x div 8) * FHeight + (y div 8);
        cellID := (y mod 8) * 8 + (x mod 8);
        bitMask[blockID].Bits[cellID] := True;
      end;
  end;
  
  //client blocks
  SetLength(clients, 0);
  CEDServerInstance.TCPServer.IterReset;
  while CEDServerInstance.TCPServer.IterNext do
  begin
    netState := TNetState(CEDServerInstance.TCPServer.Iterator.UserData);
    if netState <> nil then
    begin
      SetLength(clients, Length(clients) + 1);
      clients[High(clients)].NetState := netState;
      SetLength(clients[High(clients)].Blocks, 0);
    end;
  end;

  operations := TList.Create;
  
  cmOperation := nil;
  if ABuffer.ReadBoolean then
  begin
    cmOperation := TLSCopyMove.Init(ABuffer, Self);
    if (cmOperation.OffsetX <> 0) or (cmOperation.OffsetY <> 0) then
    begin
      operations.Add(cmOperation);

      if cmOperation.OffsetX > 0 then
      begin
        blockOffX := FWidth - 1;
        cellOffX := 7;
        modX := -1;
      end else
      begin
        blockOffX := 0;
        cellOffX := 0;
        modX := 1;
      end;

      if cmOperation.OffsetY > 0 then
      begin
        blockOffY := FHeight - 1;
        cellOffY := 7;
        modY := -1;
      end else
      begin
        blockOffY := 0;
        cellOffY := 0;
        modY := 1;
      end;
    end else
      FreeAndNil(cmOperation);
  end;
  if cmOperation = nil then
  begin
    blockOffX := 0;
    cellOffX := 0;
    modX := 1;
    blockOffY := 0;
    cellOffY := 0;
    modY := 1;
  end;
  if ABuffer.ReadBoolean then operations.Add(TLSSetAltitude.Init(ABuffer, Self));
  if ABuffer.ReadBoolean then operations.Add(TLSDrawTerrain.Init(ABuffer, Self));
  if ABuffer.ReadBoolean then operations.Add(TLSDeleteStatics.Init(ABuffer, Self));
  if ABuffer.ReadBoolean then operations.Add(TLSInsertStatics.Init(ABuffer, Self));
  
  FRadarMap.BeginUpdate;
  for blockX := 0 to FWidth - 1 do
  begin
    realBlockX := blockOffX + modX * blockX;
    for blockY := 0 to FHeight - 1 do
    begin
      realBlockY := blockOffY + modY * blockY;
      blockID := (realBlockX * FHeight) + realBlockY;
      if bitMask[blockID].Equals(emptyBits) then Continue;
      
      for cellY := 0 to 7 do
      begin
        realCellY := cellOffY + modY * cellY;
        for cellX := 0 to 7 do
        begin
          realCellX := cellOffX + modX * cellX;
          if bitMask[blockID].Bits[(realCellY * 8) + realCellX] then
          begin
            x := realBlockX * 8 + realCellX;
            y := realBlockY * 8 + realCellY;
            mapTile := GetMapCell(x, y);
            statics := GetStaticList(x, y);
            for i := 0 to operations.Count - 1 do
              TLargeScaleOperation(operations.Items[i]).Apply(mapTile, statics,
                additionalAffectedBlocks);
            SortStaticsList(statics);
              
            UpdateRadar(x, y);
          end;
        end;
      end;

      //Find out, which clients are affected by which blocks.
      //This is used to efficiently update the block subscriptions.
      subscriptions := FBlockSubscriptions[realBlockY * FWidth + realBlockX];
      for i := Low(clients) to High(clients) do
      begin
        subscriptionItem := nil;
        while subscriptions.Iterate(subscriptionItem) do
        begin
          if TNetState(subscriptionItem^.Data) = clients[i].NetState then
          begin
            SetLength(clients[i].Blocks, Length(clients[i].Blocks) + 1);
            with clients[i].Blocks[High(clients[i].Blocks)] do
            begin
              X := realBlockX;
              Y := realBlockY;
            end;
            Break;
          end;
        end;
      end;
      
    end;
  end;
  
  //additional blocks
  for blockX := 0 to FWidth - 1 do
  begin
    for blockY := 0 to FHeight - 1 do
    begin
      blockID := (blockX * FHeight) + blockY;
      if bitMask[blockID].Equals(emptyBits) and additionalAffectedBlocks[blockID] then
      begin
        //Update the information, which client is affected on which subscribed
        //block.
        subscriptions := FBlockSubscriptions[blockY * FWidth + blockX];
        for i := Low(clients) to High(clients) do
        begin
          subscriptionItem := nil;
          while subscriptions.Iterate(subscriptionItem) do
          begin
            if TNetState(subscriptionItem^.Data) = clients[i].NetState then
            begin
              SetLength(clients[i].Blocks, Length(clients[i].Blocks) + 1);
              with clients[i].Blocks[High(clients[i].Blocks)] do
              begin
                X := blockX;
                Y := blockY;
              end;
              Break;
            end;
          end;
        end;
        
        UpdateRadar(blockX * 8, blockY * 8);
        
      end;
    end;
  end;
  
  //clean up
  for i := Low(bitMask) to High(bitMask) do
    bitMask[i].Free;
  emptyBits.Free;
  additionalAffectedBlocks.Free;
  
  for i := 0 to operations.Count - 1 do
    TLargeScaleOperation(operations.Items[i]).Free;
  operations.Free;
  
  //Update clients
  FRadarMap.EndUpdate;
  for i := Low(clients) to High(clients) do
  begin
    if Length(clients[i].Blocks) > 0 then
    begin
      CEDServerInstance.SendPacket(clients[i].NetState, TCompressedPacket.Create(
        TBlockPacket.Create(clients[i].Blocks, nil)));
      clients[i].NetState.LastAction := Now;
    end;
  end;

  CEDServerInstance.SendPacket(nil, TServerStatePacket.Create(ssRunning));
  Writeln(TimeStamp, 'Large scale operation ended.');
end;

end.

