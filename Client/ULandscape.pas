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
  SysUtils, Classes, math, LCLIntf, GL, GLU, ImagingOpenGL, Imaging,
  ImagingClasses, ImagingTypes, ImagingUtility,
  UGenericIndex, UMap, UStatics, UArt, UTexture, UTiledata, UHue, UWorldItem,
  UMulBlock,
  UVector, UEnhancedMemoryStream,
  UCacheManager;

type
  PNormals = ^TNormals;
  TNormals = array[0..3] of TVector;
  PRadarBlock = ^TRadarBlock;
  TRadarBlock = array[0..7, 0..7] of Word;
  
  { TMaterial }             //TODO : add ref counting
  
  TMaterial = class
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
  
  TLandTextureManager = class
    constructor Create;
    destructor Destroy; override;
  protected
    FArtCache: TCacheManager;
    FFlatLandArtCache: TCacheManager;
    FTexCache: TCacheManager;
  public
    function GetArtMaterial(ATileID: Word): TMaterial; overload;
    function GetArtMaterial(ATileID: Word; AHue: THue;
      APartialHue: Boolean): TMaterial; overload;
    function GetFlatLandMaterial(ATileID: Word): TMaterial;
    function GetStaticMaterial(AStaticItem: TStaticItem;
      AOverrideHue: Integer = -1): TMaterial;
    function GetTexMaterial(ATileID: Word): TMaterial;
  end;

 { TSeperatedStaticBlock }

  TSeperatedStaticBlock = class(TStaticBlock)
    constructor Create(AData: TStream; AIndex: TGenericIndex; AX, AY: Word); overload;
    constructor Create(AData: TStream; AIndex: TGenericIndex); overload;
    destructor Destroy; override;
  public
    Cells: array[0..63] of TStaticItemList;
    { Methods }
    function Clone: TSeperatedStaticBlock; override;
    function GetSize: Integer; override;
    procedure RebuildList;
  end;

  TLandscape = class;
  
  { TBlock }

  TBlock = class
    constructor Create(AMap: TMapBlock; AStatics: TStaticBlock);
    destructor Destroy; override;
  protected
    { Fields }
    FMapBlock: TMapBlock;
    FStaticBlock: TStaticBlock;
  public
    { Fields }
    property Map: TMapBlock read FMapBlock;
    property Static: TStaticBlock read FStaticBlock;
    { Methods }
    procedure UpdateBlockAcess(ALandscape: TLandscape);
  end;
  
  TLandscapeChangeEvent = procedure of object;
  TMapChangedEvent = procedure(AMapCell: TMapCell) of object;
  TNewBlockEvent = procedure(ABlock: TBlock) of object;
  TStaticChangedEvent = procedure(AStaticItem: TStaticItem) of object;

  TScreenBuffer = class;

  { TLandscape }

  TLandscape = class
    constructor Create(AWidth, AHeight: Word);
    destructor Destroy; override;
  protected
    { Members }
    FWidth: Word;
    FHeight: Word;
    FCellWidth: Word;
    FCellHeight: Word;
    FBlockCache: TCacheManager;
    FOnChange: TLandscapeChangeEvent;
    FOnMapChanged: TMapChangedEvent;
    FOnNewBlock: TNewBlockEvent;
    FOnStaticInserted: TStaticChangedEvent;
    FOnStaticDeleted: TStaticChangedEvent;
    FOnStaticElevated: TStaticChangedEvent;
    FOnStaticHued: TStaticChangedEvent;
    FOpenRequests: TBits;
    FWriteMap: TBits;
    { Methods }
    function GetMapBlock(AX, AY: Word): TMapBlock;
    function GetMapCell(AX, AY: Word): TMapCell;
    function GetNormals(AX, AY: Word): TNormals;
    function GetStaticBlock(AX, AY: Word): TSeperatedStaticBlock;
    function GetStaticList(AX, AY: Word): TStaticItemList;
    { Events }
    procedure OnRemoveCachedObject(AObject: TObject);
    procedure OnBlocksPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnDrawMapPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnInsertStaticPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnDeleteStaticPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnElevateStaticPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnMoveStaticPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnHueStaticPacket(ABuffer: TEnhancedMemoryStream);
  public
    { Fields }
    property Width: Word read FWidth;
    property Height: Word read FHeight;
    property CellWidth: Word read FCellWidth;
    property CellHeight: Word read FCellHeight;
    property MapCell[X, Y: Word]: TMapCell read GetMapCell;
    property StaticList[X, Y: Word]: TStaticItemList read GetStaticList;
    property Normals[X, Y: Word]: TNormals read GetNormals;
    property OnChange: TLandscapeChangeEvent read FOnChange write FOnChange;
    property OnMapChanged: TMapChangedEvent read FOnMapChanged write FOnMapChanged;
    property OnNewBlock: TNewBlockEvent read FOnNewBlock write FOnNewBlock;
    property OnStaticInserted: TStaticChangedEvent read FOnStaticInserted
      write FOnStaticInserted;
    property OnStaticDeleted: TStaticChangedEvent read FOnStaticDeleted
      write FOnStaticDeleted;
    property OnStaticElevated: TStaticChangedEvent read FOnStaticElevated
      write FOnStaticElevated;
    property OnStaticHued: TStaticChangedEvent read FOnStaticHued
      write FOnStaticHued;
    { Methods }
    function CanWrite(AX, AY: Word): Boolean;
    procedure FillDrawList(ADrawList: TScreenBuffer; AX, AY, AWidth,
      AHeight: Word; AMap, AStatics: Boolean; ANoDraw: Boolean;
      AAdditionalTiles: TWorldItemList = nil);
    function GetEffectiveAltitude(ATile: TMapCell): ShortInt;
    function GetLandAlt(AX, AY: Word; ADefault: ShortInt): ShortInt;
    procedure GetNormals(AX, AY: Word; var ANormals: TNormals);
    procedure MoveStatic(AStatic: TStaticItem; AX, AY: Word);
    procedure PrepareBlocks(AX1, AY1, AX2, AY2: Word);
    procedure UpdateBlockAccess;
    procedure UpdateWriteMap(AStream: TEnhancedMemoryStream);
  end;

  TScreenState = (ssNormal, ssFiltered, ssGhost);

  PBlockInfo = ^TBlockInfo;
  TBlockInfo = record
    ScreenRect: TRect;
    DrawQuad: array[0..3,0..1] of TGLfloat;
    Item: TWorldItem;
    HighRes: TMaterial;
    LowRes: TMaterial;
    Normals: PNormals;
    State: TScreenState;
    Highlighted: Boolean;
    HueOverride: Boolean;
    Next: PBlockInfo;
  end;

  { TScreenBuffer }

  TScreenBuffer = class
    constructor Create; virtual;
    destructor Destroy; override;
  protected
    { Members }
    FCount: Cardinal;
    FShortCuts: array[-1..10] of PBlockInfo; //-1 = last, 0 = first, 1..10 = other shortcuts
    FShortCutsValid: Boolean;
    FSerial: Cardinal;
  public
    { Methods }
    function Add(AItem: TWorldItem): PBlockInfo;
    procedure Clear;
    procedure Delete(AItem: TWorldItem);
    function Find(AScreenPosition: TPoint): PBlockInfo;
    function GetSerial: Cardinal;
    function Insert(AItem: TWorldItem): PBlockInfo;
    function Iterate(var ABlockInfo: PBlockInfo): Boolean;
    procedure UpdateShortcuts;
    function UpdateSortOrder(AItem: TWorldItem): PBlockInfo;
    { Events }
    procedure OnTileRemoved(ATile: TMulBlock);
  end;
  
  TStaticInfo = packed record
    X: Word;
    Y: Word;
    Z: ShortInt;
    TileID: Word;
    Hue: Word;
  end;

implementation

uses
  UGameResources, UdmNetwork, UPackets, UPacketHandlers, Logging;

function GetID(AX, AY: Word): Integer;
begin
  Result := ((AX and $7FFF) shl 15) or (AY and $7FFF);
end;

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

function TLandTextureManager.GetArtMaterial(ATileID: Word; AHue: THue;
  APartialHue: Boolean): TMaterial;
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

function TLandTextureManager.GetStaticMaterial(AStaticItem: TStaticItem;
  AOverrideHue: Integer = -1): TMaterial;
var
  staticTiledata: TStaticTiledata;
  hue: THue;
begin
  staticTiledata := ResMan.Tiledata.StaticTiles[AStaticItem.TileID];
  if AOverrideHue < 0 then
    AOverrideHue := AStaticItem.Hue;

  if AOverrideHue > 0 then
    hue := ResMan.Hue.Hues[AOverrideHue]
  else
    hue := nil;

  Result := GetArtMaterial($4000 + AStaticItem.TileID, hue,
    tdfPartialHue in staticTiledata.Flags);
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
    Cells[i] := TStaticItemList.Create;

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
  i: Integer;
begin
  FreeAndNil(FItems);

  for i := 0 to 63 do
  begin
    if Cells[i] <> nil then
      FreeAndNil(Cells[i]);
  end;

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
        TStaticItem(Cells[i].Items[j]).UpdatePriorities(
          ResMan.Tiledata.StaticTiles[TStaticItem(Cells[i].Items[j]).TileID],
          solver);
        Inc(solver);
      end;
    end;
  end;
  Sort;
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

procedure TBlock.UpdateBlockAcess(ALandscape: TLandscape);
var
  staticItem: TStaticItem;
  i: Integer;
begin
  for i := Low(FMapBlock.Cells) to High(FMapBlock.Cells) do
  begin
    FMapBlock.Cells[i].CanBeEdited := ALandscape.CanWrite(FMapBlock.Cells[i].X,
      FMapBlock.Cells[i].Y);
  end;

  if FStaticBlock is TSeperatedStaticBlock then
    TSeperatedStaticBlock(FStaticBlock).RebuildList; //fill items

  for i := 0 to FStaticBlock.Items.Count - 1 do
  begin
    staticItem := TStaticItem(FStaticBlock.Items[i]);
    staticItem.CanBeEdited := ALandscape.CanWrite(staticItem.X,
      staticItem.Y);
  end;
end;

{ TLandscape }

constructor TLandscape.Create(AWidth, AHeight: Word);
var
  i: Integer;
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FCellWidth := FWidth * 8;
  FCellHeight := FHeight * 8;
  FBlockCache := TCacheManager.Create(256);
  FBlockCache.OnRemoveObject := @OnRemoveCachedObject;

  FOnChange := nil;
  FOnNewBlock := nil;
  FOnStaticDeleted := nil;
  FOnStaticElevated := nil;
  FOnStaticHued := nil;
  FOnStaticInserted := nil;

  FOpenRequests := TBits.Create(FWidth * FHeight);
  FOpenRequests.Clearall; //set all to 0
  FWriteMap := TBits.Create(FCellWidth * FCellHeight);
  for i := 0 to FWriteMap.Size - 1 do
    FWriteMap[i] := True;

  RegisterPacketHandler($04, TPacketHandler.Create(0, @OnBlocksPacket));
  RegisterPacketHandler($06, TPacketHandler.Create(8, @OnDrawMapPacket));
  RegisterPacketHandler($07, TPacketHandler.Create(10, @OnInsertStaticPacket));
  RegisterPacketHandler($08, TPacketHandler.Create(10, @OnDeleteStaticPacket));
  RegisterPacketHandler($09, TPacketHandler.Create(11, @OnElevateStaticPacket));
  RegisterPacketHandler($0A, TPacketHandler.Create(14, @OnMoveStaticPacket));
  RegisterPacketHandler($0B, TPacketHandler.Create(12, @OnHueStaticPacket));
end;

destructor TLandscape.Destroy;
begin
  if FBlockCache <> nil then
  begin
    FBlockCache.OnRemoveObject := nil;
    FreeAndNil(FBlockCache);
  end;

  FreeAndNil(FOpenRequests);
  FreeAndNil(FWriteMap);
  
  RegisterPacketHandler($04, nil);
  RegisterPacketHandler($06, nil);
  RegisterPacketHandler($07, nil);
  RegisterPacketHandler($08, nil);
  RegisterPacketHandler($09, nil);
  RegisterPacketHandler($0A, nil);
  RegisterPacketHandler($0B, nil);
  
  inherited Destroy;
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

function TLandscape.GetNormals(AX, AY: Word): TNormals;
begin
  GetNormals(AX, AY, Result);
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

procedure TLandscape.OnRemoveCachedObject(AObject: TObject);
var
  block: TBlock;
begin
  block := AObject as TBlock;
  if block <> nil then
    dmNetwork.Send(TFreeBlockPacket.Create(block.Map.X, block.Map.Y));
end;

procedure TLandscape.OnBlocksPacket(ABuffer: TEnhancedMemoryStream);
var
  index: TGenericIndex;
  map: TMapBlock;
  statics: TStaticBlock;
  coords: TBlockCoords;
  count: Word;
  id: Integer;
  block: TBlock;
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
      index.Lookup := -1;
    index.Size := count * 7;
    statics := TSeperatedStaticBlock.Create(ABuffer, index, coords.X, coords.Y);

    FBlockCache.RemoveID(id);
    block := TBlock.Create(map, statics);
    block.UpdateBlockAcess(Self);
    FBlockCache.StoreID(id, block);

    FOpenRequests[coords.Y * FWidth + coords.X] := False;

    if Assigned(FOnNewBlock) then FOnNewBlock(block);
  end;
  index.Free;
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
    if Assigned(FOnMapChanged) then FOnMapChanged(cell);
  end;
end;

procedure TLandscape.OnInsertStaticPacket(ABuffer: TEnhancedMemoryStream);
var
  x, y: Word;
  block: TSeperatedStaticBlock;
  staticItem: TStaticItem;
  targetStaticList: TStaticItemList;
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
      targetStaticList.Items[i].UpdatePriorities(
        ResMan.Tiledata.StaticTiles[targetStaticList.Items[i].TileID],
        i);
    targetStaticList.Sort(@CompareStaticItems);
    staticItem.Owner := block;
    staticItem.CanBeEdited := CanWrite(x, y);

    if Assigned(FOnStaticInserted) then FOnStaticInserted(staticItem);
  end;
end;

procedure TLandscape.OnDeleteStaticPacket(ABuffer: TEnhancedMemoryStream);
var
  block: TSeperatedStaticBlock;
  i: Integer;
  statics: TStaticItemList;
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
      staticItem := statics.Items[i];
      if (staticItem.Z = staticInfo.Z) and
         (staticItem.TileID = staticInfo.TileID) and
         (staticItem.Hue = staticInfo.Hue) then
      begin
        if Assigned(FOnStaticDeleted) then FOnStaticDeleted(staticItem);
        staticItem.Delete;
        statics.Delete(i);

        Break;
      end;
    end;
  end;
end;

procedure TLandscape.OnElevateStaticPacket(ABuffer: TEnhancedMemoryStream);
var
  block: TSeperatedStaticBlock;
  i,j : Integer;
  statics: TStaticItemList;
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
      staticItem := statics.Items[i];
      if (staticItem.Z = staticInfo.Z) and
         (staticItem.TileID = staticInfo.TileID) and
         (staticItem.Hue = staticInfo.Hue) then
      begin
        staticItem.Z := ABuffer.ReadShortInt;
        for j := 0 to statics.Count - 1 do
          statics.Items[j].UpdatePriorities(
            ResMan.Tiledata.StaticTiles[statics.Items[j].TileID],
            j);
        statics.Sort(@CompareStaticItems);

        if Assigned(FOnStaticElevated) then FOnStaticElevated(staticItem);

        Break;
      end;
    end;
  end;
end;

procedure TLandscape.OnMoveStaticPacket(ABuffer: TEnhancedMemoryStream);
var
  sourceBlock, targetBlock: TSeperatedStaticBlock;
  i: Integer;
  statics: TStaticItemList;
  staticInfo: TStaticInfo;
  staticItem: TStaticItem;
  newX, newY: Word;
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
      staticItem := statics.Items[i];
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
      if Assigned(FOnStaticDeleted) then FOnStaticDeleted(staticItem);
      staticItem.Delete;
      statics.Remove(staticItem);
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
      TStaticItem(statics.Items[i]).UpdatePriorities(
        ResMan.Tiledata.StaticTiles[TStaticItem(statics.Items[i]).TileID],
        i);
    statics.Sort(@CompareStaticItems);
    staticItem.Owner := targetBlock;
    staticItem.CanBeEdited := CanWrite(newX, newY);

    if Assigned(FOnStaticInserted) then FOnStaticInserted(staticItem);
  end;
end;

procedure TLandscape.OnHueStaticPacket(ABuffer: TEnhancedMemoryStream);
var
  block: TSeperatedStaticBlock;
  i : Integer;
  statics: TStaticItemList;
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
      staticItem := statics.Items[i];
      if (staticItem.Z = staticInfo.Z) and
         (staticItem.TileID = staticInfo.TileID) and
         (staticItem.Hue = staticInfo.Hue) then
      begin
        staticItem.Hue := ABuffer.ReadWord;
        if Assigned(FOnStaticHued) then FOnStaticHued(staticItem);
        Break;
      end;
    end;
  end;
end;

function TLandscape.CanWrite(AX, AY: Word): Boolean;
begin
  Result := FWriteMap[AX * FCellHeight + AY];
end;

procedure TLandscape.FillDrawList(ADrawList: TScreenBuffer; AX, AY, AWidth,
  AHeight: Word; AMap, AStatics: Boolean; ANoDraw: Boolean;
  AAdditionalTiles: TWorldItemList = nil);
var
  drawMapCell: TMapCell;
  drawStatics: TStaticItemList;
  i, x, y: Integer;
  tempDrawList: TWorldItemList;
begin
  ADrawList.Clear;
  tempDrawList := TWorldItemList.Create(False);;
  for x := AX to AX + AWidth do
  begin
    for y := AY to AY + AWidth do
    begin
      if AMap then
      begin
        drawMapCell := GetMapCell(x, y);
        if (drawMapCell <> nil) and (ANoDraw or (drawMapCell.TileID > 2)) then
        begin
          drawMapCell.Priority := GetEffectiveAltitude(drawMapCell);
          drawMapCell.PriorityBonus := 0;
          drawMapCell.PrioritySolver := 0;
          tempDrawList.Add(drawMapCell);
        end;
      end;

      if AStatics then
      begin
        drawStatics := GetStaticList(x, y);
        if drawStatics <> nil then
          for i := 0 to drawStatics.Count - 1 do
          begin
            drawStatics[i].UpdatePriorities(
              ResMan.Tiledata.StaticTiles[drawStatics[i].TileID],
              ADrawList.GetSerial);
            tempDrawList.Add(drawStatics[i]);
          end;
      end;
    end;
  end;

  for i := 0 to AAdditionalTiles.Count - 1 do
    tempDrawList.Add(AAdditionalTiles[i]);

  tempDrawList.Sort(@CompareWorldItems);
  for i := 0 to tempDrawList.Count - 1 do
    ADrawList.Add(TWorldItem(tempDrawList[i]));
  tempDrawList.Free;
end;

function TLandscape.GetEffectiveAltitude(ATile: TMapCell): ShortInt;
var
  north, west, south, east: ShortInt;
begin
  north := ATile.Altitude;
  west := GetLandAlt(ATile.X, ATile.Y + 1, north);
  south := GetLandAlt(ATile.X + 1, ATile.Y + 1, north);
  east := GetLandAlt(ATile.X + 1, ATile.Y, north);

  if Abs(north - south) >= Abs(west - east) then
    Result := Min(north, south) + Abs(west - east) div 2
  else
    Result := Min(north, south) + Abs(north - south) div 2;
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

procedure TLandscape.GetNormals(AX, AY: Word; var ANormals: TNormals);
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
    if cell <> nil then
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
  ANormals[0] := VectorNorm(VectorAdd(VectorAdd(VectorAdd(north, west), east), south));

  north := cells[1, 0][2];
  west := cells[1, 1][1];
  east := cells[2, 0][3];
  south := cells[2, 1][0];
  ANormals[1] := VectorNorm(VectorAdd(VectorAdd(VectorAdd(north, west), east), south));

  north := cells[1, 1][2];
  west := cells[1, 2][1];
  east := cells[2, 1][3];
  south := cells[2, 2][0];
  ANormals[2] := VectorNorm(VectorAdd(VectorAdd(VectorAdd(north, west), east), south));

  north := cells[0, 1][2];
  west := cells[0, 2][1];
  east := cells[1, 1][3];
  south := cells[1, 2][0];
  ANormals[3] := VectorNorm(VectorAdd(VectorAdd(VectorAdd(north, west), east), south));
end;

procedure TLandscape.MoveStatic(AStatic: TStaticItem; AX, AY: Word);
var
  sourceBlock, targetBlock: TSeperatedStaticBlock;
  targetStaticList: TStaticItemList;
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
        targetStaticList.Items[i].UpdatePriorities(
          ResMan.Tiledata.StaticTiles[targetStaticList.Items[i].TileID],
          i);
      targetStaticList.Sort(@CompareStaticItems);
      AStatic.UpdatePos(AX, AY, AStatic.Z);
      AStatic.Owner := targetBlock;
    end;
  end;
end;

procedure TLandscape.PrepareBlocks(AX1, AY1, AX2, AY2: Word);
var
  x, y, i: Integer;
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

procedure TLandscape.UpdateBlockAccess;
var
  cacheEntry: PCacheEntry;
begin
  cacheEntry := nil;
  while FBlockCache.Iterate(cacheEntry) do
    if cacheEntry^.Obj <> nil then
      TBlock(cacheEntry^.Obj).UpdateBlockAcess(Self);
end;

procedure TLandscape.UpdateWriteMap(AStream: TEnhancedMemoryStream);
var
  x1, y1, x2, y2: Word;
  i, areaCount, cellX, cellY: Integer;
begin
  Logger.EnterMethod([lcLandscape, lcDebug], 'TLandscape.UpdateWriteMap');

  areaCount := AStream.ReadWord;
  Logger.Send([lcLandscape, lcDebug], 'AreaCount', areaCount);

  if areaCount > 0 then
  begin
    FWriteMap.Clearall;
    for i := 0 to areaCount - 1 do
    begin
      x1 := AStream.ReadWord;
      y1 := AStream.ReadWord;
      x2 := AStream.ReadWord;
      y2 := AStream.ReadWord;
      for cellX := x1 to x2 do
        for cellY := y1 to y2 do
          FWriteMap[cellX * FCellHeight + cellY] := True;
    end;
  end else
    for i := 0 to FWriteMap.Size - 1 do
      FWriteMap[i] := True;

  Logger.Send([lcLandscape, lcDebug], 'WriteMap @ 0,0', FWriteMap[0]);

  UpdateBlockAccess;
  Logger.ExitMethod([lcLandscape, lcDebug], 'TLandscape.UpdateWriteMap');
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
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
end;

{ TScreenBuffer }

constructor TScreenBuffer.Create;
begin
  inherited Create;
  FCount := 0;
  FSerial := 0;
  UpdateShortcuts;
end;

destructor TScreenBuffer.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TScreenBuffer.Add(AItem: TWorldItem): PBlockInfo;
begin
  New(Result);
  AItem.Locked := True;
  AItem.OnDestroy.RegisterEvent(@OnTileRemoved);
  Result^.Item := AItem;
  Result^.HighRes := nil;
  Result^.LowRes := nil;
  Result^.Normals := nil;
  Result^.State := ssNormal;
  Result^.Highlighted := False;
  Result^.Next := nil;

  if FShortCuts[0] = nil then //First element
  begin
    FShortCuts[0] := Result;
    FShortCuts[-1] := Result; //Last element
  end else
  begin
    FShortCuts[-1]^.Next := Result;
    FShortCuts[-1] := Result;
  end;

  Inc(FCount);
end;

procedure TScreenBuffer.Clear;
var
  current, next: PBlockInfo;
begin
  current := FShortCuts[0];
  while current <> nil do
  begin
    next := current^.Next;
    current^.Item.Locked := False;
    current^.Item.OnDestroy.UnregisterEvent(@OnTileRemoved);
    if current^.Normals <> nil then Dispose(current^.Normals);
    Dispose(current);
    current := next;
  end;
  FShortCuts[0] := nil;
  FShortCuts[-1] := nil;

  FCount := 0;
  FSerial := 0;

  UpdateShortcuts;
end;

procedure TScreenBuffer.Delete(AItem: TWorldItem);
var
  current, last, next: PBlockInfo;
begin
  last := nil;
  current := FShortCuts[0];
  while current <> nil do
  begin
    if current^.Item = AItem then
    begin
      if FShortCuts[-1] = current then FShortCuts[-1] := last;
      if FShortCuts[0] = current then FShortCuts[0] := current^.Next;
      if last <> nil then last^.Next := current^.Next;

      if current^.Normals <> nil then Dispose(current^.Normals);

      Dispose(current);
      Dec(FCount);
      FShortCutsValid := False;
      next := nil;
    end else
      next := current^.Next;

    last := current;
    current := next;
  end;
end;

function TScreenBuffer.Find(AScreenPosition: TPoint): PBlockInfo;
var
  current: PBlockInfo;
begin
  Result := nil;
  current := FShortCuts[0];
  while current <> nil do //search the last matching tile
  begin
    if (current^.State = ssNormal) and
       PtInRect(current^.ScreenRect, AScreenPosition) and
       current^.LowRes.HitTest(AScreenPosition.x - current^.ScreenRect.Left,
                               AScreenPosition.y - current^.ScreenRect.Top) then
    begin
      Result := current;
    end;
    current := current^.Next;
  end;
end;

function TScreenBuffer.GetSerial: Cardinal;
begin
  Result := FSerial;
  Inc(FSerial);
end;

function TScreenBuffer.Insert(AItem: TWorldItem): PBlockInfo;
var
  current: PBlockInfo;
  shortcut: Integer;
begin
  if not FShortCutsValid then
    UpdateShortcuts;

  New(Result);
  AItem.Locked := True;
  AItem.OnDestroy.RegisterEvent(@OnTileRemoved);
  Result^.Item := AItem;
  Result^.HighRes := nil;
  Result^.LowRes := nil;
  Result^.Normals := nil;
  Result^.State := ssNormal;
  Result^.Highlighted := False;

  if (FShortCuts[0] = nil) or (CompareWorldItems(AItem, FShortCuts[0]^.Item) < 0) then
  begin
    if FShortCuts[0] = nil then
      FShortCuts[-1] := Result;  //Update last item

    Result^.Next := FShortCuts[0];
    FShortCuts[0] := Result;
  end else
  begin
    //find best entry point
    shortcut := 0;
    while (shortcut <= 10) and (FShortCuts[shortcut] <> nil) and
      (CompareWorldItems(AItem, FShortCuts[shortcut]^.Item) >= 0) do
    begin
      current := FShortCuts[shortcut];
      Inc(shortcut);
    end;

    //now find the real match
    while (current^.Next <> nil) and
          (CompareWorldItems(AItem, current^.Next^.Item) > 0) do
    begin
      current := current^.Next;
    end;

    if FShortCuts[-1] = current^.Next then
      FShortCuts[-1] := Result;  //Update last item

    Result^.Next := current^.Next;
    current^.Next := Result;
  end;

  Inc(FCount);
end;

function TScreenBuffer.Iterate(var ABlockInfo: PBlockInfo): Boolean;
begin
  if ABlockInfo = nil then
    ABlockInfo := FShortCuts[0]
  else
    ABlockInfo := ABlockInfo^.Next;
  Result := ABlockInfo <> nil;
end;

procedure TScreenBuffer.UpdateShortcuts;
var
  shortcut, step, nextStep, stepSize: Integer;
  blockInfo: PBlockInfo;
begin
  if FCount < 10 then
  begin
    for shortcut := 1 to 10 do
      FShortCuts[shortcut] := nil;
  end
  else if FShortCuts[0] <> nil then
  begin
    stepSize := FCount div 10;
    nextStep := stepSize;
    step := 0;
    shortcut := 1;
    blockInfo := FShortCuts[0];
    repeat
      if step = nextStep then
      begin
        FShortCuts[shortcut] := blockInfo;
        Inc(shortcut);
        Inc(nextStep, stepSize);
      end;

      Inc(step);

      FShortCuts[-1] := blockInfo; //update last known item
      blockInfo := blockInfo^.Next;
    until (blockInfo = nil);
  end;
  FShortCutsValid := True;
end;

function TScreenBuffer.UpdateSortOrder(AItem: TWorldItem): PBlockInfo;
var
  newNodePosition, oldNode, oldNodePrev, current: PBlockInfo;
begin
  newNodePosition := nil;
  oldNode := nil;
  oldNodePrev := nil;
  current := FShortCuts[0];

  while (current <> nil) and ((oldNode = nil) or (newNodePosition = nil)) do
  begin
    if current^.Item = AItem then
      oldNode := current
    else if oldNode = nil then
      oldNodePrev := current;

    if newNodePosition = nil then
    begin
      if (current^.Next = nil) or (CompareWorldItems(AItem, current^.Next^.Item) < 0) then
        newNodePosition := current;
    end;

    current := current^.Next;
  end;

  if oldNode <> newNodePosition then
  begin
    if oldNodePrev <> oldNode then
    begin
      if oldNodePrev = nil then
        FShortCuts[0] := oldNode^.Next
      else
        oldNodePrev^.Next := oldNode^.Next;
    end;

    if (newNodePosition = FShortCuts[0]) and (CompareWorldItems(AItem, FShortCuts[0]^.Item) < 0) then
    begin
      oldNode^.Next := FShortCuts[0];
      FShortCuts[0] := oldNode;
    end else
    begin
      oldNode^.Next := newNodePosition^.Next;
      newNodePosition^.Next := oldNode;
    end;
  end;

  Result := oldNode;
end;

procedure TScreenBuffer.OnTileRemoved(ATile: TMulBlock);
begin
  Delete(TWorldItem(ATile));
end;

end.

