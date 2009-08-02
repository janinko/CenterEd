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
unit UStatics;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, UGenericIndex, UWorldItem, UTiledata;

type
  { TStaticItem }

  TStaticItem = class(TWorldItem)
    constructor Create(AOwner: TWorldBlock; AData: TStream; ABlockX, ABlockY: Word); overload;
    constructor Create(AOwner: TWorldBlock; AData: TStream); overload;
  protected
    { Members }
    FHue: Word;
    FOrgHue: Word;
    { Methods }
    function HasChanged: Boolean; override;
    procedure SetHue(AHue: Word);
  public
    { Fields }
    property Hue: Word read FHue write SetHue;
    { Methods }
    function Clone: TStaticItem; override;
    function GetSize: Integer; override;
    procedure InitOriginalState; override;
    procedure UpdatePriorities(ASolver: Integer);
    procedure Write(AData: TStream); override;
  end;

  { TStaticBlock}

  TStaticBlock = class(TWorldBlock)
    constructor Create(AData: TStream; AIndex: TGenericIndex; AX, AY: Word); overload;
    constructor Create(AData: TStream; AIndex: TGenericIndex); overload;
    destructor Destroy; override;
  protected
    { Members }
    FItems: TList;
  public
    { Fields }
    property Items: TList read FItems write FItems;
    { Methods }
    function Clone: TStaticBlock; override;
    function GetSize: Integer; override;
    procedure ReverseWrite(AData: TStream);
    procedure Sort;
    procedure Write(AData: TStream); override;
  end;

  { TSeperatedStaticBlock }

  TSeperatedStaticBlock = class(TStaticBlock)
    constructor Create(AData: TStream; AIndex: TGenericIndex; AX, AY: Word); overload;
    constructor Create(AData: TStream; AIndex: TGenericIndex); overload;
    destructor Destroy; override;
  public
    Cells: array[0..63] of TList;
    { Methods }
    function Clone: TSeperatedStaticBlock; override;
    function GetSize: Integer; override;
    procedure RebuildList;
  end;

implementation

uses
  UGameResources; //Used for priority calculation

{ TStaticItem }

constructor TStaticItem.Create(AOwner: TWorldBlock; AData: TStream; ABlockX, ABlockY: Word);
var
  iX, iY: Byte;
begin
  inherited Create(AOwner);
  if assigned(AData) then
  begin
    AData.Read(FTileID, SizeOf(SmallInt));
    AData.Read(iX, SizeOf(Byte));
    AData.Read(iY, SizeOf(Byte));
    AData.Read(FZ, SizeOf(ShortInt));
    AData.Read(FHue, SizeOf(SmallInt));

    FX := ABlockX * 8 + iX;
    FY := ABlockY * 8 + iY;
  end;
  InitOriginalState;
end;

constructor TStaticItem.Create(AOwner: TWorldBlock; AData: TStream);
begin
  Create(AOwner, AData, 0, 0);
end;

function TStaticItem.HasChanged: Boolean;
begin
  Result := (FHue <> FOrgHue) or inherited HasChanged;
end;

procedure TStaticItem.SetHue(AHue: Word);
begin
  FHue := AHue;
  DoChanged;
end;

function TStaticItem.Clone: TStaticItem;
begin
  Result := TStaticItem.Create(nil, nil);
  Result.FTileID := FTileID;
  Result.FX := FX;
  Result.FY := FY;
  Result.FZ := FZ;
  Result.FHue := FHue;
end;

function TStaticItem.GetSize: Integer;
begin
  Result := 7;
end;

procedure TStaticItem.InitOriginalState;
begin
  FOrgHue := FHue;
  inherited InitOriginalState;
end;

procedure TStaticItem.UpdatePriorities(ASolver: Integer);
var
  staticTileData: TStaticTileData;
begin
  staticTileData := ResMan.Tiledata.StaticTiles[FTileID];
  FPriorityBonus := 0;
  if not ((staticTileData.Flags and tdfBackground) = tdfBackground) then
    Inc(FPriorityBonus);
  if staticTileData.Height > 0 then
    Inc(FPriorityBonus);
  FPriority := Z + FPriorityBonus;
  FPrioritySolver := ASolver;
end;

procedure TStaticItem.Write(AData: TStream);
var
  iX, iY: Byte;
begin
  iX := FX mod 8;
  iY := FY mod 8;

  AData.Write(FTileID, SizeOf(SmallInt));
  AData.Write(iX, SizeOf(Byte));
  AData.Write(iY, SizeOf(Byte));
  AData.Write(FZ, SizeOf(ShortInt));
  AData.Write(FHue, SizeOf(SmallInt));
end;

{ TStaticBlock }

constructor TStaticBlock.Create(AData: TStream; AIndex: TGenericIndex; AX, AY: Word);
var
  i: Integer;
  block: TMemoryStream;
begin
  inherited Create;
  FX := AX;
  FY := AY;

  FItems := TList.Create;
  if assigned(AData) and (AIndex.Lookup > 0) and (AIndex.Size > 0) then
  begin
    AData.Position := AIndex.Lookup;
    block := TMemoryStream.Create;
    block.CopyFrom(AData, AIndex.Size);
    block.Position := 0;
    for i := 1 to (AIndex.Size div 7) do
      FItems.Add(TStaticItem.Create(Self, block, AX, AY));
    block.Free;
  end;
end;

constructor TStaticBlock.Create(AData: TStream; AIndex: TGenericIndex);
begin
  Create(AData, AIndex, 0, 0);
end;

destructor TStaticBlock.Destroy;
var
  i: Integer;
begin
  if Assigned(FItems) then
  begin
    for i := 0 to FItems.Count - 1 do
      if Assigned(FItems[i]) then
      begin
        TStaticItem(FItems[i]).Free;
        FItems[i] := nil;
      end;
    FItems.Free;
    FItems := nil;
  end;
  inherited;
end;

function TStaticBlock.Clone: TStaticBlock;
var
  i: Integer;
begin
  Result := TStaticBlock.Create(nil, nil, FX, FY);
  for i := 0 to FItems.Count - 1 do
    Result.FItems.Add(TStaticItem(FItems.Items[i]).Clone);
end;

function TStaticBlock.GetSize: Integer;
begin
  Result := FItems.Count * 7;
end;

procedure TStaticBlock.ReverseWrite(AData: TStream);
var
  i: Integer;
begin
  for i := FItems.Count - 1 downto 0 do
  begin
    TStaticItem(FItems[i]).Write(AData);
  end;
end;

procedure TStaticBlock.Sort;
begin
  FItems.Sort(@CompareWorldItems);
end;

procedure TStaticBlock.Write(AData: TStream);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    TStaticItem(FItems[i]).Write(AData);
end;

{ TSeperatedStaticBlock }

constructor TSeperatedStaticBlock.Create(AData: TStream; AIndex: TGenericIndex; AX, AY: Word);
var
  i: Integer;
  item: TStaticItem;
  block: TMemoryStream;
begin
  inherited Create;
  FItems := TList.Create;

  FX := AX;
  FY := AY;

  for i := 0 to 63 do
    Cells[i] := TList.Create;

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
  if Assigned(FItems) then FreeAndNil(FItems);
  for i := 0 to 63 do
  begin
    if Cells[i] <> nil then
    begin
      for j := 0 to Cells[i].Count - 1 do
      begin
        if Cells[i][j] <> nil then
        begin
          TStaticItem(Cells[i][j]).Free;
          Cells[i][j] := nil;
        end;
      end;
      Cells[i].Free;
      Cells[i] := nil;
    end;
  end;
  inherited Destroy;
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
        TStaticItem(Cells[i].Items[j]).UpdatePriorities(solver);
        Inc(solver);
      end;
    end;
  end;
  Sort;
end;

function TSeperatedStaticBlock.Clone: TSeperatedStaticBlock;
var
  i, j: Integer;
begin
  Result := TSeperatedStaticBlock.Create(nil, nil, FX, FY);

  for i := 0 to 63 do
    for j := 0 to Cells[i].Count - 1 do
      Result.Cells[i].Add(TSeperatedStaticBlock(Cells[i].Items[j]).Clone);
end;

function TSeperatedStaticBlock.GetSize: Integer;
begin
  RebuildList;
  Result := inherited GetSize;
end;

end.

