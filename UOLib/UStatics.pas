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
unit UStatics;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, UMulBlock, UGenericIndex, UTiledata, UWorldItem;

type
  TStaticItem = class(TWorldItem)
    constructor Create(AOwner: TWorldBlock; AData: TStream; ABlockX, ABlockY: Word); overload;
    constructor Create(AOwner: TWorldBlock; AData: TStream); overload;
    function Clone: TStaticItem; override;
    function GetIdentifier: Integer;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  protected
    FHue, FOrgHue: Word;
    procedure SetHue(AHue: Word);
    function HasChanged: Boolean; override;
  public
    procedure InitOriginalState; override;
    property Hue: Word read FHue write SetHue;
  end;
  TStaticBlock = class(TWorldBlock)
    constructor Create(AData: TStream; AIndex: TGenericIndex; AX, AY: Word); overload;
    constructor Create(AData: TStream; AIndex: TGenericIndex); overload;
    destructor Destroy; override;
    function Clone: TStaticBlock; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
    procedure ReverseWrite(AData: TStream);
    procedure Sort;
  protected
    FItems: TList;
  public
    property Items: TList read FItems write FItems;
  end;
  TSeperatedStaticBlock = class(TStaticBlock)
    constructor Create(AData: TStream; AIndex: TGenericIndex; AX, AY: Word); overload;
    constructor Create(AData: TStream; AIndex: TGenericIndex); overload;
    destructor Destroy; override;
    function Clone: TSeperatedStaticBlock; override;
    function GetSize: Integer; override;
  protected
    procedure RefreshList;
  public
    Cells: array[0..63] of TList;
  end;

implementation

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

function TStaticItem.Clone: TStaticItem;
begin
  Result := TStaticItem.Create(nil, nil);
  Result.FTileID := FTileID;
  Result.FX := FX;
  Result.FY := FY;
  Result.FZ := FZ;
  Result.FHue := FHue;
end;

function TStaticItem.GetIdentifier: Integer;
begin
  Result := 0 or (((FX mod 8) shl 28) and $F0000000) or (((FY mod 8) shl 24) and $0F000000) or ((Byte(FZ) shl 16) and $00FF0000) or (Word(FTileID) and $0000FFFF);
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

function TStaticItem.GetSize: Integer;
begin
  Result := 7;
end;

function TStaticItem.HasChanged: Boolean;
begin
  Result := (FHue <> FOrgHue) or inherited HasChanged;
end;

procedure TStaticItem.InitOriginalState;
begin
  FOrgHue := FHue;
  inherited InitOriginalState;
end;

procedure TStaticItem.SetHue(AHue: Word);
begin
  FHue := AHue;
  DoChanged;
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

procedure TStaticBlock.Write(AData: TStream);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    TStaticItem(FItems[i]).Write(AData);
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
var
  iMin, iMax: Integer;

  procedure sift;
  var
    i, j: integer;
  begin
    i := iMin;
    j := 2 * i;
    FItems[0] := FItems[i];
    while j <= iMax do
    begin
      if j < iMax then
        if TStaticItem(FItems[j]).GetIdentifier < TStaticItem(FItems[j + 1]).GetIdentifier then inc(j);
      if TStaticItem(FItems[0]).GetIdentifier >= TStaticItem(FItems[j]).GetIdentifier then break;
      FItems[i] := FItems[j];
      i := j;
      j := 2 * i;
    end;
    FItems[i] := FItems[0];
  end;

begin
  if FItems.Count > 0 then
  begin
    iMax := FItems.Count;
    iMin := iMax div 2 + 1;
    FItems.Insert(0, nil);
    while iMin > 1 do
    begin
      dec(iMin);
      sift;
    end;
    while iMax > 1 do
    begin
      FItems[0] := FItems[iMin];
      FItems[iMin] := FItems[iMax];
      FItems[iMax] := FItems[0];
      dec(iMax);
      sift;
    end;
    FItems.Delete(0);
  end;
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
  RefreshList;
  Result := inherited GetSize;
end;

procedure TSeperatedStaticBlock.RefreshList;
var
  i, j: Integer;
begin
  FItems.Clear;
  for i := 0 to 63 do
  begin
    if Cells[i] <> nil then
    begin
      for j := 0 to Cells[i].Count - 1 do
        if Cells[i].Items[j] <> nil then
          FItems.Add(Cells[i].Items[j]);
    end;
  end;
  Sort;
end;

end.

