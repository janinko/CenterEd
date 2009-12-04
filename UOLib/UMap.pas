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
unit UMap;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fgl, UMulBlock, UWorldItem;

const
  MapCellSize = 3;
  MapBlockSize = 4 + (64 * MapCellSize);

type

  { TMapCell }

  TMapCell = class(TWorldItem)
    constructor Create(AOwner: TWorldBlock; AData: TStream; AX, AY: Word); overload;
    constructor Create(AOwner: TWorldBlock; AData: TStream); overload;
  protected
    FIsGhost: Boolean;
    FGhostZ: ShortInt;
    FGhostID: Word;
    function GetTileID: Word; override;
    function GetZ: ShortInt; override;
  public
    property Altitude: ShortInt read GetZ write SetZ;
    property IsGhost: Boolean read FIsGhost write FIsGhost;
    property GhostZ: ShortInt read FGhostZ write FGhostZ;
    property GhostID: Word write FGhostID;

    function Clone: TMapCell; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  end;

  TMapCellList = specialize TFPGObjectList<TMapCell>;

  { TMapBlock }

  TMapBlock = class(TWorldBlock)
    constructor Create(AData: TStream; AX, AY: Word); overload;
    constructor Create(AData: TStream); overload;
    destructor Destroy; override;
  protected
    FHeader: LongInt;
  public
    Cells: array[0..63] of TMapCell;
    property Header: LongInt read FHeader write FHeader;
    function Clone: TMapBlock; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  end;

function GetMapCellOffset(ABlock: Integer): Integer;

implementation

function GetMapCellOffset(ABlock: Integer): Integer;
var
  group, tile: Integer;
begin
  group := ABlock div 64;
  tile := ABlock mod 64;

  Result := group * MapBlockSize + 4 + tile * MapCellSize;
end;

{ TMapCell }

constructor TMapCell.Create(AOwner: TWorldBlock; AData: TStream; AX, AY: Word);
begin
  inherited Create(AOwner);

  FX := AX;
  FY := AY;
  if AData <> nil then
  begin
    AData.Read(FTileID, SizeOf(Word));
    AData.Read(FZ, SizeOf(ShortInt));
  end;

  FIsGhost := False;

  InitOriginalState;
end;

constructor TMapCell.Create(AOwner: TWorldBlock; AData: TStream);
begin
  Create(AOwner, AData, 0, 0);
end;

function TMapCell.GetTileID: Word;
begin
  if FIsGhost then
    Result := FGhostID
  else
    Result := FTileID;
end;

function TMapCell.GetZ: ShortInt;
begin
  if FIsGhost then
    Result := FGhostZ
  else
    Result := FZ;
end;

function TMapCell.Clone: TMapCell;
begin
  Result := TMapCell.Create(nil, nil);
  Result.FX := FX;
  Result.FY := FY;
  Result.FZ := FZ;
  Result.FTileID := FTileID;
end;

procedure TMapCell.Write(AData: TStream);
begin
  AData.Write(FTileID, SizeOf(Word));
  AData.Write(FZ, SizeOf(ShortInt));
end;

function TMapCell.GetSize: Integer;
begin
  Result := MapCellSize;
end;

{ TMapBlock }

constructor TMapBlock.Create(AData: TStream; AX, AY: Word);
var
  iX, iY: Integer;
  buffer: TMemoryStream;
begin
  inherited Create;
  FX := AX;
  FY := AY;
  try
    buffer := nil;
    if Assigned(AData) then
    begin
      buffer := TMemoryStream.Create;
      buffer.CopyFrom(AData, 196);
      buffer.Position := 0;
      buffer.Read(FHeader, SizeOf(LongInt));
    end;
    for iY := 0 to 7 do
      for iX := 0 to 7 do
        Cells[iY * 8 + iX] := TMapCell.Create(Self, buffer, AX * 8 + iX, AY * 8 + iY);
  finally
    if Assigned(buffer) then FreeAndNil(buffer);
  end;
end;

constructor TMapBlock.Create(AData: TStream);
begin
  Create(AData, 0, 0);
end;

destructor TMapBlock.Destroy;
var
  i: Integer;
begin
  for i := 0 to 63 do
    Cells[i].Free;
  inherited;
end;

function TMapBlock.Clone: TMapBlock;
var
  i: Integer;
begin
  Result := TMapBlock.Create(nil);
  Result.FX := FX;
  Result.FY := FY;
  for i := 0 to 63 do
    Result.Cells[i] := Cells[i].Clone;
end;

procedure TMapBlock.Write(AData: TStream);
var
  i: Integer;
begin
  AData.Write(FHeader, SizeOf(LongInt));
  for i := 0 to 63 do
    Cells[i].Write(AData);
end;

function TMapBlock.GetSize: Integer;
begin
  Result := MapBlockSize;
end;

end.

