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
 *      Portions Copyright 2012 Andreas Schneider
 *)
unit UTileDataProvider;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, UMulProvider, UMulBlock, UTiledata;

type
  TLandTileDataArray = array[$0..$3FFF] of TLandTileData;
  TStaticTileDataArray = array of TStaticTileData;

  { TTiledataProvider }

  TTiledataProvider = class(TMulProvider)
    constructor Create(AData: TStream; AReadOnly: Boolean = False); overload; override;
    constructor Create(AData: string; AReadOnly: Boolean = False); overload; override;
    destructor Destroy; override;
  protected
    FLandTiles: TLandTileDataArray;
    FStaticTiles: TStaticTileDataArray;
    FStaticCount: Cardinal;
    procedure InitArray;
    function CalculateOffset(AID: Integer): Integer; override;
    function GetData(AID, AOffset: Integer): TMulBlock; override;
    procedure SetData(AID, AOffset: Integer; ABlock: TMulBlock); override;
    function GetTileData(AID: Integer): TTiledata;
  public
    function GetBlock(AID: Integer): TMulBlock; override;
    property LandTiles: TLandTileDataArray read FLandTiles;
    property StaticTiles: TStaticTileDataArray read FStaticTiles;
    property TileData[AID: Integer]: TTiledata read GetTileData; //all tiles, no cloning
    property StaticCount: Cardinal read FStaticCount;
  end;

implementation

uses
  Logging;

{ TTiledataProvider }

function TTiledataProvider.CalculateOffset(AID: Integer): Integer;
begin
  Result := GetTileDataOffset(AID);
end;

constructor TTiledataProvider.Create(AData: TStream; AReadOnly: Boolean = False);
begin
  inherited;
  InitArray;
end;

constructor TTiledataProvider.Create(AData: string; AReadOnly: Boolean = False);
begin
  inherited;
  InitArray;
end;

destructor TTiledataProvider.Destroy;
var
  i: Integer;
begin
  for i := $0 to $3FFF do
    FreeAndNil(FLandTiles[i]);
  for i := 0 to FStaticCount - 1 do
    FreeAndNil(FStaticTiles[i]);

  inherited Destroy;
end;

function TTiledataProvider.GetBlock(AID: Integer): TMulBlock;
begin
  Result := GetData(AID, 0);
end;

function TTiledataProvider.GetData(AID, AOffset: Integer): TMulBlock;
begin
  if AID < $4000 then
    Result := TMulBlock(FLandTiles[AID].Clone)
  else
    Result := TMulBlock(FStaticTiles[AID - $4000].Clone);
  Result.ID := AID;
  Result.OnChanged := @OnChanged;
  Result.OnFinished := @OnFinished;
end;

procedure TTiledataProvider.InitArray;
var
  i: Integer;
  version: TTileDataVersion;
begin
  //Guess the version by looking at the filesize.
  if FData.Size >= 3188736 then
    version := tdvHighSeas
  else
    version := tdvLegacy;

  FData.Position := 0;
  Logger.Send([lcInfo], 'Loading $4000 LandTiledata Entries');
  for i := $0 to $3FFF do
  begin
    //In High Seas, the first header comes AFTER the unknown tile (for whatever
    //reason). Therefore special handling is required.
    if ((version = tdvLegacy) and (i mod 32 = 0)) or
      ((version >= tdvHighSeas) and ((i = 1) or ((i > 1) and (i mod 32 = 0)))) then
      FData.Seek(4, soFromCurrent);
    FLandTiles[i] := TLandTileData.Create(FData, version);
  end;

  FStaticCount := ((FData.Size - FData.Position) div StaticTileGroupSize) * 32;
  Logger.Send([lcInfo], 'Loading $%x StaticTiledata Entries', [FStaticCount]);
  SetLength(FStaticTiles, FStaticCount);

  for i := 0 to FStaticCount - 1 do
  begin
    if i mod 32 = 0 then
      FData.Seek(4, soFromCurrent);
    FStaticTiles[i] := TStaticTileData.Create(FData, version);
  end;
end;

procedure TTiledataProvider.SetData(AID, AOffset: Integer;
  ABlock: TMulBlock);
begin
  if AID >= $4000 + FStaticCount then
    Exit;

  if AID < $4000 then
  begin
    FreeAndNil(FLandTiles[AID]);
    FLandTiles[AID] := TLandTileData(ABlock.Clone);
  end else
  begin
    FreeAndNil(FStaticTiles[AID - $4000]);
    FStaticTiles[AID - $4000] := TStaticTileData(ABlock.Clone);
  end;

  if not FReadOnly then
  begin
    FData.Position := AOffset;
    ABlock.Write(FData);
  end;
end;

function TTiledataProvider.GetTileData(AID: Integer): TTiledata;
begin
  if AID < $4000 then
    Result := FLandTiles[AID]
  else
    Result := FStaticTiles[AID - $4000];
end;

end.

