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
unit UWorldItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, heContnrs, UMulBlock;

type
  TWorldBlock = class;

  { TWorldItem }

  TWorldItem = class(TMulBlock)
    constructor Create(AOwner: TWorldBlock);
  protected
    FOwner: TWorldBlock;
    FTileID: Word;
    FX: Word;
    FY: Word;
    FZ: ShortInt;
    FSelected: Boolean;
    FCanBeEdited: Boolean;
    FLocked: Boolean;
    FPriority: Integer;
    FPriorityBonus: ShortInt;
    FPrioritySolver: Integer;
    procedure DoChanged;
    function  GetTileID: Word; virtual;
    function  GetZ: ShortInt; virtual;
    procedure SetLocked(ALocked: Boolean);
    procedure SetOwner(AOwner: TWorldBlock);
    procedure SetSelected(ASelected: Boolean);
    procedure SetTileID(AValue: Word);
    procedure SetX(AValue: Word);
    procedure SetY(AValue: Word);
    procedure SetZ(AValue: ShortInt);
  public
    procedure UpdatePos(AX, AY: Word; AZ: ShortInt);
    procedure Delete;

    property Owner: TWorldBlock read FOwner write SetOwner;
    property TileID: Word read GetTileID write SetTileID;
    property X: Word read FX write SetX;
    property Y: Word read FY write SetY;
    property Z: ShortInt read GetZ write SetZ;
    property Selected: Boolean read FSelected write SetSelected;
    property CanBeEdited: Boolean read FCanBeEdited write FCanBeEdited;
    property Locked: Boolean read FLocked write SetLocked;
    property Priority: Integer read FPriority write FPriority;
    property PriorityBonus: ShortInt read FPriorityBonus write FPriorityBonus;
    property PrioritySolver: Integer read FPrioritySolver write FPrioritySolver;

    property RawTileID: Word read FTileID;
    property RawZ: ShortInt read FZ;
  end;

  TWorldItemList = specialize TheObjectVector<TWorldItem>;

  { TWorldBlock }

  TWorldBlock = class(TMulBlock)
    constructor Create;
  protected
    FX: Word;
    FY: Word;
    FRefCount: Integer;
    FChanged: Boolean;
  public
    property X: Word read FX write FX;
    property Y: Word read FY write FY;
    property RefCount: Integer read FRefCount;
    property Changed: Boolean read FChanged write FChanged;
    procedure AddRef;
    procedure RemoveRef;
  end;

  TVirtualTile = class(TWorldItem);

function CompareWorldItems(const AItem1, AItem2: TWorldItem): Integer;

implementation

uses
  UMap, UStatics;

function CompareWorldItems(const AItem1, AItem2: TWorldItem): Integer;
begin
  if AItem1.X <> AItem2.X then
    Exit(AItem1.X - AItem2.X);

  if AItem1.Y <> AItem2.Y then
    Exit(AItem1.Y - AItem2.Y);

  Result := AItem1.Priority - AItem2.Priority;
  if Result = 0 then
  begin
    if (AItem1 is TMapCell) and (AItem2 is TStaticItem) then
      Result := -1
    else if (AItem1 is TStaticItem) and (AItem2 is TMapCell) then
      Result := 1
    else if (AItem1 is TMapCell) and (AItem2 is TVirtualTile) then
      Result := -1
    else if (AItem1 is TVirtualTile) and (AItem2 is TMapCell) then
      Result := 1;
  end;

  if Result = 0 then
    Result := AItem1.PrioritySolver - AItem2.PrioritySolver;
end;

{ TWorldItem }

constructor TWorldItem.Create(AOwner: TWorldBlock);
begin
  inherited Create;
  FSelected := False;
  FLocked := False;
  FOwner := AOwner;
end;

procedure TWorldItem.DoChanged;
begin
  if FOwner <> nil then
    FOwner.Changed := True;
end;

function TWorldItem.GetTileID: Word;
begin
  Result := FTileID;
end;

function TWorldItem.GetZ: ShortInt;
begin
  Result := FZ;
end;

procedure TWorldItem.Delete;
begin
  SetSelected(False);
  SetLocked(False);
  DoChanged;
end;

procedure TWorldItem.SetLocked(ALocked: Boolean);
begin
  if FLocked <> ALocked then
  begin
    FLocked := ALocked;
    if FOwner <> nil then
      if FLocked then
        FOwner.AddRef
      else
        FOwner.RemoveRef;
  end;
end;

procedure TWorldItem.SetOwner(AOwner: TWorldBlock);
begin
  if FOwner <> AOwner then
  begin
    if FOwner <> nil then
    begin
      FOwner.Changed := True;
      if FLocked then FOwner.RemoveRef;
      if FSelected then FOwner.RemoveRef;
    end;
    FOwner := AOwner;
    if FOwner <> nil then
    begin
      FOwner.Changed := True;
      if FLocked then FOwner.AddRef;
      if FSelected then FOwner.AddRef;
    end;
  end;
end;

procedure TWorldItem.SetSelected(ASelected: Boolean);
begin
  if (FOwner <> nil) and (ASelected <> FSelected) then
    if ASelected then
      FOwner.AddRef
    else
      FOwner.RemoveRef;
  FSelected := ASelected;
end;

procedure TWorldItem.SetTileID(AValue: Word);
begin
  if FTileID = AValue then
    Exit;

  FTileID := AValue;
  DoChanged;
end;

procedure TWorldItem.SetX(AValue: Word);
begin
  if FX = AValue then
    Exit;

  FX := AValue;
  DoChanged;
end;

procedure TWorldItem.SetY(AValue: Word);
begin
  if FY = AValue then
    Exit;

  FY := AValue;
  DoChanged;
end;

procedure TWorldItem.SetZ(AValue: ShortInt);
begin
  if FZ = AValue then
    Exit;

  FZ := AValue;
  DoChanged;
end;

procedure TWorldItem.UpdatePos(AX, AY: Word; AZ: ShortInt);
begin
  FX := AX;
  FY := AY;
  FZ := AZ;
  DoChanged;
end;

{ TWorldBlock }

procedure TWorldBlock.AddRef;
begin
  Inc(FRefCount);
end;

constructor TWorldBlock.Create;
begin
  inherited Create;
  FRefCount := 0;
  FChanged := False;
end;

procedure TWorldBlock.RemoveRef;
begin
  if FRefCount > 0 then
    Dec(FRefCount);
end;

end.

