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
unit UWorldItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, UMulBlock;

type
  TWorldBlock = class;

  { TWorldItem }

  TWorldItem = class(TMulBlock)
    constructor Create(AOwner: TWorldBlock);
  protected
    FOwner, FOrgOwner: TWorldBlock;
    FTileID, FOrgTileID: Word;
    FX, FOrgX: Word;
    FY, FOrgY: Word;
    FZ, FOrgZ: ShortInt;
    FSelected: Boolean;
    FCanBeEdited: Boolean;
    FLocked: Boolean;
    FChanged: Boolean;
    FPriority: Integer;
    FPriorityBonus: ShortInt;
    FPrioritySolver: Integer;
    function GetTileID: Word; virtual;
    function GetZ: ShortInt; virtual;
    procedure SetTileID(ATileID: Word);
    procedure SetX(AX: Word);
    procedure SetY(AY: Word);
    procedure SetZ(AZ: ShortInt);
    procedure SetSelected(ASelected: Boolean);
    procedure SetOwner(AOwner: TWorldBlock);
    procedure SetLocked(ALocked: Boolean);
    procedure DoChanged;
    function HasChanged: Boolean; virtual;
  public
    procedure UpdatePos(AX, AY: Word; AZ: ShortInt);
    procedure Delete;
    procedure InitOriginalState; virtual;

    property Owner: TWorldBlock read FOwner write SetOwner;
    property TileID: Word read GetTileID write SetTileID;
    property X: Word read FX write SetX;
    property Y: Word read FY write SetY;
    property Z: ShortInt read GetZ write SetZ;
    property Selected: Boolean read FSelected write SetSelected;
    property CanBeEdited: Boolean read FCanBeEdited write FCanBeEdited;
    property Locked: Boolean read FLocked write SetLocked;
    property Changed: Boolean read FChanged;
    property Priority: Integer read FPriority write FPriority;
    property PriorityBonus: ShortInt read FPriorityBonus write FPriorityBonus;
    property PrioritySolver: Integer read FPrioritySolver write FPrioritySolver;

    property RawTileID: Word read FTileID;
    property RawZ: ShortInt read FZ;
  end;

  TWorldItemList = specialize TFPGObjectList<TWorldItem>;

  { TWorldBlock }

  TWorldBlock = class(TMulBlock)
    constructor Create;
  protected
    FX: Word;
    FY: Word;
    FRefCount: Integer;
    FChanges: Integer;
    function GetChanged: Boolean;
    procedure SetChanged(AChanged: Boolean);
    procedure DoStateChanged;
  public
    property X: Word read FX write FX;
    property Y: Word read FY write FY;
    property RefCount: Integer read FRefCount;
    property Changed: Boolean read GetChanged write SetChanged;
    procedure AddRef;
    procedure RemoveRef;
    procedure CleanUp;
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
    Result := AItem1.PriorityBonus - AItem2.PriorityBonus;

  if Result = 0 then
    Result := AItem1.PrioritySolver - AItem2.PrioritySolver;
end;

{ TWorldItem }

constructor TWorldItem.Create(AOwner: TWorldBlock);
begin
  inherited Create;
  FSelected := False;
  FLocked := False;
  FChanged := False;
  FOwner := AOwner;
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
  if (FOwner <> FOrgOwner) then
    FOwner.Changed := False
  else if Assigned(FOrgOwner) and (not FChanged) then
    FOrgOwner.Changed := True;
end;

procedure TWorldItem.DoChanged;
var
  blockChanged: Boolean;
begin
  blockChanged := HasChanged;
  if Assigned(FOwner) then
  begin
    if FChanged and (not blockChanged) then
      FOwner.Changed := False
    else if (not FChanged) and blockChanged then
      FOwner.Changed := True;
  end;
  FChanged := blockChanged;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TWorldItem.HasChanged: Boolean;
begin
  Result := (FX <> FOrgX) or (FY <> FOrgY) or (FZ <> FOrgZ) or
    (FTileID <> FOrgTileID) or (FOrgOwner <> FOwner);
end;

procedure TWorldItem.InitOriginalState;
begin
  {if Assigned(FOrgOwner) and (FOwner <> FOrgOwner) then
    FOrgOwner.Changed := False;}
  FOrgOwner := FOwner;
  FOrgTileID := FTileID;
  FOrgX := FX;
  FOrgY := FY;
  FOrgZ := FZ;
  DoChanged;
end;

procedure TWorldItem.SetLocked(ALocked: Boolean);
begin
  if FLocked <> ALocked then
  begin
    FLocked := ALocked;
    if Assigned(FOwner) then
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
    if Assigned(FOwner) then
    begin
      if FOwner <> FOrgOwner then
        FOwner.Changed := False;
      if FLocked then FOwner.RemoveRef;
      if FSelected then FOwner.RemoveRef;
    end;
    FOwner := AOwner;
    if Assigned(FOwner) then
    begin
      if FOwner <> FOrgOwner then
        FOwner.Changed := True;
      if FLocked then FOwner.AddRef;
      if FSelected then FOwner.AddRef;
    end;
    DoChanged;
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

procedure TWorldItem.SetTileID(ATileID: Word);
begin
  FTileID := ATileID;
  DoChanged;
end;

procedure TWorldItem.SetX(AX: Word);
begin
  FX := AX;
  DoChanged;
end;

procedure TWorldItem.SetY(AY: Word);
begin
  FY := AY;
  DoChanged
end;

procedure TWorldItem.SetZ(AZ: ShortInt);
begin
  FZ := AZ;
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
  DoStateChanged;
end;

procedure TWorldBlock.CleanUp;
begin
  FChanges := 0;
  DoStateChanged;
end;

constructor TWorldBlock.Create;
begin
  inherited Create;
  FRefCount := 0;
  FChanges := 0;
end;

procedure TWorldBlock.DoStateChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TWorldBlock.GetChanged: Boolean;
begin
  Result := (FChanges <> 0);
end;

procedure TWorldBlock.RemoveRef;
begin
  if FRefCount > 0 then
    Dec(FRefCount);
  DoStateChanged;
end;

procedure TWorldBlock.SetChanged(AChanged: Boolean);
begin
  if AChanged then
    Inc(FChanges)
  else
    Dec(FChanges);
  DoStateChanged;
end;

end.

