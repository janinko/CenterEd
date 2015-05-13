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
 *      Portions Copyright 2015 Andreas Schneider
 *)
unit USelectionHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSelectedRangeCallback = procedure(AX1, AY1, AX2, AY2: Word) of object;

procedure SelectRange(ACallback: TSelectedRangeCallback);

implementation

uses
  UfrmMain, UWorldItem, math;

type

  { TRangeSelectionHelper }

  TRangeSelectionHelper = class
    constructor Create(ACallback: TSelectedRangeCallback);
  protected
    FCallback: TSelectedRangeCallback;
    FItem1: TWorldItem;
    FItem2: TWorldItem;
    procedure TileSelected(AWorldItem: TWorldItem);
    procedure Finish;
  public
    procedure Run;
  end;

procedure SelectRange(ACallback: TSelectedRangeCallback);
var
  helper: TRangeSelectionHelper;
begin
  helper := TRangeSelectionHelper.Create(ACallback);
  helper.Run;
  //Cleanup will follow asynchroneously
end;

{ TRangeSelectionHelper }

constructor TRangeSelectionHelper.Create(ACallback: TSelectedRangeCallback);
begin
  FCallback := ACallback;
  FItem1 := nil;
  FItem2 := nil;
end;

procedure TRangeSelectionHelper.TileSelected(AWorldItem: TWorldItem);
begin
  if FItem1 = nil then
    FItem1 := AWorldItem
  else if FItem2 = nil then
  begin
    FItem2 := AWorldItem;
    Finish;
  end;
end;

procedure TRangeSelectionHelper.Finish;
var
  minX, minY: Word;
  maxX, maxY: Word;
begin
  frmMain.UnregisterSelectionListener(@TileSelected);
  minX := Min(FItem1.X, FItem2.X);
  minY := Min(FItem1.Y, FItem2.Y);
  maxX := Max(FItem1.X, FItem2.X);
  maxY := Max(FItem1.Y, FItem2.Y);
  FCallback(minX, minY, maxX, maxY);
  Free; //We use this class only once, so it can cleanup after itself
end;

procedure TRangeSelectionHelper.Run;
begin
  //TODO show indicator and option to cancel
  //TODO keep track of instance (global variable maybe?)
  frmMain.RegisterSelectionListener(@TileSelected);
  frmMain.SwitchToSelection;
end;

end.

