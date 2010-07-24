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
unit UGump;

{$mode objfpc}{$H+}

interface

uses
  Classes, Imaging, ImagingTypes, ImagingClasses, UMulBlock, UGenericIndex;

type
  TGumpIndex = class(TGenericIndex)
  protected
    function GetWidth: SmallInt;
    function GetHeight: SmallInt;
    procedure SetWidth(AValue: SmallInt);
    procedure SetHeight(AValue: SmallInt);
  published
    property Width: SmallInt read GetWidth write SetWidth;
    property Height: SmallInt read GetHeight write SetHeight;
  end;
  TGump = class(TMulBlock)
    constructor Create(AData: TStream; AIndex: TGumpIndex); overload;
    constructor Create(AWidth, AHeight: Integer); overload;
    destructor Destroy; override;
    function Clone: TGump; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
    procedure RefreshBuffer;
  protected
    FGraphic: TSingleImage;
    FBuffer: TStream;
  published
    property Graphic: TSingleImage read FGraphic;
  end;

implementation

type
  PWordArray = ^TWordArray;
  TWordArray = array[0..16383] of Word;

{ TGumpIndex }

function TGumpIndex.GetHeight: SmallInt;
var
  sizeInfo: LongInt;
  sizeInfoW: array[0..1] of SmallInt absolute sizeInfo;
begin
  sizeInfo := FVarious;
  Result := sizeInfoW[0];
end;

function TGumpIndex.GetWidth: SmallInt;
var
  sizeInfo: LongInt;
  sizeInfoW: array[0..1] of SmallInt absolute sizeInfo;
begin
  sizeInfo := FVarious;
  Result := sizeInfoW[1];
end;

procedure TGumpIndex.SetHeight(AValue: SmallInt);
var
  sizeInfo: LongInt;
  sizeInfoW: array[0..1] of SmallInt absolute sizeInfo;
begin
  sizeInfo := FVarious;
  sizeInfoW[0] := AValue;
  FVarious := sizeInfo;
end;

procedure TGumpIndex.SetWidth(AValue: SmallInt);
var
  sizeInfo: LongInt;
  sizeInfoW: array[0..1] of SmallInt absolute sizeInfo;
begin
  sizeInfo := FVarious;
  sizeInfoW[1] := AValue;
  FVarious := sizeInfo;
end;

{ TGump }

constructor TGump.Create(AData: TStream; AIndex: TGumpIndex);
var
  iCurrentHeight, iCurrentWidth, i: Integer;
  RowLookup: array of integer;
  Offset: Integer;
  Value, Run: Word;
  block: TMemoryStream;
begin
  inherited Create;
  FGraphic := TSingleImage.CreateFromParams(AIndex.Width, AIndex.Height, ifA1R5G5B5);
  FBuffer := TMemoryStream.Create;
  SetLength(RowLookup, AIndex.Height);
  if assigned(AData) then
  begin
    AData.Position := AIndex.Lookup;
    block := TMemoryStream.Create;
    block.CopyFrom(AData, AIndex.Size);
    block.Position := 0;
    for i := 0 to AIndex.Height - 1 do
    begin
      block.Read(Offset, SizeOf(Integer));
      RowLookup[i] := Offset * 4;
    end;
    for iCurrentHeight := 0 to AIndex.Height - 1 do
    begin
      block.Position := RowLookup[iCurrentHeight];
      iCurrentWidth := 0;
      while iCurrentWidth < AIndex.Width do
      begin
        block.Read(Value, SizeOf(Word));
        block.Read(Run, SizeOf(Word));
        if Value > 0 then Value := Value or $8000; //Set alpha bit of non-black colors
        for i := 0 to Run - 1 do
          PWordArray(FGraphic.Bits + iCurrentHeight * AIndex.Width * 2)^[iCurrentWidth + i] := Value;
        inc(iCurrentWidth, Run);
      end;
    end;
    block.Free;
  end;
  FGraphic.Format := ifA8R8G8B8;
end;

constructor TGump.Create(AWidth, AHeight: Integer);
begin
  {TODO : WARNING! Width and Height got switched since MulEditor!}
  inherited Create;
  FGraphic := TSingleImage.CreateFromParams(AWidth, AHeight, ifA8R8G8B8);
  FBuffer := TMemoryStream.Create;
end;

destructor TGump.Destroy;
begin
  if assigned(FGraphic) then FGraphic.Free;
  if assigned(FBuffer) then FBuffer.Free;
  inherited Destroy;
end;

function TGump.Clone: TGump;
begin
  Result := TGump.Create(FGraphic.Width, FGraphic.Height);
  Result.FGraphic.Assign(FGraphic);
end;

procedure TGump.Write(AData: TStream);
begin
  FBuffer.Position := 0;
  AData.CopyFrom(FBuffer, FBuffer.Size);
end;

function TGump.GetSize: Integer;
begin
  RefreshBuffer;
  Result := FBuffer.Size;
end;

procedure TGump.RefreshBuffer;
var
  argbGraphic: TSingleImage;
  colorBuffer: PWordArray;
  runBuffer: array of Word;
  offsetBuffer: array of Integer;
  currentColor, currentRun: Integer;
  iCurrentHeight, i: Integer;
begin
  argbGraphic := TSingleImage.CreateFromImage(FGraphic);
  argbGraphic.Format := ifA1R5G5B5;
  SetLength(runBuffer, argbGraphic.Width);
  SetLength(offsetBuffer, argbGraphic.Height);
  FBuffer.Size := argbGraphic.Height * SizeOf(Integer);
  FBuffer.Position := FBuffer.Size;
  for iCurrentHeight := 0 to argbGraphic.Height - 1 do
  begin
    colorBuffer := argbGraphic.Bits + iCurrentHeight * argbGraphic.Width * 2;
    for i := 0 to argbGraphic.Width - 1 do
    begin
      runBuffer[i] := 1;
      colorBuffer^[i] := colorBuffer^[i] and not $8000; //eleminate alpha bit
    end;
    currentRun := 0;
    currentColor := colorBuffer^[0];
    for i := 1 to argbGraphic.Width - 1 do
    begin
      if colorBuffer^[i] = currentColor then
        Inc(runBuffer[currentRun])
      else
        Inc(currentRun);
      currentColor := colorBuffer^[i];
    end;

    offsetBuffer[iCurrentHeight] := FBuffer.Position div 4;
    currentColor := 0;
    for i := 0 to currentRun do
    begin
      FBuffer.Write(colorBuffer^[currentColor], SizeOf(Word));
      FBuffer.Write(runBuffer[i], SizeOf(Word));
      Inc(currentColor, runBuffer[i]);
    end;
  end;
  FBuffer.Position := 0;
  for i := 0 to argbGraphic.Height - 1 do FBuffer.Write(offsetBuffer[i], SizeOf(Integer));
  argbGraphic.Free;
end;

end.

