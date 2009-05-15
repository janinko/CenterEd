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
unit UArt;

{$mode objfpc}{$H+}

interface

uses
  Classes, Imaging, ImagingTypes, ImagingCanvases, ImagingClasses,
  UMulBlock, UGenericIndex, UHue;

type
  TArtType = (atLand, atStatic, atLandFlat);
  TArt = class(TMulBlock)
    constructor Create(AData: TStream; AIndex: TGenericIndex; AArtType: TArtType); overload;
    constructor Create(AData: TStream; AIndex: TGenericIndex; AArtType: TArtType; AHue: THue; APartialHue: Boolean); overload;
    constructor Create(AData: TStream; AIndex: TGenericIndex; AArtType: TArtType; AArtColor: Word; AHue: THue; APartialHue: Boolean); overload;
    destructor Destroy; override;
    function Clone: TArt; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
    procedure RefreshBuffer;
  protected
    FArtType: TArtType;
    FHeader: LongInt;
    FGraphic: TSingleImage;
    FBuffer: TStream;
  public
    property ArtType: TArtType read FArtType write FArtType;
    property Header: LongInt read FHeader write FHeader;
    property Graphic: TSingleImage read FGraphic;
    property Buffer: TStream read FBuffer;
  end;

implementation

type
  PWordArray = ^TWordArray;
  TWordArray = array[0..16383] of Word;

constructor TArt.Create(AData: TStream; AIndex: TGenericIndex; AArtType: TArtType);
begin
  Create(AData, AIndex, AArtType, 0, nil, False);
end;

constructor TArt.Create(AData: TStream; AIndex: TGenericIndex; AArtType: TArtType; AHue: THue; APartialHue: Boolean);
begin
  Create(AData, AIndex, AArtType, 0, AHue, APartialHue);
end;

constructor TArt.Create(AData: TStream; AIndex: TGenericIndex; AArtType: TArtType; AArtColor: Word; AHue: THue; APartialHue: Boolean);
var
  i, x, y, start: Integer;
  iCurrentHeight, iCurrentWidth: Integer;
  width, height: SmallInt;
  lookup: array of integer;
  color, run, offset: Word;
  block: TMemoryStream;
  P: PWordArray;
  r, g, b: Byte;

begin
  FBuffer := TMemoryStream.Create;
  FArtType := AArtType;
  AArtColor := AArtColor or $8000; //set alpha bit on background
  if Assigned(AData) and (AIndex.Lookup > -1) then
  begin
    AData.Position := AIndex.Lookup;
    block := TMemoryStream.Create;
    block.CopyFrom(AData, AIndex.Size);
    block.Position := 0;

    if AArtType = atLand then
    begin
      FGraphic:= TSingleImage.CreateFromParams(44, 44, ifA1R5G5B5);
      FillWord(FGraphic.Bits^, 44 * 44, AArtColor);
      for y := 0 to 21 do
      begin
        P := FGraphic.Bits + y * 44 * 2;
        block.Read(P^[22 - (y + 1)], (y + 1) * 4);
      end;
      for y := 0 to 21 do
      begin
        P := FGraphic.Bits + (22 + y) * 44 * 2;
        block.Read(P^[y], (22 - y) * 4);
      end;
      for i := 0 to 44 * 44 - 1 do
        PWordArray(FGraphic.Bits)^[i] := PWordArray(FGraphic.Bits)^[i] xor $8000; //invert alpha bit
    end else if AArtType = atLandFlat then
    begin
      FGraphic:= TSingleImage.CreateFromParams(44, 44, ifA1R5G5B5);
      for i := 1 to 22 do
      begin
        for x := 0 to i * 2 - 1 do
        begin
          y := i * 2 - x - 1;
          block.Read(color, SizeOf(Word));
          PWordArray(FGraphic.Bits + y * 44 * 2)^[x] := color;
          if y > 0 then
            PWordArray(FGraphic.Bits + (y - 1) * 44 * 2)^[x] := color;
        end;
      end;
      for i := 22 to 43 do
      begin
        for y := 0 to (44 - i) * 2 - 1 do
        begin
          x := 42 - (43 - i) * 2 + y;
          block.Read(color, SizeOf(Word));
          PWordArray(FGraphic.Bits + (43 - y) * 44 * 2)^[x] := color;
          if y > 0 then
            PWordArray(FGraphic.Bits + (44 - y) * 44 * 2)^[x] := color;
        end;
      end;
      for i := 0 to 44 * 44 - 1 do
        PWordArray(FGraphic.Bits)^[i] := PWordArray(FGraphic.Bits)^[i] xor $8000; //invert alpha bit
    end else if AArtType = atStatic then
    begin
      block.Read(FHeader, SizeOf(LongInt));
      block.Read(width, SizeOf(SmallInt));
      block.Read(height, SizeOf(SmallInt));
      FGraphic:= TSingleImage.CreateFromParams(width, height, ifA1R5G5B5);
      FillWord(FGraphic.Bits^, width * height, AArtColor);
      SetLength(lookup, height);
      start := block.Position + (height * 2);
      for i := 0 to height - 1 do
      begin
        block.Read(offset, SizeOf(Word));
        lookup[i] := start + (offset * 2);
      end;
      for iCurrentHeight := 0 to height - 1 do
      begin
        block.Position := lookup[iCurrentHeight];
        iCurrentWidth := 0;
        P := FGraphic.Bits + iCurrentHeight * width * 2;
        while (block.Read(offset, SizeOf(Word)) = SizeOf(Word)) and (block.Read(run, SizeOf(Word)) = SizeOf(Word)) and (offset + run <> 0) do
        begin
          inc(iCurrentWidth, offset);
          for i := 0 to run - 1 do
          begin
            block.Read(color, SizeOf(Word));
            P^[iCurrentWidth + i] := color;
          end;
          inc(iCurrentWidth, run);
        end;
      end;
      
      if AHue <> nil then
      begin
        for i := 0 to width * height - 1 do
        begin
          color := PWordArray(FGraphic.Bits)^[i];
          if color <> AArtColor then
          begin
            r := (color and $7C00) shr 10;
            if APartialHue then
            begin
              g := (color and $3E0) shr 5;
              b := color and $1F;
              if (r = g) and (g = b) then
                color := AHue.ColorTable[r];
            end else
              color := AHue.ColorTable[r];
          end;
          PWordArray(FGraphic.Bits)^[i] := color;
        end;
      end;
      
      for i := 0 to width * height - 1 do
        PWordArray(FGraphic.Bits)^[i] := PWordArray(FGraphic.Bits)^[i] xor $8000; //invert alpha bit
    end else
      FGraphic:= TSingleImage.Create;
    if Assigned(block) then block.Free;
  end else
  begin
    FHeader := 1;
    FGraphic := TSingleImage.Create;
  end;
  FGraphic.Format := ifA8R8G8B8;
end;

destructor TArt.Destroy;
begin
  if assigned(FGraphic) then FGraphic.Free;
  if assigned(FBuffer) then FBuffer.Free;
  inherited;
end;

function TArt.Clone: TArt;
begin
  Result := TArt.Create(nil, nil, FArtType);
  Result.FHeader := FHeader;
  Result.FGraphic.Assign(FGraphic);
end;

procedure TArt.Write(AData: TStream);
begin
  FBuffer.Position := 0;
  AData.CopyFrom(FBuffer, FBuffer.Size);
end;

function TArt.GetSize: Integer;
begin
  RefreshBuffer;
  Result := FBuffer.Size
end;

procedure TArt.RefreshBuffer;
var
  argbGraphic: TSingleImage;
  i, j, x, y, lineWidth, start: Integer;
  iCurrentHeight, iCurrentWidth: Integer;
  width, height: SmallInt;
  color, run, offset: Word;
  lookup: array of SmallInt;
begin
  argbGraphic := TSingleImage.CreateFromImage(FGraphic);
  argbGraphic.Format := ifA1R5G5B5;
  for i := 0 to argbGraphic.Width * argbGraphic.Height - 1 do
    PWordArray(argbGraphic.Bits)^[i] := PWordArray(argbGraphic.Bits)^[i] xor $8000; //invert alpha bit
  FBuffer.Size := 0;
  if FArtType = atLand then
  begin
    if (argbGraphic.Height <> 44) or (argbGraphic.Width <> 44) then Exit;
    x := 21;
    y := 0;
    lineWidth := 2;
    for i := 1 to 22 do
    begin
      Dec(x);
      FBuffer.Write(PWordArray(argbGraphic.Bits + y * 44 * 2)^[x + j], lineWidth);
      Inc(y);
      Inc(lineWidth, 2);
    end;
    for i := 1 to 22 do
    begin
      Dec(lineWidth, 2);
      FBuffer.Write(PWordArray(argbGraphic.Bits + y * 44 * 2)^[x + j], lineWidth);
      Inc(x);
      Inc(y);
    end;
  end else if FArtType = atStatic then
  begin
    if (argbGraphic.Height = 0) or (argbGraphic.Width = 0) then Exit;
    width := argbGraphic.Width;
    height := argbGraphic.Height;
    FBuffer.Write(FHeader, SizeOf(LongInt));
    FBuffer.Write(width, SizeOf(SmallInt));
    FBuffer.Write(height, SizeOf(SmallInt));
    SetLength(lookup, height);
    for i := 0 to height - 1 do
      FBuffer.Write(lookup[i], SizeOf(SmallInt)); //placeholders for the lookup table
    start := FBuffer.Position;
    for iCurrentHeight := 0 to height - 1 do
    begin
      lookup[iCurrentHeight] := SmallInt((FBuffer.Position - start) div 2); //remember the lookup offset for the current line
      offset := 0;
      run := 0;
      for iCurrentWidth := 0 to width - 1 do //process every pixel on the current line
      begin
        color := PWordArray(FGraphic.Bits + iCurrentHeight * width * 2)^[iCurrentWidth];
        if (color and $8000 = 0) and (run = 0) then //new visible pixel found
        begin
          FBuffer.Write(offset, SizeOf(Word));
          FBuffer.Write(offset, SizeOf(Word)); //just a placeholder for the "run length"
          run := 1;
          FBuffer.Write(color, SizeOf(Word));
        end else if (color and $8000 = 0) and (run > 0) then //another visible pixel found
        begin
          inc(run);
          FBuffer.Write(color, SizeOf(Word));
        end else if (color and $8000 = $8000) and (run > 0) then //after some visible pixels this one is invisible, so stop the current run
        begin
          FBuffer.Seek(Integer(-((run + 1) * 2)), soFromCurrent); //jump back ...
          FBuffer.Write(run, SizeOf(Word));                       //... to write the actual "run length" ...
          FBuffer.Seek(Integer(run * 2), soFromCurrent);          //... and jump forth again to proceed
          run := 0;
          offset := 1;
        end else
          inc(offset);
      end;
      if run > 0 then //no more pixels but the "run" didn't end yet ;-)
      begin
        FBuffer.Seek(Integer(-((run + 1) * 2)), soFromCurrent);
        FBuffer.Write(run, SizeOf(Word));
        FBuffer.Seek(Integer(run * 2), soFromCurrent);
        run := 0;
      end;
      FBuffer.Write(run, SizeOf(Word)); //just write "0"
      FBuffer.Write(run, SizeOf(Word)); //... two times, to indicate the end of that line
    end;
    FBuffer.Position := start - (height * 2); //now update the lookup table with our new values
    for i := 0 to height - 1 do
      FBuffer.Write(lookup[i], SizeOf(SmallInt));
  end;
  argbGraphic.Free;
end;

end.

