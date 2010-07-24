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
unit UMultiMap;

interface

uses
  Classes, Graphics, UProgress;

type
  TMultiMap = class(TObject)
    constructor Create(Data: TStream; OnProgress: TOnProgressEvent = nil); overload;
    constructor Create(Height, Width: Integer; OnProgress: TOnProgressEvent = nil); overload;
    destructor Destroy; override;
    procedure Write(Data: TStream);
  protected
    FGraphic: TBitmap;
    FOnProgress: TOnProgressEvent;
  public
    property Graphic: TBitmap read FGraphic;
    property OnProgress: TOnProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TMultiMap }

constructor TMultiMap.Create(Data: TStream; OnProgress: TOnProgressEvent = nil);
var
  height, width: Integer;
  x, y, run: Integer;
  pixelData: Byte;
  color: TColor;
begin
  if Assigned(Data) then
  begin
    Data.Read(width, SizeOf(Integer));
    Data.Read(height, SizeOf(Integer));
    Create(height, width, OnProgress);
    if Assigned(FGraphic) then
    begin
      if Assigned(FOnProgress) then FOnProgress(height, 0);
      x := 0;
      y := 0;
      while y < height do
      begin
        while (x < width) and (y < height) do
        begin
          Data.Read(pixelData, SizeOf(Byte));
          if (pixelData and $80) = $80 then color := clBlack else color := clWhite;
          for run := 1 to (pixelData and $7F) do
          begin
            FGraphic.Canvas.Pixels[x,y] := color;
            Inc(x);
            if x = width then
            begin
              x := 0;
              inc(y);
              if Assigned(FOnProgress) then FOnProgress(height, y);
              if y = height then Break;
            end;
          end; //for
        end; //while x & y
        Inc(y);
        if Assigned(FOnProgress) then FOnProgress(height, y);
      end; //while y
      if Assigned(FOnProgress) then FOnProgress(0, 0);
    end; //if assigned
  end else
    Create(0, 0, OnProgress);
end;

constructor TMultiMap.Create(Height, Width: Integer; OnProgress: TOnProgressEvent = nil);
begin
  FGraphic := TBitmap.Create;
  FGraphic.Height := Height;
  FGraphic.Width := Width;
  FGraphic.PixelFormat := pf1bit;
  FGraphic.HandleType := bmDIB;
  FOnProgress := OnProgress;
end;

destructor TMultiMap.Destroy;
begin
  if Assigned(FGraphic) then FGraphic.Free;
  inherited;
end;

procedure TMultiMap.Write(Data: TStream);
var
  height, width, x, y: Integer;
  run: Byte;
  state, newState: Boolean;

  procedure DoWrite;
  var
    pixelData: Byte;
  begin
    pixelData := run;
    if state then pixelData := pixelData or $80;
    Data.Write(pixelData, SizeOf(Byte));
  end;

begin
  height := FGraphic.Height;
  width := FGraphic.Width;
  Data.Write(width, SizeOf(Integer));
  Data.Write(height, SizeOf(Integer));
  run := 0;
  state := not (FGraphic.Canvas.Pixels[0,0] = clWhite);
  if Assigned(FOnProgress) then FOnProgress(0, 0);
  for y := 0 to height - 1 do
  begin
    for x := 0 to width - 1 do
    begin
      newState := not (FGraphic.Canvas.Pixels[x,y] = clWhite);
      if (state = newState) and (run < $7F) then
      begin
        inc(run);
      end else
      begin
        DoWrite;
        state := newState;
        run := 1;
      end;
    end;
    if Assigned(FOnProgress) then FOnProgress(height, y);
  end;
  if run > 0 then DoWrite;
  if Assigned(FOnProgress) then FOnProgress(0, 0);
end;

end.

