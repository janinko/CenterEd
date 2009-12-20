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
unit ULightManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Imaging, ImagingTypes, ImagingClasses, ImagingCanvases,
  ImagingOpenGL, GL, fgl, ULandscape, UWorldItem;

type

  TCalculateOffset = procedure(ARelativeX, ARelativeY: Integer; out DrawX,
    DrawY: Integer) of object;

  { TLightSource }

  TLightSource = class
    constructor Create(AWorldItem: TWorldItem);
  protected
    FX: Integer;
    FY: Integer;
    FZ: smallint;
  public
    property X: Integer read FX;
    property Y: Integer read FY;
    property Z: smallint read FZ;
  end;

  TLightSources = specialize TFPGObjectList<TLightSource>;

  { TLightManager }

  TLightManager = class
    constructor Create(ACalculateOffset: TCalculateOffset);
    destructor Destroy; override;
  protected
    FLightSources: TLightSources;
    FOverlay: TSingleImage;
    FOverlayTexture: GLuint;
    FLightLevel: byte;
    FValid: Boolean;
    FCalculateOffset: TCalculateOffset;
    procedure UpdateOverlay(AScreenRect: TRect; FX, FY: Integer);
  public
    procedure UpdateLightMap(ALeft, AWidth, ATop, AHeight: Integer;
      AScreenBuffer: TScreenBuffer);
    procedure Draw(AScreenRect: TRect; FX, FY: Integer);
  end;

implementation

uses
  UGameResources, UTiledata, UStatics, Logging;

{ TLightManager }

constructor TLightManager.Create(ACalculateOffset: TCalculateOffset);
begin
  FCalculateOffset := ACalculateOffset;
  FLightSources := TLightSources.Create(True);
  FLightLevel := 15; //TODO : 0 ...
end;

destructor TLightManager.Destroy;
begin
  FreeAndNil(FLightSources);
  FreeAndNil(FOverlay);
  glDeleteTextures(1, @FOverlayTexture);
  inherited Destroy;
end;

procedure TLightManager.UpdateOverlay(AScreenRect: TRect; FX, FY: Integer);
var
  canvas: TFastARGB32Canvas;
  color: TColor32Rec;
  i, drawX, drawY, drawZ: Integer;
begin
  FOverlay.Free;
  glDeleteTextures(1, @FOverlayTexture);

  color.A := $FF;
  color.R := ((32 - FLightLevel) * 255)  div 32;
  color.G := color.R;
  color.B := color.R;

  FOverlay := TSingleImage.CreateFromParams(AScreenRect.Right, AScreenRect.Bottom,
    ifA8R8G8B8);
  canvas := TFastARGB32Canvas.CreateForImage(FOverlay);
  try
    canvas.FillColor32 := color.Color;
    canvas.FillRect(AScreenRect);
  finally
    canvas.Free;
  end;

  for i := 0 to FLightSources.Count - 1 do
  begin
    FCalculateOffset(FLightSources[i].X - FX, FLightSources[i].Y - FY,
      drawX, drawY);
    drawZ := FLightSources[i].Z * 4;
    color.A := $20;
    color.R := 220;
    color.G := 0;
    color.B := 0;
    canvas.FillColor32 := color.Color;
    canvas.FillRectBlend(Rect(drawX - 22, drawY - drawZ, drawX + 22,
      drawY + 44 - drawZ), bfOne, bfOne);
  end;

  //TODO : PowerOfTwo!!!
  FOverlayTexture := CreateGLTextureFromImage(FOverlay.ImageDataPointer^);

  FValid := True;
end;

procedure TLightManager.UpdateLightMap(ALeft, AWidth, ATop, AHeight: Integer;
  AScreenBuffer: TScreenBuffer);
var
  blockInfo: PBlockInfo;
  itemMap, lightMap: array of array of TWorldItem;
  x, y: Integer;
begin
  //Logger.EnterMethod([lcClient, lcDebug], 'UpdateLightMap');
  FLightSources.Clear;
  {Logger.Send([lcClient, lcDebug], 'AWidth', AWidth);
  Logger.Send([lcClient, lcDebug], 'AHeight', AHeight);}
  SetLength(itemMap, AWidth, AHeight);
  SetLength(lightMap, AWidth, AHeight);
  for x := 0 to AWidth - 1 do
    for y := 0 to AHeight - 1 do
    begin
      itemMap[x, y] := nil;
      lightMap[x, y] := nil;
    end;

  blockInfo := nil;
  while AScreenBuffer.Iterate(blockInfo) do
  begin
    if blockInfo^.State = ssNormal then
    begin
      x := blockInfo^.Item.X - ALeft;
      y := blockInfo^.Item.Y - ATop;
      itemMap[x, y] := blockInfo^.Item;
      if (blockInfo^.Item is TStaticItem) and (tdfLightSource in
        ResMan.Tiledata.StaticTiles[blockInfo^.Item.TileID].Flags) then
        lightMap[x, y] := blockInfo^.Item;
    end;
  end;

  for x := 0 to AWidth - 2 do
    for y := 0 to AHeight - 2 do
      if lightMap[x, y] <> nil then
      begin
        if ((itemMap[x, y] = nil) or (itemMap[x, y].Z < lightMap[x, y].Z + 3)) or
           ((itemMap[x + 1, y] = nil) or (itemMap[x + 1, y].Z < lightMap[x, y].Z + 3)) or
           ((itemMap[x + 1, y + 1] = nil) or (itemMap[x + 1, y + 1].Z < lightMap[x, y].Z + 3)) or
           ((itemMap[x, y + 1] = nil) or (itemMap[x, y + 1].Z < lightMap[x, y].Z + 3)) then
        begin
          FLightSources.Add(TLightSource.Create(lightMap[x, y]));
        end;
      end;
  FValid := False;
  //Logger.ExitMethod([lcClient, lcDebug], 'UpdateLightMap');
end;

procedure TLightManager.Draw(AScreenRect: TRect; FX, FY: Integer);
begin
  if not FValid then
    UpdateOverlay(AScreenRect, FX, FY);

  glBindTexture(GL_TEXTURE_2D, FOverlayTexture);
  glBlendFunc(GL_ZERO, GL_SRC_COLOR);
  glBegin(GL_QUADS);
    glTexCoord2i(0, 0);
    glVertex2i(AScreenRect.Left, AScreenRect.Top);
    glTexCoord2i(0, 1);
    glVertex2i(AScreenRect.Left, AScreenRect.Bottom);
    glTexCoord2i(1, 1);
    glVertex2i(AScreenRect.Right, AScreenRect.Bottom);
    glTexCoord2i(1, 0);
    glVertex2i(AScreenRect.Right, AScreenRect.Top);
  glEnd;
end;

{ TLightSource }

constructor TLightSource.Create(AWorldItem: TWorldItem);
begin
  FX := AWorldItem.X;
  FY := AWorldItem.Y;
  FZ := AWorldItem.Z;
end;

end.

