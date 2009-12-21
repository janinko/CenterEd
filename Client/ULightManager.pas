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
  ImagingOpenGL, GL, fgl, ULandscape, UWorldItem, UCacheManager;

type

  TCalculateOffset = procedure(AX, AY: Integer; out DrawX, DrawY: Integer) of object;

  { TLightMaterial }

  TLightMaterial = class(ICacheable)
    constructor Create(AGraphic: TBaseImage);
    destructor Destroy; override;
  protected
    FRefCount: Integer;
    FGraphic: TSingleImage;
    FCanvas: TFastARGB32Canvas;
  public
    property Graphic: TSingleImage read FGraphic;
    property Canvas: TFastARGB32Canvas read FCanvas;
    procedure AddRef;
    procedure DelRef;

    {ICacheable}
    function CanBeRemoved: Boolean;
    procedure RemoveFromCache;
  end;

  TLightCache = specialize TCacheManager<TLightMaterial>;

  TLightManager = class;

  { TLightSource }

  TLightSource = class
    constructor Create(AManager: TLightManager; AWorldItem: TWorldItem);
    destructor Destroy; override;
  protected
    FX: Integer;
    FY: Integer;
    FZ: SmallInt;
    FMaterial: TLightMaterial;
  public
    property X: Integer read FX;
    property Y: Integer read FY;
    property Z: SmallInt read FZ;
    property Material: TLightMaterial read FMaterial;
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
    FLightLevel: Byte;
    FValid: Boolean;
    FCalculateOffset: TCalculateOffset;
    FLightCache: TLightCache;
    function GetLight(AID: Integer): TLightMaterial;
    procedure SetLightLevel(AValue: Byte);
    procedure UpdateOverlay(AScreenRect: TRect);
  public
    property LightLevel: Byte read FLightLevel write SetLightLevel;
    procedure UpdateLightMap(ALeft, AWidth, ATop, AHeight: Integer;
      AScreenBuffer: TScreenBuffer);
    procedure Draw(AScreenRect: TRect);
  end;

implementation

uses
  UGameResources, UTiledata, UStatics, ULight, Logging;

{ TLightManager }

constructor TLightManager.Create(ACalculateOffset: TCalculateOffset);
begin
  FCalculateOffset := ACalculateOffset;
  FLightSources := TLightSources.Create(True);
  FLightLevel := 0;
  FLightCache := TLightCache.Create(32);
end;

destructor TLightManager.Destroy;
begin
  FreeAndNil(FLightSources);
  FreeAndNil(FOverlay);
  FreeAndNil(FLightCache);
  glDeleteTextures(1, @FOverlayTexture);
  inherited Destroy;
end;

function TLightManager.GetLight(AID: Integer): TLightMaterial;
var
  light: TLight;
begin
  Result := nil;
  if not FLightCache.QueryID(AID, Result) then
  begin
    if ResMan.Lights.Exists(AID) then
    begin
      light := ResMan.Lights.GetLight(AID);
      Result := TLightMaterial.Create(light.Graphic);
      FLightCache.StoreID(AID, Result);
      light.Free;
    end;
  end;
end;

procedure TLightManager.SetLightLevel(AValue: Byte);
begin
  FLightLevel := AValue;
  FValid := False;
end;

procedure TLightManager.UpdateOverlay(AScreenRect: TRect);
var
  canvas, lightCanvas: TFastARGB32Canvas;
  color: TColor32Rec;
  i: Integer;
  lightMaterial: TLightMaterial;
begin
  FOverlay.Free;
  glDeleteTextures(1, @FOverlayTexture);

  color.A := $FF;
  color.R := ((32 - FLightLevel) * 255) div 32;
  color.G := color.R;
  color.B := color.R;

  FOverlay := TSingleImage.CreateFromParams(AScreenRect.Right,
    AScreenRect.Bottom, ifA8R8G8B8);
  canvas := TFastARGB32Canvas.CreateForImage(FOverlay);
  try
    canvas.FillColor32 := color.Color;
    canvas.FillRect(AScreenRect);
  finally
    canvas.Free;
  end;

  for i := 0 to FLightSources.Count - 1 do
  begin
    lightMaterial := FLightSources[i].Material;
    if lightMaterial <> nil then
    begin
      lightMaterial.Canvas.DrawAdd(lightMaterial.Canvas.ClipRect, canvas,
        FLightSources[i].FX - lightMaterial.Graphic.Width div 2,
        FLightSources[i].FY - lightMaterial.Graphic.Height div 2);
    end;
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
          ((itemMap[x + 1, y + 1] = nil) or (itemMap[x + 1, y + 1].Z <
          lightMap[x, y].Z + 3)) or ((itemMap[x, y + 1] = nil) or
          (itemMap[x, y + 1].Z < lightMap[x, y].Z + 3)) then
        begin
          FLightSources.Add(TLightSource.Create(Self, lightMap[x, y]));
        end;
      end;
  FValid := False;
  //Logger.ExitMethod([lcClient, lcDebug], 'UpdateLightMap');
end;

procedure TLightManager.Draw(AScreenRect: TRect);
begin
  if not FValid then
    UpdateOverlay(AScreenRect);

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

constructor TLightSource.Create(AManager: TLightManager; AWorldItem: TWorldItem);
var
  lightID: Byte;
begin
  lightID := ResMan.Tiledata.StaticTiles[AWorldItem.TileID].Quality;
  FMaterial := AManager.GetLight(lightID);
  if FMaterial <> nil then
  begin
    AManager.FCalculateOffset(AWorldItem.X, AWorldItem.Y, FX, FY);
    FZ := AWorldItem.Z * 4;
    FY := FY + 22 - FZ;
    FMaterial.AddRef;
  end;
end;

destructor TLightSource.Destroy;
begin
  if FMaterial <> nil then
    FMaterial.DelRef;
  inherited Destroy;
end;

{ TLightMaterial }

constructor TLightMaterial.Create(AGraphic: TBaseImage);
begin
  FRefCount := 1;
  FGraphic := TSingleImage.CreateFromImage(AGraphic);
  FCanvas := TFastARGB32Canvas.CreateForImage(FGraphic);
end;

destructor TLightMaterial.Destroy;
begin
  FreeAndNil(FCanvas);
  FreeAndNil(FGraphic);
  inherited Destroy;
end;

procedure TLightMaterial.AddRef;
begin
  Inc(FRefCount);
end;

procedure TLightMaterial.DelRef;
begin
  Dec(FRefCount);
  if FRefCount < 1 then
    Free;
end;

function TLightMaterial.CanBeRemoved: Boolean;
begin
  Result := (FRefCount <= 1);
end;

procedure TLightMaterial.RemoveFromCache;
begin
  DelRef;
end;

end.

