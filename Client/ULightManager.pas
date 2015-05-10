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
 *      Portions Copyright 2015 StaticZ
 *)
unit ULightManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Imaging, ImagingTypes, ImagingClasses, ImagingCanvases,
  ImagingOpenGL, GL, GLu, GLext, Math, heContnrs, ULandscape, UWorldItem,
  UCacheManager, DOM, XMLRead;

const
  ColorsCount = 15;

type

  TLightColor = record
    r: Float;
    g: Float;
    b: Float;
  end;
  PLightColor = ^TLightColor;

  TColorRefArray = array of Byte;

  TCalculateOffset = procedure(AX, AY: Integer; out DrawX, DrawY: Integer) of object;

  { TLightMaterial }

  TLightMaterial = class(TSimpleMaterial)
    constructor Create(AGraphic: TBaseImage);
    destructor Destroy; override;
  protected
    FCanvas: TFastARGB32Canvas;
  public
    property Canvas: TFastARGB32Canvas read FCanvas;
  end;

  TLightCache = specialize TCacheManager<TLightMaterial>;

  TLightManager = class;

  { TLightSource }

  TLightSource = class
    constructor Create(AManager: TLightManager; AWorldItem: TWorldItem);
    destructor Destroy; override;
  protected
    FColorID: Byte;
    FX: Integer;
    FY: Integer;
    FZ: SmallInt;
    FMaterial: TLightMaterial;
  public
    property ColorID: Byte read FColorID;
    property X: Integer read FX;
    property Y: Integer read FY;
    property Z: SmallInt read FZ;
    property Material: TLightMaterial read FMaterial;
  end;

  TLightSources = specialize TheObjectVector<TLightSource>;

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
    FUseFBO: Boolean;
    FInitialized: Boolean;
    FLightColors: array[1..ColorsCount] of TLightColor;
    FTileCol: TColorRefArray;
    function GetLight(AID: Integer): TLightMaterial;
    procedure SetLightLevel(AValue: Byte);
    procedure UpdateOverlay(AScreenRect: TRect);
  private
    property TileCol: TColorRefArray read FTileCol;
  public
    property LightLevel: Byte read FLightLevel write SetLightLevel;
    procedure InitGL;
    procedure UpdateLightMap(ALeft, AWidth, ATop, AHeight: Integer;
      AScreenBuffer: TScreenBuffer);
    procedure Draw(AScreenRect: TRect);
    procedure LoadConfig(AFileName: String);
  end;

implementation

uses
  UGameResources, UTiledata, UStatics, UMap, ULight, Logging;

{ TLightManager }

constructor TLightManager.Create(ACalculateOffset: TCalculateOffset);
begin
  FCalculateOffset := ACalculateOffset;
  FLightSources := TLightSources.Create(True);
  FLightLevel := 0;
  FLightCache := TLightCache.Create(32);
  FInitialized := False;
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
  canvas: TFastARGB32Canvas;
  color: TColor32Rec;
  i: Integer;
  lightMaterial: TLightMaterial;
  colorGL: GLclampf;
  fbo: GLuint;
  colorref: PLightColor;
begin
  glDeleteTextures(1, @FOverlayTexture);
  if FUseFBO then
  begin
    glGenTextures(1, @FOverlayTexture);
    glBindTexture(GL_TEXTURE_2D, FOverlayTexture);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, AScreenRect.Right,
      AScreenRect.Bottom, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
    glBindTexture(GL_TEXTURE_2D, 0);

    glGenFramebuffersEXT(1, @fbo);
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fbo);
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT,
      GL_TEXTURE_2D, FOverlayTexture, 0);

    colorGL :=(32 - lightLevel) / 32;
    glClearColor(colorGL, colorGL, colorGL, 1);
    glClear(GL_COLOR_BUFFER_BIT);

    glBlendFunc(GL_ONE, GL_ONE);
    for i := 0 to FLightSources.Count - 1 do
      begin
        lightMaterial := FLightSources[i].Material;
        if lightMaterial <> nil then
        begin
          colorref := @FLightColors[FLightSources[i].ColorID];
          glBindTexture(GL_TEXTURE_2D, lightMaterial.Texture);
          glColor3f(colorref^.R, colorref^.G, colorref^.B);
          glBegin(GL_QUADS);
            glTexCoord2i(0, 0);
            glVertex2i(FLightSources[i].FX - lightMaterial.RealWidth div 2,
              FLightSources[i].FY - lightMaterial.RealHeight div 2);
            glTexCoord2i(0, 1);
            glVertex2i(FLightSources[i].FX - lightMaterial.RealWidth div 2,
              FLightSources[i].FY - lightMaterial.RealHeight div 2 +
              lightMaterial.Height);
            glTexCoord2i(1, 1);
            glVertex2i(FLightSources[i].FX - lightMaterial.RealWidth div 2 +
              lightMaterial.Width, FLightSources[i].FY -
              lightMaterial.RealHeight div 2 + lightMaterial.Height);
            glTexCoord2i(1, 0);
            glVertex2i(FLightSources[i].FX - lightMaterial.RealWidth div 2 +
              lightMaterial.Width,
              FLightSources[i].FY - lightMaterial.RealHeight div 2);
          glEnd;
        end;
      end;

    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
    glDeleteFramebuffersEXT(1, @fbo);
  end else
  begin
    FOverlay.Free;

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

      for i := 0 to FLightSources.Count - 1 do
      begin
        lightMaterial := FLightSources[i].Material;
        if lightMaterial <> nil then
        begin
          lightMaterial.Canvas.DrawAdd(lightMaterial.Canvas.ClipRect, canvas,
            FLightSources[i].FX - lightMaterial.RealWidth div 2,
            FLightSources[i].FY - lightMaterial.RealHeight div 2);
        end;
      end;
    finally
      canvas.Free;
    end;

    FOverlayTexture := CreateGLTextureFromImage(FOverlay.ImageDataPointer^);
  end;

  FValid := True;
end;

procedure TLightManager.InitGL;
begin
  FUseFBO := Load_GL_EXT_framebuffer_object;
end;

procedure TLightManager.UpdateLightMap(ALeft, AWidth, ATop, AHeight: Integer;
  AScreenBuffer: TScreenBuffer);
var
  blockInfo: PBlockInfo;
  lights: TWorldItemList;
  i, x, y, tileID: Integer;
  tileData: TTiledata;
  tileMap: array of array of TWorldItem;
begin
  //Logger.EnterMethod([lcClient, lcDebug], 'UpdateLightMap');
  FLightSources.Clear;
  {Logger.Send([lcClient, lcDebug], 'AWidth', AWidth);
  Logger.Send([lcClient, lcDebug], 'AHeight', AHeight);}
  lights := TWorldItemList.Create(False);
  SetLength(tileMap, AWidth, AHeight);
  for x := 0 to AWidth - 1 do
    for y := 0 to AHeight - 1 do
      tileMap[x,y] := nil;

  blockInfo := nil;
  while AScreenBuffer.Iterate(blockInfo) do
  begin
    if blockInfo^.State = ssNormal then
    begin
      if blockInfo^.Item is TStaticItem then
        tileID := blockInfo^.Item.TileID + $4000
      else
        tileID := blockInfo^.Item.TileID;
      tileData := ResMan.Tiledata.TileData[tileID];

      if tdfLightSource in tileData.Flags then
        lights.Add(blockInfo^.Item)
      else
        x := blockInfo^.Item.X - ALeft;
        y := blockInfo^.Item.Y - ATop;
        if InRange(x, 0, AWidth - 1) and InRange(y, 0, AHeight - 1) then
          tileMap[x, y] := blockInfo^.Item;
    end;
  end;

  for i := 0 to lights.Count - 1 do
  begin
    x := lights[i].X + 1 - ALeft;
    y := lights[i].Y + 1 - ATop;
    if (x = AWidth) or (y = AHeight) or
      (InRange(x, 0, AWidth - 1) and InRange(y, 0, AHeight - 1) and
        ((tileMap[x,y] = nil) or (tileMap[x,y].Z < lights[i].Z + 5))) then
      FLightSources.Add(TLightSource.Create(Self, lights[i]));
  end;

  lights.Free;

  FValid := False;
  //Logger.ExitMethod([lcClient, lcDebug], 'UpdateLightMap');
end;

procedure TLightManager.Draw(AScreenRect: TRect);
begin
  if not FInitialized then
    InitGL;

  glColor4f(1, 1, 1, 1);

  if not FValid then
    UpdateOverlay(AScreenRect);

  glBindTexture(GL_TEXTURE_2D, FOverlayTexture);
  glBlendFunc(GL_ZERO, GL_SRC_COLOR);
  glBegin(GL_QUADS);
  if FUseFBO then
  begin
    glTexCoord2i(0, 1);
    glVertex2i(AScreenRect.Left, AScreenRect.Top);
    glTexCoord2i(0, 0);
    glVertex2i(AScreenRect.Left, AScreenRect.Bottom);
    glTexCoord2i(1, 0);
    glVertex2i(AScreenRect.Right, AScreenRect.Bottom);
    glTexCoord2i(1, 1);
    glVertex2i(AScreenRect.Right, AScreenRect.Top);
  end else
  begin
    glTexCoord2i(0, 0);
    glVertex2i(AScreenRect.Left, AScreenRect.Top);
    glTexCoord2i(0, 1);
    glVertex2i(AScreenRect.Left, AScreenRect.Bottom);
    glTexCoord2i(1, 1);
    glVertex2i(AScreenRect.Right, AScreenRect.Bottom);
    glTexCoord2i(1, 0);
    glVertex2i(AScreenRect.Right, AScreenRect.Top);
  end;
  glEnd;
end;

procedure TLightManager.LoadConfig(AFileName: String);
var
  XMLDoc:  TXMLDocument;
  iNode, node: TDOMNode;
  s: string;
  i, id, col, r, g, b: Integer;
begin
  writeln('Loading Colors from ', AFileName); //TODO
  for i := 1 to ColorsCount do begin
    FLightColors[i].R := 1.0;
    FLightColors[i].G := 1.0;
    FLightColors[i].B := 1.0;
  end;
  SetLength(FTileCol, ResMan.Landscape.MaxStaticID + 1);
  for i := 0 to ResMan.Landscape.MaxStaticID do
    FTileCol[i] := 1;

  //frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['ColorLight.xml']));
  // Read xml file from your hard drive
  ReadXMLFile(XMLDoc, AFileName);
  if LowerCase(XMLDoc.DocumentElement.NodeName) = 'colorlight' then
  begin
    iNode := XMLDoc.DocumentElement.FirstChild;
    while iNode <> nil do
    begin
      if LowerCase(iNode.NodeName) = 'colors' then
      begin
        node := iNode.FirstChild;
        while node <> nil do
        begin
          if (LowerCase(node.NodeName) = 'color') then
          begin
            id := -1;
            r := 255;
            g := 255;
            b := 255;
            for i := node.Attributes.Length - 1 downto 0 do
            begin
              s := LowerCase(node.Attributes[i].NodeName);
              if (s = 'id') then
                TryStrToInt(node.Attributes[i].NodeValue, id);
              if (s = 'r') then
                TryStrToInt(node.Attributes[i].NodeValue, r);
              if (s = 'g') then
                TryStrToInt(node.Attributes[i].NodeValue, g);
              if (s = 'b') then
                TryStrToInt(node.Attributes[i].NodeValue, b);
            end;
            if (id > 0) and (id <= ColorsCount) then
            begin
              if (r <   0) then r :=   0;
              if (g <   0) then g :=   0;
              if (b <   0) then b :=   0;
              if (r > 255) then r := 255;
              if (g > 255) then g := 255;
              if (b > 255) then b := 255;
              FLightColors[id].R := (Float(r)) / 255.0;
              FLightColors[id].G := (Float(g)) / 255.0;
              FLightColors[id].B := (Float(b)) / 255.0;
            end;
          end;
          node := node.NextSibling;
        end;
      end;
      if LowerCase(iNode.NodeName) = 'sources' then
      begin
        node := iNode.FirstChild;
        while node <> nil do
        begin
          s := LowerCase(node.NodeName);
          if (s = 'tile') or (s = 'item') then begin
            col := 1;
            id := -1;
            for i := node.Attributes.Length - 1 downto 0 do begin
              if LowerCase(node.Attributes[i].NodeName) = 'id' then
                if TryStrToInt(node.Attributes[i].NodeValue, id) then
                begin
                  if s = 'tile' then
                    Dec(id, $4000);
                end;
              if LowerCase(node.Attributes[i].NodeName) = 'color' then
                if TryStrToInt(node.Attributes[i].NodeValue, col) then
                begin
                  if (col < 1) or (col > ColorsCount) then
                    col := 1;
                end;
            end;
            if (id >= 0) and (id <= ResMan.Landscape.MaxStaticID) then
              FTileCol[id] := col;
          end;
          node := node.NextSibling;
        end;
      end;
      iNode := iNode.NextSibling;
    end;
  end;
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
    FColorID := AManager.TileCol[AWorldItem.TileID];
    if (FColorID < 1) or (FColorID > ColorsCount) then
      FColorID := 1;
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
  inherited Create(AGraphic);
  FCanvas := TFastARGB32Canvas.CreateForImage(FGraphic);
end;

destructor TLightMaterial.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

end.

