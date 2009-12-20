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
unit UOverlayUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Gl, GLU, Imaging, ImagingTypes, ImagingClasses,
  ImagingOpenGL, OpenGLContext, ImagingUtility;
  
type

  { TGLArrow }

  TGLArrow = class(TObject)
    constructor Create(AGraphic: TSingleImage);
    destructor Destroy; override;
  protected
    FGraphic: TSingleImage;
    FTexture: GLuint;
    FRealWidth: Integer;
    FRealHeight: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FCurrentX: Integer;
    FCurrentY: Integer;
    procedure UpdateTexture;
  public
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property CurrentX: Integer read FCurrentX;
    property CurrentY: Integer read FCurrentY;
  
    function HitTest(AX, AY: Integer): Boolean;
    procedure DrawGL(AX, AY: Integer; AActive: Boolean = False);
  end;
  
  { TOverlayUI }

  TOverlayUI = class(TObject)
    constructor Create;
    destructor Destroy; override;
  protected
    FArrows: array[0..7] of TGLArrow;
    FActiveArrow: Integer;
    FVisible: Boolean;
  public
    property ActiveArrow: Integer read FActiveArrow write FActiveArrow;
    property Visible: Boolean read FVisible write FVisible;
    function HitTest(AX, AY: Integer): Integer;
    procedure Draw(AContext: TOpenGLControl);
  end;

implementation

uses
  UResourceManager;

{ TGLArrow }

constructor TGLArrow.Create(AGraphic: TSingleImage);
var
  caps: TGLTextureCaps;
begin
  inherited Create;
  FRealWidth := AGraphic.Width;
  FRealHeight := AGraphic.Height;
  GetGLTextureCaps(caps);
  if caps.NonPowerOfTwo then
  begin
    FWidth := FRealWidth;
    FHeight := FRealHeight;
  end else
  begin
    if IsPow2(FRealWidth) then FWidth := FRealWidth else FWidth := NextPow2(FRealWidth);
    if IsPow2(FRealHeight) then FHeight := FRealHeight else FHeight := NextPow2(FRealHeight);
  end;
  FGraphic := TSingleImage.CreateFromParams(FWidth, FHeight, ifA8R8G8B8);
  AGraphic.CopyTo(0, 0, FRealWidth, FRealHeight, FGraphic, 0, 0);
  FTexture := 0;
end;

destructor TGLArrow.Destroy;
begin
  if FGraphic <> nil then FreeAndNil(FGraphic);
  if FTexture <> 0 then glDeleteTextures(1, @FTexture);
  inherited Destroy;
end;

procedure TGLArrow.UpdateTexture;
begin
  if (FGraphic <> nil) and (FRealWidth > 0) and (FRealWidth > 0) then
  begin
    FTexture := CreateGLTextureFromImage(FGraphic.ImageDataPointer^, 0, 0, False);

    glBindTexture(GL_TEXTURE_2D, FTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  end;
end;

function TGLArrow.HitTest(AX, AY: Integer): Boolean;
var
  pixel: TColor32Rec;
begin
  if (AX > -1) and (AX < FRealWidth) and (AY > -1) and (AY < FRealHeight) then
  begin
    pixel := GetPixel32(FGraphic.ImageDataPointer^, AX, AY);
    Result := pixel.A > 0;
  end else
    Result := False;
end;

procedure TGLArrow.DrawGL(AX, AY: Integer; AActive: Boolean = False);
begin
  FCurrentX := AX;
  FCurrentY := AY;

  if FTexture = 0 then UpdateTexture;

  if FTexture <> 0 then
  begin
    if AActive then
    begin
      glEnable(GL_COLOR_LOGIC_OP);
      glLogicOp(GL_COPY_INVERTED);
    end;

    glBindTexture(GL_TEXTURE_2D, FTexture);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex2d(AX, AY);
      glTexCoord2f(1, 0); glVertex2d(AX + FWidth, AY);
      glTexCoord2f(1, 1); glVertex2d(AX + FWidth, AY + FHeight);
      glTexCoord2f(0, 1); glVertex2d(AX, AY + FHeight);
    glEnd;
    
    if AActive then
      glDisable(GL_COLOR_LOGIC_OP);
  end;
end;

{ TOverlayUI }

constructor TOverlayUI.Create;
var
  i: Integer;
  arrow: TSingleImage;
begin
  inherited Create;
  FActiveArrow := -1;
  FVisible := False;
  
  arrow := TSingleImage.CreateFromStream(ResourceManager.GetResource(0));
  for i := 0 to 3 do
  begin
    FArrows[2*i] := TGLArrow.Create(arrow);
    if i < 3 then
      arrow.Rotate(-90);
  end;
  arrow.Free;

  arrow := TSingleImage.CreateFromStream(ResourceManager.GetResource(1));
  for i := 0 to 3 do
  begin
    FArrows[2*i+1] := TGLArrow.Create(arrow);
    if i < 3 then
      arrow.Rotate(-90);
  end;
  arrow.Free;
end;

destructor TOverlayUI.Destroy;
var
  i: Integer;
begin
  for i := 0 to 7 do
    if FArrows[i] <> nil then FreeAndNil(FArrows[i]);
    
  inherited Destroy;
end;

function TOverlayUI.HitTest(AX, AY: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := 0;
  while (i <= 7) and (Result = -1) do
  begin
    if FArrows[i].HitTest(AX - FArrows[i].CurrentX, AY - FArrows[i].CurrentY) then
      Result := i;
    Inc(i);
  end;
end;

procedure TOverlayUI.Draw(AContext: TOpenGLControl);
begin
  if FVisible then
  begin
    glColor4f(1.0, 1.0, 1.0, 1.0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    FArrows[0].DrawGL(10, 10, FActiveArrow = 0);
    FArrows[1].DrawGL(AContext.Width div 2 - FArrows[1].Width div 2, 10,
                      FActiveArrow = 1);
    FArrows[2].DrawGL(AContext.Width - 10 - FArrows[2].Width, 10,
                      FActiveArrow = 2);

    FArrows[3].DrawGL(AContext.Width - 10 - FArrows[3].Width,
                      AContext.Height div 2 - FArrows[3].Height div 2,
                      FActiveArrow = 3);

    FArrows[4].DrawGL(AContext.Width - 10 - FArrows[4].Width,
                      AContext.Height - 10 - FArrows[4].Height,
                      FActiveArrow = 4);
    FArrows[5].DrawGL(AContext.Width div 2 - FArrows[5].Width div 2,
                      AContext.Height - 10 - FArrows[5].Height,
                      FActiveArrow = 5);
    FArrows[6].DrawGL(10, AContext.Height - 10 - FArrows[6].Height,
                      FActiveArrow = 6);

    FArrows[7].DrawGL(10, AContext.Height div 2 - FArrows[7].Height div 2,
                      FActiveArrow = 7);
  end;
end;

end.

