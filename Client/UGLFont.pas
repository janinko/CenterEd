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
unit UGLFont;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, ImagingClasses, ImagingTypes, ImagingOpenGL, GL;

type

  TFontInfo = packed record
    Character: Char;
    LeftOffset: SmallInt;
    CharWidth: Word;
    Width: Word;
    Height: Word;
    X1: Single;
    Y1: Single;
    X2: Single;
    Y2: Single;
  end;

  { TGLFont }

  TGLFont = class
    constructor Create;
    destructor Destroy; override;
  protected
    FFontImage: TSingleImage;
    FFontTexture: TGLuint;
    FSpaceWidth: Word;
    FFontInfo: array of TFontInfo;
    function FindCharInfo(AChar: Char): Integer;
  public
    function GetTextHeight(AText: String): Integer;
    function GetTextWidth(AText: String): Integer;
    procedure DrawText(AX, AY: Integer; AText: String);
    procedure LoadImage(AImage: TStream);
    procedure LoadFontInfo(AFontInfo: TStream);
    procedure UpdateTexture;
  end;

implementation

uses
  Logging;

{ TGLFont }


constructor TGLFont.Create;
begin
  FFontTexture := 0;
end;

destructor TGLFont.Destroy;
begin
  FreeAndNil(FFontImage);
  if FFontTexture <> 0 then
    glDeleteTextures(1, @FFontTexture);
  inherited Destroy;
end;

function TGLFont.FindCharInfo(AChar: Char): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := 0;
  while (i < Length(FFontInfo)) and (Result = -1) do
  begin
    if FFontInfo[i].Character = AChar then
      Result := i
    else
      Inc(i);
  end;
end;

function TGLFont.GetTextHeight(AText: String): Integer;
var
  i, charInfo: Integer;
begin
  Result := 0;
  for i := 1 to Length(AText) do
  begin
    if AText[i] <> ' ' then
    begin
      charInfo := FindCharInfo(AText[i]);
      if charInfo > -1 then
        Result := Max(Result, FFontInfo[charInfo].Height);
    end;
  end;
end;

function TGLFont.GetTextWidth(AText: String): Integer;
var
  i, charInfo: Integer;
begin
  Result := 0;
  for i := 1 to Length(AText) do
  begin
    if AText[i] = ' ' then
      Inc(Result, FSpaceWidth)
    else
    begin
      charInfo := FindCharInfo(AText[i]);
      if charInfo > -1 then
        Result := Result + FFontInfo[charInfo].LeftOffset +
          FFontInfo[charInfo].CharWidth;
    end;
  end;
end;

procedure TGLFont.DrawText(AX, AY: Integer; AText: String);
var
  i, charInfo: Integer;
  curX: Integer;
  x1, y1, x2, y2: Single;
begin
  if FFontTexture = 0 then UpdateTexture;
  glBindTexture(GL_TEXTURE_2D, FFontTexture);

  curX := AX;
  for i := 1 to Length(AText) do
  begin
    if AText[i] = ' ' then
      Inc(curX, FSpaceWidth)
    else
    begin
      charInfo := FindCharInfo(AText[i]);
      if charInfo > -1 then
      begin
        x1 := FFontInfo[charInfo].X1;
        y1 := FFontInfo[charInfo].Y1;
        x2 := FFontInfo[charInfo].X2;
        y2 := FFontInfo[charInfo].Y2;

        Inc(curX, FFontInfo[charInfo].LeftOffset);
        glBegin(GL_QUADS);
          glTexCoord2f(x1, y1); glVertex2i(curX, AY);
          glTexCoord2f(x2, y1); glVertex2i(curX + FFontInfo[charInfo].Width, AY);
          glTexCoord2f(x2, y2); glVertex2i(curX + FFontInfo[charInfo].Width,
                                           AY + FFontInfo[charInfo].Height);
          glTexCoord2f(x1, y2); glVertex2i(curX, AY + FFontInfo[charInfo].Height);
        glEnd;
        Inc(curX, FFontInfo[charInfo].CharWidth);
      end;
    end;
  end;
end;

procedure TGLFont.LoadImage(AImage: TStream);
begin
  FFontImage := TSingleImage.CreateFromStream(AImage);
end;

procedure TGLFont.LoadFontInfo(AFontInfo: TStream);
begin
  AFontInfo.Read(FSpaceWidth, SizeOf(FSpaceWidth));
  SetLength(FFontInfo, (AFontInfo.Size - AFontInfo.Position) div
    SizeOf(TFontInfo));
  AFontInfo.Read(FFontInfo[0], Length(FFontInfo) * SizeOf(TFontInfo));
end;

procedure TGLFont.UpdateTexture;
begin
  Logger.Send('UpdateTexture');
  if FFontTexture <> 0 then glDeleteTextures(1, @FFontTexture);

  FFontTexture := CreateGLTextureFromImage(FFontImage.ImageDataPointer^, 0, 0,
    True, ifUnknown);
  glBindTexture(GL_TEXTURE_2D, FFontTexture);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
end;

end.

