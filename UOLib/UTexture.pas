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
unit UTexture;

{$mode objfpc}{$H+}

interface

uses
  Classes, Imaging, ImagingTypes, ImagingClasses, UMulBlock, UGenericIndex;

type
  TTexture = class(TMulBlock)
    constructor Create(AData: TStream; AIndex: TGenericIndex); overload;
    constructor Create(AExtra: Integer); overload;
    destructor Destroy; override;
    function Clone: TTexture; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
    procedure RefreshBuffer;
  protected
    FGraphic: TSingleImage;
    FBuffer: TStream;
    FExtra: Integer;
  public
    property Graphic: TSingleImage read FGraphic;
    property Buffer: TStream read FBuffer;
    property Extra: Integer read FExtra write FExtra;
  end;

implementation

constructor TTexture.Create(AData: TStream; AIndex: TGenericIndex);
var
  size: Integer;
begin
  FExtra := AIndex.Various;
  if FExtra = 0 then
    size := 64
  else
    size := 128;
  FGraphic := TSingleImage.CreateFromParams(size, size, ifX1R5G5B5);
  if assigned(AData) then
  begin
    AData.Position := AIndex.Lookup;
    AData.Read(FGraphic.Bits^, size * size * 2);
  end;
  FGraphic.Format := ifX8R8G8B8;
end;

constructor TTexture.Create(AExtra: Integer);
var
  size: Integer;
begin
  FExtra := AExtra;
  if AExtra = 0 then
    size := 64
  else
    size := 128;
  FGraphic := TSingleImage.CreateFromParams(size, size, ifX8R8G8B8);
  FBuffer := TMemoryStream.Create;
end;

destructor TTexture.Destroy;
begin
  if FGraphic <> nil then FGraphic.Free;
  if FBuffer <> nil then FBuffer.Free;
  inherited;
end;

function TTexture.Clone: TTexture;
begin
  Result := TTexture.Create(FExtra);
  Result.FGraphic.Assign(Self.Graphic);
end;

procedure TTexture.Write(AData: TStream);
begin
  FBuffer.Position := 0;
  AData.CopyFrom(FBuffer, FBuffer.Size);
end;

function TTexture.GetSize: Integer;
begin
  RefreshBuffer;
  Result := FBuffer.Size
end;

procedure TTexture.RefreshBuffer;
var
  argbGraphic: TSingleImage;
begin
  argbGraphic := TSingleImage.CreateFromImage(FGraphic);
  argbGraphic.Format := ifX1R5G5B5;
  FBuffer.Size := 0;
  if (argbGraphic.Height > 0) and (argbGraphic.Width > 0) then
  begin
    if (argbGraphic.Height < 128) or (argbGraphic.Width < 128) then
    begin
      FExtra := 0;
      argbGraphic.Resize(64, 64, rfNearest);
    end else
    begin
      FExtra := 1;
      argbGraphic.Resize(128, 128, rfNearest);
    end;
    FBuffer.Write(argbGraphic.Bits^, argbGraphic.Height * argbGraphic.Width * 2);
  end;
  argbGraphic.Free;
end;

end.

