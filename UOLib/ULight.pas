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
unit ULight;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Imaging, ImagingClasses, ImagingTypes, UMulBlock,
  UGenericIndex;

type

  { TLight }

  TLight = class(TMulBlock)
    constructor Create(AData: TStream; AIndex: TGenericIndex);
    destructor Destroy; override;
    function Clone: TLight; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  protected
    FGraphic: TSingleImage;
  public
    property Graphic: TSingleImage read FGraphic;
  end;

implementation

{ TLight }

constructor TLight.Create(AData: TStream; AIndex: TGenericIndex);
var
  buffer: TMemoryStream;
  Width, Height: Word;
  color: Byte;
  color32: TColor32Rec;
  x, y: Integer;
begin
  if (AIndex <> nil) and (AIndex.Lookup > -1) and (AIndex.Size > 0) then
  begin
    Width := word(AIndex.Various shr 16);
    Height := AIndex.Various and $FFFF;
    FGraphic := TSingleImage.CreateFromParams(Width, Height, ifA8R8G8B8);

    if AData <> nil then
    begin
      AData.Position := AIndex.Lookup;
      buffer := TMemoryStream.Create;
      buffer.CopyFrom(AData, AIndex.Size);
      buffer.Position := 0;
      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
        begin
          buffer.Read(color, SizeOf(byte));
          color32.R := color * 8;
          color32.G := color32.R;
          color32.B := color32.R;
          if color > 0 then
            color32.A := 255
          else
            color32.A := 0;
          PColor32(FGraphic.PixelPointers[x, y])^ := color32.Color;
        end;
      buffer.Free;
    end;
  end;

  if FGraphic = nil then
    FGraphic := TSingleImage.CreateFromParams(0, 0, ifA8R8G8B8);
end;

destructor TLight.Destroy;
begin
  FreeAndNil(FGraphic);
  inherited Destroy;
end;

function TLight.Clone: TLight;
begin
  Result := TLight.Create(nil, nil);
  Result.Graphic.Assign(FGraphic);
end;

function TLight.GetSize: Integer;
begin
  Result := 0;
  raise Exception.Create('Not implemented: TLight.GetSize');
end;

procedure TLight.Write(AData: TStream);
begin
  raise Exception.Create('Not implemented: TLight.Write');
end;

end.

