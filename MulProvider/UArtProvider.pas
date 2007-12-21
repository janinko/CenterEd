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
unit UArtProvider;

{$mode objfpc}{$H+}

interface

uses
  Graphics, UMulProvider, UMulBlock, UGenericIndex, UArt, UHue;

type
  TArtProvider = class(TIndexedMulProvider)
  protected
    function GetData(AID: Integer; AIndex: TGenericIndex): TMulBlock; override;
    function GetArtData(AID: Integer; AIndex: TGenericIndex; AColor: Word; AHue: THue; APartialHue: Boolean): TArt;
  public
    function GetArt(AID: Integer; AColor: Word; AHue: THue; APartialHue: Boolean): TArt;
    function GetFlatLand(AID: Integer): TArt;
  end;

implementation

{ TArtProvider }

function TArtProvider.GetData(AID: Integer; AIndex: TGenericIndex): TMulBlock;
begin
  Result := GetArtData(AID, AIndex, clBlack, nil, False);
end;

function TArtProvider.GetArtData(AID: Integer; AIndex: TGenericIndex;
  AColor: Word; AHue: THue; APartialHue: Boolean): TArt;
begin
  if AIndex.Lookup <> LongInt($FFFFFFFF) then
  begin
    if AID < $4000 then
      Result := TArt.Create(FData, AIndex, atLand, AColor, AHue, APartialHue)
    else
      Result := TArt.Create(FData, AIndex, atStatic, AColor, AHue, APartialHue);
  end else
  begin
    if AID < $4000 then
      Result := TArt.Create(nil, nil, atLand, AColor, AHue, APartialHue)
    else
      Result := TArt.Create(nil, nil, atStatic, AColor, AHue, APartialHue);
  end;
  Result.ID := AID;
end;

function TArtProvider.GetArt(AID: Integer; AColor: Word; AHue: THue; APartialHue: Boolean): TArt;
var
  genericIndex: TGenericIndex;
begin
  FIndex.Position := CalculateIndexOffset(AID);
  genericIndex := TGenericIndex.Create(FIndex);
  Result := GetArtData(AID, genericIndex, AColor, AHue, APartialHue);
  genericIndex.Free;
  Result.OnChanged := @OnChanged;
  Result.OnFinished := @OnFinished;
end;

function TArtProvider.GetFlatLand(AID: Integer): TArt;
var
  genericIndex: TGenericIndex;
begin
  FIndex.Position := CalculateIndexOffset(AID);
  genericIndex := TGenericIndex.Create(FIndex);
  Result := TArt.Create(FData, genericIndex, atLandFlat);
  genericIndex.Free;
  Result.OnChanged := @OnChanged;
  Result.OnFinished := @OnFinished;
end;

end.
