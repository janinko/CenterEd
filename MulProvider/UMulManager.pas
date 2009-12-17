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
unit UMulManager;

interface

uses
  SysUtils, UTileDataProvider, UArtProvider, UGumpProvider, UTexmapProvider,
  UHueProvider, URadarProvider, UAnimDataProvider;

type

  { TMulManager }

  TMulManager = class
    destructor Destroy; override;
  protected
    FArtProvider: TArtProvider;
    FGumpProvider: TGumpProvider;
    FTexmapProvider: TTexmapProvider;
    FTileDataProvider: TTileDataProvider;
    FAnimDataProvider: TAnimDataProvider;
    FHueProvider: THueProvider;
    FRadarProvider: TRadarProvider;
  public
    procedure RegisterArtProvider(AArtProvider: TArtProvider);
    procedure RegisterGumpProvider(AGumpProvider: TGumpProvider);
    procedure RegisterTexmapProvider(ATexmapProvider: TTexmapProvider);
    procedure RegisterTileDataProvider(ATileDataProvider: TTileDataProvider);
    procedure RegisterAnimDataProvider(AAnimDataProvider: TAnimDataProvider);
    procedure RegisterHueProvider(AHueProvider: THueProvider);
    procedure RegisterRadarProvider(ARadarProvider: TRadarProvider);
    property ArtProvider: TArtProvider read FArtProvider;
    property GumpProvider:  TGumpProvider read FGumpProvider;
    property TexmapProvider: TTexmapProvider read FTexmapProvider;
    property TileDataProvider: TTileDataProvider read FTileDataProvider;
    property AnimDataProvider: TAnimDataProvider read FAnimDataProvider;
    property HueProvider: THueProvider read FHueProvider;
    property RadarProvider: TRadarPRovider read FRadarProvider;
  end;

implementation

{ TMulManager }

destructor TMulManager.Destroy;
begin
  RegisterArtProvider(nil);
  RegisterGumpProvider(nil);
  RegisterTexmapProvider(nil);
  RegisterTileDataProvider(nil);
  RegisterHueProvider(nil);
  RegisterRadarProvider(nil);
  inherited Destroy;
end;

procedure TMulManager.RegisterArtProvider(
  AArtProvider: TArtProvider);
begin
  FreeAndNil(FArtProvider);
  FArtProvider := AArtProvider;
end;

procedure TMulManager.RegisterGumpProvider(
  AGumpProvider: TGumpProvider);
begin
  FreeAndNil(FGumpProvider);
  FGumpProvider := AGumpProvider;
end;

procedure TMulManager.RegisterHueProvider(
  AHueProvider: THueProvider);
begin
  FreeAndNil(FHueProvider);
  FHueProvider := AHueProvider;
end;

procedure TMulManager.RegisterRadarProvider(
  ARadarProvider: TRadarProvider);
begin
  FreeAndNil(FRadarProvider);
  FRadarProvider := ARadarProvider;
end;

procedure TMulManager.RegisterTexmapProvider(
  ATexmapProvider: TTexmapProvider);
begin
  FreeAndNil(FTexmapProvider);
  FTexmapProvider := ATexmapProvider;
end;

procedure TMulManager.RegisterTileDataProvider(
  ATileDataProvider: TTileDataProvider);
begin
  FreeAndNil(FTileDataProvider);
  FTileDataProvider := ATileDataProvider;
end;

procedure TMulManager.RegisterAnimDataProvider(
  AAnimDataProvider: TAnimDataProvider);
begin
  FreeAndNil(FAnimDataProvider);
  FAnimDataProvider := AAnimDataProvider;
end;

end.
