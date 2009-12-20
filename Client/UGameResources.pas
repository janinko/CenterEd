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
unit UGameResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UArtProvider, UTileDataProvider, UTexmapProvider,
  ULandscape, UHueProvider, UAnimDataProvider, ULightProvider;

type

  { TGameResourceManager }

  TGameResourceManager = class
    constructor Create(ADataDir: String);
    destructor Destroy; override;
  protected
    { Members }
    FDataDir: String;
    FArtProvider: TArtProvider;
    FTiledataProvider: TTiledataProvider;
    FAnimdataProvider: TAnimdataProvider;
    FTexmapProvider: TTexmapProvider;
    FHueProvider: THueProvider;
    FLightProvider: TLightProvider;
    FLandscape: TLandscape;
  public
    { Fields }
    property Art: TArtProvider read FArtProvider;
    property Hue: THueProvider read FHueProvider;
    property Landscape: TLandscape read FLandscape;
    property Tiledata: TTiledataProvider read FTiledataProvider;
    property Animdata: TAnimDataProvider read FAnimdataProvider;
    property Texmaps: TTexmapProvider read FTexmapProvider;
    property Lights: TLightProvider read FLightProvider;

    { Methods }
    function GetFile(AFileName: String): String;
    procedure InitLandscape(AWidth, AHeight: Word);
  end;

var
  GameResourceManager: TGameResourceManager;
  ResMan: TGameResourceManager absolute GameResourceManager;

procedure InitGameResourceManager(ADataDir: String);

implementation

procedure InitGameResourceManager(ADataDir: String);
begin
  FreeAndNil(GameResourceManager);
  GameResourceManager := TGameResourceManager.Create(ADataDir);
end;

{ TGameResourceManager }

constructor TGameResourceManager.Create(ADataDir: String);
begin
  inherited Create;
  FDataDir := IncludeTrailingPathDelimiter(ADataDir);

  FArtProvider := TArtProvider.Create(GetFile('art.mul'), GetFile('artidx.mul'), True);
  FTiledataProvider := TTiledataProvider.Create(GetFile('tiledata.mul'), True);
  FAnimdataProvider := TAnimDataProvider.Create(GetFile('animdata.mul'), True);
  FTexmapProvider := TTexmapProvider.Create(GetFile('texmaps.mul'),
    GetFile('texidx.mul'), True);
  FHueProvider := THueProvider.Create(GetFile('hues.mul'), True);
  FLightProvider := TLightProvider.Create(GetFile('light.mul'),
    GetFile('lightidx.mul'), True);
end;

destructor TGameResourceManager.Destroy;
begin
  FreeAndNil(FArtProvider);
  FreeAndNil(FTiledataProvider);
  FreeAndNil(FAnimdataProvider);
  FreeAndNil(FTexmapProvider);
  FreeAndNil(FHueProvider);
  FreeAndNil(FLightProvider);
  FreeAndNil(FLandscape);
  inherited Destroy;
end;

function TGameResourceManager.GetFile(AFileName: String): String;
begin
  Result := FDataDir + AFileName;
end;

procedure TGameResourceManager.InitLandscape(AWidth, AHeight: Word);
begin
  FreeAndNil(FLandscape);
  FLandscape := TLandscape.Create(AWidth, AHeight);
end;

finalization
  FreeAndNil(GameResourceManager);

end.
