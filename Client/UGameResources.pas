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
unit UGameResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UArtProvider, UTileDataProvider, UTexmapProvider,
  ULandscape, {URadarProvider,} UHueProvider;
  
type

  { TGameResourceManager }

  TGameResourceManager = class(TObject)
    constructor Create(ADataDir: string);
    destructor Destroy; override;
  protected
    FDataDir: string;
    FArtProvider: TArtProvider;
    FTiledataProvider: TTiledataProvider;
    FTexmapProvider: TTexmapProvider;
    //FRadarProvider: TRadarProvider;
    FHueProvider: THueProvider;
    
    FLandscape: TLandscape;
  public
    procedure InitLandscape(AWidth, AHeight: Word);
    function GetFile(AFileName: string): string;
    
    property Art: TArtProvider read FArtProvider;
    property Tiledata: TTiledataProvider read FTiledataProvider;
    property Texmaps: TTexmapProvider read FTexmapProvider;
    //property Radar: TRadarProvider read FRadarProvider;
    property Hue: THueProvider read FHueProvider;
    property Landscape: TLandscape read FLandscape;
  end;
  
var
  GameResourceManager: TGameResourceManager;
  ResMan: TGameResourceManager absolute GameResourceManager;
  
procedure InitGameResourceManager(ADataDir: string);

implementation

procedure InitGameResourceManager(ADataDir: string);
begin
  if GameResourceManager <> nil then FreeAndNil(GameResourceManager);
  GameResourceManager := TGameResourceManager.Create(ADataDir);
end;

{ TGameResourceManager }

constructor TGameResourceManager.Create(ADataDir: string);
begin
  inherited Create;
  FDataDir := IncludeTrailingPathDelimiter(ADataDir);
  
  FArtProvider := TArtProvider.Create(GetFile('art.mul'), GetFile('artidx.mul'), True);
  FTiledataProvider := TTiledataProvider.Create(GetFile('tiledata.mul'), True);
  FTexmapProvider := TTexmapProvider.Create(GetFile('texmaps.mul'), GetFile('texidx.mul'), True);
  //FRadarProvider := TRadarProvider.Create(GetFile('radarcol.mul'));
  FHueProvider := THueProvider.Create(GetFile('hues.mul'), True);
end;

destructor TGameResourceManager.Destroy;
begin
  if FArtProvider <> nil then FreeAndNil(FArtProvider);
  if FTiledataProvider <> nil then FreeAndNil(FTiledataProvider);
  if FTexmapProvider <> nil then FreeAndNil(FTexmapProvider);
  //if FRadarProvider <> nil then FreeAndNil(FRadarProvider);
  if FHueProvider <> nil then FreeAndNil(FHueProvider);
  if FLandscape <> nil then FreeAndNil(FLandscape);
  inherited Destroy;
end;

function TGameResourceManager.GetFile(AFileName: string): string;
begin
  Result := FDataDir + AFileName;
end;

procedure TGameResourceManager.InitLandscape(AWidth, AHeight: Word);
begin
  if FLandscape <> nil then FreeAndNil(FLandscape);
  FLandscape := TLandscape.Create(AWidth, AHeight);
end;

finalization
begin
  if GameResourceManager <> nil then FreeAndNil(GameResourceManager);
end;

end.

