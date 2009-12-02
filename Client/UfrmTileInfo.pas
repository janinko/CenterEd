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
unit UfrmTileInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLIntf, LCLType, LMessages, strutils;

type

  { TfrmTileInfo }

  TfrmTileInfo = class(TForm)
    lblName: TLabel;
    lblFlags: TLabel;
    lblTileID: TLabel;
    tmHide: TTimer;
    procedure FormShow(Sender: TObject);
    procedure tmHideTimer(Sender: TObject);
  private
    { private declarations }
  public
    procedure Update(ATileID: Word);
    //procedure Show; overload; reintroduce;
    procedure Show(ATileID: Word); overload;
  end; 

var
  frmTileInfo: TfrmTileInfo;

implementation

uses
  UGameResources, UTiledata;

{ TfrmTileInfo }

procedure TfrmTileInfo.tmHideTimer(Sender: TObject);
begin
  tmHide.Enabled := False;
  Hide;
end;

procedure TfrmTileInfo.FormShow(Sender: TObject);
begin
  tmHide.Enabled := True;
  Left := Mouse.CursorPos.x + 8;
  Top := Mouse.CursorPos.y + 8;
end;

procedure TfrmTileInfo.Update(ATileID: Word);
var
  tileData: TTiledata;
  prefix, flags: string;
  
  procedure UpdateFlags(AFlag: TTileDataFlag; AName: string);
  begin
    if AFlag in tileData.Flags then
    begin
      if flags <> '' then
        flags := flags + ', ' + AName
      else
        flags := AName;
    end;
  end;
  
begin
  if Visible then
  begin
    Left := Mouse.CursorPos.x + 8;
    Top := Mouse.CursorPos.y + 8;
  end;

  flags := '';

  if ATileID < $4000 then
  begin
    tileData := ResMan.Tiledata.LandTiles[ATileID];
    if TLandTiledata(tileData).TextureID > 0 then
      flags := 'Stretchable';
  end else
  begin
    Dec(ATileID, $4000);
    tileData := ResMan.Tiledata.StaticTiles[ATileID];
  end;

  if tdfArticleA in tileData.Flags then
    prefix := 'a '
  else if tdfArticleAn in tileData.Flags then
    prefix := 'an '
  else
    prefix := '';

  lblName.Caption := AnsiProperCase(Format('%s%s', [prefix, tileData.TileName]), [' ']);
  lblTileID.Caption := Format('Tile ID: $%x (%0:d)', [ATileID]);
  
  UpdateFlags(tdfBackground, 'Background');
  UpdateFlags(tdfWeapon, 'Weapon');
  UpdateFlags(tdfTransparent, 'Transparent');
  UpdateFlags(tdfTranslucent, 'Translucent');
  UpdateFlags(tdfWall, 'Wall');
  UpdateFlags(tdfDamaging, 'Damaging');
  UpdateFlags(tdfImpassable, 'Impassable');
  UpdateFlags(tdfWet, 'Wet');
  UpdateFlags(tdfSurface, 'Surface');
  UpdateFlags(tdfBridge, 'Bridge');
  UpdateFlags(tdfGeneric, 'Generic');
  UpdateFlags(tdfWindow, 'Window');
  UpdateFlags(tdfNoShoot, 'NoShoot');
  UpdateFlags(tdfInternal, 'Internal');
  UpdateFlags(tdfFoliage, 'Foliage');
  UpdateFlags(tdfPartialHue, 'PartialHue');
  UpdateFlags(tdfMap, 'Map');
  UpdateFlags(tdfContainer, 'Container');
  UpdateFlags(tdfWearable, 'Wearable');
  UpdateFlags(tdfLightSource, 'Lightsource');
  UpdateFlags(tdfAnimation, 'Animation');
  UpdateFlags(tdfNoDiagonal, 'NoDiagonal');
  UpdateFlags(tdfArmor, 'Armor');
  UpdateFlags(tdfRoof, 'Roof');
  UpdateFlags(tdfDoor, 'Door');
  UpdateFlags(tdfStairBack, 'StairBack');
  UpdateFlags(tdfStairRight, 'StairRight');
  
  lblFlags.Caption := Format('Flags = [%s]', [flags]);
  
  if tmHide.Enabled then
  begin
    tmHide.Enabled := False;
    tmHide.Enabled := True; //Refresh timer
  end;
end;

{procedure TfrmTileInfo.Show;
begin
  ShowWindow(Handle, SW_SHOWNOACTIVATE);
  Include(FormState, fsVisible);
  VisibleChanging;
  try
    Perform(CM_VISIBLECHANGED, WParam(Ord(True)), 0);
    AdjustSize;
    RequestAlign;
  finally
    VisibleChanged;
  end;
  //FormShow(Self);
end;}

procedure TfrmTileInfo.Show(ATileID: Word);
begin
  Update(ATileID);
  Show;
end;

initialization
  {$I UfrmTileInfo.lrs}

end.

