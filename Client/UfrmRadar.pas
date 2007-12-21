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
unit UfrmRadar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ImagingClasses, ImagingComponents, ImagingTypes, UEnhancedMemoryStream, crc,
  StdCtrls;

type

  TRadarColorMap = array of Word;

  { TfrmRadarMap }

  TfrmRadarMap = class(TForm)
    lblPosition: TLabel;
    pbRadar: TPaintBox;
    pnlBottom: TPanel;
    sbMain: TScrollBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure pbRadarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbRadarMouseLeave(Sender: TObject);
    procedure pbRadarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbRadarPaint(Sender: TObject);
  protected
    FRadar: TSingleImage;
    FRadarDependencies: TList;
    procedure OnRadarHandlingPacket(ABuffer: TEnhancedMemoryStream);
    procedure RefreshRadar(ARadarMap: TRadarColorMap);
    procedure RepaintRadar;
  public
    property Radar: TSingleImage read FRadar;
    property Dependencies: TList read FRadarDependencies;
  end; 

var
  frmRadarMap: TfrmRadarMap;

implementation

uses
  UdmNetwork, UGameResources, UPacketHandlers, UPackets, UfrmInitialize,
  UfrmMain, UGraphicHelper;

{ TfrmRadarMap }

procedure TfrmRadarMap.FormCreate(Sender: TObject);
begin
  FRadar := TSingleImage.CreateFromParams(ResMan.Landscape.Width,
    ResMan.Landscape.Height, ifA8R8G8B8);
  pbRadar.Width := FRadar.Width;
  pbRadar.Height := FRadar.Height;
  sbMain.ClientWidth := FRadar.Width;
  sbMain.ClientHeight := FRadar.Height;
  ClientWidth := sbMain.Width + sbMain.VertScrollBar.Size;
  ClientHeight := sbMain.Height + sbMain.HorzScrollBar.Size + pnlBottom.Height;
  Constraints.MaxWidth := Width;
  Constraints.MaxHeight := Height;
  
  FRadarDependencies := TList.Create;
    
  RegisterPacketHandler($0D, TPacketHandler.Create(0, @OnRadarHandlingPacket));
    
  dmNetwork.Send(TRequestRadarChecksumPacket.Create);
end;

procedure TfrmRadarMap.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmRadarMap.FormDestroy(Sender: TObject);
var
  radarMap: TRadarColorMap;
  x, y: Integer;
  radarMapFile: TFileStream;
begin
  RegisterPacketHandler($0D, nil);

  SetLength(radarMap, FRadar.Width * FRadar.Height);
  for x := 0 to FRadar.Width - 1 do
    for y := 0 to FRadar.Height - 1 do
      radarMap[x * FRadar.Height + y] := EncodeUOColor(PInteger(FRadar.PixelPointers[x, y])^);

  radarMapFile := TFileStream.Create(IncludeTrailingPathDelimiter(
    ExtractFilePath(Application.ExeName)) + 'RadarMap.cache', fmCreate);
  radarMapFile.Write(radarMap[0], Length(radarMap) * SizeOf(Word));
  radarMapFile.Free;

  if FRadarDependencies <> nil then FreeAndNil(FRadarDependencies);
  if FRadar <> nil then FreeAndNil(FRadar);
end;

procedure TfrmRadarMap.FormResize(Sender: TObject);
begin
  sbMain.AutoScroll := (Width < Constraints.MaxWidth) or (Height < Constraints.MaxHeight);
end;

procedure TfrmRadarMap.pbRadarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  frmMain.SetPos(X * 8, Y * 8);
end;

procedure TfrmRadarMap.pbRadarMouseLeave(Sender: TObject);
begin
  lblPosition.Caption := '';
end;

procedure TfrmRadarMap.pbRadarMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lblPosition.Caption := Format('X: %d, Y: %d', [X * 8, Y * 8]);
end;

procedure TfrmRadarMap.pbRadarPaint(Sender: TObject);
var
  posX, posY: Word;
begin
  DisplayImage(pbRadar.Canvas, 0, 0, FRadar);
  posX := frmMain.X div 8;
  posY := frmMain.Y div 8;
  pbRadar.Canvas.Pen.Color := clBlack;
  pbRadar.Canvas.Pen.Style := psSolid;
  pbRadar.Canvas.Brush.Color := clRed;
  pbRadar.Canvas.Brush.Style := bsSolid;
  pbRadar.Canvas.Ellipse(posX - 3, posY - 3, posX + 3, posY + 3);
  {pbRadar.Canvas.Pen.Color := clRed;
  pbRadar.Canvas.Pen.Style := psDash;
  pbRadar.Canvas.Line(0, posY, pbRadar.Width, posY);
  pbRadar.Canvas.Line(posX, 0, posX, pbRadar.Height);}
end;

procedure TfrmRadarMap.OnRadarHandlingPacket(ABuffer: TEnhancedMemoryStream);
var
  subID: Byte;
  checksum, realChecksum: Cardinal;
  radarMapFile: TFileStream;
  radarMapFileName: string;
  radarMap: TRadarColorMap;
  x, y: Integer;
begin
  subID := ABuffer.ReadByte;
  case subID of
    $01: //checksum
      begin
        checksum := ABuffer.ReadCardinal;
        realChecksum := crc32(0, nil, 0);
        radarMapFileName := IncludeTrailingPathDelimiter(ExtractFilePath(
          Application.ExeName)) + 'RadarMap.cache';
        if FileExists(radarMapFileName) then
        begin
          radarMapFile := TFileStream.Create(radarMapFileName, fmOpenRead);
          SetLength(radarMap, radarMapFile.Size div SizeOf(Word));
          radarMapFile.Read(radarMap[0], radarMapFile.Size);
          radarMapFile.Free;
          
          realChecksum := crc32(realChecksum, @radarMap[0], Length(radarMap) * SizeOf(Word));
        end;
        
        if checksum <> realChecksum then
        begin
          frmInitialize.lblStatus.Caption := 'Updating Radar Map';
          frmInitialize.Show;
          frmInitialize.SetModal;
          //frmMain.Enabled := False;
          dmNetwork.Send(TRequestRadarMapPacket.Create);
        end else
          RefreshRadar(radarMap);
      end;
    $02: //radar map
      begin
        SetLength(radarMap, (ABuffer.Size - ABuffer.Position) div SizeOf(Word));
        ABuffer.Read(radarMap[0], Length(radarMap) * SizeOf(Word));
        RefreshRadar(radarMap);
        //frmMain.Enabled := True;
        frmInitialize.UnsetModal;
        frmInitialize.Hide;
      end;
    $03: //update radar
      begin
        x := ABuffer.ReadWord;
        y := ABuffer.ReadWord;
        PInteger(FRadar.PixelPointers[x, y])^ := DecodeUOColor(ABuffer.ReadWord);
        RepaintRadar;
      end;
  end;
end;

procedure TfrmRadarMap.RefreshRadar(ARadarMap: TRadarColorMap);
var
  x, y: Integer;
begin
  for x := 0 to FRadar.Width - 1 do
    for y := 0 to FRadar.Height - 1 do
      PInteger(FRadar.PixelPointers[x, y])^ := DecodeUOColor(ARadarMap[x * FRadar.Height + y]);
  RepaintRadar;
end;

procedure TfrmRadarMap.RepaintRadar;
var
  i: Integer;
begin
  pbRadar.Repaint;
  for i := 0 to FRadarDependencies.Count - 1 do
    TWinControl(FRadarDependencies.Items[i]).Repaint;
end;

initialization
  {$I UfrmRadar.lrs}

end.

