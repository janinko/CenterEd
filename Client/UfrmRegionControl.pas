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
 *      Portions Copyright 2008 Andreas Schneider
 *)
unit UfrmRegionControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CheckLst,
  VirtualTrees, ExtCtrls, ImagingComponents, StdCtrls, Buttons, Spin, LCLIntf,
  math, UPlatformTypes, UEnhancedMemoryStream, Menus, contnrs, URectList;

type
  TAreaMoveType = (amLeft, amTop, amRight, amBottom);
  TAreaMove = set of TAreaMoveType;

  { TfrmRegionControl }

  TfrmRegionControl = class(TForm)
    btnAddArea: TSpeedButton;
    btnClearArea: TSpeedButton;
    btnDeleteArea: TSpeedButton;
    btnExit: TButton;
    btnSave: TButton;
    Label1: TLabel;
    lblX: TLabel;
    lblY: TLabel;
    mnuAddRegion: TMenuItem;
    mnuRemoveRegion: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pbArea: TPaintBox;
    pnlAreaControls: TPanel;
    pmRegions: TPopupMenu;
    sbArea: TScrollBox;
    seX1: TSpinEdit;
    seX2: TSpinEdit;
    seY1: TSpinEdit;
    seY2: TSpinEdit;
    vstRegions: TVirtualStringTree;
    vstArea: TVirtualStringTree;
    procedure acAddGroup(Sender: TObject);
    procedure accRemoveGroup(Sender: TObject);
    procedure btnAddAreaClick(Sender: TObject);
    procedure btnClearAreaClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDeleteAreaClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbAreaMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure pbAreaPaint(Sender: TObject);
    procedure seX1Change(Sender: TObject);
    procedure vstAreaChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstAreaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstRegionsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstRegionsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstRegionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstRegionsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: WideString);
    procedure vstRegionsOnEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
  protected
    FLastX: Integer;
    FLastY: Integer;
    FAreaMove: TAreaMove;
    procedure OnListRegionsPacket(ABuffer: TEnhancedMemoryStream);
  private
    { private declarations }
  public
    { public declarations }
  end; 
  
var
  frmRegionControl: TfrmRegionControl;

implementation

uses
  UGameResources, UfrmRadar, UfrmMain, UdmNetwork, UPacket, UPackets,
  UGUIPlatformUtils, UAdminHandling, UPacketHandlers;

type

  { TRequestRegionListPacket }

  TRequestRegionListPacket = class(TPacket)
    constructor Create;
  end;
  
  PRegionInfo = ^TRegionInfo;
  TRegionInfo = record
    Name: string;
    Areas: TRectList;
  end;

{ TRequestRegionListPacket }

constructor TRequestRegionListPacket.Create;
begin
  inherited Create($03, 0);
  FStream.WriteByte($0A);
end;

{ TfrmRegionControl }

procedure TfrmRegionControl.OnListRegionsPacket(ABuffer: TEnhancedMemoryStream);
var
  regionCount, areaCount: Byte;
  i, j, x1, x2, y1, y2: Integer;
  node: PVirtualNode;
  regionInfo: PRegionInfo;
begin
  vstRegions.BeginUpdate;
  vstRegions.Clear;
  regionCount := ABuffer.ReadByte;
  for i := 0 to regionCount - 1 do
  begin
   node := vstRegions.AddChild(nil);
   regionInfo := vstRegions.GetNodeData(node);
   regionInfo^.Name := ABuffer.ReadStringNull;
   regionInfo^.Areas := TRectList.Create;
   areaCount := ABuffer.ReadByte;
   for j := 0 to areaCount - 1 do
   begin
     x1 := ABuffer.ReadWord;
     y1 := ABuffer.ReadWord;
     x2 := ABuffer.ReadWord;
     y2 := ABuffer.ReadWord;
     regionInfo^.Areas.Add(x1, y1, x2, y2);
   end;
  end;
  vstRegions.EndUpdate;
end;


procedure TfrmRegionControl.FormCreate(Sender: TObject);
begin
  pbArea.Width := frmRadarMap.Radar.Width;
  pbArea.Height := frmRadarMap.Radar.Height;
  seX1.MaxValue := ResMan.Landscape.CellWidth;
  seX2.MaxValue := ResMan.Landscape.CellWidth;
  seY1.MaxValue := ResMan.Landscape.CellHeight;
  seY2.MaxValue := ResMan.Landscape.CellHeight;
  
  vstArea.NodeDataSize := SizeOf(TRect);
  vstRegions.NodeDataSize := SizeOf(TRegionInfo);
  
  frmRadarMap.Dependencies.Add(pbArea);

  AdminPacketHandlers[$0A] := TPacketHandler.Create(0, @OnListRegionsPacket);
end;

procedure TfrmRegionControl.btnDeleteAreaClick(Sender: TObject);
var
  infoGroup: PRegionInfo;
  i: Integer;
begin
  if vstRegions.GetFirstSelected <> nil then
  begin
   infoGroup := vstRegions.GetNodeData(vstRegions.GetFirstSelected);
   infoGroup^.Areas.Delete(vstArea.AbsoluteIndex(vstArea.GetFirstSelected));
   vstRegionsChange(vstRegions, vstRegions.GetFirstSelected);
  end;
end;

procedure TfrmRegionControl.btnSaveClick(Sender: TObject);
var
  packet: TPacket;
  stream: TEnhancedMemoryStream;
  groupCount,areaCount: Byte;
  i, j: Integer;
  node: PVirtualNode;
  groupInfo: PRegionInfo;
begin
  packet := TPacket.Create($03, 0);
  stream := packet.Stream;
  stream.Position := stream.Size;
  stream.WriteByte($09);

  groupCount := Min(vstRegions.RootNodeCount, 255);
  stream.WriteByte(groupCount);
  if groupCount = 0 then Exit;

  i := 0;
  node := vstRegions.GetFirst;
  while (node <> nil) and (i < groupCount) do
  begin
    groupInfo := vstRegions.GetNodeData(node);
    stream.WriteStringNull(groupInfo^.Name);
    areaCount:=Min(groupInfo^.Areas.Count,255);
    stream.WriteByte(areaCount);
    for j := 0 to areaCount-1 do
      with groupInfo^.Areas.Rects[j] do
      begin
        stream.WriteWord(Min(Left, Right));
        stream.WriteWord(Min(Top,  Bottom));
        stream.WriteWord(Max(Left, Right));
        stream.WriteWord(Max(Top,  Bottom));
      end;
    node := vstRegions.GetNext(node);
    Inc(i);
  end;
  dmNetwork.Send(TCompressedPacket.Create(packet));
  Close;
end;

procedure TfrmRegionControl.acAddGroup(Sender: TObject);
var
  node : PVirtualNode;
  infoGroup : PRegionInfo;
begin
  node := vstRegions.AddChild(nil);
  infoGroup := vstRegions.GetNodeData(node);
  infoGroup^.Name := 'Unnamed';
  infoGroup^.Areas := TRectList.Create;
end;

procedure TfrmRegionControl.accRemoveGroup(Sender: TObject);
begin
  vstRegions.DeleteSelectedNodes;
  vstRegionsChange(vstRegions, nil);
end;

procedure TfrmRegionControl.btnAddAreaClick(Sender: TObject);
var
  node, selected: PVirtualNode;
  areaInfo: ^TRect;
  regionInfo: PRegionInfo;
begin
  selected := vstRegions.GetFirstSelected;
  if selected <> nil then
  begin
    regionInfo := vstRegions.GetNodeData(selected);
    node := vstArea.AddChild(nil);
    areaInfo := vstArea.GetNodeData(node);
    areaInfo^.Left := 0;
    areaInfo^.Top := 0;
    areaInfo^.Right := 0;
    areaInfo^.Bottom := 0;
    regionInfo^.Areas.Add(0, 0, 0, 0);
    vstArea.ClearSelection;
    vstArea.Selected[node] := True;
    vstArea.FocusedNode := node;
  end;
end;

procedure TfrmRegionControl.btnClearAreaClick(Sender: TObject);
var
  regionNode: PVirtualNode;
  regionInfo: PRegionInfo;
  i: Integer;
begin
  regionNode := vstRegions.GetFirstSelected;
  if regionNode <> nil then
  begin
    regionInfo := vstRegions.GetNodeData(regionNode);
    regionInfo^.Areas.Clear;
    vstRegionsChange(vstRegions, vstRegions.GetFirstSelected);
  end;
end;

procedure TfrmRegionControl.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmRegionControl.FormDestroy(Sender: TObject);
begin
  frmRadarMap.Dependencies.Remove(pbArea);
  if AdminPacketHandlers[$0A] <> nil then FreeAndNil(AdminPacketHandlers[$0A]);
end;

procedure TfrmRegionControl.FormShow(Sender: TObject);
begin
  SetWindowParent(Handle, frmMain.Handle);
  dmNetwork.Send(TRequestRegionListPacket.Create);
end;

procedure TfrmRegionControl.pbAreaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  areaNode, regionNode, match: PVirtualNode;
  areaInfo: ^TRect;
  p: TPoint;
  i: Integer;
  regionInfo: PRegionInfo;
begin
  FAreaMove := [];
  p := Point(X * 8, Y * 8);
  match := nil;
  areaNode := vstArea.GetFirst;
  while areaNode <> nil do //find the last matching area
  begin
    areaInfo := vstArea.GetNodeData(areaNode);
    if PtInRect(areaInfo^, p) then
      match := areaNode;
    areaNode := vstArea.GetNext(areaNode);
  end;
  if match <> nil then
  begin
    areaInfo := vstArea.GetNodeData(match);
    if p.x - areaInfo^.Left <= 64 then Include(FAreaMove, amLeft);
    if p.y - areaInfo^.Top <= 64 then Include(FAreaMove, amTop);
    if areaInfo^.Right - p.x <= 64 then Include(FAreaMove, amRight);
    if areaInfo^.Bottom - p.y <= 64 then Include(FAreaMove, amBottom);
    if FAreaMove = [] then
      FAreaMove := [amLeft, amTop, amRight, amBottom];
  end else
  begin
    regionNode := vstRegions.GetFirstSelected;
    if regionNode <> nil then
    begin
      regionInfo := vstRegions.GetNodeData(regionNode);
      match := vstArea.AddChild(nil);
      areaInfo := vstArea.GetNodeData(match);
      areaInfo^.Left := p.x;
      areaInfo^.Top := p.y;
      areaInfo^.Right := p.x;
      areaInfo^.Bottom := p.y;
      regionInfo^.Areas.Add(p.x, p.y, p.x, p.y);

      pbArea.Repaint;

      FAreaMove := [amRight, amBottom];
    end;
  end;
  vstArea.ClearSelection;
  vstArea.Selected[match] := True;
  FLastX := X;
  FLastY := Y;
end;

procedure TfrmRegionControl.pbAreaMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  offsetX, offsetY: Integer;
begin
  if (ssLeft in Shift) and (vstArea.GetFirstSelected <> nil) then
  begin
    offsetX := (X - FLastX) * 8;
    offsetY := (Y - FLastY) * 8;
    if amLeft in FAreaMove then seX1.Value := seX1.Value + offsetX;
    if amRight in FAreaMove then seX2.Value := seX2.Value + offsetX;
    if amTop in FAreaMove then seY1.Value := seY1.Value + offsetY;
    if amBottom in FAreaMove then seY2.Value := seY2.Value + offsetY;
    FLastX := X;
    FLastY := Y;
    seX1Change(nil);
  end;
end;

procedure TfrmRegionControl.pbAreaPaint(Sender: TObject);
var
  i: Integer;
  node: PVirtualNode;
  areaInfo: ^TRect;
begin
  DisplayImage(pbArea.Canvas, 0, 0, frmRadarMap.Radar);
  pbArea.Canvas.Pen.Color := clRed;
  pbArea.Canvas.Brush.Color := clMaroon;
  pbArea.Canvas.Brush.Style := bsFDiagonal;
  node := vstArea.GetFirst;
  while node <> nil do
  begin
    if vstArea.Selected[node] then
    begin
      pbArea.Canvas.Pen.Width := 2;
      pbArea.Canvas.Pen.Style := psSolid;
    end else
    begin
      pbArea.Canvas.Pen.Width := 1;
      pbArea.Canvas.Pen.Style := psDot;
    end;
    areaInfo := vstArea.GetNodeData(node);
    pbArea.Canvas.Rectangle(areaInfo^.Left div 8, areaInfo^.Top div 8,
      areaInfo^.Right div 8 + 1, areaInfo^.Bottom div 8 + 1);
    node := vstArea.GetNext(node);
  end;
end;

procedure TfrmRegionControl.seX1Change(Sender: TObject);
var
  node: PVirtualNode;
  areaInfo: ^TRect;
  regionInfo: PRegionInfo;
begin
  node := vstArea.GetFirstSelected;
  if node <> nil then
  begin
    areaInfo := vstArea.GetNodeData(node);
    areaInfo^.Left := seX1.Value;
    areaInfo^.Right := seX2.Value;
    areaInfo^.Top := seY1.Value;
    areaInfo^.Bottom := seY2.Value;
    regionInfo:= vstRegions.GetNodeData(vstRegions.GetFirstSelected);
    regionInfo^.Areas.Rects[vstArea.AbsoluteIndex(node)] := areaInfo^;
    vstArea.InvalidateNode(node);
    pbArea.Repaint;
  end;
end;

procedure TfrmRegionControl.vstAreaChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  areaInfo: ^TRect;
  selected: Boolean;
begin
  selected := (Node <> nil) and Sender.Selected[Node];
  btnDeleteArea.Enabled := selected;
  lblX.Enabled := selected;
  lblY.Enabled := selected;
  seX1.Enabled := selected;
  seX2.Enabled := selected;
  seY1.Enabled := selected;
  seY2.Enabled := selected;
  if selected then
  begin
    areaInfo := Sender.GetNodeData(Node);
    seX1.Value := areaInfo^.Left;
    seX2.Value := areaInfo^.Right;
    seY1.Value := areaInfo^.Top;
    seY2.Value := areaInfo^.Bottom;
  end;
  pbArea.Repaint;
end;

procedure TfrmRegionControl.vstAreaGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  areaInfo: ^TRect;
begin
  areaInfo := Sender.GetNodeData(Node);
  CellText := Format('(%d, %d), (%d, %d)', [areaInfo^.Left, areaInfo^.Top,
    areaInfo^.Right, areaInfo^.Bottom]);
end;

procedure TfrmRegionControl.vstRegionsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  i: Integer;
  areaNode: PVirtualNode;
  regionInfo: PRegionInfo;
  areaInfo: ^TRect;
begin
  vstArea.BeginUpdate;
  vstArea.Clear;
  if Node <> nil then
  begin
    regionInfo := Sender.GetNodeData(Node);
    for i := 0 to regionInfo^.Areas.Count - 1 do
    begin
      areaNode := vstArea.AddChild(nil);
      areaInfo := vstArea.GetNodeData(areaNode);
      with regionInfo^.Areas.Rects[i] do
      begin
        areaInfo^.Left   := Left;
        areaInfo^.Top    := Top;
        areaInfo^.Right  := Right;
        areaInfo^.Bottom := Bottom;
      end;
    end;
  end;
  vstArea.EndUpdate;
  pbArea.Repaint;
end;

procedure TfrmRegionControl.vstRegionsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  regionInfo: PRegionInfo;
begin
  regionInfo := Sender.GetNodeData(Node);
  if regionInfo^.Areas <> nil then FreeAndNil(regionInfo^.Areas);
end;

procedure TfrmRegionControl.vstRegionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  regionInfo: PRegionInfo;
begin
  regionInfo := Sender.GetNodeData(Node);
  CellText := regionInfo^.Name;
end;

procedure TfrmRegionControl.vstRegionsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: WideString);
var
  regionInfo: PRegionInfo;
begin
  if (Node <> nil) then begin
    regionInfo := Sender.GetNodeData(Node);
    regionInfo^.Name := NewText;
  end;
end;

procedure TfrmRegionControl.vstRegionsOnEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

initialization
  {$I UfrmRegionControl.lrs}

end.
