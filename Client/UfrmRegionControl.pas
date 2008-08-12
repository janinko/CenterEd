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
  math, UPlatformTypes, UEnhancedMemoryStream, Menus,contnrs, UInterfaces,
  URectList;

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
    mnuAddGroup: TMenuItem;
    mnuRemoveGroup: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pbArea: TPaintBox;
    pnlAreaControls: TPanel;
    pmGroup: TPopupMenu;
    sbArea: TScrollBox;
    seX1: TSpinEdit;
    seX2: TSpinEdit;
    seY1: TSpinEdit;
    seY2: TSpinEdit;
    vstGroups: TVirtualStringTree;
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
    procedure vstGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstGroupsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: WideString);
    procedure vstGroupsOnEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
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
  vstGroups.BeginUpdate;
  vstGroups.Clear;
  regionCount := ABuffer.ReadByte;
  for i := 0 to regionCount - 1 do
  begin
   node := vstGroups.AddChild(nil);
   regionInfo := vstGroups.GetNodeData(node);
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
  vstGroups.EndUpdate;
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
  vstGroups.NodeDataSize := SizeOf(TRegionInfo);
  
  frmRadarMap.Dependencies.Add(pbArea);

  AdminPacketHandlers[$09] := TPacketHandler.Create(0, @OnListRegionsPacket);
end;

procedure TfrmRegionControl.btnDeleteAreaClick(Sender: TObject);
var
  infoGroup: PRegionInfo;
  i: Integer;
begin
  if vstGroups.GetFirstSelected <> nil then
  begin
   infoGroup := vstGroups.GetNodeData(vstGroups.GetFirstSelected);
   infoGroup^.Areas.Delete(vstArea.AbsoluteIndex(vstArea.GetFirstSelected));
   vstGroupsChange(vstGroups, vstGroups.GetFirstSelected);
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

  groupCount := Min(vstGroups.RootNodeCount, 255);
  stream.WriteByte(groupCount);
  if groupCount = 0 then Exit;

  i := 0;
  node := vstGroups.GetFirst;
  while (node <> nil) and (i < groupCount) do
  begin
    groupInfo := vstGroups.GetNodeData(node);
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
    node := vstGroups.GetNext(node);
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
  node := vstGroups.AddChild(nil);
  infoGroup := vstGroups.GetNodeData(node);
  infoGroup^.Name := 'Unnamed';
  infoGroup^.Areas := TRectList.Create;
end;

procedure TfrmRegionControl.accRemoveGroup(Sender: TObject);
begin
  vstGroups.DeleteSelectedNodes;
  vstGroupsChange(vstGroups, nil);
end;

procedure TfrmRegionControl.btnAddAreaClick(Sender: TObject);
var
  node: PVirtualNode;
  nodeInfo: ^TRect;
  infoGroup : PRegionInfo;
begin
   infoGroup:=vstGroups.GetNodeData(vstGroups.GetFirstSelected);
   node := vstArea.AddChild(nil);
   nodeInfo := vstArea.GetNodeData(node);
   nodeInfo^.Left := 0;
   nodeInfo^.Top := 0;
   nodeInfo^.Right := 0;
   nodeInfo^.Bottom := 0;
   infoGroup^.Areas.Add(0, 0, 0, 0);
   vstArea.ClearSelection;
   vstArea.Selected[node] := True;
   vstArea.FocusedNode := node;
end;

procedure TfrmRegionControl.btnClearAreaClick(Sender: TObject);
var
  infoGroup: PRegionInfo;
  infoArea : TRect;
  i: Integer;
begin
  if vstGroups.GetFirstSelected <> nil then
  begin
    infoGroup := vstGroups.GetNodeData(vstGroups.GetFirstSelected);
    infoGroup^.Areas.Clear;
    vstGroupsChange(vstGroups, vstGroups.GetFirstSelected);
  end;
end;

procedure TfrmRegionControl.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmRegionControl.FormDestroy(Sender: TObject);
begin
  frmRadarMap.Dependencies.Remove(pbArea);
  if AdminPacketHandlers[$09] <> nil then FreeAndNil(AdminPacketHandlers[$09]);
end;

procedure TfrmRegionControl.FormShow(Sender: TObject);
begin
  SetWindowParent(Handle, frmMain.Handle);
  dmNetwork.Send(TRequestRegionListPacket.Create);
end;

procedure TfrmRegionControl.pbAreaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  node, match: PVirtualNode;
  nodeInfo: ^TRect;
  p: TPoint;
  i: Integer;
  infoArea: TRect;
  infoGroup: PRegionInfo;
begin
  FAreaMove := [];
  p := Point(X * 8, Y * 8);
  match := nil;
  node := vstArea.GetFirst;
  while node <> nil do
  begin
    nodeInfo := vstArea.GetNodeData(node);
    if PtInRect(nodeInfo^, p) then
      match := node;
    node := vstArea.GetNext(node);
  end;
  if match <> nil then
  begin
    nodeInfo := vstArea.GetNodeData(match);
    if p.x - nodeInfo^.Left <= 64 then Include(FAreaMove, amLeft);
    if p.y - nodeInfo^.Top <= 64 then Include(FAreaMove, amTop);
    if nodeInfo^.Right - p.x <= 64 then Include(FAreaMove, amRight);
    if nodeInfo^.Bottom - p.y <= 64 then Include(FAreaMove, amBottom);
    if FAreaMove = [] then
      FAreaMove := [amLeft, amTop, amRight, amBottom];
  end else
  begin
   if vstGroups.GetFirstSelected <> nil then
   begin
     infoGroup:=vstGroups.GetNodeData(vstGroups.GetFirstSelected);
     match := vstArea.AddChild(nil);
     nodeInfo:=vstArea.GetNodeData(match);
     nodeInfo^.Left := p.x;
     nodeInfo^.Top := p.y;
     nodeInfo^.Right := p.x;
     nodeInfo^.Bottom := p.y;
     infoGroup^.Areas.Add(p.x, p.y, p.x, p.y);

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
  nodeInfo: ^TRect;
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
    nodeInfo := vstArea.GetNodeData(node);
    pbArea.Canvas.Rectangle(nodeInfo^.Left div 8, nodeInfo^.Top div 8,
      nodeInfo^.Right div 8 + 1, nodeInfo^.Bottom div 8 + 1);
    node := vstArea.GetNext(node);
  end;
end;

procedure TfrmRegionControl.seX1Change(Sender: TObject);
var
  node: PVirtualNode;
  nodeInfo: ^TRect;
  infoGroup: PRegionInfo;
begin
  node := vstArea.GetFirstSelected;
  if node <> nil then
  begin
    nodeInfo := vstArea.GetNodeData(node);
    nodeInfo^.Left := seX1.Value;
    nodeInfo^.Right := seX2.Value;
    nodeInfo^.Top := seY1.Value;
    nodeInfo^.Bottom := seY2.Value;
    infoGroup:= vstGroups.GetNodeData(vstGroups.GetFirstSelected);
    infoGroup^.Areas.Rects[vstArea.AbsoluteIndex(node)] := nodeinfo^;
    vstArea.InvalidateNode(node);
    pbArea.Repaint;
  end;
end;

procedure TfrmRegionControl.vstAreaChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  nodeInfo: ^TRect;
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
    nodeInfo := Sender.GetNodeData(Node);
    seX1.Value := nodeInfo^.Left;
    seX2.Value := nodeInfo^.Right;
    seY1.Value := nodeInfo^.Top;
    seY2.Value := nodeInfo^.Bottom;
  end;
end;

procedure TfrmRegionControl.vstAreaGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  nodeInfo: ^TRect;
begin
  nodeInfo := Sender.GetNodeData(Node);
  CellText := Format('(%d, %d), (%d, %d)', [nodeInfo^.Left, nodeInfo^.Top,
    nodeInfo^.Right, nodeInfo^.Bottom]);
end;

procedure TfrmRegionControl.vstGroupsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  i: Integer;
  nodeArea: PVirtualNode;
  infoGroup: PRegionInfo;
  infoArea: ^TRect;
  Area: ^TRect;
begin
  vstArea.BeginUpdate;
  vstArea.Clear;
  if Node <> nil then
  begin {TODO : code style!!!!}
    infoGroup:=Sender.GetNodeData(Node);
    for i:=0 to infoGroup^.Areas.Count-1 do
    begin
      nodeArea := vstArea.AddChild(nil);
      infoArea := vstArea.GetNodeData(nodeArea);
      Area := infoGroup^.Areas[i];
      infoArea^.Left := Area^.Left;
      infoArea^.Top := Area^.Top;
      infoArea^.Right := Area^.Right;
      infoArea^.Bottom := Area^.Bottom;
    end;
  end;
  vstArea.EndUpdate;
  pbArea.Repaint;
end;

procedure TfrmRegionControl.vstGroupsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  nodeInfo: PRegionInfo;
begin
  nodeInfo := Sender.GetNodeData(Node);
  CellText := nodeInfo^.Name;
end;

procedure TfrmRegionControl.vstGroupsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: WideString);
var
  nodeInfo: PRegionInfo;
begin
  if (Node <> nil) then begin
    nodeInfo := Sender.GetNodeData(Node);
    nodeInfo^.Name := NewText;
  end;
end;

procedure TfrmRegionControl.vstGroupsOnEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;


initialization
  {$I UfrmRegionControl.lrs}

end.
