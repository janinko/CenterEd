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
unit UfrmLargeScaleCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CheckLst,
  VirtualTrees, ExtCtrls, ImagingComponents, StdCtrls, Buttons, Spin, LCLIntf,
  math, UPlatformTypes, UEnhancedMemoryStream;

type

  TAreaMoveType = (amLeft, amTop, amRight, amBottom);
  TAreaMove = set of TAreaMoveType;

  { TfrmLargeScaleCommand }

  TfrmLargeScaleCommand = class(TForm)
    btnClearTerrain: TSpeedButton;
    btnClearIStaticsTiles: TSpeedButton;
    btnClearDStaticsTiles: TSpeedButton;
    btnDeleteTerrain: TSpeedButton;
    btnDeleteIStaticsTiles: TSpeedButton;
    btnDeleteDStaticsTiles: TSpeedButton;
    btnExecute: TButton;
    btnClose: TButton;
    cbCMEraseTarget: TCheckBox;
    gbDrawTerrainTiles: TGroupBox;
    gbDeleteStaticsTiles: TGroupBox;
    gbInserStaticsTiles: TGroupBox;
    gbStaticsProbability: TGroupBox;
    gbStaticsPlacement: TGroupBox;
    GroupBox1: TGroupBox;
    gbCMOffset: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblDrawTerrainTilesDesc: TLabel;
    lblDeleteStaticsTilesDesc: TLabel;
    lblInsertStaticsTiles: TLabel;
    lblX: TLabel;
    lblY: TLabel;
    nbActions: TNotebook;
    pgCopyMove: TPage;
    pgDeleteStatics: TPage;
    pgInsertStatics: TPage;
    pgModifyAltitude: TPage;
    pnlControls: TPanel;
    pnlDrawTerrainTilesControls: TPanel;
    pnlAreaControls: TPanel;
    pnlDrawTerrainTilesControls1: TPanel;
    pnlDrawTerrainTilesControls2: TPanel;
    pnlLeft: TPanel;
    pbArea: TPaintBox;
    pgArea: TPage;
    pgDrawTerrain: TPage;
    rgCMAction: TRadioGroup;
    rbPlaceStaticsOnTerrain: TRadioButton;
    rbPlaceStaticsOnTop: TRadioButton;
    rbPlaceStaticsOnZ: TRadioButton;
    rbSetTerrainAltitude: TRadioButton;
    rbRelativeAltitudeChange: TRadioButton;
    sbArea: TScrollBox;
    btnAddArea: TSpeedButton;
    btnDeleteArea: TSpeedButton;
    seDeleteStaticsZ1: TSpinEdit;
    seDeleteStaticsZ2: TSpinEdit;
    seX1: TSpinEdit;
    seX2: TSpinEdit;
    seY1: TSpinEdit;
    seY2: TSpinEdit;
    btnClearArea: TSpeedButton;
    seTerrainAltitude1: TSpinEdit;
    seTerrainAltitude2: TSpinEdit;
    seRelativeAltitude: TSpinEdit;
    seStaticsProbability: TSpinEdit;
    seInsertStaticsZ: TSpinEdit;
    seCMOffsetX: TSpinEdit;
    seCMOffsetY: TSpinEdit;
    vdtTerrainTiles: TVirtualDrawTree;
    vdtInsertStaticsTiles: TVirtualDrawTree;
    vdtDeleteStaticsTiles: TVirtualDrawTree;
    vstArea: TVirtualStringTree;
    vstActions: TVirtualStringTree;
    procedure FormShow(Sender: TObject);
    procedure btnAddAreaClick(Sender: TObject);
    procedure btnClearDStaticsTilesClick(Sender: TObject);
    procedure btnClearIStaticsTilesClick(Sender: TObject);
    procedure btnClearTerrainClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDeleteDStaticsTilesClick(Sender: TObject);
    procedure btnDeleteIStaticsTilesClick(Sender: TObject);
    procedure btnDeleteTerrainClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbAreaMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure pbAreaPaint(Sender: TObject);
    procedure btnDeleteAreaClick(Sender: TObject);
    procedure btnClearAreaClick(Sender: TObject);
    procedure seX1Change(Sender: TObject);
    procedure vdtTerrainTilesDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vdtTerrainTilesDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure vdtTerrainTilesDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure vstActionsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstActionsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure vstActionsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vstAreaChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstAreaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  protected
    FLastX: Integer;
    FLastY: Integer;
    FAreaMove: TAreaMove;
    procedure AddNode(AActionID: Integer; ACaption: string);
    function FindNode(AActionID: Integer): PVirtualNode;
    procedure SerializeTiles(ATileList: TVirtualDrawTree;
      AStream: TEnhancedMemoryStream);
  public
    { public declarations }
  end;

var
  frmLargeScaleCommand: TfrmLargeScaleCommand;

implementation

uses
  UGameResources, UfrmRadar, UfrmMain, UdmNetwork, UPacket, UPackets,
  UGUIPlatformUtils;

type
  PNodeInfo = ^TNodeInfo;
  TNodeInfo = record
    ActionID: Integer;
    Caption: string;
  end;
  PTileInfo = ^TTileInfo;
  TTileInfo = record
    ID: Word;
  end;

{ TfrmLargeScaleCommand }

procedure TfrmLargeScaleCommand.FormCreate(Sender: TObject);
begin
  vstActions.NodeDataSize := SizeOf(TNodeInfo);
  AddNode(-1, 'Target Area');
  AddNode(0, 'Copy/Move');
  AddNode(1, 'Modify altitude');
  AddNode(2, 'Draw terrain');
  AddNode(3, 'Delete statics');
  AddNode(4, 'Insert statics');
  vstActions.Selected[vstActions.GetFirst] := True;

  vstArea.NodeDataSize := SizeOf(TRect);

  pbArea.Width := frmRadarMap.Radar.Width;
  pbArea.Height := frmRadarMap.Radar.Height;
  seX1.MaxValue := ResMan.Landscape.CellWidth;
  seX2.MaxValue := ResMan.Landscape.CellWidth;
  seY1.MaxValue := ResMan.Landscape.CellHeight;
  seY2.MaxValue := ResMan.Landscape.CellHeight;

  vdtTerrainTiles.NodeDataSize := SizeOf(TTileInfo);
  vdtInsertStaticsTiles.NodeDataSize := SizeOf(TTileInfo);
  vdtDeleteStaticsTiles.NodeDataSize := SizeOf(TTileInfo);

  seCMOffsetX.MinValue := -ResMan.Landscape.CellWidth;
  seCMOffsetX.MaxValue := ResMan.Landscape.CellWidth;
  seCMOffsetY.MinValue := -ResMan.Landscape.CellHeight;
  seCMOffsetY.MaxValue := ResMan.Landscape.CellHeight;

  frmRadarMap.Dependencies.Add(pbArea);
end;

procedure TfrmLargeScaleCommand.FormDestroy(Sender: TObject);
begin
  frmRadarMap.Dependencies.Remove(pbArea);
end;

procedure TfrmLargeScaleCommand.pbAreaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  node, match: PVirtualNode;
  nodeInfo: ^TRect;
  p: TPoint;
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
    match := vstArea.AddChild(nil);
    nodeInfo := vstArea.GetNodeData(match);
    nodeInfo^.Left := p.x;
    nodeInfo^.Top := p.y;
    nodeInfo^.Right := p.x;
    nodeInfo^.Bottom := p.y;
    FAreaMove := [amRight, amBottom];
  end;
  vstArea.ClearSelection;
  vstArea.Selected[match] := True;
  FLastX := X;
  FLastY := Y;
end;

procedure TfrmLargeScaleCommand.pbAreaMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  node: PVirtualNode;
  nodeInfo: ^TRect;
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

procedure TfrmLargeScaleCommand.btnAddAreaClick(Sender: TObject);
var
  node: PVirtualNode;
  nodeInfo: ^TRect;
begin
  node := vstArea.AddChild(nil);
  nodeInfo := vstArea.GetNodeData(node);
  nodeInfo^.Left := 0;
  nodeInfo^.Top := 0;
  nodeInfo^.Right := 0;
  nodeInfo^.Bottom := 0;
  vstArea.ClearSelection;
  vstArea.Selected[node] := True;
  vstArea.FocusedNode := node;
end;

procedure TfrmLargeScaleCommand.FormShow(Sender: TObject);
begin
  SetWindowParent(Handle, frmMain.Handle);
end;

procedure TfrmLargeScaleCommand.btnClearDStaticsTilesClick(Sender: TObject);
begin
  vdtDeleteStaticsTiles.Clear;
end;

procedure TfrmLargeScaleCommand.btnClearIStaticsTilesClick(Sender: TObject);
begin
  vdtInsertStaticsTiles.Clear;
end;

procedure TfrmLargeScaleCommand.btnClearTerrainClick(Sender: TObject);
begin
  vdtTerrainTiles.Clear;
end;

procedure TfrmLargeScaleCommand.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmLargeScaleCommand.btnDeleteDStaticsTilesClick(Sender: TObject);
begin
  vdtDeleteStaticsTiles.DeleteSelectedNodes;
end;

procedure TfrmLargeScaleCommand.btnDeleteIStaticsTilesClick(Sender: TObject);
begin
  vdtInsertStaticsTiles.DeleteSelectedNodes;
end;

procedure TfrmLargeScaleCommand.btnDeleteTerrainClick(Sender: TObject);
begin
  vdtTerrainTiles.DeleteSelectedNodes;
end;

procedure TfrmLargeScaleCommand.btnExecuteClick(Sender: TObject);
var
  packet: TPacket;
  stream: TEnhancedMemoryStream;
  areaCount: Byte;
  i: Integer;
  node: PVirtualNode;
  areaInfo: ^TRect;
begin
  packet := TPacket.Create($0E, 0);
  stream := packet.Stream;
  stream.Position := stream.Size;

  //Area
  areaCount := Min(vstArea.RootNodeCount, 255);
  stream.WriteByte(areaCount);
  if areaCount = 0 then Exit;
  i := 0;
  node := vstArea.GetFirst;
  while (node <> nil) and (i < areaCount) do
  begin
    areaInfo := vstArea.GetNodeData(node);
    stream.WriteWord(Min(areaInfo^.Left, areaInfo^.Right));
    stream.WriteWord(Min(areaInfo^.Top, areaInfo^.Bottom));
    stream.WriteWord(Max(areaInfo^.Left, areaInfo^.Right));
    stream.WriteWord(Max(areaInfo^.Top, areaInfo^.Bottom));
    node := vstArea.GetNext(node);
    Inc(i);
  end;

  //Copy/Move
  node := FindNode(0);
  if vstActions.CheckState[node] = csCheckedNormal then
  begin
    stream.WriteBoolean(True);
    stream.WriteByte(rgCMAction.ItemIndex);
    stream.WriteInteger(seCMOffsetX.Value);
    stream.WriteInteger(seCMOffsetY.Value);
    stream.WriteBoolean(cbCMEraseTarget.Checked);
  end else
    stream.WriteBoolean(False);

  //Modify altitude
  node := FindNode(1);
  if vstActions.CheckState[node] = csCheckedNormal then
  begin
    stream.WriteBoolean(True);
    if rbSetTerrainAltitude.Checked then
    begin
      stream.WriteByte(1);
      stream.WriteShortInt(Min(seTerrainAltitude1.Value, seTerrainAltitude2.Value));
      stream.WriteShortInt(Max(seTerrainAltitude1.Value, seTerrainAltitude2.Value));
    end else
    begin
      stream.WriteByte(2);
      stream.WriteShortInt(seRelativeAltitude.Value);
    end;
  end else
    stream.WriteBoolean(False);

  //Draw terrain
  node := FindNode(2);
  if vstActions.CheckState[node] = csCheckedNormal then
  begin
    stream.WriteBoolean(True);
    SerializeTiles(vdtTerrainTiles, stream);
  end else
    stream.WriteBoolean(False);

  //Delete statics
  node := FindNode(3);
  if vstActions.CheckState[node] = csCheckedNormal then
  begin
    stream.WriteBoolean(True);
    SerializeTiles(vdtDeleteStaticsTiles, stream);
    stream.WriteShortInt(Min(seDeleteStaticsZ1.Value, seDeleteStaticsZ2.Value));
    stream.WriteShortInt(Max(seDeleteStaticsZ1.Value, seDeleteStaticsZ2.Value));
  end else
    stream.WriteBoolean(False);

  //Insert statics
  node := FindNode(4);
  if vstActions.CheckState[node] = csCheckedNormal then
  begin
    stream.WriteBoolean(True);
    SerializeTiles(vdtInsertStaticsTiles, stream);
    stream.WriteByte(seStaticsProbability.Value);
    if rbPlaceStaticsOnZ.Checked then
    begin
      stream.WriteByte(3);
      stream.WriteShortInt(seInsertStaticsZ.Value);
    end else if rbPlaceStaticsOnTerrain.Checked then
      stream.WriteByte(1)
    else
      stream.WriteByte(2);
  end else
    stream.WriteBoolean(False);

  dmNetwork.Send(TCompressedPacket.Create(packet));
  Close;
end;

procedure TfrmLargeScaleCommand.pbAreaPaint(Sender: TObject);
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
      //pbArea.Canvas.Brush.Color := clRed;
    end else
    begin
      pbArea.Canvas.Pen.Width := 1;
      pbArea.Canvas.Pen.Style := psDot;
      //pbArea.Canvas.Brush.Color := clMaroon;
    end;
    nodeInfo := vstArea.GetNodeData(node);
    pbArea.Canvas.Rectangle(nodeInfo^.Left div 8, nodeInfo^.Top div 8,
      nodeInfo^.Right div 8 + 1, nodeInfo^.Bottom div 8 + 1);
    node := vstArea.GetNext(node);
  end;
end;

procedure TfrmLargeScaleCommand.btnDeleteAreaClick(Sender: TObject);
begin
  vstArea.DeleteSelectedNodes;
  vstAreaChange(vstArea, nil);
end;

procedure TfrmLargeScaleCommand.btnClearAreaClick(Sender: TObject);
begin
  vstArea.Clear;
  vstAreaChange(vstArea, nil);
end;

procedure TfrmLargeScaleCommand.seX1Change(Sender: TObject);
var
  node: PVirtualNode;
  nodeInfo: ^TRect;
begin
  node := vstArea.GetFirstSelected;
  if node <> nil then
  begin
    nodeInfo := vstArea.GetNodeData(node);
    nodeInfo^.Left := seX1.Value;
    nodeInfo^.Right := seX2.Value;
    nodeInfo^.Top := seY1.Value;
    nodeInfo^.Bottom := seY2.Value;
    vstArea.InvalidateNode(node);
    pbArea.Repaint;
  end;
end;

procedure TfrmLargeScaleCommand.vdtTerrainTilesDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  sourceTree: TVirtualDrawTree;
  selected, node: PVirtualNode;
  sourceTileInfo, targetTileInfo: PTileInfo;
begin
  sourceTree := Source as TVirtualDrawTree;
  if (sourceTree <> Sender) and (sourceTree <> nil) and
    (sourceTree.Tag = 1) then
  begin
    Sender.BeginUpdate;
    selected := sourceTree.GetFirstSelected;
    while selected <> nil do
    begin
      sourceTileInfo := sourceTree.GetNodeData(selected);
      if ((Sender = vdtTerrainTiles) and (sourceTileInfo^.ID < $4000)) or
         ((Sender = vdtInsertStaticsTiles) and (sourceTileInfo^.ID > $3FFF)) or
         ((Sender = vdtDeleteStaticsTiles) and (sourceTileInfo^.ID > $3FFF)) then
      begin
        node := Sender.AddChild(nil);
        targetTileInfo := Sender.GetNodeData(node);
        targetTileInfo^.ID := sourceTileInfo^.ID;
      end;
      selected := sourceTree.GetNextSelected(selected);
    end;
    Sender.EndUpdate;
  end;
end;

procedure TfrmLargeScaleCommand.vdtTerrainTilesDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  if (Source <> Sender) and (Source is TVirtualDrawTree) and
    (TVirtualDrawTree(Source).Tag = 1) then
  begin
    Accept := True;
  end;
end;

procedure TfrmLargeScaleCommand.vdtTerrainTilesDrawNode(
  Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
begin
  frmMain.vdtTilesDrawNode(Sender, PaintInfo);
end;

procedure TfrmLargeScaleCommand.vstActionsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  nodeInfo: PNodeInfo;
begin
  if Sender.Selected[Node] then
  begin
    nodeInfo := Sender.GetNodeData(Node);
    nbActions.PageIndex := nodeInfo^.ActionID + 1;
  end;
end;

procedure TfrmLargeScaleCommand.vstActionsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
var
  nodeInfo: PNodeInfo;
begin
  nodeInfo := Sender.GetNodeData(Node);
  CellText := nodeInfo^.Caption;
end;

procedure TfrmLargeScaleCommand.vstActionsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if Sender.Selected[Node] then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
end;

procedure TfrmLargeScaleCommand.vstAreaChange(Sender: TBaseVirtualTree;
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
  pbArea.Repaint;
end;

procedure TfrmLargeScaleCommand.vstAreaGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  nodeInfo: ^TRect;
begin
  nodeInfo := Sender.GetNodeData(Node);
  CellText := Format('(%d, %d), (%d, %d)', [nodeInfo^.Left, nodeInfo^.Top,
    nodeInfo^.Right, nodeInfo^.Bottom]);
end;

procedure TfrmLargeScaleCommand.AddNode(AActionID: Integer; ACaption: string);
var
  node: PVirtualNode;
  nodeInfo: PNodeInfo;
begin
  node := vstActions.AddChild(nil);
  nodeInfo := vstActions.GetNodeData(node);
  nodeInfo^.ActionID := AActionID;
  nodeInfo^.Caption := ACaption;
  if AActionID > -1 then
    vstActions.CheckType[node] := ctCheckBox;
end;

function TfrmLargeScaleCommand.FindNode(AActionID: Integer): PVirtualNode;
var
  node: PVirtualNode;
  nodeInfo: PNodeInfo;
begin
  Result := nil;
  node := vstActions.GetFirst;
  while (node <> nil) and (Result = nil) do
  begin
    nodeInfo := vstActions.GetNodeData(node);
    if nodeInfo^.ActionID = AActionID then
      Result := node;
    node := vstActions.GetNext(node);
  end;
end;

procedure TfrmLargeScaleCommand.SerializeTiles(ATileList: TVirtualDrawTree;
  AStream: TEnhancedMemoryStream);
var
  node: PVirtualNode;
  tileInfo: PTileInfo;
begin
  AStream.WriteWord(ATileList.RootNodeCount);
  node := ATileList.GetFirst;
  while node <> nil do
  begin
    tileInfo := ATileList.GetNodeData(node);
    AStream.WriteWord(tileInfo^.ID);
    node := ATileList.GetNext(node);
  end;
end;

initialization
  {$I UfrmLargeScaleCommand.lrs}

end.

