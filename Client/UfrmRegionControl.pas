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
  Classes, SysUtils, math, LResources, Forms, Controls, Graphics, Dialogs,
  VirtualTrees, ExtCtrls, ImagingComponents, StdCtrls, Buttons, Spin, LCLIntf,
  UEnhancedMemoryStream, Menus, URectList, UEnums;

type
  TAreaMoveType = (amLeft, amTop, amRight, amBottom);
  TAreaMove = set of TAreaMoveType;

  PRegionInfo = ^TRegionInfo;
  TRegionInfo = record
    Name: string;
    Areas: TRectList;
  end;

  TRegionModifiedEvent = procedure(ARegionInfo: TRegionInfo) of object;
  TRegionDeletedEvent = procedure(ARegionName: string) of object;
  TRegionListEvent = procedure of object;

  { TfrmRegionControl }

  TfrmRegionControl = class(TForm)
    btnAddArea: TSpeedButton;
    btnAddRegion: TSpeedButton;
    btnClearArea: TSpeedButton;
    btnDeleteArea: TSpeedButton;
    btnClose: TButton;
    btnDeleteRegion: TSpeedButton;

    btnSave: TButton;
    Label1: TLabel;
    lblX: TLabel;
    lblY: TLabel;
    mnuAddRegion: TMenuItem;
    mnuDeleteRegion: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pbArea: TPaintBox;
    pnlAreaControls: TPanel;
    pmRegions: TPopupMenu;
    sbArea: TScrollBox;
    seX1: TSpinEdit;
    seX2: TSpinEdit;
    seY1: TSpinEdit;
    seY2: TSpinEdit;
    spRegionsArea: TSplitter;
    vstArea: TVirtualStringTree;
    vstRegions: TVirtualStringTree;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure mnuAddRegionClick(Sender: TObject);
    procedure mnuDeleteRegionClick(Sender: TObject);
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
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: UTF8String);
    procedure vstRegionsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstRegionsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstRegionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: UTF8String);
  protected
    FLastX: Integer;
    FLastY: Integer;
    FAreaMove: TAreaMove;
    FTempRegionNode: PVirtualNode;
    FOnRegionModified: TRegionModifiedEvent;
    FOnRegionDeleted: TRegionDeletedEvent;
    FOnRegionList: TRegionListEvent;
    function FindRegion(AName: string): PVirtualNode;
    procedure CheckUnsaved;
    procedure OnModifyRegionPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnDeleteRegionPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnListRegionsPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnAccessChanged(AAccessLevel: TAccessLevel);
  public
    property OnRegionModified: TRegionModifiedEvent read FOnRegionModified write FOnRegionModified;
    property OnRegionDeleted: TRegionDeletedEvent read FOnRegionDeleted write FOnRegionDeleted;
    property OnRegionList: TRegionListEvent read FOnRegionList write FOnRegionList;
  end;

var
  frmRegionControl: TfrmRegionControl;

implementation

uses
  UGameResources, UfrmRadar, UfrmMain, UdmNetwork, UPacket, UGUIPlatformUtils,
  UAdminHandling, UPacketHandlers;

type
  { TModifyRegionPacket }

  TModifyRegionPacket = class(TPacket)
    constructor Create(ARegionInfo: TRegionInfo);
  end;

  { TDeleteRegionPacket }

  TDeleteRegionPacket = class(TPacket)
    constructor Create(AName: string);
  end;

  { TRequestRegionListPacket }

  TRequestRegionListPacket = class(TPacket)
    constructor Create;
  end;

{ TModifyRegionPacket }

constructor TModifyRegionPacket.Create(ARegionInfo: TRegionInfo);
var
  i: Integer;
  count: Byte;
  area: TRect;
begin
  inherited Create($03, 0); //Admin Packet
  FStream.WriteByte($08); //Admin PacketID
  FStream.WriteStringNull(ARegionInfo.Name);
  count := Min(ARegionInfo.Areas.Count, 256);
  FStream.WriteByte(count);
  for i := 0 to count - 1 do
  begin
    area := ARegionInfo.Areas.Rects[i];
    FStream.WriteWord(area.Left);
    FStream.WriteWord(area.Top);
    FStream.WriteWord(area.Right);
    FStream.WriteWord(area.Bottom);
  end;
end;

{ TDeleteRegionPacket }

constructor TDeleteRegionPacket.Create(AName: string);
begin
  inherited Create($03, 0); //Admin Packet
  FStream.WriteByte($09); //Admin PacketID
  FStream.WriteStringNull(AName);
end;

{ TRequestRegionListPacket }

constructor TRequestRegionListPacket.Create;
begin
  inherited Create($03, 0); //Admin Packet
  FStream.WriteByte($0A); //Admin PacketID
end;

{ TfrmRegionControl }

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

  FTempRegionNode := nil;
  
  frmRadarMap.Dependencies.Add(pbArea);
  frmMain.RegisterAccessChangedListener(@OnAccessChanged);

  AssignAdminPacketHandler($08, TPacketHandler.Create(0, @OnModifyRegionPacket));
  AssignAdminPacketHandler($09, TPacketHandler.Create(0, @OnDeleteRegionPacket));
  AssignAdminPacketHandler($0A, TPacketHandler.Create(0, @OnListRegionsPacket));

  dmNetwork.Send(TRequestRegionListPacket.Create);
end;

procedure TfrmRegionControl.FormDestroy(Sender: TObject);
begin
  frmRadarMap.Dependencies.Remove(pbArea);
  if AdminPacketHandlers[$08] <> nil then FreeAndNil(AdminPacketHandlers[$08]);
  if AdminPacketHandlers[$09] <> nil then FreeAndNil(AdminPacketHandlers[$09]);
  if AdminPacketHandlers[$0A] <> nil then FreeAndNil(AdminPacketHandlers[$0A]);
end;

procedure TfrmRegionControl.FormShow(Sender: TObject);
begin
  SetWindowParent(Handle, frmMain.Handle);
  btnSave.Enabled := False; //no changes yet
end;

procedure TfrmRegionControl.btnSaveClick(Sender: TObject);
var
  regionNode: PVirtualNode;
  regionInfo: PRegionInfo;
  areaNode: PVirtualNode;
  areaInfo: PRect;
begin
  btnSave.Enabled := False;

  //Refresh the current region
  if FTempRegionNode <> nil then
    regionNode := FTempRegionNode
  else
    regionNode := vstRegions.GetFirstSelected;
  if regionNode <> nil then
  begin
    regionInfo := vstRegions.GetNodeData(regionNode);
    regionInfo^.Areas.Clear;
    areaNode := vstArea.GetFirst;
    while areaNode <> nil do
    begin
      areaInfo := vstArea.GetNodeData(areaNode);
      regionInfo^.Areas.Add(areaInfo^.Left, areaInfo^.Top, areaInfo^.Right,
        areaInfo^.Bottom);
      areaNode := vstArea.GetNext(areaNode);
    end;

    //Send the modified values
    dmNetwork.Send(TModifyRegionPacket.Create(regionInfo^));
  end;

  //Clear the selection
  vstRegions.ClearSelection;

  FTempRegionNode := nil;
end;

procedure TfrmRegionControl.mnuAddRegionClick(Sender: TObject);
var
  regionName: string;
  regionInfo: PRegionInfo;
begin
  regionName := '';
  if InputQuery('New Region', 'Enter the name for the new region:', regionName) then
  begin
    CheckUnsaved;

    if FindRegion(regionName) = nil then
    begin
      FTempRegionNode := vstRegions.AddChild(nil);
      regionInfo := vstRegions.GetNodeData(FTempRegionNode);
      regionInfo^.Name := regionName;
      regionInfo^.Areas := TRectList.Create;
      vstRegions.ClearSelection;
      vstRegions.Selected[FTempRegionNode] := True;
      btnSave.Enabled := True;
    end else
    begin
      MessageDlg('New Region', 'The region could not be added. A region with ' +
        'that name already exists.', mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmRegionControl.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CheckUnsaved;
end;

procedure TfrmRegionControl.mnuDeleteRegionClick(Sender: TObject);
var
  regionNode: PVirtualNode;
  regionInfo: PRegionInfo;
begin
  regionNode := vstRegions.GetFirstSelected;
  if (regionNode <> nil) and (MessageDlg('Delete Region', 'Are you sure, you ' +
    'want to delete the selected region?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    regionInfo := vstRegions.GetNodeData(regionNode);
    dmNetwork.Send(TDeleteRegionPacket.Create(regionInfo^.Name));
    vstRegions.Selected[regionNode] := False;
  end;
end;

procedure TfrmRegionControl.btnAddAreaClick(Sender: TObject);
var
  node: PVirtualNode;
  areaInfo: PRect;
begin
  node := vstArea.AddChild(nil);
  areaInfo := vstArea.GetNodeData(node);
  areaInfo^.Left := 0;
  areaInfo^.Top := 0;
  areaInfo^.Right := 0;
  areaInfo^.Bottom := 0;
  vstArea.ClearSelection;
  vstArea.Selected[node] := True;
  vstArea.FocusedNode := node;

  btnSave.Enabled := True; //possible change to be saved
end;

procedure TfrmRegionControl.btnClearAreaClick(Sender: TObject);
begin
  vstArea.Clear;
  vstAreaChange(vstArea, nil);
end;

procedure TfrmRegionControl.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmRegionControl.btnDeleteAreaClick(Sender: TObject);
begin
  vstArea.DeleteSelectedNodes;
  vstAreaChange(vstArea, nil);

  btnSave.Enabled := True; //possible change to be saved
end;

procedure TfrmRegionControl.pbAreaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  areaNode, match: PVirtualNode;
  areaInfo: PRect;
  p: TPoint;
begin
  if vstRegions.GetFirstSelected = nil then Exit;

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
    match := vstArea.AddChild(nil);
    areaInfo := vstArea.GetNodeData(match);
    areaInfo^.Left := p.x;
    areaInfo^.Top := p.y;
    areaInfo^.Right := p.x;
    areaInfo^.Bottom := p.y;
    pbArea.Repaint;
    FAreaMove := [amRight, amBottom];
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
  node: PVirtualNode;
  areaInfo: PRect;
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
  areaInfo: PRect;
begin
  node := vstArea.GetFirstSelected;
  if node <> nil then
  begin
    areaInfo := vstArea.GetNodeData(node);
    areaInfo^.Left := seX1.Value;
    areaInfo^.Right := seX2.Value;
    areaInfo^.Top := seY1.Value;
    areaInfo^.Bottom := seY2.Value;
    vstArea.InvalidateNode(node);
    pbArea.Repaint;

    btnSave.Enabled := True; //possible change to be saved
  end;
end;

procedure TfrmRegionControl.vstAreaChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  areaInfo: PRect;
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
  var CellText: UTF8String);
var
  areaInfo: PRect;
begin
  areaInfo := Sender.GetNodeData(Node);
  CellText := Format('(%d, %d), (%d, %d)', [areaInfo^.Left, areaInfo^.Top,
    areaInfo^.Right, areaInfo^.Bottom]);
end;

procedure TfrmRegionControl.vstRegionsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  i: Integer;
  selected, areaNode: PVirtualNode;
  regionInfo: PRegionInfo;
  areaInfo: PRect;
begin
  CheckUnsaved;

  vstArea.BeginUpdate;
  vstArea.Clear;
  selected := Sender.GetFirstSelected;
  if selected <> nil then
  begin
    btnAddArea.Enabled := True;
    btnClearArea.Enabled := True;
    mnuDeleteRegion.Enabled := (selected <> FTempRegionNode);
    btnDeleteRegion.Enabled := (selected <> FTempRegionNode);

    regionInfo := Sender.GetNodeData(selected);
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
  end else
  begin
    btnAddArea.Enabled := False;
    btnDeleteArea.Enabled := False;
    btnClearArea.Enabled := False;
    mnuDeleteRegion.Enabled := False;
    btnDeleteRegion.Enabled := False;
  end;
  vstArea.EndUpdate;
  pbArea.Repaint;

  btnSave.Enabled := False; //no changes to be saved
end;

procedure TfrmRegionControl.vstRegionsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  regionInfo: PRegionInfo;
begin
  regionInfo := Sender.GetNodeData(Node);
  regionInfo^.Name := '';
  if regionInfo^.Areas <> nil then FreeAndNil(regionInfo^.Areas);
end;

procedure TfrmRegionControl.vstRegionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: UTF8String);
var
  regionInfo: PRegionInfo;
begin
  regionInfo := Sender.GetNodeData(Node);
  CellText := UTF8Encode(regionInfo^.Name);
end;

function TfrmRegionControl.FindRegion(AName: string): PVirtualNode;
var
  regionInfo: PRegionInfo;
  found: Boolean;
begin
  found := False;
  Result := vstRegions.GetFirst;
  while (Result <> nil) and (not found) do
  begin
    regionInfo := vstRegions.GetNodeData(Result);
    if regionInfo^.Name = AName then
      found := True
    else
      Result := vstRegions.GetNext(Result);
  end;
end;

procedure TfrmRegionControl.CheckUnsaved;
begin
  if btnSave.Enabled then
  begin
    if MessageDlg('Unsaved changes', 'There are unsaved ' +
      'changes.' + #13#10+#13#10+ 'Do you want to save them now?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      btnSaveClick(nil);
    end else if FTempRegionNode <> nil then
    begin
      btnSave.Enabled := False;
      vstRegions.DeleteNode(FTempRegionNode);
      FTempRegionNode := nil;
    end;
  end;
end;

procedure TfrmRegionControl.OnModifyRegionPacket(ABuffer: TEnhancedMemoryStream);
var
  regionName: string;
  regionNode: PVirtualNode;
  regionInfo: PRegionInfo;
  areaCount: Byte;
  i: Integer;
  x1, y1, x2, y2: Word;
begin
  ABuffer.ReadByte; //status, not used yet

  //TODO : Ask user how to proceed, if the added/modified packet conflicts with the currently edited region

  regionName := ABuffer.ReadStringNull;
  regionNode := FindRegion(regionName);
  if regionNode = nil then
  begin
    regionNode := vstRegions.AddChild(nil);
    regionInfo := vstRegions.GetNodeData(regionNode);
    regionInfo^.Name := regionName;
    regionInfo^.Areas := TRectList.Create;
  end else
  begin
    regionInfo := vstRegions.GetNodeData(regionNode);
    regionInfo^.Areas.Clear;
  end;

  areaCount := ABuffer.ReadByte;
  for i := 0 to areaCount - 1 do
  begin
    x1 := ABuffer.ReadWord;
    y1 := ABuffer.ReadWord;
    x2 := ABuffer.ReadWord;
    y2 := ABuffer.ReadWord;
    regionInfo^.Areas.Add(x1, y1, x2, y2);
  end;

  if vstRegions.Selected[regionNode] then
  begin
    btnSave.Enabled := False;
    vstRegionsChange(vstRegions, regionNode);
  end;

  if Assigned(FOnRegionModified) then
    FOnRegionModified(regionInfo^);
end;

procedure TfrmRegionControl.OnDeleteRegionPacket(ABuffer: TEnhancedMemoryStream);
var
  regionName: string;
  regionNode: PVirtualNode;
begin
  ABuffer.ReadByte; //status, not used yet
  regionName := ABuffer.ReadStringNull;
  regionNode := FindRegion(regionName);

  //TODO : Ask user how to proceed, if the deleted packet conflicts with the currently edited region

  if regionNode <> nil then
    vstRegions.DeleteNode(regionNode);

  if Assigned(FOnRegionDeleted) then
    FOnRegionDeleted(regionName);
end;

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

  if Assigned(FOnRegionList) then
    FOnRegionList;
end;

procedure TfrmRegionControl.OnAccessChanged(AAccessLevel: TAccessLevel);
begin
  if AAccessLevel >= alAdministrator then
    dmNetwork.Send(TRequestRegionListPacket.Create);
end;

initialization
  {$I UfrmRegionControl.lrs}

end.
