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
unit UfrmFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VirtualTrees, LCLIntf, LMessages, Buttons, UPlatformTypes, UStatics,
  PairSplitter, Menus;

type

  { TfrmFilter }

  TfrmFilter = class(TForm)
    btnClear: TSpeedButton;
    btnDelete: TSpeedButton;
    btnRandomPresetDelete: TSpeedButton;
    btnRandomPresetSave: TSpeedButton;
    cbRandomPreset: TComboBox;
    cbTileFilter: TCheckBox;
    cbHueFilter: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    mnuUncheckHues: TMenuItem;
    mnuCheckHues: TMenuItem;
    pnlControls: TPanel;
    pnlRandomPreset: TPanel;
    pmHues: TPopupMenu;
    rgFilterType: TRadioGroup;
    Splitter1: TSplitter;
    vdtFilter: TVirtualDrawTree;
    vdtHues: TVirtualDrawTree;
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuUncheckHuesClick(Sender: TObject);
    procedure mnuCheckHuesClick(Sender: TObject);
    procedure vdtFilterDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vdtFilterDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure vdtFilterDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure vdtHuesChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vdtHuesDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
  protected
    FLocked: Boolean;
    FCheckedHues: TBits;
    procedure MouseLeave(var msg: TLMessage); message CM_MouseLeave;
  public
    property Locked: Boolean read FLocked write FLocked;
    function Filter(AStatic: TStaticItem): Boolean;
    procedure JumpToHue(AHueID: Word);
  end; 

var
  frmFilter: TfrmFilter;

implementation

uses
  UfrmMain, UGameResources, UHue, UGraphicHelper, UGUIPlatformUtils;
  
type
  PTileInfo = ^TTileInfo;
  TTileInfo = record
    ID: Word;
  end;
  PHueInfo = ^THueInfo;
  THueInfo = record
    ID: Word;
    Hue: THue;
  end;

{ TfrmFilter }

procedure TfrmFilter.FormShow(Sender: TObject);
var
  upperLeft, lowerLeft: TPoint;
begin
  upperLeft := frmMain.pcLeft.ClientToScreen(Point(frmMain.pcLeft.Width, 0));
  lowerLeft := frmMain.pcLeft.ClientToScreen(Point(frmMain.pcLeft.Width,
    frmMain.pcLeft.Height));
  Left := upperLeft.x;
  Top := upperLeft.y;
  Height := lowerLeft.y - upperLeft.y;

  SetWindowParent(Handle, frmMain.Handle);
end;

procedure TfrmFilter.mnuUncheckHuesClick(Sender: TObject);
begin
  vdtHues.ClearChecked;
end;

procedure TfrmFilter.mnuCheckHuesClick(Sender: TObject);
var
  node: PVirtualNode;
begin
  node := vdtHues.GetFirst;
  while node <> nil do
  begin
    vdtHues.CheckState[node] := csCheckedNormal;
    node := vdtHues.GetNext(node);
  end;
end;

procedure TfrmFilter.vdtFilterDragDrop(Sender: TBaseVirtualTree;
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
      if sourceTileInfo^.ID > $3FFF then
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

procedure TfrmFilter.vdtFilterDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  if (Source <> Sender) and (Source is TVirtualDrawTree) and
    (TVirtualDrawTree(Source).Tag = 1) then
  begin
    Accept := True;
  end;
end;

procedure TfrmFilter.vdtFilterDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
begin
  frmMain.vdtTilesDrawNode(Sender, PaintInfo);
end;

procedure TfrmFilter.vdtHuesChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  hueInfo: PHueInfo;
begin
  hueInfo := Sender.GetNodeData(Node);
  FCheckedHues.Bits[hueInfo^.ID] := (Sender.CheckState[node] = csCheckedNormal);
end;

procedure TfrmFilter.vdtHuesDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
var
  hueInfo: PHueInfo;
  hueColor: TColor;
  i: Integer;
  textStyle: TTextStyle;
begin
  hueInfo := Sender.GetNodeData(PaintInfo.Node);
  textStyle := PaintInfo.Canvas.TextStyle;
  textStyle.Alignment := taLeftJustify;
  textStyle.Layout := tlCenter;
  textStyle.Wordbreak := True;
  case PaintInfo.Column of
    1:
      begin
        for i := 0 to 31 do
        begin
          hueColor := ARGB2RGB(hueInfo^.Hue.ColorTable[i]);
          PaintInfo.Canvas.Pen.Color := hueColor;
          PaintInfo.Canvas.MoveTo(PaintInfo.CellRect.Left + 2 + i, PaintInfo.CellRect.Top + 1);
          PaintInfo.Canvas.LineTo(PaintInfo.CellRect.Left + 2 + i, PaintInfo.CellRect.Bottom - 1);
        end;
      end;
    2:
      begin
        PaintInfo.Canvas.TextRect(PaintInfo.CellRect, PaintInfo.CellRect.Left, PaintInfo.CellRect.Top, Format('$%x (%s)', [hueInfo^.ID, hueInfo^.Hue.Name]), textStyle);
      end;
  end;
end;

procedure TfrmFilter.MouseLeave(var msg: TLMessage);
begin
  {if Active and (not PtInRect(ClientRect, ScreenToClient(Mouse.CursorPos))) then
    Close;}
end;

function TfrmFilter.Filter(AStatic: TStaticItem): Boolean;
var
  found: Boolean;
  tileInfo: PTileInfo;
  node: PVirtualNode;
  id: Word;
begin
  if cbTileFilter.Checked then
  begin
    id := AStatic.TileID + $4000;

    found := False;
    node := vdtFilter.GetFirst;
    while (node <> nil) and (not found) do
    begin
      tileInfo := vdtFilter.GetNodeData(node);
      if tileInfo^.ID = id then
        found := True
      else
        node := vdtFilter.GetNext(node);
    end;

    Result := ((rgFilterType.ItemIndex = 0) and (not found)) or
              ((rgFilterType.ItemIndex = 1) and found);
  end else
    Result := True;
    
  if cbHueFilter.Checked then
  begin
    Result := Result and (
                ((rgFilterType.ItemIndex = 0) and (not FCheckedHues.Bits[AStatic.Hue])) or
                ((rgFilterType.ItemIndex = 1) and (FCheckedHues.Bits[AStatic.Hue]))
              );
  end;
end;

procedure TfrmFilter.JumpToHue(AHueID: Word);
var
  hueInfo: PHueInfo;
  node: PVirtualNode;
begin
  node := vdtHues.GetFirst;
  while node <> nil do
  begin
    hueInfo := vdtHues.GetNodeData(node);
    if hueInfo^.ID = AHueID then
    begin
      vdtHues.ClearSelection;
      vdtHues.Selected[node] := True;
      vdtHues.FocusedNode := node;
      node := nil;
    end else
      node := vdtHues.GetNext(node);
  end;
end;

procedure TfrmFilter.FormCreate(Sender: TObject);
var
  i: Integer;
  hueInfo: PHueInfo;
  node: PVirtualNode;
begin
  FLocked := False;
  vdtFilter.NodeDataSize := SizeOf(TTileInfo);
  vdtHues.NodeDataSize := SizeOf(THueInfo);
  
  vdtHues.BeginUpdate;
  vdtHues.Clear;
  for i := 0 to ResMan.Hue.Count - 1 do
  begin
    node := vdtHues.AddChild(nil);
    hueInfo := vdtHues.GetNodeData(node);
    hueInfo^.ID := i + 1;
    hueInfo^.Hue := ResMan.Hue.Hues[i];
    vdtHues.CheckType[node] := ctCheckBox;
  end;
  vdtHues.EndUpdate;
  FCheckedHues := TBits.Create(ResMan.Hue.Count + 1);
  //FCheckedHues.Bits[0] := True;
end;

procedure TfrmFilter.FormDestroy(Sender: TObject);
begin
  if FCheckedHues <> nil then FreeAndNil(FCheckedHues);
end;

procedure TfrmFilter.btnDeleteClick(Sender: TObject);
begin
  vdtFilter.DeleteSelectedNodes;
end;

procedure TfrmFilter.btnClearClick(Sender: TObject);
begin
  vdtFilter.Clear;
end;

initialization
  {$I UfrmFilter.lrs}

end.

