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
unit UfrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, OpenGLContext, GL, GLU, UGameResources, ULandscape, ExtCtrls,
  StdCtrls, Spin, UEnums, VirtualTrees, Buttons, UMulBlock, UWorldItem, math,
  LCLIntf, UOverlayUI, UStatics, UEnhancedMemoryStream, ActnList,
  ImagingClasses, dateutils, UPlatformTypes;

type

  TVirtualTile = class(TStaticItem);
  TVirtualTileArray = array of TVirtualTile;

  TAccessChangedListener = procedure(AAccessLevel: TAccessLevel) of object;

  { TfrmMain }

  TfrmMain = class(TForm)
    acSelect: TAction;
    acDraw: TAction;
    acMove: TAction;
    acElevate: TAction;
    acDelete: TAction;
    acHue: TAction;
    acBoundaries: TAction;
    acFilter: TAction;
    acFlat: TAction;
    acNoDraw: TAction;
    acVirtualLayer: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    btnAddLocation: TSpeedButton;
    btnClearLocations: TSpeedButton;
    btnDeleteLocation: TSpeedButton;
    btnGoTo: TButton;
    cbRandomPreset: TComboBox;
    cbTerrain: TCheckBox;
    cbStatics: TCheckBox;
    edFilter: TEdit;
    edChat: TEdit;
    edSearchID: TEdit;
    gbRandom: TGroupBox;
    ImageList1: TImageList;
    lblChatHeaderCaption: TLabel;
    lblTipC: TLabel;
    lblTip: TLabel;
    lblTileInfo: TLabel;
    lblFilter: TLabel;
    lblX: TLabel;
    lblY: TLabel;
    lbClients: TListBox;
    MainMenu1: TMainMenu;
    mnuRegionControl: TMenuItem;
    mnuVirtualLayer: TMenuItem;
    mnuGrabTileID: TMenuItem;
    mnuGrabHue: TMenuItem;
    mnuLargeScaleCommands: TMenuItem;
    mnuSetHue: TMenuItem;
    mnuGoToClient: TMenuItem;
    mnuAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuSeparator3: TMenuItem;
    mnuBoundaries: TMenuItem;
    mnuSelect: TMenuItem;
    mnuDraw: TMenuItem;
    mnuMove: TMenuItem;
    mnuElevate: TMenuItem;
    mnuDelete: TMenuItem;
    mnuAddToRandom: TMenuItem;
    mnuFlush: TMenuItem;
    mnuShutdown: TMenuItem;
    mnuSeparator2: TMenuItem;
    mnuAccountControl: TMenuItem;
    mnuAdministration: TMenuItem;
    mnuSeparator1: TMenuItem;
    mnuExit: TMenuItem;
    mnuDisconnect: TMenuItem;
    mnuCentrED: TMenuItem;
    oglGameWindow: TOpenGLControl;
    pnlRandomPreset: TPanel;
    pnlLocationControls: TPanel;
    pnlChat: TPanel;
    pnlChatHeader: TPanel;
    pnlMain: TPanel;
    pnlRandomControl: TPanel;
    pnlTileListSettings: TPanel;
    pcLeft: TPageControl;
    pnlBottom: TPanel;
    edX: TSpinEdit;
    edY: TSpinEdit;
    pmTileList: TPopupMenu;
    btnAddRandom: TSpeedButton;
    btnDeleteRandom: TSpeedButton;
    btnClearRandom: TSpeedButton;
    pmTools: TPopupMenu;
    pmClients: TPopupMenu;
    pmGrabTileInfo: TPopupMenu;
    spChat: TSplitter;
    btnRandomPresetSave: TSpeedButton;
    btnRandomPresetDelete: TSpeedButton;
    spTileList: TSplitter;
    tbFilter: TToolButton;
    tbFlat: TToolButton;
    tbNoDraw: TToolButton;
    tmTileHint: TTimer;
    tsLocations: TTabSheet;
    tbSetHue: TToolButton;
    tmGrabTileInfo: TTimer;
    tmMovement: TTimer;
    tbSeparator4: TToolButton;
    tbRadarMap: TToolButton;
    tbVirtualLayer: TToolButton;
    tsClients: TTabSheet;
    tbMain: TToolBar;
    tbDisconnect: TToolButton;
    tbSeparator1: TToolButton;
    tbSelect: TToolButton;
    tbDrawTile: TToolButton;
    tbMoveTile: TToolButton;
    tbElevateTile: TToolButton;
    tbDeleteTile: TToolButton;
    tbSeparator2: TToolButton;
    tbBoundaries: TToolButton;
    tbSeparator3: TToolButton;
    tbTerrain: TToolButton;
    tbStatics: TToolButton;
    tsTiles: TTabSheet;
    vdtTiles: TVirtualDrawTree;
    vdtRandom: TVirtualDrawTree;
    vstLocations: TVirtualStringTree;
    vstChat: TVirtualStringTree;
    procedure acBoundariesExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDrawExecute(Sender: TObject);
    procedure acElevateExecute(Sender: TObject);
    procedure acFilterExecute(Sender: TObject);
    procedure acFlatExecute(Sender: TObject);
    procedure acHueExecute(Sender: TObject);
    procedure acMoveExecute(Sender: TObject);
    procedure acNoDrawExecute(Sender: TObject);
    procedure acSelectExecute(Sender: TObject);
    procedure acVirtualLayerExecute(Sender: TObject);
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure btnAddLocationClick(Sender: TObject);
    procedure btnAddRandomClick(Sender: TObject);
    procedure btnClearLocationsClick(Sender: TObject);
    procedure btnClearRandomClick(Sender: TObject);
    procedure btnDeleteLocationClick(Sender: TObject);
    procedure btnDeleteRandomClick(Sender: TObject);
    procedure btnGoToClick(Sender: TObject);
    procedure btnRandomPresetDeleteClick(Sender: TObject);
    procedure btnRandomPresetSaveClick(Sender: TObject);
    procedure cbRandomPresetChange(Sender: TObject);
    procedure cbStaticsChange(Sender: TObject);
    procedure cbTerrainChange(Sender: TObject);
    procedure edChatKeyPress(Sender: TObject; var Key: char);
    procedure edFilterEditingDone(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edSearchIDExit(Sender: TObject);
    procedure edSearchIDKeyPress(Sender: TObject; var Key: char);
    procedure lblChatHeaderCaptionClick(Sender: TObject);
    procedure lblChatHeaderCaptionMouseEnter(Sender: TObject);
    procedure lblChatHeaderCaptionMouseLeave(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuAccountControlClick(Sender: TObject);
    procedure mnuDisconnectClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuFlushClick(Sender: TObject);
    procedure mnuGoToClientClick(Sender: TObject);
    procedure mnuGrabHueClick(Sender: TObject);
    procedure mnuGrabTileIDClick(Sender: TObject);
    procedure mnuLargeScaleCommandsClick(Sender: TObject);
    procedure mnuRegionControlClick(Sender: TObject);
    procedure mnuShutdownClick(Sender: TObject);
    procedure oglGameWindowDblClick(Sender: TObject);
    procedure oglGameWindowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure oglGameWindowMouseEnter(Sender: TObject);
    procedure oglGameWindowMouseLeave(Sender: TObject);
    procedure oglGameWindowMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure oglGameWindowMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure oglGameWindowMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure oglGameWindowPaint(Sender: TObject);
    procedure pmGrabTileInfoPopup(Sender: TObject);
    procedure tbFilterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tbRadarMapClick(Sender: TObject);
    procedure tmGrabTileInfoTimer(Sender: TObject);
    procedure tmMovementTimer(Sender: TObject);
    procedure tmTileHintTimer(Sender: TObject);
    procedure vdtRandomDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vdtRandomDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure vdtRandomLoadNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Stream: TStream);
    procedure vdtRandomSaveNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Stream: TStream);
    procedure vdtRandomUpdating(Sender: TBaseVirtualTree; State: TVTUpdateState);
    procedure vdtTilesClick(Sender: TObject);
    procedure vdtTilesDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure vdtTilesEnter(Sender: TObject);
    procedure vdtTilesExit(Sender: TObject);
    procedure vdtTilesHotChange(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode);
    procedure vdtTilesKeyPress(Sender: TObject; var Key: char);
    procedure vdtTilesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure vdtTilesScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure vstChatClick(Sender: TObject);
    procedure vstChatFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstChatGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstChatPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vstLocationsDblClick(Sender: TObject);
    procedure vstLocationsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode
      );
    procedure vstLocationsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstLocationsLoadNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure vstLocationsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure vstLocationsSaveNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
  protected
    FX: Integer;
    FY: Integer;
    FLandscape: TLandscape;
    FTextureManager: TLandTextureManager;
    FScreenBuffer: TScreenBuffer;
    FCurrentTile: TWorldItem;
    FSelectedTile: TWorldItem;
    FGhostTile: TWorldItem;
    FVirtualLayer: array of TVirtualTileArray;
    FVLayerMaterial: TMaterial;
    FOverlayUI: TOverlayUI;
    FLocationsFile: string;
    FRandomPresetLocation: string;
    FLastDraw: TDateTime;
    FAccessChangedListeners: array of TAccessChangedListener;
    procedure SetX(const AValue: Integer);
    procedure SetY(const AValue: Integer);
    procedure SetCurrentTile(const AValue: TWorldItem);
    procedure SetSelectedTile(const AValue: TWorldItem);
    procedure SetNormalLights; inline;
    procedure SetDarkLights; inline;
    procedure InitRender;
    procedure InitSize;
    procedure Render;
    procedure OnLandscapeChanged;
    procedure BuildTileList;
    procedure ProcessToolState;
    procedure ProcessAccessLevel;
    procedure UpdateCurrentTile;
    procedure UpdateCurrentTile(AX, AY: Integer);
    procedure TileRemoved(ATile: TMulBlock);
    procedure WriteChatMessage(ASender, AMessage: string);
    procedure PrepareVirtualLayer(AWidth, AHeight: Word);
    procedure OnClientHandlingPacket(ABuffer: TEnhancedMemoryStream);
    function GetInternalTileID(ATile: TWorldItem): Word;
    function GetSelectedRect: TRect;
    function ConfirmAction: Boolean;
    function CanBeModified(ATile: TWorldItem): Boolean;
  public
    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
    property Landscape: TLandscape read FLandscape;
    property CurrentTile: TWorldItem read FCurrentTile write SetCurrentTile;
    property SelectedTile: TWorldItem read FSelectedTile write SetSelectedTile;
    
    procedure SetPos(AX, AY: Word);
    procedure RegisterAccessChangedListener(AListener: TAccessChangedListener);
    procedure UnregisterAccessChangedListener(AListener: TAccessChangedListener);
  end; 

var
  frmMain: TfrmMain;

implementation

uses
  UdmNetwork, UMap, UArt, UTiledata, UHue, UAdminHandling, UPackets,
  UfrmAccountControl, UGraphicHelper, ImagingComponents, UfrmDrawSettings,
  UfrmBoundaries, UfrmElevateSettings, UfrmConfirmation, UfrmMoveSettings,
  UfrmAbout, UPacketHandlers, UfrmHueSettings, UfrmRadar, UfrmLargeScaleCommand,
  UfrmLogin, UResourceManager, UfrmVirtualLayer, UfrmFilter, UfrmTileInfo,
  UfrmRegionControl;

type
  TGLArrayf4 = array[0..3] of GLfloat;
  PTileInfo = ^TTileInfo;
  TTileInfo = record
    ID: Word;
  end;
  PChatInfo = ^TChatInfo;
  TChatInfo = record
    Time: TDateTime;
    Sender: string;
    Msg: string;
  end;
  PLocationInfo = ^TLocationInfo;
  TLocationInfo = record
    X: Word;
    Y: Word;
    Name: string;
  end;

{ TfrmMain }

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuFlushClick(Sender: TObject);
begin
  dmNetwork.Send(TFlushServerPacket.Create);
end;

procedure TfrmMain.mnuGoToClientClick(Sender: TObject);
begin
  if lbClients.ItemIndex > -1 then
    dmNetwork.Send(TGotoClientPosPacket.Create(lbClients.Items.Strings[lbClients.ItemIndex]));
end;

procedure TfrmMain.mnuGrabHueClick(Sender: TObject);
begin
  if CurrentTile is TStaticItem then
  begin
    frmHueSettings.lbHue.ItemIndex := TStaticItem(CurrentTile).Hue;
    frmFilter.JumpToHue(TStaticItem(CurrentTile).Hue);
  end;
end;

procedure TfrmMain.mnuGrabTileIDClick(Sender: TObject);
var
  internalTileID: Integer;
  node: PVirtualNode;
  tileInfo: PTileInfo;
begin
  if CurrentTile <> nil then
  begin
    internalTileID := GetInternalTileID(CurrentTile);
    node := vdtTiles.GetFirst;
    while node <> nil do
    begin
      tileInfo := vdtTiles.GetNodeData(node);
      if tileInfo^.ID = internalTileID then
      begin
        vdtTiles.ClearSelection;
        vdtTiles.Selected[node] := True;
        vdtTiles.FocusedNode := node;
        Break;
      end;
      node := vdtTiles.GetNext(node);
    end;
  end;
end;

procedure TfrmMain.mnuLargeScaleCommandsClick(Sender: TObject);
begin
  frmLargeScaleCommand.Show;
end;

procedure TfrmMain.mnuRegionControlClick(Sender: TObject);
begin
  frmRegionControl.Show;
end;

procedure TfrmMain.mnuShutdownClick(Sender: TObject);
begin
  dmNetwork.Send(TQuitServerPacket.Create(''));
end;

procedure TfrmMain.oglGameWindowDblClick(Sender: TObject);
begin
  if (acSelect.Checked) and (CurrentTile <> nil) then
    btnAddRandomClick(Sender);
end;

procedure TfrmMain.oglGameWindowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    pmTools.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    
  if Button <> mbLeft then
    Exit;

  UpdateCurrentTile(X, Y);
  if FOverlayUI.ActiveArrow > -1 then
    tmMovement.Enabled := True;
  
  SelectedTile := CurrentTile;
  if CurrentTile = nil then Exit;

  if acSelect.Checked then                        //***** Selection Mode *****//
    tmGrabTileInfo.Enabled := True;
end;

procedure TfrmMain.oglGameWindowMouseEnter(Sender: TObject);
begin
  if Active then
    oglGameWindow.SetFocus;

  FOverlayUI.Visible := True;

  if frmFilter.Visible then
  begin
    frmFilter.Locked := True;
    frmFilter.Hide;
    frmFilter.Locked := False;
  end;
end;

procedure TfrmMain.oglGameWindowMouseLeave(Sender: TObject);
begin
  if not (frmConfirmation.Visible or
          (frmMoveSettings.Visible and (fsModal in frmMoveSettings.FormState))
         ) then //during confirmation the mouse would leave ...
  begin
    lblTileInfo.Caption := '';
    CurrentTile := nil;
    FOverlayUI.Visible := False;
  end;
end;

procedure TfrmMain.oglGameWindowMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  lastTile: TWorldItem;
  offsetX, offsetY: Integer;
begin
  lastTile := CurrentTile;
  
  if ssMiddle in Shift then
  begin
    UpdateCurrentTile(X, Y);
    if (lastTile <> nil) and (CurrentTile <> nil) and (lastTile <> CurrentTile) then
    begin
      offsetX := lastTile.X - CurrentTile.X;
      offsetY := lastTile.Y - CurrentTile.Y;
      if InRange(offsetX, -8, 8) and InRange(offsetY, -8, 8) then
        SetPos(FX - offsetX * 4, FY - offsetY * 4);
    end;
  end;
  
  lblTileInfo.Caption := '';
  CurrentTile := nil;

  UpdateCurrentTile(X, Y);
end;

procedure TfrmMain.oglGameWindowMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  node: PVirtualNode;
  tileInfo: PTileInfo;
  map: TMapCell;
  i: Integer;
  z: ShortInt;
  blockInfo: PBlockInfo;
  targetRect: TRect;
  tileX, tileY: Word;
  offsetX, offsetY: Integer;
  tile: TWorldItem;
  targetTiles: TList;
  targetTile: TWorldItem;
begin
  if Button <> mbLeft then
    Exit;

  UpdateCurrentTile(X, Y);
  tmMovement.Enabled := False;
  if CurrentTile = nil then Exit;
  targetTile := CurrentTile;
  
  if acSelect.Checked and tmGrabTileInfo.Enabled then
  begin
    tmGrabTileInfo.Enabled := False;
    mnuGrabTileIDClick(nil);
  end;

  if (not acSelect.Checked) and (targetTile <> nil) then
  begin
    targetRect := GetSelectedRect;

    if (SelectedTile = targetTile) or ConfirmAction then
    begin
      if acDraw.Checked then                        //***** Drawing Mode *****//
      begin
        if FGhostTile <> nil then
        begin
          for tileX := targetRect.Left to targetRect.Right - 1 do
          begin
            for tileY := targetRect.Top to targetRect.Bottom - 1 do
            begin
              tileInfo := nil;
              if frmDrawSettings.rbTileList.Checked then
              begin
                node := vdtTiles.GetFirstSelected;
                if node <> nil then
                  tileInfo := vdtTiles.GetNodeData(node);
              end else if frmDrawSettings.rbRandom.Checked then
              begin
                node := vdtRandom.GetFirst;
                for i := 1 to Random(vdtRandom.RootNodeCount) do
                  node := vdtRandom.GetNext(node);

                if node <> nil then
                  tileInfo := vdtRandom.GetNodeData(node);
              end;

              if tileInfo^.ID < $4000 then
              begin
                map := FLandscape.MapCell[tileX, tileY];
                if frmDrawSettings.cbForceAltitude.Checked then
                  map.Altitude := frmDrawSettings.seForceAltitude.Value;
                if frmDrawSettings.cbRandomHeight.Checked then
                  Inc(map.Altitude, Random(frmDrawSettings.seRandomHeight.Value));
                dmNetwork.Send(TDrawMapPacket.Create(map.X, map.Y, map.Z, tileInfo^.ID));
              end else
              begin
                dmNetwork.Send(TInsertStaticPacket.Create(tileX, tileY,
                  FGhostTile.Z, tileInfo^.ID - $4000,
                  TStaticItem(FGhostTile).Hue));
              end;
            end;
          end;
        end;
      end else if (SelectedTile <> targetTile) or CanBeModified(targetTile) then
      begin
        if (not acMove.Checked) or (SelectedTile <> targetTile) or
           (not frmMoveSettings.cbAsk.Checked) or ConfirmAction then
        begin
          targetTiles := TList.Create;
          if SelectedTile = targetTile then
          begin
            targetTiles.Add(targetTile)
          end else
          begin
            blockInfo := nil;
            while FScreenBuffer.Iterate(blockInfo) do
            begin
              if PtInRect(targetRect, Point(blockInfo^.Item.X, blockInfo^.Item.Y)) and
                CanBeModified(blockInfo^.Item) then
                targetTiles.Insert(0, blockInfo^.Item);
            end;
          end;

          if acMove.Checked then                       //***** Move tile *****//
          begin
            offsetX := frmMoveSettings.GetOffsetX;
            offsetY := frmMoveSettings.GetOffsetY;
            for i := 0 to targetTiles.Count - 1 do
            begin
              tile := TWorldItem(targetTiles.Items[i]);

              if tile is TStaticItem then
              begin
                dmNetwork.Send(TMoveStaticPacket.Create(TStaticItem(tile),
                  EnsureRange(tile.X + offsetX, 0, FLandscape.CellWidth - 1),
                  EnsureRange(tile.Y + offsetY, 0, FLandscape.CellHeight - 1)));
              end;
            end;
          end else if acElevate.Checked then        //***** Elevate tile *****//
          begin
            for i := 0 to targetTiles.Count - 1 do
            begin
              tile := TWorldItem(targetTiles.Items[i]);

              z := frmElevateSettings.seZ.Value;
              if frmElevateSettings.rbRaise.Checked then
                z := EnsureRange(tile.Z + z, -128, 127)
              else if frmElevateSettings.rbLower.Checked then
                z := EnsureRange(tile.Z - z, -128, 127);

              if tile is TMapCell then
              begin
                if frmElevateSettings.cbRandomHeight.Checked then
                  Inc(z, Random(frmElevateSettings.seRandomHeight.Value));
                dmNetwork.Send(TDrawMapPacket.Create(tile.X, tile.Y,
                  z, tile.TileID));
              end else
              begin
                dmNetwork.Send(TElevateStaticPacket.Create(TStaticItem(tile), z));
              end;
            end;
          end else if acDelete.Checked then          //***** Delete tile *****//
          begin
            for i := 0 to targetTiles.Count - 1 do
            begin
              tile := TWorldItem(targetTiles.Items[i]);

              if tile is TStaticItem then
                dmNetwork.Send(TDeleteStaticPacket.Create(TStaticItem(tile)));
            end;
          end else if acHue.Checked then                //***** Hue tile *****//
          begin
            for i := 0 to targetTiles.Count - 1 do
            begin
              tile := TWorldItem(targetTiles.Items[i]);

              if (tile is TStaticItem) and
                (TStaticItem(tile).Hue <> frmHueSettings.lbHue.ItemIndex) then
              begin
                dmNetwork.Send(THueStaticPacket.Create(TStaticItem(tile),
                  frmHueSettings.lbHue.ItemIndex));
              end;
            end;
          end;

          targetTiles.Free;
        end;
      end;
    end;
  end;
  SelectedTile := nil;
end;

procedure TfrmMain.oglGameWindowMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  cursorNeedsUpdate: Boolean;
begin
  //We want single steps ...
  WheelDelta := WheelDelta div WHEEL_DELTA;

  cursorNeedsUpdate := False;
  if (CurrentTile is TVirtualTile) or ((ssCtrl in Shift) and (frmVirtualLayer.cbShowLayer.Checked)) then
  begin
    frmVirtualLayer.seZ.Value := EnsureRange(frmVirtualLayer.seZ.Value + WheelDelta, -128, 127);
    cursorNeedsUpdate := True;
    Handled := True;
  end else if not (ssCtrl in Shift) then
  begin
    if CurrentTile is TStaticItem then
    begin
      dmNetwork.Send(TElevateStaticPacket.Create(TStaticItem(CurrentTile),
        EnsureRange(CurrentTile.Z + WheelDelta, -128, 127)));
      cursorNeedsUpdate := True;
      Handled := True;
    end else if CurrentTile is TMapCell then
    begin
      dmNetwork.Send(TDrawMapPacket.Create(CurrentTile.X, CurrentTile.Y,
        EnsureRange(CurrentTile.Z + WheelDelta, -128, 127), CurrentTile.TileID));
      Handled := True;
    end;
  end;
  
  if cursorNeedsUpdate then
  begin
    SetCursorPos(Mouse.CursorPos.X, Mouse.CursorPos.Y - 4 * WheelDelta);
    UpdateCurrentTile(MousePos.X, MousePos.Y - 4 * WheelDelta);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  virtualLayerGraphic: TSingleImage;
  searchRec: TSearchRec;
begin
  FLandscape := ResMan.Landscape;
  FLandscape.OnChange := @OnLandscapeChanged;
  FTextureManager := TLandTextureManager.Create;
  FScreenBuffer := TScreenBuffer.Create;
  X := 0;
  Y := 0;
  edX.MaxValue := FLandscape.CellWidth;
  edY.MaxValue := FLandscape.CellHeight;
  FOverlayUI := TOverlayUI.Create;
  
  ProcessAccessLevel;
  
  vdtTiles.NodeDataSize := SizeOf(TTileInfo);
  vdtRandom.NodeDataSize := SizeOf(TTileInfo);
  BuildTileList;
  Randomize;
  
  vstChat.NodeDataSize := SizeOf(TChatInfo);
  
  FLocationsFile := IncludeTrailingPathDelimiter(ExtractFilePath(
                    Application.ExeName)) + 'Locations.dat';
  vstLocations.NodeDataSize := SizeOf(TLocationInfo);
  if FileExists(FLocationsFile) then vstLocations.LoadFromFile(FLocationsFile);

  RegisterPacketHandler($0C, TPacketHandler.Create(0, @OnClientHandlingPacket));

  virtualLayerGraphic := TSingleImage.CreateFromStream(ResourceManager.GetResource(2));
  FVLayerMaterial := TMaterial.Create(virtualLayerGraphic.Width, virtualLayerGraphic.Height,
    virtualLayerGraphic);
  virtualLayerGraphic.Free;
  
  FRandomPresetLocation := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'RandomPresets' + PathDelim;
  if not DirectoryExists(FRandomPresetLocation) then CreateDir(FRandomPresetLocation);
  if FindFirst(FRandomPresetLocation + '*.dat', faAnyFile, searchRec) = 0 then
  begin
    repeat
      cbRandomPreset.Items.Add(ChangeFileExt(searchRec.Name, ''));
    until FindNext(searchRec) <> 0;
  end;
  FindClose(searchRec);
  
  FLastDraw := Now;
end;

procedure TfrmMain.btnGoToClick(Sender: TObject);
begin
  SetPos(edX.Value, edY.Value);
end;

procedure TfrmMain.btnRandomPresetDeleteClick(Sender: TObject);
begin
  if cbRandomPreset.ItemIndex > -1 then
  begin
    DeleteFile(FRandomPresetLocation + cbRandomPreset.Text + '.dat');
    cbRandomPreset.Items.Delete(cbRandomPreset.ItemIndex);
    cbRandomPreset.ItemIndex := -1;
  end;
end;

procedure TfrmMain.btnRandomPresetSaveClick(Sender: TObject);
var
  fileName: string;
  index: Integer;
begin
  fileName := cbRandomPreset.Text;
  if InputQuery('Save Preset', 'Enter the name of the preset:', fileName) then
  begin
    vdtRandom.SaveToFile(FRandomPresetLocation + fileName + '.dat');;
    index := cbRandomPreset.Items.IndexOf(fileName);
    if index = -1 then
    begin
      cbRandomPreset.Items.Add(fileName);
      index := cbRandomPreset.Items.Count - 1;
    end;
    cbRandomPreset.ItemIndex := index;
  end;
end;

procedure TfrmMain.cbRandomPresetChange(Sender: TObject);
begin
  if cbRandomPreset.ItemIndex > -1 then
    vdtRandom.LoadFromFile(FRandomPresetLocation + cbRandomPreset.Text + '.dat');
end;

procedure TfrmMain.btnAddRandomClick(Sender: TObject);
var
  selected, node: PVirtualNode;
  sourceTileInfo, targetTileInfo: PTileInfo;
begin
  vdtRandom.BeginUpdate;
  selected := vdtTiles.GetFirstSelected;
  while selected <> nil do
  begin
    sourceTileInfo := vdtTiles.GetNodeData(selected);
    node := vdtRandom.AddChild(nil);
    targetTileInfo := vdtRandom.GetNodeData(node);
    targetTileInfo^.ID := sourceTileInfo^.ID;
    selected := vdtTiles.GetNextSelected(selected);
  end;
  vdtRandom.EndUpdate;
end;

procedure TfrmMain.btnClearLocationsClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to delete all saved locations?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    vstLocations.Clear;
  end;
end;

procedure TfrmMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  if MilliSecondsBetween(FLastDraw, Now) > 30 then
  begin
    oglGameWindow.Repaint;
    FLastDraw := Now;
  end;
end;

procedure TfrmMain.btnAddLocationClick(Sender: TObject);
var
  locationName: string;
  locationInfo: PLocationInfo;
begin
  locationName := '';
  if InputQuery('New Location', 'Enter the name of the new location:', locationName) then
  begin
    locationInfo := vstLocations.GetNodeData(vstLocations.AddChild(nil));
    locationInfo^.X := X;
    locationInfo^.Y := Y;
    locationInfo^.Name := locationName;
  end;
end;

procedure TfrmMain.acSelectExecute(Sender: TObject);
begin
  acSelect.Checked := True;
  tbSelect.Down := True;
  mnuSelect.Checked := True;
  ProcessToolState;
end;

procedure TfrmMain.acVirtualLayerExecute(Sender: TObject);
begin
  frmVirtualLayer.Left := Mouse.CursorPos.x - 8;
  frmVirtualLayer.Top := Mouse.CursorPos.y - 8;
  frmVirtualLayer.Show;
end;

procedure TfrmMain.acDrawExecute(Sender: TObject);
begin
  acDraw.Checked := True;
  tbDrawTile.Down := True;
  mnuDraw.Checked := True;
  frmDrawSettings.Left := Mouse.CursorPos.x - 8;
  frmDrawSettings.Top := Mouse.CursorPos.y - 8;
  frmDrawSettings.ShowModal;
  ProcessToolState;
end;

procedure TfrmMain.acDeleteExecute(Sender: TObject);
begin
  acDelete.Checked := True;
  tbDeleteTile.Down := True;
  mnuDelete.Checked := True;
  ProcessToolState;
end;

procedure TfrmMain.acBoundariesExecute(Sender: TObject);
begin
  frmBoundaries.Left := Mouse.CursorPos.x - 8;
  frmBoundaries.Top := Mouse.CursorPos.y - 8;
  frmBoundaries.Show;
end;

procedure TfrmMain.acElevateExecute(Sender: TObject);
begin
  acElevate.Checked := True;
  tbElevateTile.Down := True;
  mnuElevate.Checked := True;
  ProcessToolState;
  frmElevateSettings.Left := Mouse.CursorPos.x - 8;
  frmElevateSettings.Top := Mouse.CursorPos.y - 8;
  frmElevateSettings.Show;
end;

procedure TfrmMain.acFilterExecute(Sender: TObject);
begin
  if acFilter.Checked then
  begin
    frmFilter.Show;
    frmFilter.Locked := False;
  end else
    frmFilter.Hide;
end;

procedure TfrmMain.acFlatExecute(Sender: TObject);
begin
  acFlat.Checked := not acFlat.Checked;
end;

procedure TfrmMain.acHueExecute(Sender: TObject);
begin
  acHue.Checked := True;
  tbSetHue.Down := True;
  mnuSetHue.Checked := True;
  ProcessToolState;
  frmHueSettings.Left := Mouse.CursorPos.x - 8;
  frmHueSettings.Top := Mouse.CursorPos.y - 8;
  frmHueSettings.Show;
end;

procedure TfrmMain.acMoveExecute(Sender: TObject);
begin
  acMove.Checked := True;
  tbMoveTile.Down := True;
  mnuMove.Checked := True;
  ProcessToolState;
  frmMoveSettings.Left := Mouse.CursorPos.x - 8;
  frmMoveSettings.Top := Mouse.CursorPos.y - 8;
  frmMoveSettings.Show;
end;

procedure TfrmMain.acNoDrawExecute(Sender: TObject);
begin
  acNoDraw.Checked := not acNoDraw.Checked;
end;

procedure TfrmMain.btnClearRandomClick(Sender: TObject);
begin
  vdtRandom.BeginUpdate;
  vdtRandom.Clear;
  vdtRandom.EndUpdate;
end;

procedure TfrmMain.btnDeleteLocationClick(Sender: TObject);
begin
  vstLocations.DeleteSelectedNodes;
end;

procedure TfrmMain.btnDeleteRandomClick(Sender: TObject);
begin
  vdtRandom.BeginUpdate;
  vdtRandom.DeleteSelectedNodes;
  vdtRandom.EndUpdate;
end;

procedure TfrmMain.cbStaticsChange(Sender: TObject);
begin
  if (not cbStatics.Checked) and (not cbTerrain.Checked) then
    cbTerrain.Checked := True;
  BuildTileList;
end;

procedure TfrmMain.cbTerrainChange(Sender: TObject);
begin
  if (not cbTerrain.Checked) and (not cbStatics.Checked) then
    cbStatics.Checked := True;
  BuildTileList;
end;

procedure TfrmMain.edChatKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    if edChat.Text <> '' then
    begin
      dmNetwork.Send(TChatMessagePacket.Create(edChat.Text));
      edChat.Text := '';
    end;
  end;
end;

procedure TfrmMain.edFilterEditingDone(Sender: TObject);
begin
  BuildTileList;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if oglGameWindow.MouseEntered then
    oglGameWindowMouseEnter(Sender);
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  dmNetwork.CheckClose(Self);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  CurrentTile := nil;
  SelectedTile := nil;
  
  vstLocations.SaveToFile(FLocationsFile);

  if FTextureManager <> nil then FreeAndNil(FTextureManager);
  if FScreenBuffer <> nil then FreeAndNil(FScreenBuffer);
  if FOverlayUI <> nil then FreeAndNil(FOverlayUI);
  if FGhostTile <> nil then FreeAndNil(FGhostTile);
  if FVLayerMaterial <> nil then FreeAndNil(FVLayerMaterial);
  PrepareVirtualLayer(0, 0); //Clear
  
  RegisterPacketHandler($0C, nil);
end;

procedure TfrmMain.edSearchIDExit(Sender: TObject);
begin
  edSearchID.Visible := False;
  edSearchID.Text := '';
  //edSearchID.Font.Color := clWindowText;
end;

procedure TfrmMain.edSearchIDKeyPress(Sender: TObject; var Key: char);
var
  enteredText: String;
  tileID: Integer;
  tileType: Char;
  node: PVirtualNode;
  tileInfo: PTileInfo;
begin
  if Key = #13 then
  begin
    Key := #0;
    enteredText := edSearchID.Text;
    tileType := #0;
    if Length(enteredText) > 1 then
      tileType := enteredText[Length(enteredText)];

    if not (tileType in ['S', 'T']) then
    begin
      if cbTerrain.Checked then
        tileType := 'T'
      else
        tileType := 'S';
    end else
      Delete(enteredText, Length(enteredText), 1);
    
    tileID := 0;
    if not TryStrToInt(enteredText, tileID) then
    begin
      //edSearchID.Font.Color := clRed;
      MessageDlg('Error', 'The specified TileID is invalid.', mtError, [mbOK], 0);
      vdtTiles.SetFocus;
      Exit;
    end;
    
    if tileType = 'S' then
      Inc(tileID, $4000);
      
    node := vdtTiles.GetFirst;
    while node <> nil do
    begin
      tileInfo := vdtTiles.GetNodeData(node);
      if tileInfo^.ID = tileID then
      begin
        vdtTiles.ClearSelection;
        vdtTiles.Selected[node] := True;
        vdtTiles.FocusedNode := node;
        Break;
      end;
      node := vdtTiles.GetNext(node);
    end;
    
    if node = nil then
    begin
      //edSearchID.Font.Color := clRed;
      MessageDlg('Error', 'The tile with the specified ID could not be found.' + LineEnding +
        'Check for conflicting filter settings.', mtError, [mbOK], 0);
      vdtTiles.SetFocus;
      Exit;
    end;
    //edSearchID.Font.Color := clWindowText;
    edSearchID.Visible := False;
  end else if Key = #27 then
  begin
    edSearchID.Visible := False;
    //edSearchID.Font.Color := clWindowText;
    Key := #0;
  end else if not (Key in ['$', '0'..'9', 'a'..'f', 'A'..'F', 's', 'S', 't', 'T', #8]) then
    Key := #0;
end;

procedure TfrmMain.lblChatHeaderCaptionClick(Sender: TObject);
begin
  if pnlChat.Visible then
  begin
    pnlChat.Visible := False;
    spChat.Visible := False;
  end else
  begin
    spChat.Visible := True;
    pnlChat.Visible := True;
    spChat.Top := pnlChatHeader.Top + pnlChatHeader.Height;
    pnlChat.Top := spChat.Top + spChat.Height;
    
    lblChatHeaderCaption.Font.Bold := False;
    lblChatHeaderCaption.Font.Italic := False;
    lblChatHeaderCaption.Font.Color := clWindowText;
    
    edChat.SetFocus;
  end;
end;

procedure TfrmMain.lblChatHeaderCaptionMouseEnter(Sender: TObject);
begin
  lblChatHeaderCaption.Font.Underline := True;
end;

procedure TfrmMain.lblChatHeaderCaptionMouseLeave(Sender: TObject);
begin
  lblChatHeaderCaption.Font.Underline := False;
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.mnuAccountControlClick(Sender: TObject);
begin
  frmAccountControl.Show;
end;

procedure TfrmMain.mnuDisconnectClick(Sender: TObject);
begin
  dmNetwork.Disconnect;
end;

procedure TfrmMain.oglGameWindowPaint(Sender: TObject);
begin
  glClear(GL_COLOR_BUFFER_BIT);

  InitRender;
  InitSize;
  
  if FVLayerMaterial.Texture = 0 then
    FVLayerMaterial.UpdateTexture;

  glDisable(GL_DEPTH_TEST);
  Render;

  oglGameWindow.SwapBuffers;
end;

procedure TfrmMain.pmGrabTileInfoPopup(Sender: TObject);
begin
  mnuGrabHue.Enabled := CurrentTile is TStaticItem;
end;

procedure TfrmMain.tbFilterMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if acFilter.Checked and (not frmFilter.Visible) then
    frmFilter.Show;
end;

procedure TfrmMain.tbRadarMapClick(Sender: TObject);
begin
  frmRadarMap.Show;
  frmRadarMap.BringToFront;
end;

procedure TfrmMain.tmGrabTileInfoTimer(Sender: TObject);
begin
  tmGrabTileInfo.Enabled := False;
  if CurrentTile <> nil then
    pmGrabTileInfo.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    
  SelectedTile := nil;
end;

procedure TfrmMain.tmMovementTimer(Sender: TObject);

  procedure MoveBy(AOffsetX, AOffsetY: Integer);
  begin
    SetPos(EnsureRange(FX + AOffsetX, 0, FLandscape.CellWidth - 1),
           EnsureRange(FY + AOffsetY, 0, FLandscape.CellHeight - 1));
  end;

begin
  case FOverlayUI.ActiveArrow of
    0: MoveBy(-8, 0);
    1: MoveBy(-8, -8);
    2: MoveBy(0, -8);
    3: MoveBy(8, -8);
    4: MoveBy(8, 0);
    5: MoveBy(8, 8);
    6: MoveBy(0, 8);
    7: MoveBy(-8, 8);
  end;
end;

procedure TfrmMain.tmTileHintTimer(Sender: TObject);
begin
  frmTileInfo.Show;
  tmTileHint.Enabled := False;
end;

procedure TfrmMain.vdtRandomDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  Pt: TPoint; var Effect: Integer; Mode: TDropMode);
begin
  if Source = vdtTiles then
    btnAddRandomClick(Sender);
end;

procedure TfrmMain.vdtRandomDragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer; var Accept: Boolean);
begin
  if source = vdtTiles then Accept := True;
end;

procedure TfrmMain.vdtRandomLoadNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  tileInfo: PTileInfo;
begin
  tileInfo := Sender.GetNodeData(Node);
  Stream.Read(tileInfo^.ID, SizeOf(tileInfo^.ID));
end;

procedure TfrmMain.vdtRandomSaveNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  tileInfo: PTileInfo;
begin
  tileInfo := Sender.GetNodeData(Node);
  Stream.Write(tileInfo^.ID, SizeOf(tileInfo^.ID));
end;

procedure TfrmMain.vdtRandomUpdating(Sender: TBaseVirtualTree;
  State: TVTUpdateState);
begin
  if acDraw.Checked then
    ProcessToolState;
end;

procedure TfrmMain.vdtTilesClick(Sender: TObject);
begin
  {if vdtTiles.GetFirstSelected <> nil then
  begin
    if not tbDrawTile.Down then
    begin
      frmDrawSettings.rbTileList.Checked := True;
      tbDrawTileClick(Sender);
    end else
      ProcessToolState;
  end;}
  if acDraw.Checked then
    ProcessToolState;
end;

procedure TfrmMain.vdtTilesDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
var
  tileInfo: PTileInfo;
  textStyle: TTextStyle;
  artEntry: TArt;
  tileData: TTileData;
  id: Integer;
begin
  tileInfo := Sender.GetNodeData(PaintInfo.Node);
  textStyle := PaintInfo.Canvas.TextStyle;
  textStyle.Alignment := taCenter;
  textStyle.Layout := tlCenter;
  textStyle.Wordbreak := True;
  case PaintInfo.Column of
    0:
      begin
        id := tileInfo^.ID;
        if id > $3FFF then
          Dec(id, $4000);
        PaintInfo.Canvas.TextRect(PaintInfo.CellRect, 0, 0, Format('$%x', [id]), textStyle);
      end;
    1:
      begin
        if ResMan.Art.Exists(tileInfo^.ID) then
        begin
          artEntry := ResMan.Art.GetArt(tileInfo^.ID, RGB2ARGB(PaintInfo.Canvas.Pixels[PaintInfo.CellRect.Left, PaintInfo.CellRect.Top]), nil, False);
          DisplayImage(PaintInfo.Canvas, PaintInfo.CellRect, artEntry.Graphic);
          artEntry.Free;
        end;
      end;
    2:
      begin
        tileData := TTileData(ResMan.Tiledata.Block[tileInfo^.ID]);
        PaintInfo.Canvas.TextRect(PaintInfo.CellRect, PaintInfo.CellRect.Left, PaintInfo.CellRect.Top, Trim(tileData.TileName), textStyle);
        tileData.Free;
      end;
  end;
end;

procedure TfrmMain.vdtTilesEnter(Sender: TObject);
begin
  if acFilter.Checked and (not frmFilter.Visible) and (not frmFilter.Locked) then
  begin
    frmFilter.Locked := True;
    frmFilter.Show;
    frmMain.SetFocus;
    frmFilter.Locked := False;
  end;
end;

procedure TfrmMain.vdtTilesExit(Sender: TObject);
begin
  {TODO : Fix mouse over on !Windows platforms}
  {$IFDEF Windows}
  tmTileHint.Enabled := False;
  {$ENDIF Windows}
end;

procedure TfrmMain.vdtTilesHotChange(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode);
{$IFDEF Windows}
var
  tileInfo: PTileInfo;
{$ENDIF Windows}
begin
  {TODO : Fix mouse over on !Windows platforms}
  {$IFDEF Windows}
  if NewNode <> nil then
  begin
    tileInfo := vdtTiles.GetNodeData(NewNode);
    frmTileInfo.Update(tileInfo^.ID);
    tmTileHint.Enabled := True;
  end else
  begin
    frmTileInfo.Hide;
    tmTileHint.Enabled := False;
  end;
  {$ENDIF Windows}
end;

procedure TfrmMain.vdtTilesKeyPress(Sender: TObject; var Key: char);
begin
  if Key in ['$', '0'..'9'] then
  begin
    edSearchID.Top := vdtTiles.Top + vdtTiles.Height - edSearchID.Height - 4;
    edSearchID.Left := vdtTiles.Left + vdtTiles.Width - edSearchID.Width - 4;
    edSearchID.Text := Key;
    edSearchID.Visible := True;
    edSearchID.SetFocus;
    edSearchID.SelStart := 1;
    Key := #0;
  end;
end;

procedure TfrmMain.vdtTilesMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if tmTileHint.Enabled then
  begin
    tmTileHint.Enabled := False;
    tmTileHint.Enabled := True; //Restart timer
  end;
  
  if frmTileInfo.Visible then
  begin
    frmTileInfo.Hide;
    tmTileHint.Enabled := True;
  end;
end;

procedure TfrmMain.vdtTilesScroll(Sender: TBaseVirtualTree; DeltaX,
  DeltaY: Integer);
begin
  if Sender.CanFocus and Sender.MouseEntered then
    Sender.SetFocus;
end;

procedure TfrmMain.vstChatClick(Sender: TObject);
begin
  edChat.SetFocus;
end;

procedure TfrmMain.vstChatFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  chatInfo: PChatInfo;
begin
  chatInfo := Sender.GetNodeData(Node);
  chatInfo^.Sender := '';
  chatInfo^.Msg := '';
end;

procedure TfrmMain.vstChatGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  chatInfo: PChatInfo;
begin
  chatInfo := Sender.GetNodeData(Node);
  case Column of
    0: CellText := TimeToStr(chatInfo^.Time);
    1: CellText := chatInfo^.Sender;
    2: CellText := chatInfo^.Msg;
  end;
end;

procedure TfrmMain.vstChatPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  chatInfo: PChatInfo;
begin
  chatInfo := Sender.GetNodeData(Node);
  if chatInfo^.Sender = 'System' then
  begin
    if Column = 1 then
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic, fsBold]
    else
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic];
  end;
end;

procedure TfrmMain.vstLocationsDblClick(Sender: TObject);
var
  node: PVirtualNode;
  locationInfo: PLocationInfo;
begin
  node := vstLocations.GetFirstSelected;
  if node <> nil then
  begin
    locationInfo := vstLocations.GetNodeData(node);
    SetPos(locationInfo^.X, locationInfo^.Y);
  end;
end;

procedure TfrmMain.vstLocationsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  locationInfo: PLocationInfo;
begin
  locationInfo := Sender.GetNodeData(Node);
  locationInfo^.Name := '';
end;

procedure TfrmMain.vstLocationsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  locationInfo: PLocationInfo;
begin
  locationInfo := Sender.GetNodeData(Node);
  case Column of
    0: CellText := Format('%d, %d', [locationInfo^.X, locationInfo^.Y]);
    1: CellText := locationInfo^.Name;
  end;
end;

procedure TfrmMain.vstLocationsLoadNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  locationInfo: PLocationInfo;
  stringLength: Integer;
  s: string;
begin
  locationInfo := Sender.GetNodeData(Node);
  Stream.Read(locationInfo^.X, SizeOf(Word));
  Stream.Read(locationInfo^.Y, SizeOf(Word));
  stringLength := 0;
  Stream.Read(stringLength, SizeOf(Integer));
  SetLength(s, stringLength);
  Stream.Read(s[1], stringLength);
  locationInfo^.Name := s;
end;

procedure TfrmMain.vstLocationsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  locationInfo: PLocationInfo;
begin
  if Column = 1 then
  begin
    locationInfo := Sender.GetNodeData(Node);
    locationInfo^.Name := NewText;
  end;
end;

procedure TfrmMain.vstLocationsSaveNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  locationInfo: PLocationInfo;
  stringLength: Integer;
begin
  locationInfo := Sender.GetNodeData(Node);
  Stream.Write(locationInfo^.X, SizeOf(Word));
  Stream.Write(locationInfo^.Y, SizeOf(Word));
  stringLength := Length(locationInfo^.Name);
  Stream.Write(stringLength, SizeOf(Integer));
  Stream.Write(locationInfo^.Name[1], stringLength);
end;

procedure TfrmMain.SetX(const AValue: Integer);
begin
  SetPos(AValue, FY);
end;

procedure TfrmMain.SetY(const AValue: Integer);
begin
  SetPos(FX, AValue);
end;

procedure TfrmMain.SetPos(AX, AY: Word);
begin
  if InRange(AX, 0, FLandscape.CellWidth - 1) and InRange(AY, 0, FLandscape.CellHeight - 1) then
  begin
    FX := AX;
    edX.Value := FX;
    FY := AY;
    edY.Value := FY;
    dmNetwork.Send(TUpdateClientPosPacket.Create(AX, AY));
    Repaint;
    if frmRadarMap <> nil then frmRadarMap.Repaint;
  end;
end;

procedure TfrmMain.RegisterAccessChangedListener(
  AListener: TAccessChangedListener);
var
  i: Integer;
begin
  for i := Low(FAccessChangedListeners) to High(FAccessChangedListeners) do
    if FAccessChangedListeners[i] = AListener then
      Exit; //Prevent duplicates
  SetLength(FAccessChangedListeners, Length(FAccessChangedListeners) + 1);
  FAccessChangedListeners[High(FAccessChangedListeners)] := AListener;
end;

procedure TfrmMain.UnregisterAccessChangedListener(
  AListener: TAccessChangedListener);
var
  i: Integer;
  found: Boolean;
begin
  i := Low(FAccessChangedListeners);
  found := False;
  while (i <= High(FAccessChangedListeners)) and (not found) do
  begin
    if FAccessChangedListeners[i] = AListener then
    begin
      if i < High(FAccessChangedListeners) then
        Move(FAccessChangedListeners[i+1], FAccessChangedListeners[i],
          (High(FAccessChangedListeners) - Low(FAccessChangedListeners) - i) *
          SizeOf(TAccessChangedListener)); //move subsequent entries
      SetLength(FAccessChangedListeners, Length(FAccessChangedListeners) - 1);
      found := True;
    end else
      Inc(i);
  end;
end;

procedure TfrmMain.SetCurrentTile(const AValue: TWorldItem);
begin
  if FCurrentTile <> nil then
    FCurrentTile.OnDestroy.UnregisterEvent(@TileRemoved);
  FCurrentTile := AValue;
  if FCurrentTile <> nil then
    FCurrentTile.OnDestroy.RegisterEvent(@TileRemoved);
end;

procedure TfrmMain.SetSelectedTile(const AValue: TWorldItem);
begin
  if FSelectedTile <> nil then
    FSelectedTile.OnDestroy.UnregisterEvent(@TileRemoved);
  FSelectedTile := AValue;
  if FSelectedTile <> nil then
    FSelectedTile.OnDestroy.RegisterEvent(@TileRemoved);
end;

procedure TfrmMain.SetNormalLights;
const
  specular: TGLArrayf4 = (2, 2, 2, 1);
  ambient: TGLArrayf4 = (1, 1, 1, 1);
begin
  glLightfv(GL_LIGHT0, GL_AMBIENT, @specular);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @ambient);
end;

procedure TfrmMain.SetDarkLights;
const
  specularDark: TGLArrayf4 = (0.5, 0.5, 0.5, 1);
  ambientDark: TGLArrayf4 = (0.25, 0.25, 0.25, 1);
begin
  glLightfv(GL_LIGHT0, GL_AMBIENT, @specularDark);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @ambientDark);
end;

procedure TfrmMain.InitRender;
const
  lightPosition: TGLArrayf4 = (-1, -1, 0.5, 0);
begin
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER, 0.1);
  glEnable(GL_TEXTURE_2D);
  glDisable(GL_DITHER);
  glEnable(GL_BLEND); // Enable alpha blending of textures
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glShadeModel(GL_SMOOTH); // Go with flat shading for now
  glEnable(GL_NORMALIZE);

  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_POSITION, @lightPosition);
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_FALSE);
end;

procedure TfrmMain.InitSize;
begin
  glViewport(0, 0, oglGameWindow.Width, oglGameWindow.Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  //glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  gluOrtho2D(0, oglGameWindow.Width, oglGameWindow.Height, 0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TfrmMain.Render;
var
  drawDistance: Integer;
  offsetX, offsetY: Integer;
  lowOffX, lowOffY, highOffX, highOffY: Integer;
  z: ShortInt;
  mat: TMaterial;
  cell: TMapCell;
  west, south, east: Single;
  drawX, drawY: Single;
  draw: TList;
  staticItem: TStaticItem;
  i, j, k: Integer;
  startOffX, endOffX, rangeX, rangeY: Integer;
  normals: TNormals;
  staticTileData: TStaticTileData;
  hue: THue;
  highlight, singleTarget, multiTarget: Boolean;
  ghostTile: TWorldItem;
  tileRect: TRect;
  virtualTile: TVirtualTile;
  staticsFilter: TStaticFilter;
  editing: Boolean;
  intensity: GLfloat;

  procedure GetMapDrawOffset(x, y: Integer; out drawX, drawY: Single);
  begin
    drawX := (oglGameWindow.Width div 2) + (x - y) * 22;
    drawY := (oglGamewindow.Height div 2) + (x + y) * 22;
  end;
begin
  drawDistance := Trunc(Sqrt(oglGameWindow.Width * oglGameWindow.Width + oglGamewindow.Height * oglGamewindow.Height) / 44);

  {$HINTS off}{$WARNINGS off}
  if FX - drawDistance < 0 then lowOffX := -FX else lowOffX := -drawDistance;
  if FY - drawDistance < 0 then lowOffY := -FY else lowOffY := -drawDistance;
  if FX + drawDistance >= FLandscape.Width * 8 then highOffX := FLandscape.Width * 8 - FX - 1 else highOffX := drawDistance;
  if FY + drawDistance >= FLandscape.Height * 8 then highOffY := FLandscape.Height * 8 - FY - 1 else highOffY := drawDistance;
  {$HINTS on}{$WARNINGS on}

  FLandscape.PrepareBlocks((FX + lowOffX) div 8, (FY + lowOffY) div 8, (FX + highOffX) div 8 + 1, (FY + highOffY) div 8 + 1);
  PrepareVirtualLayer(drawDistance * 2 + 1, drawDistance * 2 + 1);

  tileRect := GetSelectedRect;
  FScreenBuffer.Clear;

  rangeX := highOffX - lowOffX;
  rangeY := highOffY - lowOffY;
  
  if acFilter.Checked then
    staticsFilter := @frmFilter.Filter
  else
    staticsFilter := nil;
  
  for j := 0 to rangeX + rangeY - 2 do
  begin
    if j > rangeY then
    begin
      startOffX := j - rangeY + 1;
      endOffX := rangeX;
    end else
    begin
      startOffX := 0;
      endOffX := j;
    end;
    for k := startOffX to endOffX do
    begin
      offsetY := j - k + lowOffY;
      offsetX := k + lowOffX;
      GetMapDrawOffset(offsetX, offsetY, drawX, drawY);

      singleTarget := (CurrentTile <> nil) and
                      (FX + offsetX = CurrentTile.X) and
                      (FY + offsetY = CurrentTile.Y);
      multiTarget := (CurrentTile <> nil) and
                     (SelectedTile <> nil) and
                     (CurrentTile <> SelectedTile) and
                     PtInRect(tileRect, Point(FX + offsetX, FY + offsetY));

      if acSelect.Checked or dmNetwork.CanWrite(FX + offsetX, FY + offsetY)  then
      begin
        editing := True;
        intensity := 1.0;
        SetNormalLights;
      end else
      begin
        editing := False;
        intensity := 0.5;
        SetDarkLights;
      end;

      if editing and acDraw.Checked and (singleTarget or multiTarget) then
      begin
        ghostTile := FGhostTile;
        if (ghostTile is TMapCell) and (not frmDrawSettings.cbForceAltitude.Checked) then
          ghostTile.Z := FLandscape.MapCell[FX + offsetX, FY + offsetY].Z;
      end else
        ghostTile := nil;
      
      if frmVirtualLayer.cbShowLayer.Checked then
      begin
        virtualTile := FVirtualLayer[k, j - k];
        virtualTile.X := FX + offsetX;
        virtualTile.Y := FY + offsetY;
        virtualTile.Z := frmVirtualLayer.seZ.Value;
      end else
        virtualTile := nil;
      
      draw := FLandscape.GetDrawList(FX + offsetX, FY + offsetY,
        frmBoundaries.tbMinZ.Position, frmBoundaries.tbMaxZ.Position,
        ghostTile, virtualTile, tbTerrain.Down, tbStatics.Down,
        acNoDraw.Checked, staticsFilter);
        
      for i := 0 to draw.Count - 1 do
      begin
        if not editing then
          highlight := False
        else if TObject(draw[i]) = virtualTile then
          highlight := False
        else if acDelete.Checked and multiTarget and (TObject(draw[i]) is TStaticItem) then
          highlight := True
        else if ((acElevate.Checked) or (acMove.Checked)) and multiTarget then
          highlight := True
        else if (acHue.Checked and multiTarget and (TObject(draw[i]) is TMapCell)) then
          highlight := True
        else
          highlight := (not acSelect.Checked) and
                       (not acHue.Checked) and
                       ((TObject(draw[i]) = CurrentTile) or
                       ((TObject(draw[i]) is TMapCell) and (TObject(draw[i]) = ghostTile)));

        if highlight then
        begin
          glEnable(GL_COLOR_LOGIC_OP);
          glLogicOp(GL_COPY_INVERTED);
        end;
        
        if acFlat.Checked then
          z := 0
        else
          z := TWorldItem(draw[i]).Z;

        glColor4f(intensity, intensity, intensity, 1.0);
        
        if TObject(draw[i]) = virtualTile then
        begin
          glBindTexture(GL_TEXTURE_2D, FVLayerMaterial.Texture);
          glBegin(GL_QUADS);
            glTexCoord2f(0, 0); glVertex2d(drawX - 22, drawY - z * 4);
            glTexCoord2f(1, 0); glVertex2d(drawX - 22 + FVLayerMaterial.Width, drawY - z * 4);
            glTexCoord2f(1, 1); glVertex2d(drawX - 22 + FVLayerMaterial.Width, drawY + FVLayerMaterial.Height - z * 4);
            glTexCoord2f(0, 1); glVertex2d(drawX - 22, drawY + FVLayerMaterial.Height - z * 4);
          glEnd;

          FScreenBuffer.Store(Bounds(Trunc(drawX - 22), Trunc(drawY - z * 4), 44, 44), virtualTile, FVLayerMaterial);
        end else if TObject(draw[i]) is TMapCell then
        begin
          cell := TMapCell(draw[i]);
          
          {if ResMan.Tiledata.LandTiles[cell.TileID].HasFlag(tdfTranslucent) then
            glColor4f(intensity, intensity, intensity, 0.5);} //Possible, but probably not like the OSI client

          mat := nil;

          if not acFlat.Checked then
          begin
            west := FLandscape.GetLandAlt(FX + offsetX, FY + offsetY + 1, z);
            south := FLandscape.GetLandAlt(FX + offsetX + 1, FY + offsetY + 1, z);
            east := FLandscape.GetLandAlt(FX + offsetX + 1, FY + offsetY, z);

            if  (west <> z) or (south <> z) or (east <> z) then
            begin
              mat := FTextureManager.GetTexMaterial(cell.TileID);
            end;
          end;

          if mat = nil then
          begin
            mat := FTextureManager.GetArtMaterial(cell.TileID);
            if (not (ghostTile is TMapCell)) or
               (TObject(draw[i]) = ghostTile) then //when we have a ghosttile, only draw that, but still store the real one
            begin
              glBindTexture(GL_TEXTURE_2D, mat.Texture);
              glBegin(GL_QUADS);
                glTexCoord2f(0, 0); glVertex2d(drawX - 22, drawY - z * 4);
                glTexCoord2f(1, 0); glVertex2d(drawX - 22 + mat.Width, drawY - z * 4);
                glTexCoord2f(1, 1); glVertex2d(drawX - 22 + mat.Width, drawY + mat.Height - z * 4);
                glTexCoord2f(0, 1); glVertex2d(drawX - 22, drawY + mat.Height - z * 4);
              glEnd;
            end;
            
            if TObject(draw[i]) <> ghostTile then
              FScreenBuffer.Store(Bounds(Trunc(drawX - 22), Trunc(drawY - z * 4), 44, 44), cell, mat);
          end else // Texture found
          begin
            if (not (ghostTile is TMapCell)) or
               (TObject(draw[i]) = ghostTile) then //when we have a ghosttile, only draw that, but still store the real one
            begin
              glBindTexture(GL_TEXTURE_2D, mat.Texture);
              //if (not cell.Selected) and (intensity = 1.0) then
              if not cell.Selected then
                glEnable(GL_LIGHTING);
              normals := FLandscape.Normals[offsetX, offsetY];
              glBegin(GL_TRIANGLES);
                glNormal3f(normals[3].X, normals[3].Y, normals[3].Z);
                glTexCoord2f(0, 1); glVertex2d(drawX - 22, drawY + 22 - west * 4);
                glNormal3f(normals[0].X, normals[0].Y, normals[0].Z);
                glTexCoord2f(0, 0); glVertex2d(drawX, drawY - z * 4);
                glNormal3f(normals[1].X, normals[1].Y, normals[1].Z);
                glTexCoord2f(1, 0); glVertex2d(drawX + 22, drawY + 22 - east * 4);
                glNormal3f(normals[1].X, normals[1].Y, normals[1].Z);
                glTexCoord2f(1, 0); glVertex2d(drawX + 22, drawY + 22 - east * 4);
                glNormal3f(normals[2].X, normals[2].Y, normals[2].Z);
                glTexCoord2f(1, 1); glVertex2d(drawX, drawY + 44 - south * 4);
                glNormal3f(normals[3].X, normals[3].Y, normals[3].Z);
                glTexCoord2f(0, 1); glVertex2d(drawX - 22, drawY + 22 - west * 4);
              glEnd;
              //if (not cell.Selected) and (intensity = 1.0) then
              if not cell.Selected then
                glDisable(GL_LIGHTING);
            end;
              
            if TObject(draw[i]) <> ghostTile then
              FScreenBuffer.Store(Rect(Trunc(drawX - 22), Trunc(drawY - z * 4), Trunc(drawX + 22), Trunc(drawY + 44 - south * 4)), cell, mat);
          end;
        end else if TObject(draw[i]) is TStaticItem then
        begin
          staticItem := TStaticItem(draw[i]);

          staticTileData := ResMan.Tiledata.StaticTiles[staticItem.TileID];
          if tbSetHue.Down and ((singleTarget and (TObject(draw[i]) = CurrentTile)) or multiTarget) then
          begin
            if frmHueSettings.lbHue.ItemIndex > 0 then
              hue := ResMan.Hue.Hues[frmHueSettings.lbHue.ItemIndex - 1]
            else
              hue := nil;
          end else if staticItem.Hue > 0 then
            hue := ResMan.Hue.Hues[staticItem.Hue - 1]
          else
            hue := nil;
            
          if staticTileData.HasFlag(tdfTranslucent) then
            glColor4f(intensity, intensity, intensity, 0.5);
            
          mat := FTextureManager.GetArtMaterial($4000 + staticItem.TileID, hue, (staticTileData.Flags and tdfPartialHue) = tdfPartialHue);
          south := mat.RealHeight;
          east := mat.RealWidth div 2;
          glBindTexture(GL_TEXTURE_2D, mat.Texture);
          glBegin(GL_QUADS);
            glTexCoord2f(0, 0); glVertex2d(drawX - east, drawY + 44 - south - z * 4);
            glTexCoord2f(1, 0); glVertex2d(drawX - east + mat.Width, drawY + 44 - south - z * 4);
            glTexCoord2f(1, 1); glVertex2d(drawX - east + mat.Width, drawY + 44 - south + mat.Height - z * 4);
            glTexCoord2f(0, 1); glVertex2d(drawX - east, drawY + 44 - south + mat.Height - z * 4);
          glEnd;
          
          if TObject(draw[i]) <> ghostTile then
            FScreenBuffer.Store(Bounds(Trunc(drawX - east), Trunc(drawY + 44 - south - z * 4), mat.RealWidth, Trunc(south)), staticItem, mat);
        end;

        if highlight then
          glDisable(GL_COLOR_LOGIC_OP);
      end;
      draw.Free;
    end;
  end;

  FOverlayUI.Draw(oglGameWindow);
end;

procedure TfrmMain.OnLandscapeChanged;
begin
  oglGameWindow.Repaint;
  UpdateCurrentTile;
end;

procedure TfrmMain.BuildTileList;
var
  minID, maxID, i, lastID: Integer;
  node: PVirtualNode;
  tileInfo: PTileInfo;
  filter: string;
begin
  if cbTerrain.Checked then minID := $0 else minID := $4000;
  if cbStatics.Checked then maxID := $7FFF else maxID := $3FFF;
  filter := AnsiLowerCase(edFilter.Text);
  
  node := vdtTiles.GetFirstSelected;
  if node <> nil then
  begin
    tileInfo := vdtTiles.GetNodeData(node);
    lastID := tileInfo^.ID;
  end else
    lastID := -1;
  
  vdtTiles.BeginUpdate;
  vdtTiles.Clear;
  
  for i := minID to maxID do
  begin
    if ResMan.Art.Exists(i) then
    begin
      if (filter <> '') and (Pos(filter, AnsiLowerCase(TTileData(ResMan.Tiledata.Block[i]).TileName)) = 0) then Continue;
      node := vdtTiles.AddChild(nil);
      tileInfo := vdtTiles.GetNodeData(node);
      tileInfo^.ID := i;
      if i = lastID then
        vdtTiles.Selected[node] := True;
    end;
  end;
  
  if vdtTiles.GetFirstSelected = nil then
  begin
    node := vdtTiles.GetFirst;
    if node <> nil then
      vdtTiles.Selected[node] := True;
  end;
  
  vdtTiles.EndUpdate;
  
  node := vdtTiles.GetFirstSelected;
  if node <> nil then
    vdtTiles.FocusedNode := node;
end;

procedure TfrmMain.ProcessToolState;
var
  node: PVirtualNode;
  tileInfo: PTileInfo;
  i: Integer;
begin
  if acSelect.Checked then
  begin
    //lblTip.Caption := 'Right click shows a menu with all the tools.';
    lblTip.Caption := 'Press and hold the left mouse button to show a list with actions (eg. grab hue).';
    oglGameWindow.Cursor := crDefault;
  end else
  begin
    lblTip.Caption := 'Press and hold the left mouse button to target an area.';
    oglGameWindow.Cursor := crHandPoint;
  end;

  if FGhostTile <> nil then FreeAndNil(FGhostTile);

  if acDraw.Checked then
  begin
    tileInfo := nil;
    if frmDrawSettings.rbTileList.Checked then
    begin
      node := vdtTiles.GetFirstSelected;
      if node <> nil then
        tileInfo := vdtTiles.GetNodeData(node);
    end else if frmDrawSettings.rbRandom.Checked then
    begin
      node := vdtRandom.GetFirst;
      for i := 1 to Random(vdtRandom.RootNodeCount) do
        node := vdtRandom.GetNext(node);

      if node <> nil then
        tileInfo := vdtRandom.GetNodeData(node);
    end;

    if tileInfo <> nil then
    begin
      if tileInfo^.ID < $4000 then
      begin
        FGhostTile := TMapCell.Create(nil, nil, 0, 0);
        FGhostTile.TileID := tileInfo^.ID;
      end else
      begin
        FGhostTile := TStaticItem.Create(nil, nil, 0, 0);
        FGhostTile.TileID := tileInfo^.ID - $4000;
        TStaticItem(FGhostTile).Hue := frmHueSettings.lbHue.ItemIndex;
      end;
    end;
  end;
end;

procedure TfrmMain.ProcessAccessLevel;
begin
  mnuAdministration.Visible := (dmNetwork.AccessLevel >= alAdministrator);
  acDraw.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acMove.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acElevate.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acDelete.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acHue.Enabled := (dmNetwork.AccessLevel >= alNormal);
  Caption := Format('UO CentrED - [%s (%s)]', [dmNetwork.Username, GetAccessLevelString(dmNetwork.AccessLevel)]);
end;

procedure TfrmMain.UpdateCurrentTile;
var
  localPos: TPoint;
begin
  if oglGameWindow.MouseEntered then
  begin
    localPos := oglGameWindow.ScreenToClient(Mouse.CursorPos);
    UpdateCurrentTile(localPos.X, localPos.Y);
  end;
end;

procedure TfrmMain.UpdateCurrentTile(AX, AY: Integer);
var
  info: PBlockInfo;
begin
  FOverlayUI.ActiveArrow := FOverlayUI.HitTest(AX, AY);
  if FOverlayUI.ActiveArrow > -1 then Exit;

  info := FScreenBuffer.Find(Point(AX, AY));
  if info <> nil then
  begin
    CurrentTile := info^.Item;

    if CurrentTile is TVirtualTile then
      lblTileInfo.Caption := Format('Virtual Layer: X: %d, Y: %d, Z: %d', [CurrentTile.X, CurrentTile.Y, CurrentTile.Z])
    else if CurrentTile is TMapCell then
      lblTileInfo.Caption := Format('Terrain TileID: $%x, X: %d, Y: %d, Z: %d', [CurrentTile.TileID, CurrentTile.X, CurrentTile.Y, CurrentTile.Z])
    else
      lblTileInfo.Caption := Format('Static TileID: $%x, X: %d, Y: %d, Z: %d, Hue: $%x', [CurrentTile.TileID, CurrentTile.X, CurrentTile.Y, CurrentTile.Z, TStaticItem(CurrentTile).Hue]);
      
    if (acDraw.Checked) and (SelectedTile = nil) then
    begin
      if FGhostTile <> nil then
      begin
        if (FGhostTile is TStaticItem) and (not frmDrawSettings.cbForceAltitude.Checked) then
        begin
          FGhostTile.Z := CurrentTile.Z;
          if CurrentTile is TStaticItem then
            Inc(FGhostTile.Z, ResMan.Tiledata.StaticTiles[CurrentTile.TileID].Height);
        end else
          FGhostTile.Z := frmDrawSettings.seForceAltitude.Value;
      end;
    end;
  end;
end;

procedure TfrmMain.TileRemoved(ATile: TMulBlock);
begin
  if ATile = FCurrentTile then
    FCurrentTile := nil
  else if ATile = FSelectedTile then
    FSelectedTile := nil;
end;

procedure TfrmMain.WriteChatMessage(ASender, AMessage: string);
var
  node: PVirtualNode;
  chatInfo: PChatInfo;
begin
  node := vstChat.AddChild(nil);
  chatInfo := vstChat.GetNodeData(node);
  chatInfo^.Time := Now;
  chatInfo^.Sender := ASender;
  chatInfo^.Msg := AMessage;
  if vstChat.RootNodeCount > 30 then
    vstChat.DeleteNode(vstChat.GetFirst);
  vstChat.ScrollIntoView(node, False);
  
  if not pnlChat.Visible then
  begin
    lblChatHeaderCaption.Font.Bold := True;
    lblChatHeaderCaption.Font.Italic := True;
    lblChatHeaderCaption.Font.Color := clRed;
  end;
end;

procedure TfrmMain.PrepareVirtualLayer(AWidth, AHeight: Word);
var
  oldWidth, oldHeight: Word;
  i, j: Integer;
begin
  for i := Low(FVirtualLayer) to High(FVirtualLayer) do
  begin
    if AHeight < Length(FVirtualLayer[i]) then
    begin
      for j := AHeight to Length(FVirtualLayer[i]) - 1 do
        FVirtualLayer[i][j].Free;
      SetLength(FVirtualLayer[i], AHeight);
    end else if AHeight > Length(FVirtualLayer[i]) then
    begin
      oldHeight := Length(FVirtualLayer[i]);
      SetLength(FVirtualLayer[i], AHeight);
      for j := oldHeight to AHeight - 1 do
      begin
        FVirtualLayer[i][j] := TVirtualTile.Create(nil, nil, 0, 0);
        FVirtualLayer[i][j].TileID := 0;
        FVirtualLayer[i][j].Hue := 0;
      end;
    end;
  end;

  if AWidth < Length(FVirtualLayer) then
  begin
    for i := AWidth to Length(FVirtualLayer) - 1 do
    begin
      for j := Low(FVirtualLayer[i]) to High(FVirtualLayer[i]) do
        FVirtualLayer[i][j].Free;
    end;
    SetLength(FVirtualLayer, AWidth);
  end else if AWidth > Length(FVirtualLayer) then
  begin
    oldWidth := Length(FVirtualLayer);
    SetLength(FVirtualLayer, AWidth);
    for i := oldWidth to AWidth - 1 do
    begin
      SetLength(FVirtualLayer[i], AHeight);
      for j := Low(FVirtualLayer[i]) to High(FVirtualLayer[i]) do
      begin
        FVirtualLayer[i][j] := TVirtualTile.Create(nil, nil, 0, 0);
        FVirtualLayer[i][j].TileID := 0;
        FVirtualLayer[i][j].Hue := 0;
      end;
    end;
  end;
end;

procedure TfrmMain.OnClientHandlingPacket(ABuffer: TEnhancedMemoryStream);
var
  sender, msg: string;
  i: Integer;
  accessLevel: TAccessLevel;
begin
  case ABuffer.ReadByte of
    $01: //client connected
      begin
        sender := ABuffer.ReadStringNull;
        lbClients.Items.Add(sender);
        if sender <> dmNetwork.Username then
          WriteChatMessage('System', Format('User "%s" has connected.', [sender]));
      end;
    $02:
      begin
        sender := ABuffer.ReadStringNull;
        lbClients.Items.Delete(lbClients.Items.IndexOf(sender));
        if sender <> dmNetwork.Username then
          WriteChatMessage('System', Format('User "%s" has disconnected.', [sender]));
      end;
    $03: //Client list
      begin
        lbClients.Clear;
        while ABuffer.Position < ABuffer.Size do
          lbClients.Items.Add(ABuffer.ReadStringNull);
      end;
    $04: //Set pos
      begin
        FX := ABuffer.ReadWord;
        FY := ABuffer.ReadWord;
        SetPos(FX, FY);
      end;
    $05: //chat
      begin
        sender := ABuffer.ReadStringNull;
        msg := ABuffer.ReadStringNull;
        WriteChatMessage(sender, msg);
      end;
    $07: //access changed
      begin
        accessLevel := TAccessLevel(ABuffer.ReadByte);
        dmNetwork.UpdateWriteMap(ABuffer);

        if accessLevel <> dmNetwork.AccessLevel then
        begin
          dmNetwork.AccessLevel := accessLevel;
          if accessLevel = alNone then
          begin
            MessageDlg('AccessLevel change', 'Your account has been locked.', mtWarning, [mbOK], 0);
            mnuDisconnectClick(nil);
          end else
          begin
            ProcessAccessLevel;
            MessageDlg('AccessLevel change', Format('Your accesslevel has been changed to %s.', [GetAccessLevelString(accessLevel)]), mtWarning, [mbOK], 0);
          end;
        end;

        for i := Low(FAccessChangedListeners) to High(FAccessChangedListeners) do
          FAccessChangedListeners[i](accessLevel);
      end;
  end;
end;

function TfrmMain.GetInternalTileID(ATile: TWorldItem): Word;
begin
  Result := ATile.TileID;
  if ATile is TStaticItem then
    Inc(Result, $4000);
end;

function TfrmMain.GetSelectedRect: TRect;
begin
  if CurrentTile <> nil then
  begin
    if SelectedTile <> nil then
    begin
      Result.Left := Min(CurrentTile.X, SelectedTile.X);
      Result.Top := Min(CurrentTile.Y, SelectedTile.Y);
      Result.Right := Max(CurrentTile.X, SelectedTile.X) + 1;
      Result.Bottom := Max(CurrentTile.Y, SelectedTile.Y) + 1;
    end else
    begin
      Result.Left := CurrentTile.X;
      Result.Top := CurrentTile.Y;
      Result.Right := CurrentTile.X + 1;
      Result.Bottom := CurrentTile.Y + 1;
    end;
  end;
end;

function TfrmMain.ConfirmAction: Boolean;
begin
  if acMove.Checked and frmMoveSettings.cbAsk.Checked then
  begin
    frmMoveSettings.Left := Mouse.CursorPos.x - 8;
    frmMoveSettings.Top := Mouse.CursorPos.y - 8;
    Result := frmMoveSettings.ShowModal = mrYes;
  end else
  begin
    frmConfirmation.Left := Mouse.CursorPos.x - frmConfirmation.btnYes.Left - frmConfirmation.btnYes.Width div 2;
    frmConfirmation.Top := Mouse.CursorPos.y - frmConfirmation.btnYes.Top - frmConfirmation.btnYes.Height div 2;
    Result := frmConfirmation.ShowModal = mrYes;
  end;
  if not oglGameWindow.MouseEntered then
    oglGameWindowMouseLeave(nil);
end;

function TfrmMain.CanBeModified(ATile: TWorldItem): Boolean;
begin
  Result := (not (ATile is TVirtualTile)) and
    dmNetwork.CanWrite(ATile.X, ATile.Y);
end;

initialization
  {$I UfrmMain.lrs}

end.

