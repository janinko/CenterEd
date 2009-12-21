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
unit UfrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, OpenGLContext, GL, GLu, UGameResources, ULandscape, ExtCtrls,
  StdCtrls, Spin, UEnums, VirtualTrees, Buttons, UMulBlock, UWorldItem, math,
  LCLIntf, UOverlayUI, UStatics, UEnhancedMemoryStream, ActnList,
  XMLPropStorage, fgl, ImagingClasses, dateutils, UPlatformTypes, UMap, UPacket,
  UGLFont, DOM, XMLRead, XMLWrite, strutils, ULightManager;

type
  TAccessChangedListener = procedure(AAccessLevel: TAccessLevel) of object;
  TSelectionListener = procedure(AWorldItem: TWorldItem) of object;
  TScreenBufferState = (sbsValid, sbsIndexed, sbsFiltered);
  TScreenBufferStates = set of TScreenBufferState;

  TGhostTile = class(TStaticItem);
  TPacketList = specialize TFPGObjectList<TPacket>;
  TAccessChangedListeners = specialize TFPGList<TAccessChangedListener>;
  TSelectionListeners = specialize TFPGList<TSelectionListener>;

  TTileHintInfo = record
    Name: String;
    Flags: String;
    NameRect: TRect;
    FlagsRect: TRect;
  end;

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
    acUndo: TAction;
    acVirtualLayer: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    btnAddLocation: TSpeedButton;
    btnAddRandom: TSpeedButton;
    btnClearLocations: TSpeedButton;
    btnClearRandom: TSpeedButton;
    btnDeleteLocation: TSpeedButton;
    btnDeleteRandom: TSpeedButton;
    btnGoTo: TButton;
    btnRandomPresetDelete: TSpeedButton;
    btnRandomPresetSave: TSpeedButton;
    cbRandomPreset: TComboBox;
    cbStatics: TCheckBox;
    cbTerrain: TCheckBox;
    edChat: TEdit;
    edFilter: TEdit;
    edSearchID: TEdit;
    gbRandom: TGroupBox;
    ImageList1: TImageList;
    lblChatHeaderCaption: TLabel;
    lblFilter: TLabel;
    lblTipC: TLabel;
    lblTip: TLabel;
    lblTileInfo: TLabel;
    lblX: TLabel;
    lblY: TLabel;
    lbClients: TListBox;
    MainMenu1: TMainMenu;
    mnuSecurityQuestion: TMenuItem;
    mnuShowAnimations: TMenuItem;
    mnuSettings: TMenuItem;
    mnuFlatShowHeight: TMenuItem;
    mnuGrabHue: TMenuItem;
    mnuGrabTileID: TMenuItem;
    mnuRegionControl: TMenuItem;
    mnuVirtualLayer: TMenuItem;
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
    pcLeft: TPageControl;
    pmGrabTileInfo: TPopupMenu;
    pnlBottom: TPanel;
    edX: TSpinEdit;
    edY: TSpinEdit;
    pmTileList: TPopupMenu;
    pmTools: TPopupMenu;
    pmClients: TPopupMenu;
    pnlChat: TPanel;
    pnlChatHeader: TPanel;
    pmFlatViewSettings: TPopupMenu;
    spChat: TSplitter;
    spTileList: TSplitter;
    tbFilter: TToolButton;
    tbFlat: TToolButton;
    tbNoDraw: TToolButton;
    tbSeparator2: TToolButton;
    tbUndo: TToolButton;
    tsLocations: TTabSheet;
    tbSetHue: TToolButton;
    tmGrabTileInfo: TTimer;
    tmMovement: TTimer;
    tbSeparator5: TToolButton;
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
    tbSeparator3: TToolButton;
    tbBoundaries: TToolButton;
    tbSeparator4: TToolButton;
    tbTerrain: TToolButton;
    tbStatics: TToolButton;
    tsTiles: TTabSheet;
    vdtTiles: TVirtualDrawTree;
    vdtRandom: TVirtualDrawTree;
    vstChat: TVirtualStringTree;
    vstLocations: TVirtualStringTree;
    XMLPropStorage1: TXMLPropStorage;
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
    procedure acUndoExecute(Sender: TObject);
    procedure acVirtualLayerExecute(Sender: TObject);
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure ApplicationProperties1ShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
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
    procedure mnuFlatShowHeightClick(Sender: TObject);
    procedure mnuFlushClick(Sender: TObject);
    procedure mnuGoToClientClick(Sender: TObject);
    procedure mnuGrabHueClick(Sender: TObject);
    procedure mnuGrabTileIDClick(Sender: TObject);
    procedure mnuLargeScaleCommandsClick(Sender: TObject);
    procedure mnuRegionControlClick(Sender: TObject);
    procedure mnuShowAnimationsClick(Sender: TObject);
    procedure mnuShutdownClick(Sender: TObject);
    procedure oglGameWindowDblClick(Sender: TObject);
    procedure oglGameWindowKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
    procedure oglGameWindowResize(Sender: TObject);
    procedure pmGrabTileInfoPopup(Sender: TObject);
    procedure tbFilterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tbRadarMapClick(Sender: TObject);
    procedure tbStaticsClick(Sender: TObject);
    procedure tbTerrainClick(Sender: TObject);
    procedure tmGrabTileInfoTimer(Sender: TObject);
    procedure tmMovementTimer(Sender: TObject);
    procedure vdtRandomClick(Sender: TObject);
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
    procedure vdtTilesDrawHint(Sender: TBaseVirtualTree; HintCanvas: TCanvas;
      Node: PVirtualNode; const R: TRect; Column: TColumnIndex);
    procedure vdtTilesDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure vdtTilesEnter(Sender: TObject);
    procedure vdtTilesGetHintSize(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var R: TRect);
    procedure vdtTilesKeyPress(Sender: TObject; var Key: char);
    procedure vdtTilesScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure vstChatClick(Sender: TObject);
    procedure vstChatFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstChatGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstChatPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vstLocationsDblClick(Sender: TObject);
    procedure vstLocationsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode
      );
    procedure vstLocationsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstLocationsLoadNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure vstLocationsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
    procedure vstLocationsSaveNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure XMLPropStorage1RestoreProperties(Sender: TObject);
  protected
    { Members }
    FAppDir: String;
    FConfigDir: String;
    FX: Integer;
    FY: Integer;
    FDrawDistance: Integer;
    FLowOffsetX: Integer;
    FLowOffsetY: Integer;
    FHighOffsetX: Integer;
    FHighOffsetY: Integer;
    FRangeX: Integer;
    FRangeY: Integer;
    FLandscape: TLandscape;
    FTextureManager: TLandTextureManager;
    FScreenBuffer: TScreenBuffer;
    FScreenBufferState: TScreenBufferStates;
    FCurrentTile: TWorldItem;
    FSelectedTile: TWorldItem;
    FVirtualTiles: TWorldItemList;
    FVLayerImage: TSingleImage;
    FVLayerMaterial: TMaterial;
    FOverlayUI: TOverlayUI;
    FLocationsFile: string;
    FRandomPresetsFile: string;
    FRandomPresetsDoc: TXMLDocument;
    FLastDraw: TDateTime;
    FAccessChangedListeners: TAccessChangedListeners;
    FRepaintNeeded: Boolean;
    FSelection: TRect;
    FUndoList: TPacketList;
    FGLFont: TGLFont;
    FSelectionListeners: TSelectionListeners;
    FTileHint: TTileHintInfo;
    FLightManager: TLightManager;
    { Methods }
    procedure BuildTileList;
    function  ConfirmAction: Boolean;
    function  FindRandomPreset(AName: String): TDOMElement;
    procedure ForceUpdateCurrentTile;
    procedure GetDrawOffset(AX, AY: Integer; out DrawX, DrawY: Integer); inline;
    function  GetInternalTileID(ATile: TWorldItem): Word;
    function  GetSelectedRect: TRect;
    procedure InitRender;
    procedure InitSize;
    procedure LoadLocations;
    procedure LoadRandomPresets;
    procedure MoveBy(AOffsetX, AOffsetY: Integer); inline;
    procedure PrepareMapCell(AMapCell: TMapCell);
    procedure PrepareScreenBlock(ABlockInfo: PBlockInfo);
    procedure ProcessToolState;
    procedure ProcessAccessLevel;
    procedure RebuildScreenBuffer;
    procedure Render;
    procedure SaveLocations;
    procedure SaveRandomPresets;
    procedure SetCurrentTile(const AValue: TWorldItem);
    procedure SetDarkLights; inline;
    procedure SetNormalLights; inline;
    procedure SetSelectedTile(const AValue: TWorldItem);
    procedure SetX(const AValue: Integer);
    procedure SetY(const AValue: Integer);
    procedure UpdateCurrentTile;
    procedure UpdateCurrentTile(AX, AY: Integer);
    procedure UpdateFilter;
    procedure UpdateSelection;
    procedure WriteChatMessage(ASender, AMessage: string);
    { Events }
    procedure OnClientHandlingPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnLandscapeChanged;
    procedure OnMapChanged(AMapCell: TMapCell);
    procedure OnNewBlock(ABlock: TBlock);
    procedure OnStaticDeleted(AStaticItem: TStaticItem);
    procedure OnStaticElevated(AStaticItem: TStaticItem);
    procedure OnStaticHued(AStaticItem: TStaticItem);
    procedure OnStaticInserted(AStaticItem: TStaticItem);
    procedure OnTileRemoved(ATile: TMulBlock);
  public
    { Fields }
    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
    property Landscape: TLandscape read FLandscape;
    property CurrentTile: TWorldItem read FCurrentTile write SetCurrentTile;
    property SelectedTile: TWorldItem read FSelectedTile write SetSelectedTile;
    { Methods }
    procedure InvalidateFilter;
    procedure InvalidateScreenBuffer;
    procedure RegisterAccessChangedListener(AListener: TAccessChangedListener);
    procedure RegisterSelectionListener(AListener: TSelectionListener);
    procedure SetPos(AX, AY: Word);
    procedure SwitchToSelection;
    procedure UnregisterAccessChangedListener(AListener: TAccessChangedListener);
    procedure UnregisterSelectionListener(AListener: TSelectionListener);
  end; 

var
  frmMain: TfrmMain;

implementation

uses
  UdmNetwork, UArt, UTiledata, UAdminHandling, UPackets,
  UfrmAccountControl, UGraphicHelper, ImagingComponents, UfrmDrawSettings,
  UfrmBoundaries, UfrmElevateSettings, UfrmConfirmation, UfrmMoveSettings,
  UfrmAbout, UPacketHandlers, UfrmHueSettings, UfrmRadar, UfrmLargeScaleCommand,
  UfrmLogin, UResourceManager, UfrmVirtualLayer, UfrmFilter, UfrmRegionControl,
  Logging, LConvEncoding, LCLType;

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

const
  CScreenBufferValid = [sbsValid, sbsIndexed, sbsFiltered];

function IsInRect(const AX, AY: Integer; const ARect: TRect): Boolean; inline;
begin
  Result := (AX >= ARect.Left) and
            (AX <= ARect.Right) and
            (AY >= ARect.Top) and
            (AY <= ARect.Bottom);
end;

{ TfrmMain }

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuFlatShowHeightClick(Sender: TObject);
begin
  RebuildScreenBuffer;
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

procedure TfrmMain.mnuShowAnimationsClick(Sender: TObject);
begin
  FTextureManager.UseAnims := mnuShowAnimations.Checked;
  RebuildScreenBuffer;
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

procedure TfrmMain.oglGameWindowKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_W, VK_NUMPAD8, VK_UP:
      MoveBy(-8, -8);
    VK_S, VK_NUMPAD2, VK_DOWN:
      MoveBy(8, 8);
    VK_A, VK_NUMPAD4, VK_LEFT:
      MoveBy(-8, 8);
    VK_D, VK_NUMPAD6, VK_RIGHT:
      MoveBy(8, -8);
    VK_Q, VK_NUMPAD7:
      MoveBy(-8, 0);
    VK_E, VK_NUMPAD9:
      MoveBy(0, -8);
    VK_Y, VK_NUMPAD1:
      MoveBy(0, 8);
    VK_C, VK_NUMPAD3:
      MoveBy(8, 0);
  end;
end;

procedure TfrmMain.oglGameWindowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Logger.EnterMethod([lcClient, lcDebug], 'MouseDown');
  try
    if Button = mbRight then
      pmTools.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);

    if Button <> mbLeft then
      Exit;

    UpdateCurrentTile(X, Y);
    if FOverlayUI.ActiveArrow > -1 then
      tmMovement.Enabled := True;

    SelectedTile := CurrentTile;
    if CurrentTile = nil then Exit;

    if acSelect.Checked then                      //***** Selection Mode *****//
      tmGrabTileInfo.Enabled := True;

    FRepaintNeeded := True;
  finally
    Logger.ExitMethod([lcClient, lcDebug], 'MouseDown');
  end;
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

  FRepaintNeeded := True;
end;

procedure TfrmMain.oglGameWindowMouseLeave(Sender: TObject);
begin
  if not (frmConfirmation.Visible or
          (frmMoveSettings.Visible and (fsModal in frmMoveSettings.FormState))
         ) then //during confirmation the mouse would leave ...
  begin
    CurrentTile := nil;
    FOverlayUI.Visible := False;
  end;

  FRepaintNeeded := True;
end;

procedure TfrmMain.oglGameWindowMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  lastTile: TWorldItem;
  offsetX, offsetY: Integer;
begin
  Logger.EnterMethod([lcClient, lcDebug], 'MouseMove');
  lastTile := CurrentTile;
  
  if ssMiddle in Shift then
  begin
    UpdateCurrentTile(X, Y);
    if (lastTile <> nil) and (CurrentTile <> nil) and (lastTile <> CurrentTile) then
    begin
      offsetX := lastTile.X - CurrentTile.X;
      offsetY := lastTile.Y - CurrentTile.Y;
      if InRange(offsetX, -8, 8) and InRange(offsetY, -8, 8) then
        SetPos(FX - offsetX, FY - offsetY);
    end;
  end;

  UpdateCurrentTile(X, Y);

  FRepaintNeeded := True;
  Logger.ExitMethod([lcClient, lcDebug], 'MouseMove');
end;

procedure TfrmMain.oglGameWindowMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  map: TMapCell;
  i: Integer;
  z: ShortInt;
  blockInfo: PBlockInfo;
  targetRect: TRect;
  offsetX, offsetY: Integer;
  tile: TWorldItem;
  tileX, tileY, newX, newY: Word;
  targetTiles: TWorldItemList;
  targetTile: TWorldItem;
begin
  Logger.EnterMethod([lcClient, lcDebug], 'MouseUp');
  if Button <> mbLeft then
  begin
    Logger.ExitMethod([lcClient, lcDebug], 'MouseUp');
    Exit;
  end;

  UpdateCurrentTile(X, Y);
  tmMovement.Enabled := False;
  if CurrentTile = nil then
  begin
    SelectedTile := nil;
    Logger.ExitMethod([lcClient, lcDebug], 'MouseUp');
    Exit;
  end;

  targetTile := CurrentTile;
  
  if acSelect.Checked then
  begin
    if tmGrabTileInfo.Enabled then
    begin
      tmGrabTileInfo.Enabled := False;
      mnuGrabTileIDClick(nil);
    end;

    for i := FSelectionListeners.Count - 1 downto 0 do
      FSelectionListeners[i](CurrentTile);
  end;

  if (not acSelect.Checked) and (targetTile <> nil) and (SelectedTile <> nil) then
  begin
    targetRect := GetSelectedRect;
    FUndoList.Clear;

    if (SelectedTile = targetTile) or ConfirmAction then
    begin
      if acDraw.Checked then                        //***** Drawing Mode *****//
      begin
        for tileX := FSelection.Left to FSelection.Right do
          for tileY := FSelection.Top to FSelection.Bottom do
          begin
            map := FLandscape.MapCell[tileX, tileY];
            if map.IsGhost then
            begin
              FUndoList.Add(TDrawMapPacket.Create(tileX, tileY, map.RawZ,
                map.RawTileID));
              dmNetwork.Send(TDrawMapPacket.Create(tileX, tileY, map.Z,
                map.TileID));
            end;
          end;

        Logger.Send([lcClient, lcDebug], 'Virtual tiles', FVirtualTiles.Count);
        for i := 0 to FVirtualTiles.Count - 1 do
        begin
          tile := FVirtualTiles[i];
          if tile is TGhostTile then
          begin
            dmNetwork.Send(TInsertStaticPacket.Create(tile.X, tile.Y, tile.Z,
              tile.TileID, TGhostTile(tile).Hue));
            FUndoList.Add(TDeleteStaticPacket.Create(TGhostTile(tile)));
          end;
        end;
      end else if (SelectedTile <> targetTile) or targetTile.CanBeEdited then
      begin
        if (not acMove.Checked) or (SelectedTile <> targetTile) or
           (not frmMoveSettings.cbAsk.Checked) or ConfirmAction then
        begin
          targetTiles := TWorldItemList.Create(False);
          if SelectedTile = targetTile then
          begin
            targetTiles.Add(targetTile)
          end else
          begin
            blockInfo := nil;
            while FScreenBuffer.Iterate(blockInfo) do
            begin
              if (blockInfo^.State = ssNormal) and
                blockInfo^.Item.CanBeEdited and
                IsInRect(blockInfo^.Item.X, blockInfo^.Item.Y, targetRect) then
              begin
                targetTiles.Add(blockInfo^.Item);
              end;
            end;
          end;

          if acMove.Checked then                       //***** Move tile *****//
          begin
            offsetX := frmMoveSettings.GetOffsetX;
            offsetY := frmMoveSettings.GetOffsetY;
            for i := 0 to targetTiles.Count - 1 do
            begin
              tile := targetTiles.Items[i];

              if tile is TStaticItem then
              begin
                newX := EnsureRange(tile.X + offsetX, 0, FLandscape.CellWidth - 1);
                newY := EnsureRange(tile.Y + offsetY, 0, FLandscape.CellHeight - 1);
                FUndoList.Add(TMoveStaticPacket.Create(newX, newY, tile.Z,
                  tile.TileID, TStaticItem(tile).Hue, tile.X, tile.Y));
                dmNetwork.Send(TMoveStaticPacket.Create(TStaticItem(tile),
                  newX, newY));
              end;
            end;
          end else if acElevate.Checked then        //***** Elevate tile *****//
          begin
            for i := 0 to targetTiles.Count - 1 do
            begin
              tile := targetTiles.Items[i];

              z := frmElevateSettings.seZ.Value;
              if frmElevateSettings.rbRaise.Checked then
                z := EnsureRange(tile.Z + z, -128, 127)
              else if frmElevateSettings.rbLower.Checked then
                z := EnsureRange(tile.Z - z, -128, 127);

              if tile is TMapCell then
              begin
                if frmElevateSettings.cbRandomHeight.Checked then
                  Inc(z, Random(frmElevateSettings.seRandomHeight.Value));
                FUndoList.Add(TDrawMapPacket.Create(tile.X, tile.Y, tile.Z,
                  tile.TileID));
                dmNetwork.Send(TDrawMapPacket.Create(tile.X, tile.Y, z,
                  tile.TileID));
              end else
              begin
                FUndoList.Add(TElevateStaticPacket.Create(tile.X, tile.Y,
                  z, tile.TileID, TStaticItem(tile).Hue, tile.Z));
                dmNetwork.Send(TElevateStaticPacket.Create(TStaticItem(tile), z));
              end;
            end;
          end else if acDelete.Checked then          //***** Delete tile *****//
          begin
            for i := 0 to targetTiles.Count - 1 do
            begin
              tile := targetTiles.Items[i];

              if tile is TStaticItem then
              begin
                FUndoList.Add(TInsertStaticPacket.Create(tile.X, tile.Y,
                  tile.Z, tile.TileID, TStaticItem(tile).Hue));
                dmNetwork.Send(TDeleteStaticPacket.Create(TStaticItem(tile)));
              end;
            end;
          end else if acHue.Checked then                //***** Hue tile *****//
          begin
            for i := 0 to targetTiles.Count - 1 do
            begin
              tile := targetTiles.Items[i];

              if (tile is TStaticItem) and
                (TStaticItem(tile).Hue <> frmHueSettings.lbHue.ItemIndex) then
              begin
                FUndoList.Add(THueStaticPacket.Create(tile.X, tile.Y, tile.Z,
                  tile.TileID, frmHueSettings.lbHue.ItemIndex,
                  TStaticItem(tile).Hue));
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
  acUndo.Enabled := FUndoList.Count > 0;
  SelectedTile := nil;
  FRepaintNeeded := True;
  Logger.ExitMethod([lcClient, lcDebug], 'MouseUp');
end;

procedure TfrmMain.oglGameWindowMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  cursorNeedsUpdate: Boolean;
  newZ: ShortInt;
begin
  if CurrentTile = nil then
    Exit;

  //We want single steps ...
  WheelDelta := WheelDelta div WHEEL_DELTA;

  cursorNeedsUpdate := False;
  if (CurrentTile is TVirtualTile) or ((ssCtrl in Shift) and
     (frmVirtualLayer.cbShowLayer.Checked)) then
  begin
    frmVirtualLayer.seZ.Value := EnsureRange(frmVirtualLayer.seZ.Value +
      WheelDelta, -128, 127);
    frmVirtualLayer.seZChange(frmVirtualLayer.seZ);
    cursorNeedsUpdate := True;
    Handled := True;
  end else if not (ssCtrl in Shift) then
  begin
    FUndoList.Clear;
    newZ := EnsureRange(CurrentTile.Z + WheelDelta, -128, 127);
    if CurrentTile is TStaticItem then
    begin
      FUndoList.Add(TElevateStaticPacket.Create(CurrentTile.X, CurrentTile.Y,
        newZ, CurrentTile.TileID, TStaticItem(CurrentTile).Hue,
        CurrentTile.Z));
      dmNetwork.Send(TElevateStaticPacket.Create(TStaticItem(CurrentTile),
        newZ));
      cursorNeedsUpdate := True;
      Handled := True;
    end else if CurrentTile is TMapCell then
    begin
      FUndoList.Add(TDrawMapPacket.Create(CurrentTile.X, CurrentTile.Y,
        CurrentTile.Z, CurrentTile.TileID));
      dmNetwork.Send(TDrawMapPacket.Create(CurrentTile.X, CurrentTile.Y,
        newZ, CurrentTile.TileID));
      Handled := True;
    end;
    acUndo.Enabled := FUndoList.Count > 0;
  end;
  
  if cursorNeedsUpdate then
  begin
    SetCursorPos(Mouse.CursorPos.X, Mouse.CursorPos.Y - 4 * WheelDelta);
    UpdateCurrentTile(MousePos.X, MousePos.Y - 4 * WheelDelta);
  end;

  FRepaintNeeded := True;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FAppDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  FConfigDir := GetAppConfigDir(False);
  ForceDirectories(FConfigDir);

  XMLPropStorage1.FileName := FConfigDir + 'CentrED.xml';
  XMLPropStorage1.Active := True;

  FLandscape := ResMan.Landscape;
  FLandscape.OnChange := @OnLandscapeChanged;
  FLandscape.OnMapChanged := @OnMapChanged;
  FLandscape.OnNewBlock := @OnNewBlock;
  FLandscape.OnStaticDeleted := @OnStaticDeleted;
  FLandscape.OnStaticElevated := @OnStaticElevated;
  FLandscape.OnStaticHued := @OnStaticHued;
  FLandscape.OnStaticInserted := @OnStaticInserted;

  if FileExists(FAppDir + 'nodraw.txt') then
    FLandscape.LoadNoDrawMap(FAppDir + 'nodraw.txt');
  if FileExists(FConfigDir + 'nodraw.txt') then
    FLandscape.LoadNoDrawMap(FConfigDir + 'nodraw.txt');
  if FileExists(ResMan.GetFile('nodraw.txt')) then
    FLandscape.LoadNoDrawMap(ResMan.GetFile('nodraw.txt'));

  FTextureManager := TLandTextureManager.Create;
  FScreenBuffer := TScreenBuffer.Create;
  FScreenBufferState := [];
  X := 0;
  Y := 0;
  edX.MaxValue := FLandscape.CellWidth;
  edY.MaxValue := FLandscape.CellHeight;
  FOverlayUI := TOverlayUI.Create;
  FLightManager := TLightManager.Create(@GetDrawOffset);
  
  ProcessAccessLevel;
  
  vdtTiles.NodeDataSize := SizeOf(TTileInfo);
  vdtRandom.NodeDataSize := SizeOf(TTileInfo);
  BuildTileList;
  Randomize;
  
  vstChat.NodeDataSize := SizeOf(TChatInfo);
  pnlChatHeader.AnchorSide[akBottom].Control := pnlBottom;
  
  FLocationsFile := FConfigDir + 'Locations.xml';
  vstLocations.NodeDataSize := SizeOf(TLocationInfo);
  LoadLocations;

  RegisterPacketHandler($0C, TPacketHandler.Create(0, @OnClientHandlingPacket));

  FVLayerImage := TSingleImage.CreateFromStream(ResourceManager.GetResource(2));

  FGLFont := TGLFont.Create;
  FGLFont.LoadImage(ResourceManager.GetResource(3));
  FGLFont.LoadFontInfo(ResourceManager.GetResource(4));

  FVirtualTiles := TWorldItemList.Create(True);
  FUndoList := TPacketList.Create(True);

  FRandomPresetsFile := FConfigDir + 'RandomPresets.xml';
  LoadRandomPresets;

  DoubleBuffered := True;
  pnlBottom.DoubleBuffered := True;

  FAccessChangedListeners := TAccessChangedListeners.Create;
  FSelectionListeners := TSelectionListeners.Create;
  
  FLastDraw := Now;
end;

procedure TfrmMain.btnGoToClick(Sender: TObject);
begin
  SetPos(edX.Value, edY.Value);
end;

procedure TfrmMain.btnRandomPresetDeleteClick(Sender: TObject);
var
  preset: TDOMElement;
begin
  if cbRandomPreset.ItemIndex > -1 then
  begin
    preset := TDOMElement(cbRandomPreset.Items.Objects[cbRandomPreset.ItemIndex]);
    FRandomPresetsDoc.DocumentElement.RemoveChild(preset);
    cbRandomPreset.Items.Delete(cbRandomPreset.ItemIndex);
    cbRandomPreset.ItemIndex := -1;
  end;
end;

procedure TfrmMain.btnRandomPresetSaveClick(Sender: TObject);
var
  presetName: string;
  i: Integer;
  preset, tile: TDOMElement;
  children: TDOMNodeList;
  tileNode: PVirtualNode;
  tileInfo: PTileInfo;
begin
  presetName := cbRandomPreset.Text;
  if InputQuery('Save Preset', 'Enter the name of the preset:', presetName) then
  begin
    preset := FindRandomPreset(presetName);
    if preset = nil then
    begin
      preset := FRandomPresetsDoc.CreateElement('Preset');
      preset.AttribStrings['Name'] := presetName;
      FRandomPresetsDoc.DocumentElement.AppendChild(preset);
      cbRandomPreset.Items.AddObject(presetName, preset);
    end else
    begin
      children := preset.ChildNodes;
      for i := children.Count - 1 downto 0 do
        preset.RemoveChild(children[i]);
    end;

    tileNode := vdtRandom.GetFirst;
    while tileNode <> nil do
    begin
      tileInfo := vdtRandom.GetNodeData(tileNode);
      tile := FRandomPresetsDoc.CreateElement('Tile');
      tile.AttribStrings['ID'] := IntToStr(tileInfo^.ID);
      preset.AppendChild(tile);
      tileNode := vdtRandom.GetNext(tileNode);
    end;

    cbRandomPreset.ItemIndex := cbRandomPreset.Items.IndexOfObject(preset);

    SaveRandomPresets;
  end;
end;

procedure TfrmMain.cbRandomPresetChange(Sender: TObject);
var
  preset, tile: TDOMElement;
  tiles: TDOMNodeList;
  tileNode: PVirtualNode;
  tileInfo: PTileInfo;
  i, id: Integer;
begin
  if cbRandomPreset.ItemIndex > -1 then
  begin
    vdtRandom.Clear;
    preset := TDOMElement(cbRandomPreset.Items.Objects[cbRandomPreset.ItemIndex]);
    tiles := preset.ChildNodes;
    for i := 0 to tiles.Count - 1 do
    begin
      tile := TDOMElement(tiles[i]);
      if (tile.NodeName = 'Tile') and
         TryStrToInt(tile.AttribStrings['ID'], id) and
         (id < FLandscape.MaxStaticID + $4000) then
      begin
        tileNode := vdtRandom.AddChild(nil);
        tileInfo := vdtRandom.GetNodeData(tileNode);
        tileInfo^.ID := id;
      end;
    end;
  end;
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
  if MessageDlg('Are you sure you want to delete all saved locations?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    vstLocations.Clear;
  end;
end;

procedure TfrmMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  if (FScreenBufferState <> CScreenBufferValid) or
     ((FRepaintNeeded or mnuShowAnimations.Checked) and
      (MilliSecondsBetween(Now, FLastDraw) > 50)) then
  begin
    //Logger.Send([lcClient, lcDebug], 'Repainting Game Window');
    oglGameWindow.Repaint;
    FLastDraw := Now;
    FRepaintNeeded := False;
  end;
  Sleep(1);
  Done := False;
end;

procedure TfrmMain.ApplicationProperties1ShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  //that check is a bit dirty, but serves its purpose
  //(i.e. to set the timeout for the tile info hints)
  if HintStr = '-' then
    HintInfo.HideTimeout := Application.HintHidePause +
      Application.HintHidePausePerChar * (Length(FTileHint.Name) +
      Length(FTileHint.Flags));
end;

procedure TfrmMain.btnAddLocationClick(Sender: TObject);
var
  locationName: string;
  locationInfo: PLocationInfo;
begin
  locationName := '';
  if InputQuery('New Location', 'Enter the name of the new location:',
    locationName) then
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

procedure TfrmMain.acUndoExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := FUndoList.Count - 1 downto 0 do
  begin
    dmNetwork.Send(FUndoList[i]);
    FUndoList[i] := nil;
  end;
  FUndoList.Clear;
  acUndo.Enabled := False;
end;

procedure TfrmMain.acVirtualLayerExecute(Sender: TObject);
begin
  frmVirtualLayer.Show;
end;

procedure TfrmMain.acDrawExecute(Sender: TObject);
begin
  acDraw.Checked := True;
  tbDrawTile.Down := True;
  mnuDraw.Checked := True;
  frmDrawSettings.Show;
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
  frmBoundaries.Show;
end;

procedure TfrmMain.acElevateExecute(Sender: TObject);
begin
  acElevate.Checked := True;
  tbElevateTile.Down := True;
  mnuElevate.Checked := True;
  ProcessToolState;
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
  InvalidateFilter;
end;

procedure TfrmMain.acFlatExecute(Sender: TObject);
begin
  acFlat.Checked := not acFlat.Checked;
  RebuildScreenBuffer;
end;

procedure TfrmMain.acHueExecute(Sender: TObject);
begin
  acHue.Checked := True;
  tbSetHue.Down := True;
  mnuSetHue.Checked := True;
  ProcessToolState;
  frmHueSettings.Show;
end;

procedure TfrmMain.acMoveExecute(Sender: TObject);
begin
  acMove.Checked := True;
  tbMoveTile.Down := True;
  mnuMove.Checked := True;
  ProcessToolState;
  frmMoveSettings.Show;
end;

procedure TfrmMain.acNoDrawExecute(Sender: TObject);
begin
  acNoDraw.Checked := not acNoDraw.Checked;
  RebuildScreenBuffer;
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

  SaveLocations;
  SaveRandomPresets;

  FreeAndNil(FTextureManager);
  FreeAndNil(FScreenBuffer);
  FreeAndNil(FOverlayUI);
  FreeAndNil(FLightManager);
  FreeAndNil(FVLayerImage);
  FreeAndNil(FVLayerMaterial);
  FreeAndNil(FVirtualTiles);
  FreeAndNil(FUndoList);
  FreeAndNil(FGLFont);
  FreeAndNil(FRandomPresetsDoc);
  FreeAndNil(FAccessChangedListeners);
  FreeAndNil(FSelectionListeners);
  
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
      MessageDlg('Error', 'The tile with the specified ID could not be found.' +
        LineEnding + 'Check for conflicting filter settings.', mtError, [mbOK], 0);
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
  end else if not (Key in ['$', '0'..'9', 'a'..'f', 'A'..'F', 's', 'S',
    't', 'T', #8]) then
    Key := #0;
end;

procedure TfrmMain.lblChatHeaderCaptionClick(Sender: TObject);
begin
  if pnlChat.Visible then
  begin
    pnlChat.Visible := False;
    spChat.Visible := False;
    pnlChatHeader.AnchorSide[akBottom].Control := pnlBottom;
  end else
  begin
    spChat.Visible := True;
    pnlChat.Visible := True;
    spChat.Top := pnlChat.Top - spChat.Height;
    pnlChatHeader.AnchorSide[akBottom].Control := spChat;
    
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

  glDisable(GL_DEPTH_TEST);
  Render;

  oglGameWindow.SwapBuffers;
end;

procedure TfrmMain.oglGameWindowResize(Sender: TObject);
begin
  InvalidateScreenBuffer;
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

procedure TfrmMain.tbStaticsClick(Sender: TObject);
begin
  RebuildScreenBuffer;
end;

procedure TfrmMain.tbTerrainClick(Sender: TObject);
begin
  RebuildScreenBuffer;
end;

procedure TfrmMain.tmGrabTileInfoTimer(Sender: TObject);
begin
  tmGrabTileInfo.Enabled := False;
  if CurrentTile <> nil then
    pmGrabTileInfo.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    
  SelectedTile := nil;
end;

procedure TfrmMain.tmMovementTimer(Sender: TObject);
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

procedure TfrmMain.vdtRandomClick(Sender: TObject);
var
  node: PVirtualNode;
  tileInfo: PTileInfo;
  selectedID: Integer;
begin
  if vdtRandom.SelectedCount = 1 then
  begin
    node := vdtRandom.GetFirstSelected;
    if node <> nil then
    begin
      tileInfo := vdtRandom.GetNodeData(node);
      selectedID := tileInfo^.ID;

      node := vdtTiles.GetFirst;
      while node <> nil do
      begin
        tileInfo := vdtTiles.GetNodeData(node);
        if tileInfo^.ID = selectedID then
        begin
          vdtTiles.ClearSelection;
          vdtTiles.Selected[node] := True;
          vdtTiles.FocusedNode := node;
          node := nil;
        end else
          node := vdtTiles.GetNext(node);
      end;
    end;
  end;
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

procedure TfrmMain.vdtTilesDrawHint(Sender: TBaseVirtualTree;
  HintCanvas: TCanvas; Node: PVirtualNode; const R: TRect; Column: TColumnIndex
  );
begin
  HintCanvas.Font.Style := [fsBold];
  DrawText(HintCanvas.Handle, PChar(FTileHint.Name), Length(FTileHint.Name),
    FTileHint.NameRect, 0);
  HintCanvas.Font.Style := [fsItalic];
  DrawText(HintCanvas.Handle, PChar(FTileHint.Flags), Length(FTileHint.Flags),
    FTileHint.FlagsRect, DT_WORDBREAK);
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
        PaintInfo.Canvas.TextRect(PaintInfo.CellRect, 0, 0, Format('$%x', [id]),
          textStyle);
      end;
    1:
      begin
        if ResMan.Art.Exists(tileInfo^.ID) then
        begin
          artEntry := ResMan.Art.GetArt(tileInfo^.ID,
            RGB2ARGB(PaintInfo.Canvas.Pixels[PaintInfo.CellRect.Left,
              PaintInfo.CellRect.Top]), nil, False);
          DisplayImage(PaintInfo.Canvas, PaintInfo.CellRect, artEntry.Graphic);
          artEntry.Free;
        end;
      end;
    2:
      begin
        tileData := TTileData(ResMan.Tiledata.Block[tileInfo^.ID]);
        PaintInfo.Canvas.TextRect(PaintInfo.CellRect, PaintInfo.CellRect.Left,
          PaintInfo.CellRect.Top, ISO_8859_1ToUTF8(Trim(tileData.TileName)),
          textStyle);
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

procedure TfrmMain.vdtTilesGetHintSize(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var R: TRect);
var
  tileInfo: PTileInfo;
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
  tileInfo := Sender.GetNodeData(Node);
  flags := '';

  tileData := ResMan.Tiledata.TileData[tileInfo^.ID];
  if tileInfo^.ID < $4000 then
  begin
    if TLandTiledata(tileData).TextureID > 0 then
      flags := 'Stretchable';
  end;

  if tdfArticleA in tileData.Flags then
    prefix := 'a '
  else if tdfArticleAn in tileData.Flags then
    prefix := 'an '
  else
    prefix := '';

  FTileHint.Name := AnsiProperCase(Format('%s%s',
    [prefix, tileData.TileName]), [' ']);
  FTileHint.NameRect.Left := 5;
  FTileHint.NameRect.Top := 5;
  Sender.Canvas.Font.Style := [fsBold];
  DrawText(Sender.Canvas.Handle, PChar(FTileHint.Name), Length(FTileHint.Name),
    FTileHint.NameRect, DT_CALCRECT);

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

  FTileHint.Flags := Format('Flags = [%s]', [flags]);
  FTileHint.FlagsRect.Left := 5;
  FTileHint.FlagsRect.Top := FTileHint.NameRect.Bottom + 5;
  FTileHint.FlagsRect.Right := 145;
  Sender.Canvas.Font.Style := [fsItalic];
  DrawText(Sender.Canvas.Handle, PChar(FTileHint.Flags), Length(FTileHint.Flags),
    FTileHint.FlagsRect, DT_CALCRECT or DT_WORDBREAK);

  R := Rect(0, 0, Max(FTileHint.NameRect.Right, FTileHint.FlagsRect.Right) + 5,
    FTileHint.FlagsRect.Bottom + 5);
end;

procedure TfrmMain.vdtTilesKeyPress(Sender: TObject; var Key: char);
begin
  if Key in ['$', '0'..'9'] then
  begin
    edSearchID.Text := Key;
    edSearchID.Visible := True;
    edSearchID.SetFocus;
    edSearchID.SelStart := 1;
    Key := #0;
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
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
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
  locationInfo^.Name := EmptyStr;
end;

procedure TfrmMain.vstLocationsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
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
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
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

procedure TfrmMain.XMLPropStorage1RestoreProperties(Sender: TObject);
begin
  FTextureManager.UseAnims := mnuShowAnimations.Checked;
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
  if InRange(AX, 0, FLandscape.CellWidth - 1) and InRange(AY, 0,
    FLandscape.CellHeight - 1) then
  begin
    FX := AX;
    edX.Value := FX;
    FY := AY;
    edY.Value := FY;
    dmNetwork.Send(TUpdateClientPosPacket.Create(AX, AY));
    InvalidateScreenBuffer;
    if frmRadarMap <> nil then frmRadarMap.Repaint;
  end;
end;

procedure TfrmMain.SwitchToSelection;
begin
  acSelect.Checked := True;
  BringToFront;
end;

procedure TfrmMain.RegisterAccessChangedListener(
  AListener: TAccessChangedListener);
begin
  if FAccessChangedListeners.IndexOf(AListener) < 0 then
    FAccessChangedListeners.Add(AListener);
end;

procedure TfrmMain.RegisterSelectionListener(AListener: TSelectionListener);
begin
  if FSelectionListeners.IndexOf(AListener) < 0 then
    FSelectionListeners.Add(AListener);
end;

procedure TfrmMain.UnregisterAccessChangedListener(
  AListener: TAccessChangedListener);
begin
  FAccessChangedListeners.Remove(AListener);
end;

procedure TfrmMain.UnregisterSelectionListener(AListener: TSelectionListener);
begin
  FSelectionListeners.Remove(AListener);
end;

procedure TfrmMain.SetCurrentTile(const AValue: TWorldItem);
begin
  Logger.EnterMethod([lcClient, lcDebug], 'SetCurrentTile');
  if AValue = FCurrentTile then
  begin
    Logger.ExitMethod([lcClient, lcDebug], 'SetCurrentTile');
    Exit;
  end;
  Logger.Send([lcClient, lcDebug], 'Value', AValue);

  if FCurrentTile <> nil then
    FCurrentTile.OnDestroy.UnregisterEvent(@OnTileRemoved);
  FCurrentTile := AValue;

  if FCurrentTile = nil then
  begin
    lblTileInfo.Caption := '';
  end else
  begin
    FCurrentTile.OnDestroy.RegisterEvent(@OnTileRemoved);
    if FCurrentTile is TVirtualTile then
      lblTileInfo.Caption := Format('Virtual Layer: X: %d, Y: %d, Z: %d',
        [FCurrentTile.X, FCurrentTile.Y, FCurrentTile.Z])
    else if FCurrentTile is TMapCell then
      lblTileInfo.Caption := Format('Terrain TileID: $%x, X: %d, Y: %d, Z: %d',
        [FCurrentTile.TileID, FCurrentTile.X, FCurrentTile.Y, FCurrentTile.Z])
    else if FCurrentTile is TStaticItem then
      lblTileInfo.Caption := Format('Static TileID: $%x, X: %d, Y: %d, Z: %d, Hue: $%x',
        [FCurrentTile.TileID, FCurrentTile.X, FCurrentTile.Y, FCurrentTile.Z,
         TStaticItem(FCurrentTile).Hue]);
  end;

  UpdateSelection;
  Logger.ExitMethod([lcClient, lcDebug], 'SetCurrentTile');
end;

procedure TfrmMain.SetSelectedTile(const AValue: TWorldItem);
begin
  Logger.EnterMethod([lcClient, lcDebug], 'SetSelectedTile');
  if AValue = FSelectedTile then
  begin
    Logger.ExitMethod([lcClient, lcDebug], 'SetSelectedTile');
    Exit;
  end;
  Logger.Send([lcClient, lcDebug], 'Value', AValue);

  if FSelectedTile <> nil then
    FSelectedTile.OnDestroy.UnregisterEvent(@OnTileRemoved);
  FSelectedTile := AValue;
  if FSelectedTile <> nil then
    FSelectedTile.OnDestroy.RegisterEvent(@OnTileRemoved);

  UpdateSelection;
  Logger.ExitMethod([lcClient, lcDebug], 'SetSelectedTile');
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
  glShadeModel(GL_SMOOTH);
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
  gluOrtho2D(0, oglGameWindow.Width, oglGameWindow.Height, 0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TfrmMain.LoadLocations;
var
  xmlDoc: TXMLDocument;
  location: TDOMElement;
  locationNode: PVirtualNode;
  locationInfo: PLocationInfo;
  locations: TDOMNodeList;
  i, j: Integer;
begin
  vstLocations.Clear;

  if FileExists(FLocationsFile) then
  begin
    ReadXMLFile(xmlDoc, FLocationsFile);
    if xmlDoc.DocumentElement.NodeName = 'Locations' then
    begin
      locations := xmlDoc.DocumentElement.ChildNodes;
      for i := 0 to locations.Count - 1 do
      begin
        location := TDOMElement(locations[i]);
        if location.NodeName = 'Location' then
        begin
          locationNode := vstLocations.AddChild(nil);
          locationInfo := vstLocations.GetNodeData(locationNode);
          locationInfo^.Name := location.AttribStrings['Name'];

          if TryStrToInt(location.AttribStrings['X'], j) then
            locationInfo^.X := j
          else
            locationInfo^.X := 0;

          if TryStrToInt(location.AttribStrings['Y'], j) then
            locationInfo^.Y := j
          else
            locationInfo^.Y := 0;
        end;
      end;
    end;

    xmlDoc.Free;
  end;
end;

procedure TfrmMain.LoadRandomPresets;
var
  presets: TDOMNodeList;
  i: Integer;
begin
  cbRandomPreset.Clear;

  FreeAndNil(FRandomPresetsDoc);
  if FileExists(FRandomPresetsFile) then
  begin
    ReadXMLFile(FRandomPresetsDoc, FRandomPresetsFile);
    presets := FRandomPresetsDoc.DocumentElement.ChildNodes;
    for i := 0 to presets.Count - 1 do
    begin
      if presets[i].NodeName = 'Preset' then
      begin
        cbRandomPreset.Items.AddObject(TDOMElement(presets[i]).AttribStrings['Name'],
          presets[i]);
      end;
    end;
  end else
  begin
    FRandomPresetsDoc := TXMLDocument.Create;
    FRandomPresetsDoc.AppendChild(FRandomPresetsDoc.CreateElement('RandomPresets'));
  end;
end;

procedure TfrmMain.MoveBy(AOffsetX, AOffsetY: Integer); inline;
begin
  SetPos(EnsureRange(FX + AOffsetX, 0, FLandscape.CellWidth - 1),
         EnsureRange(FY + AOffsetY, 0, FLandscape.CellHeight - 1));
  UpdateCurrentTile;
end;

procedure TfrmMain.PrepareMapCell(AMapCell: TMapCell);
var
  current, north, east, west: PBlockInfo;
  cell: TMapCell;
begin
  current := FScreenBuffer.UpdateSortOrder(AMapCell);
  if current = nil then
    Exit; //off-screen update

  PrepareScreenBlock(current);
  Exclude(FScreenBufferState, sbsIndexed);

  //Find surrounding cells
  current := nil;
  north := nil;
  east := nil;
  west := nil;
  while ((north = nil) or (east = nil) or (west = nil)) and
    FScreenBuffer.Iterate(current) do
  begin
    if current^.Item is TMapCell then
    begin
      cell := TMapCell(current^.Item);
      if (cell.X = AMapCell.X - 1) and (cell.Y = AMapCell.Y - 1) then
        north := current
      else if (cell.X = AMapCell.X) and (cell.Y = AMapCell.Y - 1) then
        east := current
      else if (cell.X = AMapCell.X - 1) and (cell.Y = AMapCell.Y) then
        west := current;
    end;
  end;

  if north <> nil then PrepareScreenBlock(north);
  if east <> nil then PrepareScreenBlock(east);
  if west <> nil then PrepareScreenBlock(west);
end;

procedure TfrmMain.InvalidateFilter;
begin
  Exclude(FScreenBufferState, sbsFiltered);
end;

procedure TfrmMain.InvalidateScreenBuffer;
begin
  Exclude(FScreenBufferState, sbsValid);
end;

procedure TfrmMain.PrepareScreenBlock(ABlockInfo: PBlockInfo);

  procedure GetLandAlt(const AX, AY: Integer; const ADefaultZ,
    ADefaultRaw: SmallInt; out Z, RawZ: SmallInt);
  var
    cell: TMapCell;
  begin
    cell := FLandscape.MapCell[AX, AY];
    if cell <> nil then
    begin
      Z := cell.Z;
      RawZ := cell.RawZ;
    end else
    begin
      Z := ADefaultZ;
      RawZ := ADefaultRaw;
    end;
  end;

var
  item: TWorldItem;
  drawX, drawY: Integer;
  z, west, south, east: SmallInt;
  rawZ, rawWest, rawSouth, rawEast: SmallInt;
  staticItem: TStaticItem;
begin
  //add normals to map tiles and materials where possible

  item := ABlockInfo^.Item;

  GetDrawOffset(item.X , item.Y, drawX, drawY);

  if acFlat.Checked then
  begin
    z := 0;
    rawZ := 0;
  end else
  begin
    z := item.Z;
    rawZ := item.RawZ;
  end;

  if ABlockInfo^.HighRes <> nil then ABlockInfo^.HighRes.DelRef;
  if ABlockInfo^.LowRes <> nil then ABlockInfo^.LowRes.DelRef;

  ABlockInfo^.HighRes := nil;
  ABlockInfo^.CheckRealQuad := False;
  ABlockInfo^.Text.Free;

  if item is TMapCell then
  begin
    if not acFlat.Checked then
    begin
      GetLandAlt(item.X, item.Y + 1, z, rawZ, west, rawWest);
      GetLandAlt(item.X + 1, item.Y + 1, z, rawZ, south, rawSouth);
      GetLandAlt(item.X + 1, item.Y, z, rawZ, east, rawEast);

      if  (west <> z) or (south <> z) or (east <> z) then
        ABlockInfo^.HighRes := FTextureManager.GetTexMaterial(item.TileID);

      if (rawWest <> rawZ) or (rawSouth <> rawZ) or (rawEast <> rawZ) then
      begin
        ABlockInfo^.RealQuad[0][0] := drawX;
        ABlockInfo^.RealQuad[0][1] := drawY - rawZ * 4;
        ABlockInfo^.RealQuad[1][0] := drawX + 22;
        ABlockInfo^.RealQuad[1][1] := drawY + 22 - rawEast * 4;
        ABlockInfo^.RealQuad[2][0] := drawX;
        ABlockInfo^.RealQuad[2][1] := drawY + 44 - rawSouth * 4;
        ABlockInfo^.RealQuad[3][0] := drawX - 22;
        ABlockInfo^.RealQuad[3][1] := drawY + 22 - rawWest * 4;

        with ABlockInfo^ do
        begin
          with ScreenRect do
          begin
            Left := drawX - 22;
            Right := drawX + 22;
            Top := RealQuad[0][1];
            Bottom := RealQuad[0][1];

            if RealQuad[1][1] < Top then Top := RealQuad[1][1];
            if RealQuad[1][1] > Bottom then Bottom := RealQuad[1][1];

            if RealQuad[2][1] < Top then Top := RealQuad[2][1];
            if RealQuad[2][1] > Bottom then Bottom := RealQuad[2][1];

            if RealQuad[3][1] < Top then Top := RealQuad[3][1];
            if RealQuad[3][1] > Bottom then Bottom := RealQuad[3][1];
          end;
          CheckRealQuad := True;
        end;
      end;
    end else
    begin
      if mnuFlatShowHeight.Checked then
        ABlockInfo^.Text := TGLText.Create(FGLFont, IntToStr(item.Z));
    end;

    if not ABlockInfo^.CheckRealQuad then
      ABlockInfo^.ScreenRect := Bounds(Trunc(drawX - 22),
        Trunc(drawY - rawZ * 4), 44, 44);

    ABlockInfo^.LowRes := FTextureManager.GetArtMaterial(item.TileID);

    if ABlockInfo^.HighRes <> nil then
    begin
      if ABlockInfo^.Normals = nil then
        New(ABlockInfo^.Normals);
      FLandscape.GetNormals(item.X, item.Y, ABlockInfo^.Normals^);
      ABlockInfo^.DrawQuad[0][0] := drawX;
      ABlockInfo^.DrawQuad[0][1] := drawY - z * 4;
      ABlockInfo^.DrawQuad[1][0] := drawX + 22;
      ABlockInfo^.DrawQuad[1][1] := drawY + 22 - east * 4;
      ABlockInfo^.DrawQuad[2][0] := drawX;
      ABlockInfo^.DrawQuad[2][1] := drawY + 44 - south * 4;
      ABlockInfo^.DrawQuad[3][0] := drawX - 22;
      ABlockInfo^.DrawQuad[3][1] := drawY + 22 - west * 4;
    end else
    begin
      ABlockInfo^.DrawQuad[0][0] := drawX - 22;
      ABlockInfo^.DrawQuad[0][1] := drawY - z * 4;
      ABlockInfo^.DrawQuad[1][0] := drawX - 22 + ABlockInfo^.LowRes.Width;
      ABlockInfo^.DrawQuad[1][1] := drawY - z * 4;
      ABlockInfo^.DrawQuad[2][0] := drawX - 22 + ABlockInfo^.LowRes.Width;
      ABlockInfo^.DrawQuad[2][1] := drawY + ABlockInfo^.LowRes.Height - z * 4;
      ABlockInfo^.DrawQuad[3][0] := drawX - 22;
      ABlockInfo^.DrawQuad[3][1] := drawY + ABlockInfo^.LowRes.Height - z * 4;
    end;
  end else
  if item is TVirtualTile then
  begin
    ABlockInfo^.LowRes := FVLayerMaterial;
    ABlockInfo^.LowRes.AddRef;
    ABlockInfo^.ScreenRect := Bounds(Trunc(drawX - 22), Trunc(drawY - z * 4),
      44, 44);
    ABlockInfo^.DrawQuad[0][0] := drawX - 22;
    ABlockInfo^.DrawQuad[0][1] := drawY - z * 4;
    ABlockInfo^.DrawQuad[1][0] := drawX - 22 + ABlockInfo^.LowRes.Width;
    ABlockInfo^.DrawQuad[1][1] := drawY - z * 4;
    ABlockInfo^.DrawQuad[2][0] := drawX - 22 + ABlockInfo^.LowRes.Width;
    ABlockInfo^.DrawQuad[2][1] := drawY + ABlockInfo^.LowRes.Height - z * 4;
    ABlockInfo^.DrawQuad[3][0] := drawX - 22;
    ABlockInfo^.DrawQuad[3][1] := drawY + ABlockInfo^.LowRes.Height - z * 4;
  end else
  begin
    staticItem := TStaticItem(item);

    ABlockInfo^.LowRes := FTextureManager.GetStaticMaterial(staticItem);
    ABlockInfo^.HueOverride := False;
    ABlockInfo^.ScreenRect := Bounds(Trunc(drawX - ABlockInfo^.LowRes.RealWidth / 2),
      Trunc(drawY + 44 - ABlockInfo^.LowRes.RealHeight - z * 4),
      ABlockInfo^.LowRes.RealWidth,
      ABlockInfo^.LowRes.RealHeight);

    ABlockInfo^.Translucent := tdfTranslucent in
      ResMan.Tiledata.StaticTiles[staticItem.TileID].Flags;

    south := ABlockInfo^.LowRes.RealHeight;
    east := ABlockInfo^.LowRes.RealWidth div 2;

    ABlockInfo^.DrawQuad[0][0] := drawX - east;
    ABlockInfo^.DrawQuad[0][1] := drawY + 44 - south - z * 4;
    ABlockInfo^.DrawQuad[1][0] := drawX - east + ABlockInfo^.LowRes.Width;
    ABlockInfo^.DrawQuad[1][1] := drawY + 44 - south - z * 4;
    ABlockInfo^.DrawQuad[2][0] := drawX - east + ABlockInfo^.LowRes.Width;
    ABlockInfo^.DrawQuad[2][1] := drawY + 44 - south + ABlockInfo^.LowRes.Height - z * 4;
    ABlockInfo^.DrawQuad[3][0] := drawX - east;
    ABlockInfo^.DrawQuad[3][1] := drawY + 44 - south + ABlockInfo^.LowRes.Height - z * 4;
  end;
end;

procedure TfrmMain.Render;
var
  highlight: Boolean;
  intensity: GLfloat;
  blockInfo: PBlockInfo;
  item: TWorldItem;
begin
  if not (sbsValid in FScreenBufferState) then
    RebuildScreenBuffer;

  if not (sbsIndexed in FScreenBufferState) then
  begin
    FScreenBuffer.UpdateShortcuts;
    Include(FScreenBufferState, sbsIndexed);
  end;

  if not (sbsFiltered in FScreenBufferState) then
    UpdateFilter;

  blockInfo := nil;
  while FScreenBuffer.Iterate(blockInfo) do
  begin
    if blockInfo^.State = ssFiltered then
      Continue;

    item := blockInfo^.Item;

    if acSelect.Checked or item.CanBeEdited or (item is TVirtualTile) then
    begin
      intensity := 1.0;
      SetNormalLights;
    end else
    begin
      intensity := 0.5;
      SetDarkLights;
    end;

    if blockInfo^.Translucent then
      glColor4f(intensity, intensity, intensity, 0.5)
    else
      glColor4f(intensity, intensity, intensity, 1.0);

    highlight := blockInfo^.Highlighted and item.CanBeEdited;

    if highlight then
    begin
      glEnable(GL_COLOR_LOGIC_OP);
      glLogicOp(GL_COPY_INVERTED);
    end;

    if blockInfo^.HighRes <> nil then
    begin
      glBindTexture(GL_TEXTURE_2D, blockInfo^.HighRes.Texture);

      if not highlight then
        glEnable(GL_LIGHTING);

      glBegin(GL_QUADS);
        glNormal3fv(@blockInfo^.Normals^[0]);
        glTexCoord2i(0, 0); glVertex2iv(@blockInfo^.DrawQuad[0]);
        glNormal3fv(@blockInfo^.Normals^[3]);
        glTexCoord2i(0, 1); glVertex2iv(@blockInfo^.DrawQuad[3]);
        glNormal3fv(@blockInfo^.Normals^[2]);
        glTexCoord2i(1, 1); glVertex2iv(@blockInfo^.DrawQuad[2]);
        glNormal3fv(@blockInfo^.Normals^[1]);
        glTexCoord2i(1, 0); glVertex2iv(@blockInfo^.DrawQuad[1]);
      glEnd;

      if not highlight then
        glDisable(GL_LIGHTING);
    end else
    begin
      glBindTexture(GL_TEXTURE_2D, blockInfo^.LowRes.Texture);
      glBegin(GL_QUADS);
        glTexCoord2i(0, 0); glVertex2iv(@blockInfo^.DrawQuad[0]);
        glTexCoord2i(1, 0); glVertex2iv(@blockInfo^.DrawQuad[1]);
        glTexCoord2i(1, 1); glVertex2iv(@blockInfo^.DrawQuad[2]);
        glTexCoord2i(0, 1); glVertex2iv(@blockInfo^.DrawQuad[3]);
      glEnd;
    end;

    if highlight then
      glDisable(GL_COLOR_LOGIC_OP);

    if (blockInfo^.Text <> nil) then
      blockInfo^.Text.Render(blockInfo^.ScreenRect);
  end;

  if (FLightManager.LightLevel > 0) and not acFlat.Checked then
    FLightManager.Draw(oglGameWindow.ClientRect);
  FOverlayUI.Draw(oglGameWindow);
end;

procedure TfrmMain.SaveLocations;
var
  xmlDoc: TXMLDocument;
  location: TDOMElement;
  locationNode: PVirtualNode;
  locationInfo: PLocationInfo;
begin
  xmlDoc := TXMLDocument.Create;
  xmlDoc.AppendChild(xmlDoc.CreateElement('Locations'));

  locationNode := vstLocations.GetFirst;
  while locationNode <> nil do
  begin
    locationInfo := vstLocations.GetNodeData(locationNode);
    location := xmlDoc.CreateElement('Location');
    location.AttribStrings['Name'] := locationInfo^.Name;
    location.AttribStrings['X'] := IntToStr(locationInfo^.X);
    location.AttribStrings['Y'] := IntToStr(locationInfo^.Y);
    xmlDoc.DocumentElement.AppendChild(location);

    locationNode := vstLocations.GetNext(locationNode);
  end;

  WriteXMLFile(xmlDoc, FLocationsFile);
  xmlDoc.Free;
end;

procedure TfrmMain.SaveRandomPresets;
begin
  WriteXMLFile(FRandomPresetsDoc, FRandomPresetsFile);
end;

procedure TfrmMain.OnLandscapeChanged;
begin
  InvalidateScreenBuffer;
  oglGameWindow.Repaint;
  UpdateCurrentTile;
end;

procedure TfrmMain.OnMapChanged(AMapCell: TMapCell);
begin
  PrepareMapCell(AMapCell);
  InvalidateFilter;
end;

procedure TfrmMain.OnNewBlock(ABlock: TBlock);
begin
  InvalidateScreenBuffer;
end;

procedure TfrmMain.OnStaticDeleted(AStaticItem: TStaticItem);
begin
  FScreenBuffer.Delete(AStaticItem);
  UpdateCurrentTile;
  FRepaintNeeded := True;
  ForceUpdateCurrentTile;
end;

procedure TfrmMain.OnStaticElevated(AStaticItem: TStaticItem);
var
  blockInfo: PBlockInfo;
begin
  AStaticItem.PrioritySolver := FScreenBuffer.GetSerial;
  blockInfo := FScreenBuffer.UpdateSortOrder(AStaticItem);
  if blockInfo <> nil then
  begin
    PrepareScreenBlock(blockInfo);
    Exclude(FScreenBufferState, sbsIndexed);
    InvalidateFilter;
  end;
end;

procedure TfrmMain.OnStaticHued(AStaticItem: TStaticItem);
var
  blockInfo: PBlockInfo;
begin
  blockInfo := nil;
  while FScreenBuffer.Iterate(blockInfo) do
  begin
    if blockInfo^.Item = AStaticItem then
    begin
      PrepareScreenBlock(blockInfo);
      FRepaintNeeded := True;
      InvalidateFilter;
      Break;
    end;
  end;
end;

procedure TfrmMain.OnStaticInserted(AStaticItem: TStaticItem);
begin
  if (AStaticItem.X >= FX + FLowOffsetX) and
     (AStaticItem.X <= FX + FHighOffsetX) and
     (AStaticItem.Y >= FY + FLowOffsetY) and
     (AStaticItem.Y <= FY + FHighOffsetY) then
  begin
    AStaticItem.PrioritySolver := FScreenBuffer.GetSerial;
    PrepareScreenBlock(FScreenBuffer.Insert(AStaticItem));
    FRepaintNeeded := True;
    InvalidateFilter;
  end;
end;

procedure TfrmMain.BuildTileList;
var
  minID, maxID, i, lastID: Integer;
  node: PVirtualNode;
  tileInfo: PTileInfo;
  filter: string;
begin
  maxID := $3FFF;
  if cbTerrain.Checked then minID := $0 else minID := $4000;
  if cbStatics.Checked then maxID := maxID + FLandscape.MaxStaticID;
  filter := AnsiLowerCase(UTF8ToISO_8859_1(edFilter.Text));
  
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
      if (filter <> '') and (Pos(filter, AnsiLowerCase(
        ResMan.Tiledata.TileData[i].TileName)) = 0) then Continue;
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
  blockInfo: PBlockInfo;
begin
  if acSelect.Checked then
  begin
    //lblTip.Caption := 'Right click shows a menu with all the tools.';
    lblTip.Caption := 'Press and hold the left mouse button to show a list with'
      + ' actions (eg. grab hue).';
    oglGameWindow.Cursor := crDefault;

    //no highlighted tiles in "selection" mode
    Logger.Send([lcClient, lcDebug], 'Disable highlighting');
    blockInfo := nil;
    while FScreenBuffer.Iterate(blockInfo) do
      if blockInfo^.State = ssNormal then
        blockInfo^.Highlighted := False;
  end else
  begin
    lblTip.Caption := 'Press and hold the left mouse button to target an area.';
    oglGameWindow.Cursor := crHandPoint;
  end;

  FRepaintNeeded := True;
end;

procedure TfrmMain.ProcessAccessLevel;
begin
  mnuAdministration.Visible := (dmNetwork.AccessLevel >= alAdministrator);
  acDraw.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acMove.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acElevate.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acDelete.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acHue.Enabled := (dmNetwork.AccessLevel >= alNormal);
  Caption := Format('UO CentrED - [%s (%s)]', [dmNetwork.Username,
    GetAccessLevelString(dmNetwork.AccessLevel)]);
end;

procedure TfrmMain.RebuildScreenBuffer;
var
  blockInfo: PBlockInfo;
  i, tileX, tileY: Integer;
  virtualTile: TVirtualTile;
begin
  //Logger.EnterMethod([lcClient], 'RebuildScreenBuffer');

  FDrawDistance := Trunc(Sqrt(oglGameWindow.Width * oglGameWindow.Width +
    oglGamewindow.Height * oglGamewindow.Height) / 44);
  //Logger.Send([lcClient], 'DrawDistance', FDrawDistance);

  {$HINTS off}{$WARNINGS off}
  if FX - FDrawDistance < 0 then FLowOffsetX := -FX else FLowOffsetX := -FDrawDistance;
  if FY - FDrawDistance < 0 then FLowOffsetY := -FY else FLowOffsetY := -FDrawDistance;
  if FX + FDrawDistance >= FLandscape.Width * 8 then FHighOffsetX := FLandscape.Width * 8 - FX - 1 else FHighOffsetX := FDrawDistance;
  if FY + FDrawDistance >= FLandscape.Height * 8 then FHighOffsetY := FLandscape.Height * 8 - FY - 1 else FHighOffsetY := FDrawDistance;
  {$HINTS on}{$WARNINGS on}

  FRangeX := FHighOffsetX - FLowOffsetX;
  FRangeY := FHighOffsetY - FLowOffsetY;

  FLandscape.PrepareBlocks((FX + FLowOffsetX) div 8, (FY + FLowOffsetY) div 8,
    (FX + FHighOffsetX) div 8 + 1, (FY + FHighOffsetY) div 8 + 1);

  if frmVirtualLayer.cbShowLayer.Checked then
  begin
    //Logger.Send([lcClient, lcDebug], 'Preparing Virtual Layer');

    if FVLayerMaterial = nil then
      FVLayerMaterial := TSimpleMaterial.Create(FVLayerImage);

    i := 0;
    for tileX := FX + FLowOffsetX to FX + FHighOffsetX do
    begin
      for tileY := FY + FLowOffsetY to FY + FHighOffsetY do
      begin
        while (i < FVirtualTiles.Count) and (not (FVirtualTiles[i] is TVirtualTile)) do
          Inc(i);

        if i < FVirtualTiles.Count then
        begin
          virtualTile := TVirtualTile(FVirtualTiles[i]);
        end else
        begin
          virtualTile := TVirtualTile.Create(nil);
          FVirtualTiles.Add(virtualTile);
        end;

        virtualTile.X := tileX;
        virtualTile.Y := tileY;
        virtualTile.Z := frmVirtualLayer.seZ.Value;
        virtualTile.Priority := virtualTile.Z;
        virtualTile.PriorityBonus := High(ShortInt);

        Inc(i);
      end;
    end;
    while i < FVirtualTiles.Count do
    begin
      if FVirtualTiles[i] is TVirtualTile then
        FVirtualTiles.Delete(i)
      else
        Inc(i);
    end;
  end else
  begin
    for i := FVirtualTiles.Count - 1 downto 0 do
      if FVirtualTiles[i] is TVirtualTile then
        FVirtualTiles.Delete(i);
  end;

  //Logger.Send([lcClient, lcDebug], 'VirtualTiles', FVirtualTiles.Count);

  FLandscape.FillDrawList(FScreenBuffer, FX + FLowOffsetX, FY + FLowOffsetY,
    FRangeX, FRangeY, tbTerrain.Down, tbStatics.Down, acNoDraw.Checked,
    FVirtualTiles);

  //Pre-process the buffer
  blockInfo := nil;
  while FScreenBuffer.Iterate(blockInfo) do
    PrepareScreenBlock(blockInfo);

  FScreenBuffer.UpdateShortcuts;
  FScreenBufferState := [sbsValid, sbsIndexed];

  //Logger.ExitMethod([lcClient], 'RebuildScreenBuffer');
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
  blockInfo: PBlockInfo;
begin
  //Logger.EnterMethod([lcClient, lcDebug], 'UpdateCurrentTile');
  FOverlayUI.ActiveArrow := FOverlayUI.HitTest(AX, AY);
  if FOverlayUI.ActiveArrow > -1 then
  begin
    //Logger.Send([lcClient, lcDebug], 'Overlay active');
    CurrentTile := nil;
    //Logger.ExitMethod([lcClient, lcDebug], 'UpdateCurrentTile');
    Exit;
  end;

  blockInfo := FScreenBuffer.Find(Point(AX, AY));
  if blockInfo <> nil then
    CurrentTile := blockInfo^.Item
  else
    CurrentTile := nil;

  //Logger.ExitMethod([lcClient, lcDebug], 'UpdateCurrentTile');
end;

procedure TfrmMain.UpdateFilter;
var
  blockInfo: PBlockInfo;
begin
  blockInfo := nil;
  while FScreenBuffer.Iterate(blockInfo) do
  begin
    if blockInfo^.State in [ssNormal, ssFiltered] then
    begin
      blockInfo^.State := ssNormal;
      if (blockInfo^.Item.Z < frmBoundaries.tbMinZ.Position) or
        (blockInfo^.Item.Z > frmBoundaries.tbMaxZ.Position) then
      begin
        blockInfo^.State := ssFiltered;
      end else
      if tbFilter.Down and (blockInfo^.Item is TStaticItem) and
        (not frmFilter.Filter(TStaticItem(blockInfo^.Item))) then
      begin
        blockInfo^.State := ssFiltered;
      end;
    end;
  end;
  Include(FScreenBufferState, sbsFiltered);

  ForceUpdateCurrentTile;

  if (FLightManager.LightLevel > 0) and not acFlat.Checked then
    FLightManager.UpdateLightMap(FX + FLowOffsetX, FRangeX + 1, FY + FLowOffsetY,
      FRangeY + 1, FScreenBuffer);
end;

procedure TfrmMain.UpdateSelection;

  procedure SetHighlight(ABlockInfo: PBlockInfo; AHighlighted: Boolean);
  begin
    if (ABlockInfo^.Item is TStaticItem) and acHue.Checked then
    begin
      if ABlockInfo^.HueOverride <> AHighlighted then
      begin
        ABlockInfo^.HueOverride := AHighlighted;
        if AHighlighted then
          ABlockInfo^.LowRes := FTextureManager.GetStaticMaterial(
            TStaticItem(ABlockInfo^.Item), frmHueSettings.lbHue.ItemIndex)
        else
          ABlockInfo^.LowRes := FTextureManager.GetStaticMaterial(
            TStaticItem(ABlockInfo^.Item));
      end;
    end else
    begin
      ABlockInfo^.Highlighted := AHighlighted;
    end;
  end;

  procedure AddGhostTile(AX, AY: Word; ABaseTile: TWorldItem);
  var
    blockInfo: PBlockInfo;
    tileInfo: PTileInfo;
    node: PVirtualNode;
    cell: TMapCell;
    ghostTile: TGhostTile;
    i: Integer;
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
        cell := FLandscape.MapCell[AX, AY];
        if cell <> nil then
        begin
          cell.IsGhost := True;
          cell.GhostID := tileInfo^.ID;
          if frmDrawSettings.cbForceAltitude.Checked then
            cell.GhostZ := frmDrawSettings.seForceAltitude.Value
          else
            cell.GhostZ := cell.RawZ;
          if frmDrawSettings.cbRandomHeight.Checked then
            cell.GhostZ := cell.GhostZ + Random(frmDrawSettings.seRandomHeight.Value);

          PrepareMapCell(cell);
        end;
      end else
      begin
        ghostTile := TGhostTile.Create(nil, nil, 0, 0);
        ghostTile.TileID := tileInfo^.ID - $4000;
        ghostTile.Hue := frmHueSettings.lbHue.ItemIndex;
        ghostTile.X := AX;
        ghostTile.Y := AY;
        if not frmDrawSettings.cbForceAltitude.Checked then
        begin
          ghostTile.Z := ABaseTile.Z;
          if ABaseTile is TStaticItem then
            ghostTile.Z := ghostTile.Z +
              ResMan.Tiledata.StaticTiles[ABaseTile.TileID].Height;
        end else
          ghostTile.Z := frmDrawSettings.seForceAltitude.Value;
        if frmDrawSettings.cbRandomHeight.Checked then
          ghostTile.Z := ghostTile.Z +
            Random(frmDrawSettings.seRandomHeight.Value);

        ghostTile.UpdatePriorities(ResMan.Tiledata.StaticTiles[ghostTile.TileID],
          MaxInt);
        ghostTile.CanBeEdited := True;

        FVirtualTiles.Add(ghostTile);
        blockInfo := FScreenBuffer.Insert(ghostTile);
        blockInfo^.State := ssGhost;
        PrepareScreenBlock(blockInfo);
      end;

    end;
  end;

var
  selectedRect: TRect;
  blockInfo: PBlockInfo;
  item: TWorldItem;
  cell: TMapCell;
  i, tileX, tileY: Integer;
begin
  //Logger.EnterMethod([lcClient, lcDebug], 'UpdateSelection');

  //If the current tile is nil, but we still have a selected tile, the
  //procedure is pointless - the selection should stay intact.
  if (CurrentTile <> nil) or (SelectedTile = nil) then
  begin
    if CurrentTile = nil then
      selectedRect := Rect(-1, -1, -1, -1)
    else
      selectedRect := GetSelectedRect;

    //clean up old ghost tiles
    //Logger.Send([lcClient, lcDebug], 'Cleaning ghost tiles');
    for i := FVirtualTiles.Count - 1 downto 0 do
    begin
      item := FVirtualTiles[i];
      if (item is TGhostTile) and not IsInRect(item.X, item.Y, selectedRect) then
      begin
        FScreenBuffer.Delete(item);
        FVirtualTiles.Delete(i);
      end;
    end;
    //Logger.Send([lcClient, lcDebug], 'FSelection', FSelection);
    for tileX := FSelection.Left to FSelection.Right do
      for tileY := FSelection.Top to FSelection.Bottom do
        if not IsInRect(tileX, tileY, selectedRect) then
        begin
          cell := FLandscape.MapCell[tileX, tileY];
          if (cell <> nil) and cell.IsGhost then
          begin
            cell.IsGhost := False;
            PrepareMapCell(cell);
          end;
        end;

    if (CurrentTile <> nil) and (not acSelect.Checked) then
    begin
      blockInfo := nil;
      if (SelectedTile <> nil) and (CurrentTile <> SelectedTile) then
      begin
        {Logger.Send([lcClient, lcDebug], 'Multiple Targets');
        Logger.Send([lcClient, lcDebug], 'SelectedRect', selectedRect);}
        //set new ghost tiles
        if acDraw.Checked then
          for tileX := selectedRect.Left to selectedRect.Right do
            for tileY := selectedRect.Top to selectedRect.Bottom do
              if not IsInRect(tileX, tileY, FSelection) then
                AddGhostTile(tileX, tileY, SelectedTile);
        while FScreenBuffer.Iterate(blockInfo) do
          if (blockInfo^.State = ssNormal) then
            SetHighlight(blockInfo, IsInRect(blockInfo^.Item.X, blockInfo^.Item.Y,
              selectedRect) and not acDraw.Checked);
      end else
      begin
        //Logger.Send([lcClient, lcDebug], 'Single Target');
        if acDraw.Checked and not IsInRect(CurrentTile.X, CurrentTile.Y,
          FSelection) then
          AddGhostTile(CurrentTile.X, CurrentTile.Y, CurrentTile);
        while FScreenBuffer.Iterate(blockInfo) do
          if blockInfo^.State = ssNormal then
            SetHighlight(blockInfo, (blockInfo^.Item = CurrentTile) and
              not acDraw.Checked);
      end;
    end;
    FSelection := selectedRect;
  end;
  {Logger.Send([lcClient, lcDebug], 'Virtual Tiles', FVirtualTiles.Count);
  Logger.ExitMethod([lcClient, lcDebug], 'UpdateSelection');}
end;

procedure TfrmMain.OnTileRemoved(ATile: TMulBlock);
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
        FLandscape.UpdateWriteMap(ABuffer);
        FRepaintNeeded := True;

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

        for i := FAccessChangedListeners.Count - 1 downto 0 do
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
      Result.Right := Max(CurrentTile.X, SelectedTile.X);
      Result.Bottom := Max(CurrentTile.Y, SelectedTile.Y);
    end else
    begin
      Result.Left := CurrentTile.X;
      Result.Top := CurrentTile.Y;
      Result.Right := CurrentTile.X;
      Result.Bottom := CurrentTile.Y;
    end;
  end;
end;

function TfrmMain.ConfirmAction: Boolean;
begin
  if acMove.Checked and frmMoveSettings.cbAsk.Checked then
  begin
    Result := frmMoveSettings.ShowModal = mrYes;
  end else
  if not mnuSecurityQuestion.Checked then
  begin
    Result := True;
  end else
  begin
    frmConfirmation.Left := Mouse.CursorPos.x - frmConfirmation.btnYes.Left - frmConfirmation.btnYes.Width div 2;
    frmConfirmation.Top := Mouse.CursorPos.y - frmConfirmation.btnYes.Top - frmConfirmation.btnYes.Height div 2;
    Result := frmConfirmation.ShowModal = mrYes;
  end;

  if not oglGameWindow.MouseEntered then
    oglGameWindowMouseLeave(nil);
end;

function TfrmMain.FindRandomPreset(AName: String): TDOMElement;
var
  preset: TDOMElement;
  presets: TDOMNodeList;
  i: Integer;
begin
  presets := FRandomPresetsDoc.DocumentElement.ChildNodes;
  Result := nil;
  i := 0;
  while (i < presets.Count) and (Result = nil) do
  begin
    preset := TDOMElement(presets[i]);
    if SameText(preset.AttribStrings['Name'], AName) then
      Result := preset
    else
      Inc(i);
  end;
end;

procedure TfrmMain.ForceUpdateCurrentTile;
begin
  CurrentTile := nil;
  UpdateCurrentTile;
end;

procedure TfrmMain.GetDrawOffset(AX, AY: Integer; out DrawX, DrawY: Integer); inline;
begin
  Dec(AX, FX);
  Dec(AY, FY);
  DrawX := (oglGameWindow.Width div 2) + (AX - AY) * 22;
  DrawY := (oglGamewindow.Height div 2) + (AX + AY) * 22;
end;

initialization
  {$I UfrmMain.lrs}

end.

