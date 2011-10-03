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
 *      Portions Copyright 2011 Andreas Schneider
 *)
unit UfrmHueSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, UfrmToolWindow, UHue,
  XMLRead, XMLWrite, DOM;

type

  { TfrmHueSettings }

  TfrmHueSettings = class(TfrmToolWindow)
    btnAddRandom: TSpeedButton;
    btnClearRandom: TSpeedButton;
    btnDeleteRandom: TSpeedButton;
    btnRandomPresetDelete: TSpeedButton;
    btnRandomPresetSave: TSpeedButton;
    cbRandomPreset: TComboBox;
    cbRandom: TCheckBox;
    edHue: TEdit;
    gbRandom: TGroupBox;
    lblHue: TLabel;
    lbHue: TListBox;
    lbRandom: TListBox;
    procedure btnAddRandomClick(Sender: TObject);
    procedure btnClearRandomClick(Sender: TObject);
    procedure btnDeleteRandomClick(Sender: TObject);
    procedure btnRandomPresetDeleteClick(Sender: TObject);
    procedure btnRandomPresetSaveClick(Sender: TObject);
    procedure cbRandomChange(Sender: TObject);
    procedure cbRandomPresetChange(Sender: TObject);
    procedure edHueEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbHueDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure lbHueSelectionChange(Sender: TObject; User: boolean);
    procedure lbRandomDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbRandomDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    FConfigDir: String;
    FRandomHuePresetsFile: String;
    FRandomHuePresetsDoc: TXMLDocument;
    function FindRandomPreset(AName: String): TDOMElement;
    procedure LoadRandomPresets;
    procedure SaveRandomPresets;
  public
    function GetHue: Word;
  public
    class procedure DrawHue(AHue: THue; ACanvas: TCanvas; ARect: TRect;
      ACaption: string);
  end; 

var
  frmHueSettings: TfrmHueSettings;

implementation

uses
  UGameResources, UGraphicHelper;

{ TfrmHueSettings }

procedure TfrmHueSettings.edHueEditingDone(Sender: TObject);
var
  hueID: Integer;
begin
  if (not TryStrToInt(edHue.Text, hueID)) or (hueID >= lbHue.Items.Count) then
  begin
    edHue.Text := Format('$%x', [lbHue.ItemIndex]);
    MessageDlg('Invalid Hue', 'The hue you''ve entered is invalid.', mtWarning, [mbOK], 0);
  end else
    lbHue.ItemIndex := hueID;
end;

procedure TfrmHueSettings.btnDeleteRandomClick(Sender: TObject);
var
  i: Integer;
begin
  lbRandom.Items.BeginUpdate;
  for i := lbRandom.Items.Count - 1 downto 0 do
    if lbRandom.Selected[i] then
      lbRandom.Items.Delete(i);
  lbRandom.Items.EndUpdate;
end;

procedure TfrmHueSettings.btnRandomPresetDeleteClick(Sender: TObject);
var
  preset: TDOMElement;
begin
  if cbRandomPreset.ItemIndex > -1 then
  begin
    preset := TDOMElement(cbRandomPreset.Items.Objects[cbRandomPreset.ItemIndex]);
    FRandomHuePresetsDoc.DocumentElement.RemoveChild(preset);
    cbRandomPreset.Items.Delete(cbRandomPreset.ItemIndex);
    cbRandomPreset.ItemIndex := -1;
  end;
end;

procedure TfrmHueSettings.btnRandomPresetSaveClick(Sender: TObject);
var
  presetName: string;
  i: Integer;
  preset, hue: TDOMElement;
  children: TDOMNodeList;
begin
  presetName := cbRandomPreset.Text;
  if InputQuery('Save Preset', 'Enter the name of the preset:', presetName) then
  begin
    preset := FindRandomPreset(presetName);
    if preset = nil then
    begin
      preset := FRandomHuePresetsDoc.CreateElement('Preset');
      preset.AttribStrings['Name'] := presetName;
      FRandomHuePresetsDoc.DocumentElement.AppendChild(preset);
      cbRandomPreset.Items.AddObject(presetName, preset);
    end else
    begin
      children := preset.GetChildNodes;
      for i := children.Count - 1 downto 0 do
        preset.RemoveChild(children[i]);
    end;

    for i := 0 to lbRandom.Items.Count - 1 do
    begin
      hue := FRandomHuePresetsDoc.CreateElement('Hue');
      hue.AttribStrings['ID'] := IntToStr(PtrInt(lbRandom.Items.Objects[i]));
      preset.AppendChild(hue);
    end;

    cbRandomPreset.ItemIndex := cbRandomPreset.Items.IndexOfObject(preset);

    SaveRandomPresets;
  end;
end;

procedure TfrmHueSettings.cbRandomChange(Sender: TObject);
begin
  lbHue.MultiSelect := cbRandom.Checked;
  gbRandom.Visible := cbRandom.Checked;
end;

procedure TfrmHueSettings.cbRandomPresetChange(Sender: TObject);
var
  preset, hue: TDOMElement;
  id: PtrInt;
begin
  lbRandom.Clear;
  if cbRandomPreset.ItemIndex > -1 then
  begin
    preset := TDOMElement(cbRandomPreset.Items.Objects[cbRandomPreset.ItemIndex]);
    hue := TDOMElement(preset.FirstChild);

    while hue <> nil do
    begin
      if hue.NodeName = 'Hue' then
      begin
        id := StrToInt(hue.AttribStrings['ID']);
        lbRandom.Items.AddObject(lbHue.Items.Strings[id], TObject(id));
      end;
      hue := TDOMElement(hue.NextSibling);
    end;
  end;
end;

procedure TfrmHueSettings.btnClearRandomClick(Sender: TObject);
begin
  lbRandom.Items.Clear;
end;

procedure TfrmHueSettings.btnAddRandomClick(Sender: TObject);
var
  i: PtrInt;
begin
  lbRandom.Items.BeginUpdate;
  for i := 0 to lbHue.Count - 1 do
    if lbHue.Selected[i] then
      lbRandom.Items.AddObject(lbHue.Items.Strings[i], TObject(i));
  lbRandom.Items.EndUpdate;
end;

procedure TfrmHueSettings.FormCreate(Sender: TObject);
var
  i: Integer;
  hue: THue;
begin
  lbHue.Clear;
  lbHue.Items.Add('$0 (no hue)');
  for i := 1 to ResMan.Hue.Count do
  begin
    hue := ResMan.Hue.Hues[i-1];
    lbHue.Items.AddObject(Format('$%x (%s)', [i, hue.Name]), hue);
  end;
  lbHue.ItemIndex := 0;

  FConfigDir := GetAppConfigDir(False);
  ForceDirectories(FConfigDir);
  FRandomHuePresetsFile := FConfigDir + 'RandomHuePresets.xml';

  LoadRandomPresets;
end;

procedure TfrmHueSettings.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRandomHuePresetsDoc);
end;

procedure TfrmHueSettings.lbHueDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  hue: THue;
begin
  if Index > 0 then
    hue := ResMan.Hue.Hues[Index-1]
  else
    hue := nil;
  DrawHue(hue, TListBox(Control).Canvas, ARect, TListBox(Control).Items.Strings[Index]);
end;

procedure TfrmHueSettings.lbHueSelectionChange(Sender: TObject; User: boolean);
begin
  edHue.Text := Format('$%x', [lbHue.ItemIndex]);
end;

procedure TfrmHueSettings.lbRandomDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if Source = lbHue then
    btnAddRandomClick(Sender);
end;

procedure TfrmHueSettings.lbRandomDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Source = lbHue then Accept := True;
end;

function TfrmHueSettings.FindRandomPreset(AName: String): TDOMElement;
begin
  Result := TDOMElement(FRandomHuePresetsDoc.DocumentElement.FirstChild);
  while Result <> nil do
  begin
    if SameText(Result.AttribStrings['Name'], AName) then
      Break;

    Result := TDOMElement(Result.NextSibling);
  end;
end;

procedure TfrmHueSettings.LoadRandomPresets;
var
  presetElement, hueElement: TDOMElement;
begin
  FreeAndNil(FRandomHuePresetsDoc);
  cbRandomPreset.Items.Clear;
  if FileExists(FRandomHuePresetsFile) then
  begin
    ReadXMLFile(FRandomHuePresetsDoc, FRandomHuePresetsFile);
    presetElement := TDOMElement(FRandomHuePresetsDoc.DocumentElement.FirstChild);
    while presetElement <> nil do
    begin
      if presetElement.NodeName = 'Preset' then
        cbRandomPreset.Items.AddObject(presetElement.AttribStrings['Name'], presetElement);
      presetElement := TDOMElement(presetElement.NextSibling);
    end;
  end else
  begin
    FRandomHuePresetsDoc := TXMLDocument.Create;
    FRandomHuePresetsDoc.AppendChild(FRandomHuePresetsDoc.CreateElement('RandomHuePresets'));
  end;
end;

procedure TfrmHueSettings.SaveRandomPresets;
begin
  WriteXMLFile(FRandomHuePresetsDoc, FRandomHuePresetsFile);
end;

function TfrmHueSettings.GetHue: Word;
begin
  if cbRandom.Checked and (lbRandom.Items.Count > 0) then
    Result := PtrInt(lbRandom.Items.Objects[Random(lbRandom.Items.Count)])
  else
    Result := lbHue.ItemIndex;
end;

class procedure TfrmHueSettings.DrawHue(AHue: THue; ACanvas: TCanvas; ARect: TRect;
  ACaption: string);
var
  hueColor: TColor;
  i: Integer;
begin
  ACanvas.Pen.Color := clWhite;
  ACanvas.Rectangle(ARect);
  if AHue <> nil then
    for i := 0 to 31 do
    begin
      hueColor := ARGB2RGB(AHue.ColorTable[i]);
      ACanvas.Pen.Color := hueColor;
      ACanvas.MoveTo(ARect.Left + 2 + i, ARect.Top + 1);
      ACanvas.LineTo(ARect.Left + 2 + i, ARect.Bottom - 1);
    end;
  ACanvas.TextOut(ARect.Left + 36, ARect.Top + 1, ACaption);
end;

initialization
  {$I UfrmHueSettings.lrs}

end.

