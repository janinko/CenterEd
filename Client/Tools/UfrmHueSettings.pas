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
unit UfrmHueSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UfrmToolWindow, UHue;

type

  { TfrmHueSettings }

  TfrmHueSettings = class(TfrmToolWindow)
    edHue: TEdit;
    lblHue: TLabel;
    lbHue: TListBox;
    procedure edHueEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbHueDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure lbHueSelectionChange(Sender: TObject; User: boolean);
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
  DrawHue(hue, lbHue.Canvas, ARect, lbHue.Items.Strings[Index]);
end;

procedure TfrmHueSettings.lbHueSelectionChange(Sender: TObject; User: boolean);
begin
  edHue.Text := Format('$%x', [lbHue.ItemIndex]);
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
  ACanvas.TextOut(ARect.Left + 36, ARect.Top, ACaption);
end;

initialization
  {$I UfrmHueSettings.lrs}

end.

