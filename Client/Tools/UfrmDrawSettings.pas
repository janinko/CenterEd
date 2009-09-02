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
unit UfrmDrawSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, LMessages, UfrmToolWindow;

type

  { TfrmDrawSettings }

  TfrmDrawSettings = class(TfrmToolWindow)
    cbForceAltitude: TCheckBox;
    cbRandomHeight: TCheckBox;
    gbHue: TGroupBox;
    pbHue: TPaintBox;
    rbRandom: TRadioButton;
    rbTileList: TRadioButton;
    seForceAltitude: TSpinEdit;
    seRandomHeight: TSpinEdit;
    procedure pbHueClick(Sender: TObject);
    procedure pbHuePaint(Sender: TObject);
    procedure seForceAltitudeChange(Sender: TObject);
    procedure seRandomHeightChange(Sender: TObject);
  public
    { public declarations }
  end; 

var
  frmDrawSettings: TfrmDrawSettings;

implementation

uses
  UGameResources, UHue, UfrmHueSettings;

{ TfrmDrawSettings }

procedure TfrmDrawSettings.pbHueClick(Sender: TObject);
var
  msg: TLMessage;
begin
  frmHueSettings.Left := Mouse.CursorPos.x - 8;
  frmHueSettings.Top := Mouse.CursorPos.y - 8;
  frmHueSettings.ShowModal;
  pbHue.Repaint;
  MouseLeave(msg);
end;

procedure TfrmDrawSettings.pbHuePaint(Sender: TObject);
var
  hue: THue;
begin
  if frmHueSettings <> nil then
  begin
    if frmHueSettings.lbHue.ItemIndex > 0 then
      hue := ResMan.Hue.Hues[frmHueSettings.lbHue.ItemIndex - 1]
    else
      hue := nil;
    TfrmHueSettings.DrawHue(hue, pbHue.Canvas, pbHue.Canvas.ClipRect,
      frmHueSettings.lbHue.Items.Strings[frmHueSettings.lbHue.ItemIndex]);
  end;
end;

procedure TfrmDrawSettings.seForceAltitudeChange(Sender: TObject);
begin
  cbForceAltitude.Checked := True;
end;

procedure TfrmDrawSettings.seRandomHeightChange(Sender: TObject);
begin
  cbRandomHeight.Checked := True;
end;

//TODO : canclose ---> hue settings

initialization
  {$I UfrmDrawSettings.lrs}

end.

