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
unit UfrmDrawSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, LMessages, LCLIntf;

type

  { TfrmDrawSettings }

  TfrmDrawSettings = class(TForm)
    cbForceAltitude: TCheckBox;
    gbHue: TGroupBox;
    pbHue: TPaintBox;
    rbRandom: TRadioButton;
    rbTileList: TRadioButton;
    seForceAltitude: TSpinEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbHueClick(Sender: TObject);
    procedure pbHuePaint(Sender: TObject);
    procedure seForceAltitudeChange(Sender: TObject);
  protected
    procedure MouseLeave(var msg: TLMessage); message CM_MouseLeave;
  public
    { public declarations }
  end; 

var
  frmDrawSettings: TfrmDrawSettings;

implementation

uses
  UGameResources, UHue, UfrmHueSettings;

{ TfrmDrawSettings }

procedure TfrmDrawSettings.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmDrawSettings.FormDeactivate(Sender: TObject);
begin
  if not frmHueSettings.Visible then
    Close;
end;

procedure TfrmDrawSettings.FormShow(Sender: TObject);
begin
  Left := Mouse.CursorPos.x - 8;
  Top := Mouse.CursorPos.y - 8;
end;

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

procedure TfrmDrawSettings.MouseLeave(var msg: TLMessage);
begin
  try
    if (not frmHueSettings.Visible) and (not PtInRect(ClientRect, ScreenToClient(Mouse.CursorPos))) then
      Close;
  except
    Close;
  end;
end;

initialization
  {$I UfrmDrawSettings.lrs}

end.

