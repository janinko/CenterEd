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
unit UfrmBoundaries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LMessages,
  LCLIntf, StdCtrls, ComCtrls, Spin, ExtCtrls;

type

  { TfrmBoundaries }

  TfrmBoundaries = class(TForm)
    lblMaxZ: TLabel;
    lblMinZ: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    seMaxZ: TSpinEdit;
    seMinZ: TSpinEdit;
    tbMinZ: TTrackBar;
    tbMaxZ: TTrackBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure seMaxZChange(Sender: TObject);
    procedure seMinZChange(Sender: TObject);
    procedure tbMaxZChange(Sender: TObject);
    procedure tbMinZChange(Sender: TObject);
  protected
    procedure MouseLeave(var msg: TLMessage); message CM_MouseLeave;
  public
    { public declarations }
  end; 

var
  frmBoundaries: TfrmBoundaries;

implementation

uses
  UfrmMain;

{ TfrmBoundaries }

procedure TfrmBoundaries.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmBoundaries.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmBoundaries.seMaxZChange(Sender: TObject);
begin
  tbMaxZ.Position := seMaxZ.Value;
end;

procedure TfrmBoundaries.seMinZChange(Sender: TObject);
begin
  tbMinZ.Position := seMinZ.Value;
end;

procedure TfrmBoundaries.tbMaxZChange(Sender: TObject);
begin
  seMaxZ.Value := tbMaxZ.Position;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.tbMinZChange(Sender: TObject);
begin
  seMinZ.Value := tbMinZ.Position;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.MouseLeave(var msg: TLMessage);
begin
  if not PtInRect(ClientRect, ScreenToClient(Mouse.CursorPos)) then
    Close;
end;

initialization
  {$I UfrmBoundaries.lrs}

end.

