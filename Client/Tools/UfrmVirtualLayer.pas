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
unit UfrmVirtualLayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLIntf,
  LMessages, StdCtrls, Spin, ComCtrls, ExtCtrls;

type

  { TfrmVirtualLayer }

  TfrmVirtualLayer = class(TForm)
    cbShowLayer: TCheckBox;
    Panel1: TPanel;
    seZ: TSpinEdit;
    tbZ: TTrackBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure seZChange(Sender: TObject);
    procedure tbZChange(Sender: TObject);
  protected
    procedure MouseLeave(var msg: TLMessage); message CM_MouseLeave;
  public
    { public declarations }
  end; 

var
  frmVirtualLayer: TfrmVirtualLayer;

implementation

{ TfrmVirtualLayer }

procedure TfrmVirtualLayer.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmVirtualLayer.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmVirtualLayer.seZChange(Sender: TObject);
begin
  tbZ.Position := seZ.Value;
end;

procedure TfrmVirtualLayer.tbZChange(Sender: TObject);
begin
  seZ.Value := tbZ.Position;
end;

procedure TfrmVirtualLayer.MouseLeave(var msg: TLMessage);
begin
  if not PtInRect(ClientRect, ScreenToClient(Mouse.CursorPos)) then
    Close;
end;

initialization
  {$I UfrmVirtualLayer.lrs}

end.

