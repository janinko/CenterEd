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
unit UfrmToolWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  LCLIntf, LMessages, ExtCtrls;

type

  { TfrmToolWindow }

  TfrmToolWindow = class(TForm)
    tmClose: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject); virtual;
    procedure FormShow(Sender: TObject); virtual;
    procedure tmCloseTimer(Sender: TObject);
  protected
    function CanClose: Boolean; virtual;
    procedure MouseLeave(var msg: TLMessage); message CM_MouseLeave;
  public
    { public declarations }
  end; 

var
  frmToolWindow: TfrmToolWindow;

implementation

{ TfrmToolWindow }

procedure TfrmToolWindow.FormDeactivate(Sender: TObject);
begin
  if CanClose then
    Close;
end;

procedure TfrmToolWindow.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmToolWindow.FormShow(Sender: TObject);
begin
  Top := Mouse.CursorPos.y - 8;
  Left := Mouse.CursorPos.x - 8;

  OnDeactivate := nil;
  tmClose.Enabled := True;
end;

procedure TfrmToolWindow.tmCloseTimer(Sender: TObject);
begin
  tmClose.Enabled := False;
  OnDeactivate := @FormDeactivate;
  if CanClose then
    Close;
end;

function TfrmToolWindow.CanClose: Boolean;
begin
  Result := not PtInRect(ClientRect, ScreenToClient(Mouse.CursorPos));
end;

procedure TfrmToolWindow.MouseLeave(var msg: TLMessage);
begin
  if CanClose then
    Close;
end;

initialization
  {$I UfrmToolWindow.lrs}

end.

