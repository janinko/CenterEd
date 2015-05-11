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
 *      Portions Copyright 2015 Andreas Schneider
 *      Portions Copyright 2015 StaticZ
 *)
unit UfrmBoundaries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Spin, ExtCtrls, UfrmToolWindow;

type

  { TfrmBoundaries }

  TfrmBoundaries = class(TfrmToolWindow)
    gbZRestriction: TGroupBox;
    gbViewRestriction: TGroupBox;
    lblYSep: TLabel;
    lblXSep: TLabel;
    lblY: TLabel;
    lblX: TLabel;
    lblMaxZ: TLabel;
    lblMinZ: TLabel;
    seMaxZ: TSpinEdit;
    seMinZ: TSpinEdit;
    seMinX: TSpinEdit;
    seMaxX: TSpinEdit;
    seMinY: TSpinEdit;
    seMaxY: TSpinEdit;
    tbMaxZ: TTrackBar;
    tbMinZ: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure seMaxXChange(Sender: TObject);
    procedure seMaxYChange(Sender: TObject);
    procedure seMaxZChange(Sender: TObject);
    procedure seMinXChange(Sender: TObject);
    procedure seMinYChange(Sender: TObject);
    procedure seMinZChange(Sender: TObject);
    procedure tbMaxZChange(Sender: TObject);
    procedure tbMinZChange(Sender: TObject);
  public
    { public declarations }
  end; 

var
  frmBoundaries: TfrmBoundaries;

implementation

uses
  UfrmMain;

{ TfrmBoundaries }

procedure TfrmBoundaries.FormCreate(Sender: TObject);
begin
  seMinX.MaxValue := frmMain.Landscape.CellWidth - 1;
  seMaxX.MaxValue := seMinX.MaxValue;
  seMaxX.Value := seMaxX.MaxValue;

  seMinY.MaxValue := frmMain.Landscape.CellHeight - 1;
  seMaxY.MaxValue := seMinY.MaxValue;
  seMaxY.Value := seMaxX.MaxValue;
end;

procedure TfrmBoundaries.seMaxXChange(Sender: TObject);
begin
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.seMaxYChange(Sender: TObject);
begin
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.seMaxZChange(Sender: TObject);
begin
  tbMaxZ.Position := seMaxZ.Value;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.seMinXChange(Sender: TObject);
begin
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.seMinYChange(Sender: TObject);
begin
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.seMinZChange(Sender: TObject);
begin
  tbMinZ.Position := seMinZ.Value;
  frmMain.InvalidateFilter;
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

initialization
  {$I UfrmBoundaries.lrs}

end.

