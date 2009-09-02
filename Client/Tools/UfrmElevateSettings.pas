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
unit UfrmElevateSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, UfrmToolWindow;

type

  { TfrmElevateSettings }

  TfrmElevateSettings = class(TfrmToolWindow)
    cbRandomHeight: TCheckBox;
    rbLower: TRadioButton;
    rbRaise: TRadioButton;
    rbSet: TRadioButton;
    seRandomHeight: TSpinEdit;
    seZ: TSpinEdit;
    procedure seRandomHeightChange(Sender: TObject);
  public
    { public declarations }
  end; 

var
  frmElevateSettings: TfrmElevateSettings;

implementation

{ TfrmElevateSettings }

procedure TfrmElevateSettings.seRandomHeightChange(Sender: TObject);
begin
  cbRandomHeight.Checked := True;
end;

initialization
  {$I UfrmElevateSettings.lrs}

end.

