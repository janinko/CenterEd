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
unit UfrmEditAccount;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UEnums, ComCtrls, ExtCtrls, CheckLst;

type

  { TfrmEditAccount }

  TfrmEditAccount = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    cbAccessLevel: TComboBox;
    CheckListBox1: TCheckListBox;
    edPassword: TEdit;
    edUsername: TEdit;
    Label1: TLabel;
    lblAccessLevel: TLabel;
    lblPassword: TLabel;
    lblPasswordHint: TLabel;
    lblUsername: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    tsGeneral: TTabSheet;
    tsRegions: TTabSheet;
  public
    function GetAccessLevel: TAccessLevel;
    procedure SetAccessLevel(AAccessLevel: TAccessLevel);
  end; 

var
  frmEditAccount: TfrmEditAccount;

implementation

{ TfrmEditAccount }

function TfrmEditAccount.GetAccessLevel: TAccessLevel;
begin
  case cbAccessLevel.ItemIndex of
    0: Result := alNone;
    1: Result := alView;
    2: Result := alNormal;
    3: Result := alAdministrator;
  end;
end;

procedure TfrmEditAccount.SetAccessLevel(AAccessLevel: TAccessLevel);
begin
  case AAccessLevel of
    alNone: cbAccessLevel.ItemIndex := 0;
    alView: cbAccessLevel.ItemIndex := 1;
    alNormal: cbAccessLevel.ItemIndex := 2;
    alAdministrator: cbAccessLevel.ItemIndex := 3;
  end;
end;

initialization
  {$I UfrmEditAccount.lrs}

end.

