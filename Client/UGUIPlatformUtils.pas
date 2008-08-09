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
 *      Portions Copyright 2008 Andreas Schneider
 *)
unit UGUIPlatformUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF LCLWin32}, windows{$ENDIF}
  {$IFDEF LCLGtk}, gtk{$ENDIF}
  {$IFDEF LCLGtk2}, gtk2{$ENDIF};
  
procedure SetWindowParent(AHandle, AParent: THANDLE);

implementation

procedure SetWindowParent(AHandle, AParent: THANDLE);
begin
  {$IFDEF LCLWin32}
  SetWindowLong(AHandle, GWL_HWNDPARENT, AParent);
  {$ENDIF}
  {$IFDEF LCLGtk}
  gtk_window_set_transient_for(PGtkWindow(AHandle), PGtkWindow(AParent));
  {$ENDIF}
  {$IFDEF LCLGtk2}
  gtk_window_set_transient_for(PGtkWindow(AHandle), PGtkWindow(AParent));
  {$ENDIF}
end;

initialization
  {$IFDEF LCLGtk2}
  gtk_rc_parse_string('style "user-font" { font_name = "Sans 8" } widget_class "*" style "user-font"');
  {$ENDIF}

end.

