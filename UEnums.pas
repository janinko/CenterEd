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
unit UEnums;

{$mode objfpc}{$H+}

interface

type
  TLoginState = (lsOK              = 0,
                 lsInvalidUser     = 1,
                 lsInvalidPassword = 2,
                 lsAlreadyLoggedIn = 3,
                 lsNoAccess        = 4);

  TServerState = (ssRunning = 0,
                  ssFrozen  = 1,
                  ssOther   = 2);

  TAccessLevel = (alNone          = 0,
                  alView          = 1,
                  alNormal        = 2,
                  alAdministrator = 255);

  TModifyUserStatus = (muInvalidUsername = 0,
                       muAdded           = 1,
                       muModified        = 2);

  TDeleteUserStatus = (duNotFound = 0,
                       duDeleted  = 1);

  TModifyRegionStatus = (mrAdded           = 0,
                         mrModified        = 1);
  TDeleteRegionStatus = (drNotFound = 0,
                         drDeleted  = 1);

  TPasswordChangeStatus = (pcSuccess      = 0,
                           pcOldPwInvalid = 1,
                           pcNewPwInvalid = 2,
                           pcIdentical    = 3);
                       
function GetAccessLevelString(AAccessLevel: TAccessLevel): string;

implementation

function GetAccessLevelString(AAccessLevel: TAccessLevel): string;
begin
  Result := '';
  case AAccessLevel of
    alNone: Result := 'None';
    alView: Result := 'Viewer';
    alNormal: Result := 'Normal';
    alAdministrator: Result := 'Administrator';
  end;
end;

end. 
