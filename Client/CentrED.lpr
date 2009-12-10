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
program CentrED;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, lnetvisual, LazOpenGLContext, UdmNetwork, UfrmMain,
  UfrmLogin, UfrmInitialize, UfrmAccountControl, virtualtreeview_package,
  multiloglaz, UfrmEditAccount, UfrmDrawSettings, UfrmBoundaries,
  UfrmElevateSettings, UOverlayUI, UResourceManager, UfrmConfirmation,
  UfrmMoveSettings, UfrmAbout, UfrmHueSettings, UfrmRadar,
  UfrmLargeScaleCommand, UfrmVirtualLayer, UfrmFilter, UfrmTileInfo,
  UGUIPlatformUtils, UPlatformTypes, UfrmRegionControl, UPackets,
  UPacketHandlers, UAdminHandling, UGameResources, ULandscape, UfrmToolWindow,
  Logging, UTileDataProvider, UMap, UWorldItem, UStatics, UTiledata, UAnimData,
  UGLFont, UAnimDataProvider, UMulManager, UArtProvider, UTexmapProvider;

{$IFDEF WINDOWS}{$R CentrED.rc}{$ENDIF}

function GetApplicationName: String;
begin
  Result := 'CentrED';
end;

begin
  {$I CentrED.lrs}
  OnGetApplicationName := @GetApplicationName;
  Application.Initialize;
  Application.CreateForm(TdmNetwork, dmNetwork);
  Application.Run;
end.

