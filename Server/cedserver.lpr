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
program cedserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes,
  lnetbase,
  UConfig, UCEDServer, URadarMap, ULargeScaleOperations, UPackets,
  UAdminHandling, UClientHandling, ULandscape, UPacketHandlers, URegions;
  
{$I version.inc}
  
begin
  Writeln('');
  Writeln('CentrED Server Version ', ProductVersion);
  Writeln('Copyright ', Copyright);
  //Writeln('================================');
  Writeln('');

  {$IFDEF Windows}
  if FileExists(ConfigFile) then
    Config := TConfig.Create(ConfigFile)
  else
    Config := TConfig.Init(ConfigFile);
  {$ELSE}
  if ParamStr(1) = '--init' then
    Config := TConfig.Init(ConfigFile)
  else if FileExists(ConfigFile) then
    Config := TConfig.Create(ConfigFile)
  else begin
    Writeln('No valid config file was found. Use --init to create one.');
    Halt;
  end;
  {$ENDIF}
  
  Writeln(TimeStamp, 'Initialization started');
  Randomize;
  CEDServerInstance := TCEDServer.Create;
  Writeln(TimeStamp, 'Initialization done');
  CEDServerInstance.Run;
  Write(TimeStamp, 'Shutting down ... ');
  FreeAndNil(CEDServerInstance);
  Config.Flush;
  FreeAndNil(Config);
  Writeln('done');
end.

