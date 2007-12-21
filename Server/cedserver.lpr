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
program cedserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes,
  lnetbase,
  UConfig, UCEDServer, URadarMap, ULargeScaleOperations;
  
{$I version.inc}
  
begin
  Writeln('UO CentrED Server Version ', ProductVersion);
  Writeln('Copyright ', Copyright);
  //Writeln('================================');
  Writeln('');

  {$IFDEF Windows}
  if not LoadConfig then
  begin
    InitConfig;
    Writeln('');
  end;
  {$ELSE}
  if ParamStr(1) = '--init' then
  begin
    InitConfig;
    Halt;
  end;
  
  if not LoadConfig then
  begin
    Writeln('No valid config file was found. Use --init to create one.');
    Halt;
  end;
  {$ENDIF}
  
  Write(TimeStamp, 'Initializing ... ');
  Randomize;
  CEDServerInstance := TCEDServer.Create;
  Writeln('Done');
  CEDServerInstance.Run;
  Write(TimeStamp, 'Terminating ... ');
  CEDServerInstance.Free;
  Writeln('Done');
end.

