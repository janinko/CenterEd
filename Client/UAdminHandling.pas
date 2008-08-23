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
unit UAdminHandling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UPacket, UPacketHandlers, UEnhancedMemoryStream, UEnums;
  
type

  TAdminHandlerAlreadyAssignedException = class(Exception);

  { TFlushServerPacket }

  TFlushServerPacket = class(TPacket)
    constructor Create;
  end;
  
  { TQuitServerPacket }

  TQuitServerPacket = class(TPacket)
    constructor Create(AReason: string);
  end;
  
procedure AssignAdminPacketHandler(APacketID: Byte; AHandler: TPacketHandler);
procedure OnAdminHandlerPacket(ABuffer: TEnhancedMemoryStream);

var
  AdminPacketHandlers: array[0..$FF] of TPacketHandler;

implementation

procedure AssignAdminPacketHandler(APacketID: Byte; AHandler: TPacketHandler);
begin
  if AdminPacketHandlers[APacketID] <> nil then
    raise TAdminHandlerAlreadyAssignedException.CreateFmt(
      'The AdminPacketHandler $%.2x is already assigned!', [APacketID]);

  AdminPacketHandlers[APacketID] := AHandler;
end;

procedure OnAdminHandlerPacket(ABuffer: TEnhancedMemoryStream);
var
  packetHandler: TPacketHandler;
begin
  packetHandler := AdminPacketHandlers[ABuffer.ReadByte];
  if packetHandler <> nil then
    packetHandler.Process(ABuffer);
end;

{ TFlushServerPacket }

constructor TFlushServerPacket.Create;
begin
  inherited Create($03, 0);
  FStream.WriteByte($01);
end;

{ TQuitServerPacket }

constructor TQuitServerPacket.Create(AReason: string);
begin
  inherited Create($03, 0);
  FStream.WriteByte($02);
  FStream.WriteStringNull(AReason);
end;

{$WARNINGS OFF}
var
  i: Integer;

initialization
  for i := 0 to $FF do
    AdminPacketHandlers[i] := nil;
finalization
  for i := 0 to $FF do
    if AdminPacketHandlers[i] <> nil then
      AdminPacketHandlers[i].Free;
{$WARNINGS ON}

end.

