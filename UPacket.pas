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
unit UPacket;

interface

uses
  Classes, UEnhancedMemoryStream;

type
  TPacket = class(TObject)
    constructor Create(APacketID: Byte; ALength: Cardinal);
    destructor Destroy; override;
  protected
    FStream: TEnhancedMemoryStream;
    FPacketID: Byte;
    FLength: Cardinal;
    function GetStream: TEnhancedMemoryStream;
  published
    property Stream: TEnhancedMemoryStream read GetStream;
    property PacketID: Byte read FPacketID;
    property PacketLength: Cardinal read FLength;
  end;

implementation

constructor TPacket.Create(APacketID: Byte; ALength: Cardinal);
begin
  FStream := TEnhancedMemoryStream.Create;
  FPacketID := APacketID;
  FLength := ALength;
  FStream.WriteByte(FPacketID);
  if FLength = 0 then
    FStream.WriteCardinal(0);
end;

destructor TPacket.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TPacket.GetStream: TEnhancedMemoryStream;
begin
  if FLength = 0 then
  begin
    FStream.Position := 1;
    FStream.WriteCardinal(FStream.Size);
  end;
  FStream.Position := 0;
  Result := FStream;
end;

end.

