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
unit UBufferedStreams;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, UEnhancedMemoryStream;

type
  TBufferedStream = class(TEnhancedMemoryStream)
    constructor Create(ABaseStream: TStream; AOwnsBaseStream: Boolean = false); virtual;
    destructor Destroy; override;
  protected
    FBaseStream: TStream;
    FOwnsBaseStream: Boolean;
  public
    procedure Refresh; virtual;
    procedure Flush; virtual;
  end;
  TBufferedReader = class(TBufferedStream)
    constructor Create(ABaseStream: TStream; AOwnsBaseStream: Boolean = false); override;
    destructor Destroy; override;
  protected
    FReadBuffer: TEnhancedMemoryStream;
  public
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Refresh; override;
  end;

implementation

{ TBufferedStream }

constructor TBufferedStream.Create(ABaseStream: TStream;
  AOwnsBaseStream: Boolean);
begin
  inherited Create;
  FBaseStream := ABaseStream;
  FOwnsBaseStream := AOwnsBaseStream;
  Refresh;
end;

destructor TBufferedStream.Destroy;
begin
  if FOwnsBaseStream and Assigned(FBaseStream) then
    FreeAndNil(FBaseStream);
  inherited;
end;

procedure TBufferedStream.Flush;
begin
  FBaseStream.Size := Size;
  FBaseStream.Position := 0;
  FBaseStream.CopyFrom(Self, 0);
end;

procedure TBufferedStream.Refresh;
begin
  Size := FBaseStream.Size;
  Position := 0;  
  CopyFrom(FBaseStream, 0);
end;

{ TBufferedReader }

constructor TBufferedReader.Create(ABaseStream: TStream;
  AOwnsBaseStream: Boolean);
begin
  FReadBuffer := TEnhancedMemoryStream.Create;
  inherited;
end;

destructor TBufferedReader.Destroy;
begin
  if Assigned(FReadBuffer) then FreeAndNil(FReadBuffer);
  inherited;
end;

function TBufferedReader.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FReadBuffer.Read(Buffer, Count);
end;

procedure TBufferedReader.Refresh;
begin
  FReadBuffer.Size := FBaseStream.Size;
  FReadBuffer.Position := 0;
  FReadBuffer.CopyFrom(FBaseStream, 0);
  FReadBuffer.Position := 0;
  FBaseStream.Position := 0;
end;

function TBufferedReader.Seek(Offset: Integer; Origin: Word): Longint;
begin
  FBaseStream.Seek(Offset, Origin);
  Result := FReadBuffer.Seek(Offset, Origin);
end;

function TBufferedReader.Write(const Buffer; Count: Integer): Longint;
begin
  FBaseStream.Position := FReadBuffer.Position;
  FBaseStream.Write(Buffer, Count);
  Result := FReadBuffer.Write(Buffer, Count);
end;

end.

