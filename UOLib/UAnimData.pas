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
unit UAnimData;

interface

uses
  Classes, UMulBlock;

const
  AnimDataSize = 68;
  AnimDataGroupSize = 4 + (8 * AnimDataSize);

type
  TAnimData = class(TMulBlock)
    constructor Create(Data: TStream);
    function GetSize: Integer; override;
    procedure Write(Data: TStream); override;
  private
    FOffset: Int64;
    FUnknown: Byte;
    FFrameCount: Byte;
    FFrameInterval: Byte;
    FFrameStart: Byte;
  public
    FrameData: array[0..63] of ShortInt;
  published
    property Offset: Int64 read FOffset write FOffset;
    property Unknown: Byte read FUnknown write FUnknown;
    property FrameCount: Byte read FFrameCount write FFrameCount;
    property FrameInterval: Byte read FFrameInterval write FFrameInterval;
    property FrameStart: Byte read FFrameStart write FFrameStart;
  end;
  TAnimDataGroup = class(TMulBlock)
    constructor Create(Data: TStream);
    destructor Destroy; override;
    function GetSize: Integer; override;
    procedure Write(Data: TStream); override;
  private
    FOffset: Int64;
    FUnknown: LongInt;
  public
    AnimData: array[0..7] of TAnimData;
  published
    property Offset: Int64 read FOffset write FOffset;
    property Unknown: LongInt read FUnknown write FUnknown;
  end;

function GetAnimDataOffset(Block: Integer): Integer;

implementation

function GetAnimDataOffset;
var
  group, tile: Integer;
begin
  group := Block div 8;
  tile := Block mod 8;

  Result := group * AnimDataGroupSize + 4 + tile * AnimDataSize;
end;

constructor TAnimData.Create;
begin
  if assigned(Data) then
  begin
    FOffset := Data.Position;
    Data.Read(FrameData, 64);
    Data.Read(FUnknown, SizeOf(Byte));
    Data.Read(FFrameCount, SizeOf(Byte));
    Data.Read(FFrameInterval, SizeOf(Byte));
    Data.Read(FFrameStart, SizeOf(Byte));
  end;
end;

procedure TAnimData.Write;
begin
  Data.Write(FrameData, 64);
  Data.Write(FUnknown, SizeOf(Byte));
  Data.Write(FFrameCount, SizeOf(Byte));
  Data.Write(FFrameInterval, SizeOf(Byte));
  Data.Write(FFrameStart, SizeOf(Byte));
end;

function TAnimData.GetSize;
begin
  GetSize := AnimDataSize;
end;

constructor TAnimDataGroup.Create;
var
  i: Integer;
begin
  if assigned(Data) then
  begin
    FOffset := Data.Position;
    Data.Read(FUnknown, SizeOf(LongInt));
  end;
  for i := 0 to 7 do
    AnimData[i] := TAnimData.Create(Data);
end;

destructor TAnimDataGroup.Destroy;
var
  i: Integer;
begin
  for i := 0 to 7 do
    AnimData[i].Free;
end;

procedure TAnimDataGroup.Write;
var
  i: Integer;
begin
  Data.Write(FUnknown, SizeOf(LongInt));
  for i := 0 to 7 do
    AnimData[i].Write(Data);
end;

function TAnimDataGroup.GetSize;
begin
  GetSize := AnimDataGroupSize;
end;

end.

