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
unit UAnimData;

interface

uses
  Classes, UMulBlock;

const
  AnimDataSize = 68;
  AnimDataGroupSize = 4 + (8 * AnimDataSize);

type

  { TAnimData }

  TAnimData = class(TMulBlock)
    constructor Create(AData: TStream);
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
    function Clone: TAnimData; override;
  protected
    FUnknown: Byte;
    FFrameCount: Byte;
    FFrameInterval: Byte;
    FFrameStart: Byte;
  public
    FrameData: array[0..63] of ShortInt;
    property Unknown: Byte read FUnknown write FUnknown;
    property FrameCount: Byte read FFrameCount write FFrameCount;
    property FrameInterval: Byte read FFrameInterval write FFrameInterval;
    property FrameStart: Byte read FFrameStart write FFrameStart;
  end;

  { TAnimDataGroup }

  TAnimDataGroup = class(TMulBlock)
    constructor Create(AData: TStream);
    destructor Destroy; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  protected
    FUnknown: LongInt;
    AnimData: array[0..7] of TAnimData;
    property Unknown: LongInt read FUnknown write FUnknown;
  end;

function GetAnimDataOffset(AID: Integer): Integer;

implementation

function GetAnimDataOffset(AID: Integer): Integer;
var
  group, tile: Integer;
begin
  group := AID div 8;
  tile := AID mod 8;

  Result := group * AnimDataGroupSize + 4 + tile * AnimDataSize;
end;

{ TAnimData }

constructor TAnimData.Create(AData: TStream);
begin
  if AData <> nil then
  begin
    AData.Read(FrameData, SizeOf(FrameData[0]) * Length(FrameData));
    AData.Read(FUnknown, SizeOf(FUnknown));
    AData.Read(FFrameCount, SizeOf(FFrameCount));
    AData.Read(FFrameInterval, SizeOf(FFrameInterval));
    AData.Read(FFrameStart, SizeOf(FFrameStart));
  end;
end;

procedure TAnimData.Write(AData: TStream);
begin
  AData.Write(FrameData, SizeOf(FrameData[0]) * Length(FrameData));
  AData.Write(FUnknown, SizeOf(FUnknown));
  AData.Write(FFrameCount, SizeOf(FFrameCount));
  AData.Write(FFrameInterval, SizeOf(FFrameInterval));
  AData.Write(FFrameStart, SizeOf(FFrameStart));
end;

function TAnimData.Clone: TAnimData;
var
  i: Integer;
begin
  Result := TAnimData.Create(nil);
  Result.FUnknown := FUnknown;
  Result.FFrameCount := FFrameCount;
  Result.FFrameInterval := FFrameInterval;
  Result.FFrameStart := FFrameStart;
  for i := 0 to 63 do
    Result.FrameData[i] := FrameData[i];
end;

function TAnimData.GetSize: Integer;
begin
  Result := AnimDataSize;
end;

{ TAnimDataGroup }

constructor TAnimDataGroup.Create(AData: TStream);
var
  i: Integer;
begin
  if AData <> nil then
  begin
    AData.Read(FUnknown, SizeOf(FUnknown));
  end;
  for i := 0 to 7 do
    AnimData[i] := TAnimData.Create(AData);
end;

destructor TAnimDataGroup.Destroy;
var
  i: Integer;
begin
  for i := 0 to 7 do
    AnimData[i].Free;
end;

procedure TAnimDataGroup.Write(AData: TStream);
var
  i: Integer;
begin
  AData.Write(FUnknown, SizeOf(FUnknown));
  for i := 0 to 7 do
    AnimData[i].Write(AData);
end;

function TAnimDataGroup.GetSize: Integer;
begin
  Result := AnimDataGroupSize;
end;

end.

