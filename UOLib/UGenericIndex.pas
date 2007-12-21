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
unit UGenericIndex;

{$mode objfpc}{$H+}

interface

uses
  Classes, UMulBlock;

type
  TGenericIndex = class(TMulBlock)
    constructor Create(Data: TStream);
    function Clone: TGenericIndex; override;
    function GetSize: Integer; override;
    procedure Write(Data: TStream); override;
  protected
    FLookup: LongInt;
    FSize: LongInt;
    FVarious: LongInt;
  published
    property Lookup: LongInt read FLookup write FLookup;
    property Size: LongInt read FSize write FSize;
    property Various: LongInt read FVarious write FVarious;
  end;

implementation

constructor TGenericIndex.Create(Data: TStream);
begin
  if assigned(Data) then
  begin
    Data.Read(FLookup, SizeOf(LongInt));
    Data.Read(FSize, SizeOf(LongInt));
    Data.Read(FVarious, SizeOf(LongInt));
  end;
end;

function TGenericIndex.Clone: TGenericIndex;
begin
  Result := TGenericIndex.Create(nil);
  Result.FLookup := FLookup;
  Result.FSize := FSize;
  Result.FVarious := FVarious;
end;

procedure TGenericIndex.Write(Data: TStream);
begin
  Data.Write(FLookup, SizeOf(LongInt));
  Data.Write(FSize, SizeOf(LongInt));
  Data.Write(FVarious, SizeOf(LongInt));
end;

function TGenericIndex.GetSize: Integer;
begin
  Result := 12;
end;

end.
