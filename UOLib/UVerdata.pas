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
unit UVerdata;

interface

uses
  Classes, UGenericIndex;

type
  TFileType = (map0 = $00, staidx0, statics0, artidx, art, animidx, anim,
    soundidx, sound, texidx, texmaps, gumpidx, gumpart, multiidx, multi,
    skillsidx, skills, tiledata = $1E, animdata);
  TVerdataIndex = class(TGenericIndex)
    constructor Create(Data: TStream);
    function Clone: TVerdataIndex; override;
    procedure Write(Data: TStream); override;
    function GetSize: Integer; override;
  protected
    FFileID: TFileType;
    FBlock: LongInt;
  published
    property FileID: TFileType read FFileID write FFileID;
    property Block: LongInt read FBlock write FBlock;
  end;

implementation

constructor TVerdataIndex.Create;
var
  fileID: LongInt;

begin
  if assigned(Data) then
  begin
    Data.Read(fileID, SizeOf(LongInt));
    Data.Read(FBlock, SizeOf(LongInt));
    FFileID := TFileType(fileID);
  end;
  inherited;
end;

function TVerdataIndex.Clone: TVerdataIndex;
begin
  Result := TVerdataIndex.Create(nil);
  Result.FFileID := FFileID;
  Result.FBlock := FBlock;
  Result.FLookup := FLookup;
  Result.FSize := FSize;
  Result.FVarious := FVarious;
end;

procedure TVerdataIndex.Write;
var
  fileID: LongInt;
begin
  fileID := LongInt(FFileID);
  Data.Write(fileID, SizeOf(LongInt));
  Data.Write(FBlock, SizeOf(LongInt));
  inherited;
end;

function TVerdataIndex.GetSize: Integer;
begin
  Result := inherited GetSize + 8;
end;

end.

