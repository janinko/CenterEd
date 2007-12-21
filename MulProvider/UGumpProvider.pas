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
unit UGumpProvider;

{$mode objfpc}{$H+}

interface

uses
  UMulProvider, UMulBlock, UGenericIndex, UGump;

type
  TGumpProvider = class(TIndexedMulProvider)
  protected
    function GetData(AID: Integer; AIndex: TGenericIndex): TMulBlock; override;
    function GetVarious(AID: Integer; ABlock: TMulBlock; ADefault: Integer): Integer; override;
  end;

implementation

{ TGumpProvider }

function TGumpProvider.GetData(AID: Integer; AIndex: TGenericIndex): TMulBlock;
begin
  if AIndex.Lookup <> LongInt($FFFFFFFF) then
    Result := TGump.Create(FData, TGumpIndex(AIndex))
  else
    Result := TGump.Create(0, 0);
  Result.ID := AID;
end;

function TGumpProvider.GetVarious(AID: Integer; ABlock: TMulBlock;
  ADefault: Integer): Integer;
begin
  Result := TGump(ABlock).Graphic.Height or (TGump(ABlock).Graphic.Width shl 16);
end;

end.

 
