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
unit UListSort;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TListSortCompare = function(Left, Right: TObject): Integer of object;

procedure ListSort(List: TList; Compare: TListSortCompare);

implementation

procedure ListSort(List: TList; Compare: TListSortCompare);
var
  iMin, iMax: Integer;
  Temp: Pointer;

  procedure sift;
  var
    i, j: integer;
  begin
    i := iMin;
    j := 2 * i;
    Temp := Pointer(List[i]);
    while j <= iMax do
    begin
      if j < iMax then
        if Compare(TObject(List[j]), TObject(List[j + 1])) > 0 then inc(j);
      if Compare(TObject(Temp), TObject(List[j])) <= 0 then break;
      List[i] := Pointer(List[j]);
      i := j;
      j := 2 * i;
    end;
    List[i] := Temp;
  end;

begin
  if List.Count > 0 then
  begin
    iMax := List.Count - 1;
    iMin := iMax div 2 + 1;
    while iMin > 0 do
    begin
      dec(iMin);
      sift;
    end;
    while iMax > 0 do
    begin
      Temp := Pointer(List[iMin]);
      List[iMin] := Pointer(List[iMax]);
      List[iMax] := Temp;
      dec(iMax);
      sift;
    end;
  end;
end;

end.
