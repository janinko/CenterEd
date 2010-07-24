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
unit UGraphicHelper;

{$mode objfpc}{$H+}

interface

function ARGB2RGB(Value: Word): Integer;
function RGB2ARGB(Value: Integer): Word;

//New functions for Vampyre Imaging Lib
function DecodeUOColor(Value: Word): Integer;
function EncodeUOColor(Value: Integer): Word;

implementation

function ARGB2RGB(Value: Word): Integer;
var
  R, G, B: Byte;
begin
  R := ((Value shr 10) and $1F) * 8;
  G := ((Value shr 5) and $1F) * 8;
  B := (Value and $1F) * 8;

  Result := R + G shl 8 + B shl 16;
end;

function RGB2ARGB(Value: Integer): Word;
var
  R, G, B: Byte;
begin
  R := (Value and $FF) div 8;
  G := ((Value shr 8) and $FF) div 8;
  B := ((Value shr 16) and $FF) div 8;

  Result := (R shl 10) + (G shl 5) + B;
end;

function DecodeUOColor(Value: Word): Integer;
var
  R, G, B: Byte;
begin
  R := ((Value shr 10) and $1F) * 8;
  G := ((Value shr 5) and $1F) * 8;
  B := (Value and $1F) * 8;

  Result := B + G shl 8 + R shl 16;
end;

function EncodeUOColor(Value: Integer): Word;
var
  R, G, B: Byte;
begin
  B := (Value and $FF) div 8;
  G := ((Value shr 8) and $FF) div 8;
  R := ((Value shr 16) and $FF) div 8;

  Result := (R shl 10) + (G shl 5) + B;
end;

end.
