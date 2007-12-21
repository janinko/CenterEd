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
unit UVector;

interface

uses
  Classes;

type
  TVector = record
    X: Real;
    Y: Real;
    Z: Real;
  end;

function Vector(AX, AY, AZ: Real): TVector;
function VectorAdd(AVector1, AVector2: TVector): TVector;
function VectorDot(AVector1, AVector2: TVector): TVector;
function VectorCross(AVector1, AVector2: TVector): TVector;
function VectorNorm(AVector: TVector): TVector;

implementation

function Vector(AX, AY, AZ: Real): TVector;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

function VectorAdd(AVector1, AVector2: TVector): TVector;
begin
  Result.X := AVector1.X + AVector2.X;
  Result.Y := AVector1.Y + AVector2.Y;
  Result.Z := AVector1.Z + AVector2.Z;
end;

function VectorDot(AVector1, AVector2: TVector): TVector;
begin
  Result.X := AVector1.X * AVector2.X;
  Result.Y := AVector1.Y * AVector2.Y;
  Result.Z := AVector1.Z * AVector2.Z;
end;

function VectorCross(AVector1, AVector2: TVector): TVector;
begin
  Result.X := AVector1.Y * AVector2.Z - AVector1.Z * AVector2.Y;
  Result.Y := AVector1.Z * AVector2.X - AVector1.X * AVector2.Z;
  Result.Z := AVector1.X * AVector2.Y - AVector1.Y * AVector2.X;
end;

function VectorNorm(AVector: TVector): TVector;
var
  abs: Real;
begin
  abs := Sqrt(AVector.X * AVector.X + AVector.Y * AVector.Y + AVector.Z * AVector.Z);
  Result.X := AVector.X / abs;
  Result.Y := AVector.Y / abs;
  Result.Z := AVector.Z / abs;
end;

end.

