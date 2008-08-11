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
 *      Portions Copyright 2008 Andreas Schneider
 *)
unit URectList;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,Classes;
  
type
  TRectList = class(TList)
  protected
    function GetRect(AIndex: Integer): TRect;
    procedure SetRect(AIndex: Integer; ARect: TRect);
  public
    function Add(ALeft, ATop, ARight, ABottom: Integer): Integer;
    procedure Clear; override;
    procedure Delete(AIndex: Integer); reintroduce;
    property Rects[Index: Integer]: TRect read GetRect write SetRect;
  end;
  PRect = ^TRect;

implementation

{ TRectList }

function TRectList.GetRect(AIndex: Integer): TRect;
begin
  Result := PRect(Items[AIndex])^;
end;

procedure TRectList.SetRect(AIndex: Integer; ARect: TRect);
var
  internalRect: PRect;
begin
  internalRect := Items[AIndex];
  System.Move(ARect, internalRect^, SizeOf(TRect));
end;

function TRectList.Add(ALeft, ATop, ARight, ABottom: Integer): Integer;
var
  internalRect: PRect;
begin
  new(internalRect);
  internalRect^.Left := ALeft;
  internalRect^.Top := ATop;
  internalRect^.Right := ARight;
  internalRect^.Bottom := ABottom;
  Result := inherited Add(internalRect);
end;

procedure TRectList.Clear;
var
  i: Integer;
  internalRect: PRect;
begin
  for i := 0 to Count - 1 do
  begin
    internalRect := Items[i];
    dispose(internalRect);
  end;
  inherited;
end;

procedure TRectList.Delete(AIndex: Integer);
var
  internalRect: PRect;
begin
  internalRect := Items[AIndex];
  dispose(internalRect);
  inherited Delete(AIndex);
end;

end.

