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
unit ULinkedList;

interface

uses
  SysUtils;

type
  PLinkedItem = ^TLinkedItem;
  TLinkedItem = record
    ID: Integer;
    Data: Pointer;
    Next: PLinkedItem;
  end;
  TLinkedList = class(TObject)
    constructor Create; virtual;
    destructor Destroy; override;
  protected
    FFirst: PLinkedItem;
    FLast: PLinkedItem;
  public
    procedure Clear; virtual;
    function Iterate(var ALinkedItem: PLinkedItem): Boolean; virtual;
    function Add(AID: Integer; AData: Pointer): PLinkedItem; virtual;
    procedure Delete(AData: Pointer); overload; virtual;
    procedure Delete(AID: Integer); overload; virtual;
    function Get(AID: Integer): Pointer; virtual;
    property Last: PLinkedItem read FLast;
  end;

implementation

{ TBlockList }

function TLinkedList.Add(AID: Integer; AData: Pointer): PLinkedItem;
var
  current: PLinkedItem;
begin
  New(current);
  current^.ID := AID;
  current^.Data := AData;
  current^.Next := nil;
  if FFirst = nil then FFirst := current;
  if FLast <> nil then FLast^.Next := current;
  FLast := current;
  Result := current;
end;

procedure TLinkedList.Clear;
var
  current, next: PLinkedItem;
begin
  current := FFirst;
  while current <> nil do
  begin
    next := current^.Next;
    Dispose(current);
    current := next;
  end;
  FFirst := nil;
  FLast := nil;
end;

constructor TLinkedList.Create;
begin
  inherited Create;
  FFirst := nil;
  FLast := nil;
end;

procedure TLinkedList.Delete(AData: Pointer);
var
  currentItem, lastItem, nextItem: PLinkedItem;
begin
  lastItem := nil;
  currentItem := FFirst;
  while currentItem <> nil do
  begin
    if currentItem^.Data = AData then
    begin
      if FFirst = currentItem then FFirst := currentItem^.Next;
      if FLast = currentItem then FLast := lastItem;
      if lastItem <> nil then lastItem^.Next := currentItem^.Next;
      Dispose(currentItem);
      nextItem := nil;
    end else
      nextItem := currentItem^.Next;
    lastItem := currentItem;
    currentItem := nextItem;
  end;
end;

procedure TLinkedList.Delete(AID: Integer);
var
  currentItem, lastItem, nextItem: PLinkedItem;
begin
  lastItem := nil;
  currentItem := FFirst;
  while currentItem <> nil do
  begin
    if currentItem^.ID = AID then
    begin
      if FFirst = currentItem then FFirst := currentItem^.Next;
      if FLast = currentItem then FLast := lastItem;
      if lastItem <> nil then lastItem^.Next := currentItem^.Next;
      Dispose(currentItem);
      nextItem := nil;
    end else
      nextItem := currentItem^.Next;
    lastItem := currentItem;
    currentItem := nextItem;
  end;
end;

destructor TLinkedList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TLinkedList.Get(AID: Integer): Pointer;
var
  item: PLinkedItem;
begin
  Result := nil;
  item := nil;
  while Iterate(item) and (Result = nil) do
    if item^.ID = AID then
      Result := item^.Data;
end;

function TLinkedList.Iterate(var ALinkedItem: PLinkedItem): Boolean;
begin
  if ALinkedItem = nil then
    ALinkedItem := FFirst
  else
    ALinkedItem := ALinkedItem^.Next;
  Result := ALinkedItem <> nil;
end;

end.

