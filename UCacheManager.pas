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
unit UCacheManager;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  SysUtils, Classes;

type

  ICacheable = interface['{0ABAA4DE-8128-47B3-ABFE-5250A74A0428}']
    function CanBeRemoved: Boolean;
    procedure RemoveFromCache;
  end;

  { TCacheManager }

  generic TCacheManager<T> = class
  type public
    { Types }
    TRemoveObjectEvent = procedure(AObject: T) of object;

    PCacheEntry = ^TCacheEntry;
    TCacheEntry = record
      ID: Integer;
      Obj: T;
      Next: PCacheEntry;
    end;
  var protected
    { Members }
    FSize: Integer;
    FFirst: PCacheEntry;
    FLast: PCacheEntry;
    FOnRemoveObject: TRemoveObjectEvent;
    procedure DoRemoveObject(var AObject: T; ANotify: Boolean = True);
  public
    constructor Create(ASize: Integer);
    destructor Destroy; override;

    { Fields }
    property OnRemoveObject: TRemoveObjectEvent read FOnRemoveObject
      write FOnRemoveObject;

    { Methods }
    function QueryID(const AID: Integer; out AObj: T): Boolean;
    procedure StoreID(AID: Integer; AObj: T);
    procedure DiscardID(AID: Integer);
    procedure DiscardObj(AObj: T);
    procedure RemoveID(AID: Integer);
    procedure Clear;
    function Iterate(var ACacheEntry: PCacheEntry): Boolean;
  end;

implementation

uses
  Logging;

{ TCacheManager }

procedure TCacheManager.DoRemoveObject(var AObject: T; ANotify: Boolean = True);
var
  cacheable: ICacheable;
begin
  if ANotify and Assigned(FOnRemoveObject) then FOnRemoveObject(AObject);

  if TObject(AObject).GetInterface(ICacheable, cacheable) then
    cacheable.RemoveFromCache
  else
    TObject(AObject).Free;
  TObject(AObject) := nil;
end;

constructor TCacheManager.Create(ASize: Integer);
var
  i: Integer;
  current: PCacheEntry;
begin
  FOnRemoveObject := nil;
  FSize := ASize;
  if FSize > 0 then
  begin
    New(FFirst);
    current := FFirst;
    current^.ID := LongInt($FFFFFFFF);
    current^.Obj := nil;
    for i := 2 to FSize do
    begin
      New(current^.Next);
      FLast := current;
      current := current^.Next;
      current^.ID := LongInt($FFFFFFFF);
      current^.Obj := nil;
    end;
    current^.Next := nil;
  end;
end;

destructor TCacheManager.Destroy;
var
  i: Integer;
  current, last: PCacheEntry;
begin
  current := FFirst;
  for i := 1 to FSize do
  begin
    if Pointer(current^.Obj) <> nil then
      DoRemoveObject(current^.Obj);
    last := current;
    current := current^.Next;
    Dispose(last);
  end;
  inherited;
end;

procedure TCacheManager.DiscardID(AID: Integer);
var
  current: PCacheEntry;
begin
  current := FFirst;
  while (current <> nil) do
  begin
    if (current^.ID = AID) then
    begin
      current^.ID := LongInt($FFFFFFFF);
      current^.Obj := nil;
      current := nil;
    end else
      current := current^.Next;
  end;
end;

procedure TCacheManager.DiscardObj(AObj: T);
var
  current: PCacheEntry;
begin
  current := FFirst;
  while (current <> nil) do
  begin
    if (current^.Obj = AObj) then
    begin
      current^.ID := LongInt($FFFFFFFF);
      current^.Obj := nil;
      current := nil;
    end else
      current := current^.Next;
  end;
end;

procedure TCacheManager.RemoveID(AID: Integer);
var
  current: PCacheEntry;
begin
  current := FFirst;
  FLast := current;
  while (current <> nil) do
  begin
    if (current^.ID = AID) then
    begin
      current^.ID := LongInt($FFFFFFFF);
      if Pointer(current^.Obj) <> nil then
        DoRemoveObject(current^.Obj, False);
    end;
    if (current^.Next <> nil) then
      FLast := current;
    current := current^.Next;
  end;
end;

procedure TCacheManager.Clear;
var
  current: PCacheEntry;
begin
  current := FFirst;
  while current <> nil do
  begin
    if Pointer(current^.Obj) <> nil then
    begin
      current^.ID := LongInt($FFFFFFFF);
      DoRemoveObject(current^.Obj);
    end;
    current := current^.Next;
  end;
end;

function TCacheManager.Iterate(var ACacheEntry: PCacheEntry): Boolean;
begin
  if ACacheEntry = nil then
    ACacheEntry := FFirst
  else
    ACacheEntry := ACacheEntry^.Next;
  Result := ACacheEntry <> nil;
end;

function TCacheManager.QueryID(const AID: Integer;
  out AObj: T): Boolean;
var
  current: PCacheEntry;
begin
  current := FFirst;
  FLast := current;
  Result := False;
  while (current <> nil) and (not Result) do
  begin
    if (current^.ID = AID) then
    begin
      Result := True;
      AObj := current^.Obj;
      if current <> FFirst then
      begin
        FLast^.Next := current^.Next;
        current^.Next := FFirst;
        FFirst := current;
      end;
    end;
    if (current^.Next <> nil) then
      FLast := current;
    current := current^.Next;
  end;
end;

procedure TCacheManager.StoreID(AID: Integer; AObj: T);
var
  current: PCacheEntry;
  cacheable: ICacheable;
  i: Integer;
begin
  current := FLast^.Next; //well, FLast is not really the last, but the one before the last ;)
  FLast^.Next := nil;
  current^.Next := FFirst;
  FFirst := current;
  if Pointer(FFirst^.Obj) <> nil then //if the last cache entry did contain an object, remove it now or grow
  begin
    if TObject(FFirst^.Obj).GetInterface(ICacheable, cacheable) and
      not cacheable.CanBeRemoved then
    begin
      Logger.Send([lcInfo], 'Cache growing (%s)', [ClassName]);
      New(FLast^.Next);
      current := FLast^.Next;
      current^.ID := FFirst^.ID;
      current^.Obj := FFirst^.Obj;
      for i := 2 to FSize do
      begin
        New(current^.Next);
        FLast := current;
        current := current^.Next;
        current^.ID := LongInt($FFFFFFFF);
        current^.Obj := nil;
      end;
      current^.Next := nil;
      FSize := FSize * 2;
    end else
      DoRemoveObject(current^.Obj);
  end;
  FFirst^.ID := AID;
  FFirst^.Obj := AObj;
end;

end.

