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
unit UMulBlock;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  TMulBlock = class;
  TMulBlockChanged = procedure(ABlock: TMulBlock) of object;
  
  { TMulBlockEventHandler }

  TMulBlockEventHandler = class(TObject)
    constructor Create;
    destructor Destroy; override;
  protected
    FEvents: TList;
  public
    procedure RegisterEvent(AEvent: TMulBlockChanged);
    procedure UnregisterEvent(AEvent: TMulBlockChanged);
    procedure FireEvents(ABlock: TMulBlock);
  end;

  { TMulBlock }

  TMulBlock = class(TObject)
    constructor Create;
    destructor Destroy; override;
  protected
    FID: Integer;
    FOnChanged: TMulBlockChanged;
    FOnFinished: TMulBlockChanged;
    FOnDestroy: TMulBlockEventHandler;
  public
    class procedure Change(ABlock: TMulBlock); virtual;
    class procedure Finish(var ABlock: TMulBlock); virtual;
    function Clone: TMulBlock; virtual; abstract;
    function GetSize: Integer; virtual; abstract;
    procedure Write(AData: TStream); virtual; abstract;
    property ID: Integer read FID write FID;
    property OnChanged: TMulBlockChanged read FOnChanged write FOnChanged;
    property OnFinished: TMulBlockChanged read FOnFinished write FOnFinished;
    property OnDestroy: TMulBlockEventHandler read FOnDestroy;
  end;

implementation

type
  PMethod = ^TMethod;

{ TMulBlockEventHandler }

constructor TMulBlockEventHandler.Create;
begin
  inherited Create;
  FEvents := TList.Create;
end;

destructor TMulBlockEventHandler.Destroy;
var
  i: Integer;
begin
  if FEvents <> nil then
  begin
    for i := 0 to FEvents.Count - 1 do
      Dispose(PMethod(FEvents.Items[i]));
    FreeAndNil(FEvents);
  end;
  inherited Destroy;
end;

procedure TMulBlockEventHandler.RegisterEvent(AEvent: TMulBlockChanged);
var
  eventInfo: PMethod;
begin
  //UnregisterEvent(AEvent);
  New(eventInfo);
  eventInfo^.Code := TMethod(AEvent).Code;
  eventInfo^.Data := TMethod(AEvent).Data;
  FEvents.Add(eventInfo);
end;

procedure TMulBlockEventHandler.UnregisterEvent(AEvent: TMulBlockChanged);
var
  i: Integer;

  function RemoveEntry: Boolean;
  begin
    Dispose(PMethod(FEvents.Items[i]));
    FEvents.Delete(i);
    Result := True;
  end;

begin
  i := 0;
  while (i < FEvents.Count) and ((TMethod(AEvent).Code <> TMethod(FEvents.Items[i]^).Code) or (TMethod(AEvent).Data <> TMethod(FEvents.Items[i]^).Data) or not RemoveEntry) do
    Inc(i);
end;

procedure TMulBlockEventHandler.FireEvents(ABlock: TMulBlock);
var
  i: Integer;
begin
  for i := 0 to FEvents.Count - 1 do
    TMulBlockChanged(FEvents.Items[i]^)(ABlock);
end;

{ TMulBlock }

constructor TMulBlock.Create;
begin
  inherited Create;
  FOnDestroy := TMulBlockEventHandler.Create;
end;

destructor TMulBlock.Destroy;
begin
  if FOnDestroy <> nil then
  begin
    FOnDestroy.FireEvents(Self);
    FreeAndNil(FOnDestroy);
  end;
  inherited Destroy;
end;

class procedure TMulBlock.Change(ABlock: TMulBlock);
begin
  if ABlock <> nil then
  begin
    if ABlock.OnChanged <> nil then ABlock.OnChanged(ABlock);
  end;
end;

class procedure TMulBlock.Finish(var ABlock: TMulBlock);
begin
  if ABlock <> nil then
  begin
    if ABlock.OnFinished <> nil then ABlock.OnFinished(ABlock) else ABlock.Free;
    ABlock := nil;
  end;
end;

end.
