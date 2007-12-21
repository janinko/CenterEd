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
unit UMulProvider;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, UBufferedStreams, UMulBlock, UGenericIndex;

type
  TOnProgressEvent = procedure(Total, Current: Integer) of object;
  TMulEventHandler = class(TObject)
    constructor Create;
    destructor Destroy; override;
  protected
    FEvents: TList;
  public
    procedure RegisterEvent(AEvent: TMulBlockChanged);
    procedure UnregisterEvent(AEvent: TMulBlockChanged);
    procedure FireEvents(ABlock: TMulBlock);
  end;
  TMulProvider = class(TObject)
    constructor Create; overload; virtual;
    constructor Create(AData: TStream; AReadOnly: Boolean = False); overload; virtual;
    constructor Create(AData: string; AReadOnly: Boolean = False); overload; virtual;
    destructor Destroy; override;
  protected
    FData: TStream;
    FOwnsData: Boolean;
    FReadOnly: Boolean;
    FChangeEvents: TMulEventHandler;
    FFinishedEvents: TMulEventHandler;
    function CalculateOffset(AID: Integer): Integer; virtual; abstract;
    function GetData(AID, AOffset: Integer): TMulBlock; virtual; abstract;
    procedure SetData(AID, AOffset: Integer; ABlock: TMulBlock); virtual;
    procedure OnChanged(ABlock: TMulBlock);
    procedure OnFinished(ABlock: TMulBlock);
  public
    function GetBlock(AID: Integer): TMulBlock; virtual;
    procedure SetBlock(AID: Integer; ABlock: TMulBlock); virtual;
    procedure RegisterOnChangeEvent(AEvent: TMulBlockChanged);
    procedure UnregisterOnChangeEvent(AEvent: TMulBlockChanged);
    procedure RegisterOnFinishedEvent(AEvent: TMulBlockChanged);
    procedure UnregisterOnFinishedEvent(AEvent: TMulBlockChanged);
    property Block[ID: Integer]: TMulBlock read GetBlock write SetBlock;
    property Data: TStream read FData;
  end;
  TIndexedMulProvider = class(TMulProvider)
    constructor Create(AData, AIndex: TStream; AReadOnly: Boolean = False); overload; virtual;
    constructor Create(AData, AIndex: string; AReadOnly: Boolean = False); overload; virtual;
    destructor Destroy; override;
  protected
    FIndex: TBufferedReader;
    function CalculateIndexOffset(AID: Integer): Integer; virtual;
    function GetData(AID: Integer; AIndex: TGenericIndex): TMulBlock; reintroduce; virtual; abstract;
    procedure SetData(AID: Integer; AIndex: TGenericIndex; ABlock: TMulBlock); reintroduce; virtual;
    function GetVarious(AID: Integer; ABlock: TMulBlock; ADefault: Integer): Integer; virtual;
  public
    function GetBlock(AID: Integer): TMulBlock; override;
    procedure GetBlockEx(AID: Integer; var ABlock: TMulBlock; var AIndex: TGenericIndex); virtual;
    procedure SetBlock(AID: Integer; ABlock: TMulBlock); override;
    function Exists(AID: Integer): Boolean; virtual;
    procedure Defragment(ATempStream: TStream; AOnProgress: TOnProgressEvent = nil); virtual;
    property Index: TBufferedReader read FIndex;
  end;

implementation

type
  PMethod = ^TMethod;

{ TMulEventHandler }

constructor TMulEventHandler.Create;
begin
  inherited;
  FEvents := TList.Create;
end;

destructor TMulEventHandler.Destroy;
var
  i: Integer;
begin
  if Assigned(FEvents) then
  begin
    for i := 0 to FEvents.Count - 1 do
      Dispose(PMethod(FEvents.Items[i]));
    FreeAndNil(FEvents);
  end;
  inherited;
end;

procedure TMulEventHandler.FireEvents(ABlock: TMulBlock);
var
  i: Integer;
begin
  for i := 0 to FEvents.Count - 1 do
    TMulBlockChanged(FEvents.Items[i]^)(ABlock);
end;

procedure TMulEventHandler.RegisterEvent(AEvent: TMulBlockChanged);
var
  eventInfo: PMethod;
begin
  UnregisterEvent(AEvent);
  New(eventInfo);
  eventInfo^.Code := TMethod(AEvent).Code;
  eventInfo^.Data := TMethod(AEvent).Data;
  FEvents.Add(eventInfo);
end;

procedure TMulEventHandler.UnregisterEvent(AEvent: TMulBlockChanged);
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

{ TMulProvider }

constructor TMulProvider.Create(AData: TStream; AReadOnly: Boolean = False);
begin
  Create;
  FData := AData;
  FOwnsData := False;
  FReadOnly := AReadOnly;
end;

constructor TMulProvider.Create(AData: string; AReadOnly: Boolean = False);
var
  mode: Word;
begin
  Create;
  if AReadOnly then
    mode := fmOpenRead or fmShareDenyWrite
  else
    mode := fmOpenReadWrite or fmShareDenyWrite;
  FData := TFileStream.Create(AData, mode);
  FOwnsData := True;
  FReadOnly := AReadOnly;
end;

constructor TMulProvider.Create;
begin
  inherited;
  FChangeEvents := TMulEventHandler.Create;
  FFinishedEvents := TMulEventHandler.Create;
end;

destructor TMulProvider.Destroy;
begin
  if FOwnsData and Assigned(FData) then FreeAndNil(FData);
  if Assigned(FChangeEvents) then FreeAndNil(FChangeEvents);
  if Assigned(FFinishedEvents) then FreeAndNil(FFinishedEvents);
  inherited;
end;

function TMulProvider.GetBlock(AID: Integer): TMulBlock;
begin
  Result := GetData(AID, CalculateOffset(AID));
  Result.OnChanged := @OnChanged;
  Result.OnFinished := @OnFinished;
end;

procedure TMulProvider.OnChanged(ABlock: TMulBlock);
begin
  SetBlock(ABlock.ID, ABlock);
  FChangeEvents.FireEvents(ABlock);
end;

procedure TMulProvider.OnFinished(ABlock: TMulBlock);
begin
  FFinishedEvents.FireEvents(ABlock);
  ABlock.Free;
end;

procedure TMulProvider.RegisterOnChangeEvent(AEvent: TMulBlockChanged);
begin
  FChangeEvents.RegisterEvent(AEvent);
end;

procedure TMulProvider.RegisterOnFinishedEvent(AEvent: TMulBlockChanged);
begin
  FFinishedEvents.RegisterEvent(AEvent);
end;

procedure TMulProvider.SetBlock(AID: Integer; ABlock: TMulBlock);
begin
  if FReadOnly then Exit;
  SetData(AID, CalculateOffset(AID), ABlock);
end;

procedure TMulProvider.SetData(AID, AOffset: Integer; ABlock: TMulBlock);
begin
  if FReadOnly then Exit;
  FData.Position := AOffset;
  ABlock.Write(FData);
end;

procedure TMulProvider.UnregisterOnChangeEvent(AEvent: TMulBlockChanged);
begin
  FChangeEvents.UnregisterEvent(AEvent);
end;

procedure TMulProvider.UnregisterOnFinishedEvent(AEvent: TMulBlockChanged);
begin
  FFinishedEvents.UnregisterEvent(AEvent);
end;

{ TIndexedMulProvider }

function TIndexedMulProvider.CalculateIndexOffset(AID: Integer): Integer;
begin
  Result := 12 * AID;
end;

constructor TIndexedMulProvider.Create(AData, AIndex: TStream; AReadOnly: Boolean = False);
begin
  inherited Create(AData, AReadOnly);
  FIndex := TBufferedReader.Create(AIndex);
end;

constructor TIndexedMulProvider.Create(AData, AIndex: string; AReadOnly: Boolean = False);
var
  mode: Word;
begin
  inherited Create(AData, AReadOnly);
  if AReadOnly then
    mode := fmOpenRead or fmShareDenyWrite
  else
    mode := fmOpenReadWrite or fmShareDenyWrite;
  FIndex := TBufferedReader.Create(TFileStream.Create(AIndex, mode), True);
end;

procedure TIndexedMulProvider.Defragment(ATempStream: TStream; AOnProgress: TOnProgressEvent = nil);
var
  genericIndex: TGenericIndex;
begin
  if FReadOnly then Exit;
  ATempStream.Size := FData.Size;
  ATempStream.Position := 0;
  FIndex.Position := 0;
  while FIndex.Position < FIndex.Size do
  begin
    genericIndex := TGenericIndex.Create(FIndex);
    if genericIndex.Lookup <> LongInt($FFFFFFFF) then
    begin
      FData.Position := genericIndex.Lookup;
      genericIndex.Lookup := ATempStream.Position;
      ATempStream.CopyFrom(FData, genericIndex.Size);
      FIndex.Seek(-12, soFromCurrent);
      genericIndex.Write(FIndex);
    end;
    genericIndex.Free;
    if Assigned(AOnProgress) and (FIndex.Position mod 1200 = 0) then
      AOnProgress(FIndex.Size, FIndex.Position);
  end;
  FData.Size := ATempStream.Position;
  FData.Position := 0;
  ATempStream.Position := 0;
  FData.CopyFrom(ATempStream, FData.Size);
end;

destructor TIndexedMulProvider.Destroy;
begin
  if Assigned(FIndex) then FreeAndNil(FIndex);
  inherited;
end;

function TIndexedMulProvider.Exists(AID: Integer): Boolean;
var
  genericIndex: TGenericIndex;
begin
  FIndex.Position := CalculateIndexOffset(AID);
  genericIndex := TGenericIndex.Create(FIndex);
  Result := genericIndex.Lookup <> LongInt($FFFFFFFF);
  genericIndex.Free;
end;

function TIndexedMulProvider.GetBlock(AID: Integer): TMulBlock;
var
  genericIndex: TGenericIndex;
begin
  GetBlockEx(AID, Result, genericIndex);
  genericIndex.Free;
end;

procedure TIndexedMulProvider.GetBlockEx(AID: Integer;
  var ABlock: TMulBlock; var AIndex: TGenericIndex);
begin
  FIndex.Position := CalculateIndexOffset(AID);
  AIndex := TGenericIndex.Create(FIndex);
  ABlock := GetData(AID, AIndex);
  ABlock.OnChanged := @OnChanged;
  ABlock.OnFinished := @OnFinished;
end;

function TIndexedMulProvider.GetVarious(AID: Integer; ABlock: TMulBlock;
  ADefault: Integer): Integer;
begin
  Result := ADefault;
end;

procedure TIndexedMulProvider.SetBlock(AID: Integer; ABlock: TMulBlock);
var
  genericIndex: TGenericIndex;
begin
  if FReadOnly then Exit;
  FIndex.Position := CalculateIndexOffset(AID);
  genericIndex := TGenericIndex.Create(FIndex);
  SetData(AID, genericIndex, ABlock);
  FIndex.Position := CalculateIndexOffset(AID);
  genericIndex.Various := GetVarious(AID, ABlock, genericIndex.Various);
  genericIndex.Write(FIndex);
  genericIndex.Free;
end;

procedure TIndexedMulProvider.SetData(AID: Integer; AIndex: TGenericIndex;
  ABlock: TMulBlock);
var
  size: Integer;
begin
  if FReadOnly then Exit;
  size := ABlock.GetSize;
  if size = 0 then
  begin
    AIndex.Lookup := LongInt($FFFFFFFF);
    AIndex.Various := LongInt($FFFFFFFF);
  end else if (size > AIndex.Size) or (AIndex.Lookup = LongInt($FFFFFFFF)) then
  begin
    FData.Position := FData.Size;
    AIndex.Lookup := FData.Position;
    ABlock.Write(FData);
  end else
  begin
    FData.Position := AIndex.Lookup;
    ABlock.Write(FData);
  end;
  AIndex.Size := size;
end;

end.

