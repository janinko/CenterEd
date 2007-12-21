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
unit UHueProvider;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Contnrs, UMulProvider, UMulBlock, UHue;

type
  THueProvider = class(TMulProvider)
    constructor Create(AData: TStream; AReadOnly: Boolean = False); overload; override;
    constructor Create(AData: string; AReadOnly: Boolean = False); overload; override;
    destructor Destroy; override;
  protected
    FHueGroups: TObjectList;
    procedure InitList;
    function CalculateOffset(AID: Integer): Integer; override;
    function GetData(AID, AOffset: Integer): TMulBlock; override;
    procedure SetData(AID, AOffset: Integer; ABlock: TMulBlock); override;
    function GetHue(AIndex: Integer): THue;
    function GetCount: Integer;
  public
    function GetBlock(AID: Integer): TMulBlock; override;
    property Hues[Index: Integer]: THue read GetHue;
    property Count: Integer read GetCount;
  end;

implementation

{ THueProvider }

function THueProvider.CalculateOffset(AID: Integer): Integer;
begin
  Result := (AID div 8) * 708 + (AID mod 8) * 88;
end;

constructor THueProvider.Create(AData: TStream; AReadOnly: Boolean = False);
begin
  inherited;
  InitList;
end;

constructor THueProvider.Create(AData: string; AReadOnly: Boolean = False);
begin
  inherited;
  InitList;
end;

destructor THueProvider.Destroy;
begin
  FHueGroups.Free;
  inherited;
end;

function THueProvider.GetBlock(AID: Integer): TMulBlock;
begin
  Result := GetData(AID, 0);
end;

function THueProvider.GetCount: Integer;
begin
  Result := FHueGroups.Count * 8;
end;

function THueProvider.GetData(AID, AOffset: Integer): TMulBlock;
var
  group, entry: Integer;
begin
  group := (AID div 8) mod FHueGroups.Count;
  entry := AID mod 8;
  Result := TMulBlock(THueGroup(FHueGroups.Items[group]).HueEntries[entry].Clone);
  Result.ID := AID;
  Result.OnChanged := @OnChanged;
  Result.OnFinished := @OnFinished;
end;

function THueProvider.GetHue(AIndex: Integer): THue;
var
  group, entry: Integer;
begin
  group := (AIndex div 8) mod FHueGroups.Count;
  entry := AIndex mod 8;
  Result := THue(THueGroup(FHueGroups.Items[group]).HueEntries[entry]);
  Result.ID := AIndex;
end;

procedure THueProvider.InitList;
var
  i: Integer;
begin
  FHueGroups := TObjectList.Create;
  FHueGroups.Count := FData.Size div 708;
  FData.Position := 0;
  i := 0;
  while FData.Position < FData.Size do
  begin
    FHueGroups.Items[i] := THueGroup.Create(FData);
    Inc(i);
  end;
end;

procedure THueProvider.SetData(AID, AOffset: Integer;
  ABlock: TMulBlock);
var
  group, entry: Integer;
begin
  group := AID div 8;
  entry := AID mod 8;

  if (group >= FHueGroups.Count) or (group < 0) then
  begin
    group := FHueGroups.Count;
    FHueGroups.Add(THueGroup.Create(nil));
    entry := 0;
  end;

  THueGroup(FHueGroups.Items[group]).HueEntries[entry] := THue(ABlock.Clone);

  if not FReadOnly then
  begin
    FData.Position := AOffset;
    ABlock.Write(FData);
  end;
end;

end.

