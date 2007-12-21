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
unit UHue;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, UMulBlock, UGraphicHelper;

type
  TColorTable = array[0..31] of Word;
  THue = class(TMulBlock)
    constructor Create(AData: TStream);
    function Clone: THue; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  protected
    FColorTable: TColorTable;
    FTableStart: Word;
    FTableEnd: Word;
    FName: string;
    procedure SetName(AValue: string);
    function GetName: string;
  public
    property ColorTable: TColorTable read FColorTable write FColorTable;
    property TableStart: Word read FTableStart write FTableStart;
    property TableEnd: Word read FTableEnd write FTableEnd;
    property Name: string read GetName write SetName;
  end;
  THueEntries = array[0..7] of THue;
  THueGroup = class(TMulBlock)
    constructor Create(AData: TStream);
    destructor Destroy; override;
    function Clone: THueGroup; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  protected
    FHeader: LongWord;
    FHueEntries: THueEntries;
    function GetHueEntry(AIndex: Integer): THue;
    procedure SetHueEntry(AIndex: Integer; AValue: THue);
  public
    property Header: LongWord read FHeader write FHeader;
    property HueEntries[Index: Integer]: THue read GetHueEntry write SetHueEntry;
  end;

implementation

{ THue }

function THue.Clone: THue;
var
  i: Integer;
begin
  Result := THue.Create(nil);
  for i := 0 to 31 do
    Result.FColorTable[i] := FColorTable[i];
  Result.FTableStart := FTableStart;
  Result.FTableEnd := FTableEnd;
  Result.FName := FName;
end;

constructor THue.Create(AData: TStream);
var
  i: Integer;
  buffer: TMemoryStream;
  color: Word;
begin
  SetLength(FName, 20);
  if Assigned(AData) then
  begin
    buffer := TMemoryStream.Create;
    buffer.CopyFrom(AData, 88);
    buffer.Position := 0;
    for i := 0 to 31 do
    begin
      buffer.Read(color, SizeOf(Word));
      FColorTable[i] := color;
    end;
    buffer.Read(FTableStart, SizeOf(Word));
    buffer.Read(FTableEnd, SizeOf(Word));
    buffer.Read(PChar(FName)^, 20);
    buffer.Free;    
  end;
end;

function THue.GetName: string;
begin
  Result := Trim(FName);
end;

function THue.GetSize: Integer;
begin
  Result := 88;
end;

procedure THue.SetName(AValue: string);
begin
  FName := AValue;
  SetLength(FName, 20);
end;

procedure THue.Write(AData: TStream);
var
  i: Integer;
  color: Word;
begin
  SetLength(FName, 20);
  for i := 0 to 31 do
  begin
    color := FColorTable[i];
    AData.Write(color, SizeOf(Word));
  end;
  AData.Write(FTableStart, SizeOf(Word));
  AData.Write(FTableEnd, SizeOf(Word));
  AData.Write(PChar(FName)^, 20);
end;

{ THueGroup }

function THueGroup.Clone: THueGroup;
var
  i: Integer;
begin
  Result := THueGroup.Create(nil);
  Result.FHeader := FHeader;
  for i := 0 to 7 do
    Result.SetHueEntry(i, FHueEntries[i].Clone);
end;

constructor THueGroup.Create(AData: TStream);
var
  i: Integer;
  buffer: TMemoryStream;
begin
  if Assigned(AData) then
  begin
    buffer := TMemoryStream.Create;
    buffer.CopyFrom(AData, 708);
    buffer.Position := 0;
    buffer.Read(FHeader, SizeOf(LongWord));
  end else
    buffer := nil;

  for i := 0 to 7 do
    FHueEntries[i] := THue.Create(buffer);

  if Assigned(buffer) then FreeAndNil(buffer);
end;

destructor THueGroup.Destroy;
var
  i: Integer;
begin
  for i := 0 to 7 do
    if Assigned(FHueEntries[i]) then
      FreeAndNil(FHueEntries[i]);
  inherited;
end;

function THueGroup.GetHueEntry(AIndex: Integer): THue;
begin
  Result := FHueEntries[AIndex];
end;

function THueGroup.GetSize: Integer;
begin
  Result := 708;
end;

procedure THueGroup.SetHueEntry(AIndex: Integer; AValue: THue);
begin
  if Assigned(FHueEntries[AIndex]) then FreeAndNil(FHueEntries[AIndex]);
  FHueEntries[AIndex] := AValue;
end;

procedure THueGroup.Write(AData: TStream);
var
  i: Integer;
begin
  AData.Write(FHeader, SizeOf(LongWord));
  for i := 0 to 7 do
    FHueEntries[i].Write(AData);
end;

end.

