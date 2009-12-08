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
unit UAnimDataProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMulProvider, UMulBlock, UAnimData;

type

  TAnimDataArray = array[$0..$3FFF] of TAnimData;

  { TAnimDataProvider }

  TAnimDataProvider = class(TMulProvider)
    constructor Create(AData: TStream; AReadOnly: Boolean = False); overload;
        override;
    constructor Create(AData: string; AReadOnly: Boolean = False); overload;
         override;
    destructor Destroy; override;
  protected
    FAnimData: TAnimDataArray;
    function CalculateOffset(AID: Integer): Integer; override;
    function GetData(AID, AOffset: Integer): TAnimData; override;
    procedure InitArray;
    procedure SetData(AID, AOffset: Integer; ABlock: TMulBlock); override;
  public
    property AnimData: TAnimDataArray read FAnimData;
    function GetBlock(AID: Integer): TAnimData; override;
  end;

implementation

{ TAnimDataProvider }

constructor TAnimDataProvider.Create(AData: TStream; AReadOnly: Boolean);
begin
  inherited Create(AData, AReadOnly);
  InitArray;
end;

constructor TAnimDataProvider.Create(AData: string; AReadOnly: Boolean);
begin
  inherited Create(AData, AReadOnly);
  InitArray;
end;

destructor TAnimDataProvider.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FAnimData) - 1 do
    FreeAndNil(FAnimData[i]);

  inherited Destroy;
end;

function TAnimDataProvider.CalculateOffset(AID: Integer): Integer;
begin
  Result := GetAnimDataOffset(AID);
end;

function TAnimDataProvider.GetData(AID, AOffset: Integer): TAnimData;
begin
  Result := FAnimData[AID];
end;

procedure TAnimDataProvider.InitArray;
var
  i: Integer;
begin
  for i := 0 to Length(FAnimData) - 1 do
  begin
    FData.Position := GetAnimDataOffset(i);
    FAnimData[i] := TAnimData.Create(FData);
  end;
end;

procedure TAnimDataProvider.SetData(AID, AOffset: Integer; ABlock: TMulBlock);
begin
  FreeAndNil(FAnimData[AID]);
  FAnimData[AID] := TAnimData(ABlock.Clone);

  if not FReadOnly then
  begin
    FData.Position := AOffset;
    ABlock.Write(FData);
  end;
end;

function TAnimDataProvider.GetBlock(AID: Integer): TAnimData;
begin
  Result := FAnimData[AID].Clone;
end;

end.

