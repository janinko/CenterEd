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
unit URadarProvider;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, UBufferedStreams;

type

  { TRadarProvider }

  TRadarProvider = class
    constructor Create; overload; virtual;
    constructor Create(AData: TStream; AReadOnly: Boolean = False); overload; virtual;
    constructor Create(AData: string; AReadOnly: Boolean = False); overload; virtual;
    destructor Destroy; override;    
  protected
    FData: TBufferedReader;
    FReadOnly: Boolean;
  public
    function GetColor(AID: Integer): Word;
    procedure SetColor(AID: Integer; AColor: Word);
  end;

implementation

{ TRaderProvider }

constructor TRadarProvider.Create;
begin
  inherited Create;
end;

constructor TRadarProvider.Create(AData: TStream; AReadOnly: Boolean);
begin
  Create;
  FData := TBufferedReader.Create(AData, False);
  FReadOnly := AReadOnly;
end;

constructor TRadarProvider.Create(AData: string; AReadOnly: Boolean);
var
  mode: Word;
begin
  Create;
  if AReadOnly then
    mode := fmOpenRead or fmShareDenyWrite
  else
    mode := fmOpenReadWrite or fmShareDenyWrite;
  FData := TBufferedReader.Create(TFileStream.Create(AData, mode), True);
  FReadOnly := AReadOnly;
end;

destructor TRadarProvider.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

function TRadarProvider.GetColor(AID: Integer): Word;
begin
  Result := 0;
  if (AID >= 0) and (AID < $10000) then
  begin
    FData.Position := SizeOf(Word) * AID;
    FData.Read(Result, SizeOf(Word));
  end;
end;

procedure TRadarProvider.SetColor(AID: Integer; AColor: Word);
begin
  if (not FReadOnly) and (AID >= 0) and (AID < $10000) then
  begin
    FData.Position := SizeOf(Word) * AID;
    FData.Write(AColor, SizeOf(Word));
  end;
end;

end.
