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
unit UAccount;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, md5, contnrs, math, UEnums;
  
type

  { TAccount }

  TAccount = class(TObject)
    constructor Create(AAccountString: string);
    constructor Create(AName, APasswordHash: string; AAccessLevel: TAccessLevel);
  protected
    FName: string;
    FAccessLevel: TAccessLevel;
    FPasswordHash: string;
    FLastPos: TPoint;
    procedure SetAccessLevel(const AValue: TAccessLevel);
    procedure SetPasswordHash(const AValue: string);
    procedure SetLastPos(const AValue: TPoint);
  public
    property Name: string read FName;
    property AccessLevel: TAccessLevel read FAccessLevel write SetAccessLevel;
    property PasswordHash: string read FPasswordHash write SetPasswordHash;
    property LastPos: TPoint read FLastPos write SetLastPos;
    procedure Flush;
  end;
  
  { TAccountList }

  TAccountList = class(TObjectList)
    constructor Create; reintroduce;
  public
    function IndexOf(AName: string): Integer;
    function Find(AName: string): TAccount;
    procedure Delete(AName: string);
  end;

implementation

uses
  UCEDServer, UConfig;

{ TAccount }

constructor TAccount.Create(AAccountString: string);
var
  i: Integer;
  attribs: TStringList;
begin
  inherited Create;
  i := Pos('=', AAccountString);
  if i > 0 then
    FName := Trim(Copy(AAccountString, 1, i-1));
  AAccountString := Copy(AAccountString, i+1, Length(AAccountString));

  attribs := TStringList.Create;
  if ExtractStrings([':'], [' '], PChar(AAccountString), attribs) >= 2 then
  begin
    FAccessLevel := TAccessLevel(StrToInt(attribs.Strings[0]));
    FPasswordHash := attribs.Strings[1];
  end;
  if attribs.Count >= 4 then
  begin
    FLastPos.x := EnsureRange(StrToInt(attribs.Strings[2]), 0, Config.ReadInteger('Parameters', 'Width', 0) * 8 - 1);
    FLastPos.y := EnsureRange(StrToInt(attribs.Strings[3]), 0, Config.ReadInteger('Parameters', 'Height', 0) * 8 - 1);
  end else
  begin
    FLastPos.x := 0;
    FLastPos.y := 0;
  end;
  attribs.Free;
end;

constructor TAccount.Create(AName, APasswordHash: string;
  AAccessLevel: TAccessLevel);
begin
  inherited Create;
  FName := AName;
  FPasswordHash := APasswordHash;
  FAccessLevel := AAccessLevel;
  Flush;
end;

procedure TAccount.SetAccessLevel(const AValue: TAccessLevel);
begin
  FAccessLevel := AValue;
  Flush;
end;

procedure TAccount.SetPasswordHash(const AValue: string);
begin
  FPasswordHash := AValue;
  Flush;
end;

procedure TAccount.SetLastPos(const AValue: TPoint);
begin
  FLastPos.x := EnsureRange(AValue.x, 0, CEDServerInstance.Landscape.CellWidth - 1);
  FLastPos.y := EnsureRange(AValue.y, 0, CEDServerInstance.Landscape.CellHeight - 1);
  Flush;
end;

procedure TAccount.Flush;
begin
  Config.WriteString('Accounts', FName, IntToStr(Byte(FAccessLevel)) + ':' +
    FPasswordHash + ':' + IntToStr(FLastPos.x) + ':' + IntToStr(FLastPos.y));
end;

{ TAccountList }

constructor TAccountList.Create;
begin
  inherited Create(True);
end;

function TAccountList.IndexOf(AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := 0;
  while (i < Count) and (Result = -1) do
  begin
    if TAccount(Items[i]).Name = AName then
      Result := i;
    Inc(i);
  end;
end;

function TAccountList.Find(AName: string): TAccount;
var
  i: Integer;
begin
  i := IndexOf(AName);
  if i > -1 then
    Result := TAccount(Items[i])
  else
    Result := nil;
end;

procedure TAccountList.Delete(AName: string);
var
  i: Integer;
begin
  i := IndexOf(AName);
  if i > -1 then
    inherited Delete(i);
end;

end.

