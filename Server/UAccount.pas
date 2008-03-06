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
unit UAccount;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, md5, contnrs, math, DOM, UXmlHelper, UInterfaces,
  UEnums;
  
type

  { TAccount }

  TAccount = class(TObject, ISerializable, IInvalidate)
    constructor Create(AOwner: IInvalidate; AName, APasswordHash: string;
      AAccessLevel: TAccessLevel);
    constructor Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
    procedure Serialize(AElement: TDOMElement);
  protected
    FOwner: IInvalidate;
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
    procedure Invalidate;
  end;
  
  { TAccountList }

  TAccountList = class(TObjectList, ISerializable, IInvalidate)
    constructor Create(AOwner: IInvalidate); reintroduce;
    constructor Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
    procedure Serialize(AElement: TDOMElement);
  protected
    FOwner: IInvalidate;
  public
    function IndexOf(AName: string): Integer;
    function Find(AName: string): TAccount;
    procedure Delete(AName: string);
    procedure Invalidate;
  end;

implementation

uses
  UCEDServer, UConfig;

{ TAccount }

constructor TAccount.Create(AOwner: IInvalidate; AName, APasswordHash: string;
  AAccessLevel: TAccessLevel);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
  FPasswordHash := APasswordHash;
  FAccessLevel := AAccessLevel;
end;

constructor TAccount.Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
begin
  inherited Create;
  FOwner := AOwner;
  FName := TXmlHelper.ReadString(AElement, 'Name', '');
  FAccessLevel := TAccessLevel(TXmlHelper.ReadInteger(AElement, 'AccessLevel', 0));
  FPasswordHash := TXmlHelper.ReadString(AElement, 'PasswordHash', '');
  FLastPos := Point(0, 0);
  TXmlHelper.ReadCoords(AElement, 'LastPos', FLastPos.X, FLastPos.Y);
end;

procedure TAccount.SetAccessLevel(const AValue: TAccessLevel);
begin
  FAccessLevel := AValue;
  Invalidate;
end;

procedure TAccount.SetPasswordHash(const AValue: string);
begin
  FPasswordHash := AValue;
  Invalidate;
end;

procedure TAccount.SetLastPos(const AValue: TPoint);
begin
  FLastPos.x := EnsureRange(AValue.x, 0, CEDServerInstance.Landscape.CellWidth - 1);
  FLastPos.y := EnsureRange(AValue.y, 0, CEDServerInstance.Landscape.CellHeight - 1);
  Invalidate;
end;

procedure TAccount.Invalidate;
begin
  FOwner.Invalidate;
end;

procedure TAccount.Serialize(AElement: TDOMElement);
begin
  TXmlHelper.WriteString(AElement, 'Name', FName);
  TXmlHelper.WriteString(AElement, 'PasswordHash', FPasswordHash);
  TXmlHelper.WriteInteger(AElement, 'AccessLevel', Integer(FAccessLevel));
  TXmlHelper.WriteCoords(AElement, 'LastPos', FLastPos.X, FLastPos.Y);
end;

{ TAccountList }

constructor TAccountList.Create(AOwner: IInvalidate);
begin
  inherited Create(True);
  FOwner := AOwner;
end;

constructor TAccountList.Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
var
  nodelist: TDOMNodeList;
  i: Integer;
begin
  Create(AOwner);
  nodeList := AElement.GetChildNodes;
  for i := 0 to nodeList.Count - 1 do
  begin
    if nodeList.Item[i].NodeName = 'Account' then
      Add(TAccount.Deserialize(Self, TDOMElement(nodeList.Item[i])));
  end;
  nodeList.Free;
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

procedure TAccountList.Invalidate;
begin
  FOwner.Invalidate;
end;

procedure TAccountList.Serialize(AElement: TDOMElement);
var
  i: Integer;
  xmlAccount: TDOMElement;
begin
  for i := 0 to Count - 1 do
  begin
    xmlAccount := AElement.OwnerDocument.CreateElement('Account');
    AElement.AppendChild(xmlAccount);
    TAccount(Items[i]).Serialize(xmlAccount);
  end;
end;

end.

