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
unit URegions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, DOM, UXmlHelper, UInterfaces, UEnums, URectList;

type

  { TRegion }

  TRegion = class(TObject, ISerializable, IInvalidate)
    constructor Create(AOwner: IInvalidate; AName: string);
    constructor Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
    destructor Destroy; override;
    procedure Serialize(AElement: TDOMElement);
  protected
    FOwner: IInvalidate;
    FName: string;
    FAreas: TRectList;
    procedure SetName(const AValue: string);
  public
    property Name: string read FName write SetName;
    property Areas: TRectList read FAreas write FAreas;
    procedure Invalidate;
  end;

  { TRegionList }

  TRegionList = class(TObjectList, ISerializable, IInvalidate)
    constructor Create(AOwner: IInvalidate); reintroduce;
    constructor Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
    procedure Serialize(AElement: TDOMElement);
  protected
    FOwner: IInvalidate;
  public
    function IndexOf(AName: string): Integer;
    function Find(AName: string): TRegion;
    procedure Delete(AName: string); overload;
    procedure Invalidate;
  end;
  
implementation

uses
  UCEDServer, UConfig;

{ TRegion }

constructor TRegion.Create(AOwner: IInvalidate; AName: string);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
  FAreas := TRectList.Create;
end;

constructor TRegion.Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
var
  nodelist: TDOMNodeList;
  i, x1, y1, x2, y2: Integer;
  xmlElement, xmlArea: TDOMElement;
begin
  inherited Create;
  FOwner := AOwner;
  FName := TXmlHelper.ReadString(AElement, 'Name', 'Unnamed');
  FAreas := TRectList.Create;
  xmlElement := TDOMElement(AElement.FindNode('Area'));
  if xmlElement <> nil then
  begin
    nodeList := xmlElement.GetChildNodes;
    for i := 0 to nodeList.Count - 1 do
    begin
      if nodeList.Item[i].NodeName = 'Rect' then
      begin
        xmlArea := TDOMElement(nodeList.Item[i]);
        if TryStrToInt(xmlArea.AttribStrings['x1'], x1) and
          TryStrToInt(xmlArea.AttribStrings['y1'], y1) and
          TryStrToInt(xmlArea.AttribStrings['x2'], x2) and
          TryStrToInt(xmlArea.AttribStrings['y2'], y2) then
        begin
          FAreas.Add(x1, y1, x2, y2);
        end else
          raise TInvalidConfigException.Create('Invalid area.');
      end;
    end;
    nodeList.Free;
  end;
end;

destructor TRegion.Destroy;
begin
  if FAreas <> nil then FreeAndNil(FAreas);
  inherited Destroy;
end;

procedure TRegion.SetName(const AValue: string);
begin
  FName := AValue;
  Invalidate;
end;

procedure TRegion.Invalidate;
begin
  FOwner.Invalidate;
end;

procedure TRegion.Serialize(AElement: TDOMElement);
var
  i : Integer;
  child, area: TDOMElement;
begin
  TXmlHelper.WriteString(AElement, 'Name', FName);
  child := TXmlHelper.AssureElement(AElement, 'Area');
  for i := 0 to FAreas.Count -1 do
  begin
    area := child.OwnerDocument.CreateElement('Rect');
    child.AppendChild(area);
    area.AttribStrings['x1'] := IntToStr(FAreas.Rects[i].Left);
    area.AttribStrings['y1'] := IntToStr(FAreas.Rects[i].Top);
    area.AttribStrings['x2'] := IntToStr(FAreas.Rects[i].Right);
    area.AttribStrings['y2'] := IntToStr(FAreas.Rects[i].Bottom);
  end;
end;

{ TRegionList }

constructor TRegionList.Create(AOwner: IInvalidate);
begin
  inherited Create(True);
  FOwner := AOwner;
end;

constructor TRegionList.Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
var
  nodelist: TDOMNodeList;
  i: Integer;
begin
  Create(AOwner);
  nodeList := AElement.GetChildNodes;
  for i := 0 to nodeList.Count - 1 do
  begin
    if nodeList.Item[i].NodeName = 'Region' then
      Add(TRegion.Deserialize(Self, TDOMElement(nodeList.Item[i])));
  end;
  nodeList.Free;
end;

function TRegionList.IndexOf(AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := 0;
  while (i < Count) and (Result = -1) do
  begin
    if TRegion(Items[i]).Name = AName then
      Result := i;
    Inc(i);
  end;
end;

function TRegionList.Find(AName: string): TRegion;
var
  i: Integer;
begin
  i := IndexOf(AName);
  if i > -1 then
    Result := TRegion(Items[i])
  else
    Result := nil;
end;

procedure TRegionList.Delete(AName: string);
var
  i: Integer;
begin
  i := IndexOf(AName);
  if i > -1 then
    inherited Delete(i);
end;

procedure TRegionList.Invalidate;
begin
  FOwner.Invalidate;
end;

procedure TRegionList.Serialize(AElement: TDOMElement);
var
  i: Integer;
  xmlRegion: TDOMElement;
begin
  for i := 0 to Count - 1 do
  begin
    xmlRegion := AElement.OwnerDocument.CreateElement('Region');
    AElement.AppendChild(xmlRegion);
    TRegion(Items[i]).Serialize(xmlRegion);
  end;
end;

end.

