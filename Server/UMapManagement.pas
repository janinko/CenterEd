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
  *      Portions Copyright 2013 Andreas Schneider
  *)
unit UMapManagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UPacket, UInterfaces, UXmlHelper, UEnhancedMemoryStream,
  DOM, fgl;

type

  { TMapState }

  TMapState = class(TObject, IInvalidate, ISerializable)
  private
    FOwner: IInvalidate;
    FName: String;
    FPath: String;
    FDescription: String;
    procedure WriteToStream(AStream: TEnhancedMemoryStream);
  public
    constructor Create(AOwner: IInvalidate);
    constructor Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
    procedure Invalidate;
    procedure Serialize(AElement: TDOMElement);
  end;

  { TMapStates }

  TMapStates = class(specialize TFPGObjectList<TMapState>, IInvalidate,
    ISerializable)
  private
    FOwner: IInvalidate;
  public
    constructor Create(AOwner: IInvalidate);
    constructor Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
    procedure Invalidate;
    procedure Serialize(AElement: TDOMElement);

    function Add(const Item: TMapState): Integer;
    function Remove(const Item: TMapState): Integer;
  end;

  //Network

  { TMapStateListPacket }

  TMapStateListPacket = class(TPacket)
    constructor Create(AMapStates: TMapStates);
  end;

implementation

{ TMapState }

procedure TMapState.WriteToStream(AStream: TEnhancedMemoryStream);
begin
  AStream.WriteStringNull(FName);
  AStream.WriteStringNull(FPath);
  AStream.WriteStringNull(FDescription);
end;

constructor TMapState.Create(AOwner: IInvalidate);
begin
  inherited Create;
  FOwner := AOwner;
end;

constructor TMapState.Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
begin
  inherited Create;
  FOwner := AOwner;
  FName := TXmlHelper.ReadString(AElement, 'Name', '');
  FPath := TXmlHelper.ReadString(AElement, 'Path', '');
  FDescription := TXmlHelper.ReadString(AElement, 'Description', '');
end;

procedure TMapState.Invalidate;
begin
  FOwner.Invalidate;
end;

procedure TMapState.Serialize(AElement: TDOMElement);
begin
  TXmlHelper.WriteString(AElement, 'Name', FName);
  TXmlHelper.WriteString(AElement, 'Path', FPath);
  TXmlHelper.WriteString(AElement, 'Description', FDescription);
end;

{ TMapStates }

constructor TMapStates.Create(AOwner: IInvalidate);
begin
  inherited Create(True);
  FOwner := AOwner;
end;

constructor TMapStates.Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
var
  nodeList: TDOMNodeList;
  node: TDOMNode;
begin
  inherited Create(True);
  FOwner := AOwner;
  for node in AElement.ChildNodes do
  begin
    if node.NodeName = 'MapState' then
      Add(TMapState.Deserialize(Self, TDOMElement(node)));
  end;
end;

procedure TMapStates.Invalidate;
begin
  FOwner.Invalidate;
end;

procedure TMapStates.Serialize(AElement: TDOMElement);
var
  mapState: TMapState;
  childElement: TDOMElement;
begin
  for mapState in Self do
  begin
    childElement := AElement.OwnerDocument.CreateElement('MapState');
    AElement.AppendChild(childElement);
    mapState.Serialize(childElement);
  end;
end;

function TMapStates.Add(const Item: TMapState): Integer;
begin
  Result := inherited Add(Item);
  Invalidate;
end;

function TMapStates.Remove(const Item: TMapState): Integer;
begin
  Result := inherited Remove(Item);
  Invalidate;
end;

{ TMapStateListPacket }

constructor TMapStateListPacket.Create(AMapStates: TMapStates);
begin
  inherited Create($0F, 0);
  FStream.WriteByte($01);
end;

end.

