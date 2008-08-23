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
unit UXmlHelper;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, dom;
  
type

  { TXmlHelper }

  TXmlHelper = class(TObject)
    class function FindChild(AParent: TDOMElement; AName: string): TDOMElement;
    class function AssureElement(AParent: TDOMElement; AName: string): TDOMElement;
    class procedure WriteString(AParent: TDOMElement; AName, AValue: string);
    class function ReadString(AParent: TDOMElement; AName, ADefault: string): string;
    class procedure WriteInteger(AParent: TDOMElement; AName: string; AValue: Integer);
    class function ReadInteger(AParent: TDOMElement; AName: string; ADefault: Integer): Integer;
    class procedure WriteBoolean(AParent: TDOMElement; AName: string; AValue: Boolean);
    class function ReadBoolean(AParent: TDOMElement; AName: string; ADefault: Boolean): Boolean;
    class procedure WriteCoords(AParent: TDOMElement; AName: string; AX, AY: Integer);
    class function ReadCoords(AParent: TDOMElement; AName: string; out X, Y: Integer): Boolean;
  end;

implementation

{ TXmlHelper }

class function TXmlHelper.FindChild(AParent: TDOMElement; AName: string): TDOMElement;
var
  i: LongWord;
  nodeList: TDOMNodeList;
begin
  Result := nil;
  nodeList := AParent.GetChildNodes;
  i := 0;
  while (Result = nil) and (i < nodeList.Count) do
  begin
    if nodeList.Item[i].NodeName = AName then
      Result := TDOMElement(nodeList[i]);
    inc(i);
  end;
  nodeList.Free;
end;

class function TXmlHelper.AssureElement(AParent: TDOMElement; AName: string): TDOMElement;
begin
  Result := FindChild(AParent, AName);
  if Result = nil then
  begin
    Result := AParent.OwnerDocument.CreateElement(AName);
    AParent.AppendChild(Result);
  end;
end;

class procedure TXmlHelper.WriteString(AParent: TDOMElement; AName, AValue: string);
var
  element: TDOMElement;
begin
  element := AssureElement(AParent, AName);
  if assigned(element.FirstChild) then
    TDOMText(element.FirstChild).NodeValue := AValue
  else
    element.AppendChild(AParent.OwnerDocument.CreateTextNode(AValue));
end;

class function TXmlHelper.ReadString(AParent: TDOMElement; AName, ADefault: string): string;
var
  element: TDOMElement;
begin
  element := FindChild(AParent, AName);
  if assigned(element) and assigned(element.FirstChild) then
    Result := TDOMText(element.FirstChild).Data
  else
    Result := ADefault;
end;

class procedure TXmlHelper.WriteInteger(AParent: TDOMElement; AName: string;
  AValue: Integer);
begin
  WriteString(AParent, AName, IntToStr(AValue));
end;

class function TXmlHelper.ReadInteger(AParent: TDOMElement; AName: string;
  ADefault: Integer): Integer;
begin
  if not TryStrToInt(ReadString(AParent, AName, ''), Result) then
    Result := ADefault;
end;

class procedure TXmlHelper.WriteBoolean(AParent: TDOMElement; AName: string;
  AValue: Boolean);
begin
  WriteString(AParent, AName, BoolToStr(AValue));
end;

class function TXmlHelper.ReadBoolean(AParent: TDOMElement; AName: string;
  ADefault: Boolean): Boolean;
begin
  Result := StrToBool(ReadString(AParent, AName, BoolToStr(ADefault)));
end;

class procedure TXmlHelper.WriteCoords(AParent: TDOMElement; AName: string; AX,
  AY: Integer);
var
  element: TDOMElement;
begin
  element := AssureElement(AParent, AName);
  element.AttribStrings['x'] := IntToStr(AX);
  element.AttribStrings['y'] := IntToStr(AY);
end;

class function TXmlHelper.ReadCoords(AParent: TDOMElement; AName: string; out
  X, Y: Integer): Boolean;
var
  element: TDOMElement;
  tempX, tempY: Integer;
begin
  element := FindChild(AParent, AName);
  Result := assigned(element) and TryStrToInt(element.AttribStrings['x'], tempX)
    and TryStrToInt(element.AttribStrings['y'], tempY);

  if Result then
  begin
    X := tempX;
    Y := tempY;
  end;
end;

end.

