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
unit ULocalization;

interface

uses
  Classes;

type
  TLocalizationEntry = class(TObject)
    constructor Create;
    constructor Deserialize(Data: TStream);
    procedure Serialize(Data: TStream);
    function GetSize: Integer;
  private
    FNumber: Integer;
    FUnknown: Byte;
    FText: string;
  published
    property Number: Integer read FNumber write FNumber;
    property Unknown: Byte read FUnknown write FUnknown;
    property Text: string read FText write FText;
  end;

implementation

constructor TLocalizationEntry.Create;
begin
  FNumber := 0;
  FUnknown := 0;
  FText := '';
end;

constructor TLocalizationEntry.Deserialize(Data: TStream);
var
  length: SmallInt;
begin
  if assigned(Data) then
  begin
    Data.Read(FNumber, SizeOf(Integer));
    Data.Read(FUnknown, SizeOf(Byte));
    Data.Read(length, SizeOf(SmallInt));
    SetLength(FText, length);
    Data.Read(PChar(FText)^, length);
    FText := UTF8Decode(FText);
  end;
end;

procedure TLocalizationEntry.Serialize(Data: TStream);
var
  iLength: SmallInt;
  text: string;
begin
  Data.Write(FNumber, SizeOf(Integer));
  Data.Write(FUnknown, SizeOf(Byte));
  text := UTF8Encode(FText);
  iLength := Length(text);
  Data.Write(iLength, SizeOf(SmallInt));
  Data.Write(PChar(text)^, iLength);
end;

function TLocalizationEntry.GetSize: Integer;
begin
  Result := SizeOf(Integer) + SizeOf(Byte) + SizeOf(SmallInt) + Length(FText);
end;

end.

