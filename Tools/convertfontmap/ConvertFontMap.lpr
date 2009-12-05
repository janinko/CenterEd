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
program ConvertFontMap;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, DOM, XMLRead;

{$IFDEF WINDOWS}{$R ConvertFontMap.rc}{$ENDIF}

type
  TFontInfo = packed record
    Character: char;
    LeftOffset: SmallInt;
    CharWidth: Word;
    Width: Word;
    Height: Word;
    X1: Single;
    Y1: Single;
    X2: Single;
    Y2: Single;
  end;

var
  xmlDoc: TXMLDocument;
  chars: TDOMNodeList;
  root, parent, charNode: TDOMElement;
  outFile: TFileStream;
  spaceWidth: Word;
  fontInfo: TFontInfo;
  i: Integer;

begin
  if ParamCount = 2 then
  begin
    ReadXMLFile(xmlDoc, ParamStr(1));
    outFile := TFileStream.Create(ParamStr(2), fmCreate);

    root := xmlDoc.DocumentElement;
    parent := TDOMElement(root.FindNode('characters'));
    chars := parent.ChildNodes;

    spaceWidth := StrToInt(root.AttribStrings['spacewidth']);
    outFile.Write(spaceWidth, SizeOf(spaceWidth));

    for i := 0 to chars.Count - 1 do
    begin
      charNode := TDOMElement(chars[i]);
      fontInfo.Character := Char(StrToInt(charNode.AttribStrings['char']));
      fontInfo.LeftOffset := StrToInt(charNode.AttribStrings['A']);
      fontInfo.CharWidth := StrToInt(charNode.AttribStrings['C']);
      fontInfo.Width := StrToInt(charNode.AttribStrings['wid']);
      fontInfo.Height := StrToInt(charNode.AttribStrings['hgt']);
      fontInfo.X1 := StrToFloat(charNode.AttribStrings['X1']);
      fontInfo.Y1 := StrToFloat(charNode.AttribStrings['Y1']);
      fontInfo.X2 := StrToFloat(charNode.AttribStrings['X2']);
      fontInfo.Y2 := StrToFloat(charNode.AttribStrings['Y2']);
      outFile.Write(fontInfo, SizeOf(fontInfo));
    end;

    outFile.Free;
    xmlDoc.Free;

  end else
    Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' <In Font XML> <Out Font Bin>');
end.

