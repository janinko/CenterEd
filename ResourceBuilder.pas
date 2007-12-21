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
program ResourceBuilder;

{$mode objfpc}{$H+}

uses
	SysUtils, Classes;

var
	fileList: TStringList;
	infile, outfile: TFileStream;
	i, count: Integer;
	size: Cardinal;
	lookupTable: array of Cardinal;
	
begin
	if ParamCount <> 2 then
	begin
	  writeln('Usage: ResourceBuilder <FileList> <ResourceFile>');
	  halt;
	end;

	fileList := TStringList.Create;
	fileList.LoadFromFile(ParamStr(1));
	outfile := TFileStream.Create(ParamStr(2), fmCreate);
	count := fileList.Count;
	outfile.Write(count, SizeOf(Integer));
	SetLength(lookupTable, count);
	outfile.Write(lookupTable[0], count * SizeOf(Cardinal));
	for i := 0 to count - 1 do
	begin
	  lookupTable[i] := outfile.Position;
	  writeln(i, ': ', fileList.Strings[i]);
	  infile := TFileStream.Create(fileList.Strings[i], fmOpenRead);
	  infile.Position := 0;
	  size := infile.Size;
	  outfile.Write(size, SizeOf(Cardinal));
	  outfile.CopyFrom(infile, infile.Size);
	  infile.Free;
	end;
	outfile.Position := SizeOf(Integer);
	outfile.Write(lookupTable[0], count * SizeOf(Cardinal));
	outfile.Free;
	fileList.Free;
end.
