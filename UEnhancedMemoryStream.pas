
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
unit UEnhancedMemoryStream;

{$mode delphi}{$H+}

interface

uses
  Classes, UStreamHelper;

type

  { TEnhancedMemoryStream }

  TEnhancedMemoryStream = class(TFifoStream)
  public
    function ReadBoolean: Boolean;
    function ReadByte: Byte;
    function ReadCardinal: Cardinal;
    function ReadInteger: Integer;
    function ReadInt64: Int64;
    function ReadShortInt: ShortInt;
    function ReadSmallInt: SmallInt;
    function ReadWord: Word;
    function ReadStringNull: string;
    function ReadStringFixed(ALength: Integer): string;
    function ReadStringBigUniNull: WideString;
    procedure WriteBoolean(AValue: Boolean);
    procedure WriteByte(AValue: Byte);
    procedure WriteCardinal(AValue: Cardinal);
    procedure WriteInteger(AValue: Integer);
    procedure WriteInt64(AValue: Int64);
    procedure WriteShortInt(AValue: ShortInt);
    procedure WriteSmallInt(AValue: SmallInt);
    procedure WriteWord(AValue: Word);
    procedure WriteStringNull(AValue: string);
    procedure WriteStringFixed(AValue: string; ALength: Integer);
    procedure WriteStringBigUniNull(AValue: WideString);
    procedure WriteStringLittleUniNull(AValue: WideString);
  end;

implementation

type
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt - 1] of Byte;

function SwapWideChar(Char: WideChar): WideChar;
begin
  Result := WideChar((Word(Char) shl 8) or ((Word(Char) shr 8) and $FF));
end;  

{ TEnhancedMemoryStream }

function TEnhancedMemoryStream.ReadBoolean: Boolean;
begin
  Read(Result, SizeOf(Boolean));
end;

function TEnhancedMemoryStream.ReadByte: Byte;
begin
  Read(Result, SizeOf(Byte));
end;

function TEnhancedMemoryStream.ReadCardinal: Cardinal;
begin
  Read(Result, SizeOf(Cardinal));
end;

function TEnhancedMemoryStream.ReadInt64: Int64;
begin
  Read(Result, SizeOf(Int64));
end;

function TEnhancedMemoryStream.ReadShortInt: ShortInt;
begin
  Read(Result, SizeOf(ShortInt));
end;

function TEnhancedMemoryStream.ReadInteger: Integer;
begin
  Read(Result, SizeOf(Integer));
end;

function TEnhancedMemoryStream.ReadSmallInt: SmallInt;
begin
  Read(Result, SizeOf(SmallInt));
end;

function TEnhancedMemoryStream.ReadStringBigUniNull: WideString;
var
  buffer: PWideChar;
  length: Integer;
begin
  Result := '';
  buffer := Pointer(PtrInt(Memory) + Position);
  length := 0;
  while (buffer[length] <> #0) and (length < (Size - Position)) do
  begin
    if (SwapWideChar(buffer[length]) = #10) and (SwapWideChar(buffer[length - 1]) <> #13) then
      Result := Result + #13;
    Result := Result + SwapWideChar(buffer[length]);
    if (SwapWideChar(buffer[length]) = #13) and (SwapWideChar(buffer[length + 1]) <> #10) then
      Result := Result + #10;
    inc(length);
  end;
  Position := Position + (Length + 1) * 2;
end;

function TEnhancedMemoryStream.ReadStringFixed(ALength: Integer): string;
var
  buffer: PChar;
  length: Integer;
begin
  Result := '';
  buffer := Pointer(PtrInt(FMemory) + FPosition);
  length := 0;
  while (length < ALength) and (length < (FSize - (FPosition - FLockOffset))) do
  begin
    if (buffer[length] = #10) and (buffer[length - 1] <> #13) then
      Result := Result + #13;
    Result := Result + buffer[length];
    if (buffer[length] = #13) and (buffer[length + 1] <> #10) then
      Result := Result + #10;
    inc(length);
  end;
  FPosition := FPosition + length + 1;
end;

function TEnhancedMemoryStream.ReadStringNull: string;
var
  buffer: PByteArray;
  length: Integer;
begin
  Result := '';
  buffer := Pointer(PtrInt(FMemory) + FPosition);
  length := 0;
  while (buffer^[length] <> 0) and (length < (FSize - (FPosition - FLockOffset))) do
  begin
    if (buffer^[length] = 10) and (buffer^[length - 1] <> 13) then
      Result := Result + #13;
    Result := Result + Char(buffer^[length]);
    if (buffer^[length] = 13) and (buffer^[length + 1] <> 10) then
      Result := Result + #10;
    inc(length);
  end;
  FPosition := FPosition + length + 1;
end;

function TEnhancedMemoryStream.ReadWord: Word;
begin
  Read(Result, SizeOf(Word));
end;

procedure TEnhancedMemoryStream.WriteBoolean(AValue: Boolean);
begin
  Write(AValue, SizeOf(Boolean));
end;

procedure TEnhancedMemoryStream.WriteByte(AValue: Byte);
begin
  Write(AValue, SizeOf(Byte));
end;

procedure TEnhancedMemoryStream.WriteCardinal(AValue: Cardinal);
begin
  Write(AValue, SizeOf(Cardinal));
end;

procedure TEnhancedMemoryStream.WriteInt64(AValue: Int64);
begin
  Write(AValue, SizeOf(Int64));
end;

procedure TEnhancedMemoryStream.WriteShortInt(AValue: ShortInt);
begin
  Write(AValue, SizeOf(ShortInt));
end;

procedure TEnhancedMemoryStream.WriteInteger(AValue: Integer);
begin
  Write(AValue, SizeOf(Integer));
end;

procedure TEnhancedMemoryStream.WriteSmallInt(AValue: SmallInt);
begin
  Write(AValue, SizeOf(SmallInt));
end;

procedure TEnhancedMemoryStream.WriteStringBigUniNull(AValue: WideString);
var
  i: Integer;
begin
  for i := 1 to Length(AValue) do
    WriteWord(Word(SwapWideChar(AValue[i])));
  WriteWord(0);
end;

procedure TEnhancedMemoryStream.WriteStringFixed(AValue: string;
  ALength: Integer);
var
  i: Integer;
begin
  for i := Length(AValue) to ALength do
    AValue := AValue + #0;
  Write(PChar(AValue)^, ALength);
end;

procedure TEnhancedMemoryStream.WriteStringLittleUniNull(
  AValue: WideString);
var
  i: Integer;
begin
  for i := 1 to Length(AValue) do
    WriteWord(Word(AValue[i]));
  WriteWord(0);
end;

procedure TEnhancedMemoryStream.WriteStringNull(AValue: string);
begin
  write(PChar(AValue)^, Length(AValue) + 1);
end;

procedure TEnhancedMemoryStream.WriteWord(AValue: Word);
begin
  Write(AValue, SizeOf(Word));
end;

end.
