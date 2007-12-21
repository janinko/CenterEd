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

{This unit contains the interface for objects which offer read/write access and
therefore streaming capability.

@author(Andreas Schneider <aksdb@gmx.de>)
@created(2007-07-08)
@lastmod(2007-11-14)}
unit UIStream;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes;
  
type
  {@abstract(The @name interface is used in objects which offer read/write access and
  therefore streaming capability.)}
  IStream = interface
    function ReadBoolean: Boolean;                                              //<Reads a @link(Boolean) at the current position. @returns(The @link(Boolean) at the current position.)
    function ReadByte: Byte;                                                    //<Reads a @link(Byte) at the current position. @returns(The @link(Byte) at the current position.)
    function ReadCardinal: Cardinal;                                            //<Reads a @link(Cardinal) at the current position. @returns(The @link(Cardinal) at the current position.)
    function ReadInteger: Integer;                                              //<Reads a @link(Integer) at the current position. @returns(The @link(Integer) at the current position.)
    function ReadInt64: Int64;                                                  //<Reads a @link(Int64) at the current position. @returns(The @link(Int64) at the current position.)
    function ReadSmallInt: SmallInt;                                            //<Reads a @link(SmallInt) at the current position. @returns(The @link(SmallInt) at the current position.)
    function ReadWord: Word;                                                    //<Reads a @link(Word) at the current position. @returns(The @link(Word) at the current position.)
    function ReadString: string;                                                //<Reads a @link(String) at the current position, by first querying the size (read as @link(Integer)). @returns(The @link(String) at the current position.)
    function ReadStringFixed(ALength: Integer): string;                         //<Reads a @link(String) at the current position with the given length. @param(ALength The length of the @link(String) to be read.) @returns(The @link(String) at the current position.)
    procedure WriteBoolean(AValue: Boolean);                                    //<Writes a @link(Boolean) to the current position. @param(AValue The @link(Boolean) value to be written.)
    procedure WriteByte(AValue: Byte);                                          //<Writes a @link(Byte) to the current position. @param(AValue The @link(Byte) value to be written.)
    procedure WriteCardinal(AValue: Cardinal);                                  //<Writes a @link(Cardinal) to the current position. @param(AValue The @link(Cardinal) value to be written.)
    procedure WriteInteger(AValue: Integer);                                    //<Writes a @link(Integer) to the current position. @param(AValue The @link(Integer) value to be written.)
    procedure WriteInt64(AValue: Int64);                                        //<Writes a @link(Int64) to the current position. @param(AValue The @link(Int64) value to be written.)
    procedure WriteSmallInt(AValue: SmallInt);                                  //<Writes a @link(SmallInt) to the current position. @param(AValue The @link(SmallInt) value to be written.)
    procedure WriteWord(AValue: Word);                                          //<Writes a @link(Word) to the current position. @param(AValue The @link(Word) value to be written.)
    procedure WriteString(AValue: string);                                      //<Writes a @link(String) to the current position, preceeded by the length as @link(Integer). @param(AValue The @link(String) value to be written.)
    procedure WriteStringFixed(AValue: string; ALength: Integer);               //<Writes a @link(String) with the given length to the current position. @param(AValue The @link(String) value to be written.) @param(ALength The length of the @link(String).)

    function Read(ABuffer: PByte; ACount: Cardinal): Cardinal;                  //<Reads a given number of bytes from the stream. @param(ABuffer A Pointer to the memory to write to.) @param(ACount The number of bytes to read.) @returns(The number of bytes actually read.)
    function Write(ABuffer: PByte; ACount: Cardinal): Cardinal;                 //<Writes a given buffer to the stream. @param(ABuffer A Pointer to the memory to write to the stream.) @param(ACount The number of bytes to write.) @returns(The number of bytes actually written.)
    
    procedure Skip(ACount: Cardinal);                                           //<Skips a certain number of bytes from the current position. @param(ACount The number of bytes to skip.)
  end;

implementation

end.

