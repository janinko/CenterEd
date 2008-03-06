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

{@abstract(This unit contains procedures and classes to help with stream handling.
It can be used to ease copying of streams and to assist in writing and reading
specialized types to/from streams.)

@bold(Warning!!!)@br
Due to a problem with generics in FPC 2.2.0 I introduced @link(TStreamType) as
workaround to reference the actual type of stream used inside the
@link(TStreamWrapper).

@author(Andreas Schneider <aksdb@gmx.de>)
@created(2007-07-08)
@lastmod(2007-11-14)}
unit UStreamHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, RtlConsts, SysUtils;

type
  {@name is the stub for the method which will handle the OnProgress callbacks.
  @param(ATotal Specifies the complete size of the operation.)
  @param(ACurrent Specifies the current position during the operation.)}
  TOnProgressEvent = procedure(ATotal, ACurrent: Cardinal) of object;
  
  { TFifoStream }
  
  {@abstract(The @name contains special handling for queuing and dequeing. It is
  meant to be used as a network queue.)}
  TFifoStream = class(TStream)
    destructor Destroy; override;
  protected
    FMemory: Pointer;
    FSize, FRealSize, FPosition, FLockOffset: Longint;
    FCapacity: Longint;
    procedure SetCapacity(ANewCapacity: Longint);
    procedure SetPointer(APtr: Pointer; ASize: Longint);
    function GetOptimalCapacity(ANewCapacity: Longint): Longint;
    function Realloc(var NewCapacity: Longint): Pointer; virtual;
    property Capacity: Longint read FCapacity write SetCapacity;
  public
    function GetSize: Int64; override;
    function Read(var Buffer; ACount: Longint): Longint; override;
    function Write(const Buffer; ACount: Longint): Longint; override;
    function Seek(AOffset: Longint; AOrigin: Word): Longint; override;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure SetSize(ANewSize: Longint); override;
    procedure Clear;
    procedure Dequeue(ACount: Longint);                                         //<Removes a specified number of bytes from the queue. @param(ACount The number of bytes to remove from the queue.)
    procedure Enqueue(const Buffer; ACount: Longint);                           //<Adds a specified number of bytes from a given buffer to the end of the queue. @param(Buffer The buffer containing the data to enqueue.) @param(ACount The number of bytes to enqueue from the buffer.)
    procedure Lock(AOffset, ASize: Longint);                                    //<Restricts the visible area of the stream. @param(AOffset is the starting position of the area.) @param(ASize The size of the area.)
    procedure Unlock;                                                           //<Removes the restrictions from the stream and resets the visible area to the original size.
    property Memory: Pointer read FMemory;
  end;
  

  { TStreamWrapper }

  {The @name is just a placeholder for the type used in the
  @link(TStreamWrapper). It is currently in place to work around a problem with
  generics in fpc 2.2.0}
  TStreamType = TFifoStream;
  //generic TStreamWrapper<TStreamType> = class(TObject{, IStream})
  {@abstract(@name implements @link(IStream) and offers a bunch of functions to
  ease reading and writing special types (like @link(Integer)s or @link(String)s.))}
  TStreamWrapper = class(TObject)
    constructor Create(AStream: TStreamType; AOwnsStream: Boolean = True);      //<Creates a new instance of @classname. @param(AStream The underlying stream to perform the actual operations on.) @param(AOwnsStream Defines wheather to free the stream on destruction of @classname or not. Defaults to @false.)
    destructor Destroy; override;                                               //<Is called when the current instance of @classname is destroyed. If it owns the underlying stream it is destroyed aswell.
  protected
    FStream: TStream;
    FOwnsStream: Boolean;
    function GetStream: TStreamType;
    procedure SetStream(AStream: TStreamType);
  public
    property Raw: TStreamType read GetStream write SetStream;                   //<Provides raw access to the underlying stream. Useful for manipulation of the stream position and other class specific calls.
    
    function ReadBoolean: Boolean;                                              //<Implementation of @link(IStream.ReadBoolean).
    function ReadByte: Byte;                                                    //<Implementation of @link(IStream.ReadByte).
    function ReadCardinal: Cardinal;                                            //<Implementation of @link(IStream.ReadCardinal).
    function ReadInteger: Integer;                                              //<Implementation of @link(IStream.ReadInteger).
    function ReadInt64: Int64;                                                  //<Implementation of @link(IStream.ReadInt64).
    function ReadSmallInt: SmallInt;                                            //<Implementation of @link(IStream.ReadSmallInt).
    function ReadWord: Word;                                                    //<Implementation of @link(IStream.ReadWord).
    function ReadString: string;                                                //<Implementation of @link(IStream.ReadString).
    function ReadStringFixed(ALength: Integer): string;                         //<Implementation of @link(IStream.ReadStringFixed).
    procedure WriteBoolean(AValue: Boolean);                                    //<Implementation of @link(IStream.WriteBoolean).
    procedure WriteByte(AValue: Byte);                                          //<Implementation of @link(IStream.WriteByte).
    procedure WriteCardinal(AValue: Cardinal);                                  //<Implementation of @link(IStream.WriteCardinal).
    procedure WriteInteger(AValue: Integer);                                    //<Implementation of @link(IStream.WriteInteger).
    procedure WriteInt64(AValue: Int64);                                        //<Implementation of @link(IStream.WriteInt64).
    procedure WriteSmallInt(AValue: SmallInt);                                  //<Implementation of @link(IStream.WriteSmallInt).
    procedure WriteWord(AValue: Word);                                          //<Implementation of @link(IStream.WriteWord).
    procedure WriteString(AValue: string);                                      //<Implementation of @link(IStream.WriteString).
    procedure WriteStringFixed(AValue: string; ALength: Integer);               //<Implementation of @link(IStream.WriteStringFixed).

    function Read(ABuffer: PByte; ACount: Cardinal): Cardinal;                  //<Implementation of @link(IStream.Read).
    function Write(ABuffer: PByte; ACount: Cardinal): Cardinal;                 //<Implementation of @link(IStream.Write).
    
    procedure Skip(ACount: Cardinal);                                           //<Implementation of @link(IStream.Skip).
  end;

{@name is used to have a progress (see @link(TOnProgressEvent)) for a copy
action of the content of one stream into another. This is especially useful
for writing and reading to @link(TFileStream).
@param(ASource The stream from which the content is copied.)
@param(ATarget The stream to which the content is copied.)
@param(ACount Specifies the amount to copy. 0 means, that the whole stream is processed.)
@param(AOnProgress The callback for the @link(TOnProgressEvent). Defaults to @nil.)
@returns(The amount of bytes copied.)}
function StreamCopy(ASource, ATarget: TStream; ACount: Int64; AOnProgress: TOnProgressEvent = nil): Int64;

implementation

function StreamCopy(ASource, ATarget: TStream; ACount: Int64; AOnProgress: TOnProgressEvent = nil): Int64;
var
  i, targetSize: Int64;
  buffer: array[0..4095] of byte;
begin
  Result := 0;
  if (ACount = 0) then
  begin
    //This WILL fail for non-seekable streams...
    ASource.Position := 0;
    ACount := ASource.Size;
  end;
  targetSize := ACount;
  while ACount > 0 do
  begin
    if (ACount > SizeOf(buffer)) then
      i := SizeOf(Buffer)
    else
      i := ACount;
    i := ASource.Read(buffer, i);
    i := ATarget.Write(buffer, i);
    if i = 0 then break;
    Dec(ACount, i);
    Inc(Result, i);
    if Assigned(AOnProgress) then
      AOnProgress(targetSize, Result);
  end;
end;

{ TFifoStream }

const TMSGrow = 4096; { Use 4k blocks. }

destructor TFifoStream.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TFifoStream.SetCapacity(ANewCapacity: Longint);
begin
  SetPointer(Realloc(ANewCapacity), FSize);
  FCapacity := ANewCapacity;
end;

procedure TFifoStream.SetPointer(APtr: Pointer; ASize: Longint);
begin
  FMemory := APtr;
  FSize := ASize;
end;

function TFifoStream.GetOptimalCapacity(ANewCapacity: Longint): Longint;
begin
  Result := ANewCapacity;
  if Result <= 0 then
    Result := 0
  else
  begin
    // if growing, grow at least a quarter
    if (Result > FCapacity) and (Result < (5 * FCapacity) div 4) then
      Result := (5 * FCapacity) div 4;
    // round off to block size.
    Result := (Result + (TMSGrow-1)) and not (TMSGROW-1);
  end;
end;

function TFifoStream.Realloc(var NewCapacity: Longint): Pointer;
begin
  NewCapacity := GetOptimalCapacity(NewCapacity);
  // Only now check !
  if NewCapacity = FCapacity then
    Result := FMemory
  else
  begin
    Result := ReAllocMem(FMemory, NewCapacity);
    if (Result = nil) and (NewCapacity > 0) then
      raise EStreamError.Create(SMemoryStreamError);
  end;
end;

function TFifoStream.GetSize: Int64;
begin
  Result := FSize;
end;

function TFifoStream.Read(var Buffer; ACount: Longint): Longint;
begin
  Result := 0;
  If (FSize > 0) and (FPosition - FLockOffset < FSize) then
  begin
    Result := FSize - (FPosition - FLockOffset);
    if Result > ACount then Result := ACount;
    Move((FMemory + FPosition)^, Buffer, Result);
    FPosition := FPosition + Result;
  end;
end;

function TFifoStream.Write(const Buffer; ACount: Longint): Longint;
var
  NewPos: Longint;
begin
  Unlock;
  if ACount = 0 then
    Exit(0);
  NewPos := FPosition + ACount;
  if NewPos > FSize then
  begin
    if NewPos > FCapacity then
      SetCapacity(NewPos);
    FSize := NewPos;
  end;
  System.Move(Buffer, (FMemory + FPosition)^, ACount);
  FPosition := NewPos;
  Result := ACount;
end;

function TFifoStream.Seek(AOffset: Longint; AOrigin: Word): Longint;
begin
  case AOrigin of
    soFromBeginning : FPosition := AOffset + FLockOffset;
    soFromEnd       : FPosition := FSize + AOffset + FLockOffset;
    soFromCurrent   : FPosition := FPosition + AOffset;
  end;
  Result := FPosition - FLockOffset;
end;

procedure TFifoStream.LoadFromStream(AStream: TStream);
begin
  Unlock;
  AStream.Position := 0;
  SetSize(AStream.Size);
  If FSize > 0 then AStream.ReadBuffer(FMemory^,FSize);
end;

procedure TFifoStream.SaveToStream(AStream: TStream);
begin
  if FSize > 0 then AStream.WriteBuffer((FMemory + FLockOffset)^, FSize);
end;

procedure TFifoStream.LoadFromFile(const FileName: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.free;
  end;
end;

procedure TFifoStream.SaveToFile(const FileName: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.free;
  end;
end;

procedure TFifoStream.SetSize(ANewSize: Longint);
begin
  Unlock;
  SetCapacity(ANewSize);
  FSize := ANewSize;
  if FPosition > FSize then
    FPosition := FSize;
end;

procedure TFifoStream.Clear;
begin
  FSize := 0;
  FRealSize := 0;
  FPosition := 0;
  FLockOffset := 0;
  SetCapacity(0);
end;

procedure TFifoStream.Dequeue(ACount: Longint);
var
  newCapacity, newSize: Longint;
  queue, newMemory: Pointer;
begin
  Unlock;
  if ACount >= FSize then
  begin
    Size := 0;
    Exit;
  end;
  
  queue := FMemory + ACount;
  newSize := FSize - ACount;
  
  newCapacity := GetOptimalCapacity(newSize);
  if newCapacity <> FCapacity then
  begin
    newMemory := GetMem(newCapacity);
    System.Move(queue^, newMemory^, newSize);
    if (newMemory = nil) and (newCapacity > 0) then
      raise EStreamError.Create(SMemoryStreamError);
    FreeMem(FMemory);
    FMemory := newMemory;
    FCapacity := newCapacity;
  end else
    System.Move(queue^, FMemory^, newSize);

  FSize := newSize;
  if FPosition > ACount then
    Dec(FPosition, ACount)
  else
    FPosition := 0;
end;

procedure TFifoStream.Enqueue(const Buffer; ACount: Longint);
var
  oldPos: Int64;
begin
  Unlock;
  oldPos := FPosition;
  FPosition := FSize;
  Write(Buffer, ACount);
  FPosition := oldPos;
end;

procedure TFifoStream.Lock(AOffset, ASize: Longint);
begin
  if (FLockOffset <> 0) or (FRealSize <> 0) then Exit;
  FLockOffset := AOffset;
  FRealSize := FSize;
  FSize := ASize;
end;

procedure TFifoStream.Unlock;
begin
  if (FLockOffset = 0) and (FRealSize = 0) then Exit;
  FLockOffset := 0;
  FSize := FRealSize;
  FRealSize := 0;
end;

{ TStreamWrapper }

constructor TStreamWrapper.Create(AStream: TStreamType; AOwnsStream: Boolean);
begin
  inherited Create;
  FStream := TStream(AStream);
  FOwnsStream := AOwnsStream;
end;

destructor TStreamWrapper.Destroy;
begin
  if FOwnsStream and Assigned(FStream) then FreeAndNil(FStream);
  inherited Destroy;
end;

function TStreamWrapper.GetStream: TStreamType;
begin
  Result := TStreamType(FStream);
end;

procedure TStreamWrapper.SetStream(AStream: TStreamType);
begin
  FStream := TStream(AStream);
end;

function TStreamWrapper.ReadBoolean: Boolean;
begin
  if not Assigned(FStream) then Exit(False);
  FStream.Read(Result, SizeOf(Boolean));
end;

function TStreamWrapper.ReadByte: Byte;
begin
  if not Assigned(FStream) then Exit(0);
  FStream.Read(Result, SizeOf(Byte));
end;

function TStreamWrapper.ReadCardinal: Cardinal;
begin
  if not Assigned(FStream) then Exit(0);
  FStream.Read(Result, SizeOf(Cardinal));
end;

function TStreamWrapper.ReadInteger: Integer;
begin
  if not Assigned(FStream) then Exit(0);
  FStream.Read(Result, SizeOf(Integer));
end;

function TStreamWrapper.ReadInt64: Int64;
begin
  if not Assigned(FStream) then Exit(0);
  FStream.Read(Result, SizeOf(Int64));
end;

function TStreamWrapper.ReadSmallInt: SmallInt;
begin
  if not Assigned(FStream) then Exit(0);
  FStream.Read(Result, SizeOf(SmallInt));
end;

function TStreamWrapper.ReadWord: Word;
begin
  if not Assigned(FStream) then Exit(0);
  FStream.Read(Result, SizeOf(Word));
end;

function TStreamWrapper.ReadString: string;
begin
  if not Assigned(FStream) then Exit('');
  Result := ReadStringFixed(ReadInteger);
end;

function TStreamWrapper.ReadStringFixed(ALength: Integer): string;
begin
  if not Assigned(FStream) then Exit('');
  SetLength(Result, ALength);
  FStream.Read(PChar(Result)^, ALength);
end;

procedure TStreamWrapper.WriteBoolean(AValue: Boolean);
begin
  if not Assigned(FStream) then Exit;
  FStream.Write(AValue, SizeOf(Boolean));
end;

procedure TStreamWrapper.WriteByte(AValue: Byte);
begin
  if not Assigned(FStream) then Exit;
  FStream.Write(AValue, SizeOf(Byte));
end;

procedure TStreamWrapper.WriteCardinal(AValue: Cardinal);
begin
  if not Assigned(FStream) then Exit;
  FStream.Write(AValue, SizeOf(Cardinal));
end;

procedure TStreamWrapper.WriteInteger(AValue: Integer);
begin
  if not Assigned(FStream) then Exit;
  FStream.Write(AValue, SizeOf(Integer));
end;

procedure TStreamWrapper.WriteInt64(AValue: Int64);
begin
  if not Assigned(FStream) then Exit;
  FStream.Write(AValue, SizeOf(Int64));
end;

procedure TStreamWrapper.WriteSmallInt(AValue: SmallInt);
begin
  if not Assigned(FStream) then Exit;
  FStream.Write(AValue, SizeOf(SmallInt));
end;

procedure TStreamWrapper.WriteWord(AValue: Word);
begin
  if not Assigned(FStream) then Exit;
  FStream.Write(AValue, SizeOf(Word));
end;

procedure TStreamWrapper.WriteString(AValue: string);
var
  stringLength: Integer;
begin
  if not Assigned(FStream) then Exit;
  stringLength := Length(AValue);
  WriteInteger(stringLength);
  WriteStringFixed(AValue, stringLength);
end;

procedure TStreamWrapper.WriteStringFixed(AValue: string; ALength: Integer);
begin
  if not Assigned(FStream) then Exit;
  FStream.Write(PChar(AValue)^, ALength);
end;

function TStreamWrapper.Read(ABuffer: PByte; ACount: Cardinal): Cardinal;
begin
  Result := FStream.Read(ABuffer^, ACount);
end;

function TStreamWrapper.Write(ABuffer: PByte; ACount: Cardinal): Cardinal;
begin
  Result := FStream.Write(ABuffer^, ACount);
end;

procedure TStreamWrapper.Skip(ACount: Cardinal);
begin
  FStream.Seek(ACount, soFromCurrent);
end;

end.

