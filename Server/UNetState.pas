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
unit UNetState;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lNet, UEnhancedMemoryStream, UAccount, ULinkedList;
  
type

  { TNetState }

  TNetState = class(TObject)
    constructor Create(ASocket: TLSocket);
    destructor Destroy; override;
  protected
    FSocket: TLSocket;
    FSendQueue: TEnhancedMemoryStream;
    FReceiveQueue: TEnhancedMemoryStream;
    FAccount: TAccount;
    FSubscriptions: TList;
    FLastAction: TDateTime;
  public
    property Socket: TLSocket read FSocket;
    property SendQueue: TEnhancedMemoryStream read FSendQueue;
    property ReceiveQueue: TEnhancedMemoryStream read FReceiveQueue;
    property Account: TAccount read FAccount write FAccount;
    property Subscriptions: TList read FSubscriptions;
    property LastAction: TDateTime read FLastAction write FLastAction;
  end;

implementation

{ TNetState }

constructor TNetState.Create(ASocket: TLSocket);
begin
  inherited Create;
  FSocket := ASocket;
  FSendQueue := TEnhancedMemoryStream.Create;
  FReceiveQueue := TEnhancedMemoryStream.Create;
  FAccount := nil;
  FSubscriptions := TList.Create;
  FLastAction := Now;
end;

destructor TNetState.Destroy;
var
  i: Integer;
begin
  if FSendQueue <> nil then FreeAndNil(FSendQueue);
  if FReceiveQueue <> nil then FreeAndNil(FReceiveQueue);
  if FSubscriptions <> nil then
  begin
    for i := 0 to FSubscriptions.Count - 1 do
      TLinkedList(FSubscriptions.Items[i]).Delete(Self);
    FreeAndNil(FSubscriptions);
  end;
  inherited Destroy;
end;

end.

