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
unit UdmNetwork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, lNetComponents, lNet,
  UEnhancedMemoryStream, UPacket, UEnums, ExtCtrls, dateutils, URectList,
  LCLIntf;

type

  { TdmNetwork }

  TdmNetwork = class(TDataModule)
    TCPClient: TLTCPComponent;
    tmNoOp: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TCPClientConnect(aSocket: TLSocket);
    procedure TCPClientDisconnect(aSocket: TLSocket);
    procedure TCPClientError(const msg: string; aSocket: TLSocket);
    procedure TCPClientReceive(aSocket: TLSocket);
    procedure tmNoOpStartTimer(Sender: TObject);
    procedure tmNoOpTimer(Sender: TObject);
  protected
    FSendQueue: TEnhancedMemoryStream;
    FReceiveQueue: TEnhancedMemoryStream;
    FUsername: string;
    FPassword: string;
    FAccessLevel: TAccessLevel;
    FDataDir: string;
    FLastPacket: TDateTime;
    FWriteMap: TRectList;
    procedure OnCanSend(ASocket: TLSocket);
    procedure OnConnectionHandlingPacket(ABuffer: TEnhancedMemoryStream);
    procedure ProcessQueue;
    procedure DoLogin;
  public
    property Username: string read FUsername;
    property AccessLevel: TAccessLevel read FAccessLevel write FAccessLevel;
    function CanWrite(AX, AY: Word): Boolean;
    procedure Send(APacket: TPacket);
    procedure Disconnect;
    procedure CheckClose(ASender: TForm);
    procedure UpdateWriteMap(AStream: TEnhancedMemoryStream);
  end; 

var
  dmNetwork: TdmNetwork;

implementation

uses
  UPacketHandlers, UPackets, UfrmMain, UfrmLogin, UfrmInitialize,
  UGameResources, UfrmAccountControl, UfrmEditAccount, UfrmDrawSettings,
  UfrmBoundaries, UfrmElevateSettings, UfrmConfirmation, UfrmMoveSettings,
  UfrmAbout, UfrmHueSettings, UfrmRadar, UfrmLargeScaleCommand,
  UfrmVirtualLayer, UfrmFilter, UfrmTileInfo, UfrmRegionControl;
  
{$I version.inc}

{ TdmNetwork }

procedure TdmNetwork.DataModuleCreate(Sender: TObject);
begin
  FSendQueue := TEnhancedMemoryStream.Create;
  FReceiveQueue := TEnhancedMemoryStream.Create;
  TCPClient.OnCanSend := @OnCanSend;
  PacketHandlers[$02] := TPacketHandler.Create(0, @OnConnectionHandlingPacket);
  DoLogin;
end;

procedure TdmNetwork.DataModuleDestroy(Sender: TObject);
begin
  if FSendQueue <> nil then FreeAndNil(FSendQueue);
  if FReceiveQueue <> nil then FreeAndNil(FReceiveQueue);
  if PacketHandlers[$02] <> nil then FreeAndNil(PacketHandlers[$02]);
  if FWriteMap <> nil then FreeAndNil(FWriteMap);
end;

procedure TdmNetwork.TCPClientConnect(aSocket: TLSocket);
begin
  FSendQueue.Clear;
  FReceiveQueue.Clear;
end;

procedure TdmNetwork.TCPClientDisconnect(aSocket: TLSocket);
begin
  FSendQueue.Clear;
  FReceiveQueue.Clear;
  DoLogin;
end;

procedure TdmNetwork.TCPClientError(const msg: string; aSocket: TLSocket);
begin
  MessageDlg('Connection error', msg, mtError, [mbOK], 0);
  if not TCPClient.Connected then
    TCPClientDisconnect(aSocket);
end;

procedure TdmNetwork.TCPClientReceive(aSocket: TLSocket);
var
  buffer: array[0..4095] of byte;
  size: Integer;
begin
  repeat
    size := TCPClient.Get(buffer, 4096);
    if size > 0 then
      FReceiveQueue.Enqueue(buffer, size);
  until size <= 0;
  ProcessQueue;
end;

procedure TdmNetwork.tmNoOpStartTimer(Sender: TObject);
begin
  FLastPacket := Now;
end;

procedure TdmNetwork.tmNoOpTimer(Sender: TObject);
begin
  if SecondsBetween(FLastPacket, Now) > 25 then
    Send(TNoOpPacket.Create);
end;

procedure TdmNetwork.OnCanSend(ASocket: TLSocket);
var
  size: Integer;
begin
  while FSendQueue.Size > 0 do
  begin
    FLastPacket := Now;
    size := TCPClient.Send(FSendQueue.Memory^, FSendQueue.Size);
    if size > 0 then
      FSendQueue.Dequeue(size)
    else
      Break;
  end;
end;

procedure TdmNetwork.OnConnectionHandlingPacket(ABuffer: TEnhancedMemoryStream);
var
  subID: Byte;
  loginState: TLoginState;
  width, height: Word;
  serverState: TServerState;
begin
  subID := ABuffer.ReadByte;
  case subID of
    $01:
      begin
        if ABuffer.ReadCardinal = ProtocolVersion then
        begin
          frmInitialize.lblStatus.Caption := 'Authenticating';
          Send(TLoginRequestPacket.Create(FUsername, FPassword));
        end else
        begin
          MessageDlg('Error', 'Invalid protocol version. Maybe your client is outdated.', mtError, [mbOK], 0);
          Disconnect;
        end;
      end;
    $03:
      begin
        loginState := TLoginState(ABuffer.ReadByte);
        if loginState = lsOK then
        begin
          frmInitialize.lblStatus.Caption := 'Initializing';
          frmInitialize.Repaint;
          frmInitialize.lblStatus.Repaint;
          Application.ProcessMessages;
          FAccessLevel := TAccessLevel(ABuffer.ReadByte);
          InitGameResourceManager(FDataDir);
          width := ABuffer.ReadWord;
          height := ABuffer.ReadWord;
          ResMan.InitLandscape(width, height);

          {FWriteMap := TBits.Create(ResMan.Landscape.CellWidth * ResMan.Landscape.CellHeight);
          FWriteMap.XorBits(FWriteMap); //set all to 1}
          FWriteMap := TRectList.Create;
          UpdateWriteMap(ABuffer);

          frmMain := TfrmMain.Create(dmNetwork);
          frmRadarMap := TfrmRadarMap.Create(frmMain);
          frmLargeScaleCommand := TfrmLargeScaleCommand.Create(frmMain);
          frmRegionControl := TfrmRegionControl.Create(frmMain);
          frmAccountControl := TfrmAccountControl.Create(frmMain);
          frmEditAccount := TfrmEditAccount.Create(frmAccountControl);
          frmConfirmation := TfrmConfirmation.Create(frmMain);
          frmDrawSettings := TfrmDrawSettings.Create(frmMain);
          frmMoveSettings := TfrmMoveSettings.Create(frmMain);
          frmElevateSettings := TfrmElevateSettings.Create(frmMain);
          frmHueSettings := TfrmHueSettings.Create(frmMain);
          frmBoundaries := TfrmBoundaries.Create(frmMain);
          frmFilter := TfrmFilter.Create(frmMain);
          frmVirtualLayer := TfrmVirtualLayer.Create(frmMain);
          frmAbout := TfrmAbout.Create(frmMain);
          frmTileInfo := TfrmTileInfo.Create(frmMain);
          frmMain.Show;
          frmInitialize.Hide;
          tmNoOp.Enabled := True;
        end else
        begin
          if loginState = lsInvalidUser then
            MessageDlg('Error', 'The username you specified is incorrect.', mtWarning, [mbOK], 0)
          else if loginState = lsInvalidPassword then
            MessageDlg('Error', 'The password you specified is incorrect.', mtWarning, [mbOK], 0)
          else if loginState = lsAlreadyLoggedIn then
            MessageDlg('Error', 'There is already a client logged in using that account.', mtWarning, [mbOK], 0)
          else if loginState = lsNoAccess then
            MessageDlg('Error', 'This account has no access.', mtWarning, [mbOK], 0);
        end;
      end;
    $04: //Server state
      begin
        serverState := TServerState(ABuffer.ReadByte);
        if serverState = ssRunning then
        begin
          frmInitialize.UnsetModal;
          frmInitialize.Hide;
          tmNoOp.Enabled := True;
        end else
        begin
          case serverState of
            ssFrozen: frmInitialize.lblStatus.Caption := 'The server is currently paused.';
            ssOther: frmInitialize.lblStatus.Caption := ABuffer.ReadStringNull
          end;
          tmNoOp.Enabled := False;
          frmInitialize.Show;
          frmInitialize.SetModal;
        end;
      end;
  end;
end;

procedure TdmNetwork.ProcessQueue;
var
  packetHandler: TPacketHandler;
  size: Cardinal;
begin
  FReceiveQueue.Position := 0;
  while FReceiveQueue.Size >= 1 do
  begin
    packetHandler := PacketHandlers[FReceiveQueue.ReadByte];
    if packetHandler <> nil then
    begin
      size := packetHandler.PacketLength;
      if size = 0 then
      begin
        if FReceiveQueue.Size > 5 then
          size := FReceiveQueue.ReadCardinal
        else
          Break; //wait for more data
      end;

      if FReceiveQueue.Size >= size then
      begin
        FReceiveQueue.Lock(FReceiveQueue.Position, size - FReceiveQueue.Position); //prevent handler from reading too much
        packetHandler.Process(FReceiveQueue);
        FReceiveQueue.Unlock;
        FReceiveQueue.Dequeue(size);
      end else
        Break; //wait for more data
    end else
    begin
      {Writeln('Dropping client due to unknown packet: ', ANetState.Socket.PeerAddress);}
      Disconnect;
      FReceiveQueue.Clear;
    end;
  end;
end;

procedure TdmNetwork.DoLogin;
begin
  tmNoOp.Enabled := False;
  frmLogin := TfrmLogin.Create(dmNetwork);
  if frmInitialize = nil then frmInitialize := TfrmInitialize.Create(dmNetwork);
  if frmTileInfo <> nil then FreeAndNil(frmTileInfo);
  if frmEditAccount <> nil then FreeAndNil(frmEditAccount);
  if frmAccountControl <> nil then FreeAndNil(frmAccountControl);
  if frmConfirmation <> nil then FreeAndNil(frmConfirmation);
  if frmDrawSettings <> nil then FreeAndNil(frmDrawSettings);
  if frmMoveSettings <> nil then FreeAndNil(frmMoveSettings);
  if frmElevateSettings <> nil then FreeAndNil(frmElevateSettings);
  if frmHueSettings <> nil then FreeAndNil(frmHueSettings);
  if frmBoundaries <> nil then FreeAndNil(frmBoundaries);
  if frmFilter <> nil then FreeAndNil(frmFilter);
  if frmVirtualLayer <> nil then FreeAndNil(frmVirtualLayer);
  if frmAbout <> nil then FreeAndNil(frmAbout);
  if frmRegionControl <> nil then FreeAndNil(frmRegionControl);
  if frmLargeScaleCommand <> nil then FreeAndNil(frmLargeScaleCommand);
  if frmRadarMap <> nil then FreeAndNil(frmRadarMap);
  if frmMain <> nil then
  begin
    frmMain.ApplicationProperties1.OnIdle := nil;
    FreeAndNil(frmMain);
  end;
  if FWriteMap <> nil then FreeAndNil(FWriteMap);
  if GameResourceManager <> nil then FreeAndNil(GameResourceManager);
  frmInitialize.Hide;
  while frmLogin.ShowModal = mrOK do
  begin
    if TCPClient.Connect(frmLogin.edHost.Text, frmLogin.edPort.Value) then
    begin
      FUsername := frmLogin.edUsername.Text;
      FPassword := frmLogin.edPassword.Text;
      FDataDir := frmLogin.edData.Text;
      frmInitialize.lblStatus.Caption := 'Connecting';
      frmInitialize.Show;
      Break;
    end else
      MessageDlg('Error', 'Cannot connect to the specified server.', mtError, [mbOK], 0);
  end;
  frmLogin.Close;
  FreeAndNil(frmLogin);
end;

function TdmNetwork.CanWrite(AX, AY: Word): Boolean;
var
  i: Integer;
  pt: TPoint;
begin
  if FWriteMap.Count > 0 then
  begin
    pt := Point(AX, AY);
    for i := 0 to FWriteMap.Count - 1 do
      if PtInRect(FWriteMap.Rects[i], pt) then
        Exit(True);

    Result := False;
  end else
    Result := True;
end;

procedure TdmNetwork.Send(APacket: TPacket);
var
  source: TEnhancedMemoryStream;
begin
  if TCPClient.Connected then
  begin
    FSendQueue.Seek(0, soFromEnd);
    source := APacket.Stream;
    FSendQueue.CopyFrom(source, 0);
    OnCanSend(nil);
  end;
  APacket.Free;
end;

procedure TdmNetwork.Disconnect;
begin
  Send(TQuitPacket.Create);
end;

procedure TdmNetwork.CheckClose(ASender: TForm);
begin
  if ((frmLogin = nil) or (ASender = frmLogin)) and
     ((frmMain = nil) or (ASender = frmMain)) and
     ((frmInitialize = nil) or (not frmInitialize.Visible)) then
  begin
    Application.Terminate;
  end;
end;

procedure TdmNetwork.UpdateWriteMap(AStream: TEnhancedMemoryStream);
var
  x1, y1, x2, y2: Word;
  i, areaCount: Integer;
begin
  FWriteMap.Clear;
  areaCount := AStream.ReadWord;
  for i := 0 to areaCount - 1 do
  begin
    x1 := AStream.ReadWord;
    y1 := AStream.ReadWord;
    x2 := AStream.ReadWord;
    y2 := AStream.ReadWord;
    FWriteMap.Add(x1, y1, x2, y2);
  end;
end;

initialization
  {$I UdmNetwork.lrs}

end.

