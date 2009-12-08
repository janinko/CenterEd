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
unit UfrmLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, EditBtn, Buttons, IniFiles;

type

  { TfrmLogin }

  TfrmLogin = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    cbProfile: TComboBox;
    edData: TDirectoryEdit;
    edHost: TEdit;
    edUsername: TEdit;
    edPassword: TEdit;
    gbConnection: TGroupBox;
    gbData: TGroupBox;
    gbActions: TGroupBox;
    gbProfiles: TGroupBox;
    imgHost: TImage;
    imgUsername: TImage;
    imgPassword: TImage;
    lblCopyright: TLabel;
    lblHost: TLabel;
    lblUsername: TLabel;
    lblPassword: TLabel;
    edPort: TSpinEdit;
    lblData: TLabel;
    btnSaveProfile: TSpeedButton;
    btnDeleteProfile: TSpeedButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnDeleteProfileClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSaveProfileClick(Sender: TObject);
    procedure cbProfileChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  protected
    FProfilePath: string;
  public
    { public declarations }
  end; 

var
  frmLogin: TfrmLogin;

implementation

uses
  UdmNetwork;
  
{$I version.inc}

{ TfrmLogin }

procedure TfrmLogin.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmLogin.btnDeleteProfileClick(Sender: TObject);
begin
  if cbProfile.ItemIndex > -1 then
  begin
    DeleteFile(FProfilePath + cbProfile.Text + '.ini');
    cbProfile.Items.Delete(cbProfile.ItemIndex);
  end;
end;

procedure TfrmLogin.btnOKClick(Sender: TObject);
var
  path: string;
begin
  path := IncludeTrailingPathDelimiter(edData.Text);
  if (not FileExists(path + 'art.mul')) or
     (not FileExists(path + 'artidx.mul')) or
     (not FileExists(path + 'hues.mul')) or
     (not FileExists(path + 'tiledata.mul')) or
     (not FileExists(path + 'animdata.mul')) or
     (not FileExists(path + 'texmaps.mul')) or
     (not FileExists(path + 'texidx.mul')) then
  begin
    MessageDlg('Incorrect directory', 'The data path you specified does not '
      + 'seem to be correct.', mtWarning, [mbOK], 0);
    edData.SetFocus;
  end else
    ModalResult := mrOK;
end;

procedure TfrmLogin.btnSaveProfileClick(Sender: TObject);
var
  profileName: string;
  profile: TIniFile;
begin
  profileName := cbProfile.Text;
  if InputQuery('Save profile', 'Enter the name of the profile:', profileName) then
  begin
    profile := TIniFile.Create(FProfilePath + profileName + '.ini');
    profile.WriteString('Connection', 'Host', edHost.Text);
    profile.WriteInteger('Connection', 'Port', edPort.Value);
    profile.WriteString('Connection', 'Username', edUsername.Text);
    profile.WriteString('Data', 'Path', edData.Text);
    profile.Free;
    cbProfile.ItemIndex := cbProfile.Items.IndexOf(profileName);
    if cbProfile.ItemIndex = -1 then
    begin
      cbProfile.Items.Add(profileName);
      cbProfile.ItemIndex := cbProfile.Items.Count - 1;
    end;
  end;
end;

procedure TfrmLogin.cbProfileChange(Sender: TObject);
var
  profile: TIniFile;
begin
  if cbProfile.ItemIndex > -1 then
  begin
    profile := TIniFile.Create(FProfilePath + cbProfile.Text + '.ini');
    edHost.Text := profile.ReadString('Connection', 'Host', '');
    edPort.Value := profile.ReadInteger('Connection', 'Port', 2597);
    edUsername.Text := profile.ReadString('Connection', 'Username', '');
    edPassword.Text := '';
    edData.Text := profile.ReadString('Data', 'Path', '');
    edPassword.SetFocus;
    profile.Free;
  end;
end;

procedure TfrmLogin.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOK then
    dmNetwork.CheckClose(Self);
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
var
  searchRec: TSearchRec;
begin
  lblCopyright.Caption := Format('UO CentrED Client Version %s (c) %s',
    [ProductVersion, Copyright]);

  FProfilePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
    + 'Profiles' + PathDelim;
  ForceDirectories(FProfilePath);
  if FindFirst(FProfilePath + '*.ini', faAnyFile, searchRec) = 0 then
  begin
    repeat
      cbProfile.Items.Add(ChangeFileExt(searchRec.Name, ''));
    until FindNext(searchRec) <> 0;
  end;
  FindClose(searchRec);
end;

initialization
  {$I UfrmLogin.lrs}

end.

