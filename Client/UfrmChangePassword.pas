unit UfrmChangePassword;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmChangePassword }

  TfrmChangePassword = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edOldPwd: TEdit;
    edNewPwd: TEdit;
    edNewPwdRepeat: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblNewPwdRepeat: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure edNewPwdChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmChangePassword: TfrmChangePassword;

implementation

uses
  UdmNetwork, UPackets, UEnums;

{$R *.lfm}

{ TfrmChangePassword }

procedure TfrmChangePassword.FormShow(Sender: TObject);
begin
  edOldPwd.Text := '';
  edNewPwd.Text := '';
  edNewPwdRepeat.Text := '';
end;

procedure TfrmChangePassword.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmChangePassword.btnOKClick(Sender: TObject);
begin
  dmNetwork.Send(TChangePasswordPacket.Create(edOldPwd.Text,
    edNewPwd.Text));
end;

procedure TfrmChangePassword.edNewPwdChange(Sender: TObject);
var
  pwdValid: Boolean;
begin
  if edNewPwd.Text <> edNewPwdRepeat.Text then
  begin
    pwdValid := False;
    lblNewPwdRepeat.Font.Color := clRed;
  end else
  begin
    pwdValid := True;
    lblNewPwdRepeat.Font.Color := clDefault;
  end;

  btnOK.Enabled := (Length(edNewPwd.Text) > 0) and pwdValid;
end;

end.

