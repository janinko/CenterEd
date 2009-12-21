unit UfrmLightlevel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, UfrmToolWindow;

type

  { TfrmLightlevel }

  TfrmLightlevel = class(TfrmToolWindow)
    tbLightlevel: TTrackBar;
    procedure tbLightlevelChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmLightlevel: TfrmLightlevel;

implementation

uses
  UfrmMain;

{ TfrmLightlevel }

procedure TfrmLightlevel.tbLightlevelChange(Sender: TObject);
begin
  if frmMain.LightManager.LightLevel = 0 then
  begin
    frmMain.LightManager.LightLevel := tbLightlevel.Position;
    frmMain.InvalidateFilter;
  end else
    frmMain.LightManager.LightLevel := tbLightlevel.Position;
end;

initialization
  {$I UfrmLightlevel.lrs}

end.

