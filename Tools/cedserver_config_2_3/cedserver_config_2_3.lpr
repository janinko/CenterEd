program cedserver_config_2_3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, IniFiles, XMLWrite, dom;

procedure WriteXMLString(AElement: TDOMElement; AName, AValue: string);
var
  child: TDOMElement;
begin
  child := AElement.OwnerDocument.CreateElement(AName);
  AElement.AppendChild(child);
  child.AppendChild(AElement.OwnerDocument.CreateTextNode(AValue));
end;

procedure WriteXMLAccount(AElement: TDOMElement; AAccountString: string);
var
  splitPos: Integer;
  lastPos: TDOMElement;
begin
  //Account name
  splitPos := Pos('=', AAccountString);
  WriteXMLString(AElement, 'Name', Copy(AAccountString, 1, splitPos - 1));
  Delete(AAccountString, 1, splitPos);

  //Accesslevel
  splitPos := Pos(':', AAccountString);
  WriteXMLString(AElement, 'AccessLevel', Copy(AAccountString, 1, splitPos - 1));
  Delete(AAccountString, 1, splitPos);

  lastPos := AElement.OwnerDocument.CreateElement('LastPos');
  lastPos.AttribStrings['x'] := '0';
  lastPos.AttribStrings['y'] := '0';

  //Passwordhash
  splitPos := Pos(':', AAccountString);
  if splitPos > 0 then
  begin
    WriteXMLString(AElement, 'PasswordHash', Copy(AAccountString, 1, splitPos - 1));
    Delete(AAccountString, 1, splitPos);

    //LastPos
    splitPos := Pos(':', AAccountString);
    if splitPos > 0 then
    begin
      lastPos.AttribStrings['x'] := Copy(AAccountString, 1, splitPos - 1);
      lastPos.AttribStrings['y'] := Copy(AAccountString, splitPos + 1, Length(AAccountString));
    end;
  end else
    WriteXMLString(AElement, 'PasswordHash', AAccountString);

  AElement.AppendChild(lastPos);
  AElement.AppendChild(AElement.OwnerDocument.CreateElement('Regions'));
end;

var
  v2ini: TIniFile;
  v3xml: TXMLDocument;
  root, parent, child: TDOMElement;
  i: Integer;
  rawAccounts: TStringList;

begin
  writeln('Opening "cedserver.ini"');
  v2ini := TIniFile.Create('cedserver.ini');

  writeln('Preparing XML structure');
  v3xml := TXMLDocument.Create;
  root := v3xml.CreateElement('CEDConfig');
  root.AttribStrings['Version'] := '3';
  v3xml.AppendChild(root);

  writeln('Transfering settings');

  WriteXMLString(root, 'Port', v2ini.ReadString('Network', 'Port', '2597'));

  parent := v3xml.CreateElement('Map');
  root.AppendChild(parent);
  WriteXMLString(parent, 'Map', v2ini.ReadString('Paths', 'map', 'map0.mul'));
  WriteXMLString(parent, 'StaIdx', v2ini.ReadString('Paths', 'staidx', 'staidx0.mul'));
  WriteXMLString(parent, 'Statics', v2ini.ReadString('Paths', 'statics', 'statics0.mul'));
  WriteXMLString(parent, 'Width', v2ini.ReadString('Parameters', 'Width', '768'));
  WriteXMLString(parent, 'Height', v2ini.ReadString('Parameters', 'Height', '512'));

  WriteXmlString(root, 'Tiledata', v2ini.ReadString('Paths', 'tiledata', 'tiledata.mul'));
  WriteXmlString(root, 'Radarcol', v2ini.ReadString('Paths', 'radarcol', 'radarcol.mul'));

  writeln('Transfering accounts');

  parent := v3xml.CreateElement('Accounts');
  root.AppendChild(parent);
  rawAccounts := TStringList.Create;
  v2ini.ReadSectionRaw('Accounts', rawAccounts);
  for i := 0 to rawAccounts.Count - 1 do
  begin
    child := v3xml.CreateElement('Account');
    parent.AppendChild(child);
    WriteXMLAccount(child, rawAccounts.Strings[i]);
  end;
  rawAccounts.Free;

  root.AppendChild(v3xml.CreateElement('Regions'));

  v2ini.Free;

  writeln('Writing "cedserver.xml"');
  WriteXMLFile(v3xml, 'cedserver.xml');
  Writeln('Done.');
end.

