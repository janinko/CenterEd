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
unit UConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, md5, Keyboard, UAccount;

  
var
  AppDir: string;
  Config: TIniFile;
  Accounts: TAccountList;
  
procedure InitConfig;
function LoadConfig: Boolean;
function TimeStamp: string;

implementation

const
  CONFIGVERSION = 2;
  
function QueryPassword: String;
var
  pwChar: char;
begin
  Result := '';

  InitKeyboard;
  try
    repeat
      pwChar := GetKeyEventChar(TranslateKeyEvent(GetKeyEvent));
      case pwChar of
        #8: Result := Copy(Result, 1, Length(Result) - 1);
        #13: break;
      else
        Result := Result + pwChar;
      end;
    until pwChar = #13;
  finally
    DoneKeyboard;
  end;
  writeln('');
end;

procedure InitConfig;
var
  configFile: string;
  stringValue, password: string;
  intValue: Integer;
begin
  configFile := ChangeFileExt(ParamStr(0), '.ini');
  DeleteFile(configFile);
  Config := TIniFile.Create(configFile);
  Config.WriteInteger('Config', 'Version', CONFIGVERSION);
  
  Writeln('Configuring Network');
  Writeln('===================');
  Write  ('Port [2597]: ');
  Readln (stringValue);
  if not TryStrToInt(stringValue, intValue) then intValue := 2597;
  Config.WriteInteger('Network', 'Port', intValue);
  Writeln('');
  
  Writeln('Configuring Paths');
  Writeln('=================');
  Write  ('map [map0.mul]: ');
  Readln (stringValue);
  if stringValue = '' then stringValue := 'map0.mul';
  Config.WriteString('Paths', 'map', stringValue);
  Write  ('statics [statics0.mul]: ');
  Readln (stringValue);
  if stringValue = '' then stringValue := 'statics0.mul';
  Config.WriteString('Paths', 'statics', stringValue);
  Write  ('staidx [staidx0.mul]: ');
  Readln (stringValue);
  if stringValue = '' then stringValue := 'staidx0.mul';
  Config.WriteString('Paths', 'staidx', stringValue);
  Write  ('tiledata [tiledata.mul]: ');
  Readln (stringValue);
  if stringValue = '' then stringValue := 'tiledata.mul';
  Config.WriteString('Paths', 'tiledata', stringValue);
  Write  ('radarcol [radarcol.mul]: ');
  Readln (stringValue);
  if stringValue = '' then stringValue := 'radarcol.mul';
  Config.WriteString('Paths', 'radarcol', stringValue);
  Writeln('');
  
  Writeln('Parameters');
  Writeln('==========');
  Write  ('Map width [768]: ');
  Readln (stringValue);
  if not TryStrToInt(stringValue, intValue) then intValue := 768;
  Config.WriteInteger('Parameters', 'Width', intValue);
  Write  ('Map height [512]: ');
  Readln (stringValue);
  if not TryStrToInt(stringValue, intValue) then intValue := 512;
  Config.WriteInteger('Parameters', 'Height', intValue);
  Writeln('');
  
  Writeln('Admin account');
  Writeln('=============');
  repeat
    Write('Account name: ');
    Readln(stringValue);
  until stringValue <> '';
  Write  ('Password [hidden]: ');
  password := QueryPassword;
  Config.WriteString('Accounts', stringValue, '255:' + MD5Print(MD5String(password)));
end;

function LoadConfig: Boolean;
var
  configFile: string;
  values: TStringList;
  i: Integer;
begin
  configFile := ChangeFileExt(ParamStr(0), '.ini');
  if FileExists(configFile) then
  begin
    Config := TIniFile.Create(configFile);
    Result := (Config.ReadInteger('Config', 'Version', 0) = CONFIGVERSION);
    if Result then
    begin
      Accounts := TAccountList.Create;
      values := TStringList.Create;
      Config.ReadSectionRaw('Accounts', values);
      for i := 0 to values.Count - 1 do
        Accounts.Add(TAccount.Create(values.Strings[i]));
      values.Free;
    end;
  end else
    Result := False;
end;

function TimeStamp: string;
begin
  Result := '[' + DateTimeToStr(Now) + '] ';
end;

initialization
begin
  AppDir := ExtractFilePath(ParamStr(0));
  if AppDir[Length(AppDir)] <> PathDelim then
    AppDir := AppDir + PathDelim;
end;

finalization
begin
  if Config <> nil then FreeAndNil(Config);
  if Accounts <> nil then FreeAndNil(Accounts);
end;

end.

