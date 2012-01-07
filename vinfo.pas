unit vinfo;

{$mode objfpc}

interface

uses
  Classes, SysUtils, resource, versiontypes, versionresource;

type
  TVersionPrecision = 1..4;

  { TVersionInfo }

  TVersionInfo = class
  private
    FVersResource: TVersionResource;
    function GetFixedInfo: TVersionFixedInfo;
    function GetStringFileInfo: TVersionStringFileInfo;
    function GetVarFileInfo: TVersionVarFileInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(Instance: THandle);
    property FixedInfo: TVersionFixedInfo read GetFixedInfo;
    property StringFileInfo: TVersionStringFileInfo read GetStringFileInfo;
    property VarFileInfo: TVersionVarFileInfo read GetVarFileInfo;

    //Helper functions
    function GetProductVersionString(AMinPrecision: TVersionPrecision = 2): String;
    function GetCopyright(ALong: Boolean = false): String;
    procedure PrintStringFileInfo;
  end;

var
  VersionInfo: TVersionInfo;

implementation

{ TVersionInfo }

function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
begin
  Result := FVersResource.FixedInfo;
end;

function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
begin
  Result := FVersResource.StringFileInfo;
end;

function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
begin
  Result := FVersResource.VarFileInfo;
end;

constructor TVersionInfo.Create;
begin
  inherited Create;
  FVersResource := TVersionResource.Create;
end;

destructor TVersionInfo.Destroy;
begin
  FVersResource.Free;
  inherited Destroy;
end;

procedure TVersionInfo.Load(Instance: THandle);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, 1, PChar(RT_VERSION));
  try
    FVersResource.SetCustomRawDataStream(Stream);
    // access some property to load from the stream
    FVersResource.FixedInfo;
    // clear the stream
    FVersResource.SetCustomRawDataStream(nil);
  finally
    Stream.Free;
  end;
end;

function TVersionInfo.GetProductVersionString(AMinPrecision: TVersionPrecision = 2): String;
var
  productVersion: TFileProductVersion;
  lastVersion, i: Integer;
begin
  productVersion := FixedInfo.ProductVersion;
  lastVersion := 3;
  while (lastVersion >= AMinPrecision) and (productVersion[lastVersion] = 0) do
    dec(lastVersion);

  Result := '';
  for i := 0 to lastVersion do
  begin
    Result := Result + IntToStr(productVersion[i]);
    if i < lastVersion then
      Result := Result + '.';
  end;
end;

function TVersionInfo.GetCopyright(ALong: Boolean = False): String;
begin
  Result := StringFileInfo[0]['LegalCopyright'];
  if ALong then
    Result := StringReplace(Result, '(c) ', 'Copyright ', []);
end;

procedure TVersionInfo.PrintStringFileInfo;
var
  verStringInfo: TVersionStringFileInfo;
  verStringTable: TVersionStringTable;
  i, j: Integer;
begin
  verStringInfo := GetStringFileInfo;
  for i := 0 to verStringInfo.Count - 1 do
  begin
    writeln('Version String Info ', i + 1);
    verStringTable := verStringInfo.Items[i];
    for j := 0 to verStringTable.Count - 1 do
    begin
      writeln('  ', verStringTable.Keys[j], ' = ', verStringTable.ValuesByIndex[j]);
    end;
  end;
end;

initialization
  VersionInfo := TVersionInfo.Create;
  VersionInfo.Load(HINSTANCE);

finalization
  VersionInfo.Free;

end.

