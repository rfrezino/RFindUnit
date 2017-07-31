//Base code Font: https://github.com/RRUZ/blog/tree/master/Misc/SelectDelphiVersion
unit uDelphiInstallationCheck;

interface

uses
  Controls, SysUtils, Windows, ComCtrls;

type
  TDelphiVersions = (
    Delphi4,
    Delphi5,
    Delphi6,
    Delphi7,
    Delphi8,
    Delphi2005,
    Delphi2006,
    Delphi2007,
    Delphi2009,
    Delphi2010,
    DelphiXE,
    DelphiXE2,
    DelphiXE3,
    DelphiXE4,
    DelphiXE5,
    DelphiXE6,
    DelphiXE7,
    DelphiXE8,
    DelphiSeattle10,
    DelphiBerlin101);

  TDelphiInstallationCheck = class(TObject)
  public
    procedure LoadInstalledVersions(ImageList: TImageList; ListView: TListView);
    function GetDelphiVersionByName(DelphiName: string): TDelphiVersions;
    function GetDelphiRegPathFromVersion(Version: TDelphiVersions): string;
  end;

implementation

uses
  Registry, CommCtrl, ShellAPI;

const
  TDelphiVersionsNames: array [TDelphiVersions] of string = (
    'Delphi 4',
    'Delphi 5',
    'Delphi 6',
    'Delphi 7',
    'Delphi 8',
    'BDS 2005',
    'BDS 2006',
    'RAD Studio 2007',
    'RAD Studio 2009',
    'RAD Studio 2010',
    'RAD Studio XE',
    'RAD Studio XE 2',
    'RAD Studio XE 3',
    'RAD Studio XE 4',
    'RAD Studio XE 5',
    'RAD Studio XE 6',
    'RAD Studio XE 7',
    'RAD Studio XE 8',
    'RAD Studio 10 Seattle',
    'RAD Studio 10.1 Berlin',
    'RAD Studio 10.2 Tokyo',
    );

  TDelphiRegPaths: array [TDelphiVersions] of string = (
    '\Software\Borland\Delphi\4.0',
    '\Software\Borland\Delphi\5.0',
    '\Software\Borland\Delphi\6.0',
    '\Software\Borland\Delphi\7.0',
    '\Software\Borland\BDS\2.0',
    '\Software\Borland\BDS\3.0',
    '\Software\Borland\BDS\4.0',
    '\Software\Borland\BDS\5.0',
    '\Software\CodeGear\BDS\6.0',
    '\Software\CodeGear\BDS\7.0',
    '\Software\Embarcadero\BDS\8.0',
    '\Software\Embarcadero\BDS\9.0',
    '\Software\Embarcadero\BDS\10.0',
    '\Software\Embarcadero\BDS\11.0',
    '\Software\Embarcadero\BDS\12.0',
    '\Software\Embarcadero\BDS\14.0',
    '\Software\Embarcadero\BDS\15.0',
    '\Software\Embarcadero\BDS\16.0',
    '\Software\Embarcadero\BDS\17.0',
    '\Software\Embarcadero\BDS\18.0',
    '\Software\Embarcadero\BDS\19.0'
    );

function RegKeyExists(const RegPath: string; const RootKey: HKEY): Boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result := Reg.KeyExists(RegPath);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

function RegReadStr(const RegPath, RegValue: string; var Str: string; const RootKey: HKEY): Boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result := Reg.OpenKey(RegPath, True);
      if Result then
        Str := Reg.ReadString(RegValue);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

procedure ExtractIconFileToImageList(ImageList: TImageList; const Filename: string);
var
  FileInfo: TShFileInfo;
begin
  if FileExists(Filename) then
  begin
    FillChar(FileInfo, SizeOf(FileInfo), 0);
    SHGetFileInfo(PChar(Filename), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_SMALLICON);
    if FileInfo.hIcon <> 0 then
    begin
      ImageList_AddIcon(ImageList.Handle, FileInfo.hIcon);
      DestroyIcon(FileInfo.hIcon);
    end;
  end;
end;

{ TFrmSelDelphiVer }
function TDelphiInstallationCheck.GetDelphiRegPathFromVersion(Version: TDelphiVersions): string;
begin
  Result := TDelphiRegPaths[Version];
end;

function TDelphiInstallationCheck.GetDelphiVersionByName(DelphiName: string): TDelphiVersions;
var
  DelphiComp: TDelphiVersions;
begin
  for DelphiComp := Low(TDelphiVersions) to High(TDelphiVersions) do
    if TDelphiVersionsNames[TDelphiVersions(DelphiComp)] = DelphiName then
    begin
      Result := TDelphiVersions(DelphiComp);
    end;
end;

procedure TDelphiInstallationCheck.LoadInstalledVersions(ImageList: TImageList; ListView: TListView);
Var
  Item: TListItem;
  DelphiComp: TDelphiVersions;
  Filename: string;
  ImageIndex: Integer;
begin
  for DelphiComp := Low(TDelphiVersions) to High(TDelphiVersions) do
    if RegKeyExists(TDelphiRegPaths[DelphiComp], HKEY_CURRENT_USER) then
      if RegReadStr(TDelphiRegPaths[DelphiComp], 'App', Filename, HKEY_CURRENT_USER) and FileExists(Filename) then
      begin
        Item := ListView.Items.Add;
        Item.Caption := TDelphiVersionsNames[DelphiComp];
        Item.SubItems.Add(Filename);
        ExtractIconFileToImageList(ImageList, Filename);
        ImageIndex := ImageList.Count - 1;
        Item.ImageIndex := ImageIndex;
      end;
end;

end.
