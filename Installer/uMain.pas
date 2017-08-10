unit uMain;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, Registry,
  StdCtrls, ComCtrls, uDelphiInstallationCheck, ExtCtrls,
  uInstaller, ImgList, System.ImageList;

type
  TFrmInstall = class(TForm)
    ilDelphiIcons: TImageList;
    grpDelphiVersions: TGroupBox;
    lvDelphis: TListView;
    pnlDesc: TPanel;
    lblVersion: TLabel;
    btnInstall: TButton;
    lblDescription: TLabel;
    procedure btnInstallDelphiXeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure lvDelphisDblClick(Sender: TObject);
  private
    FDelphiVersion: TDelphiInstallationCheck;

    function GetSelectedPath: string;
    function GetDelphiDesc: string;

    procedure RefreshDesc(Desc: string);
  public
    procedure Install;
  end;

var
  FrmInstall: TFrmInstall;

implementation

const
  BPL_FILENAME = 'RfFindUnit.bpl';

{$R *.dfm}

procedure TFrmInstall.btnInstallClick(Sender: TObject);
begin
  Install;
end;

procedure TFrmInstall.btnInstallDelphiXeClick(Sender: TObject);
var
  Reg: TRegistry;
  BplPath: string;
  InstalFile: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('SOFTWARE\Embarcadero\BDS\8.0', False);
    BplPath := Reg.ReadString('RootDir');
    Reg.CloseKey;

    BplPath := BplPath + 'bin\' + BPL_FILENAME;

    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('SOFTWARE\Embarcadero\BDS\8.0\Known Packages', False);
    Reg.WriteString('$(BDS)\bin\' + BPL_FILENAME, 'RfUtils');
    Reg.CloseKey;

    InstalFile := ExtractFilePath(ParamStr(0)) + '\DelphiXE\' + BPL_FILENAME;

    if not FileExists(InstalFile) then
    begin
      MessageDlg('Installation file not found.', mtError,[mbOK],0);
      Exit;
    end;

    CopyFile(PWideChar(InstalFile), PWideChar(BplPath), True);

    MessageDlg('Installation finished!', mtInformation, [mbOK], 0);
  finally
    Reg.Free;
  end;
end;

procedure TFrmInstall.FormShow(Sender: TObject);
begin
  FDelphiVersion := TDelphiInstallationCheck.Create;

  FDelphiVersion.LoadInstalledVersions(ilDelphiIcons, lvDelphis);
end;

function TFrmInstall.GetSelectedPath: string;
var
  SelItem: TListItem;
begin
  SelItem := lvDelphis.Selected;
  if SelItem = nil then
    Raise Exception.Create('You must select a Delphi Version.');

  Result := SelItem.SubItems[0];
end;

function TFrmInstall.GetDelphiDesc: string;
var
  SelItem: TListItem;
begin
  SelItem := lvDelphis.Selected;
  if SelItem = nil then
    Raise Exception.Create('You must select a Delphi Version.');

  Result := SelItem.Caption;
end;

procedure TFrmInstall.Install;
var
  Installer: TInstaller;
begin
  lblDescription.Caption := 'Starting...';
  lblDescription.Visible := True;
  try
    Installer := TInstaller.Create(GetDelphiDesc, GetSelectedPath);
    try
      Installer.Install(RefreshDesc)
    finally
      Installer.Free;
    end;

    MessageDlg('Installation finished!', mtInformation, [mbOK], 0);
  except
    on E: exception do
      MessageDlg('Error on installation: ' + e.Message, mtError, [mbOK], 0);
  end;
  lblDescription.Visible := False;
end;

procedure TFrmInstall.lvDelphisDblClick(Sender: TObject);
begin
  Install;
end;

procedure TFrmInstall.RefreshDesc(Desc: string);
begin
  lblDescription.Caption := Desc;
  Application.ProcessMessages;
end;

end.
