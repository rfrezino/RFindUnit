unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Registry, StdCtrls, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxSkinsCore, dxSkinsDefaultPainters, jpeg, cxImage;

type
  TFrmInstall = class(TForm)
    btnInstallDelphiXe: TButton;
    procedure btnInstallDelphiXeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmInstall: TFrmInstall;

implementation

const
  BPL_FILENAME = 'RfFindUnit.bpl';

{$R *.dfm}

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

end.
