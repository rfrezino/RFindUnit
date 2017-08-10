program Installer;

uses
  Forms,
  uMain in 'uMain.pas' {FrmInstall},
  uDelphiInstallationCheck in 'uDelphiInstallationCheck.pas',
  uInstaller in 'uInstaller.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'RfUtils - Fix Compilling';
  Application.CreateForm(TFrmInstall, FrmInstall);
  Application.Run;
end.
