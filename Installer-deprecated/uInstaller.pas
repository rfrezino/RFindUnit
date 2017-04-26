unit uInstaller;

interface

uses
	ShellAPI, Windows, SysUtils, uDelphiInstallationCheck, Registry;

type
  TRetProcedure = procedure(Desc: string) of object;

  TInstaller = class(TObject)
  private
    FCallBackProc: TRetProcedure;
  const
      BPL_FILENAME = 'RFindUnit.bpl';
      DPK_FILENAME = 'RFindUnit.dpk';
      RSVARS_FILENAME = 'rsvars.bat';
      DCU32INT_EXE = 'dcu32int.exe';
  var
    FDelphiBplOutPut, FDelphiBinPath, FCurPath, FOutPutDir, FDelphiDesc,
    FUserAppDirFindUnit, FDcu32IntPath: string;
    FReg, FRegPacks: string;

    procedure LoadPaths(DelphiDesc, DelphiPath: string);

    procedure CheckDelphiRunning;
    procedure CheckDpkExists;
    procedure RemoveCurCompiledBpl;
    procedure CompileProject;
    procedure RegisterBpl;
    procedure RemoveOldDelphiBpl;
    procedure InstallBpl;
    procedure InstallDcu32Int;
  public
    constructor Create(DelphiDesc, DelphiPath: string);
    destructor Destroy; override;

    procedure Install(CallBackProc: TRetProcedure);
  end;

implementation

uses
  TlHelp32;

{ TInstaller }

function IsProcessRunning(const AExeFileName: string): Boolean;
var
  Continuar: BOOL;
  SnapshotHandle: THandle;
  Entry: TProcessEntry32;
begin
  Result := False;
  try
    SnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    Entry.dwSize := SizeOf(Entry);
    Continuar := Process32First(SnapshotHandle, Entry);
    while Integer(Continuar) <> 0 do
    begin
      if ((UpperCase(ExtractFileName(Entry.szExeFile)) = UpperCase(AExeFileName)) or
        (UpperCase(Entry.szExeFile) = UpperCase(AExeFileName))) then
      begin
        Result := True;
      end;
      Continuar := Process32Next(SnapshotHandle, Entry);
    end;
    CloseHandle(SnapshotHandle);
  except
    Result := False;
  end;
end;

procedure TInstaller.CheckDelphiRunning;
begin
  FCallBackProc('Cheking if is there same Delphi instance running...');
  if IsProcessRunning('bds.exe') then
    Raise Exception.Create('Close all you Delphi instances before install.');
end;

procedure TInstaller.CheckDpkExists;
begin
  FCallBackProc('Cheking if project file exist...');
  if not FileExists(FCurPath + DPK_FILENAME) then
    raise Exception.Create('The system was not able to find: ' + FCurPath + DPK_FILENAME);
end;

procedure TInstaller.CompileProject;
const
  GCC_CMDLINE = '/c call "%s" & dcc32 "%s" -LE"%s" & pause';
var
  GccCmd: WideString;
  I: Integer;
begin
  FCallBackProc('Compiling project...');
  GccCmd := Format(GCC_CMDLINE, [
    FDelphiBinPath + RSVARS_FILENAME,
    FCurPath + DPK_FILENAME,
    ExcludeTrailingPathDelimiter(FOutPutDir)]);

  ShellExecute(0, nil, 'cmd.exe', PChar(GccCmd), nil, SW_HIDE);

  for I := 0 to 10 do
  begin
    if FileExists(FOutPutDir + BPL_FILENAME) then
      Exit;

    if I = 9 then
      raise Exception.Create('Could not compile file: ' + FOutPutDir + BPL_FILENAME);
    Sleep(1000);
  end;
end;

constructor TInstaller.Create(DelphiDesc, DelphiPath: string);
begin
  FDelphiDesc := DelphiDesc;
  LoadPaths(DelphiDesc, DelphiPath);
end;

destructor TInstaller.Destroy;
begin

  inherited;
end;

procedure TInstaller.Install(CallBackProc: TRetProcedure);
begin
  FCallBackProc := CallBackProc;

  CheckDelphiRunning;
  CheckDpkExists;
  RemoveCurCompiledBpl;
  CompileProject;
  RegisterBpl;
  RemoveOldDelphiBpl;
  InstallBpl;
  InstallDcu32Int;
end;

procedure TInstaller.InstallBpl;
var
  I: Integer;
  Return: Boolean;
begin
  FCallBackProc('Installing new version...');
  Return := Windows.CopyFile(PChar(FOutPutDir + BPL_FILENAME), PChar(FDelphiBplOutPut + BPL_FILENAME), True);

  if not Return then
    raise Exception.Create('Could not install: ' + FDelphiBplOutPut + BPL_FILENAME + '. ' + SysErrorMessage(GetLastError));

  for I := 0 to 30 do
  begin
    if FileExists(FDelphiBplOutPut + BPL_FILENAME) then
      Exit;

    if I = 9 then
      raise Exception.Create('Could not install: ' + FDelphiBplOutPut + BPL_FILENAME);
    Sleep(500);
  end;
end;

procedure TInstaller.InstallDcu32Int;
begin
  FCallBackProc('Installing Dcu32Int');
  Windows.CopyFile(PChar(FDcu32IntPath + DCU32INT_EXE), PChar(FUserAppDirFindUnit + DCU32INT_EXE), True);
end;

procedure TInstaller.LoadPaths(DelphiDesc, DelphiPath: string);
var
  DelphiInst: TDelphiInstallationCheck;
  DelphiVersion: TDelphiVersions;
begin
  FDelphiBinPath := ExtractFilePath(DelphiPath);
  FCurPath := ExtractFilePath(ParamStr(0));

  FDelphiBplOutPut := GetEnvironmentVariable('public') + '\Documents\RAD Studio\RFindUnit\' + DelphiDesc + '\bpl\';
  FDcu32IntPath := FCurPath + '\Thirdy\Dcu32Int\';
  FUserAppDirFindUnit := GetEnvironmentVariable('appdata') + '\DelphiFindUnit\';
  FOutPutDir := FCurPath + 'Installer\build\' + FDelphiDesc + '\';

  ForceDirectories(FUserAppDirFindUnit);
  ForceDirectories(FDelphiBplOutPut);
  ForceDirectories(FOutPutDir);

  DelphiInst := TDelphiInstallationCheck.Create;
  try
    DelphiVersion := DelphiInst.GetDelphiVersionByName(DelphiDesc);
    FReg := DelphiInst.GetDelphiRegPathFromVersion(DelphiVersion);
    FRegPacks := FReg + '\Known Packages';
  finally
    DelphiInst.Free;
  end;
end;

procedure TInstaller.RegisterBpl;
var
  Reg: TRegistry;
begin
  FCallBackProc('Registering package...');

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(FRegPacks, False);
    Reg.WriteString(FDelphiBplOutPut + BPL_FILENAME, 'RfUtils');
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

procedure TInstaller.RemoveCurCompiledBpl;
var
  I: Integer;
begin
  FCallBackProc('Removing old bpl...');
  for I := 0 to 10 do
  begin
    if not FileExists(FOutPutDir + BPL_FILENAME) then
      Exit;
    DeleteFile(FOutPutDir + BPL_FILENAME);

    if I = 9 then
      raise Exception.Create('Could not remote file: ' + FOutPutDir + BPL_FILENAME);
    Sleep(200);
  end;
end;

procedure TInstaller.RemoveOldDelphiBpl;
var
  I: Integer;
begin
  FCallBackProc('Uninstalling old version...');
  for I := 0 to 10 do
  begin
    if not FileExists(FDelphiBplOutPut + BPL_FILENAME) then
      Exit;
    DeleteFile(FDelphiBplOutPut + BPL_FILENAME);

    if I = 9 then
      raise Exception.Create('Could not uninstall old version: ' + FDelphiBplOutPut + BPL_FILENAME);
    Sleep(200);
  end;
end;

end.
