unit FindUnit.DcuDecompiler;

interface

uses
  System.Classes,
  Winapi.Windows,

  FindUnit.Utils,
  Winapi.ShellAPI;

const
  DCU32INT_EXECUTABLE = 'dcu32int.exe';

type
  TDcuDecompiler = class(TObject)
  private
    FDir: string;
    FExecutablePath: string;

    FGeneratedFiles: TStringList;

    procedure CreateDirs;
    function ProcessUnit(FileName: string; OutRedir: boolean): Integer;

    procedure ProcessFilesInternal(AFiles: TStringList);
    procedure ProcessFilesFromExe(AFiles: TStringList);

    procedure FixeGeneratedFiles;
    procedure FixeGeneratedFile(AFile: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessFiles(AFiles: TStringList);

    function Dcu32IntExecutableExists: Boolean;
  end;

  function Dcu32IntExecutableExists: Boolean;
  function GetDcu32ExecutablePath: string;

implementation

uses
  System.SysUtils{$IFDEF UNICODE}, System.AnsiStrings{$ENDIF}, Log4Pascal;

{ TDcuDecompiler }

function GetDcu32ExecutablePath: string;
var
  Dcu: TDcuDecompiler;
begin
  Dcu := TDcuDecompiler.Create;
  Result := Dcu.FExecutablePath;
  Dcu.Free;
end;

function Dcu32IntExecutableExists: Boolean;
var
  Dcu: TDcuDecompiler;
begin
  Dcu := TDcuDecompiler.Create;
  Result := Dcu.Dcu32IntExecutableExists;
  Dcu.Free;
end;

procedure TDcuDecompiler.CreateDirs;
begin
  FDir := FindUnitDcuDir;
  FExecutablePath := FindUnitDir + DCU32INT_EXECUTABLE;
  CreateDir(FDir);
end;

function TDcuDecompiler.Dcu32IntExecutableExists: Boolean;
begin
  Result := FileExists(FExecutablePath);
end;

destructor TDcuDecompiler.Destroy;
begin
  FGeneratedFiles.Free;
  inherited;
end;

procedure TDcuDecompiler.FixeGeneratedFile(AFile: string);
var
  FileS: TStringList;
  I: Integer;
  Line: string;
  DeleteLine: Boolean;
  MustSave: Boolean;
begin
  if not FileExists(AFile) then
  begin
    Logger.Error('TDcuDecompiler.FixeGeneratedFile: File not fount after process: %s', [AFile]);
    Exit;
  end;

  MustSave := False;

  FileS := TStringList.Create;
  FileS.LoadFromFile(AFile);
  for I := FileS.Count -1 downto 0 do
  begin
    DeleteLine := True;
    Line := Trim(FileS[i]);
    if Line = '' then
      DeleteLine := False;

    if (DeleteLine) or (Pos(';;', Line) > 0) then
    begin
      MustSave := True;
      DeleteLine := True;
      FileS.Delete(I);
    end;
  end;

  if MustSave then
    FileS.SaveToFile(AFile);
  FileS.Free;
end;

procedure TDcuDecompiler.FixeGeneratedFiles;
var
  FileS: string;
begin
  for FileS in FGeneratedFiles do
    FixeGeneratedFile(FileS);
end;

function TDcuDecompiler.ProcessUnit(FileName: string; OutRedir: boolean): Integer;
//var
//  UnitFromDcu: TUnit;
begin
//  Result := 0;
//  try
//    FileName := ExpandFileName(FileName);
//    UnitFromDcu := nil;
//    try
//      UnitFromDcu := GetDCUByName(FileName, '', 0, false, dcuplWin32, 0);
//    finally
//      if UnitFromDcu = nil then
//        UnitFromDcu := MainUnit;
//      if UnitFromDcu <> nil then
//        UnitFromDcu.Show;
//    end;
//  except
//    on E: Exception do
//    begin
//      Result := 1;
//    end;
//  end;
//  UnitFromDcu.Free;
end;

constructor TDcuDecompiler.Create;
begin
  FGeneratedFiles := TStringList.Create;
  CreateDirs;
end;

procedure TDcuDecompiler.ProcessFiles(AFiles: TStringList);
begin
//  ProcessFilesInternal(AFiles);
  ProcessFilesFromExe(AFiles);
  FixeGeneratedFiles;
end;

procedure TDcuDecompiler.ProcessFilesFromExe(AFiles: TStringList);
var
  I: Integer;
  FileNameOut: string;
  InputParams: string;
begin
  if not Dcu32IntExecutableExists then
    Exit;

  for I := 0 to AFiles.Count -1 do
  begin
    if (not FileExists(AFiles[i])) then
      Continue;

    if Pos('\', AFiles[i]) = 1 then
      Continue;

    FileNameOut := ExtractFileName(AFiles[i]);
    FileNameOut := StringReplace(FileNameOut, '.dcu', '.pas', [rfReplaceAll, rfIgnoreCase]);
    FileNameOut := FDir + FileNameOut;
    FGeneratedFiles.Add(FileNameOut);

    try
      InputParams := Format('"%s" "-I" "-X%s"', [AFiles[i], FileNameOut]);
      ShellExecute(0, 'open', PChar(FExecutablePath), PChar(InputParams), nil, SW_HIDE);
    except
      on e: exception do
      begin
        Logger.Error('TDcuDecompiler.ProcessFile[%s]: %s', [AFiles[i], E.Message]);
        {$IFDEF RAISEMAD} raise; {$ENDIF}
      end;
    end;
  end;

end;

procedure TDcuDecompiler.ProcessFilesInternal(AFiles: TStringList);
//var
//  I: Integer;
//  FileNameOut: string;
//  DeveSair: Boolean;
begin
  {I'not using this method 'cause there are to much leaks on dcu32int that by now
  make it unusable on a single exe'}
//  DeveSair := True;
//  if DeveSair then
//    Exit;
//
//  for I := 0 to AFiles.Count -1 do
//  begin
//    FileNameOut := FDir + ExtractFileName(AFiles[i]);
//    try
//      Writer := InitOut(FileNameOut);
//      try
//        ProcessUnit(AFiles[i], True);
//      finally
//        Writer.Free;
//        Writer := nil;
//      end;
//      FreeStringWriterList;
//    except
//      on e: exception do
//        Logger.Error('TDcuDecompiler.ProcessFile[%s]: %s', [AFiles[i], E.Message]);
//    end;
//  end;
end;

end.

