unit FindUnit.DcuDecompiler;

interface

uses
  Classes, FindUnit.Utils, Windows, ShellAPI;

const
  DCU32INT_EXECUTABLE = 'dcu32int.exe';

type
  TDcuDecompiler = class(TObject)
  private
    FDir: string;
    FFiles: TStringList;
    FExecutablePath: string;

    procedure CreateDirs;
    function ProcessUnit(FileName: string; OutRedir: boolean): Integer;

    procedure ProcessFilesInternal(AFiles: TStringList);
    procedure ProcessFilesFromExe(AFiles: TStringList);
  public
    constructor Create;

    procedure ProcessFiles(AFiles: TStringList);

    function Dcu32IntExecutableExists: Boolean;
  end;

  function Dcu32IntExecutableExists: Boolean;
  function GetDcu32ExecutablePath: string;

implementation

uses
  SysUtils{$IFDEF UNICODE}, AnsiStrings{$ENDIF}, Log4Pascal;

{ TDcuDecompiler }

function GetDcu32ExecutablePath: string;
var
  Dcu: TDcuDecompiler;
begin
  Dcu := TDcuDecompiler.Create;
  Result := Dcu.FDir;
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
  CreateDirs;
end;

procedure TDcuDecompiler.ProcessFiles(AFiles: TStringList);
begin
//  ProcessFilesInternal(AFiles);
  ProcessFilesFromExe(AFiles);
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


    try
      InputParams := Format('"%s" "-I" "-X%s"', [AFiles[i], FileNameOut]);
      ShellExecute(0, 'open', PChar(FExecutablePath), PChar(InputParams), nil, SW_HIDE);
    except
      on e: exception do
        Logger.Error('TDcuDecompiler.ProcessFile[%s]: %s', [AFiles[i], E.Message]);
    end;
  end;

end;

procedure TDcuDecompiler.ProcessFilesInternal(AFiles: TStringList);
var
  I: Integer;
  FileNameOut: string;
  DeveSair: Boolean;
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

