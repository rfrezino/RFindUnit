unit FindUnit.DcuDecompiler;

interface

uses
  Classes, DCU32, FindUnit.Utils;

type
  TDcuDecompiler = class(TObject)
  private
    FDir: string;
    FFiles: TStringList;

    procedure CreateDirs;
    function ProcessUnit(FileName: string; OutRedir: boolean): Integer;
    procedure ProcessFiles(AFiles: TStringList);

    procedure ProcessFilesFromExe(AFiles: TStringList);
  public
    constructor Create(AFiles: TStringList; ADirOutPutPath: string);
  end;

implementation

uses
  SysUtils, DCU_Out, DcuTbl {$IFDEF UNICODE}, AnsiStrings{$ENDIF}, Log4Pascal;

{ TDcuDecompiler }

procedure TDcuDecompiler.CreateDirs;
begin
  FDir := FindUnitDir + 'DecompiledDcus\';
  CreateDir(FDir);
end;

function TDcuDecompiler.ProcessUnit(FileName: string; OutRedir: boolean): Integer;
var
  UnitFromDcu: TUnit;
begin
  Result := 0;
  try
    FileName := ExpandFileName(FileName);
    UnitFromDcu := nil;
    try
      UnitFromDcu := GetDCUByName(FileName, '', 0, false, dcuplWin32, 0);
    finally
      if UnitFromDcu = nil then
        UnitFromDcu := MainUnit;
      if UnitFromDcu <> nil then
        UnitFromDcu.Show;
    end;
  except
    on E: Exception do
    begin
      Result := 1;
    end;
  end;
  UnitFromDcu.Free;
end;

constructor TDcuDecompiler.Create(AFiles: TStringList; ADirOutPutPath: string);
begin
  CreateDirs;
  ProcessFiles(AFiles);
end;

procedure TDcuDecompiler.ProcessFiles(AFiles: TStringList);
var
  I: Integer;
  FileNameOut: string;
  DeveSair: Boolean;
begin
  {I'not using this method 'cause there are to much leaks on dcu32int that by now
  make it unusable on a single exe'}
  DeveSair := True;
  if DeveSair then
    Exit;

  for I := 0 to AFiles.Count -1 do
  begin
    FileNameOut := FDir + ExtractFileName(AFiles[i]);
    try
      Writer := InitOut(FileNameOut);
      try
        ProcessUnit(AFiles[i], True);
      finally
        Writer.Free;
        Writer := nil;
      end;
      FreeStringWriterList;
    except
      on e: exception do
        Logger.Error('TDcuDecompiler.ProcessFile[%s]: %s', [AFiles[i], E.Message]);
    end;
  end;
end;

procedure TDcuDecompiler.ProcessFilesFromExe(AFiles: TStringList);
begin

end;

end.

