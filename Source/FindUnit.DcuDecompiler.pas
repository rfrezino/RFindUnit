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
  public
    constructor Create(AFiles: TStringList; ADirOutPutPath: string);
  end;

implementation

uses
  SysUtils, DCU_Out, DcuTbl {$IFDEF UNICODE}, AnsiStrings{$ENDIF};

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
end;

constructor TDcuDecompiler.Create(AFiles: TStringList; ADirOutPutPath: string);
var
  I: Integer;
  FileNameOut: string;
begin
  CreateDirs;
  for I := 0 to AFiles.Count - 1 do
  begin
    FileNameOut := FDir + ExtractFileName(AFiles[i]);
    Writer := InitOut(FileNameOut);
    try
      ProcessUnit(AFiles[i], True);
    finally
      Writer.Free;
    end;
  end;
end;

end.

