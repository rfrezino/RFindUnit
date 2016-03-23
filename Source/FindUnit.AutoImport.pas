unit FindUnit.AutoImport;

interface

uses
	IniFiles, SysUtils, Log4Pascal, Classes;

type
  TAutoImport = class(TObject)
  private
    FIniFilePath: string;
    FSearchMemory: TIniFile;

    procedure LoadIniFile;
    procedure SaveIniFile;
    function LoadClassesToImport: TStringList;
  public
    constructor Create(IniFilePath: string);
    destructor Destroy; override;

    procedure Load;

    function GetMemorizedUnit(Search: string; out MemUnit: string): Boolean;
    procedure SetMemorizedUnit(NameClass, MemUnit: string);

    function LoadUnitListToImport: TStringList;
  end;

implementation

uses
  FindUnit.OTAUtils, ToolsAPI, FindUnit.Utils;

const
  SECTION = 'MEMORIZEDUNIT';

{ TAutoImport }

constructor TAutoImport.Create(IniFilePath: string);
begin
  inherited Create;
  FIniFilePath := IniFilePath;
end;

destructor TAutoImport.Destroy;
begin

  inherited;
end;

function TAutoImport.GetMemorizedUnit(Search: string; out MemUnit: string): Boolean;
begin
  Result := False;

  Search := UpperCase(Search);
  MemUnit := FSearchMemory.ReadString(SECTION, Search, '');
  Result := MemUnit <> '';
end;

function TAutoImport.LoadUnitListToImport: TStringList;
var
  ClassesList: TStringList;
  ClassItem: string;
  UnitItem: string;
begin
  Result := TStringList.Create;
  ClassesList :=  LoadClassesToImport;
  try
    for ClassItem in ClassesList do
    begin
      UnitItem := FSearchMemory.ReadString(SECTION, ClassItem, '');
      if UnitItem <> '' then
        Result.Add(UnitItem);
    end;
  finally
    ClassesList.Free;
  end;
end;

procedure TAutoImport.Load;
begin
  LoadIniFile;
end;

procedure TAutoImport.LoadIniFile;
var
  DirPath: string;
begin
  Logger.Debug('TAutoImport.LoadIniFile: %s', [FIniFilePath]);
  DirPath := ExtractFilePath(FIniFilePath);
  ForceDirectories(DirPath);
  FSearchMemory := TIniFile.Create(FIniFilePath);
end;

function TAutoImport.LoadClassesToImport: TStringList;
var
  Errors: TOTAErrors;
  ErrorItem: TOTAError;
  Item: string;

  function GetUndeclaredIdentifier(Text: string): string;
  const
    ERROR_CODE = 'Undeclared identifier';
  begin
    Result := '';
    if Pos(ERROR_CODE, Text) = 0 then
      Exit;

    Fetch(Text, #39);
    Result := Fetch(Text, #39);
  end;
begin
  Result := TStringList.Create;
  Errors := GetErrorListFromActiveModule;
  for ErrorItem in Errors do
  begin
    Item := GetUndeclaredIdentifier(ErrorItem.Text);
    if Item = '' then
      Continue;

    Result.Add(UpperCase(Item));
  end
end;

procedure TAutoImport.SaveIniFile;
begin
  FSearchMemory.UpdateFile;
end;

procedure TAutoImport.SetMemorizedUnit(NameClass, MemUnit: string);
begin
  NameClass := UpperCase(NameClass);
  FSearchMemory.WriteString(SECTION, NameClass, MemUnit);
  SaveIniFile;
end;

end.
