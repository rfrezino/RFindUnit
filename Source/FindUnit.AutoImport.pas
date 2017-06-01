unit FindUnit.AutoImport;

interface

uses
  Classes,
  IniFiles,
  Log4Pascal,
  SysUtils,
  ToolsAPI,

  FindUnit.Header,
  FindUnit.OTAUtils,
  FindUnit.StringPositionList,
  FindUnit.Utils;

type
  TAutoImport = class(TObject)
  private
    FIniFilePath: string;
    FSearchMemory: TIniFile;

    procedure LoadIniFile;
    procedure SaveIniFile;

    function LoadClassesToImport: TStringPositionList;
  public
    constructor Create(const IniFilePath: string);
    destructor Destroy; override;

    procedure Load;

    function GetMemorizedUnit(Search: string; out MemUnit: string): Boolean;
    procedure SetMemorizedUnit(NameClass, MemUnit: string);

    function LoadUnitListToImport: TStringPositionList;
  end;

implementation

const
  SECTION = 'MEMORIZEDUNIT';

{ TAutoImport }

constructor TAutoImport.Create(const IniFilePath: string);
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
  Search := UpperCase(Search);
  MemUnit := FSearchMemory.ReadString(SECTION, Search, '');
  Result := MemUnit <> '';
end;

function TAutoImport.LoadUnitListToImport: TStringPositionList;
var
  ClassesList: TStringPositionList;
  ClassItem: TStringPosition;
  UnitItem: string;
  ItemReturn: TStringPosition;
begin
  Result := TStringPositionList.Create;
  Result.Duplicates := dupIgnore;

  ClassesList := LoadClassesToImport;
  try
    for ClassItem in ClassesList do
    begin
      UnitItem := FSearchMemory.ReadString(SECTION, ClassItem.Value, '');
      if UnitItem <> '' then
      begin
        ItemReturn.Value := UnitItem;
        ItemReturn.Line := ClassItem.Line;
        Result.Add(ItemReturn);
      end;
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

function TAutoImport.LoadClassesToImport: TStringPositionList;
var
  Errors: TOTAErrors;
  ErrorItem: TOTAError;
  Item: TStringPosition;

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
  Result := TStringPositionList.Create;
  Errors := GetErrorListFromActiveModule;
  for ErrorItem in Errors do
  begin
    Item.Value := UpperCase(GetUndeclaredIdentifier(ErrorItem.Text));
    if Item.Value = '' then
      Continue;

    Item.Line := ErrorItem.Start.Line;
    Result.Add(Item);
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
