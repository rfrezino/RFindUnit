unit FindUnit.Utils;

interface

uses
  SimpleParser.Lexer.Types,
  System.Generics.Collections,
  System.StrUtils;

type
  TFileInfo = record
    Path: string;
    LastAccess: TDateTime;
  end;

  TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
  private
    FPath: string;
  public
    constructor Create(const Path: string);
    function GetIncludeFileContent(const FileName: string): string;
  end;

  TPathConverter = class(TObject)
  private const
    VAR_INIT = '$(';
    VAR_END = ')';
  public
    class function ConvertPathsToFullPath(Paths: string): string;
  end;

function GetAllFilesFromPath(const Path, Filter: string): TDictionary<string, TFileInfo>;
function GetAllPasFilesFromPath(const Path: string): TDictionary<string, TFileInfo>;
function GetAllDcuFilesFromPath(const Path: string): TDictionary<string, TFileInfo>;

function Fetch(var AInput: string; const ADelim: string = ''; const ADelete: Boolean = True;
  const ACaseSensitive: Boolean = False): string; inline;

function IsProcessRunning(const AExeFileName: string): Boolean;
function GetHashCodeFromStr(Str: PChar): Integer;
function TextExists(SubStr, Str: string; CaseSensitive: Boolean = true): Boolean; inline;

procedure GetUnitFromSearchSelection(SearchSelection: string; out UnitName, ClassName: string);

function DictionaryToString(Dir: TDictionary<string, string>): string;

var
  FindUnitDir: string;
  FindUnitDirLogger: string;
  FindUnitDcuDir: string;
  DirRealeaseWin32: string;

implementation

uses
  Log4PAscal,
  Winapi.TlHelp32,
  Winapi.Windows,
  System.IOUtils,
  System.Classes,
  System.SysUtils, System.Types;

function DictionaryToString(Dir: TDictionary<string, string>): string;
var
  Value: string;
begin
  Result := '';

  if Dir = nil then
    Exit;

  for Value in Dir.Values do
    Result := Result + Value + ',';
end;

procedure GetUnitFromSearchSelection(SearchSelection: string; out UnitName, ClassName: string);
var
  IsSetEnumItem: Boolean;
begin
  IsSetEnumItem := SearchSelection.EndsWith(' item');

  UnitName := SearchSelection;
  if Pos('.*', UnitName) > 0 then
    UnitName := Trim(Fetch(UnitName, '.*'))
  else
    UnitName := Trim(Fetch(UnitName, '-'));
  UnitName := ReverseString(UnitName);
  ClassName := Fetch(UnitName,'.');

  if IsSetEnumItem then
    ClassName := Fetch(UnitName,'.');

  ClassName := ReverseString(ClassName);
  UnitName := ReverseString(UnitName);
end;

function TextExists(SubStr, Str: string; CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := Pos(SubStr, Str) > 0
  else
    Result := Pos(UpperCase(SubStr), UpperCase(Str)) > 0
end;

function GetHashCodeFromStr(Str: PChar): Integer;
var
  Off, Len, Skip, I: Integer;
begin
  Result := 0;
  Off := 1;
  Len := StrLen(Str);
  if Len < 16 then
    for I := (Len - 1) downto 0 do
    begin
      Result := (Result * 37) + Ord(Str[Off]);
      Inc(Off);
    end
  else
  begin
    { Only sample some characters }
    Skip := Len div 8;
    I := Len - 1;
    while I >= 0 do
    begin
      Result := (Result * 39) + Ord(Str[Off]);
      Dec(I, Skip);
      Inc(Off, Skip);
    end;
  end;
end;

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

function FetchCaseInsensitive(var AInput: string; const ADelim: string; const ADelete: Boolean): string; inline;
var
  LPos: Integer;
begin
  if ADelim = #0 then
  begin
    LPos := Pos(ADelim, AInput);
  end
  else
  begin
    LPos := Pos(UpperCase(ADelim), UpperCase(AInput));
  end;
  if LPos = 0 then
  begin
    Result := AInput;
    if ADelete then
    begin
      AInput := '';
    end;
  end
  else
  begin
    Result := Copy(AInput, 1, LPos - 1);
    if ADelete then
      AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
  end;
end;

function Fetch(var AInput: string; const ADelim: string = ''; const ADelete: Boolean = True;
  const ACaseSensitive: Boolean = False): string; inline;
var
  LPos: Integer;
begin
  if ACaseSensitive then
  begin
    LPos := Pos(ADelim, AInput);
    if LPos = 0 then
    begin
      Result := AInput;
      if ADelete then
        AInput := '';
    end
    else
    begin
      Result := Copy(AInput, 1, LPos - 1);
      if ADelete then
      begin
        // slower Delete(AInput, 1, LPos + Length(ADelim) - 1); because the
        // remaining part is larger than the deleted
        AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
      end;
    end;
  end
  else
  begin
    Result := FetchCaseInsensitive(AInput, ADelim, ADelete);
  end;
end;

function GetAllFilesFromPath(const Path, Filter: string): TDictionary<string, TFileInfo>;
var
  Files: TStringDynArray;
  FilePath: string;
  FileInfo: TFileInfo;
begin
  Files := System.IOUtils.TDirectory.GetFiles(Path, Filter, TSearchOption.soTopDirectoryOnly);

  Result := TDictionary<string, TFileInfo>.Create;
  for FilePath in Files do
  begin
    FileInfo.Path := Trim(FilePath);
    if FileExists(FilePath) then
      FileInfo.LastAccess := System.IOUtils.TFile.GetLastWriteTime(FilePath)
    else
      FileInfo.LastAccess := 0;

    Result.Add(FileInfo.Path, FileInfo);
  end;
end;

function GetAllDcuFilesFromPath(const Path: string): TDictionary<string, TFileInfo>;
begin
  Result := GetAllFilesFromPath(Path, '*.dcu');
end;

function GetAllPasFilesFromPath(const Path: string): TDictionary<string, TFileInfo>;
begin
  Result := GetAllFilesFromPath(Path, '*.pas');
end;

{ TIncludeHandler }

constructor TIncludeHandler.Create(const Path: string);
begin
  inherited Create;
  FPath := Path;
end;

function TIncludeHandler.GetIncludeFileContent(const FileName: string): string;
var
  FileContent: TStringList;
begin
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(TPath.Combine(FPath, FileName));
    Result := FileContent.Text;
  finally
    FileContent.Free;
  end;
end;

procedure CarregarPaths;
begin
  FindUnitDir := GetEnvironmentVariable('APPDATA') + '\DelphiFindUnit\';
  FindUnitDirLogger := FindUnitDir + 'Logger\';
  FindUnitDcuDir := FindUnitDir + IntToStr(GetHashCodeFromStr(PChar(ParamStr(0)))) + '\';
  FindUnitDcuDir := FindUnitDcuDir + 'DecompiledDcus\';

  ForceDirectories(FindUnitDir);
  ForceDirectories(FindUnitDcuDir);
  ForceDirectories(FindUnitDirLogger);

  DirRealeaseWin32 := ExtractFilePath(ParamStr(0));
  DirRealeaseWin32 := StringReplace(DirRealeaseWin32,'\bin','\lib\win32\release',[rfReplaceAll, rfIgnoreCase]);
end;

{ TPathConverter }

class function TPathConverter.ConvertPathsToFullPath(Paths: string): string;
var
  CurVariable: string;
  CurPaths: string;
  FullPath: string;
begin
  while Pos(VAR_INIT, Paths) > 0 do
  begin
    CurPaths := Paths;
    Fetch(CurPaths, VAR_INIT);
    CurVariable := Fetch(CurPaths, VAR_END, False);

    Logger.Debug('TPathConverter.ConvertPathsToFullPath: %s', [CurVariable]);

    FullPath := GetEnvironmentVariable(CurVariable);

    Paths := StringReplace(Paths, VAR_INIT + CurVariable + VAR_END, FullPath, [rfReplaceAll]);
  end;

  Result := Paths;
end;

initialization

CarregarPaths;

end.
