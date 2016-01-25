unit FindUnit.Utils;

interface

uses
  IOUtils, Classes, SimpleParser.Lexer.Types;

type
  TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
  private
    FPath: string;
  public
    constructor Create(const Path: string);
    function GetIncludeFileContent(const FileName: string): string;
  end;

  TPathConverter = class(TObject)
  private
    const
      VAR_INIT = '$(';
      VAR_END = ')';
  public
    class function ConvertPathsToFullPath(Paths: string): string;
  end;

  function GetAllFilesFromPath(Path, Filter: string): TStringList;
  function GetAllPasFilesFromPath(Path: string): TStringList;

  function Fetch(var AInput: string; const ADelim: string = '';
    const ADelete: Boolean = True; const ACaseSensitive: Boolean = False): string; inline;

  var
    FindUnitDir: string;
    FindUnitDirLogger: string;

implementation

uses
  Types, SysUtils, Log4PAscal;

function FetchCaseInsensitive(var AInput: string; const ADelim: string;
  const ADelete: Boolean): string; inline;
var
  LPos: Integer;
begin
  if ADelim = #0 then begin
    // AnsiPos does not work with #0
    LPos := Pos(ADelim, AInput);
  end else begin
    //? may be AnsiUpperCase?
    LPos := Pos(UpperCase(ADelim), UpperCase(AInput));
  end;
  if LPos = 0 then begin
    Result := AInput;
    if ADelete then begin
      AInput := '';    {Do not Localize}
    end;
  end else begin
    Result := Copy(AInput, 1, LPos - 1);
    if ADelete then begin
      //faster than Delete(AInput, 1, LPos + Length(ADelim) - 1); because the
      //remaining part is larger than the deleted
      AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
    end;
  end;
end;

function Fetch(var AInput: string; const ADelim: string = '';
  const ADelete: Boolean = True;
  const ACaseSensitive: Boolean = False): string; inline;
var
  LPos: Integer;
begin
  if ACaseSensitive then begin
    if ADelim = #0 then begin
      // AnsiPos does not work with #0
      LPos := Pos(ADelim, AInput);
    end else begin
      LPos := Pos(ADelim, AInput);
    end;
    if LPos = 0 then begin
      Result := AInput;
      if ADelete then begin
        AInput := '';    {Do not Localize}
      end;
    end
    else begin
      Result := Copy(AInput, 1, LPos - 1);
      if ADelete then begin
        //slower Delete(AInput, 1, LPos + Length(ADelim) - 1); because the
        //remaining part is larger than the deleted
        AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
      end;
    end;
  end else begin
    Result := FetchCaseInsensitive(AInput, ADelim, ADelete);
  end;
end;


function GetAllFilesFromPath(Path, Filter: string): TStringList;
var
  Files: TStringDynArray;
  FilePath: string;
begin
  Files := IOUtils.TDirectory.GetFiles(Path, Filter, TSearchOption.soAllDirectories);

  Result := TStringList.Create;
  for FilePath in Files do
    Result.Add(FilePath);
end;

function GetAllPasFilesFromPath(Path: string): TStringList;
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

  CreateDir(FindUnitDir);
  CreateDir(FindUnitDirLogger);
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
