unit FindUnit.IncluderHandlerInc;

interface

uses
  IOUtils, Classes, Generics.Collections, SimpleParser.Lexer.Types;

type
  TIncItem = record
    Loaded: Boolean;
    Content: string;
    FilePath: string;
  end;

  TIncludeHandlerInc = class(TInterfacedObject, IIncludeHandler)
  private
    FIncList: TObjectDictionary<string, TIncItem>;
    FPossiblePaths: TStringList;

    procedure GenerateIncList;
  public
    constructor Create(PossiblePaths: string);
    destructor Destroy; override;

    procedure Process;

    function GetIncludeFileContent(const FileName: string): string;
  end;


implementation

uses
  FindUnit.Utils, SysUtils, Log4Pascal;

{ TIncludeHandlerInc }

constructor TIncludeHandlerInc.Create(PossiblePaths: string);
begin
  FIncList := TObjectDictionary<string, TIncItem>.Create;
  FPossiblePaths := TStringList.Create;
  FPossiblePaths.Text := PossiblePaths;
end;

destructor TIncludeHandlerInc.Destroy;
begin
  FPossiblePaths.Free;
  FIncList.Free;
  inherited;
end;

procedure TIncludeHandlerInc.GenerateIncList;
var
  I: Integer;
  ItemPath: string;
  Return: TStringList;
  iRet: Integer;
  FileName: string;
  FilePath: string;
  IncItem: TIncItem;
begin
  for I := 0 to FPossiblePaths.Count -1 do
  begin
    ItemPath := IncludeTrailingPathDelimiter(FPossiblePaths[i]);
    try
      Return := GetAllFilesFromPath(ItemPath, '*.inc');

      for iRet := 0 to Return.Count -1 do
      begin
        FilePath := Return[iRet];
        FileName := UpperCase(ExtractFileName(FilePath));

        IncItem.Loaded := False;
        IncItem.Content := '';
        IncItem.FilePath := FilePath;

        FIncList.AddOrSetValue(FileName, IncItem);
      end;
      Return.Free
    except
      on e: exception do
        Logger.Error('TIncludeHandlerInc.GenerateIncList[%s]: %s', [ItemPath, e.Message]);
    end;
  end;
end;

function TIncludeHandlerInc.GetIncludeFileContent(const FileName: string): string;
var
  ItemInc: TIncItem;
  FileInc: TStringList;
begin
  Result := '';
  if not FIncList.TryGetValue(UpperCase(FileName), ItemInc) then
    Exit;

  if ItemInc.Loaded then
  begin
    Result := ItemInc.Content;
    Exit;
  end;

  FileInc := TStringList.Create;
  try
    try
      FileInc.LoadFromFile(ItemInc.FilePath);
      ItemInc.Content := FileInc.Text;
      ItemInc.Loaded := True;
    except
    end;
    Result := ItemInc.Content;
  finally
    FileInc.Free;
  end;
end;

procedure TIncludeHandlerInc.Process;
begin
  GenerateIncList;
end;

end.
