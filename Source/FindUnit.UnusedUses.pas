unit FindUnit.UnusedUses;

interface

uses
  DelphiAST,
  Log4Pascal,

  DelphiAST.Classes,
  DelphiAST.Consts,
  DelphiAST.Writer,

  FindUnit.DelphiReservedWords,
  FindUnit.PasParser,
  FindUnit.Utils,

  SimpleParser.Lexer.Types,

  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  FindUnit.Settings,
  Interf.EnvironmentController;

type
  TUsesUnit = record
    Line: Integer;
    Collumn: Integer;
    Name: string;
  end;

  TUnsedUsesProcessor = class(TObject)
  private
    FFilePath: string;
    FIncluder: IIncludeHandler;
    FUnitNode: TSyntaxNode;
    FEnvControl: IRFUEnvironmentController;

    FUses: TDictionary<string, TUsesUnit>;
    FUsedTypes: TDictionary<string, string>;
    FIgnoredTypes: TDictionary<string, string>;
    FIgnoredUses: TDictionary<string, string>;
    FMatches: TDictionary<string, string>;
    FUnusedUses: TDictionary<string, TUsesUnit>;

    function GetUsedTypes: TDictionary<string, string>;
    function GetUses: TDictionary<string, TUsesUnit>;
    function GetFullMatchsForUses: TDictionary<string, string>;
    function GetIgnoredTypes: TDictionary<string, string>;
    function GetIgnoredUses: TDictionary<string, string>;
    function GetUnusedUses: TDictionary<string, TUsesUnit>;
  public
    constructor Create(AFilePath: string);
    destructor Destroy; override;

    procedure Process;

    procedure SetIncluder(Includer: IIncludeHandler);
    procedure SetEnvControl(EnvControl: IRFUEnvironmentController);

    function GetUnusedUsesAsString: string;

    property UnusedUses: TDictionary<string, TUsesUnit> read FUnusedUses;
  end;

implementation

uses
  Vcl.Dialogs;

{ TUnsedUsesProcessor }

constructor TUnsedUsesProcessor.Create(AFilePath: string);
begin
  FUnusedUses := TDictionary<string, TUsesUnit>.Create;
  FFilePath := AFilePath;
end;

destructor TUnsedUsesProcessor.Destroy;
begin
  FUses.Free;
  FUsedTypes.Free;
  FIgnoredTypes.Free;
  FIgnoredUses.Free;
  FMatches.Free;
  FUnusedUses.Free;
  inherited;
end;

function TUnsedUsesProcessor.GetFullMatchsForUses: TDictionary<string, string>;
var
  SearchType: string;
  Matches: TStringList;
  NewItem: string;
begin
  Result := TDictionary<string, string>.Create;

  if FEnvControl = nil then
    Exit;

  for SearchType in FUsedTypes.Values do
  begin
    Logger.Debug('Search for:' + SearchType);
    Matches := FEnvControl.GetFullMatch(SearchType);
    Logger.Debug('Found : ' + Matches.CommaText);
    for NewItem in Matches do
      Result.AddOrSetValue(NewItem.ToUpper, NewItem);
    Matches.Free;
  end;
end;

function TUnsedUsesProcessor.GetIgnoredTypes: TDictionary<string, string>;
var
  ReservedWords: TStringList;
  Rword: string;
begin
  Result := TDictionary<string, string>.Create;
  ReservedWords := TDelphiReservedWords.GetReservedWords;
  for Rword in ReservedWords do
    Result.Add(UpperCase(Rword), Rword);
  ReservedWords.Free;
end;

function TUnsedUsesProcessor.GetIgnoredUses: TDictionary<string, string>;
var
  IgnoredUses: TStringList;
  IgnoreUse: string;
begin
  Result := TDictionary<string, string>.Create;
  IgnoredUses := TStringList.Create;
  IgnoredUses.CommaText := GlobalSettings.IgnoreUsesUnused;
  for IgnoreUse in IgnoredUses do
    Result.Add(UpperCase(IgnoreUse), IgnoreUse);
  IgnoredUses.Free;
end;

function TUnsedUsesProcessor.GetUnusedUses: TDictionary<string, TUsesUnit>;
var
  Matches: string;
  UseFound: TPair<string, TUsesUnit>;
  UnitNameEx: string;
  ClassNameEx: string;
begin
  Result := TDictionary<string, TUsesUnit>.Create;
  for UseFound in FUses do
    Result.Add(UseFound.Key, UseFound.Value);

  for Matches in FMatches.Values do
  begin
    FindUnit.Utils.GetUnitFromSearchSelection(Matches, UnitNameEx, ClassNameEx);
    Logger.Debug('Remove used unit ' + UnitNameEx);
    Result.Remove(UnitNameEx.ToUpper);
  end;
end;

function TUnsedUsesProcessor.GetUnusedUsesAsString: string;
var
  Value: TUsesUnit;
begin
  Result := '';

  if FUnusedUses = nil then
    Exit;

  for Value in FUnusedUses.Values do
    Result := Result + Value.Name + ',';
end;

function TUnsedUsesProcessor.GetUsedTypes: TDictionary<string, string>;
var
  XmlFile: TStringList;
  Line: string;
  FetchType: string;
  I: Integer;
begin
  Result := TDictionary<string, string>.Create;

  if FUnitNode = nil then
    Exit;

  XmlFile := TStringList.Create;
  XmlFile.Text := TSyntaxTreeWriter.ToXML(FUnitNode, True);

  for I := 0 to XmlFile.Count - 1 do
  begin
    Line := XmlFile[I];

    if (Pos('<TYPE', Line) = 0) and (Pos('<NAME', Line) = 0) then
      Continue;

    FetchType := Fetch(Line, 'name="');
    FetchType := Fetch(Line, '"');

    if FIgnoredTypes.ContainsKey(FetchType.ToUpper) then
      Continue;

    Result.AddOrSetValue(FetchType.ToUpper, FetchType);
  end;
  XmlFile.Free;
end;

function TUnsedUsesProcessor.GetUses: TDictionary<string, TUsesUnit>;
var
  XmlFile: TStringList;
  Line: string;
  FetchType: string;
  I: Integer;
  UsesUnit: TUsesUnit;
  Column: string;
  UsesLine: string;
  UsesName: string;
begin
  Result := TDictionary<string, TUsesUnit>.Create;

  if FUnitNode = nil then
    Exit;

  XmlFile := TStringList.Create;
  XmlFile.Text := TSyntaxTreeWriter.ToXML(FUnitNode, True);

  for I := 0 to XmlFile.Count - 1 do
  begin
    Line := XmlFile[I];

    if Pos('<UNIT', Line) = 0 then
      Continue;

    UsesLine := Line;
    Column := Line;
    UsesName := Line;

    Fetch(UsesLine,'line="');
    UsesLine := Fetch(UsesLine, '"');

    Fetch(Column,'col="');
    Column := Fetch(Column, '"');

    Fetch(UsesName, 'name="');
    UsesName := Fetch(UsesName, '"');

    UsesUnit.Line := StrToInt(UsesLine);
    UsesUnit.Collumn := StrToInt(Column);
    UsesUnit.Name := UsesName;

    Result.AddOrSetValue(UsesUnit.Name.ToUpper, UsesUnit);
  end;
  XmlFile.Free;
end;

procedure TUnsedUsesProcessor.Process;
var
  Step: string;
begin
  FUnitNode := nil;
  if not FileExists(FFilePath) then
  begin
    Logger.Debug('TFindUnitParser.Process: File do not exists %s', [FFilePath]);
    Exit;
  end;

  try
    try
      FUnitNode := TPasSyntaxTreeBuilder.Run(FFilePath, True, nil);
    except
      on E: ESyntaxTreeException do
      begin
        FUnitNode := E.SyntaxTree;
        E.SyntaxTree := nil;
      end;
    end;

    if FUnitNode = nil then
    begin
      Exit;
    end;

    FIgnoredTypes := GetIgnoredTypes;
    FIgnoredUses := GetIgnoredUses;
    FUses := GetUses;
    FUsedTypes := GetUsedTypes;
    FMatches := GetFullMatchsForUses;

    FUnusedUses.Free;
    FUnusedUses := GetUnusedUses;
    Logger.Debug('GetUnusedUses:' + GetUnusedUsesAsString);
  except
    on E: Exception do
    begin
      Logger.Error('TFindUnitParser.Process: Trying to parse %s. Msg: %s | Step: %s', [FFilePath, E.Message, Step]);
      {$IFDEF RAISEMAD} raise; {$ENDIF}
    end;
  end;
end;

procedure TUnsedUsesProcessor.SetEnvControl(EnvControl: IRFUEnvironmentController);
begin
  FEnvControl := EnvControl;
end;

procedure TUnsedUsesProcessor.SetIncluder(Includer: IIncludeHandler);
begin
  FIncluder := Includer;
end;

end.
