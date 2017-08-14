unit FindUnit.UnusedUses;

interface

uses
  DelphiAST,
  Log4Pascal,

  DelphiAST.Classes,
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
  TUnusedErrorType = (uetUnused, uetNoPasFile);

  TUsesUnit = record
    Line: Integer;
    Collumn: Integer;
    Name: string;
    UnusedType: TUnusedErrorType;
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

    FOptionalUsesPrefix: TStringList;
    FUsesStartLine: Integer;

    function GetUsedTypes: TDictionary<string, string>;
    function GetUnitSpecifiedOnUses: TDictionary<string, TUsesUnit>;
    function GetFullMatchsForUses: TDictionary<string, string>;
    function GetIgnoredTypes: TDictionary<string, string>;
    function GetIgnoredUses: TDictionary<string, string>;
    function GetUnusedUses: TDictionary<string, TUsesUnit>;
    function GetOptionalUsesPrefix: TStringList;
  public
    constructor Create(AFilePath: string);
    destructor Destroy; override;

    procedure Process;

    procedure SetIncluder(Includer: IIncludeHandler);
    procedure SetEnvControl(EnvControl: IRFUEnvironmentController);

    function GetUnusedUsesAsString: string;

    property UnusedUses: TDictionary<string, TUsesUnit> read FUnusedUses;
    property UsesStartLine: Integer read FUsesStartLine write FUsesStartLine;
  end;

implementation

uses
  FindUnit.Header;

{ TUnsedUsesProcessor }

constructor TUnsedUsesProcessor.Create(AFilePath: string);
begin
  FUnusedUses := TDictionary<string, TUsesUnit>.Create;
  FFilePath := AFilePath;
  FUsesStartLine := -1;
end;

destructor TUnsedUsesProcessor.Destroy;
begin
  FUses.Free;
  FUsedTypes.Free;
  FIgnoredTypes.Free;
  FIgnoredUses.Free;
  FMatches.Free;
  FUnusedUses.Free;
  FOptionalUsesPrefix.Free;
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
    Matches := FEnvControl.GetFullMatch(SearchType);
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
  OptionalUses: string;
  AllPossibleMatches: TDictionary<string, string>;
  UpMatchs: string;
  PrefixVariation: string;
begin
  Result := TDictionary<string, TUsesUnit>.Create;
  AllPossibleMatches := TDictionary<string, string>.Create;

  for Matches in FMatches.Values do
  begin
    if not vSystemRunning then
      Exit;

    FindUnit.Utils.GetUnitFromSearchSelection(Matches, UnitNameEx, ClassNameEx);

    UpMatchs := UnitNameEx.ToUpper;
    AllPossibleMatches.AddOrSetValue(UpMatchs, UnitNameEx);

    for OptionalUses in FOptionalUsesPrefix do
    begin
      AllPossibleMatches.AddOrSetValue(OptionalUses + UpMatchs, Matches);

      PrefixVariation := UpMatchs.Replace(OptionalUses, '');
      AllPossibleMatches.AddOrSetValue(PrefixVariation, UnitNameEx);
    end;
  end;

  for UseFound in FUses do
    if not AllPossibleMatches.ContainsKey(UseFound.Key) then
      if not FIgnoredUses.ContainsKey(UseFound.Key) then
        Result.Add(UseFound.Key, UseFound.Value);

  AllPossibleMatches.Free;
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

    if (Pos('<TYPE', Line) = 0)
      and (Pos('<NAME', Line) = 0)
      and (Pos('<IDENTIFIER', Line) = 0) then
      Continue;

    FetchType := Fetch(Line, 'name="');
    FetchType := Fetch(Line, '"');

    if FetchType.Length <= 2 then
      Continue;

    if FIgnoredTypes.ContainsKey(FetchType.ToUpper) then
      Continue;

    if not FetchType.IsEmpty then
      Result.AddOrSetValue(FetchType.ToUpper, FetchType);
  end;
  XmlFile.Free;
end;

function TUnsedUsesProcessor.GetUnitSpecifiedOnUses: TDictionary<string, TUsesUnit>;
var
  XmlFile: TStringList;
  Line: string;
  I: Integer;
  UsesUnit: TUsesUnit;
  Column: string;
  UsesLine: string;
  UsesName: string;
  IsHeader: Boolean;
begin
  IsHeader := True;
  Result := TDictionary<string, TUsesUnit>.Create;

  if FUnitNode = nil then
    Exit;

  XmlFile := TStringList.Create;
  XmlFile.Text := TSyntaxTreeWriter.ToXML(FUnitNode, True);

  for I := 0 to XmlFile.Count - 1 do
  begin
    Line := XmlFile[I];

    if (FUsesStartLine = -1) and Line.Contains('<USES') then
    begin
      Fetch(Line,'begin_line="');
      UsesLine := Fetch(Line, '"');
      FUsesStartLine := StrToInt(UsesLine);
      Continue;
    end;

    if Pos('<UNIT', Line) = 0 then
      Continue;

    if IsHeader then
    begin
      IsHeader := False;
      Continue;
    end;

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

    if FEnvControl.PasExists(UsesUnit.Name.ToUpper + '.pas') then
      UsesUnit.UnusedType := uetUnused
    else
       UsesUnit.UnusedType := uetNoPasFile;

    Result.AddOrSetValue(UsesUnit.Name.ToUpper, UsesUnit);
  end;
  XmlFile.Free;
end;

function TUnsedUsesProcessor.GetOptionalUsesPrefix: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('WINDOWS.');
  Result.Add('MESSAGES.');
  Result.Add('SYSUTILS.');
  Result.Add('VARIANTS.');
  Result.Add('CLASSES.');
  Result.Add('GRAPHICS.');
  Result.Add('CONTROLS.');
  Result.Add('FORMS.');
  Result.Add('DIALOGS.');
  Result.Add('TYPES.');
  Result.Add('WINAPI.');
  Result.Add('WINAPI.WINDOWS.');
  Result.Add('WINAPI.MESSAGES.');
  Result.Add('SYSTEM.');
  Result.Add('SYSTEM.SYSUTILS.');
  Result.Add('SYSTEM.VARIANTS.');
  Result.Add('SYSTEM.CLASSES.');
  Result.Add('SYSTEM.TYPES.');
  Result.Add('VCL.');
  Result.Add('VCL.GRAPHICS.');
  Result.Add('VCL.CONTROLS.');
  Result.Add('VCL.FORMS.');
  Result.Add('VCL.DIALOGS.');
  Result.Add('FMX.');
  Result.Add('FMX.TYPES.');
  Result.Add('FMX.CONTROLS.');
  Result.Add('FMX.FORMS.');
  Result.Add('FMX.DIALOGS.');
  Result.Add('QTYPES.');
  Result.Add('QGRAPHICS.');
  Result.Add('QCONTROLS.');
  Result.Add('QFORMS.');
  Result.Add('QDIALOGS.');
  Result.Add('QSTDCTRLS.');
  Result.Add('DATASNAP.');
  Result.Add('DATA.WIN.');
end;

procedure TUnsedUsesProcessor.Process;
var
  Step: string;
begin
  if not FEnvControl.AreDependenciasReady then
  begin
    FEnvControl.ForceRunDependencies;
    Exit;
  end;

  FUnitNode := nil;
  if not FileExists(FFilePath) then
  begin
    Logger.Debug('TFindUnitParser.Process: File do not exists %s', [FFilePath]);
    Exit;
  end;

  try
    try
      FUnitNode := TPasSyntaxTreeBuilder.Run(FFilePath, False, nil);
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
    FUses := GetUnitSpecifiedOnUses;
    FUsedTypes := GetUsedTypes;
    FMatches := GetFullMatchsForUses;
    FOptionalUsesPrefix := GetOptionalUsesPrefix;

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
