unit FindUnit.FileEditor;

interface

uses
  ToolsApi, SimpleParser.Lexer.Types, DelphiAST.Classes, FindUnit.OtaUtils, Classes, DesignEditors, Graphics;

type
  CharPosition = record
    StartLine: Integer;
    StartPos: Integer;

    EndLine: Integer;
    EndPos: Integer;
  end;

  TFileRegion = class(TObject)
  private
    FUses: TStringList;
    FUsesPosition: CharPosition;
    FRegionPosition: CharPosition;
  public
    constructor Create;
    destructor Destroy; override;

    function HaveUses: Boolean;

    property RegionPosition: CharPosition read FRegionPosition write FRegionPosition;
    property UsesPosition: CharPosition read FUsesPosition write FUsesPosition;
  end;

  TSourceFileEditor = class(TObject)
  private
    FSource: IOTASourceEditor;
    FFileContent: TStringList;
    FInterfaceRegion: TFileRegion;
    FImplementationRegion: TFileRegion;

    procedure ParseInformations;

    function GetInformationsFor(Token: string; SearchForEnd: Boolean; StartLine, EndLine: Integer): CharPosition;

    procedure GetInfos;

    procedure WriteInformationAtPostion(Line, Position: Integer; Information: string);
    procedure AddUsesToRegion(Region: TFileRegion; UseUnit: string);
  public
    constructor Create(SourceEditor: IOTASourceEditor);
    destructor Destroy; override;

    function Prepare: Boolean;

    procedure AddUsesToInterface(UseUnit: string);
    procedure AddUsesToImplementation(UseUnit: string);
  end;

implementation

uses
  SysUtils;

{ TSourceFileEditor }

procedure TSourceFileEditor.AddUsesToImplementation(UseUnit: string);
begin
  AddUsesToRegion(FImplementationRegion, UseUnit);
end;

procedure TSourceFileEditor.AddUsesToInterface(UseUnit: string);
begin
  AddUsesToRegion(FInterfaceRegion, UseUnit);
end;

procedure TSourceFileEditor.AddUsesToRegion(Region: TFileRegion; UseUnit: string);
var
  Line: Integer;
  PosChar: Integer;
begin
  Line := Region.UsesPosition.EndLine;
  PosChar := Region.UsesPosition.EndPos -1;
  if not Region.HaveUses then
  begin
    Line := Region.RegionPosition.StartLine + 1;
    PosChar := 0;

    UseUnit := #13#10 + 'uses' + #13#10 + #9 + UseUnit + ';' + #13#10
  end
  else
    UseUnit := ', ' + UseUnit;

  WriteInformationAtPostion(Line, PosChar, UseUnit);
end;

constructor TSourceFileEditor.Create(SourceEditor: IOTASourceEditor);
begin
  FInterfaceRegion := TFileRegion.Create;
  FImplementationRegion := TFileRegion.Create;
  FFileContent := TStringList.Create;
  FSource := SourceEditor;
end;

destructor TSourceFileEditor.Destroy;
begin
  FInterfaceRegion.Destroy;
  FImplementationRegion.Destroy;
  FFileContent.Free;
  inherited;
end;

function TSourceFileEditor.GetInformationsFor(Token: string; SearchForEnd: Boolean; StartLine, EndLine: Integer): CharPosition;
var
  I: Integer;
  OutPosition: Integer;
  OutLine: Integer;

  function GetPositionOfString(SearchString: string; iStart, IEnd: Integer; out FoundLine, FoundPosition: Integer): Boolean;
  var
    Line: string;
    Position: Integer;
  begin
    Result := False;
    for iStart := iStart to IEnd do
    begin
      Line := FFileContent[iStart];
      Position := Pos(SearchString, Line);
      if Position  = 0 then
        Continue;

      FoundLine := iStart;
      FoundPosition := Position;
      Result := True;
      Break;
    end;
  end;
begin
  Result.StartLine := -1;
  Token := UpperCase(Token);
  if not GetPositionOfString(Token, StartLine, EndLine, OutLine, OutPosition) then
    Exit;

  Result.StartLine := OutLine;
  Result.StartPos := OutPosition;

  if SearchForEnd then
  begin
    if GetPositionOfString(';', OutLine, EndLine, OutLine, OutPosition) then
    begin
      Result.EndLine := OutLine;
      Result.EndPos := OutPosition;
    end;
  end;
end;

procedure TSourceFileEditor.GetInfos;
var
  InterfaceEnd: Integer;
begin
  FInterfaceRegion.RegionPosition := GetInformationsFor('interface', False, 0, FFileContent.Count -1);
  FImplementationRegion.RegionPosition := GetInformationsFor('implementation', False, 0, FFileContent.Count -1);

  if FImplementationRegion.RegionPosition.StartLine > -1 then
    InterfaceEnd := FImplementationRegion.RegionPosition.StartLine
  else
    InterfaceEnd := FFileContent.Count -1;

  FInterfaceRegion.UsesPosition := GetInformationsFor('uses', True, FInterfaceRegion.RegionPosition.StartLine, InterfaceEnd);

  if FImplementationRegion.RegionPosition.StartLine > -1 then
    FImplementationRegion.UsesPosition := GetInformationsFor('uses', True, FImplementationRegion.RegionPosition.StartLine, FFileContent.Count -1);
end;

procedure TSourceFileEditor.ParseInformations;
begin
  GetInfos;
end;

function TSourceFileEditor.Prepare: Boolean;
begin
  try
    Result := True;
    FFileContent.Text := UpperCase(EditorAsString(FSource));
    ParseInformations;
  except
    Result := False;
  end;
end;


procedure TSourceFileEditor.WriteInformationAtPostion(Line, Position: Integer; Information: string);
var
  InfoPosition: TOTACharPos;
  FileWriter: IOTAEditWriter;
  SetPosition: Integer;
begin
  if Information = '' then
    Exit;

  Line := Line + 1;
  InfoPosition.Line := Line;
  InfoPosition.CharIndex := Position;

  SetPosition := FSource.EditViews[0].CharPosToPos(InfoPosition);

  FileWriter := FSource.CreateUndoableWriter;
  try
    FileWriter.CopyTo(SetPosition);
    FileWriter.Insert(PAnsiChar(AnsiString(Information)));
  finally
    FileWriter := nil;
  end;
end;

{ TFileRegion }

constructor TFileRegion.Create;
begin
  FUses := TStringList.Create;
  FUses.Duplicates := dupIgnore;
  FUses.Sorted := True;
end;

destructor TFileRegion.Destroy;
begin

  inherited;
end;

function TFileRegion.HaveUses: Boolean;
begin
  Result := UsesPosition.StartLine > 0;
end;

end.
