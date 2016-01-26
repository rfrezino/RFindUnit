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

  TSourceFileEditor = class(TObject)
  private
    FSource: IOTASourceEditor;
    FFileContent: TStringList;

    FHaveImplementationUses: Boolean;
    FHaveInterfaceUses: Boolean;

    FInterfacePos: CharPosition;
    FIntUsesPos: CharPosition;

    FImplementationPos: CharPosition;
    FImpUses: CharPosition;

    procedure ParseInformations;

    function GetInformationsFor(Token: string; SearchForEnd: Boolean; StartLine, EndLine: Integer): CharPosition;

    procedure GetInfos;

    procedure WriteInformationAtPostion(Line, Position: Integer; Information: string);
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
var
  PosChar: Integer;
  Line: Integer;
begin
  Line := FImpUses.EndLine;
  PosChar := FImpUses.EndPos -1;
  if not FHaveImplementationUses then
  begin
    Line := FImplementationPos.StartLine + 1;
    PosChar := 0;

    UseUnit := #13#10 + 'uses' + #13#10 + #9 + UseUnit + ';' + #13#10
  end
  else
    UseUnit := ', ' + UseUnit;

  WriteInformationAtPostion(Line, PosChar, UseUnit);
end;

procedure TSourceFileEditor.AddUsesToInterface(UseUnit: string);
var
  Line: Integer;
  PosChar: Integer;
begin
  Line := FIntUsesPos.EndLine;
  PosChar := FIntUsesPos.EndPos -1;
  if not FHaveInterfaceUses then
  begin
    Line := FInterfacePos.StartLine + 1;
    PosChar := 0;

    UseUnit := #13#10 + 'uses' + #13#10 + #9 + UseUnit + ';' + #13#10
  end
  else
    UseUnit := ', ' + UseUnit;

  WriteInformationAtPostion(Line, PosChar, UseUnit);
end;

constructor TSourceFileEditor.Create(SourceEditor: IOTASourceEditor);
begin
  FFileContent := TStringList.Create;
  FSource := SourceEditor;
end;

destructor TSourceFileEditor.Destroy;
begin
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
  FInterfacePos := GetInformationsFor('interface', False, 0, FFileContent.Count -1);
  FImplementationPos := GetInformationsFor('implementation', False, 0, FFileContent.Count -1);

  if FImplementationPos.StartLine > -1 then
    InterfaceEnd := FImplementationPos.StartLine
  else
    InterfaceEnd := FFileContent.Count -1;

  FIntUsesPos := GetInformationsFor('uses', True, FInterfacePos.StartLine, InterfaceEnd);

  if FImplementationPos.StartLine > -1 then
    FImpUses := GetInformationsFor('uses', True, FImplementationPos.StartLine, FFileContent.Count -1);

  FHaveImplementationUses := FImpUses.StartLine > 0;
  FHaveInterfaceUses := FIntUsesPos.StartLine > 0;
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

end.
