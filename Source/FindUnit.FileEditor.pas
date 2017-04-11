unit FindUnit.FileEditor;

interface

uses
  Classes,
  DelphiAST.Classes,
  DesignEditors,
  FindUnit.FormMessage,
  FindUnit.Header,
  FindUnit.OtaUtils,
  FindUnit.Settings,
  FindUnit.Utils,
  Graphics,
  RegExpr,
  SimpleParser.Lexer.Types,
  SysUtils,
  ToolsApi,
  Vcl.Dialogs;

type
  TCharPosition = record
    StartLine: Integer;
    StartPos: Integer;

    EndLine: Integer;
    EndPos: Integer;
  end;

  TFileRegion = class(TObject)
  strict private
    FSource: IOTASourceEditor;
    FUsesNormalized: TStringList;
    FUses: TStringList;
    FUsesPosition: TCharPosition;
    FRegionPosition: TCharPosition;
    FFullFileText: TStringList;

    function RemoveUsesFromList(UsesList: TStrings; ItemToRemove: string): string;

    procedure DeleteInformationFrom(FromLine, ToLine: integer; var Writer: IOTAEditWriter);
    procedure WriteInformationAtPostion(Line, Position: Integer; const Information: string; var Writer: IOTAEditWriter);

    function AddUsesInternal(UseUnit: string; var Writer: IOTAEditWriter): Boolean; overload;
    function AddUsesInternal(UseUnit: TStringList; var Writer: IOTAEditWriter): Boolean; overload;
  public
    constructor Create(SourceEditor: IOTASourceEditor);
    destructor Destroy; override;

    function HaveUses: Boolean;

    property RegionPosition: TCharPosition read FRegionPosition write FRegionPosition;
    property UsesPosition: TCharPosition read FUsesPosition write FUsesPosition;

    procedure SetUsesPosition(NewUsesPosition: TCharPosition);

    procedure GetUsesFromText(FullFileText: TStringList);
    function UsesExists(UseUnit: string): Boolean;
    procedure RemoveUses(UseUnit: string; var Writer: IOTAEditWriter);
    procedure RemoveAllSection(var Writer: IOTAEditWriter);
    function AddUses(UseUnit: string; var Writer: IOTAEditWriter): Boolean;

    function GetUsesList: TStringList;
    function AreThereUnparsableCharsInSection: Boolean;
  end;

  TSourceFileEditor = class(TObject)
  private
    FSource: IOTASourceEditor;
    FFileContent: TStringList;
    FInterfaceRegion: TFileRegion;
    FImplementationRegion: TFileRegion;

    procedure ParseInformations;
    function GetInformationsFor(Token: string; SearchForEnd: Boolean; StartLine, EndLine: Integer): TCharPosition;
    procedure GetInfos;

    function AddUsesToRegion(Region: TFileRegion; UseUnit: string; var Writer: IOTAEditWriter): Boolean;
    procedure WriteInformationAtPostion(Line, Position: Integer; const Information: string; var Writer: IOTAEditWriter);
  public
    constructor Create(SourceEditor: IOTASourceEditor);
    destructor Destroy; override;

    function Prepare: Boolean;

    function AddUsesToInterface(const UseUnit: string): Boolean;
    function AddUsesToImplementation(const UseUnit: string): Boolean;

    procedure RemoveUsesFromInterface(const UseUnit: string);
    procedure RemoveUsesFromImplementation(const UseUnit: string);

    function IsLineOnImplementationSection(Line: Integer): Boolean;

    procedure AddUnit(UnitInfo: TStringPosition; ShowMessageOnAdd: Boolean = True);
  end;

implementation


{ TSourceFileEditor }

procedure TSourceFileEditor.AddUnit(UnitInfo: TStringPosition; ShowMessageOnAdd: Boolean);
var
  MessageText: string;
begin
  MessageText := '';
  if (not GlobalSettings.AlwaysUseInterfaceSection) and IsLineOnImplementationSection(UnitInfo.Line) then
  begin
    if AddUsesToImplementation(UnitInfo.Value) then
      MessageText := 'Unit ' + UnitInfo.Value + ' added to implementation''s uses.';
  end
  else
  begin
    if AddUsesToInterface(UnitInfo.Value) then
      MessageText := 'Unit ' + UnitInfo.Value + ' added to interface''s uses.';
  end;

  if (MessageText <> '') and ShowMessageOnAdd then
    TfrmMessage.ShowInfoToUser(MessageText);
end;

function TSourceFileEditor.AddUsesToImplementation(const UseUnit: string): Boolean;
var
  Writer: IOTAEditWriter;
begin
  FInterfaceRegion.RemoveUses(UseUnit, Writer);
  Writer := nil;
  Prepare;
  Result := AddUsesToRegion(FImplementationRegion, UseUnit, Writer);
end;

function TSourceFileEditor.AddUsesToInterface(const UseUnit: string): Boolean;
var
  Writer: IOTAEditWriter;
begin
  FImplementationRegion.RemoveUses(UseUnit, Writer);
  Writer := nil;
  Prepare;
  Result := AddUsesToRegion(FInterfaceRegion, UseUnit, Writer);
end;

function TSourceFileEditor.AddUsesToRegion(Region: TFileRegion; UseUnit: string; var Writer: IOTAEditWriter): Boolean;
begin
  Result := Region.AddUses(UseUnit, Writer);
end;

constructor TSourceFileEditor.Create(SourceEditor: IOTASourceEditor);
begin
  FInterfaceRegion := TFileRegion.Create(SourceEditor);
  FImplementationRegion := TFileRegion.Create(SourceEditor);
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

function TSourceFileEditor.GetInformationsFor(Token: string; SearchForEnd: Boolean; StartLine, EndLine: Integer): TCharPosition;
var
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
      Line := UpperCase(FFileContent[iStart]);
      Position := Pos(SearchString, Line);

      if Position  = 0 then
        Continue;

      if (SearchString <> ';')
        and (Length(Line) > Length(SearchString))
        and (Line[Length(Line)] <> '') then
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

  FImplementationRegion.GetUsesFromText(FFileContent);
  FInterfaceRegion.GetUsesFromText(FFileContent);
end;

function TSourceFileEditor.IsLineOnImplementationSection(Line: Integer): Boolean;
begin
  Result := Line > FImplementationRegion.RegionPosition.StartLine;
end;

procedure TSourceFileEditor.ParseInformations;
begin
  GetInfos;
end;

function TSourceFileEditor.Prepare: Boolean;
begin
  try
    Result := True;
    FFileContent.Text := EditorAsString(FSource);
    ParseInformations;
  except
    Result := False;
  end;
end;

procedure TSourceFileEditor.RemoveUsesFromImplementation(const UseUnit: string);
var
  Writer: IOTAEditWriter;
begin
  FImplementationRegion.RemoveUses(UseUnit, Writer);
end;

procedure TSourceFileEditor.RemoveUsesFromInterface(const UseUnit: string);
var
  Writer: IOTAEditWriter;
begin
  FInterfaceRegion.RemoveUses(UseUnit, Writer);
end;

procedure TSourceFileEditor.WriteInformationAtPostion(Line, Position: Integer; const Information: string; var Writer: IOTAEditWriter);
var
  InfoPosition: TOTACharPos;
  SetPosition: Integer;
begin
  if Information = '' then
    Exit;

  Line := Line + 1;
  InfoPosition.Line := Line;
  InfoPosition.CharIndex := Position;

  SetPosition := FSource.EditViews[0].CharPosToPos(InfoPosition);

  if Writer = nil then
    Writer := FSource.CreateUndoableWriter;
  Writer.CopyTo(SetPosition);
  Writer.Insert(PAnsiChar(AnsiString(Information)));
end;

{ TFileRegion }

procedure TFileRegion.DeleteInformationFrom(FromLine, ToLine: integer; var Writer: IOTAEditWriter);
var
  StartPosition, EndPosition: TOTACharPos;
  StartPos: Integer;
  EndPos: Integer;
begin
  Inc(FromLine);
  StartPosition.Line := FromLine;
  StartPosition.CharIndex := 0;

  Inc(ToLine);
  Inc(ToLine);
  EndPosition.Line := ToLine;
  EndPosition.CharIndex := 0;

  StartPos := FSource.EditViews[0].CharPosToPos(StartPosition);
  EndPos := FSource.EditViews[0].CharPosToPos(EndPosition);

  if Writer = nil then
    Writer := FSource.CreateUndoableWriter;

  Writer.CopyTo(StartPos);
  Writer.DeleteTo(EndPos);
end;

function TFileRegion.AddUses(UseUnit: string; var Writer: IOTAEditWriter): Boolean;
var
  NewUses: TStringList;
begin
  Result := False;
  if UsesExists(UseUnit) then
    Exit;

  if GlobalSettings.SortUsesAfterAdding and (not AreThereUnparsableCharsInSection) then
  begin
    RemoveAllSection(Writer);
    Writer := nil;

    NewUses := GetUsesList;
    NewUses.Add(UseUnit);
    AddUsesInternal(NewUses, Writer);
  end
  else
    AddUsesInternal(UseUnit, Writer);
end;

function TFileRegion.AddUsesInternal(UseUnit: string; var Writer: IOTAEditWriter): Boolean;
var
  Line: Integer;
  PosChar: Integer;
  NewUsesPosition: TCharPosition;
  UnitContent: string;
begin
  Result := True;

  if (UseUnit.ToUpper = 'USES') or UseUnit.IsEmpty then
    Exit;

  Line := UsesPosition.EndLine;
  PosChar := UsesPosition.EndPos -1;
  if not HaveUses then
  begin
    Line := RegionPosition.StartLine + 1;
    PosChar := 0;

    UnitContent := '  ' + UseUnit + ';';
    UseUnit := #13#10 + 'uses' + #13#10 + UnitContent + #13#10;

    NewUsesPosition.StartLine := Line;
    NewUsesPosition.StartPos := PosChar;
    NewUsesPosition.EndLine := NewUsesPosition.StartLine + 2;
    NewUsesPosition.EndPos := UnitContent.Length;

    SetUsesPosition(NewUsesPosition);

    ShowMessage(IntToStr(Line));
  end
  else
  begin
    if (PosChar > 80) or (GlobalSettings.BreakLine)  then
      UseUnit := ',' + #13#10 + '  ' + UseUnit
    else
      UseUnit := ', ' + UseUnit;
  end;

  WriteInformationAtPostion(Line, PosChar, UseUnit, Writer);
end;

function TFileRegion.AddUsesInternal(UseUnit: TStringList; var Writer: IOTAEditWriter): Boolean;
var
  Line: Integer;
  PosChar: Integer;
  NewUsesPosition: TCharPosition;
  NewUses: string;
  UseCur: string;
  CurDomain: string;
  LastDomain: string;
  MustBreak: Boolean;
begin
  Result := True;

  if GlobalSettings.SortUsesAfterAdding then
    UseUnit.Sort;

  LastDomain := '';
  NewUses := '';
  for UseCur in UseUnit do
  begin
    if (UseCur.Trim.ToUpper = 'USES')
      or (UseCur.Trim.IsEmpty) then
      Continue;

    if NewUses.IsEmpty then
    begin
      NewUses := '  ' + UseCur;

      LastDomain := UseCur;
      LastDomain := Fetch(LastDomain, '.', False);
    end
    else if GlobalSettings.BreakLine then
    begin

      if GlobalSettings.BlankLineBtwNameScapes then
      begin
        CurDomain := UseCur;
        CurDomain := Fetch(CurDomain, '.', False);
        if not LastDomain.IsEmpty  then
          MustBreak := not CurDomain.Equals(LastDomain);
        LastDomain := CurDomain;
      end;

      if MustBreak then
        NewUses := NewUses + ',' + #13#10 + #13#10 + '  ' + UseCur
      else
        NewUses := NewUses + ',' + #13#10 + '  ' + UseCur;

      MustBreak := False;
    end
    else
      NewUses := NewUses + ', ' + UseCur;
  end;
  NewUses := NewUses + ';';

  NewUses := #13#10 + 'uses' + #13#10 + NewUses;

  Line := UsesPosition.EndLine;
  PosChar := UsesPosition.EndPos -1;
  if not HaveUses then
  begin
    Line := RegionPosition.StartLine + 1;
    PosChar := 0;

    NewUsesPosition.StartLine := Line;
    NewUsesPosition.StartPos := PosChar;
    NewUsesPosition.EndLine := NewUsesPosition.StartLine + 2;
    NewUsesPosition.EndPos := NewUses.Length;

    SetUsesPosition(NewUsesPosition);
  end;
  WriteInformationAtPostion(Line, PosChar, NewUses, Writer);
end;

function TFileRegion.AreThereUnparsableCharsInSection: Boolean;
var
  I: Integer;
  UsesText: TStringList;
begin
  UsesText := TStringList.Create;
  try
    for I := FUsesPosition.StartLine to FUsesPosition.EndLine do
      UsesText.Add(Trim(FFullFileText[I]));

    Result := (UsesText.Text.IndexOf('/') > 0) or (UsesText.Text.IndexOf('{') > 0);
  finally
    UsesText.Free;
  end;
end;

constructor TFileRegion.Create(SourceEditor: IOTASourceEditor);
begin
  FUsesNormalized := TStringList.Create;
  FUsesNormalized.Duplicates := dupIgnore;
  FUsesNormalized.Sorted := True;

  FUses := TStringList.Create;
  FUses.Duplicates := dupIgnore;
  FUses.Sorted := True;

  FSource := SourceEditor;
end;

destructor TFileRegion.Destroy;
begin
  FUsesNormalized.Free;
  FUses.Free;
  inherited;
end;

procedure TFileRegion.GetUsesFromText(FullFileText: TStringList);
var
  I: Integer;
  LocalUses: TStringList;
  Line: string;
  Item: string;
begin
  FFullFileText := FullFileText;
  if not HaveUses then
    Exit;

  LocalUses := TStringList.Create;
  try
    for I := FUsesPosition.StartLine to FUsesPosition.EndLine do
      LocalUses.Add(Trim(FullFileText[I]));

    LocalUses.Text := StringReplace(LocalUses.Text,' ',',', [rfReplaceAll]);
    LocalUses.Text := StringReplace(LocalUses.Text,';',',', [rfReplaceAll]);

    for I := 0 to LocalUses.Count -1 do
    begin
      Line := LocalUses[i];
      while Line <> '' do
      begin
        Item := Fetch(Line, ',');
        FUsesNormalized.Add(UpperCase(Item));
        FUses.Add(Item);
      end;
    end;
  finally
    LocalUses.Free;
  end;
end;

function TFileRegion.GetUsesList: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := FUses.Text;
end;

function TFileRegion.HaveUses: Boolean;
begin
  Result := UsesPosition.StartLine > 0;
end;

procedure TFileRegion.RemoveAllSection(var Writer: IOTAEditWriter);
begin
  if not HaveUses then
    Exit;

  DeleteInformationFrom(FUsesPosition.StartLine, FUsesPosition.EndLine, Writer);
  FUsesPosition.StartLine := 0;
end;

procedure TFileRegion.RemoveUses(UseUnit: string; var Writer: IOTAEditWriter);
var
  I: Integer;
  LocalUses: TStringList;
begin
  if not UsesExists(UseUnit) then
    Exit;

  Writer := nil;
  FFullFileText.Text := EditorAsString(ActiveSourceEditor);
  LocalUses := TStringList.Create;
  try
    for I := FUsesPosition.StartLine to FUsesPosition.EndLine do
      LocalUses.Add(FFullFileText[I]);

    LocalUses.Text := RemoveUsesFromList(LocalUses, UseUnit);
    DeleteInformationFrom(FUsesPosition.StartLine, FUsesPosition.EndLine, Writer);
    if UpperCase(Trim(LocalUses.Text)) <> 'USES'  then
      WriteInformationAtPostion(FUsesPosition.StartLine, 0, LocalUses.Text, Writer);
  finally
    LocalUses.Free;
  end;
end;

function TFileRegion.RemoveUsesFromList(UsesList: TStrings; ItemToRemove: string): string;
const
  Mask = '[^a-zA-Z0-9]%s[^a-zA-Z0-9]';
var
  Regex: TRegExpr;
  MaskFilter: string;
begin
  MaskFilter := Format(Mask, [ItemToRemove]);
  Regex := TRegExpr.Create;
  Regex.Expression := MaskFilter;

  Result :=  Trim(Regex.Replace(UsesList.Text, '', True));
  if Result[Length(Result)] = ',' then
  begin
    Delete(Result, Length(Result), 1);
    Result := Result + ';';
  end;
end;

procedure TFileRegion.SetUsesPosition(NewUsesPosition: TCharPosition);
begin
  FUsesPosition := NewUsesPosition;
end;

function TFileRegion.UsesExists(UseUnit: string): Boolean;
begin
  Result := FUsesNormalized.IndexOf(UpperCase(UseUnit)) > -1;
end;

procedure TFileRegion.WriteInformationAtPostion(Line, Position: Integer; const Information: string; var Writer: IOTAEditWriter);
var
  InfoPosition: TOTACharPos;
  SetPosition: Integer;
begin
  if Information = '' then
    Exit;

  Line := Line + 1;
  InfoPosition.Line := Line;
  InfoPosition.CharIndex := Position;

  SetPosition := FSource.EditViews[0].CharPosToPos(InfoPosition);

////  NewUsesPosition.StartLine := FUsesPosition.StartLine;
////  NewUsesPosition.StartPos := FUsesPosition.StartPos;
////  NewUsesPosition.EndLine := InfoPosition.Line;
////  NewUsesPosition.EndPos := InfoPosition.CharIndex + Length(Information);
//
//  SetUsesPosition(NewUsesPosition);

  if Writer = nil then
    Writer := FSource.CreateUndoableWriter;

  Writer.CopyTo(SetPosition);
  Writer.Insert(PAnsiChar(AnsiToUtf8(Information)));
end;

end.
