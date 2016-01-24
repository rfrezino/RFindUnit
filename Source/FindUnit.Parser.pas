unit FindUnit.Parser;

interface

uses
  ComCtrls, Classes, SysUtils, DelphiAST.Classes, Generics.Collections, FindUnit.Utils;

type
  TFindUnitItem = class(TObject)
  private
    FHashKey: string;
    FLastModification: TDateTime;
    FOriginUnitName: string;
    FPrepared: Boolean;

    FLists: TObjectList<TStringList>;

    FClasses: TStringList;
    FProcedures: TStringList;
    FFunctions: TStringList;
    FConstants: TStringList;
    FVariables: TStringList;

    procedure PrepareList(List: TStringList; const Description: string);
    procedure PrepareLists;
  public
    constructor Create;
    destructor Destroy; override;

    property OriginUnitName: string read FOriginUnitName write FOriginUnitName;
    property LastModification: TDateTime read FLastModification write FLastModification;

    property Classes: TStringList read FClasses;
    property Procedures: TStringList read FProcedures;
    property Functions: TStringList read FFunctions;
    property Constants: TStringList read FConstants;
    property Variables: TStringList read FVariables;

    function PrepareToFind: string;
  end;

  TFindUnitParser = class(TObject)
  private
    FFilePath: string;
    FResultItem: TFindUnitItem;

    FUnitNode: TSyntaxNode;
    FInterfaceNode: TSyntaxNode;

    procedure GetUnitName;
    procedure GetFileLastModification;
    procedure GetClasses;
    procedure GetMethods;
    procedure GetVariables;
    procedure GetConstants;

    function HaveInterfaceNode: Boolean;
  public
    constructor Create(FilePath: string);

    function Process: TFindUnitItem;
  end;


implementation

uses
  SimpleParser.Lexer.Types, IOUtils, DelphiAST.Writer, DelphiAST.Consts, DelphiAST, FindUnit.Logger;

{ TFindUnitItem }

constructor TFindUnitItem.Create;

  procedure CreateAndAdd(var List: TStringList);
  begin
    List := TStringList.Create;
    List.Sorted := True;
    FLists.Add(List);
  end;
begin
  FLists := TObjectList<TStringList>.Create;
  CreateAndAdd(FClasses);
  CreateAndAdd(FProcedures);
  CreateAndAdd(FFunctions);
  CreateAndAdd(FConstants);
  CreateAndAdd(FVariables);
end;

destructor TFindUnitItem.Destroy;
begin
  FLists.Free;
  inherited;
end;

procedure TFindUnitItem.PrepareList(List: TStringList; const Description: string);
var
  I: Integer;
begin
  List.Sorted := False;
  List.Text := Trim(List.Text);
  for I := 0 to List.Count -1 do
    if Description = '' then
      List[I] := FOriginUnitName + '.' + List[i]
    else
      List[I] := FOriginUnitName + '.' + List[i] + ' - ' + Description;
  List.Sorted := True;
end;

const
  Identifications: array[0..4] of String = (
  '',
  'Procedure',
  'Function',
  'Constant',
  'Variable'
  );

procedure TFindUnitItem.PrepareLists;
var
  I: Integer;
begin
  if not FPrepared then
  begin
    for I := 0 to Length(Identifications) -1 do
      PrepareList(FLists.Items[i], Identifications[i]);

    FPrepared := True;
  end;
end;

function TFindUnitItem.PrepareToFind: string;
var
  I: Integer;
begin
  Result := '';
  PrepareLists;
  for I := 0 to FLists.Count -1 do
    Result := Result + FLists.Items[i].Text;
end;

{ TFindUnitParser }

constructor TFindUnitParser.Create(FilePath: string);
begin
  FFilePath := FilePath;
end;

procedure TFindUnitParser.GetClasses;
var
  SectionTypesNode: TSyntaxNode;
  TypeNode: TSyntaxNode;
  TypeDesc: TSyntaxNode;
  Description: string[50];
begin
  SectionTypesNode := FInterfaceNode.FindNode(ntTypeSection);
  if SectionTypesNode = nil then
    Exit;

  TypeNode := SectionTypesNode.FindNode(ntTypeDecl);
  while TypeNode <> nil do
  begin
    TypeDesc := TypeNode.FindNode(ntType);
    Description := TypeDesc.GetAttribute(anType);

    if Description = '' then
      FResultItem.FClasses.Add(TypeNode.GetAttribute(anName))
    else
    begin
      Description[1] := UpCase(Description[1]);
      FResultItem.FClasses.Add(TypeNode.GetAttribute(anName) + ' - ' + Description);
    end;

    SectionTypesNode.DeleteChild(TypeNode);
    TypeNode := SectionTypesNode.FindNode(ntTypeDecl);
  end;
end;

procedure TFindUnitParser.GetConstants;
var
  ConstantsNode: TSyntaxNode;
  ConstantItem: TSyntaxNode;
  Name: TSyntaxNode;
begin
  ConstantsNode := FInterfaceNode.FindNode(ntConstants);
  if ConstantsNode = nil then
    Exit;

  ConstantItem := ConstantsNode.FindNode(ntConstant);
  while ConstantItem <> nil do
  begin
    Name := ConstantItem.FindNode(ntName);

    FResultItem.FConstants.Add(TValuedSyntaxNode(Name).Value);

    ConstantsNode.DeleteChild(ConstantItem);
    ConstantItem := ConstantsNode.FindNode(ntConstant);
  end;
end;

procedure TFindUnitParser.GetFileLastModification;
begin
  FResultItem.FLastModification := IOUtils.TFile.GetLastWriteTime(FFilePath);
end;

procedure TFindUnitParser.GetMethods;
var
  MethodNode: TSyntaxNode;
begin
  MethodNode := FInterfaceNode.FindNode(ntMethod);
  while MethodNode <> nil do
  begin
    if MethodNode.GetAttribute(anKind) = 'procedure' then
        FResultItem.FProcedures.Add(MethodNode.GetAttribute(anName))
    else
      FResultItem.FFunctions.Add(MethodNode.GetAttribute(anName));

    FInterfaceNode.DeleteChild(MethodNode);
    MethodNode := FInterfaceNode.FindNode(ntMethod);
  end;
end;

procedure TFindUnitParser.GetUnitName;
begin
  FResultItem.OriginUnitName := FUnitNode.GetAttribute(anName);
end;

procedure TFindUnitParser.GetVariables;
var
  VariablesNode: TSyntaxNode;
  Variable: TSyntaxNode;
  VariableName: TSyntaxNode;
  Attr: TPair<TAttributeName, string>;
begin
  VariablesNode := FInterfaceNode.FindNode(ntVariables);
  if VariablesNode = nil then
    Exit;

  Variable := VariablesNode.FindNode(ntVariable);
  while Variable <> nil do
  begin
    VariableName := Variable.FindNode(ntName);
    FResultItem.FVariables.Add(TValuedSyntaxNode(VariableName).Value);

    VariablesNode.DeleteChild(Variable);
    Variable := VariablesNode.FindNode(ntVariable);
  end;
end;

function TFindUnitParser.HaveInterfaceNode: Boolean;
begin
  FInterfaceNode := FUnitNode.FindNode(ntInterface);
  Result := FInterfaceNode <> nil;
end;

function TFindUnitParser.Process: TFindUnitItem;
begin
  Result := nil;
  if not FileExists(FFilePath) then
    Exit;

  try
    FUnitNode := TPasSyntaxTreeBuilder.Run(FFilePath, True, TIncludeHandler.Create(ExtractFilePath(FFilePath)));
    if FUnitNode = nil then
      Exit;

    FResultItem := TFindUnitItem.Create;
    Result := FResultItem;
    try
      GetUnitName;
      GetFileLastModification;

      if not HaveInterfaceNode then
        Exit;

      GetClasses;
      GetMethods;
      GetVariables;
      GetConstants;
    finally
      FUnitNode.Free;
    end;
  except
    on E: Exception do
    begin
      Logger.Error('TFindUnitParser.Process: Trying to parse %s. Msg: %s', [FFilePath, E.Message]);
      Result := nil;
    end;
  end;
end;


end.
