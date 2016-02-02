unit FindUnit.PasParser;

interface

uses
  ComCtrls, Classes, SysUtils, DelphiAST.Classes, Generics.Collections, FindUnit.Utils,
  SimpleParser.Lexer.Types, FindUnit.Header;

type

  TPasFile = class(TObject)
  private
    FHashKey: string;
    FLastModification: TDateTime;
    FOriginUnitName: string;

    FLists: TObjectList<TStringList>;

    FClasses: TStringList;
    FProcedures: TStringList;
    FFunctions: TStringList;
    FConstants: TStringList;
    FVariables: TStringList;

    procedure PrepareList(List: TStringList; const Description: string);
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

    function GetListFromType(ListType: TListType): TStringList;
  end;

  TPasFileParser = class(TObject)
  private
    FFilePath: string;
    FResultItem: TPasFile;

    FUnitNode: TSyntaxNode;
    FInterfaceNode: TSyntaxNode;

    FIncluder: IIncludeHandler;

    procedure GetUnitName;
    procedure GetFileLastModification;
    procedure GetClasses;
    procedure GetMethods;
    procedure GetVariables;
    procedure GetConstants;

    function HaveInterfaceNode: Boolean;
  public
    constructor Create(const FilePath: string);

    function Process: TPasFile;

    procedure SetIncluder(Includer: IIncludeHandler);
  end;


implementation

uses
  IOUtils, DelphiAST.Writer, DelphiAST.Consts, DelphiAST, Log4PAscal;

{ TFindUnitItem }

constructor TPasFile.Create;

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

destructor TPasFile.Destroy;
begin
  FLists.Free;
  inherited;
end;

function TPasFile.GetListFromType(ListType: TListType): TStringList;
begin
  case ListType of
    ltClasses: Result := FClasses;
    ltProcedures: Result := FProcedures;
    ltFunctions: Result := FFunctions;
    ltContants: Result := FConstants;
    ltVariables: Result := FVariables;
  end;
end;

procedure TPasFile.PrepareList(List: TStringList; const Description: string);
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

{ TFindUnitParser }

constructor TPasFileParser.Create(const FilePath: string);
begin
  FFilePath := FilePath;
end;

procedure TPasFileParser.GetClasses;
var
  SectionTypesNode: TSyntaxNode;
  TypeNode: TSyntaxNode;
  TypeDesc: TSyntaxNode;
  Description: string[50];
  ItemClassName: string;
begin
  SectionTypesNode := FInterfaceNode.FindNode(ntTypeSection);

  while SectionTypesNode <> nil do
  begin
    TypeNode := SectionTypesNode.FindNode(ntTypeDecl);
    while TypeNode <> nil do
    begin
      try
        Description := ' - Class';
        ItemClassName := TypeNode.GetAttribute(anName);

        TypeDesc := TypeNode.FindNode(ntType);
        if TypeDesc <> nil then
        begin
          Description := TypeDesc.GetAttribute(anType);

          if Description = '' then
            Description := 'Enum';

          Description[1] := UpCase(Description[1]);
          Description := ' - ' + Description;
        end;
        FResultItem.FClasses.Add(ItemClassName + Description);
      except
        on e: exception do
          Logger.Error('TFindUnitParser.GetClasses: %s', [e.Message]);

      end;
      SectionTypesNode.DeleteChild(TypeNode);
      TypeNode := SectionTypesNode.FindNode(ntTypeDecl);
    end;

    FInterfaceNode.DeleteChild(SectionTypesNode);
    SectionTypesNode := FInterfaceNode.FindNode(ntTypeSection);
  end;
end;

procedure TPasFileParser.GetConstants;
var
  ConstantsNode: TSyntaxNode;
  ConstantItem: TSyntaxNode;
  Name: TSyntaxNode;
begin
  ConstantsNode := FInterfaceNode.FindNode(ntConstants);
  while ConstantsNode <> nil do
  begin
    ConstantItem := ConstantsNode.FindNode(ntConstant);
    while ConstantItem <> nil do
    begin
      Name := ConstantItem.FindNode(ntName);

      FResultItem.FConstants.Add(TValuedSyntaxNode(Name).Value);

      ConstantsNode.DeleteChild(ConstantItem);
      ConstantItem := ConstantsNode.FindNode(ntConstant);
    end;

    FInterfaceNode.DeleteChild(ConstantsNode);
    ConstantsNode := FInterfaceNode.FindNode(ntConstants);
  end;
end;

procedure TPasFileParser.GetFileLastModification;
begin
  FResultItem.FLastModification := IOUtils.TFile.GetLastWriteTime(FFilePath);
end;

procedure TPasFileParser.GetMethods;
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

procedure TPasFileParser.GetUnitName;
begin
  FResultItem.OriginUnitName := FUnitNode.GetAttribute(anName);
end;

procedure TPasFileParser.GetVariables;
var
  VariablesNode: TSyntaxNode;
  Variable: TSyntaxNode;
  VariableName: TSyntaxNode;
  Attr: TPair<TAttributeName, string>;
begin
  VariablesNode := FInterfaceNode.FindNode(ntVariables);
  while VariablesNode <> nil do
  begin
    Variable := VariablesNode.FindNode(ntVariable);
    while Variable <> nil do
    begin
      VariableName := Variable.FindNode(ntName);
      FResultItem.FVariables.Add(TValuedSyntaxNode(VariableName).Value);

      VariablesNode.DeleteChild(Variable);
      Variable := VariablesNode.FindNode(ntVariable);
    end;

    FInterfaceNode.DeleteChild(VariablesNode);
    VariablesNode := FInterfaceNode.FindNode(ntVariables);
  end;
end;

function TPasFileParser.HaveInterfaceNode: Boolean;
begin
  FInterfaceNode := FUnitNode.FindNode(ntInterface);
  Result := FInterfaceNode <> nil;
end;

function TPasFileParser.Process: TPasFile;
begin
  Result := nil;
  if not FileExists(FFilePath) then
  begin
    Logger.Debug('TFindUnitParser.Process: File do not exists %s', [FFilePath]);
    Exit;
  end;

  Logger.Debug('TFindUnitParser.Process: Parsing file %s', [FFilePath]);
  try
    FUnitNode := TPasSyntaxTreeBuilder.Run(FFilePath, True, FIncluder);
    if FUnitNode = nil then
      Exit;

    FResultItem := TPasFile.Create;
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


procedure TPasFileParser.SetIncluder(Includer: IIncludeHandler);
begin
  FIncluder := Includer;
end;

end.
