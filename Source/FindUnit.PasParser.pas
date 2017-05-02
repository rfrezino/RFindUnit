unit FindUnit.PasParser;

interface

uses
  Classes,
  ComCtrls,
  CommonOptionStrs,
  SysUtils,

  DelphiAST.Classes,

  FindUnit.DcuDecompiler,
  FindUnit.Header,
  FindUnit.IncluderHandlerInc,
  FindUnit.Utils,

  Generics.Collections,

  SimpleParser.Lexer.Types;

type

  TPasFile = class(TObject)
  private
    FLastModification: TDateTime;
    FOriginUnitName: string;

    FLists: TObjectList<TStringList>;

    FClasses: TStringList;
    FProcedures: TStringList;
    FFunctions: TStringList;
    FConstants: TStringList;
    FVariables: TStringList;
    FFilePath: string;

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
    property FilePath: string read FFilePath write FFilePath;

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
    destructor Destroy; override;

    function Process: TPasFile;

    procedure SetIncluder(Includer: IIncludeHandler);
  end;


implementation

uses
  DelphiAST,
  IOUtils,
  Log4PAscal,

  DelphiAST.Consts;

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
  inherited Create;
  FFilePath := FilePath;
end;

destructor TPasFileParser.Destroy;
begin
  FUnitNode.Free;
  inherited;
end;

procedure TPasFileParser.GetClasses;
var
  SectionTypesNode: TSyntaxNode;
  TypeNode: TSyntaxNode;
  TypeDesc: TSyntaxNode;
  Description: string[50];
  ItemClassName: string;
  EnumType: TSyntaxNode;
  IdenType: TSyntaxNode;
  AttrName: string;
begin
  SectionTypesNode := FInterfaceNode.FindNode(ntTypeSection);

  while SectionTypesNode <> nil do
  begin
    TypeNode := SectionTypesNode.FindNode(ntTypeDecl);
    while TypeNode <> nil do
    begin
      try
        Description := '.* - Class';
        ItemClassName := TypeNode.GetAttribute(anName);

        TypeDesc := TypeNode.FindNode(ntType);
        EnumType := nil;

        if TypeDesc <> nil then
        begin
          Description := TypeDesc.GetAttribute(anType);

          if Description = '' then
            Description := 'Enum';

          Description[1] := UpCase(Description[1]);
          Description := ' - ' + Description;

          FResultItem.FClasses.Add(ItemClassName + Description);

          AttrName := TypeDesc.GetAttribute(anType);

          if AttrName = 'set' then
            EnumType := TypeDesc.FindNode(ntType)
          else if AttrName = 'enum' then
            EnumType := TypeDesc
          else
            EnumType := nil;
        end;

        if EnumType <> nil then
        begin
          IdenType := EnumType.FindNode(ntIdentifier);
          while IdenType <> nil do
          begin
            FResultItem.FClasses.Add(ItemClassName + '.' + IdenType.GetAttribute(anName) + Description + ' item');

            EnumType.DeleteChild(IdenType);
            IdenType := EnumType.FindNode(ntIdentifier);
          end;
        end;

      except
        on e: exception do
        begin
          Logger.Error('TFindUnitParser.GetClasses: %s', [e.Message]);
          {$IFDEF RAISEMAD}
          raise;
          {$ENDIF}
        end;
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
  try
    FResultItem.FLastModification := IOUtils.TFile.GetLastWriteTime(FFilePath);
  except
  end;
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
  if FUnitNode <> nil then
    FResultItem.OriginUnitName := FUnitNode.GetAttribute(anName);
end;

procedure TPasFileParser.GetVariables;
var
  VariablesNode: TSyntaxNode;
  Variable: TSyntaxNode;
  VariableName: TSyntaxNode;
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
  if FUnitNode = nil then
    Exit(False);

  FInterfaceNode := FUnitNode.FindNode(ntInterface);
  Result := FInterfaceNode <> nil;
end;

function TPasFileParser.Process: TPasFile;
var
  Step: string;
begin
  FUnitNode := nil;
  Result := nil;
  if not FileExists(FFilePath) then
  begin
    Logger.Debug('TFindUnitParser.Process: File do not exists %s', [FFilePath]);
    Exit;
  end;

  try
    try
      FUnitNode := TPasSyntaxTreeBuilder.Run(FFilePath, True, FIncluder);
    except
      on E: ESyntaxTreeException do
      begin
        FUnitNode := e.SyntaxTree;
        e.SyntaxTree := nil;
      end;
    end;

    if FUnitNode = nil then
    begin
      Exit;
    end;

    FResultItem := TPasFile.Create;
    Result := FResultItem;

    Step := 'GetUnitName';
    GetUnitName;

    Step := 'GetFileLastModification';
    GetFileLastModification;

    if not HaveInterfaceNode then
      Exit;

    Step := 'GetClasses';
    GetClasses;
    Step := 'GetMethods';
    GetMethods;
    Step := 'GetVariables';
    GetVariables;
    Step := 'GetConstants';
    GetConstants;
  except
    on E: Exception do
    begin
      Logger.Error('TFindUnitParser.Process: Trying to parse %s. Msg: %s | Step: %s', [FFilePath, E.Message, Step]);
      Result := nil;
      {$IFDEF RAISEMAD} raise; {$ENDIF}
    end;
  end;
end;


procedure TPasFileParser.SetIncluder(Includer: IIncludeHandler);
begin
  FIncluder := Includer;
end;

end.
