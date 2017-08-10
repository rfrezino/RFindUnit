unit FindUnit.PasParser;

interface

uses
  System.Classes,
  System.SysUtils,

  DelphiAST.Classes,

  FindUnit.Header,

  System.Generics.Collections,

  SimpleParser.Lexer.Types;

type
  TPasFile = class(TObject)
  private
    FLastModification: TDateTime;
    FOriginUnitName: string;
    FFilePath: string;

    FLists: TObjectList<TStringList>;

    FClasses: TStringList;
    FProcedures: TStringList;
    FFunctions: TStringList;
    FConstants: TStringList;
    FVariables: TStringList;
    FClassFunctions: TStringList;
    FClassProcedure: TStringList;
    FEnumerators: TStringList;
    FInterfaces: TStringList;
    FReferences: TStringList;
    FRecords: TStringList;
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
    property ClassFunctions: TStringList read FClassFunctions write FClassFunctions;
    property ClassProcedure: TStringList read FClassProcedure write FClassProcedure;
    property Enumerators: TStringList read FEnumerators write FEnumerators;
    property Interfaces: TStringList read FInterfaces write FInterfaces;
    property References: TStringList read FReferences write FReferences;
    property Records: TStringList read FRecords write FRecords;

    function GetListFromType(ListType: TListType): TStringList;
  end;

  TPasFileParser = class(TObject)
  private
    FFilePath: string;
    FResultItem: TPasFile;

    FUnitNode: TSyntaxNode;
    FInterfaceNode: TSyntaxNode;

    FIncluder: IIncludeHandler;

    //main rotines
    procedure GetUnitName;
    procedure GetFileLastModification;
    procedure GetTypeInformation;
    procedure GetMethods;
    procedure GetVariables;
    procedure GetConstants;
    //sub rotines
    procedure GetRecords(Types: TSyntaxNode);
    procedure GetInterfaceDescription(Types: TSyntaxNode);
    procedure GetClassDescription(Types: TSyntaxNode);
    procedure GetEnumerationDescription(Types: TSyntaxNode);
    procedure GetClassMethodsFromClassNode(AClassName: string; ClassNode: TSyntaxNode);
    procedure GetProcedureReferenceDescription(Types: TSyntaxNode);
    procedure GetFunctionReferenceDescription(Types: TSyntaxNode);
    procedure GetReference(Types: TSyntaxNode);
    procedure GetSubRangeDesc(Types: TSyntaxNode);

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
  Log4PAscal,

  DelphiAST.Consts, System.IOUtils;

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
  CreateAndAdd(FClassFunctions);
  CreateAndAdd(FClassProcedure);
  CreateAndAdd(FEnumerators);
  CreateAndAdd(FInterfaces);
  CreateAndAdd(FReferences);
  CreateAndAdd(FRecords);
end;

destructor TPasFile.Destroy;
begin
  FLists.Free;
  inherited;
end;

function TPasFile.GetListFromType(ListType: TListType): TStringList;
begin
  Result := nil;
  case ListType of
    ltClasses: Result := FClasses;
    ltProcedures: Result := FProcedures;
    ltFunctions: Result := FFunctions;
    ltContants: Result := FConstants;
    ltVariables: Result := FVariables;
    ltClassFunctions: Result := FClassFunctions;
    ltClassProcedures: Result := FClassProcedure;
    ltEnumeratores: Result := FEnumerators;
    ltReferences: Result := FReferences;
    ltInterfaces: Result := FInterfaces;
    ltRecords: Result := FRecords;
  end;
end;

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

procedure TPasFileParser.GetTypeInformation;
var
  SectionTypesNode: TSyntaxNode;
  TypeNode: TSyntaxNode;
  TypeDesc: TSyntaxNode;
  TypeType: string;
begin
  SectionTypesNode := FInterfaceNode.FindNode(ntTypeSection);

  while SectionTypesNode <> nil do
  begin
    TypeNode := SectionTypesNode.FindNode(ntTypeDecl);
    while TypeNode <> nil do
    begin
      try
        if TypeNode.HasAttribute(anForwarded) then
          GetInterfaceDescription(TypeNode)
        else
        begin
          TypeDesc := TypeNode.FindNode(ntType);

          if TypeDesc = nil then
            GetReference(TypeNode)
          else
          begin
            TypeType := TypeDesc.GetAttribute(anType);
            if TypeType.IsEmpty then
              TypeType := TypeDesc.GetAttribute(anName);

            if TypeType.Equals('interface') then
              GetInterfaceDescription(TypeNode)
            else if TypeType.Equals('class') then
              GetClassDescription(TypeNode)
            else if TypeType.Equals('enum') or TypeType.Equals('set') then
              GetEnumerationDescription(TypeNode)
            else if TypeType.Equals('record') then
              GetRecords(TypeNode)
            else if TypeType.Equals('procedure') then
              GetProcedureReferenceDescription(TypeNode)
            else if TypeType.Equals('function') then
              GetFunctionReferenceDescription(TypeNode)
            else if  TypeType.Equals('subrange') then
              GetSubRangeDesc(TypeNode)
            else
            begin
              try
                GetReference(TypeNode);
              except
                on e: exception do
                  Logger.Error('TFindUnitParser.GetClasses: %s - %s | %s', [e.Message, FFilePath, TypeNode.ToString]);
              end;
            end;
          end;
        end;
      except
        on e: exception do
        begin
          Logger.Error('TFindUnitParser.GetClasses: %s - %s', [e.Message, FFilePath]);
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

procedure TPasFileParser.GetClassDescription(Types: TSyntaxNode);
var
  Description: string;
begin
  Description := Types.GetAttribute(anName);
  FResultItem.FClasses.Add(Description  + '.* - Class');

  GetClassMethodsFromClassNode(Description, Types);
end;

procedure TPasFileParser.GetClassMethodsFromClassNode(AClassName: string; ClassNode: TSyntaxNode);
var
  MethodNode: TSyntaxNode;
  IsClassMethod: Boolean;
  MethodType: string;
  MethodNameDesc: string;
  PublicNode: TSyntaxNode;
begin
  PublicNode := ClassNode.FindNode(ntType);
  if PublicNode = nil then
    Exit;

  PublicNode := PublicNode.FindNode(ntPublic);
  if PublicNode = nil then
    Exit;

  MethodNode := PublicNode.FindNode(ntMethod);
  while MethodNode <> nil do
  begin
    IsClassMethod := MethodNode.HasAttribute(anClass) and MethodNode.GetAttribute(anClass).Equals('true');
    if IsClassMethod then
    begin
      MethodNameDesc := AClassName + '.' + MethodNode.GetAttribute(anName);
      MethodType := MethodNode.GetAttribute(anKind);
      if MethodType  = 'procedure' then
        FResultItem.FClassProcedure.Add(MethodNameDesc + strListTypeDescription[ltClassProcedures])
      else
        FResultItem.FClassFunctions.Add(MethodNameDesc + strListTypeDescription[ltClassFunctions]);
    end;

    PublicNode.DeleteChild(MethodNode);
    MethodNode := PublicNode.FindNode(ntMethod);
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

procedure TPasFileParser.GetEnumerationDescription(Types: TSyntaxNode);
var
  Description: string;
  ItemClassName: string;
  EnumType: TSyntaxNode;
  IdenType: TSyntaxNode;
  AttrName: string;
  IsEnum: Boolean;
  TypeDesc: TSyntaxNode;
begin
  ItemClassName := Types.GetAttribute(anName);
  TypeDesc := Types.FindNode(ntType);
  Description := TypeDesc.GetAttribute(anName);
  if Description.IsEmpty then
    Description := TypeDesc.GetAttribute(anType);
  
  IsEnum := Description.Equals('enum');

  Description[1] := UpCase(Description[1]);
  Description := ' - ' + Description;

  FResultItem.FEnumerators.Add(ItemClassName + Description);

  AttrName := Types.GetAttribute(anType);

  if not IsEnum then
    EnumType := Types.FindNode(ntType)
  else if (AttrName = 'enum') or IsEnum then
    EnumType := TypeDesc
  else
    EnumType := nil;

  if EnumType <> nil then
  begin
    IdenType := EnumType.FindNode(ntIdentifier);
    while IdenType <> nil do
    begin
      FResultItem.FEnumerators.Add(ItemClassName + '.' + IdenType.GetAttribute(anName) + Description + ' item');

      EnumType.DeleteChild(IdenType);
      IdenType := EnumType.FindNode(ntIdentifier);
    end;
  end;
end;

procedure TPasFileParser.GetFileLastModification;
begin
  try
    FResultItem.FLastModification := System.IOUtils.TFile.GetLastWriteTime(FFilePath);
  except
  end;
end;

procedure TPasFileParser.GetFunctionReferenceDescription(Types: TSyntaxNode);
var
  Description: string;
begin
  Description := Types.GetAttribute(anName);
  Description := Description + '.* - Function Reference';

  FResultItem.FReferences.Add(Description);
end;

procedure TPasFileParser.GetInterfaceDescription(Types: TSyntaxNode);
var
  Description: string;
begin
  Description := Types.GetAttribute(anName);
  Description := Description + '.* - Interface';

  FResultItem.FInterfaces.Add(Description);
end;

procedure TPasFileParser.GetMethods;
var
  MethodNode: TSyntaxNode;
  IsClassMethod: Boolean;
  MethodType: string;
  MethodNameDesc: string;
begin
  MethodNode := FInterfaceNode.FindNode(ntMethod);
  while MethodNode <> nil do
  begin
    MethodNameDesc := MethodNode.GetAttribute(anName);
    IsClassMethod := MethodNode.HasAttribute(anClass) and MethodNode.GetAttribute(anClass).Equals('true');
    MethodType := MethodNode.GetAttribute(anKind);
    if MethodType  = 'procedure' then
    begin
      if IsClassMethod then
        FResultItem.FClassProcedure.Add(MethodNameDesc)
      else
        FResultItem.FProcedures.Add(MethodNameDesc);
    end
    else
    begin
      if IsClassMethod then
        FResultItem.FClassFunctions.Add(MethodNameDesc)
      else
        FResultItem.FFunctions.Add(MethodNameDesc);
    end;

    FInterfaceNode.DeleteChild(MethodNode);
    MethodNode := FInterfaceNode.FindNode(ntMethod);
  end;
end;

procedure TPasFileParser.GetProcedureReferenceDescription(Types: TSyntaxNode);
var
  Description: string;
begin
  Description := Types.GetAttribute(anName);
  Description := Description + '.* - Procedure Reference';

  FResultItem.FReferences.Add(Description);
end;

procedure TPasFileParser.GetRecords(Types: TSyntaxNode);
begin
  FResultItem.FRecords.Add(Types.GetAttribute(anName) + '.* - Record');
end;

procedure TPasFileParser.GetReference(Types: TSyntaxNode);
begin
  FResultItem.FReferences.Add(Types.GetAttribute(anName) + '.* - Reference');
end;

procedure TPasFileParser.GetSubRangeDesc(Types: TSyntaxNode);
begin
  FResultItem.FReferences.Add(Types.GetAttribute(anName) + '.* - Sub Range');
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
    Result.FilePath := FFilePath;

    Step := 'GetUnitName';
    GetUnitName;

    Step := 'GetFileLastModification';
    GetFileLastModification;

    if not HaveInterfaceNode then
      Exit;

    Step := 'GetTypeInformation';
    GetTypeInformation;
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
