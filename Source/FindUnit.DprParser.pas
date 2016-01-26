unit FindUnit.DprParser;

interface

uses
	Classes, DelphiAST.Consts;

type
  TDrpFile = class(TObject)
  private
    FUses: TStringList;

    procedure Parse(Path: string);
  public
    constructor Create(Path: string);
    destructor Destroy; override;

    property Files: TStringList read FUses write FUses;
  end;

implementation

uses
  DelphiAST.Classes, DelphiAST;

{ TDrpFile }

constructor TDrpFile.Create(Path: string);
begin
  FUses := TStringList.Create;
  Parse(Path);
end;

destructor TDrpFile.Destroy;
begin
  FUses.Free;
  inherited;
end;

procedure TDrpFile.Parse(Path: string);
var
  UsesSection: TSyntaxNode;
  UnitNode: TSyntaxNode;
  UsesNode: TSyntaxNode;
  UsesItem: TSyntaxNode;
begin
  try
    UnitNode := TPasSyntaxTreeBuilder.Run(Path, True, nil);
    if UnitNode = nil then
      Exit;

    UnitNode := UnitNode.FindNode(ntUnit);
    if UnitNode = nil then
      Exit;

    UsesNode := UnitNode.FindNode(ntUses);
    while UsesNode.FindNode(ntUnit) <> nil do
    begin
      UsesItem := UsesNode.FindNode(ntUnit);
      FUses.Add(UsesItem.GetAttribute(anPath));
      UsesNode.DeleteChild(UsesItem);
    end;
  finally
    UnitNode.Free;
  end;

end;

end.
