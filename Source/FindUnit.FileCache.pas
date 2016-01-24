unit FindUnit.FileCache;

interface

uses
  Generics.Collections, Classes, FindUnit.Parser;

type
  TUnitsController = class(TObject)
  private
    FNeedGenerateFindInfo: Boolean;
    FUnits: TObjectList<TFindUnitItem>;
    FContent: TStringList;
    FReady: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function GetFindInfo(SearchString: string): TStringList;

    property Units: TObjectList<TFindUnitItem> read FUnits write FUnits;
    property Ready: Boolean read FReady write FReady;
  end;

implementation

uses
  FindUnit.SearchString;



{ TUnitUpdateController }

constructor TUnitsController.Create;
begin
end;

destructor TUnitsController.Destroy;
begin
  FUnits.Free;
  inherited;
end;

function TUnitsController.GetFindInfo(SearchString: string): TStringList;
var
  Search: TSearchString;
begin
  Search := TSearchString.Create(FUnits);
  try
    Result := Search.GetMatch(SearchString);
  finally
    Search.Free;
  end;
end;

end.
