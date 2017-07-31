unit FindUnit.FileCache;

interface

uses
  FindUnit.PasParser,
  FindUnit.SearchStringCache,

  Interf.SearchStringCache,

  System.Classes,
  System.Generics.Collections,
  System.SyncObjs;

type
  TUnitsController = class(TObject)
  private
    FFullMatchSearchCache: ISearchStringCache;
    FMatchSearchCache: ISearchStringCache;

    FUnits: TDictionary<string, TPasFile>;
    FReady: Boolean;
    FRc: TCriticalSection;

    procedure SetUnits(const Value: TDictionary<string, TPasFile>);
  public
    constructor Create;
    destructor Destroy; override;

    function GetFindInfo(const SearchString: string): TStringList;
    function GetFindInfoFullMatch(const SearchString: string): TStringList;

    function GetPasFile(FilePath: string): TPasFile;
    function ExtractPasFile(FilePath: string): TPasFile;

    property Units: TDictionary<string, TPasFile> read FUnits write SetUnits;
    property Ready: Boolean read FReady write FReady;
  end;

implementation

uses
  FindUnit.SearchString;

{ TUnitUpdateController }
constructor TUnitsController.Create;
begin
  FFullMatchSearchCache := TSearchStringCache.Create;
  FRc := TCriticalSection.Create;
  inherited;
end;

destructor TUnitsController.Destroy;
begin
  FRc.Free;
  FUnits.Free;
  inherited;
end;

function TUnitsController.ExtractPasFile(FilePath: string): TPasFile;
var
  Item: TPair<string, TPasFile>;
begin
  FRc.Acquire;
  try
    Item := FUnits.ExtractPair(FilePath);
    Result := Item.Value;
  finally
    FRc.Release;
  end;
end;

function TUnitsController.GetFindInfo(const SearchString: string): TStringList;
var
  Search: TSearchString;
begin
  Search := TSearchString.Create(FUnits);
  try
    Search.MatchCache := FMatchSearchCache;
    Result := Search.GetMatch(SearchString);
  finally
    Search.Free;
  end;
end;

function TUnitsController.GetFindInfoFullMatch(const SearchString: string): TStringList;
var
  Search: TSearchString;
begin
  Search := TSearchString.Create(FUnits);
  try
    Search.FullMatchCache := FFullMatchSearchCache;
    Result := Search.GetFullMatch(SearchString);
  finally
    Search.Free;
  end;
end;

function TUnitsController.GetPasFile(FilePath: string): TPasFile;
begin
  FRc.Acquire;
  try
    FUnits.TryGetValue(FilePath, Result);
  finally
    FRc.Release;
  end;
end;

procedure TUnitsController.SetUnits(const Value: TDictionary<string, TPasFile>);
begin
  FUnits := Value;
  FMatchSearchCache := TSearchStringCache.Create;
  FFullMatchSearchCache := TSearchStringCache.Create;
end;

end.
