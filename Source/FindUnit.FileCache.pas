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
  TUnits = class(TObject)
  strict private
    FUnitsPath: TDictionary<string, TPasFile>;
    FUniqueUnitNames: TDictionary<string, string>;
  private
    function GetItem(const Key: string): TPasFile;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function ExtractPair(const Key: string): TPair<string, TPasFile>;
    function TryGetValue(const Key: string; out Value: TPasFile): Boolean;
    function Values: TDictionary<string,TPasFile>.TValueCollection;

    procedure Add(const Key: string; const Value: TPasFile);
    function FileExists(Key: string): Boolean;

    property Items[const Key: string]: TPasFile read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TUnitsController = class(TObject)
  private
    FFullMatchSearchCache: ISearchStringCache;
    FMatchSearchCache: ISearchStringCache;

    FUnits: TUnits;

    FReady: Boolean;
    FRc: TCriticalSection;

    procedure SetUnits(const Value: TUnits);
  public
    constructor Create;
    destructor Destroy; override;

    function GetFindInfo(const SearchString: string): TStringList;
    function GetFindInfoFullMatch(const SearchString: string): TStringList;

    function GetPasFile(FilePath: string): TPasFile;
    function ExtractPasFile(FilePath: string): TPasFile;

    property Units: TUnits read FUnits write SetUnits;
    property Ready: Boolean read FReady write FReady;
  end;

implementation

uses
  FindUnit.SearchString, System.SysUtils;

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

procedure TUnitsController.SetUnits(const Value: TUnits);
begin
  FUnits := Value;
  FMatchSearchCache := TSearchStringCache.Create;
  FFullMatchSearchCache := TSearchStringCache.Create;
end;

{ TUnits }

procedure TUnits.Add(const Key: string; const Value: TPasFile);
var
  CurUnitName: string;
begin
  FUnitsPath.AddOrSetValue(Key, Value);
  CurUnitName := UpperCase(ExtractFileName(Key));
  FUniqueUnitNames.AddOrSetValue(CurUnitName, Key);
end;

constructor TUnits.Create;
begin
  FUnitsPath := TDictionary<string, TPasFile>.Create;
  FUniqueUnitNames := TDictionary<string, string>.Create;
end;

destructor TUnits.Destroy;
begin

  inherited;
end;

function TUnits.ExtractPair(const Key: string): TPair<string, TPasFile>;
var
  CurUnitName: string;
begin
  Result := FUnitsPath.ExtractPair(Key);
  CurUnitName := UpperCase(ExtractFileName(Key));
  FUniqueUnitNames.Remove(CurUnitName);
end;

function TUnits.FileExists(Key: string): Boolean;
var
  CurUnitName: string;
  OutputValue: string;
begin
  CurUnitName := UpperCase(ExtractFileName(Key));
  Result := FUniqueUnitNames.TryGetValue(CurUnitName, OutputValue);
end;

function TUnits.GetCount: Integer;
begin
  Result := FUnitsPath.Count;
end;

function TUnits.GetItem(const Key: string): TPasFile;
begin
  Result := FUnitsPath.Items[Key];
end;

function TUnits.TryGetValue(const Key: string; out Value: TPasFile): Boolean;
begin
  Result := FUnitsPath.TryGetValue(Key, Value);
end;

function TUnits.Values: TDictionary<string,TPasFile>.TValueCollection;
begin
  Result := FUnitsPath.Values;
end;

end.
