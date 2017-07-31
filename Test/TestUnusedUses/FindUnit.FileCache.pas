unit FindUnit.FileCache;

interface

uses
  FindUnit.PasParser,

  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  
  Winapi.UI.Xaml;

type
  TUnitsController = class(TObject)
  private
    FSearchHistory: TDictionary<string,string>;

    FUnits: TDictionary<string, TPasFile>;
    FReady: Boolean;
    FRc: TCriticalSection;

    function SearchOnHistory(const SearchString: string): TStringList;
    procedure AddToHistory(const SearchString, Content: string);

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
  SysUtils,

  FindUnit.SearchString;

{ TUnitUpdateController }

procedure TUnitsController.AddToHistory(const SearchString, Content: string);
begin
  if FSearchHistory = nil then
    Exit;
  FSearchHistory.AddOrSetValue(UpperCase(SearchString), Content);
end;

constructor TUnitsController.Create;
begin
  FRc := TCriticalSection.Create;
  inherited;
end;

destructor TUnitsController.Destroy;
begin
  FRc.Free;
  FSearchHistory.Free;
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
  Result := SearchOnHistory(SearchString);
  if Result <> nil then
    Exit;

  Search := TSearchString.Create(FUnits);
  try
    Result := Search.GetMatch(SearchString);
    AddToHistory(SearchString, Result.Text);
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

function TUnitsController.SearchOnHistory(const SearchString: string): TStringList;
var
  Local: string;
  Content: string;
begin
  Result := nil;
  if FSearchHistory = nil then
    Exit;

  Local := UpperCase(SearchString);
  if FSearchHistory.TryGetValue(Local, Content) then
  begin
    Result := TStringList.Create;
    Result.Text := Content;
  end;
end;

procedure TUnitsController.SetUnits(const Value: TDictionary<string, TPasFile>);
begin
  FUnits := Value;
  FSearchHistory.Free;

  FSearchHistory := TDictionary<string,string>.Create;
end;

end.
