unit FindUnit.FileCache;

interface

uses
  Generics.Collections, Classes, FindUnit.PasParser;

type
  TUnitsController = class(TObject)
  private
    FSearchHistory: TDictionary<string,string>;

    FNeedGenerateFindInfo: Boolean;
    FUnits: TObjectList<TPasFile>;
    FContent: TStringList;
    FReady: Boolean;

    function SearchOnHistory(SearchString: string): TStringList;
    procedure AddToHistory(SearchString, Content: string);

    procedure SetUnits(const Value: TObjectList<TPasFile>);
  public
    constructor Create;
    destructor Destroy; override;

    function GetFindInfo(SearchString: string): TStringList;

    property Units: TObjectList<TPasFile> read FUnits write SetUnits;
    property Ready: Boolean read FReady write FReady;
  end;

implementation

uses
  FindUnit.SearchString, SysUtils;

{ TUnitUpdateController }

procedure TUnitsController.AddToHistory(SearchString, Content: string);
begin
  if FSearchHistory = nil then
    Exit;
  FSearchHistory.AddOrSetValue(UpperCase(SearchString), Content);
end;

constructor TUnitsController.Create;
begin
end;

destructor TUnitsController.Destroy;
begin
  FSearchHistory.Free;
  FUnits.Free;
  inherited;
end;

function TUnitsController.GetFindInfo(SearchString: string): TStringList;
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

function TUnitsController.SearchOnHistory(SearchString: string): TStringList;
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

procedure TUnitsController.SetUnits(const Value: TObjectList<TPasFile>);
begin
  FUnits := Value;
  FSearchHistory.Free;
  FSearchHistory := TDictionary<string,string>.Create;
end;

end.
