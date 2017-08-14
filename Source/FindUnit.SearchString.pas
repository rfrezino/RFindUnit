unit FindUnit.SearchString;

interface

uses
  System.Classes,

  FindUnit.Header,
  FindUnit.PasParser,

  System.Generics.Collections,

  System.SyncObjs,

  Log4Pascal,
  Interf.SearchStringCache,
  FindUnit.FileCache;

type
  TSearchString = class(TObject)
  private
    FRcSearch: TCriticalSection;
    FCandidates: TUnits;
    FSearchStringCache: ISearchStringCache;
    FMatchCache: ISearchStringCache;

    function FoundAllEntries(Entries: TStringList; const Text: string): Boolean;

    function GetMatcherOnItemListType(Item: TPasFile; SearchString: TStringList; List: TStringList; const Sufix: string; var ItensFound: integer): string;
    function GetMatchesOnItem(Item: TPasFile; SearchString: TStringList; var ItensFound: integer): string;
  public
    constructor Create(Candidates: TUnits);
    destructor Destroy; override;

    function GetMatch(const SearchString: string): TStringList;
    function GetFullMatch(const SearchString: string): TStringList;

    property FullMatchCache: ISearchStringCache read FSearchStringCache write FSearchStringCache;
    property MatchCache: ISearchStringCache read FMatchCache write FMatchCache;
  end;

implementation

uses
  System.SysUtils;

{ TSearchString }

constructor TSearchString.Create(Candidates: TUnits);
begin
  FCandidates := Candidates;
  FRcSearch := TCriticalSection.Create;
end;

destructor TSearchString.Destroy;
begin
  FRcSearch.Destroy;
  inherited;
end;

function TSearchString.FoundAllEntries(Entries: TStringList; const Text: string): Boolean;
var
  I: Integer;
  Entry: string;
begin
  Result := True;
  for I := 0 to Entries.Count -1 do
  begin
    Entry := Entries[i];
    Result := Pos(Entry, Text) <> 0;
    if not Result then
      Exit;
  end;
end;

function TSearchString.GetFullMatch(const SearchString: string): TStringList;
var
  I: Integer;
  Line: string;
  UpSS: string;
  Local: TStringList;
begin
  if SearchString.IsEmpty then
  begin
    Result := TStringList.Create;
    Exit;
  end;

  if Assigned(FSearchStringCache) and FSearchStringCache.GetMatch(SearchString, Result) then
    Exit;

  UpSS := SearchString.ToUpper;
  Result := GetMatch('.' + SearchString + '.');
  if Result.Count = 0 then
    Result := GetMatch(SearchString);

  for I := Result.Count - 1 downto 0 do
  begin
    Line := Result[I].ToUpper;

    if Line.StartsWith(UpSS + '.')
      or Line.Contains('.' + UpSS + '.')
      or Line.Contains('.' + UpSS + ' -') then
      Continue;

    Result.Delete(I);
  end;

  if FSearchStringCache <> nil then
  begin
    Local := TStringList.Create;
    Local.Text := Result.Text;
    FSearchStringCache.AddItemOnCache(SearchString, Local);
  end;
end;

function TSearchString.GetMatch(const SearchString: string): TStringList;
var
  Item: TPasFile;
  ItensFound: Integer;
  SearchList: TStringList;
  SearchKey: string;
begin
  Result := TStringList.Create;
  if SearchString.IsEmpty then
    Exit;

  SearchKey := UpperCase(SearchString);
  if Assigned(FMatchCache) and (FMatchCache.GetMatch(SearchKey, Result)) then
    Exit;

  FRcSearch.Acquire;
  try
    ItensFound := 0;

    SearchList := TStringList.Create;
    try
      SearchList.Delimiter := ' ';
      SearchList.DelimitedText := UpperCase(SearchString);

      for Item in FCandidates.Values do
      begin
        if FoundAllEntries(SearchList, UpperCase(Item.OriginUnitName) + '.') then
        begin
          Result.Text := Result.Text + Item.OriginUnitName + '.* - Unit';
          Inc(ItensFound);
        end;

        Result.Text := Result.Text + GetMatchesOnItem(Item, SearchList, ItensFound);
        if ItensFound >= MAX_RETURN_ITEMS then
          Exit;
      end;
    finally
      SearchList.Free;
    end;
  finally
    FRcSearch.Release;
  end;
end;

function TSearchString.GetMatcherOnItemListType(Item: TPasFile; SearchString: TStringList; List: TStringList; const Sufix: string; var ItensFound: integer): string;
var
  LocalSearchUpperCase: TStringList;
  iString: Integer;
  ItemToFind: string;
  MatchList: TStringList;
begin
  MatchList := TStringList.Create;
  LocalSearchUpperCase := TStringList.Create;
  try
    LocalSearchUpperCase.Text := UpperCase(List.Text);

    for iString := 0 to LocalSearchUpperCase.Count - 1 do
    begin
      ItemToFind :=  UpperCase(Item.OriginUnitName) + '.' + LocalSearchUpperCase[iString];

      if FoundAllEntries(SearchString, ItemToFind) then
      begin
        MatchList.Add(Item.OriginUnitName + '.' + List[iString] + Sufix);
        Inc(ItensFound);
        if ItensFound > MAX_RETURN_ITEMS then
          Exit;
      end;
    end;
  finally
    Result := MatchList.Text;
    MatchList.Free;
    LocalSearchUpperCase.Free;
  end;
end;

function TSearchString.GetMatchesOnItem(Item: TPasFile; SearchString: TStringList; var ItensFound: integer): string;
var
  ListType: TListType;
  List: TStringList;
begin
  Result := '';
  for ListType := Low(TListType) to High(TListType) do
  begin
    List := Item.GetListFromType(ListType);
    Result := Result + GetMatcherOnItemListType(Item, SearchString, List, strListTypeDescription[ListType], ItensFound);
    if ItensFound >= MAX_RETURN_ITEMS then
      Exit;
  end;
end;



end.

