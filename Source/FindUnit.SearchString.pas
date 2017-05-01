unit FindUnit.SearchString;

interface

uses
  Classes,

  FindUnit.Header,
  FindUnit.PasParser,

  Generics.Collections;

type
  TSearchString = class(TObject)
  protected
    FCandidates: TObjectList<TPasFile>;

    function FoundAllEntries(Entries: TStringList; const Text: string): Boolean;

    function GetMatcherOnItemListType(Item: TPasFile; SearchString: TStringList; List: TStringList; const Sufix: string; var ItensFound: integer): string;
    function GetMatchesOnItem(Item: TPasFile; SearchString: TStringList; var ItensFound: integer): string;
  public
    constructor Create(Candidates: TObjectList<TPasFile>);
    destructor Destroy; override;

    function GetMatch(const SearchString: string): TStringList;
  end;

implementation

uses
  SysUtils;

{ TSearchString }

constructor TSearchString.Create(Candidates: TObjectList<TPasFile>);
begin
  FCandidates := Candidates;
end;

destructor TSearchString.Destroy;
begin
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

function TSearchString.GetMatch(const SearchString: string): TStringList;
var
  I: Integer;
  Item: TPasFile;
  ItensFound: Integer;
  SearchList: TStringList;
begin
  ItensFound := 0;
  Result := TStringList.Create;

  SearchList := TStringList.Create;
  try
    SearchList.Delimiter := ' ';
    SearchList.DelimitedText := UpperCase(SearchString);

    for I := 0 to FCandidates.Count - 1 do
    begin
      Item := FCandidates[I];

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

