unit FindUnit.SearchString;

interface

uses
  Classes, FindUnit.Parser, Generics.Collections;

type
  TSearchString = class(TObject)
  protected
    FCandidates: TObjectList<TFindUnitItem>;
  public
    constructor Create(Candidates: TObjectList<TFindUnitItem>);
    destructor Destroy; override;

    function GetMatch(SearchString: string): TStringList;
  end;

implementation

uses
  SysUtils;

{ TSearchString }

constructor TSearchString.Create(Candidates: TObjectList<TFindUnitItem>);
begin
  FCandidates := Candidates;
end;

destructor TSearchString.Destroy;
begin
  inherited;
end;

function TSearchString.GetMatch(SearchString: string): TStringList;
var
  I: Integer;
  Item: TFindUnitItem;
  LocalSearch: TStringList;
  iString: Integer;
  ItemToFind: string;
  LocalSearchUpperCase: TStringList;
begin
  Result := TStringList.Create;
  SearchString := UpperCase(SearchString);

  for I := 0 to FCandidates.Count -1 do
  begin
    Item := FCandidates[i];
    LocalSearch := TStringList.Create;
    LocalSearchUpperCase := TStringList.Create;
    try
      LocalSearch.Text := Item.PrepareToFind;
      LocalSearchUpperCase.Text := UpperCase(LocalSearch.Text);

      for iString := 0 to LocalSearch.Count -1 do
      begin
        ItemToFind := LocalSearchUpperCase[iString];
        if Pos(SearchString, ItemToFind) > 0 then
        begin
         Result.Add(LocalSearch[iString]);
         if Result.Count > 200 then
          Exit;
        end;
      end;
    finally
      LocalSearchUpperCase.Free;
      LocalSearch.Free;
    end;
  end;
end;

end.
