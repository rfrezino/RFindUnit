unit FindUnit.ResultsImportanceCalculator;

interface

uses
	System.Classes;

type
  TResultImportanceCalculator = class(TObject)
  private
    FResult: TStrings;
    FBaseSearchString: string;
    FCurClass: string;
    FMostRelevantIdx: integer;

    function GetCurClass(ClassName: string): string;
    function GetPointsForItem(Item: string): Integer;

    function IsMultiSearch: Boolean;
  public
    procedure Config(Results: TStrings; BaseSearchString: string);

    procedure Process;

    property MostRelevantIdx: integer read FMostRelevantIdx write FMostRelevantIdx;
  end;

implementation

uses
	FindUnit.Utils, System.StrUtils, System.SysUtils;

{ TResultImportanceCalculator }

procedure TResultImportanceCalculator.Config(Results: TStrings; BaseSearchString: string);
begin
  FBaseSearchString := Trim(BaseSearchString);
  FResult := Results;
end;

function TResultImportanceCalculator.GetCurClass(ClassName: string): string;
var
  Base: string;
begin
  Base := ReverseString(FBaseSearchString);

  if TextExists('-', Base, True) then
    Result := Trim(Fetch(Base, '-'));

  Result := ReverseString(Trim(Fetch(Base, '.')));
end;

function TResultImportanceCalculator.GetPointsForItem(Item: string): Integer;
var
  FullClass: string;

  function MatchesFullName: Boolean;
  begin
    Result := FCurClass.ToUpper = GetCurClass(FBaseSearchString).ToUpper;
  end;

begin
  FullClass := '.' + FBaseSearchString;
  if TextExists(FullClass, Item, True) then
  begin
    Result := 50;

    if MatchesFullName then
       Inc(Result, 10);
  end
  else if TextExists(FullClass, Item, False) then
  begin
    Result := 40;

    if MatchesFullName then
       Inc(Result, 5);
  end
  else
  begin
    Result := 0 - Abs(FBaseSearchString.Length - Item.Length);
  end
end;

function TResultImportanceCalculator.IsMultiSearch: Boolean;
begin
  Result := TextExists(' ', FBaseSearchString);
end;

procedure TResultImportanceCalculator.Process;
var
  iItem: Integer;
  MostImportantItemPoints: Integer;
  CurPoints: Integer;
begin
  FMostRelevantIdx := -1;
  FCurClass := GetCurClass(FCurClass);

  MostImportantItemPoints := -100000;
  for iItem := 0 to FResult.Count -1 do
  begin
    CurPoints := GetPointsForItem(FResult[iItem]);
    if CurPoints > MostImportantItemPoints then
    begin
      MostImportantItemPoints := CurPoints;
      FMostRelevantIdx := iItem;
    end;
  end;
end;

end.
