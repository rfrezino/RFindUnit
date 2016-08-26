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

    procedure GetCurClass;
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

procedure TResultImportanceCalculator.GetCurClass;
var
  Base: string;
begin
  Base := ReverseString(FBaseSearchString);

  if TextExists('-', Base, True) then
    FCurClass := Trim(Fetch(Base, '-'));

  FCurClass := ReverseString(Trim(Fetch(Base, '.')));
end;

function TResultImportanceCalculator.GetPointsForItem(Item: string): Integer;
const
  CONV_STR = '@$%';
var
  CurString: string;
  FullClass: string;

  function MatchesFullName(CaseSensitive: Boolean): Boolean;
  begin
    if CaseSensitive then
      CurString := StringReplace(FBaseSearchString, FullClass, CONV_STR, [])
    else
      CurString := StringReplace(FBaseSearchString, FullClass, CONV_STR, [rfIgnoreCase]);

    CurString := Fetch(CurString, CONV_STR);
    Result := CurString.IsEmpty;
  end;

begin
  FullClass := '.' + Item;
  if TextExists(FullClass, FBaseSearchString, True) then
  begin
    Result := 50;

    if MatchesFullName(True) then
       Inc(Result, 10);
  end
  else if TextExists(FullClass, FBaseSearchString, False) then
  begin
    Result := 40;

    if MatchesFullName(False) then
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
  GetCurClass;

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
