unit FindUnit.ResultsImportanceCalculator;

interface

uses
  FindUnit.Settings,

  System.Classes,
  System.Math;

type
  TResultImportanceCalculator = class(TObject)
  private
    FResult: TStrings;
    FBaseSearchString: string;
    FCurClass: string;
    FMostRelevantIdx: integer;

    function GetCurClass(ClassName: string): string;
    function GetPointsForItem(Item: string): Integer;

    function LevenshteinDistance(Item1, Item2: string): integer;
    function RodrigoDistanceCalculator(Item1, Item2: string): Integer;
  public
    procedure Config(Results: TStrings; BaseSearchString: string);

    procedure Process;

    property MostRelevantIdx: integer read FMostRelevantIdx write FMostRelevantIdx;
  end;

implementation

uses
  FindUnit.Utils,

  System.StrUtils,
  System.SysUtils;

{ TResultImportanceCalculator }


function TResultImportanceCalculator.LevenshteinDistance(Item1, Item2: string): integer;
var
  d : array of array of integer;
  i,j,cost : integer;
begin
  {
  Compute the edit-distance between two strings.
  Algorithm and description may be found at either of these two links:
  http://en.wikipedia.org/wiki/Levenshtein_distance
  http://www.google.com/search?q=Levenshtein+distance
  }

  //initialize our cost array
  SetLength(d,Length(Item1)+1);
  for i := Low(d) to High(d) do begin
    SetLength(d[i],Length(Item2)+1);
  end;

  for i := Low(d) to High(d) do begin
    d[i,0] := i;
    for j := Low(d[i]) to High(d[i]) do begin
      d[0,j] := j;
    end;
  end;

  //store our costs in a 2-d grid
  for i := Low(d)+1 to High(d) do begin
    for j := Low(d[i])+1 to High(d[i]) do begin
      if Item1[i] = Item2[j] then begin
        cost := 0;
      end
      else begin
        cost := 1;
      end;

      //to use "Min", add "Math" to your uses clause!
      d[i,j] := Min(Min(
                 d[i-1,j]+1,      //deletion
                 d[i,j-1]+1),     //insertion
                 d[i-1,j-1]+cost  //substitution
                 );
    end;  //for j
  end;  //for i

  //now that we've stored the costs, return the final one
  Result := d[Length(Item1),Length(Item2)];

  //dynamic arrays are reference counted.
  //no need to deallocate them
end;


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
begin
  if GlobalSettings.UseDefaultSearchMatch then
    Result := RodrigoDistanceCalculator(Item, FBaseSearchString)
  else
    Result := LevenshteinDistance(Item, FBaseSearchString);
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

function TResultImportanceCalculator.RodrigoDistanceCalculator(Item1,
  Item2: string): Integer;
var
  FullClass: string;

  function MatchesFullName: Boolean;
  begin
    Result := FCurClass.ToUpper = GetCurClass(Item2).ToUpper;
  end;

  function MatchesPlusSpace: Boolean;
  begin
    Result := String(Item1.ToUpper + ' ').Contains(GetCurClass(Item2).ToUpper + ' ');
  end;

begin
  FullClass := '.' + Item2;
  if TextExists(FullClass, Item1, True) then
  begin
    Result := 1000 + (1000 - Length(Item1));

    if MatchesFullName then
       Inc(Result, 100);

    if MatchesPlusSpace then
       Inc(Result, 100);
  end
  else if TextExists(FullClass, Item1, False) then
  begin
    Result := 100 + (100 - Length(Item1));

    if MatchesFullName then
       Inc(Result, 50);

    if MatchesPlusSpace then
       Inc(Result, 50);
  end
  else
  begin
    Result := 0 - Item1.Length;

    if MatchesFullName then
       Inc(Result, 5);

    if MatchesPlusSpace then
       Inc(Result, 5);
  end
end;

end.
