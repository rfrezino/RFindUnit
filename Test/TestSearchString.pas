unit TestSearchString;

interface

uses
  TestFramework, Log4Pascal, FindUnit.SearchString, System.SyncObjs,
  Generics.Collections, Classes, FindUnit.Header, FindUnit.PasParser,
  FindUnit.Utils,
  System.SysUtils,
  System.Generics.Collections;

type
  TestTSearchString = class(TTestCase)
  strict private
    FSearchString: TSearchString;
    FPasFiles: TDictionary<TFilePath, TPasFile>;
  private
    FFilePath: string;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetMatch;
    procedure TestGetMatchEmpty;
    procedure TestGetFullMatch;
  end;

implementation

const
  TEST_FILE_NAME = 'TestPasParserFont';

procedure TestTSearchString.SetUp;
var
  PasParser: TPasFileParser;
begin
  FFilePath := ExtractFilePath(ParamStr(0));
  FFilePath := Fetch(FFilePath, '\Test\');
  FFilePath := FFilePath + '\Test\TestPasParser\' + TEST_FILE_NAME + '.pas';
  FPasFiles := TDictionary<string, TPasFile>.Create;

  PasParser := TPasFileParser.Create(FFilePath);
  FPasFiles.Add(FFilePath, PasParser.Process);
  PasParser.Free;
  FSearchString := TSearchString.Create(FPasFiles);
end;

procedure TestTSearchString.TearDown;
begin
  FSearchString.Free;
  FSearchString := nil;
end;

procedure TestTSearchString.TestGetMatch;
var
  ReturnValue: TStringList;
  SearchString: string;
begin
  SearchString := 'Test';
  ReturnValue := FSearchString.GetMatch(SearchString);
  Assert(ReturnValue.Count = 25);
  ReturnValue.Free;
end;

procedure TestTSearchString.TestGetMatchEmpty;
var
  ReturnValue: TStringList;
  SearchString: string;
begin
  SearchString := '';
  ReturnValue := FSearchString.GetMatch(SearchString);
  Assert(ReturnValue.Count = 0);
  ReturnValue.Free;
end;

procedure TestTSearchString.TestGetFullMatch;
var
  ReturnValue: TStringList;
  SearchString: string;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSearchString.GetFullMatch(SearchString);
  // TODO: Validate method results
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTSearchString.Suite);
end.

