unit TestSpringParser;

interface

uses
  Classes,
  SysUtils,
  TestFramework,

  DelphiAST.Classes,

  FindUnit.Header,
  FindUnit.PasParser,

  Generics.Collections;

type
  TestTSpringParser = class(TTestCase)
  strict private
    FPasFileParser: TPasFileParser;
  private
    FFilePath: string;

    function CompareListWithText(List: TStrings; ExpectItems: TArray<string>): Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParse;
  end;

implementation

uses
  FindUnit.Utils;

const
  TEST_FILE_NAME = 'Spring';

{ TestTSpringParser }

function TestTSpringParser.CompareListWithText(List: TStrings; ExpectItems: TArray<string>): Boolean;
var
  I: Integer;
begin
  Result := List.Count = Length(ExpectItems);
  if not Result then
    Exit;

  for I := 0 to Length(ExpectItems) -1 do
    if List.IndexOf(ExpectItems[I]) = -1 then
      Exit(False);
end;

procedure TestTSpringParser.SetUp;
begin
  FFilePath := ExtractFilePath(ParamStr(0));
  FFilePath := Fetch(FFilePath, '\Test\');
  FFilePath := FFilePath + '\Test\TestPasParser\' + TEST_FILE_NAME + '.pas';
  FPasFileParser := TPasFileParser.Create(FFilePath);
end;

procedure TestTSpringParser.TearDown;
begin
  FPasFileParser.Free;
  FPasFileParser := nil;
end;

procedure TestTSpringParser.TestParse;
var
  PasResult: TPasFile;
begin
  PasResult := FPasFileParser.Process;

  Assert(PasResult.FilePath = FFilePath, 'Wrong file path');
  Assert(PasResult.OriginUnitName = TEST_FILE_NAME, 'Wrong file path');

end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTSpringParser.Suite);

end.
