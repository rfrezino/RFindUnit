unit TestPasParserpas;

interface

uses
  Classes,
  SysUtils,
  TestFramework,

  DelphiAST.Classes,

  FindUnit.Header,
  FindUnit.PasParser, FindUnit.Utils,

  Generics.Collections;

type
  TestTPasFileParser = class(TTestCase)
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

    procedure TestAvCase1;
    procedure TestAvCase2;
    procedure TestListReturns;
  end;

implementation

const
  TEST_FILE_NAME = 'TestPasParserFont';

function TestTPasFileParser.CompareListWithText(List: TStrings; ExpectItems: TArray<string>): Boolean;
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

procedure TestTPasFileParser.SetUp;
begin
  FFilePath := ExtractFilePath(ParamStr(0));
  FFilePath := Fetch(FFilePath, '\Test\');
  FFilePath := FFilePath + '\Test\TestPasParser\' + TEST_FILE_NAME + '.pas';
  FPasFileParser := TPasFileParser.Create(FFilePath);
end;

procedure TestTPasFileParser.TearDown;
begin
  FPasFileParser.Free;
  FPasFileParser := nil;
end;

procedure TestTPasFileParser.TestAvCase1;
var
  LoalFile: string;
  LPasFileParser: TPasFileParser;
begin
  LoalFile := ExtractFilePath(ParamStr(0));
  LoalFile := Fetch(LoalFile, '\Test\');
  LoalFile := LoalFile + '\Test\TestPasParser\AV_Case1.pas';
  LPasFileParser := TPasFileParser.Create(LoalFile);
  LPasFileParser.Process;
end;

procedure TestTPasFileParser.TestAvCase2;
var
  LoalFile: string;
  LPasFileParser: TPasFileParser;
begin
  LoalFile := ExtractFilePath(ParamStr(0));
  LoalFile := Fetch(LoalFile, '\Test\');
  LoalFile := LoalFile + '\Test\TestPasParser\AV_Case2.pas';
  LPasFileParser := TPasFileParser.Create(LoalFile);
  LPasFileParser.Process;
end;

procedure TestTPasFileParser.TestListReturns;
var
  ListType: TListType;
  List: TStringList;
  Item: TPasFile;
begin
  Item := TPasFile.Create;
  for ListType := Low(TListType) to High(TListType) do
  begin
    List := Item.GetListFromType(ListType);
    Assert(Assigned(List), 'List not assigned');
  end;
end;

procedure TestTPasFileParser.TestParse;
var
  PasResult: TPasFile;
begin
  PasResult := FPasFileParser.Process;

  Assert(PasResult.FilePath = FFilePath, 'Wrong file path');
  Assert(PasResult.OriginUnitName = TEST_FILE_NAME, 'Wrong file path');

  Assert((PasResult.Procedures.Count = 1)
    and (CompareListWithText(PasResult.Procedures, ['YesOutProcedure'])),
    'Wrong procedure list');

  Assert((PasResult.Functions.Count = 1)
    and (CompareListWithText(PasResult.Functions, ['YesOutFunction'])),
    'Wrong function list');

  Assert((PasResult.Constants.Count = 2)
    and (CompareListWithText(PasResult.Constants, ['TEST_CONT', 'TEST_CONT_BREAK'])),
    'Wrong const list');

  Assert((PasResult.Classes.Count = 3)
    and CompareListWithText(PasResult.Classes,
      ['TDescendantClass.* - Class', 'TTest1.* - Class','TTest2.* - Class']) ,
      'Wrong class list');

  Assert((PasResult.ClassFunctions.Count = 3)
    and (CompareListWithText(PasResult.ClassFunctions,
    ['TDescendantClass.DescendantClassShow - Class Function',
      'TTest1.YesShowFunction - Class Function',
      'TTest2.YesShowFunction - Class Function'])),
    'Wrong class function list');

  Assert((PasResult.ClassProcedure.Count = 2)
    and (CompareListWithText(PasResult.ClassProcedure,
      ['TTest1.YesShow - Class Procedure',
      'TTest2.YesShow - Class Procedure'])),
    'Wrong class procedure list');

  Assert((PasResult.Enumerators.Count = 10)
    and CompareListWithText(PasResult.Enumerators,
      ['TestEnum - Enum','TestEnum.teOne - Enum item',
      'TestEnum.teThree - Enum item',
      'TestEnum.teTwo - Enum item',
      'TestEnumValue - Enum',
      'TestEnumValue.tevOne - Enum item',
      'TestEnumValue.tevThree - Enum item',
      'TestEnumValue.tevTwo - Enum item',
      'TestSet - Set','TestSetNew - Set']),
    'Wrong list of enums');

  Assert((PasResult.Interfaces.Count = 1)
    and (CompareListWithText(PasResult.Interfaces,
      ['IOtherInterface.* - Interface'])),
    'Wrong interfaes list');

  Assert(((PasResult.Records.Count = 1)
    and (CompareListWithText(PasResult.Records,
    ['TRecordWin.* - Record']))),
    'Records list is not correct');

  Assert((PasResult.References.Count = 4)
    and (CompareListWithText(PasResult.References,
    ['TFunctionTestEvent.* - Function Reference',
     'TObjectConverterAv.* - Reference',
     'TProcedureTestEvent.* - Procedure Reference',
     'TTestShortCut.* - Sub Range'])),
    'Wrong reference list');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTPasFileParser.Suite);
end.

