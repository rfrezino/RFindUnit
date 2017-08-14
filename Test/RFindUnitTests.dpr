program RFindUnitTests;
{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  System.SysUtils,
  DUnitTestRunner,
  TestUnsedUses in 'TestUnsedUses.pas',
  TestPasParserpas in 'TestPasParserpas.pas',
  TestPasParserFont in 'TestPasParser\TestPasParserFont.pas',
  TestSearchString in 'TestSearchString.pas',
  AV_Case1 in 'TestPasParser\AV_Case1.pas',
  Log4Pascal in '..\Thirdy\Log4pascal\Log4Pascal.pas',
  AV_Case2 in 'TestPasParser\AV_Case2.pas',
  TestFindUnitUtils in 'TestFindUnitUtils.pas',
  BaseUnusedUnitsSetAllUnuse in 'TestUnusedUses\BaseUnusedUnitsSetAllUnuse.pas',
  BaseUnusedUnitsSetAllUse in 'TestUnusedUses\BaseUnusedUnitsSetAllUse.pas',
  Interf.EnvironmentController in '..\Interfaces\Interf.EnvironmentController.pas',
  TestSpringParser in 'TestSpringParser.pas';

{$R *.RES}

begin
  Logger := TLogger.Create(ExtractFilePath(ParamStr(0)) + 'test.log');
  DUnitTestRunner.RunRegisteredTests;
end.

