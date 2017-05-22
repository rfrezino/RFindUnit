unit TestUnsedUses;

interface

uses
  TestFramework, System.Generics.Collections, DelphiAST.Consts, DelphiAST.Classes,
  FindUnit.Settings, FindUnit.PasParser,
  FindUnit.DelphiReservedWords, FindUnit.Utils, SimpleParser.Lexer.Types,
  System.Classes, System.SysUtils, DelphiAST.Writer, DelphiAST, Log4Pascal,
  FindUnit.UnusedUses,
  Interf.EnvironmentController;

type
  TTestEnv = class(TInterfacedObject, IRFUEnvironmentController)
    function GetFullMatch(const SearchString: string): TStringList;
  end;

  TestTUnsedUsesProcessor = class(TTestCase)
  strict private
    FUnsedUsesProcessor: TUnsedUsesProcessor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
  end;

implementation

procedure TestTUnsedUsesProcessor.SetUp;
var
  Path: string;
begin
  Path := ExtractFilePath(ParamStr(0));
  Path := Fetch(Path, '\Test\');
  FUnsedUsesProcessor := TUnsedUsesProcessor.Create(Path + '\Test\TestUnusedUses\FindUnit.FileCache.pas');
end;

procedure TestTUnsedUsesProcessor.TearDown;
begin
  FUnsedUsesProcessor.Free;
  FUnsedUsesProcessor := nil;
end;

procedure TestTUnsedUsesProcessor.TestProcess;
var
  EnvControl: IRFUEnvironmentController;
  ReturnValue: string;
  UsesUnit: string;
  ReturnInfo: TUsesUnit;
  IsResultOk: Boolean;
begin
  EnvControl := TTestEnv.Create;
  FUnsedUsesProcessor.SetEnvControl(EnvControl);
  FUnsedUsesProcessor.Process;

  UsesUnit := 'Winapi.UI.Xaml';
  UsesUnit := UsesUnit.ToUpper;

  if not FUnsedUsesProcessor.UnusedUses.TryGetValue(UsesUnit, ReturnInfo) then
    Assert(false, 'Result not found');

  IsResultOk := (ReturnInfo.Line = 12) and (ReturnInfo.Collumn = 3) and (ReturnInfo.Name.Equals('Winapi.UI.Xaml'));
  Assert(IsResultOk, 'Result is not correct');
end;

{ TTestEnv }

function TTestEnv.GetFullMatch(const SearchString: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Add('FindUnit.FileCache.TUnitsController - Class');
  Result.Add('FindUnit.PasParser.TPasFile - Class');
  Result.Add('System.Classes.TStringList - Class');
  Result.Add('System.SyncObjs.TCriticalSection - Class');
  Result.Add('System.TObject - Class');
  Result.Add('EClasses.TDictionary - Class');
  Result.Add('Spring.Collections.Dictionaries.TDictionary - Class');
  Result.Add('System.Generics.Collections.TDictionary - Class');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTUnsedUsesProcessor.Suite);
end.

