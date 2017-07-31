unit TestUnsedUses;

interface

uses
  TestFramework,

  FindUnit.UnusedUses,
  FindUnit.Utils,

  Interf.EnvironmentController,

  System.Classes,
  System.SysUtils;

type
  TTestEnv = class(TInterfacedObject, IRFUEnvironmentController)
    function GetFullMatch(const SearchString: string): TStringList;
    function AreDependenciasReady: Boolean;
    procedure ForceRunDependencies;
  end;

  TestTUnsedUsesProcessor = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
    procedure TestRegularBase;
  end;

implementation

procedure TestTUnsedUsesProcessor.SetUp;
begin
end;

procedure TestTUnsedUsesProcessor.TearDown;
begin

end;

procedure TestTUnsedUsesProcessor.TestProcess;
var
  EnvControl: IRFUEnvironmentController;
  ReturnValue: string;
  UsesUnit: string;
  ReturnInfo: TUsesUnit;
  IsResultOk: Boolean;
  Path: string;
  UnsedUsesProcessor: TUnsedUsesProcessor;
begin
  Path := ExtractFilePath(ParamStr(0));
  Path := Fetch(Path, '\Test\');
  UnsedUsesProcessor := TUnsedUsesProcessor.Create(Path + '\Test\TestUnusedUses\FindUnit.FileCache.pas');
  try
    EnvControl := TTestEnv.Create;
    UnsedUsesProcessor.SetEnvControl(EnvControl);
    UnsedUsesProcessor.Process;

    UsesUnit := 'Winapi.UI.Xaml';
    UsesUnit := UsesUnit.ToUpper;

    if not UnsedUsesProcessor.UnusedUses.TryGetValue(UsesUnit, ReturnInfo) then
      Assert(false, 'Result not found');

    IsResultOk := (ReturnInfo.Line = 12) and (ReturnInfo.Collumn = 3) and (ReturnInfo.Name.Equals('Winapi.UI.Xaml'));
    Assert(IsResultOk, 'Result is not correct');

  finally
    UnsedUsesProcessor.Free;
    UnsedUsesProcessor := nil;
  end;
end;

procedure TestTUnsedUsesProcessor.TestRegularBase;
var
  EnvControl: IRFUEnvironmentController;
  Path: string;
  UnsedUsesProcessor: TUnsedUsesProcessor;
begin
  Path := ExtractFilePath(ParamStr(0));
  Path := Fetch(Path, '\Test\');
  UnsedUsesProcessor := TUnsedUsesProcessor.Create(Path + '\Test\TestUnusedUses\BaseUnusedUnitsSetAllUse.pas');
  try
    EnvControl := TTestEnv.Create;
    UnsedUsesProcessor.SetEnvControl(EnvControl);
    UnsedUsesProcessor.Process;

    Assert(UnsedUsesProcessor.UsesStartLine = 5, 'Wrong start uses line');

    Assert(UnsedUsesProcessor.UnusedUses.Count = 0, 'All the units must be marked as found');
  finally
    UnsedUsesProcessor.Free;
    UnsedUsesProcessor := nil;
  end;
end;

{ TTestEnv }

function TTestEnv.AreDependenciasReady: Boolean;
begin
  Result := True;
end;

procedure TTestEnv.ForceRunDependencies;
begin

end;

function TTestEnv.GetFullMatch(const SearchString: string): TStringList;
begin
  Result := TStringList.Create;
  if SearchString = '' then
    Exit
  else if SearchString.ToUpper = 'REVERSESTRING' then
  begin
    Result.Add('OWideSupp.OReverseString - Function');
    Result.Add('System.AnsiStrings.AnsiReverseString - Function');
    Result.Add('System.AnsiStrings.ReverseString - Function');
    Result.Add('System.StrUtils.AnsiReverseString - Function');
    Result.Add('System.StrUtils.ReverseString - Function');
  end
  else if SearchString.ToUpper = 'TLIST' then
  begin
    Result.Add('Spring.Collections.Lists.TList.* - Class');
    Result.Add('System.Classes.TList.* - Class');
    Result.Add('System.Classes.TList.* - Interface');
    Result.Add('System.Classes.TList.Error - Class produre - Class Procedure');
    Result.Add('System.Generics.Collections.TList.* - Class');
    Result.Add('System.Generics.Collections.TList.Error - Class produre - Class Procedure');
  end
  else if SearchString.ToUpper = 'TDICTIONARY' then
  begin
    Result.Add('EClasses.TDictionary.* - Class');
    Result.Add('OXmlRTTISerialize.TSerializableObjectDictionary.* - Class');
    Result.Add('Spring.Collections.Dictionaries.TDictionary.* - Class');
    Result.Add('System.Generics.Collections.TDictionary.* - Class');
    Result.Add('System.Generics.Collections.TObjectDictionary.* - Class');
    Result.Add('Winapi.AspTlb.IRequestDictionary.* - Interface');
    Result.Add('Winapi.AspTlb.IVariantDictionary.* - Interface');
  end
  else if SearchString.ToUpper = 'FORMAT' then
  begin
    Result.Add('System.AnsiStrings.FormatBuf - Function');
    Result.Add('System.JSON.Types.TJsonDateFormatHandling.FormatSettings - Enum item');
    Result.Add('System.MaskUtils.FormatMaskText - Function');
    Result.Add('System.SysUtils.Format - Function');
    Result.Add('System.SysUtils.FormatBuf - Function');
    Result.Add('System.SysUtils.FormatCurr - Function');
    Result.Add('System.SysUtils.FormatDateTime - Function');
    Result.Add('System.SysUtils.FormatFloat - Function');
    Result.Add('System.SysUtils.FormatSettings - Variable');
    Result.Add('Winapi.DirectShow9.FORMAT_AnalogVideo - Constant');
    Result.Add('Winapi.DirectShow9.FORMAT_DolbyAC3 - Constant');
  end
  else if SearchString.ToUpper = 'TRECT' then
  begin
    Result.Add('FMX.Ani.TRectAnimation.* - Class');
    Result.Add('FMX.InertialMovement.TRectD - Record');
    Result.Add('FMX.Objects.TRectangle.* - Class');
    Result.Add('FMX.Objects3D.TRectangle3D.* - Class');
    Result.Add('FmxAnimationEditors.TRectAnimationProperty.* - Class');
    Result.Add('FmxAnimationEditors.TRectAnimationPropertyName.* - Class');
    Result.Add('System.Types.TRect - Record');
    Result.Add('System.Types.TRectF - Record');
  end
  else
  begin
    Result.Add('FindUnit.FileCache.TUnitsController - Class');
    Result.Add('FindUnit.PasParser.TPasFile - Class');
    Result.Add('System.Classes.TStringList - Class');
    Result.Add('System.SyncObjs.TCriticalSection - Class');
    Result.Add('System.TObject - Class');
    Result.Add('EClasses.TDictionary - Class');
    Result.Add('Spring.Collections.Dictionaries.TDictionary - Class');
    Result.Add('System.Generics.Collections.TDictionary - Class');
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTUnsedUsesProcessor.Suite);
end.

