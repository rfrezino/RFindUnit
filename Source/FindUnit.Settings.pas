unit FindUnit.Settings;

interface

uses
	System.IniFiles, System.Classes;

type
  TCacheSettings = record
    AutoImportEnabled: Boolean;
    AlwaysUseInterfaceSection: Boolean;
    StoreChoices: Boolean;
    BreakLine: Boolean;
    SortUsesAfterAdding: Boolean;
    UseDefaultSearchMatch: Boolean;
  end;

  TSettings = class(TObject)
  private
    FIni: TIniFile;

    function GetAutoImportEnabled: Boolean;
    procedure SetAutoImportEnabled(const Value: Boolean);

    function GetAutoImportValue: TStrings;
    procedure SetAutoImportValue(const Value: TStrings);

    function GetAlwaysUseInterfaceSection: Boolean;
    procedure SetAlwaysUseInterfaceSection(const Value: Boolean);

    function GetStoreChoices: Boolean;
    procedure SetStoreChoices(const Value: Boolean);

    function GetBreakLine: Boolean;
    procedure SetBreakLine(const Value: Boolean);

    function GetSortUsesAfterAdding: Boolean;
    procedure SetSortUsesAfterAdding(const Value: Boolean);

    function GetUseDefaultSearchMatch: Boolean;
    procedure SetUseDefaultSearchMatch(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property AutoImportEnabled: Boolean read GetAutoImportEnabled write SetAutoImportEnabled;
    property AutoImportValue: TStrings read GetAutoImportValue write SetAutoImportValue;
    property AlwaysUseInterfaceSection: Boolean read GetAlwaysUseInterfaceSection write SetAlwaysUseInterfaceSection;
    property StoreChoices: Boolean read GetStoreChoices write SetStoreChoices;
    property BreakLine: Boolean read GetBreakLine write SetBreakLine;
    property SortUsesAfterAdding: Boolean read GetSortUsesAfterAdding write SetSortUsesAfterAdding;
    property UseDefaultSearchMatch: Boolean read GetUseDefaultSearchMatch write SetUseDefaultSearchMatch;

    class function GetCacheSettings: TCacheSettings;
    class procedure ReloadSettings;
    class function SettingsFilePath: string;
  end;

var
  GlobalSettings: TCacheSettings;

implementation

uses
	FindUnit.Utils, FindUnit.Header;

const
  SETTINGS_SECTION = 'SETTINGS';
  CONF_AUTOIMPORT_ENABLED = 'AUTOIMPORT_ENABLED';
  CONF_ALWAYSUSEINTERFACESECTION_ENABLED = 'ALWAYSUSEINTERFACESECTION_ENABLED';
  CONF_STORE_CHOICES_ENABLED = 'STORE_CHOICES_ENABLED';
  CONF_BREAK_LINE = 'BREAK_LINE';
  CONF_SORT_AFTER_ADDING = 'SORT_AFTER_ADDING';
  CONF_DEFAULT_SORT_MATCH = 'DEFAULT_SORT_MATCH';

{ TSettings }

class function TSettings.GetCacheSettings: TCacheSettings;
var
  Settings: TSettings;
begin
  Settings := TSettings.Create;
  try
    Result.AutoImportEnabled := Settings.AutoImportEnabled;
    Result.AlwaysUseInterfaceSection := Settings.AlwaysUseInterfaceSection;
    Result.StoreChoices := Settings.StoreChoices;
    Result.BreakLine := Settings.BreakLine;
    Result.SortUsesAfterAdding := Settings.SortUsesAfterAdding;
    Result.UseDefaultSearchMatch := Settings.UseDefaultSearchMatch;
  finally
    Settings.Free;
  end;
end;

constructor TSettings.Create;
begin
  FIni := TIniFile.Create(SettingsFilePath);
end;

destructor TSettings.Destroy;
begin
  FIni.Free;
  inherited;
end;

function TSettings.GetAlwaysUseInterfaceSection: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_ALWAYSUSEINTERFACESECTION_ENABLED, False);
end;

function TSettings.GetAutoImportEnabled: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_AUTOIMPORT_ENABLED, True);
end;

function TSettings.GetAutoImportValue: TStrings;
begin
  Result := TStringList.Create;
  FIni.ReadSectionValues(AUTO_IMPORT_SECTION, Result);
end;

function TSettings.GetBreakLine: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_BREAK_LINE, False);
end;

function TSettings.GetSortUsesAfterAdding: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_SORT_AFTER_ADDING, False);
end;

function TSettings.GetStoreChoices: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_STORE_CHOICES_ENABLED, True);
end;

function TSettings.GetUseDefaultSearchMatch: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_DEFAULT_SORT_MATCH, True);
end;

class procedure TSettings.ReloadSettings;
begin
  GlobalSettings := GetCacheSettings;
end;

procedure TSettings.SetAlwaysUseInterfaceSection(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_ALWAYSUSEINTERFACESECTION_ENABLED, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetAutoImportEnabled(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_AUTOIMPORT_ENABLED, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetAutoImportValue(const Value: TStrings);
var
  I: Integer;
begin
  FIni.EraseSection(AUTO_IMPORT_SECTION);
  for I := 0 to Value.Count -1 do
    FIni.WriteString(AUTO_IMPORT_SECTION, Value.Names[i], Value.ValueFromIndex[i]);

  FIni.UpdateFile;
end;

procedure TSettings.SetBreakLine(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_BREAK_LINE, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetSortUsesAfterAdding(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_SORT_AFTER_ADDING, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetStoreChoices(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_STORE_CHOICES_ENABLED, Value);
  FIni.UpdateFile;
end;

class function TSettings.SettingsFilePath: string;
begin
  Result := FindUnitDir + AUTO_IMPORT_FILE;
end;

procedure TSettings.SetUseDefaultSearchMatch(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_DEFAULT_SORT_MATCH, Value);
  FIni.UpdateFile;
end;

end.
