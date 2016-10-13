unit FindUnit.Settings;

interface

uses
	System.IniFiles, System.Classes;

type
  TCacheSettings = record
    AutoImportEnabled: Boolean;
    AlwaysUseInterfaceSection: Boolean;
    StoreChoices: Boolean;
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
  public
    constructor Create;
    destructor Destroy; override;

    property AutoImportEnabled: Boolean read GetAutoImportEnabled write SetAutoImportEnabled;
    property AutoImportValue: TStrings read GetAutoImportValue write SetAutoImportValue;
    property AlwaysUseInterfaceSection: Boolean read GetAlwaysUseInterfaceSection write SetAlwaysUseInterfaceSection;
    property StoreChoices: Boolean read GetStoreChoices write SetStoreChoices;

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
  AUTOIMPORT_ENABLED = 'AUTOIMPORT_ENABLED';
  ALWAYSUSEINTERFACESECTION_ENABLED = 'ALWAYSUSEINTERFACESECTION_ENABLED';
  STORE_CHOICES_ENABLED = 'STORE_CHOICES_ENABLED';


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
  Result := FIni.ReadBool(SETTINGS_SECTION, ALWAYSUSEINTERFACESECTION_ENABLED, False);
end;

function TSettings.GetAutoImportEnabled: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, AUTOIMPORT_ENABLED, True);
end;

function TSettings.GetAutoImportValue: TStrings;
begin
  Result := TStringList.Create;
  FIni.ReadSectionValues(AUTO_IMPORT_SECTION, Result);
end;

function TSettings.GetStoreChoices: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, STORE_CHOICES_ENABLED, True);
end;

class procedure TSettings.ReloadSettings;
begin
  GlobalSettings := GetCacheSettings;
end;

procedure TSettings.SetAlwaysUseInterfaceSection(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, ALWAYSUSEINTERFACESECTION_ENABLED, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetAutoImportEnabled(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, AUTOIMPORT_ENABLED, Value);
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

procedure TSettings.SetStoreChoices(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, STORE_CHOICES_ENABLED, Value);
  FIni.UpdateFile;
end;

class function TSettings.SettingsFilePath: string;
begin
  Result := FindUnitDir + AUTO_IMPORT_FILE;
end;

end.
