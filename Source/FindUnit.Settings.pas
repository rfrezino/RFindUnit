unit FindUnit.Settings;

interface

uses
  System.Classes,

  System.IniFiles;

type
  TCacheSettings = record
    AutoImportEnabled: Boolean;
    AlwaysUseInterfaceSection: Boolean;
    StoreChoices: Boolean;
    BreakLine: Boolean;
    SortUsesAfterAdding: Boolean;
    UseDefaultSearchMatch: Boolean;
    BlankLineBtwNamespaces: Boolean;
    OrganizeUses: Boolean;
    OrganizeUsesAfterAddingNewUses: Boolean;
    BreakUsesLineAt: Cardinal;
    GroupNonNamespaceUnits: Boolean;
    SettingFormWidth: Cardinal;
    SettingFormHeight: Cardinal;
    SettingFormStartPosX: Cardinal;
    SettingFormStartPosY: Cardinal;
    IgnoreUsesUnused: string;
    EnableExperimentalFindUnusedUses: Boolean;
    BreakLineForNonDomainUses: Boolean;
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

    function GetBlankLineBtwNameScapes: Boolean;
    procedure SetBlankLineBtwNameScapes(const Value: Boolean);

    function GetOrganizeUses: Boolean;
    procedure SetOrganizeUses(const Value: Boolean);

    function GetBreakUsesLineAtPosition: Cardinal;
    procedure SetBreakUsesLineAtPosition(const Value: Cardinal);

    function GetOrganizeUsesAfterAddingNewUsesUnit: Boolean;
    procedure SetOrganizeUsesAfterAddingNewUsesUnit(const Value: Boolean);

    function GetGroupNonNamespaceUnits: Boolean;
    procedure SetGroupNonNamespaceUnits(const Value: Boolean);

    function GetSettingFormHeight: Cardinal;
    procedure SetSettingFormHeight(const Value: Cardinal);

    function GetSettingFormStartPosX: Cardinal;
    procedure SetSettingFormStartPosX(const Value: Cardinal);

    function GetSettingFormStartPosY: Cardinal;
    procedure SetSettingFormStartPosY(const Value: Cardinal);

    function GetSettingFormWidth: Cardinal;
    procedure SetSettingFormWidth(const Value: Cardinal);

    function GetIgnoreUsesUnused: string;
    procedure SetUsesUnused(const Value: string);

    function GetEnableExperimentalFindUnusedUses: Boolean;
    procedure SetEnableExperimentalFindUnusedUses(const Value: Boolean);
    function GetBreakLineForNonDomainUses: Boolean;
    procedure SetBreakLineForNonDomainUses(const Value: Boolean);
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
    property BlankLineBtwNameScapes: Boolean read GetBlankLineBtwNameScapes write SetBlankLineBtwNameScapes;
    property OrganizeUses: Boolean read GetOrganizeUses write SetOrganizeUses;
    property BreakUsesLineAtPosition: Cardinal read GetBreakUsesLineAtPosition write SetBreakUsesLineAtPosition;
    property OrganizeUsesAfterAddingNewUsesUnit: Boolean read GetOrganizeUsesAfterAddingNewUsesUnit write SetOrganizeUsesAfterAddingNewUsesUnit;
    property GroupNonNamespaceUnits: Boolean read GetGroupNonNamespaceUnits write SetGroupNonNamespaceUnits;
    property SettingFormWidth: Cardinal read GetSettingFormWidth write SetSettingFormWidth;
    property SettingFormHeight: Cardinal read GetSettingFormHeight write SetSettingFormHeight;
    property SettingFormStartPosX: Cardinal read GetSettingFormStartPosX write SetSettingFormStartPosX;
    property SettingFormStartPosY: Cardinal read GetSettingFormStartPosY write SetSettingFormStartPosY;
    property IgnoreUsesUnused: string read GetIgnoreUsesUnused write SetUsesUnused;
    property EnableExperimentalFindUnusedUses: Boolean read GetEnableExperimentalFindUnusedUses write SetEnableExperimentalFindUnusedUses;
    property BreakLineForNonDomainUses: Boolean read GetBreakLineForNonDomainUses write SetBreakLineForNonDomainUses;

    class function GetCacheSettings: TCacheSettings;
    class procedure ReloadSettings;
    class function SettingsFilePath: string;
  end;

var
  GlobalSettings: TCacheSettings;

implementation

uses
  FindUnit.Header,
  FindUnit.Utils;

const
  SETTINGS_SECTION = 'SETTINGS';
  SEARCHFORM_SETTINGS_SECTION = 'SEARCHFORMSETTINGS';

  AUTO_IMPORT_SECTION = 'MEMORIZEDUNIT';

  CONF_AUTOIMPORT_ENABLED = 'AUTOIMPORT_ENABLED';
  CONF_ALWAYSUSEINTERFACESECTION_ENABLED = 'ALWAYSUSEINTERFACESECTION_ENABLED';
  CONF_STORE_CHOICES_ENABLED = 'STORE_CHOICES_ENABLED';
  CONF_BREAK_LINE = 'BREAK_LINE';
  CONF_SORT_AFTER_ADDING = 'SORT_AFTER_ADDING';
  CONF_DEFAULT_SORT_MATCH = 'DEFAULT_SORT_MATCH';
  CONF_BLANKLINE_BTW_NAMESCAPE = 'BLANKLINE_BTW_NAMESCAPE';
  CONF_ORGANIZE_USES = 'ORGANIZE_USES';
  CONF_BREAK_USES_LINE_AT_POSITION = 'BREAK_USES_LINE_AT_POSITION';
  CONF_ORGANIZE_USES_AFTER_ADDING_NEW_USES_UNIT = 'ORGANIZE_USES_AFTER_ADDING_NEW_USES_UNIT';
  CONF_GROUP_NONNAMESPACE_UNITS = 'GROUP_NONNAMESPACE_UNITS';
  CONF_FORM_SETTINGS_WIDTH = 'FORM_SETTINGS_WIDTH';
  CONF_FORM_SETTINGS_HEIGHT = 'FORM_SETTINGS_HEIGHT';
  CONF_FORM_SETTINGS_START_X = 'FORM_SETTINGS_START_X';
  CONF_FORM_SETTINGS_START_Y = 'FORM_SETTINGS_START_Y';
  CONF_IGNORED_USES = 'IGNORED_USES';
  CONF_EXPRIMENTAL_UNUSED_USES = 'EXPRIMENTAL_UNUSED_USES';
  CONF_BREAK_LINE_NON_DOMAIN_USES = 'BREAK_LINE_NON_DOMAIN_USES';

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
    Result.BlankLineBtwNamespaces := Settings.BlankLineBtwNameScapes;
    Result.OrganizeUses := Settings.OrganizeUses;
    Result.BreakUsesLineAt := Settings.BreakUsesLineAtPosition;
    Result.OrganizeUsesAfterAddingNewUses := Settings.OrganizeUsesAfterAddingNewUsesUnit;
    Result.GroupNonNamespaceUnits := Settings.GroupNonNamespaceUnits;
    Result.SettingFormWidth := Settings.SettingFormWidth;
    Result.SettingFormHeight := Settings.SettingFormHeight;
    Result.SettingFormStartPosX := Settings.SettingFormStartPosX;
    Result.SettingFormStartPosY := Settings.SettingFormStartPosY;
    Result.IgnoreUsesUnused := Settings.IgnoreUsesUnused;
    Result.EnableExperimentalFindUnusedUses := Settings.EnableExperimentalFindUnusedUses;
    Result.BreakLineForNonDomainUses := Settings.BreakLineForNonDomainUses;
  finally
    Settings.Free;
  end;
end;

function TSettings.GetEnableExperimentalFindUnusedUses: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_EXPRIMENTAL_UNUSED_USES, False);
end;

function TSettings.GetGroupNonNamespaceUnits: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_GROUP_NONNAMESPACE_UNITS, True);
end;

function TSettings.GetIgnoreUsesUnused: string;
var
  DefaultUnits: string;
begin
  DefaultUnits := 'Windows,Messages,SysUtils,Variants,Classes,Graphics,Controls,Forms,Dialogs,Types,WinApi.Windows,Winapi.Messages,'
          + 'System.SysUtils,System.Variants,System.Classes,System.Types,Vcl.Graphics,Vcl.Controls,Vcl.Forms,Vcl.Dialogs,FMX.Types,'
          + 'FMX.Controls,FMX.Forms,FMX.Dialogs,QTypes,QGraphics,QControls,QForms,QDialogs,QStdCtrls,System.ImageList';

  Result := FIni.ReadString(SETTINGS_SECTION, CONF_IGNORED_USES, DefaultUnits);
end;

function TSettings.GetOrganizeUses: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_ORGANIZE_USES, True);
end;

function TSettings.GetOrganizeUsesAfterAddingNewUsesUnit: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_ORGANIZE_USES_AFTER_ADDING_NEW_USES_UNIT, False);
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

function TSettings.GetBlankLineBtwNameScapes: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_BLANKLINE_BTW_NAMESCAPE, False);
end;

function TSettings.GetBreakLine: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_BREAK_LINE, False);
end;

function TSettings.GetBreakLineForNonDomainUses: Boolean;
begin
  Result := FIni.ReadBool(SETTINGS_SECTION, CONF_BREAK_LINE_NON_DOMAIN_USES, True);
end;

function TSettings.GetBreakUsesLineAtPosition: Cardinal;
begin
  Result := FIni.ReadInteger(SETTINGS_SECTION, CONF_BREAK_USES_LINE_AT_POSITION, 120);
end;

function TSettings.GetSettingFormHeight: Cardinal;
begin
  Result := FIni.ReadInteger(SEARCHFORM_SETTINGS_SECTION, CONF_FORM_SETTINGS_HEIGHT, 0);
end;

function TSettings.GetSettingFormStartPosX: Cardinal;
begin
  Result := FIni.ReadInteger(SEARCHFORM_SETTINGS_SECTION, CONF_FORM_SETTINGS_START_X, 0);
end;

function TSettings.GetSettingFormStartPosY: Cardinal;
begin
  Result := FIni.ReadInteger(SEARCHFORM_SETTINGS_SECTION, CONF_FORM_SETTINGS_START_Y, 0);
end;

function TSettings.GetSettingFormWidth: Cardinal;
begin
  Result := FIni.ReadInteger(SEARCHFORM_SETTINGS_SECTION, CONF_FORM_SETTINGS_WIDTH, 0);
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

procedure TSettings.SetBlankLineBtwNameScapes(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_BLANKLINE_BTW_NAMESCAPE, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetBreakLine(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_BREAK_LINE, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetBreakLineForNonDomainUses(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_BREAK_LINE_NON_DOMAIN_USES, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetOrganizeUses(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_ORGANIZE_USES, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetOrganizeUsesAfterAddingNewUsesUnit(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_ORGANIZE_USES_AFTER_ADDING_NEW_USES_UNIT, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetSettingFormHeight(const Value: Cardinal);
begin
  FIni.WriteInteger(SEARCHFORM_SETTINGS_SECTION, CONF_FORM_SETTINGS_HEIGHT, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetSettingFormStartPosX(const Value: Cardinal);
begin
  FIni.WriteInteger(SEARCHFORM_SETTINGS_SECTION, CONF_FORM_SETTINGS_START_X, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetSettingFormStartPosY(const Value: Cardinal);
begin
  FIni.WriteInteger(SEARCHFORM_SETTINGS_SECTION, CONF_FORM_SETTINGS_START_Y, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetSettingFormWidth(const Value: Cardinal);
begin
  FIni.WriteInteger(SEARCHFORM_SETTINGS_SECTION, CONF_FORM_SETTINGS_WIDTH, Value);
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

procedure TSettings.SetUsesUnused(const Value: string);
begin
  FIni.WriteString(SETTINGS_SECTION, CONF_IGNORED_USES, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetBreakUsesLineAtPosition(const Value: Cardinal);
begin
  FIni.WriteInteger(SETTINGS_SECTION, CONF_BREAK_USES_LINE_AT_POSITION, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetEnableExperimentalFindUnusedUses(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_EXPRIMENTAL_UNUSED_USES, Value);
  FIni.UpdateFile;
end;

procedure TSettings.SetGroupNonNamespaceUnits(const Value: Boolean);
begin
  FIni.WriteBool(SETTINGS_SECTION, CONF_GROUP_NONNAMESPACE_UNITS, Value);
  FIni.UpdateFile;
end;

end.
