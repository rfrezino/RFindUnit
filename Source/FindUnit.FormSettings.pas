unit FindUnit.FormSettings;

interface

uses
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  Forms,
  Graphics,
  Messages,
  Variants,
  Windows,

  Data.DB,

  Datasnap.DBClient,

  FindUnit.Settings,

  System.IniFiles,
  System.SyncObjs,
  System.SysUtils,

  Vcl.DBCtrls,
  Vcl.DBGrids,
  Vcl.ExtCtrls,
  Vcl.Grids,
  Vcl.Mask,
  Vcl.StdCtrls,

  Winapi.ShellAPI, FindUnit.OTAUtils, ToolsAPI;

type
  TfrmSettings = class(TForm)
    pgcMain: TPageControl;
    tsAutoImport: TTabSheet;
    chkAutoEnabled: TCheckBox;
    grdAutoImport: TDBGrid;
    dtsAutoImport: TDataSource;
    grpAutoSettings: TGroupBox;
    nvgAutoImport: TDBNavigator;
    tsGeneral: TTabSheet;
    grpGeneralSettings: TGroupBox;
    btn1: TButton;
    grpSearchAlgorithm: TRadioGroup;
    grpShotCuts: TGroupBox;
    chkMemorize: TCheckBox;
    chkOrganizeUses: TCheckBox;
    grpUsesOrganization: TGroupBox;
    chkAlwaysImportToInterfaceSection: TCheckBox;
    chkSortAfterAdding: TCheckBox;
    chkBreakline: TCheckBox;
    chkBlankLineBtwNamespace: TCheckBox;
    lblLink: TLabel;
    chbOrganizeUsesAfterInsertingNewUsesUnit: TCheckBox;
    medtBreakUsesLineAtPosition: TMaskEdit;
    lblBreakLineAt: TLabel;
    chbGroupNonNameSpaceUnits: TCheckBox;
    cdsAutoImport: TClientDataSet;
    cdsAutoImportIDENTIFIER: TStringField;
    cdsAutoImportUNIT: TStringField;
    btnCreateProjectConfiguration: TButton;
    tsUnusedUses: TTabSheet;
    mmoIgnoreUses: TMemo;
    Label1: TLabel;
    chbFeatureUnusedUses: TCheckBox;
    chbDontBreakLineForNonNameSpaceUnits: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btn1Click(Sender: TObject);
    procedure chkBreaklineClick(Sender: TObject);
    procedure chkSortAfterAddingClick(Sender: TObject);
    procedure lblLinkClick(Sender: TObject);
    procedure pgcMainChange(Sender: TObject);
    procedure btnCreateProjectConfigurationClick(Sender: TObject);
  private
    FSettings: TSettings;
    FMemorizedOpened: Boolean;

    procedure InsertAutoImportInDataSet;
    procedure InsertDataSetInAutoImport;
    procedure CreateIniFile;

    procedure SaveSettings;

    procedure ConfigureAutoImportPage;
    procedure ConfigurePages;
    procedure ToggleEnableItems;

    function IsThereProjectOpen: Boolean;
  end;

implementation

{$R *.dfm}

procedure TfrmSettings.SaveSettings;
begin
  FSettings.AutoImportEnabled := chkAutoEnabled.Checked;
  FSettings.AlwaysUseInterfaceSection := chkAlwaysImportToInterfaceSection.Checked;
  FSettings.OrganizeUses := chkOrganizeUses.Checked;
  FSettings.StoreChoices := chkMemorize.Checked;

  FSettings.BreakLine := chkBreakline.Checked;
  FSettings.SortUsesAfterAdding := chkSortAfterAdding.Checked;
  FSettings.BlankLineBtwNameScapes := chkBlankLineBtwNamespace.Checked;

  FSettings.UseDefaultSearchMatch := grpSearchAlgorithm.ItemIndex = 0;

  FSettings.OrganizeUsesAfterAddingNewUsesUnit := chbOrganizeUsesAfterInsertingNewUsesUnit.Checked;
  FSettings.BreakUsesLineAtPosition := StrToInt(Trim(medtBreakUsesLineAtPosition.Text));
  FSettings.GroupNonNamespaceUnits := chbGroupNonNameSpaceUnits.Checked;

  FSettings.IgnoreUsesUnused := mmoIgnoreUses.Lines.CommaText;
  FSettings.EnableExperimentalFindUnusedUses := chbFeatureUnusedUses.Checked;
  FSettings.BreakLineForNonDomainUses := not chbDontBreakLineForNonNameSpaceUnits.Checked;

  InsertDataSetInAutoImport;
end;

procedure TfrmSettings.btn1Click(Sender: TObject);
begin
  ShellExecute(Handle, nil, PChar(TSettings.SettingsFilePath), nil, nil, SW_SHOWNORMAL)
end;

procedure TfrmSettings.btnCreateProjectConfigurationClick(Sender: TObject);
begin
  ShowMessage(GetCurrentProject.FileName);
end;

procedure TfrmSettings.chkBreaklineClick(Sender: TObject);
begin
  ToggleEnableItems;
end;

procedure TfrmSettings.chkSortAfterAddingClick(Sender: TObject);
begin
  ToggleEnableItems;
end;

procedure TfrmSettings.ToggleEnableItems;
begin
  chkBlankLineBtwNamespace.Enabled := chkBreakline.Checked and chkSortAfterAdding.Checked;
  chbGroupNonNameSpaceUnits.Enabled := chkBreakline.Checked and chkSortAfterAdding.Checked;
end;

procedure TfrmSettings.ConfigureAutoImportPage;
begin
  chkAutoEnabled.Checked := FSettings.AutoImportEnabled;
  chkAlwaysImportToInterfaceSection.Checked := FSettings.AlwaysUseInterfaceSection;
  chkMemorize.Checked := FSettings.StoreChoices;
  chkBreakline.Checked := FSettings.BreakLine;
  chkSortAfterAdding.Checked := FSettings.SortUsesAfterAdding;
  chkBlankLineBtwNamespace.Checked := FSettings.BlankLineBtwNameScapes;
  chkOrganizeUses.Checked := FSettings.OrganizeUses;
  medtBreakUsesLineAtPosition.Text := IntToStr(FSettings.BreakUsesLineAtPosition);
  chbOrganizeUsesAfterInsertingNewUsesUnit.Checked := FSettings.OrganizeUsesAfterAddingNewUsesUnit;
  chbGroupNonNameSpaceUnits.Checked := FSettings.GroupNonNamespaceUnits;
  mmoIgnoreUses.Lines.CommaText := FSettings.IgnoreUsesUnused;
  chbFeatureUnusedUses.Checked := FSettings.EnableExperimentalFindUnusedUses;
  chbDontBreakLineForNonNameSpaceUnits.Checked := not FSettings.BreakLineForNonDomainUses;

  if FSettings.UseDefaultSearchMatch then
    grpSearchAlgorithm.ItemIndex := 0
  else
    grpSearchAlgorithm.ItemIndex := 1;

  ToggleEnableItems;
end;

procedure TfrmSettings.ConfigurePages;
begin
  ConfigureAutoImportPage;
end;

procedure TfrmSettings.CreateIniFile;
begin
  FSettings := TSettings.Create;
end;

procedure TfrmSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  CreateIniFile;
  ConfigurePages;
end;

procedure TfrmSettings.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  TSettings.ReloadSettings;
  FSettings.Free;
end;

procedure TfrmSettings.InsertAutoImportInDataSet;
var
  Values: TStrings;
  I: Integer;
begin
  FMemorizedOpened := True;
  Values := FSettings.AutoImportValue;
  try
    if Values = nil then
      Exit;

    if not cdsAutoImport.Active then
      cdsAutoImport.CreateDataSet;

    for I := 0 to Values.Count -1 do
    begin
      cdsAutoImport.Append;
      cdsAutoImportIDENTIFIER.Value := Values.Names[i];
      cdsAutoImportUNIT.Value := Values.ValueFromIndex[i];
      cdsAutoImport.Post;
    end;

    with cdsAutoImport.IndexDefs.AddIndexDef do
    begin
      Name := cdsAutoImportIDENTIFIER.FieldName + 'Idx';
      Fields := cdsAutoImportIDENTIFIER.FieldName;
      Options := [ixCaseInsensitive];
    end;
    cdsAutoImport.IndexName := cdsAutoImportIDENTIFIER.FieldName + 'Idx';
  finally
    Values.Free;
  end;
end;

procedure TfrmSettings.InsertDataSetInAutoImport;
var
  Values: TStrings;
begin
  if not FMemorizedOpened then
    Exit;

  Values := TStringList.Create;
  cdsAutoImport.DisableControls;
  try
    cdsAutoImport.First;
    while not cdsAutoImport.Eof do
    begin
      Values.Values[UpperCase(cdsAutoImportIDENTIFIER.Value)] := cdsAutoImportUNIT.Value;
      cdsAutoImport.Next;
    end;
    FSettings.AutoImportValue := Values;
  finally
    Values.Free;
  end;
end;

function TfrmSettings.IsThereProjectOpen: Boolean;
begin
  Result := GetCurrentProject <> nil;
end;

procedure TfrmSettings.lblLinkClick(Sender: TObject);
var
  Link: string;
begin
  Link := 'https://github.com/rfrezino/RFindUnit';
  ShellExecute(Application.Handle, PChar('open'), PChar(Link), nil, nil, SW_SHOW);
end;

procedure TfrmSettings.pgcMainChange(Sender: TObject);
begin
  if not FMemorizedOpened then
    InsertAutoImportInDataSet;
end;

end.
