unit FindUnit.FormSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,Vcl.StdCtrls,
  System.IniFiles, Vcl.Grids, Data.DB, Datasnap.DBClient, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.DBCtrls,
  FindUnit.Settings, System.SyncObjs, Winapi.ShellAPI;

type
  TfrmSettings = class(TForm)
    pgcMain: TPageControl;
    tsAutoImport: TTabSheet;
    chkAutoEnabled: TCheckBox;
    grdAutoImport: TDBGrid;
    cdsAutoImport: TClientDataSet;
    dtsAutoImport: TDataSource;
    cdsAutoImportIDENTIFIER: TStringField;
    cdsAutoImportUNIT: TStringField;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btn1Click(Sender: TObject);
    procedure chkBreaklineClick(Sender: TObject);
    procedure chkSortAfterAddingClick(Sender: TObject);
    procedure lblLinkClick(Sender: TObject);
  private
    FSettings: TSettings;

    procedure InsertAutoImportInDataSet;
    procedure InsertDataSetInAutoImport;
    procedure CreateIniFile;
    procedure SaveSettings;

    procedure ConfigureAutoImportPage;
    procedure ConfigurePages;
    procedure ToggleEnableItems;
  end;

implementation

uses
	FindUnit.Utils, FindUnit.Header;

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

  InsertDataSetInAutoImport;
end;

procedure TfrmSettings.btn1Click(Sender: TObject);
begin
  ShellExecute(Handle, nil, PChar(TSettings.SettingsFilePath), nil, nil, SW_SHOWNORMAL)
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
end;

procedure TfrmSettings.ConfigureAutoImportPage;
begin
  InsertAutoImportInDataSet;

  chkAutoEnabled.Checked := FSettings.AutoImportEnabled;
  chkAlwaysImportToInterfaceSection.Checked := FSettings.AlwaysUseInterfaceSection;
  chkMemorize.Checked := FSettings.StoreChoices;
  chkBreakline.Checked := FSettings.BreakLine;
  chkSortAfterAdding.Checked := FSettings.SortUsesAfterAdding;
  chkBlankLineBtwNamespace.Checked := FSettings.BlankLineBtwNameScapes;
  chkOrganizeUses.Checked := FSettings.OrganizeUses;

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
  Values := FSettings.AutoImportValue;
  try
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

procedure TfrmSettings.lblLinkClick(Sender: TObject);
var
  Link: string;
begin
  Link := 'https://github.com/rfrezino/RFindUnit';
  ShellExecute(Application.Handle, PChar('open'), PChar(Link), nil, nil, SW_SHOW);
end;

end.
