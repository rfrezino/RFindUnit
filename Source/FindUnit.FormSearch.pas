 unit FindUnit.FormSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ToolsAPI, Menus, SyncObjs,
  FindUnit.EnvironmentController, StrUtils, AppEvnts,
  Buttons, ShellAPI, FindUnit.Header, FindUnit.FileEditor, Vcl.ImgList,
  FindUnit.FormSettings, FindUnit.Settings, System.ImageList,
  Vcl.Clipbrd;

type
  TFuncBoolean = function: Boolean of object;
  TFuncString = function: string of object;

  TfrmFindUnit = class(TForm)
    grpOptions: TGroupBox;
    chkSearchLibraryPath: TCheckBox;
    chkSearchProjectFiles: TCheckBox;
    grpResult: TGroupBox;
    lstResult: TListBox;
    grpSearch: TGroupBox;
    edtSearch: TEdit;
    lblWhere: TLabel;
    rbInterface: TRadioButton;
    rbImplementation: TRadioButton;
    aevKeys: TApplicationEvents;
    tmrLoadedItens: TTimer;
    lblProjectUnitsStatus: TLabel;
    lblLibraryUnitsStatus: TLabel;
    btnRefreshProject: TSpeedButton;
    btnRefreshLibraryPath: TSpeedButton;
    btnAdd: TButton;
    btnProcessDCUs: TSpeedButton;
    pnlMsg: TPanel;
    lblMessage: TLabel;
    btn1: TSpeedButton;
    btnConfig: TButton;
    ilImages: TImageList;
    procedure FormShow(Sender: TObject);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnAddClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lstResultDblClick(Sender: TObject);
    procedure edtSearchClick(Sender: TObject);
    procedure aevKeysMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure tmrLoadedItensTimer(Sender: TObject);
    procedure chkSearchProjectFilesClick(Sender: TObject);
    procedure chkSearchLibraryPathClick(Sender: TObject);
    procedure btnRefreshProjectClick(Sender: TObject);
    procedure btnRefreshLibraryPathClick(Sender: TObject);
    procedure btnProcessDCUsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnConfigClick(Sender: TObject);
    procedure lstResultClick(Sender: TObject);
  private
    FEnvControl: TEnvironmentController;
    FFileEditor: TSourceFileEditor;
    FfrmConfig: TfrmSettings;

    procedure AddUnit;

    procedure ProcessKeyCommand(var Msg: tagMSG; var Handled: Boolean);

    procedure CheckLoadingStatus(Func: TFuncBoolean; FuncStatus: TFuncString; LabelDesc: TLabel; RefreshButton: TSpeedButton);
    procedure CheckLibraryStatus;
    procedure FilterItemFromSearchString;

    procedure SaveConfigs;
    procedure LoadConfigs;

    procedure SelectTheMostSelectableItem;
    procedure ProcessDCUFiles;
    function CanProcessDCUFiles: Boolean;
    procedure DisplayMessageToMuchResults(Show: Boolean);

    procedure LoadCurrentFile;
    procedure GetSelectedItem(out UnitName, ClassName: string);
    procedure SaveFormSettings;
    procedure ConfigureForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetEnvControl(EnvControl: TEnvironmentController);
    procedure SetSearch(Filter: TStringPosition);

    procedure FilterItem(const SearchString: string);

    procedure ShowTextOnScreen(Text: string);
  end;

var
  frmFindUnit: TfrmFindUnit;

implementation

uses
  FindUnit.DcuDecompiler,
  FindUnit.FormMessage,
  FindUnit.OTAUtils,
  FindUnit.ResultsImportanceCalculator,
  FindUnit.Utils;

{$R *.dfm}


const
  IDCONT = '1';
  SEARCH_MESSAGE = 'Type your search...';

var
  CONFIG_SearchOnProjectUnits: Boolean;
  CONFIG_SearchOnLibraryPath: Boolean;


procedure TfrmFindUnit.SaveFormSettings;
var
  Settings: TSettings;
begin
  Settings := TSettings.Create;
  try
    Settings.SettingFormWidth := Self.Width;
    Settings.SettingFormHeight := Self.Height;
    Settings.SettingFormStartPosX := Self.Left;
    Settings.SettingFormStartPosY := Self.Top;
  finally
    Settings.Free;
  end;
end;

procedure TfrmFindUnit.ConfigureForm;
var
  Settings: TSettings;
begin
  Settings := TSettings.Create;
  try
    if Settings.SettingFormWidth > 0 then
      Self.Width := Settings.SettingFormWidth;

    if Settings.SettingFormHeight > 0 then
      Self.Height := Settings.SettingFormHeight;

    if Settings.SettingFormStartPosX > 0 then
      Self.Left := Settings.SettingFormStartPosX;

    if Settings.SettingFormStartPosY > 0 then
      Self.Top := Settings.SettingFormStartPosY;
  finally
    Settings.Free;
  end;
end;


procedure TfrmFindUnit.ShowTextOnScreen(Text: string);
var
  MsgForm: TfrmMessage;
begin
  MsgForm := TfrmMessage.Create(nil);

  if rbInterface.Checked then
    Text := 'Unit ' + Text + ' added to interface''s uses.'
  else
    Text := 'Unit ' + Text + ' added to implementation''s uses.';

  MsgForm.DisplayMessage(Text);
  SetFocus;
end;

procedure TfrmFindUnit.AddUnit;
var
  SelectedUnit: string;
  SelectedClass: string;
begin
  GetSelectedItem(SelectedUnit, SelectedClass);
  if SelectedUnit = '' then
    Exit;

  ShowTextOnScreen(SelectedUnit);

  if rbInterface.Checked then
    FFileEditor.AddUsesToInterface(SelectedUnit)
  else
    FFileEditor.AddUsesToImplementation(SelectedUnit);

  if GlobalSettings.StoreChoices then
    FEnvControl.AutoImport.SetMemorizedUnit(SelectedClass, SelectedUnit);

  Close;
end;

procedure TfrmFindUnit.aevKeysMessage(var Msg: tagMSG; var Handled: Boolean);
begin
  if Msg.message = WM_KEYDOWN then
    ProcessKeyCommand(Msg, Handled);
end;


procedure TfrmFindUnit.btnAddClick(Sender: TObject);
begin
  AddUnit;
end;

procedure TfrmFindUnit.btnConfigClick(Sender: TObject);
begin
  FfrmConfig := TfrmSettings.Create(Self);
  FfrmConfig.ShowModal;
  FfrmConfig := nil;
end;

procedure TfrmFindUnit.btnProcessDCUsClick(Sender: TObject);
begin
  if CanProcessDCUFiles then
    ProcessDCUFiles;
end;

function TfrmFindUnit.CanProcessDCUFiles: Boolean;
const
  MESGEM = 'The dcu32int.exe was not found. It should be at %s . If you download the project´s source you will '
    + 'find it at {PATH}\RFindUnit\Thirdy\Dcu32Int\dcu32int.exe . Copy the executable and past it on the %s, and '
    + 'try execute this command again. If you don´t know where to find this executable I can send you '
    + 'to the project page, do you want I open it to you ?';
var
  ForMessage: string;
  MesDlg: TForm;
begin
  Result := True;
  if Dcu32IntExecutableExists then
    Exit;

  Result := False;

  ForMessage := GetDcu32ExecutablePath;
  ForMessage := Format(MESGEM, [ForMessage, ForMessage]);

  MesDlg := CreateMessageDialog(ForMessage, mtError, [mbCancel, mbYes]);
  try
    MesDlg.Width := 900;
    MesDlg.Position := poScreenCenter;
    MesDlg.ShowModal;

    if MesDlg.ModalResult <> mrYes then
      Exit;
  finally
    MesDlg.Free;
  end;

  ShellExecute(0, 'open', PChar('https://github.com/rfrezino/RFindUnit/'), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmFindUnit.ProcessDCUFiles;
const
  MESGEM = 'This command will list all the DCUs files that you don´t have access to .PAS '
   + ' and process it to make it available for search.'
   + ' This process will slowdown your computer and can take some minutes (~2), '
   + ' are you sure that you want to run it now ?';
begin
  if MessageDlg(MESGEM, mtWarning, [mbCancel, mbYes], 0) <> mrYes then
    Exit;

  btnProcessDCUs.Enabled := False;
  FEnvControl.ProcessDCUFiles;
end;

procedure TfrmFindUnit.btnRefreshLibraryPathClick(Sender: TObject);
begin
  try
    FEnvControl.LoadLibraryPath;
    CheckLibraryStatus;
  except
    on E: exception do
    begin
      MessageDlg('btnRefreshLibraryPathClick Error: ' + e.Message, mtError, [mbOK], 0);
      {$IFDEF RAISEMAD}
      raise;
      {$ENDIF}
    end;
  end;
end;

procedure TfrmFindUnit.btnRefreshProjectClick(Sender: TObject);
begin
  try
    FEnvControl.LoadProjectPath;
    CheckLibraryStatus;
  except
    on E: exception do
    begin
      MessageDlg('btnRefreshProjectClick Error: ' + e.Message, mtError, [mbOK], 0);
      {$IFDEF RAISEMAD}
      raise;
      {$ENDIF}
    end;
  end;
end;

procedure TfrmFindUnit.CheckLoadingStatus(Func: TFuncBoolean; FuncStatus: TFuncString; LabelDesc: TLabel; RefreshButton: TSpeedButton);
var
  NewCaption: string;
begin
  NewCaption := '';
  if Func then
  begin
    RefreshButton.Visible := True;
    LabelDesc.Visible := False;
  end
  else
  begin
    LabelDesc.Visible := True;
    RefreshButton.Visible := False;
    NewCaption := FuncStatus;
    LabelDesc.Font.Color := $000069D2;
    LabelDesc.Font.Style := [fsItalic];
  end;

  if NewCaption <> LabelDesc.Caption then
  begin
    FilterItemFromSearchString;
    LabelDesc.Caption := NewCaption;
  end;

end;

procedure TfrmFindUnit.chkSearchLibraryPathClick(Sender: TObject);
begin
  FilterItemFromSearchString;
end;

procedure TfrmFindUnit.chkSearchProjectFilesClick(Sender: TObject);
begin
  FilterItemFromSearchString;
end;

constructor TfrmFindUnit.Create(AOwner: TComponent);
begin
  inherited;
  LoadCurrentFile;
end;

destructor TfrmFindUnit.Destroy;
begin
  FFileEditor.Free;
  inherited;
end;

procedure TfrmFindUnit.edtSearchChange(Sender: TObject);
begin
  FilterItemFromSearchString;
end;

procedure TfrmFindUnit.edtSearchClick(Sender: TObject);
begin
  if edtSearch.Text = SEARCH_MESSAGE then
    edtSearch.SelectAll;
end;

procedure TfrmFindUnit.edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    AddUnit
  else if (Key = VK_ESCAPE) then
    Close;
end;

procedure TfrmFindUnit.DisplayMessageToMuchResults(Show: Boolean);
begin
  pnlMsg.Visible := Show;
  lblMessage.Caption := '  There are to many results on your search, I''m not showing everything. Type a bigger search.' + #13#10 +
    'Remember that you can create incremental searchs like: "string   replace", I''m going to look for the arguments separately.'
end;

procedure TfrmFindUnit.FilterItem(const SearchString: string);
var
  Return: TStringList;
  ResultSearch: TStringList;
  ToMuchResults: Boolean;

  procedure IsThereToMuchResults;
  begin
    if Return.Count >= MAX_RETURN_ITEMS then
      ToMuchResults := True;
  end;
begin
  ToMuchResults := False;
  lstResult.Items.BeginUpdate;
  ResultSearch := TStringList.Create;
  try
    lstResult.Clear;
    if (SearchString = '') or (FEnvControl = nil) or (SearchString = SEARCH_MESSAGE) then
      Exit;

    if chkSearchProjectFiles.Checked then
    begin
      Return := FEnvControl.GetProjectUnits(SearchString);
      ResultSearch.Text := ResultSearch.Text + Return.Text;
      IsThereToMuchResults;
      Return.Free;
    end;

    if chkSearchLibraryPath.Checked then
    begin
      Return := FEnvControl.GetLibraryPathUnits(SearchString);
      ResultSearch.Text := ResultSearch.Text + Return.Text;
      IsThereToMuchResults;
      Return.Free;
    end;
    ResultSearch.Sorted := True;
    lstResult.Items.Text := ResultSearch.Text;

    SelectTheMostSelectableItem;
    DisplayMessageToMuchResults(ToMuchResults);
    lstResult.Count
  finally
    ResultSearch.Free;
    lstResult.Items.EndUpdate;
  end;
end;

procedure TfrmFindUnit.FilterItemFromSearchString;
begin
  try
    FilterItem(edtSearch.Text);
  except
//    Logger
    raise
  end;
end;

procedure TfrmFindUnit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveFormSettings;
  SaveConfigs;
  Action := caFree;
  frmFindUnit := nil;
end;

procedure TfrmFindUnit.FormCreate(Sender: TObject);
begin
  Caption := 'Find Uses - Version ' + VERSION_STR;
  ConfigureForm;
end;

procedure TfrmFindUnit.FormShow(Sender: TObject);
begin
  edtSearch.SelectAll;
  edtSearch.SetFocus;
  LoadConfigs;
end;

procedure TfrmFindUnit.GetSelectedItem(out UnitName, ClassName: string);
var
  I: Integer;
begin
  UnitName := '';
  if lstResult.Count = 0 then
    Exit;

  for I := 0 to lstResult.Items.Count -1 do
  begin
    if lstResult.Selected[i] then
    begin
      GetUnitFromSearchSelection(lstResult.Items[i], UnitName, ClassName);
      Exit;
    end;
  end;

  GetUnitFromSearchSelection(lstResult.Items[0], UnitName, ClassName);
end;

procedure TfrmFindUnit.LoadConfigs;
begin
  chkSearchLibraryPath.Checked := CONFIG_SearchOnLibraryPath;
  chkSearchProjectFiles.Checked := CONFIG_SearchOnProjectUnits;
end;

procedure TfrmFindUnit.LoadCurrentFile;
var
  CurEditor: IOTASourceEditor;
begin
  CurEditor := OtaGetCurrentSourceEditor;
  FFileEditor := TSourceFileEditor.Create(CurEditor);
  FFileEditor.Prepare;
end;

procedure TfrmFindUnit.lstResultClick(Sender: TObject);
begin
  {$IFDEF DEBUG}
  Clipboard.AsText := lstResult.Items.Text;
  {$ENDIF}
end;

procedure TfrmFindUnit.lstResultDblClick(Sender: TObject);
begin
  AddUnit;
  Close;
end;

procedure TfrmFindUnit.ProcessKeyCommand(var Msg: tagMSG; var Handled: Boolean);
const
  MOVE_COMMANDS = [VK_UP, VK_DOWN];

  function IsCtrlA: Boolean;
  begin
    Result := (GetKeyState(VK_CONTROL) < 0) and (Char(Msg.wParam) = 'A');
  end;
begin
  if FfrmConfig <> nil then
  begin
    Handled := False;
    Exit;
  end;

  if (Msg.wParam in MOVE_COMMANDS) then
  begin
    Msg.hwnd := lstResult.Handle;
    lstResult.SetFocus;
  end
  else
  begin
    Msg.hwnd := edtSearch.Handle;
    edtSearch.SetFocus;

    if IsCtrlA then
      edtSearch.SelectAll;
  end;
end;

procedure TfrmFindUnit.SaveConfigs;
begin
  CONFIG_SearchOnProjectUnits := chkSearchProjectFiles.Checked;
  CONFIG_SearchOnLibraryPath := chkSearchLibraryPath.Checked;
end;

procedure TfrmFindUnit.SelectTheMostSelectableItem;
var
  Calculator: TResultImportanceCalculator;
begin
  Calculator := TResultImportanceCalculator.Create;
  try
    Calculator.Config(lstResult.Items, Trim(edtSearch.Text));
    Calculator.Process;

    if Calculator.MostRelevantIdx > -1 then
      lstResult.Selected[Calculator.MostRelevantIdx] := True;
  finally
    Calculator.Free;
  end;
end;

procedure TfrmFindUnit.SetEnvControl(EnvControl: TEnvironmentController);
begin
  FEnvControl := EnvControl;
  CheckLibraryStatus;
end;

procedure TfrmFindUnit.SetSearch(Filter: TStringPosition);
begin
  if GlobalSettings.AlwaysUseInterfaceSection then
  begin
    rbImplementation.Checked := False;
    rbInterface.Checked := True;
  end
  else
  begin
    rbInterface.Checked := not FFileEditor.IsLineOnImplementationSection(Filter.Line);
    rbImplementation.Checked := FFileEditor.IsLineOnImplementationSection(Filter.Line);
  end;

  if (Filter.Value = '') or (Pos(#13#10, Filter.Value) > 0) then
    Exit;

  edtSearch.Text := Filter.Value;
end;

procedure TfrmFindUnit.CheckLibraryStatus;
begin
  btnProcessDCUs.Enabled := not FEnvControl.ProcessingDCU;
  CheckLoadingStatus(FEnvControl.IsProjectsUnitReady, FEnvControl.GetProjectPathStatus, lblProjectUnitsStatus, btnRefreshProject);
  CheckLoadingStatus(FEnvControl.IsLibraryPathsUnitReady, FEnvControl.GetLibraryPathStatus, lblLibraryUnitsStatus, btnRefreshLibraryPath);
end;

procedure TfrmFindUnit.tmrLoadedItensTimer(Sender: TObject);
begin
  CheckLibraryStatus;
end;

procedure LoadInitialConfigs;
begin
  CONFIG_SearchOnProjectUnits := True;
  CONFIG_SearchOnLibraryPath := True;
end;

initialization
  LoadInitialConfigs;


end.
