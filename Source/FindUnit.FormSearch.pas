 unit FindUnit.FormSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FindUnit.SearchString, ToolsAPI, Menus,
  FindUnit.EnvironmentController, StrUtils, AppEvnts, Buttons;

type
  TFuncBoolean = function: Boolean of object;

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
  private
    FEnvControl: TEnvironmentController;

    function GetSelectedItem: string;
    procedure AddUnit;

    procedure ProcessKeyCommand(var Msg: tagMSG; var Handled: Boolean);

    procedure CheckLoadingStatus(Func: TFuncBoolean; LabelDesc: TLabel; RefreshButton: TSpeedButton);
    procedure CheckLibraryStatus;
    procedure FilterItemFromSearchString;

    procedure SaveConfigs;
    procedure LoadConfigs;

    procedure SelectTheMostSelectableItem;
  public
    procedure SetEnvControl(EnvControl: TEnvironmentController);
    procedure SetSearch(Search: string);
    procedure FilterItem(SearchString: string);
  end;

var
  frmFindUnit: TfrmFindUnit;

implementation

uses
  WinSvc, FindUnit.OTAUtils, FindUnit.Utils, FindUnit.FileEditor;

{$R *.dfm}


const
  IDCONT = '1';
  SEARCH_MESSAGE = 'Type your search...';

var
  CONFIG_SearchOnProjectUnits: Boolean;
  CONFIG_SearchOnLibraryPath: Boolean;

procedure TfrmFindUnit.AddUnit;
var
  CurEditor: IOTASourceEditor;
  FileEditor: TSourceFileEditor;
begin
  CurEditor := OtaGetCurrentSourceEditor;
  FileEditor := TSourceFileEditor.Create(CurEditor);
  try
    FileEditor.Prepare;
    if rbInterface.Checked then
      FileEditor.AddUsesToInterface(GetSelectedItem)
    else
      FileEditor.AddUsesToImplementation(GetSelectedItem);
  finally
    FileEditor.Free;
  end;

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

procedure TfrmFindUnit.btnRefreshLibraryPathClick(Sender: TObject);
begin
  FEnvControl.LoadLibraryPath;
  CheckLibraryStatus;
end;

procedure TfrmFindUnit.btnRefreshProjectClick(Sender: TObject);
begin
  FEnvControl.LoadProjectPath;
  CheckLibraryStatus;
end;

procedure TfrmFindUnit.CheckLoadingStatus(Func: TFuncBoolean; LabelDesc: TLabel; RefreshButton: TSpeedButton);
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
    NewCaption := 'Loading...';
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

procedure TfrmFindUnit.FilterItem(SearchString: string);
var
  Return: TStringList;
  ResultSearch: TStringList;
begin
  lstResult.Items.BeginUpdate;
  ResultSearch := TStringList.Create;
  try
    lstResult.Clear;
    if (SearchString = '') or (FEnvControl = nil) then
      Exit;

    if chkSearchProjectFiles.Checked then
    begin
      Return := FEnvControl.GetProjectUnits(SearchString);
      ResultSearch.Text := ResultSearch.Text + Return.Text;
      Return.Free;
    end;

    if chkSearchLibraryPath.Checked then
    begin
      Return := FEnvControl.GetLibraryPathUnits(SearchString);
      ResultSearch.Text := ResultSearch.Text + Return.Text;
      Return.Free;
    end;

    ResultSearch.Sorted := True;
    lstResult.Items.Text := ResultSearch.Text;

    SelectTheMostSelectableItem;
  finally
    ResultSearch.Free;
    lstResult.Items.EndUpdate;
  end;
end;

procedure TfrmFindUnit.FilterItemFromSearchString;
begin
  FilterItem(edtSearch.Text);
end;

procedure TfrmFindUnit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveConfigs;
  Action := caFree;
  frmFindUnit := nil;
end;

procedure TfrmFindUnit.FormShow(Sender: TObject);
begin
  edtSearch.SelectAll;
  edtSearch.SetFocus;
  LoadConfigs;
end;


function TfrmFindUnit.GetSelectedItem: string;
var
  I: Integer;

  function CorrectUses(Item: string): string;
  begin
    Result := Item;
    Result := Trim(Fetch(Result, '-'));
    Result := ReverseString(Result);
    Fetch(Result,'.');
    Result := ReverseString(Result);
  end;
begin
  Result := '';
  if lstResult.Count = 0 then
    Exit;

  for I := 0 to lstResult.Items.Count -1 do
  begin
    if lstResult.Selected[i] then
    begin
      Result := CorrectUses(lstResult.Items[i]);
      Exit;
    end;
  end;

  Result := CorrectUses(lstResult.Items[0])
end;

procedure TfrmFindUnit.LoadConfigs;
begin
  chkSearchLibraryPath.Checked := CONFIG_SearchOnLibraryPath;
  chkSearchProjectFiles.Checked := CONFIG_SearchOnProjectUnits;
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
  SmallItemId: Integer;
  SmallItemLength: Integer;
  I: Integer;
  List: TStrings;
  PosSize: Integer;
begin
  SmallItemLength := 999999;
  SmallItemId := -1;
  List := lstResult.Items;

  if List.Count = 0 then
    Exit;

  for I := 0 to List.Count -1 do
  begin
    PosSize := Pos('-', List[i]);
    if PosSize < SmallItemLength then
    begin
      SmallItemLength := PosSize;
      SmallItemId := I;
    end;
  end;

  lstResult.Selected[SmallItemId] := True;
end;

procedure TfrmFindUnit.SetEnvControl(EnvControl: TEnvironmentController);
begin
  FEnvControl := EnvControl;
  CheckLibraryStatus;
end;

procedure TfrmFindUnit.SetSearch(Search: string);
begin
  if (Search = '') or (Pos(#13#10, Search) > 0) then
    Exit;

  edtSearch.Text := Search;
end;

procedure TfrmFindUnit.CheckLibraryStatus;
begin
  CheckLoadingStatus(FEnvControl.IsProjectsUnitReady, lblProjectUnitsStatus, btnRefreshProject);
  CheckLoadingStatus(FEnvControl.IsLibraryPathsUnitReady, lblLibraryUnitsStatus, btnRefreshLibraryPath);
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
