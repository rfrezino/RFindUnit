 unit FindUnit.FormSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FindUnit.SearchString, ToolsAPI, Menus,
  FindUnit.EnvironmentController, StrUtils, AppEvnts;

type

  TFuncBoolean = function: Boolean of object;

  TfrmFindUnit = class(TForm)
    grpOptions: TGroupBox;
    chkSearchLibraryPath: TCheckBox;
    chkSearchProjectFiles: TCheckBox;
    grpResult: TGroupBox;
    lstResult: TListBox;
    grpSearch: TGroupBox;
    pnlResultBottom: TPanel;
    btnAdd: TButton;
    edtSearch: TEdit;
    lblWhere: TLabel;
    rbInterface: TRadioButton;
    rbImplementation: TRadioButton;
    aevKeys: TApplicationEvents;
    tmrLoadedItens: TTimer;
    lblProjectUnitsStatus: TLabel;
    lblLibraryUnitsStatus: TLabel;
    procedure FormShow(Sender: TObject);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnAddClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lstResultDblClick(Sender: TObject);
    procedure edtSearchClick(Sender: TObject);
    procedure aevKeysMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure tmrLoadedItensTimer(Sender: TObject);
  private
    FEnvControl: TEnvironmentController;

    function GetSelectedItem: string;
    procedure AddUnit;

    procedure ProcessKeyCommand(var Msg: tagMSG; var Handled: Boolean);

    procedure CheckLoadingStatus(Func: TFuncBoolean; LabelDesc: TLabel);
    procedure CheckLibraryStatus;
    procedure FilterItemFromSearchString;
  public
    procedure SetEnvControl(EnvControl: TEnvironmentController);
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

procedure TfrmFindUnit.CheckLoadingStatus(Func: TFuncBoolean; LabelDesc: TLabel);
var
  NewCaption: string;
begin
  NewCaption := '';
  if Func then
  begin
    NewCaption := '';
    LabelDesc.Font.Color := $00408000;
    LabelDesc.Font.Style := [fsBold];
  end
  else
  begin
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
begin
  lstResult.Items.BeginUpdate;
  try
    lstResult.Clear;
    if (SearchString = '') or (FEnvControl = nil) then
      Exit;

    Return := FEnvControl.GetProjectUnits(SearchString);
    lstResult.Items.Text := lstResult.Items.Text + Return.Text;
    Return.Free;

    Return := FEnvControl.GetLibraryPathUnits(SearchString);
    lstResult.Items.Text := lstResult.Items.Text + Return.Text;
    Return.Free;
  finally
    lstResult.Items.EndUpdate;
  end;
end;

procedure TfrmFindUnit.FilterItemFromSearchString;
begin
  FilterItem(edtSearch.Text);
end;

procedure TfrmFindUnit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  frmFindUnit := nil;
end;

procedure TfrmFindUnit.FormShow(Sender: TObject);
begin
  edtSearch.SelectAll;
  edtSearch.SetFocus;
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

procedure TfrmFindUnit.lstResultDblClick(Sender: TObject);
begin
  AddUnit;
  Close;
end;

procedure TfrmFindUnit.ProcessKeyCommand(var Msg: tagMSG; var Handled: Boolean);
const
  MOVE_COMMANDS = [VK_UP, VK_DOWN];
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
  end;
end;

procedure TfrmFindUnit.SetEnvControl(EnvControl: TEnvironmentController);
begin
  FEnvControl := EnvControl;
  CheckLibraryStatus;
end;

procedure TfrmFindUnit.CheckLibraryStatus;
begin
  CheckLoadingStatus(FEnvControl.IsProjectsUnitReady, lblProjectUnitsStatus);
  CheckLoadingStatus(FEnvControl.IsLibraryPathsUnitReady, lblLibraryUnitsStatus);
end;

procedure TfrmFindUnit.tmrLoadedItensTimer(Sender: TObject);
begin
  CheckLibraryStatus;
end;

end.
