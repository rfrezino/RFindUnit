 unit FindUnit.FormSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FindUnit.SearchString, ToolsAPI, Menus,
  FindUnit.EnvironmentController;

type


  TfrmFindUnit = class(TForm)
    grpOptions: TGroupBox;
    chkSearchLibraryPath: TCheckBox;
    chkSearchProjectFiles: TCheckBox;
    grpResult: TGroupBox;
    lstResult: TListBox;
    grpSearch: TGroupBox;
    pnlResultBottom: TPanel;
    btnAdd: TButton;
    rgUsesAddTo: TRadioGroup;
    edtSearch: TEdit;
    chkDelphiFiles: TCheckBox;
    procedure lstResultKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnAddClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FEnvControl: TEnvironmentController;

    function GetSelectedItem: string;
    procedure AddUnit;
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

procedure TfrmFindUnit.AddUnit;
var
  CurEditor: IOTASourceEditor;
  FileEditor: TSourceFileEditor;
begin
  CurEditor := OtaGetCurrentSourceEditor;
  FileEditor := TSourceFileEditor.Create(CurEditor);
  try
    FileEditor.Prepare;
    FileEditor.AddUsesToInterface(GetSelectedItem);
  finally
    FileEditor.Free;
  end;

  Close;
end;

procedure TfrmFindUnit.btnAddClick(Sender: TObject);
begin
  AddUnit;
end;

procedure TfrmFindUnit.edtSearchChange(Sender: TObject);
begin
  FilterItem(edtSearch.Text);
end;

procedure TfrmFindUnit.edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_UP) then
  begin
    lstResult.SetFocus;
    lstResult.ItemIndex := lstResult.ItemIndex -1;
  end
  else if (Key = VK_DOWN) then
  begin
    lstResult.SetFocus;
    lstResult.ItemIndex := lstResult.ItemIndex +1;
  end
  else if (Key = VK_RETURN) then
    AddUnit;
       
end;

procedure TfrmFindUnit.FilterItem(SearchString: string);
var
  Return: TStringList;
begin
  Return := nil;
  lstResult.Items.BeginUpdate;
  try
    lstResult.Clear;
    if FEnvControl = nil then
      Exit;

    Return := FEnvControl.GetLibraryPathUnits(SearchString);

    lstResult.Items.Text := Return.Text;
  finally
    Return.Free;
    lstResult.Items.EndUpdate;
  end;
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
begin
  Result := '';
  if lstResult.Count = 0 then
    Exit;

  for I := 0 to lstResult.Items.Count -1 do
  begin
    if lstResult.Selected[i] then
    begin
      Result := lstResult.Items[i];
      Result := Fetch(Result, '.');
      Exit;
    end;
  end;

  Result := lstResult.Items[0];
end;

procedure TfrmFindUnit.lstResultKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
  begin
    AddUnit;
    Exit;
    Close;
  end;

  edtSearch.Text := edtSearch.Text + Key;
  edtSearch.SetFocus;
  edtSearch.SelStart := Length(edtSearch.Text)
end;

procedure TfrmFindUnit.SetEnvControl(EnvControl: TEnvironmentController);
begin
  FEnvControl := EnvControl;
end;

end.
