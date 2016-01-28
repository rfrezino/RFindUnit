unit FindUnit.Main;

interface

uses
  ToolsAPI, Dialogs, Classes, Menus, FindUnit.EnvironmentController;

type
  TRFindUnitMain = class(TNotifierObject, IOTAKeyboardBinding)
  private
    FEnvControl: TEnvironmentController;
    FProjectServiceIndex: Integer;
    procedure OpenForm(const Context: IOTAKeyContext; KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
  public
    constructor Create;
    destructor Destroy; override;

    function GetBindingType: TBindingType;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);

    function GetDisplayName: string;
    function GetName: string;
  end;

  procedure Register;

implementation

uses
  FindUnit.FormSearch, FindUnit.SearchString, Log4PAscal, FindUnit.Utils, FindUnit.OTAUtils, SysUtils;

var
  vKbIndex: Integer;
  VFindUnit: TRFindUnitMain;

procedure Register;
begin
  Logger := TLogger.Create(Format('%slog_%s.txt', [FindUnitDirLogger, FormatDateTime('yyyy-mm-dd', Now)]));

  VFindUnit := TRFindUnitMain.Create;
  with (BorlandIDEServices as IOTAKeyboardServices) do
  begin
    vKbIndex := AddKeyboardBinding(VFindUnit);
  end;
end;

{ TFormLauncher }

procedure TRFindUnitMain.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([ShortCut(Ord('A'), [ssCtrl, ssShift])], OpenForm, nil);
end;

constructor TRFindUnitMain.Create;
var
  ProjectFileStorageService: IOTAProjectFileStorage;
begin
  ProjectFileStorageService := BorlandIDEServices.GetService(IOTAProjectFileStorage) as IOTAProjectFileStorage;
  FEnvControl := TEnvironmentController.Create;
  FProjectServiceIndex := ProjectFileStorageService.AddNotifier(FEnvControl);
end;

destructor TRFindUnitMain.Destroy;
var
  ProjectFileStorageService: IOTAProjectFileStorage;
begin
  ProjectFileStorageService := BorlandIDEServices.GetService(IOTAProjectFileStorage) as IOTAProjectFileStorage;
  if (ProjectFileStorageService <> nil) and (FProjectServiceIndex <> 0) then
    ProjectFileStorageService.RemoveNotifier(FProjectServiceIndex);
  inherited;
end;

function TRFindUnitMain.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TRFindUnitMain.GetDisplayName: string;
begin
  Result := 'RFindUnit';
end;

function TRFindUnitMain.GetName: string;
begin
  Result := 'RFindUnit';
end;

procedure TRFindUnitMain.OpenForm(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var
  SelectedText: string;
begin
  SelectedText := GetSelectedTextFromContext(Context);

  BindingResult := krHandled;
  if frmFindUnit = nil then
  begin
    frmFindUnit := TfrmFindUnit.Create(nil);
    frmFindUnit.SetEnvControl(FEnvControl);
    frmFindUnit.SetSearch(SelectedText);
    frmFindUnit.Show;
  end;
end;

procedure Clear;
begin
  with (BorlandIDEServices as IOTAKeyboardServices) do
  begin
    RemoveKeyboardBinding(vKbIndex);
  end;
end;

initialization

finalization
//  Clear;

end.
