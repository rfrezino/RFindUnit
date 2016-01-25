unit FindUnit.Main;

interface

uses
  ToolsAPI, Dialogs, Classes, Menus, FindUnit.EnvironmentController;

type
  TRFindUnitMain = class(TNotifierObject, IOTAKeyboardBinding)
  private
    FEnvControl: TEnvironmentController;
    FProjectServiceIndex: Integer;
    procedure SetSearch(const Context: IOTAKeyContext; KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
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
  FindUnit.FormSearch, FindUnit.SearchString, Log4PAscal, FindUnit.Utils;

var
  vKbIndex: Integer;
  VFindUnit: TRFindUnitMain;

procedure Register;
begin
  Logger := TLogger.Create(FindUnitDirLogger + '\Logs.txt');

  VFindUnit := TRFindUnitMain.Create;
  with (BorlandIDEServices as IOTAKeyboardServices) do
  begin
    vKbIndex := AddKeyboardBinding(VFindUnit);
  end;
end;

{ TFormLauncher }

procedure TRFindUnitMain.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([ShortCut(Ord('A'), [ssCtrl, ssShift])], SetSearch, nil);
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
  Result := btComplete;
end;

function TRFindUnitMain.GetDisplayName: string;
begin
  Result := 'RFindUnit';
end;

function TRFindUnitMain.GetName: string;
begin
  Result := 'RFindUnit';
end;

procedure TRFindUnitMain.SetSearch(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
  BindingResult := krHandled;
  if frmFindUnit = nil then
  begin
    frmFindUnit := TfrmFindUnit.Create(nil);
    frmFindUnit.SetEnvControl(FEnvControl);
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
