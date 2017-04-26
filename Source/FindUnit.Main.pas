unit FindUnit.Main;

interface

uses
  ToolsAPI, Dialogs, Classes, Menus, FindUnit.EnvironmentController, Graphics, Windows, FindUnit.CompilerInterceptor, FindUnit.Header;

{$R RFindUnitSplash.res}
type
  TRFindUnitMain = class(TNotifierObject, IOTAKeyboardBinding)
  private
    FMenusCreated: Boolean;
    FEnvControl: TEnvironmentController;
    FProjectServiceIndex: Integer;

    procedure AutoImport(const Context: IOTAKeyContext; KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
    procedure OrganizeUses(const Context: IOTAKeyContext; KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
    procedure OpenForm(const Context: IOTAKeyContext; KeyCode: TShortCut; var BindingResult: TKeyBindingResult);

    procedure CreateMenus;
    procedure ForceRegisterKeys(Sender: TObject);
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
  FindUnit.FormSearch, Log4PAscal, FindUnit.Utils, FindUnit.OTAUtils, SysUtils, FindUnit.Settings;

var
  vKbIndex: Integer;
  VFindUnit: IInterface;
  AboutBoxServices : IOTAAboutBoxServices = nil;
  AboutBoxIndex : Integer = 0;
  vBindingServices: IOTAKeyBindingServices;

resourcestring
  resPackageName = 'RfUtils - Import Usages';
  resLicense = 'OpenSource (MIT)';
  resAboutCopyright = 'Copyright Rodrigo Farias Rezino';
  resAboutTitle = 'RfUtils - Import Usages';
  resAboutDescription = 'Help us on GitHub https://github.com/rfrezino/RFindUnit';

procedure Register;
var
  OtaKey: IOTAKeyboardBinding;
begin
  Logger := TLogger.Create(FindUnitDirLogger + Format('pid_%d_log_%s.txt', [GetCurrentProcessId, FormatDateTime('yyyy-mm-dd', Now)]));

  VFindUnit := TRFindUnitMain.Create;
  OtaKey := VFindUnit as IOTAKeyboardBinding;
  with (BorlandIDEServices as IOTAKeyboardServices) do
    vKbIndex := AddKeyboardBinding(OtaKey);
end;

procedure RegisterSplashScreen;
var
  LBmp: Graphics.TBitmap;
begin
  LBmp := Graphics.TBitmap.Create;
  LBmp.LoadFromResourceName(HInstance, 'SPLASH');
  SplashScreenServices.AddPluginBitmap(resPackageName, LBmp.Handle, False, resLicense, '');
  LBmp.Free;
end;

procedure RegisterAboutBox;
var
  LProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  LProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'SPLASH');
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(resPackageName, resAboutDescription, LProductImage, False, resLicense);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex = 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

{ TFormLauncher }

procedure TRFindUnitMain.CreateMenus;
var
  MainMenu: TMainMenu;
  NewItem: TMenuItem;
  RfItemMenu: TMenuItem;
  ToolItem: TMenuItem;
  FirstBreak: TMenuItem;
  IndexFirstBreak: Integer;
begin
  if not (Supports(BorlandIDEServices, INTAServices)) then
    Exit;

  if FMenusCreated then
    Exit;

  FMenusCreated := True;

  MainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  ToolItem := MainMenu.Items.Find('Tools');

  FirstBreak := ToolItem.Find('-');
  if Assigned(FirstBreak) then
    IndexFirstBreak := ToolItem.IndexOf(FirstBreak) + 1
  else
    IndexFirstBreak := ToolItem.Count - 1;

  RfItemMenu := ToolItem.Find('RfUtils');
  if RfItemMenu = nil then
  begin
    RfItemMenu := TMenuItem.Create(nil);
    RfItemMenu.Caption := 'RfUtils';
    ToolItem.Insert(IndexFirstBreak, RfItemMenu);
  end;

  NewItem := TMenuItem.Create(nil);
  NewItem.Caption := 'Force register shortcuts';
  NewItem.OnClick := ForceRegisterKeys;
  RfItemMenu.Add(NewItem);
end;

procedure TRFindUnitMain.ForceRegisterKeys(Sender: TObject);
begin
  vBindingServices.AddKeyBinding([ShortCut(Ord('A'), [ssCtrl, ssShift])], OpenForm, nil);
  vBindingServices.AddKeyBinding([ShortCut(Ord('I'), [ssCtrl, ssShift])], AutoImport, nil);
  vBindingServices.AddKeyBinding([ShortCut(Ord('O'), [ssCtrl, ssShift])], OrganizeUses, nil);
end;

procedure TRFindUnitMain.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  vBindingServices := BindingServices;
  BindingServices.AddKeyBinding([ShortCut(Ord('A'), [ssCtrl, ssShift])], OpenForm, nil);
  BindingServices.AddKeyBinding([ShortCut(Ord('I'), [ssCtrl, ssShift])], AutoImport, nil);
  BindingServices.AddKeyBinding([ShortCut(Ord('O'), [ssCtrl, ssShift])], OrganizeUses, nil);
  CreateMenus;
end;

constructor TRFindUnitMain.Create;
var
  ProjectFileStorageService: IOTAProjectFileStorage;
begin
  inherited;
  ProjectFileStorageService := BorlandIDEServices.GetService(IOTAProjectFileStorage) as IOTAProjectFileStorage;
  FEnvControl := TEnvironmentController.Create;
  FProjectServiceIndex := ProjectFileStorageService.AddNotifier(FEnvControl);

  CompilerInterceptor.SetEnvControl(FEnvControl);

  Logger.Debug('Version ' + VERSION_STR);

  TSettings.ReloadSettings;
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

procedure TRFindUnitMain.AutoImport(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
  BindingResult := krHandled;
  FEnvControl.ImportMissingUnits;
  FEnvControl.ForceLoadProjectPath;
end;

procedure TRFindUnitMain.OpenForm(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var
  SelectedText: TStringPosition;
begin
  FEnvControl.ForceLoadProjectPath;
  SelectedText := GetSelectedTextFromContext(Context);
  if SelectedText.Value = '' then
    SelectedText := GetWordAtCursor;

  BindingResult := krHandled;
  if frmFindUnit = nil then
  begin
    frmFindUnit := TfrmFindUnit.Create(nil);
    frmFindUnit.SetEnvControl(FEnvControl);
    frmFindUnit.SetSearch(SelectedText);
    frmFindUnit.Show;
  end;
end;

procedure TRFindUnitMain.OrganizeUses(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
  if not GlobalSettings.OrganizeUses then
    Exit;

  BindingResult := krHandled;
  FEnvControl.OrganizeUses;
end;

procedure Clear;
begin
  with (BorlandIDEServices as IOTAKeyboardServices) do
  begin
    RemoveKeyboardBinding(vKbIndex);
  end;
end;

initialization
  RegisterAboutBox;
  RegisterSplashScreen;

finalization
  UnregisterAboutBox;

end.
