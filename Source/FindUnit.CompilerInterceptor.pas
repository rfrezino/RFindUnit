unit FindUnit.CompilerInterceptor;

interface

uses
  ToolsAPI,

  FindUnit.EnvironmentController,
  System.SysUtils;

type
  TCompilerInterceptor = class(TNotifierObject, IOTANotifier, IOTAIDENotifier, IOTAIDENotifier50)
  protected
    class var FNotifierIndex: Integer;
    FEnvControl: TEnvironmentController;

    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  public
    constructor Create;

    procedure SetEnvControl(Env: TEnvironmentController);
  end;

  var
    CompilerInterceptor: TCompilerInterceptor;

  procedure Register;
  procedure CompilerInterceptorUnRegister;

implementation

uses
  FindUnit.Settings;

procedure Register;
var
  Services: IOTAServices;
begin
  if not Supports(BorlandIDEServices, IOTAServices, Services) then
    Exit;

  CompilerInterceptor := TCompilerInterceptor.Create;
  CompilerInterceptor.FNotifierIndex := Services.AddNotifier(CompilerInterceptor);
end;

{ TCompilerInterceptor }

procedure TCompilerInterceptor.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin

end;

procedure TCompilerInterceptor.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TCompilerInterceptor.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
  Cancel := False;
end;

constructor TCompilerInterceptor.Create;
begin
  inherited;
  FNotifierIndex := -1;
end;

procedure TCompilerInterceptor.BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  Cancel := False;
  if FEnvControl = nil then Exit;
  if not IsCodeInsight then Exit;

  if GlobalSettings.AutoImportEnabled then
    FEnvControl.ImportMissingUnits(False);
end;

procedure TCompilerInterceptor.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
begin
  Cancel := False;
end;

procedure TCompilerInterceptor.SetEnvControl(Env: TEnvironmentController);
begin
  FEnvControl := Env;
end;

procedure CompilerInterceptorUnRegister;
var
  Services: IOTAServices;
begin
  if (CompilerInterceptor.FNotifierIndex > -1) and Supports(BorlandIDEServices, IOTAServices, Services) then
    Services.RemoveNotifier(CompilerInterceptor.FNotifierIndex);
end;

initialization

finalization
  CompilerInterceptorUnRegister;

end.
