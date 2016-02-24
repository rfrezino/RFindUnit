unit FindUnit.EnvironmentController;

interface

uses
  Classes, Generics.Collections, FindUnit.PasParser, OtlParallelFU, ToolsAPI, XMLIntf, FindUnit.FileCache, SysUtils, Log4Pascal;

type
  TEnvironmentController = class(TInterfacedObject, IOTAProjectFileStorageNotifier)
  private
    FProjectUnits: TUnitsController;
    FLibraryPath: TUnitsController;
    FLastItem: string;

    procedure CreateLibraryPathUnits;
    procedure OnFinishedLibraryPathScan(FindUnits: TObjectList<TPasFile>);

    procedure CreateProjectPathUnits;
    procedure OnFinishedProjectPathScan(FindUnits: TObjectList<TPasFile>);

    procedure CreatingProject(const ProjectOrGroup: IOTAModule);
    //Dummy
    procedure ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    procedure ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    procedure ProjectClosing(const ProjectOrGroup: IOTAModule);
  public
    function GetName: string;

    constructor Create;
    destructor Destroy; override;

    procedure LoadLibraryPath;
    procedure LoadProjectPath;

    function GetProjectUnits(const SearchString: string): TStringList;
    function GetLibraryPathUnits(const SearchString: string): TStringList;

    function IsProjectsUnitReady: Boolean;
    function IsLibraryPathsUnitReady: Boolean;
  end;

implementation

uses
  FindUnit.Worker, FindUnit.OTAUtils;

{ TEnvUpdateControl }

constructor TEnvironmentController.Create;
begin
  LoadLibraryPath;
end;

procedure TEnvironmentController.CreateLibraryPathUnits;
var
  Paths: TStringList;
  EnvironmentOptions: IOTAEnvironmentOptions;
begin
  if FLibraryPath <> nil then
    Exit;

  while (BorlandIDEServices as IOTAServices) = nil do
    Sleep(1000);

  FLibraryPath := TUnitsController.Create;
  Paths := TStringList.Create;
  try
    Paths.Delimiter := ';';
    Paths.StrictDelimiter := True;
    EnvironmentOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
    Paths.DelimitedText := EnvironmentOptions.Values['LibraryPath'] + ';' + EnvironmentOptions.Values['BrowsingPath'];

    TParserWorker.Create(Paths, nil).Start(OnFinishedLibraryPathScan);
  finally
    Paths.Free;
  end;
end;

procedure TEnvironmentController.CreateProjectPathUnits;
var
  I: Integer;
  CurProject: IOTAProject;
  FileDesc: string;
  Files: TStringList;
begin
  while GetCurrentProject = nil do
  begin
    Logger.Debug('TEnvironmentController.CreateProjectPathUnits: waiting GetCurrentProject <> nil');
    Sleep(1000);
  end;

  FProjectUnits := TUnitsController.Create;
  CurProject :=  GetCurrentProject;

  Files := TStringList.Create;
  for I := 0 to CurProject.GetModuleCount -1 do
  begin
    FileDesc := CurProject.GetModule(i).FileName;
    if FileDesc = '' then
      Continue;

    Files.Add(FileDesc);
  end;
  TParserWorker.Create(nil, Files).Start(OnFinishedProjectPathScan);
end;

procedure TEnvironmentController.CreatingProject(const ProjectOrGroup: IOTAModule);
begin
  LoadProjectPath;
end;

destructor TEnvironmentController.Destroy;
begin
  FProjectUnits.Free;
  FLibraryPath.Free;
  inherited;
end;

function TEnvironmentController.GetLibraryPathUnits(const SearchString: string): TStringList;
begin
  if IsLibraryPathsUnitReady then
    Result := FLibraryPath.GetFindInfo(SearchString)
  else
    Result := TStringList.Create;
end;

function TEnvironmentController.GetName: string;
begin
  Result := 'RfUtils - Replace FindUnit';
end;

function TEnvironmentController.GetProjectUnits(const SearchString: string): TStringList;
begin
  if IsProjectsUnitReady then
    Result := FProjectUnits.GetFindInfo(SearchString)
  else
    Result := TStringList.Create;
end;

function TEnvironmentController.IsLibraryPathsUnitReady: Boolean;
begin
  Result := (FLibraryPath <> nil) and (FLibraryPath.Ready);
end;

function TEnvironmentController.IsProjectsUnitReady: Boolean;
begin
  Result := (FProjectUnits <> nil) and (FProjectUnits.Ready);
end;

procedure TEnvironmentController.LoadLibraryPath;
begin
  FreeAndNil(FLibraryPath);
  Parallel.Async(CreateLibraryPathUnits);
end;

procedure TEnvironmentController.LoadProjectPath;
begin
  FreeAndNil(FProjectUnits);
  Parallel.Async(CreateProjectPathUnits);
end;

procedure TEnvironmentController.OnFinishedLibraryPathScan(FindUnits: TObjectList<TPasFile>);
begin
  FLibraryPath.Units := FindUnits;
  FLibraryPath.Ready := True;
end;

procedure TEnvironmentController.OnFinishedProjectPathScan(FindUnits: TObjectList<TPasFile>);
begin
  FProjectUnits.Ready := True;
  FProjectUnits.Units := FindUnits;
end;

procedure TEnvironmentController.ProjectClosing(const ProjectOrGroup: IOTAModule);
begin

end;

procedure TEnvironmentController.ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
begin

end;

procedure TEnvironmentController.ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
begin

end;

end.

