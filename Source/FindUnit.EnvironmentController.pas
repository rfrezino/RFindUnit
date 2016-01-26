unit FindUnit.EnvironmentController;

interface

uses
  Classes, Generics.Collections, FindUnit.Parser, OtlParallel, ToolsAPI, XMLIntf, FindUnit.FileCache, SysUtils;

type
  TEnvironmentController = class(TInterfacedObject, IOTAProjectFileStorageNotifier)
  private
    FProjectUnits: TUnitsController;
    FLibraryPath: TUnitsController;
    FLastItem: string;

    procedure CreateLibraryPathUnits;
    procedure OnFinishedLibraryPathScan(FindUnits: TObjectList<TFindUnitItem>);

    procedure CreateProjectPathUnits;
    procedure OnFinishedProjectPathScan(FindUnits: TObjectList<TFindUnitItem>);

    procedure ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    procedure CreatingProject(const ProjectOrGroup: IOTAModule);
    procedure ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    procedure ProjectClosing(const ProjectOrGroup: IOTAModule);
  public
    function GetName: string;

    constructor Create;
    destructor Destroy; override;

    function GetProjectUnits(SearchString: string): TStringList;
    function GetLibraryPathUnits(SearchString: string): TStringList;
  end;

implementation

uses
  FindUnit.Worker, FindUnit.OTAUtils;

{ TEnvUpdateControl }

constructor TEnvironmentController.Create;
begin

end;

procedure TEnvironmentController.CreateLibraryPathUnits;
var
  Paths: TStringList;
  EnvironmentOptions: IOTAEnvironmentOptions;
begin
  FLibraryPath := TUnitsController.Create;
  Paths := TStringList.Create;
  try
    Paths.Delimiter := ';';
    Paths.StrictDelimiter := True;
    EnvironmentOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
    Paths.DelimitedText := EnvironmentOptions.Values['LibraryPath'] + ';' + EnvironmentOptions.Values['BrowsingPath'];

    TParserWorker.Create(Paths).Start(OnFinishedLibraryPathScan);
  finally
    Paths.Free;
  end;
end;

procedure TEnvironmentController.CreateProjectPathUnits;
var
  Files: TStringList;
  I: Integer;
  FilePas: IOTAEditor;
  CurProject: IOTAProject;
  OtaModule: IOTAModuleServices;
begin
  Sleep(2000);
  FProjectUnits := TUnitsController.Create;
  OtaModule := (BorlandIDEServices as IOTAModuleServices);
  CurProject :=  GetCurrentProject;
  try
    Files := TStringList.Create;
    CurProject.GetAssociatedFilesFromModule(Files);
    for I := 0 to CurProject.ModuleFileCount -1 do
    begin
      Files.Add(CurProject.GetModule(i).FileName);
      CurProject.GetModule(i).GetAdditionalFiles(Files);
    end;
    TParserWorker.Create(Files).Start(OnFinishedProjectPathScan);
  finally
    Files.Free;
  end;
end;

procedure TEnvironmentController.CreatingProject(const ProjectOrGroup: IOTAModule);
begin
  if FLibraryPath = nil then
    Parallel.Async(CreateLibraryPathUnits);

  FreeAndNil(FProjectUnits);
  Parallel.Async(CreateProjectPathUnits);
end;

destructor TEnvironmentController.Destroy;
begin
  FProjectUnits.Free;
  FLibraryPath.Free;
  inherited;
end;

function TEnvironmentController.GetLibraryPathUnits(SearchString: string): TStringList;
begin
  if FLibraryPath.Ready then
    Result := FLibraryPath.GetFindInfo(SearchString)
  else
    Result := TStringList.Create;
end;

function TEnvironmentController.GetName: string;
begin

end;

function TEnvironmentController.GetProjectUnits(SearchString: string): TStringList;
begin
  if FProjectUnits.Ready then
    Result := FProjectUnits.GetFindInfo(SearchString)
  else
    Result := TStringList.Create;
end;

procedure TEnvironmentController.OnFinishedLibraryPathScan(FindUnits: TObjectList<TFindUnitItem>);
begin
  FLibraryPath.Units := FindUnits;
  FLibraryPath.Ready := True;
end;

procedure TEnvironmentController.OnFinishedProjectPathScan(FindUnits: TObjectList<TFindUnitItem>);
begin

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

