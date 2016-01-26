unit FindUnit.EnvironmentController;

interface

uses
  Classes, Generics.Collections, FindUnit.PasParser, FindUnit.DprParser, OtlParallel, ToolsAPI, XMLIntf, FindUnit.FileCache, SysUtils;

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
    Sleep(1000);

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

