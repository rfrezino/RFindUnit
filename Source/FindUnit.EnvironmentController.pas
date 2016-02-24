unit FindUnit.EnvironmentController;

interface

uses
  Classes, Generics.Collections, FindUnit.PasParser, OtlParallelFU, ToolsAPI, XMLIntf, FindUnit.FileCache, SysUtils, Log4Pascal, FindUnit.Worker;

type
  TEnvironmentController = class(TInterfacedObject, IOTAProjectFileStorageNotifier)
  private
    FProcessingDCU: Boolean;

    FProjectUnits: TUnitsController;
    FLibraryPath: TUnitsController;

    FProjectPathWorker: TParserWorker;
    FLibraryPathWorker: TParserWorker;

    procedure CreateLibraryPathUnits;
    procedure OnFinishedLibraryPathScan(FindUnits: TObjectList<TPasFile>);

    procedure CreateProjectPathUnits;
    procedure OnFinishedProjectPathScan(FindUnits: TObjectList<TPasFile>);

    procedure CreatingProject(const ProjectOrGroup: IOTAModule);
    //Dummy
    procedure ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    procedure ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    procedure ProjectClosing(const ProjectOrGroup: IOTAModule);

    procedure CallProcessDcuFiles;
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

    function GetLibraryPathStatus: string;
    function GetProjectPathStatus: string;

    procedure ProcessDCUFiles;

    property ProcessingDCU: Boolean read FProcessingDCU;
  end;

implementation

uses
  FindUnit.OTAUtils, FindUnit.Utils;

{ TEnvUpdateControl }

constructor TEnvironmentController.Create;
begin
  LoadLibraryPath;
end;

procedure TEnvironmentController.CreateLibraryPathUnits;
var
  Paths, Files: TStringList;
  EnvironmentOptions: IOTAEnvironmentOptions;
begin
  if FLibraryPath <> nil then
    Exit;

  FLibraryPath := TUnitsController.Create;

  while (BorlandIDEServices as IOTAServices) = nil do
    Sleep(1000);


  Files := nil;
  Paths := TStringList.Create;
  Paths.Delimiter := ';';
  Paths.StrictDelimiter := True;
  EnvironmentOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
  Paths.DelimitedText := EnvironmentOptions.Values['LibraryPath'] + ';' + EnvironmentOptions.Values['BrowsingPath'];
  Paths.Add(FindUnitDir + 'DecompiledDcus\');

  FLibraryPathWorker := TParserWorker.Create(Paths, Files);
  FLibraryPathWorker.Start(OnFinishedLibraryPathScan);
end;

procedure TEnvironmentController.CreateProjectPathUnits;
var
  I: Integer;
  CurProject: IOTAProject;
  FileDesc: string;
  Files, Paths: TStringList;
begin
  while GetCurrentProject = nil do
  begin
    Logger.Debug('TEnvironmentController.CreateProjectPathUnits: waiting GetCurrentProject <> nil');
    Sleep(1000);
  end;

  if FProjectUnits <> nil then
    Exit;

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

  Paths := nil;
  FreeAndNil(FProjectPathWorker);
  FProjectPathWorker := TParserWorker.Create(Paths, Files);
  FProjectPathWorker.Start(OnFinishedProjectPathScan);
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

function TEnvironmentController.GetLibraryPathStatus: string;
begin
  Result := 'Ready';
  if FLibraryPathWorker <> nil then
    Result := Format('%d/%d Processing...', [FLibraryPathWorker.ParsedItems, FLibraryPathWorker.ItemsToParse]);
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

function TEnvironmentController.GetProjectPathStatus: string;
begin
  Result := 'Ready';
  if FProjectPathWorker <> nil then
    Result := Format('%d/%d Files Processed...', [FProjectPathWorker.ParsedItems, FProjectPathWorker.ItemsToParse]);
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
  if (FLibraryPath <> nil) and (not FLibraryPath.Ready) then
    Exit;

  FreeAndNil(FLibraryPath);
  Parallel.Async(CreateLibraryPathUnits);
end;

procedure TEnvironmentController.LoadProjectPath;
begin
  if (FProjectUnits <> nil) and (not FProjectUnits.Ready) then
    Exit;

  FreeAndNil(FProjectUnits);
  Parallel.Async(CreateProjectPathUnits);
end;

procedure TEnvironmentController.OnFinishedLibraryPathScan(FindUnits: TObjectList<TPasFile>);
begin
  FLibraryPath.Units := FindUnits;
  FLibraryPath.Ready := True;
//  FParserLibraryWorker.Free;
end;

procedure TEnvironmentController.OnFinishedProjectPathScan(FindUnits: TObjectList<TPasFile>);
begin
  FProjectUnits.Ready := True;
  FProjectUnits.Units := FindUnits;
end;

procedure TEnvironmentController.ProcessDCUFiles;
begin
  Parallel.Async(CallProcessDcuFiles);
end;

procedure TEnvironmentController.CallProcessDcuFiles;
var
  Paths, Files: TStringList;
  EnvironmentOptions: IOTAEnvironmentOptions;
  DcuProcess: TParserWorker;
  Items: TObject;
begin
  FProcessingDCU := True;
  while (BorlandIDEServices as IOTAServices) = nil do
    Sleep(1000);

  Paths := TStringList.Create;
  Paths.Delimiter := ';';
  Paths.StrictDelimiter := True;
  EnvironmentOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
  Paths.DelimitedText := EnvironmentOptions.Values['LibraryPath'] + ';' + EnvironmentOptions.Values['BrowsingPath'];

  Files := nil;
  DcuProcess := TParserWorker.Create(Paths, Files);
  DcuProcess.ParseDcuFile := True;
  Items := DcuProcess.Start;
  DcuProcess.Free;
  Items.Free;
  FProcessingDCU := False;
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

