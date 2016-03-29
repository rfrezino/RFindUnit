unit FindUnit.EnvironmentController;

interface

uses
  Classes, Generics.Collections, FindUnit.PasParser, OtlParallelFU, ToolsAPI, XMLIntf, FindUnit.FileCache, SysUtils,
  Log4Pascal, FindUnit.Worker, FindUnit.AutoImport, Windows, FindUnit.Header;

type
  TEnvironmentController = class(TInterfacedObject, IOTAProjectFileStorageNotifier)
  private
    FProcessingDCU: Boolean;
    FAutoImport: TAutoImport;

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
    property AutoImport: TAutoImport read FAutoImport;

    procedure ImportMissingUnits(ShowNoImport: Boolean = true);
  end;

implementation

uses
  FindUnit.OTAUtils, FindUnit.Utils, FindUnit.FileEditor, FindUnit.FormMessage, FindUnit.StringPositionList;

{ TEnvUpdateControl }

constructor TEnvironmentController.Create;
begin
  FAutoImport := TAutoImport.Create(FindUnitDir + 'memoryconfig.ini');
  FAutoImport.Load;
  LoadLibraryPath;
end;

procedure TEnvironmentController.CreateLibraryPathUnits;
var
  Paths, Files: TStringList;
  EnvironmentOptions: IOTAEnvironmentOptions;
begin
  if FLibraryPath <> nil then
    Exit;

  while (BorlandIDEServices as IOTAServices) = nil do
    Sleep(1000);


  try
    Files := nil;
    Paths := TStringList.Create;
    Paths.Delimiter := ';';
    Paths.StrictDelimiter := True;
    EnvironmentOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
    Paths.DelimitedText := EnvironmentOptions.Values['LibraryPath'] + ';' + EnvironmentOptions.Values['BrowsingPath'];
    Paths.Add(FindUnitDcuDir);

    FLibraryPath := TUnitsController.Create;
    FreeAndNil(FLibraryPathWorker);
    FLibraryPathWorker := TParserWorker.Create(Paths, Files);
    FLibraryPathWorker.Start(OnFinishedLibraryPathScan);
  except
    on E: exception do
      Logger.Error('TEnvironmentController.CreateLibraryPathUnits: %s', [e.Message]);
  end;
end;

procedure TEnvironmentController.CreateProjectPathUnits;
var
  I: Integer;
  CurProject: IOTAProject;
  FileDesc: string;
  Files, Paths: TStringList;
begin
  if FProjectUnits <> nil then
    Exit;

  while GetCurrentProject = nil do
  begin
    Logger.Debug('TEnvironmentController.CreateProjectPathUnits: waiting GetCurrentProject <> nil');
    Sleep(1000);
  end;

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
  FProjectUnits := TUnitsController.Create;
  FProjectPathWorker := TParserWorker.Create(Paths, Files);
  FProjectPathWorker.Start(OnFinishedProjectPathScan);
end;

procedure TEnvironmentController.CreatingProject(const ProjectOrGroup: IOTAModule);
begin
  LoadProjectPath;
end;

destructor TEnvironmentController.Destroy;
begin
  FAutoImport.Free;
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

procedure TEnvironmentController.ImportMissingUnits(ShowNoImport: Boolean);
var
  CurEditor: IOTASourceEditor;
  FileEditor: TSourceFileEditor;
  ListToImport: TStringPositionList;
  Item: TStringPosition;
  OldFocus: Cardinal;
begin
  if FAutoImport = nil then
    Exit;

  CurEditor := OtaGetCurrentSourceEditor;
  if CurEditor = nil then
    Exit;

  OldFocus := GetFocus;

  ListToImport := FAutoImport.LoadUnitListToImport;
  if ListToImport.Count = 0 then
  begin
    if ShowNoImport then
      TfrmMessage.ShowInfoToUser('There is not possible uses to import.');
    ListToImport.Free;
    SetFocus(OldFocus);
    Exit;
  end;

  FileEditor := TSourceFileEditor.Create(CurEditor);
  try
    FileEditor.Prepare;
    for Item in ListToImport do
    begin
      FileEditor.AddUnit(Item);
      SetFocus(OldFocus);
    end;
  finally
    FileEditor.Free;
  end;
  ListToImport.Free;
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
end;

procedure TEnvironmentController.OnFinishedProjectPathScan(FindUnits: TObjectList<TPasFile>);
begin
  FProjectUnits.Units := FindUnits;
  FProjectUnits.Ready := True;
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

