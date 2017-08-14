unit FindUnit.EnvironmentController;

interface

uses
  Log4Pascal,
  ToolsAPI,

  FindUnit.AutoImport,
  FindUnit.FileCache,
  FindUnit.Header,
  FindUnit.PasParser,
  FindUnit.Worker,

  Interf.EnvironmentController,

  System.Classes,
  System.Generics.Collections,
  System.SysUtils,

  Winapi.Windows,

  Xml.XMLIntf,
  FindUnit.Utils,
  FindUnit.OTAUtils,
  FindUnit.FileEditor,
  FindUnit.StringPositionList,
  FindUnit.FormMessage;

type
  TEnvironmentController = class(TInterfacedObject, IOTAProjectFileStorageNotifier, IRFUEnvironmentController)
  private
    FProcessingDCU: Boolean;
    FAutoImport: TAutoImport;

    FProjectUnits: TUnitsController;
    FLibraryPath: TUnitsController;

    FProjectPathWorker: TParserWorker;
    FLibraryPathWorker: TParserWorker;

    procedure CreateLibraryPathUnits(OldItems: TUnits);
    procedure OnFinishedLibraryPathScan(FindUnits: TUnits);

    procedure CreateProjectPathUnits(NewFiles: TDictionary<string, TFileInfo>;
      OldFiles: TUnits);
    procedure OnFinishedProjectPathScan(FindUnits: TUnits);

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

    procedure ForceRunDependencies;

    procedure LoadLibraryPath;
    procedure LoadProjectPath;
    procedure ForceLoadProjectPath;

    function GetProjectUnits(const SearchString: string): TStringList;
    function GetLibraryPathUnits(const SearchString: string): TStringList;

    function PasExists(PasName: string): Boolean;

    function GetFullMatch(const SearchString: string): TStringList;

    function IsProjectsUnitReady: Boolean;
    function IsLibraryPathsUnitReady: Boolean;

    function GetLibraryPathStatus: string;
    function GetProjectPathStatus: string;

    procedure ProcessDCUFiles;

    property ProcessingDCU: Boolean read FProcessingDCU;
    property AutoImport: TAutoImport read FAutoImport;

    procedure ImportMissingUnits(ShowNoImport: Boolean = true);
    procedure OrganizeUses;

    function AreDependenciasReady: Boolean;
  end;

implementation



{ TEnvUpdateControl }

constructor TEnvironmentController.Create;
begin
  FAutoImport := TAutoImport.Create(FindUnitDir + AUTO_IMPORT_FILE);
  FAutoImport.Load;
  LoadLibraryPath;
end;

procedure TEnvironmentController.CreateLibraryPathUnits(OldItems: TUnits);
var
  Paths: TStringList;
  Files: TDictionary<string, TFileInfo>;
  EnvironmentOptions: IOTAEnvironmentOptions;
begin
  if FLibraryPath <> nil then
    Exit;

  try
    FreeAndNil(FLibraryPathWorker);
  except
    on e: exception do
    begin
      Logger.Error('TEnvironmentController.CreateLibraryPathUnits: ' + e.Message);
      {$IFDEF RAISEMAD} raise; {$ENDIF}
    end;
  end;

  while (BorlandIDEServices as IOTAServices) = nil do
  begin
    Logger.Debug('TEnvironmentController.CreateLibraryPathUnits: waiting for IOTAServices');
    Sleep(1000);
  end;

  try
    Files := nil;
    Paths := TStringList.Create;
    Paths.Delimiter := ';';
    Paths.StrictDelimiter := True;
    Paths.Duplicates := dupIgnore;

    EnvironmentOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
    GetLibraryPath(Paths, 'Win32');
    Paths.Add('$(BDS)\source\rtl\win');

    FLibraryPath := TUnitsController.Create;
    FLibraryPathWorker := TParserWorker.Create(Paths, Files, OldItems);
    FLibraryPathWorker.Start(OnFinishedLibraryPathScan);
  except
    on E: exception do
    begin
      Logger.Error('TEnvironmentController.CreateLibraryPathUnits: %s', [e.Message]);
      {$IFDEF RAISEMAD} raise; {$ENDIF}
    end;
  end;
end;

procedure TEnvironmentController.CreateProjectPathUnits(NewFiles: TDictionary<string, TFileInfo>;
 OldFiles: TUnits);
var
  Paths: TStringList;
begin
  if FProjectUnits <> nil then
    Exit;

  try
    FreeAndNil(FProjectPathWorker);
  except
    on E: exception do
    begin
      Logger.Debug('TEnvironmentController.CreateProjectPathUnits: Error removing object');
      {$IFDEF RAISEMAD} raise; {$ENDIF}
    end;
  end;

  while GetCurrentProject = nil do
  begin
    Logger.Debug('TEnvironmentController.CreateProjectPathUnits: waiting GetCurrentProject <> nil');
    Sleep(1000);
  end;

  Paths := nil;

  FProjectUnits := TUnitsController.Create;
  FProjectPathWorker := TParserWorker.Create(Paths, NewFiles, OldFiles);
  FProjectPathWorker.Start(OnFinishedProjectPathScan);
end;

procedure TEnvironmentController.CreatingProject(const ProjectOrGroup: IOTAModule);
begin
  LoadProjectPath;
end;

destructor TEnvironmentController.Destroy;
begin
  if FProjectPathWorker <> nil then
    FProjectPathWorker.RemoveCallBack;
  if FLibraryPathWorker <> nil then
    FLibraryPathWorker.RemoveCallBack;

  FAutoImport.Free;
  FProjectUnits.Free;
  FLibraryPath.Free;
  inherited;
end;

procedure TEnvironmentController.ForceLoadProjectPath;
begin
  if FProjectUnits = nil then
    LoadProjectPath;
end;

procedure TEnvironmentController.ForceRunDependencies;
begin
  LoadLibraryPath;
  LoadProjectPath;
end;

function TEnvironmentController.GetFullMatch(const SearchString: string): TStringList;
var
  ProjectUnits: TStringList;
  LibraryUnits: TStringList;
begin
  ProjectUnits := nil;
  LibraryUnits := nil;
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;

  try
    if IsProjectsUnitReady then
    begin
      ProjectUnits := FProjectUnits.GetFindInfoFullMatch(SearchString);
      Result.AddStrings(ProjectUnits);
    end;

    if IsLibraryPathsUnitReady then
    begin
      LibraryUnits := FLibraryPath.GetFindInfoFullMatch(SearchString);
      Result.AddStrings(LibraryUnits);
    end;
  finally
    LibraryUnits.Free;
    ProjectUnits.Free;
  end;
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
      TfrmMessage.ShowInfoToUser('There is no possible uses to import.');
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
var
  LocalThread: TThread;
  OldLibraryPath: TUnits;
begin
  Logger.Debug('TEnvironmentController.LoadLibraryPath');
  if (FLibraryPath <> nil) and (not FLibraryPath.Ready) then
  begin
    Logger.Debug('TEnvironmentController.LoadLibraryPath: no');
    Exit;
  end;
  Logger.Debug('TEnvironmentController.LoadLibraryPath: yes');

  OldLibraryPath := nil;
  if FLibraryPath <> nil then
  begin
    OldLibraryPath := FLibraryPath.Units;
    FLibraryPath.Units := nil;
    FreeAndNil(FLibraryPath);
  end;

  LocalThread := TThread.CreateAnonymousThread(
    procedure
    begin
      CreateLibraryPathUnits(OldLibraryPath);
    end
    );
  LocalThread.FreeOnTerminate := True;
  LocalThread.Start;
end;

procedure TEnvironmentController.LoadProjectPath;
var
  LocalThread: TThread;
  OldFiles: TUnits;
  Files: TDictionary<string, TFileInfo>;
begin
  Logger.Debug('TEnvironmentController.LoadProjectPath');
  if (FProjectUnits <> nil) and (not FProjectUnits.Ready) then
  begin
    Logger.Debug('TEnvironmentController.LoadProjectPath: no');
    Exit;
  end;

  if GetCurrentProject = nil then
    Exit;

  Logger.Debug('TEnvironmentController.LoadProjectPath: yes');

  OldFiles := nil;
  if FProjectUnits <> nil then
  begin
    OldFiles := FProjectUnits.Units;
    FProjectUnits.Units := nil;
    FreeAndNil(FProjectUnits);
  end;

  Files := GetAllFilesFromProjectGroup;

  LocalThread := TThread.CreateAnonymousThread(
    procedure
    begin
      CreateProjectPathUnits(Files, OldFiles);
    end
    );
  LocalThread.FreeOnTerminate := True;
  LocalThread.Start;
end;

procedure TEnvironmentController.OnFinishedLibraryPathScan(FindUnits: TUnits);
begin
  FLibraryPath.Units := FindUnits;
  FLibraryPath.Ready := True;
end;

procedure TEnvironmentController.OnFinishedProjectPathScan(FindUnits: TUnits);
begin
  if FProjectUnits = nil then
  begin
    Exit;
  end;

  FProjectUnits.Ready := True;
  FProjectUnits.Units := FindUnits;
end;

procedure TEnvironmentController.OrganizeUses;
var
  FileEditor: TSourceFileEditor;
  CurEditor: IOTASourceEditor;
begin
  CurEditor := OtaGetCurrentSourceEditor;
  if CurEditor = nil then
    Exit;

  FileEditor := TSourceFileEditor.Create(CurEditor);
  try
    FileEditor.Prepare;
    FileEditor.OrganizeUsesImplementation;
    FileEditor.OrganizeUsesInterface;
  finally
    FileEditor.Free;
  end;
end;

function TEnvironmentController.PasExists(PasName: string): Boolean;
begin
  Result := False;
  if Assigned(FProjectUnits) and (FProjectUnits.Ready) then
    if FProjectUnits.Units.FileExists(PasName) then
      Exit(True);

  if Assigned(FLibraryPath) and (FLibraryPath.Ready) then
    if FLibraryPath.Units.FileExists(PasName) then
      Exit(True);
end;

procedure TEnvironmentController.ProcessDCUFiles;
var
  LocalThread: TThread;
begin
  LocalThread := TThread.CreateAnonymousThread(
    procedure
    begin
      CallProcessDcuFiles;
    end
    );
  LocalThread.FreeOnTerminate := True;
  LocalThread.Start;
end;

function TEnvironmentController.AreDependenciasReady: Boolean;
begin
  Result := IsProjectsUnitReady and IsLibraryPathsUnitReady;
end;

procedure TEnvironmentController.CallProcessDcuFiles;
var
  Paths: TStringList;
  Files: TDictionary<string, TFileInfo>;
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
  DcuProcess := TParserWorker.Create(Paths, Files, nil);
  DcuProcess.ParseDcuFile := True;
  Items := DcuProcess.Start;
  DcuProcess.Free;
  Items.Free;
  FProcessingDCU := False;
end;

procedure TEnvironmentController.ProjectClosing(const ProjectOrGroup: IOTAModule);
begin
  Logger.Debug('TEnvironmentController.ProjectClosing');
end;

procedure TEnvironmentController.ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
begin
  Logger.Debug('TEnvironmentController.ProjectLoaded');
end;

procedure TEnvironmentController.ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
begin
  Logger.Debug('TEnvironmentController.ProjectSaving');
end;

end.

