unit FindUnit.EnvironmentController;

interface

uses
  Classes, Generics.Collections, FindUnit.Parser, OtlParallel, ToolsAPI, XMLIntf, FindUnit.FileCache;

type
  TEnvironmentController = class(TInterfacedObject, IOTAProjectFileStorageNotifier)
  private
    FProjectUnits: TUnitsController;
    FLibraryPath: TUnitsController;
    FLastItem: string;

    procedure CreateLibraryPathUnits;
    procedure OnFinishedLibraryPathScan(FindUnits: TObjectList<TFindUnitItem>);

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
  FProjectUnits := TUnitsController.Create;
  FLibraryPath := TUnitsController.Create;
end;

procedure TEnvironmentController.CreateLibraryPathUnits;
var
  Paths: TStringList;
  EnvironmentOptions: IOTAEnvironmentOptions;
begin
  Paths := TStringList.Create;
  try
    Paths.Delimiter := ';';
    Paths.StrictDelimiter := True;
    EnvironmentOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
    Paths.DelimitedText := EnvironmentOptions.Values['LibraryPath'];

    TParserWorker.Create(Paths).Start(OnFinishedLibraryPathScan);
  finally
    Paths.Free;
  end;
end;

procedure TEnvironmentController.CreatingProject(const ProjectOrGroup: IOTAModule);
begin
  Parallel.Async(CreateLibraryPathUnits);
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
