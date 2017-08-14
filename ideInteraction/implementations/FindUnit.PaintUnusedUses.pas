unit FindUnit.PaintUnusedUses;

interface

uses
  Log4Pascal,
  ToolsAPI,

  FindUnit.ImageRepository,
  FindUnit.UnusedUses,
  FindUnit.Utils,

  Interf.EnvironmentController,

  System.Classes,
  System.DateUtils,
  System.Diagnostics,
  System.Generics.Collections,
  System.SyncObjs,
  System.SysUtils,
  System.Threading,
  System.Types,

  Vcl.Imaging.pngimage,
  System.IOUtils,
  System.RegularExpressions,
  FindUnit.Settings, Vcl.Graphics;

type
  TRfUnusedProcessStatus = (uspRunning, uspPending, uspComplete);

  TRfPaintUnsuedUses = class(TInterfacedObject, IOTANotifier, INTAEditViewNotifier)
  private
    FUsesStartLine: Integer;
  protected
    var HourGlass: TPngImage;
    var CheckedWarn: TPngImage;
    var CheckedOk: TPngImage;

    FNeedRepaint: Boolean;
    FProcessed: TRfUnusedProcessStatus;
    FUnusedUses: TList<TUsesUnit>;
    FRcDir: TCriticalSection;
    FLowestLine: Integer;
    FEditView: IOTAEditView;
    FNotifierIndex: Integer;

    //File control
    FFileName: string;
    FFileLastModification: TDateTime;

    procedure RemoveNotifier;
  public
    class var EnvControl: IRFUEnvironmentController;

    constructor Create(FileName: string; AEditView: IOTAEditView);
    destructor Destroy; override;

    { IOTANotifier }
    procedure Destroyed;

    { INTAEditViewNotifier }
    procedure EditorIdle(const View: IOTAEditView);
    procedure BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
    procedure PaintLine(const View: IOTAEditView; LineNumber: Integer;
      const LineText: PAnsiChar; const TextWidth: Word; const LineAttributes: TOTAAttributeArray;
      const Canvas: TCanvas; const TextRect: TRect; const LineRect: TRect; const CellSize: TSize);
    procedure EndPaint(const View: IOTAEditView);

    procedure SetUnsuedUses(List: TDictionary<string, TUsesUnit>);
    procedure SetUsesStartLine(Line: Integer);
    procedure ProcessFileInformation;

    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Modified;
  end;

  TRfEditorNotifier = class(TNotifierObject, IOTANotifier, IOTAEditorNotifier)
  private
    FSourceEditor: IOTASourceEditor;
    FEditViewNotifiers: TList<INTAEditViewNotifier>;
    FNotifierIndex: Integer;
    procedure RemoveNotifiers;
  public
    constructor Create(ASourceEditor: IOTASourceEditor);
    destructor Destroy; override;

    { IOTANotifier }
    procedure Destroyed;
    { IOTAEditorNotifier }
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    procedure ViewActivated(const View: IOTAEditView);
  end;

  TRfIDENotifier = class(TNotifierObject, IOTAIDENotifier)
  private
    FEditorNotifiers: TList<IOTAEditorNotifier>;
  public
    constructor Create;
    destructor Destroy; override;

    { IOTAIDENotifier }
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  end;

implementation

{ TRfPaintUnsuedUses }
procedure TRfPaintUnsuedUses.AfterSave;
begin
end;

procedure TRfPaintUnsuedUses.BeforeSave;
begin

end;

procedure TRfPaintUnsuedUses.BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
begin
  if not GlobalSettings.EnableExperimentalFindUnusedUses then
    Exit;

  if FNeedRepaint then
  begin
    FNeedRepaint := False;
    FullRepaint := True;
  end;

  if FProcessed <> uspRunning then
  begin
    try
      ProcessFileInformation;
    except
      on E: exception do
        Logger.Error('TRfPaintUnsuedUses.BeginPaint: ' + E.Message);
    end;
  end;
end;

constructor TRfPaintUnsuedUses.Create(FileName: string; AEditView: IOTAEditView);
begin
  inherited Create;
  FFileName := FileName;
  FEditView := AEditView;
  FNotifierIndex := FEditView.AddNotifier(Self);
  FRcDir := TCriticalSection.Create;
  FProcessed := uspPending;
  FUnusedUses := TList<TUsesUnit>.Create;
  FUsesStartLine := -1;

  if HourGlass = nil then
    HourGlass := TFindUnitImageRepository.GetImage(girHourGlass);

  if CheckedWarn = nil then
    CheckedWarn := TFindUnitImageRepository.GetImage(girCheckWithWarnings);

  if CheckedOk = nil then
    CheckedOk := TFindUnitImageRepository.GetImage(girCheckedAllOk);
end;

destructor TRfPaintUnsuedUses.Destroy;
begin
  RemoveNotifier;
  FRcDir.Free;
  inherited;
end;

procedure TRfPaintUnsuedUses.Destroyed;
begin
  RemoveNotifier;
end;

procedure TRfPaintUnsuedUses.EditorIdle(const View: IOTAEditView);
begin

end;

procedure TRfPaintUnsuedUses.EndPaint(const View: IOTAEditView);
begin

end;

procedure TRfPaintUnsuedUses.Modified;
begin
  if not (FProcessed = uspRunning)  then
    FProcessed := uspPending;
end;

procedure TRfPaintUnsuedUses.PaintLine(const View: IOTAEditView; LineNumber: Integer; const LineText: PAnsiChar;
  const TextWidth: Word; const LineAttributes: TOTAAttributeArray; const Canvas: TCanvas; const TextRect,
  LineRect: TRect; const CellSize: TSize);
var
  I: Integer;
  FBackgroundColor: TColor;
  Line: string;
  RegReturn: TMatchCollection;
  Match: TMatch;

  procedure DrawColor(Value: string; Color: TColor; Item: TUsesUnit; Match: TMatch);
  var
    StartPoint: Integer;
    LeftText: string;
    OutValue: TSize;
    OutRect: TRect;
  begin
    LeftText := Copy(Line, 1, Match.Index -1);
    StartPoint := Canvas.TextExtent(LeftText).Width + TextRect.Left;

    OutValue := Canvas.TextExtent(Value);

    OutRect := TextRect;
    OutRect.Left := StartPoint;
    OutRect.Size := OutValue;
    OutRect.Bottom := TextRect.Bottom;
    OutRect.Top := OutRect.Bottom - 2;

    Canvas.Brush.Color := FBackgroundColor;
    Canvas.FillRect(OutRect);
  end;

begin
  if not GlobalSettings.EnableExperimentalFindUnusedUses then
    Exit;

  {$Region 'Paint the left status'}
  if (FUsesStartLine = LineNumber) or ((FUsesStartLine = -1) and (LineNumber = 1)) then
  begin
    if ((FProcessed in [uspRunning, uspPending]) or (not Assigned(FUnusedUses))) then
    begin
      Canvas.Draw(LineRect.Left, LineRect.Top, HourGlass);
      Exit;
    end
    else if (FProcessed = uspComplete) and (FUnusedUses.Count = 0) then
    begin
      Canvas.Draw(LineRect.Left, LineRect.Top, CheckedOk);
      Exit;
    end;
  end;
  {$endregion}

  if LineNumber > FLowestLine then
  begin
    Exit;
  end;

  FRcDir.Acquire;
  try
    if (FUsesStartLine = LineNumber) then
    begin
      if (FProcessed = uspComplete) and (FUnusedUses.Count > 0) then
        Canvas.Draw(LineRect.Left, LineRect.Top, CheckedWarn)
    end;

    Line := AnsiString(LineText);
    for I := 0 to FUnusedUses.Count -1 do
    begin
      if FUnusedUses[I].UnusedType = uetNoPasFile then
        FBackgroundColor := clLtGray
      else
        FBackgroundColor := clWebOrange;

      RegReturn := TRegEx.Matches(Line, '\b' + FUnusedUses[I].Name + '[^<.>]');
      if RegReturn.Count > 0 then
        for Match in RegReturn do
          DrawColor(FUnusedUses[I].Name, clRed, FUnusedUses[I], Match);
    end;
  finally
    FRcDir.Release;
  end;
end;

procedure TRfPaintUnsuedUses.ProcessFileInformation;
var
  Task: ITask;
  PaintSelf: TRfPaintUnsuedUses;
  CurModification: TDateTime;
begin
  if not FileExists(FFileName) then
  begin
    FProcessed := uspPending;
    Exit;
  end;

  CurModification := TFile.GetLastWriteTime(FFileName);
  if (FUsesStartLine > -1) and
    (SecondsBetween(CurModification, FFileLastModification) = 0)
    then
  begin
    FProcessed := uspComplete;
    Exit;
  end;

  if not EnvControl.AreDependenciasReady then
  begin
    EnvControl.ForceRunDependencies;
    Exit;
  end;

  FFileLastModification := CurModification;
  FProcessed := uspRunning;

  Logger.Debug('Unused : CAll the thread ' + FFileName);
  PaintSelf := Self;

  Task := TTask.Create(
    procedure ()
    var
      UnusedUnit: TUnsedUsesProcessor;
      TimeSpent: TStopWatch;
    begin
      PaintSelf.SetUnsuedUses(nil);
      try
        TimeSpent := TStopwatch.StartNew;
        Logger.Debug('Unused Process file: ' + FFileName);
        UnusedUnit := TUnsedUsesProcessor.Create(FFileName);
        UnusedUnit.SetEnvControl(EnvControl);
        UnusedUnit.Process;
        Logger.Debug('unused setting the value 1');
        PaintSelf.SetUnsuedUses(UnusedUnit.UnusedUses);
        Logger.Debug('unused setting the value 2');
        PaintSelf.SetUsesStartLine(UnusedUnit.UsesStartLine);
        Logger.Debug('unused setting the value 3');
        UnusedUnit.Free;
        FProcessed := uspComplete;
        TimeSpent.Stop;
        if TimeSpent.ElapsedMilliseconds > 1000 then
          Logger.Debug('Time spent on file ' + FFileName + '. Secs ' + IntToStr(TimeSpent.ElapsedMilliseconds div 1000));
      except
        on e: exception do
          Logger.Error(e.Message);
      end;
    end
  );
  Task.Start;
end;

procedure TRfPaintUnsuedUses.RemoveNotifier;
begin
  if Assigned(FEditView) and (FNotifierIndex >= 0) then
  begin
    FEditView.RemoveNotifier(FNotifierIndex);
    FNotifierIndex := -1;
    FEditView := nil;
  end;
end;

procedure TRfPaintUnsuedUses.SetUnsuedUses(List: TDictionary<string, TUsesUnit>);
var
  UsesInfo: TUsesUnit;
begin
  FRcDir.Acquire;
  try
    FNeedRepaint := True;
    Logger.Debug('Unsed log set');

    FUnusedUses.Clear;

    FLowestLine := -1;
    if List = nil then
      Exit;

    Logger.Debug('unused list has ' + IntToStr(List.Count));
    for UsesInfo in List.Values do
    begin
      FUnusedUses.Add(UsesInfo);

      if UsesInfo.Line > FLowestLine then
        FLowestLine := UsesInfo.Line;
    end;

    Logger.Debug('unused lowest :'  + IntToStr(FLowestLine));
  finally
    FRcDir.Release;
  end;
end;

procedure TRfPaintUnsuedUses.SetUsesStartLine(Line: Integer);
begin
  FUsesStartLine := Line;
end;

{ TRfIDENotifier }

procedure TRfIDENotifier.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TRfIDENotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin

end;

constructor TRfIDENotifier.Create;
var
  ModuleServices: IOTAModuleServices;
  i, j: Integer;
  Module: IOTAModule;
  Editor: IOTASourceEditor;
  Ext: string;
begin
  inherited;
  FEditorNotifiers := TList<IOTAEditorNotifier>.Create;

  Logger.Debug('Unsued: TRfIDENotifier.Create');

  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  for i := 0 to ModuleServices.ModuleCount-1 do
  begin
    Module := ModuleServices.Modules[i];

    for j := 0 to Module.ModuleFileCount-1 do
      if Supports(Module.ModuleFileEditors[j], IOTASourceEditor, Editor) then
      begin
        Ext := ExtractFileExt(Editor.FileName).ToUpper;
        if Ext.Equals('.PAS') then
          FEditorNotifiers.Add(TRfEditorNotifier.Create(Editor));
      end;
  end;
end;

destructor TRfIDENotifier.Destroy;
var
  i: Integer;
begin
  for i := 0 to FEditorNotifiers.Count-1 do
    FEditorNotifiers[i].Destroyed;
  FEditorNotifiers.Free;
  inherited;
end;

procedure TRfIDENotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
var
  Module: IOTAModule;
  i: Integer;
  Editor: IOTASourceEditor;
  Ext: string;
begin
  if NotifyCode = ofnFileOpened then
  begin
    Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FileName);
    if not Assigned(Module) then Exit;
    for i := 0 to Module.ModuleFileCount -1 do
      if Supports(Module.ModuleFileEditors[i], IOTASourceEditor, Editor) then
      begin
        Ext := ExtractFileExt(Editor.FileName).ToUpper;
        if Ext.Equals('.PAS') then
          FEditorNotifiers.Add(TRfEditorNotifier.Create(Editor));
      end;
  end;
end;

{ TRfEditorNotifier }

constructor TRfEditorNotifier.Create(ASourceEditor: IOTASourceEditor);
var
  i: Integer;
begin
  inherited Create;
  FEditViewNotifiers := TList<INTAEditViewNotifier>.Create;
  FSourceEditor := ASourceEditor;

  FNotifierIndex := FSourceEditor.AddNotifier(Self);
  for i := 0 to FSourceEditor.EditViewCount -1 do
  begin
    Logger.Debug('TRfIDENotifier.FileNotification ' + FSourceEditor.FileName);
    FEditViewNotifiers.Add(TRfPaintUnsuedUses.Create(FSourceEditor.FileName, FSourceEditor.EditViews[i]));
  end;
end;

destructor TRfEditorNotifier.Destroy;
begin
  RemoveNotifiers;
  FEditViewNotifiers.Free;

  inherited;
end;

procedure TRfEditorNotifier.Destroyed;
begin
  RemoveNotifiers;
end;

procedure TRfEditorNotifier.RemoveNotifiers;
var
  i: Integer;
begin
  for i := 0 to FEditViewNotifiers.Count-1 do
    FEditViewNotifiers[i].Destroyed;
  FEditViewNotifiers.Clear;

  if Assigned(FSourceEditor) and (FNotifierIndex >= 0) then
  begin
    FSourceEditor.RemoveNotifier(FNotifierIndex);
    FNotifierIndex := -1;
    FSourceEditor := nil;
  end;
end;

procedure TRfEditorNotifier.ViewActivated(const View: IOTAEditView);
begin

end;

procedure TRfEditorNotifier.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  if Operation = opInsert then
  begin
    FEditViewNotifiers.Add(TRfPaintUnsuedUses.Create(FSourceEditor.FileName, View));
    Logger.Debug('Unused : TRfEditorNotifier.ViewNotification ' + FSourceEditor.FileName);
  end;
end;

end.
