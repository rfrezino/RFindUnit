unit FindUnit.PaintUnusedUses;

interface

uses
  Log4Pascal,
  ToolsAPI,

  FindUnit.OTAUtils,
  FindUnit.UnusedUses,

  System.Generics.Collections,
  System.SyncObjs,
  System.SysUtils,
  System.Threading,
  System.Types,

  Vcl.Graphics;

type
  TRfPaintUnsuedUses = class(TNotifierObject, IOTAActionServices, INTAEditViewNotifier)
  private
    procedure Modified;
  protected
    FUnusedUses: TList<TUsesUnit>;
    FRcDir: TCriticalSection;
    FLowestLine: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    { INTAEditViewNotifier }
    procedure EditorIdle(const View: IOTAEditView);
    procedure BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
    procedure PaintLine(const View: IOTAEditView; LineNumber: Integer;
      const LineText: PAnsiChar; const TextWidth: Word; const LineAttributes: TOTAAttributeArray;
      const Canvas: TCanvas; const TextRect: TRect; const LineRect: TRect; const CellSize: TSize);
    procedure EndPaint(const View: IOTAEditView);

    procedure SetUnsuedUses(List: TDictionary<string, TUsesUnit>);

    function CloseFile(const FileName: string): Boolean;
    function OpenFile(const FileName: string): Boolean;
    function OpenProject(const ProjName: string; NewProjGroup: Boolean): Boolean;
    function ReloadFile(const FileName: string): Boolean;
    function SaveFile(const FileName: string): Boolean;
  end;

implementation

{ TRfPaintUnsuedUses }
procedure TRfPaintUnsuedUses.BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
begin

end;

function TRfPaintUnsuedUses.CloseFile(const FileName: string): Boolean;
begin
end;

constructor TRfPaintUnsuedUses.Create;
begin
  FRcDir := TCriticalSection.Create;
end;

destructor TRfPaintUnsuedUses.Destroy;
begin
  FRcDir.Free;
  inherited;
end;

procedure TRfPaintUnsuedUses.EditorIdle(const View: IOTAEditView);
begin

end;

procedure TRfPaintUnsuedUses.EndPaint(const View: IOTAEditView);
begin

end;

procedure TRfPaintUnsuedUses.Modified;
begin

end;

function TRfPaintUnsuedUses.OpenFile(const FileName: string): Boolean;
begin

end;

function TRfPaintUnsuedUses.OpenProject(const ProjName: string; NewProjGroup: Boolean): Boolean;
begin

end;

procedure TRfPaintUnsuedUses.PaintLine(const View: IOTAEditView; LineNumber: Integer; const LineText: PAnsiChar;
  const TextWidth: Word; const LineAttributes: TOTAAttributeArray; const Canvas: TCanvas; const TextRect,
  LineRect: TRect; const CellSize: TSize);
var
  I: Integer;
  FBackgroundColor: TColor;
  x,y: Integer;

  procedure DrawColor(Color: TColor; Item: TUsesUnit);
  var
    LTextWidth: Integer;
    LTextHeight: Integer;
  begin
    LTextWidth := Canvas.TextWidth(Item.Name);
    LTextHeight := Canvas.TextHeight(Item.Name);

    Canvas.Brush.Color := FBackgroundColor;
    Canvas.FillRect(Bounds(x, y, LTextWidth, LTextHeight));
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := ColorToRGB(Color);
    Canvas.Rectangle(Bounds(x+1, y+1, LTextHeight-2, LTextHeight-2));
    Inc(x, LTextHeight);
  end;
begin
  if LineNumber > FLowestLine then
    Exit;

  FBackgroundColor := clRed;
  for I := 0 to FUnusedUses.Count -1 do
    if FUnusedUses[I].Line = LineNumber then
      DrawColor(clRed, FUnusedUses[I]);
end;

function TRfPaintUnsuedUses.ReloadFile(const FileName: string): Boolean;
begin

end;

function TRfPaintUnsuedUses.SaveFile(const FileName: string): Boolean;
var
  Task: ITask;
  Editor: IOTASourceEditor;
  PaintSelf: TRfPaintUnsuedUses;
begin
  Logger.Debug('CAll the thread ' + FileName);
  Editor := ActiveSourceEditor;
  if Editor = nil then
    Exit;

  Logger.Debug('CAll the thread 1 ' + Editor.FileName);

  if Editor.FileName <> FileName then
    Exit;

  Logger.Debug('CAll the thread 2');
  PaintSelf := Self;
  Task := TTask.Create(
    procedure ()
    var
      UnusedUnit: TUnsedUsesProcessor;
    begin
      PaintSelf.SetUnsuedUses(nil);
      UnusedUnit := TUnsedUsesProcessor.Create(Editor.FileName);
      try
        UnusedUnit.Process;
        PaintSelf.SetUnsuedUses(UnusedUnit.UnusedUses);
        UnusedUnit.Free;
      except
        on e: exception do
          Logger.Error(e.Message);
      end;
    end
  )
end;

procedure TRfPaintUnsuedUses.SetUnsuedUses(List: TDictionary<string, TUsesUnit>);
var
  UsesInfo: TUsesUnit;
begin
  FRcDir.Acquire;
  try
    FUnusedUses.Clear;
    FLowestLine := -1;
    if List = nil then
      Exit;

    for UsesInfo in List.Values do
    begin
      FUnusedUses.Add(UsesInfo);

      if UsesInfo.Line > FLowestLine then
        FLowestLine := UsesInfo.Line;
    end;
  finally
    FRcDir.Release;
  end;
end;

end.
