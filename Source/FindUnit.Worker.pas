unit FindUnit.Worker;

interface

uses
  Classes, FindUnit.IncluderHandlerInc, FindUnit.PasParser, Generics.Collections,
  Log4Pascal, SimpleParser.Lexer.Types,
  System.Threading;

type
  TOnFinished = procedure(FindUnits: TObjectList<TPasFile>) of object;

  TParserWorker = class(TObject)
  private
    FDirectoriesPath: TStringList;
    FOnFinished: TOnFinished;
    FPasFiles: TStringList;
    FFindUnits: TObjectList<TPasFile>;
    FIncluder: IIncludeHandler;
    FParsedItems: Integer;

    FDcuFiles: TStringList;
    FParseDcuFile: Boolean;

    procedure ListPasFiles;
    procedure ListDcuFiles;

    procedure RemoveDcuFromExistingPasFiles;
    procedure GeneratePasFromDcus;
    procedure ParseFilesParallel;
    procedure ParseFiles;
    function GetItemsToParse: Integer;
    procedure RunTasks;
  public
    constructor Create(var DirectoriesPath: TStringList; var Files: TStringList);
    destructor Destroy; override;

    procedure Start(CallBack: TOnFinished); overload;
    function Start: TObjectList<TPasFile>; overload;

    property ItemsToParse: Integer read GetItemsToParse;
    property ParsedItems: Integer read FParsedItems;

    property ParseDcuFile: Boolean read FParseDcuFile write FParseDcuFile;
  end;

implementation

uses
  SysUtils,
  Windows,

  FindUnit.DcuDecompiler,
  FindUnit.Utils;

{ TParserWorker }

constructor TParserWorker.Create(var DirectoriesPath: TStringList; var Files: TStringList);
begin
  FDirectoriesPath := TStringList.Create;
  if DirectoriesPath <> nil then
    FDirectoriesPath.Text := TPathConverter.ConvertPathsToFullPath(DirectoriesPath.Text);
  FreeAndNil(DirectoriesPath);

  FPasFiles := TStringList.Create;
  FPasFiles.Sorted := True;
  FPasFiles.Duplicates := dupIgnore;
  if Files <> nil then
    FPasFiles.AddStrings(Files);
  FreeAndNil(Files);

  FDcuFiles := TStringList.Create;
  FDcuFiles.Sorted := True;
  FDcuFiles.Duplicates := dupIgnore;

  FFindUnits := TObjectList<TPasFile>.Create;

  FIncluder := TIncludeHandlerInc.Create(FDirectoriesPath.Text) as IIncludeHandler;
end;

destructor TParserWorker.Destroy;
begin
  FPasFiles.Free;
  FDirectoriesPath.Free;
//  FIncluder.Free; //Weak reference
  FDcuFiles.Free;
  inherited;
end;

function TParserWorker.GetItemsToParse: Integer;
begin
  Result := 0;
  if (FPasFiles <> nil) then
    Result := FPasFiles.Count;
end;

procedure TParserWorker.GeneratePasFromDcus;
var
  DcuDecompiler: TDcuDecompiler;
begin
  if not FParseDcuFile then
    Exit;

  DcuDecompiler := TDcuDecompiler.Create;
  try
    DcuDecompiler.ProcessFiles(FDcuFiles);
  finally
    DcuDecompiler.Free;
  end;
end;

procedure TParserWorker.ListDcuFiles;
var
  ResultList: TThreadList<string>;
  DcuFile: string;
begin
  if not FParseDcuFile then
    Exit;

  FDirectoriesPath.Add(DirRealeaseWin32);
  ResultList := TThreadList<string>.Create;

  TParallel.&For(0, FDirectoriesPath.Count -1,
    procedure (index: Integer)
      var
      DcuFiles: TStringList;
      DcuFile: string;
      begin
        try
          DcuFiles := GetAllDcuFilesFromPath(FDirectoriesPath[index]);
          try
            for DcuFile in DcuFiles do
              ResultList.Add(Trim(DcuFile));
          finally
            DcuFiles.Free;
          end;
        except
          on E: exception do
            Logger.Error('TParserWorker.ListDcuFiles: ' + e.Message);
        end;
      end);

  for DcuFile in ResultList.LockList do
    FDcuFiles.Add(DcuFile);

  ResultList.Free;
end;

procedure TParserWorker.ListPasFiles;
var
  ResultList: TThreadList<string>;
  PasValue: string;
begin
  //DEBUG
//  FPasFiles.Add('C:\Program Files (x86)\Embarcadero\RAD Studio\8.0\source\rtl\common\Classes.pas');
//  Exit;

  if FPasFiles.Count > 0 then
    Exit;

  ResultList := TThreadList<string>.Create;

  TParallel.&For(0, FDirectoriesPath.Count -1,
      procedure (index: Integer)
      var
      PasFiles: TStringList;
      PasFile: string;
      begin
        try
          if not DirectoryExists(FDirectoriesPath[index]) then
            Exit;

          PasFiles := GetAllPasFilesFromPath(FDirectoriesPath[index]);
          try
            for PasFile in PasFiles do
              ResultList.Add(Trim(PasFile));
          finally
            PasFiles.Free;
          end;
        except
          on E: exception do
            Logger.Error('TParserWorker.ListPasFiles: ' + e.Message);
        end;
      end
    );

  for PasValue in ResultList.LockList do
    FPasFiles.Add(PasValue);

  ResultList.Free;
end;

procedure TParserWorker.ParseFiles;
var
  I: Integer;
  Parser: TPasFileParser;
  Item: TPasFile;
  Step: string;
begin
  for I := 0 to FPasFiles.Count -1 do
  begin
    try
      Parser := TPasFileParser.Create(FPasFiles[I]);
      try
        Step := 'Parser.SetIncluder(FIncluder)';
        Parser.SetIncluder(FIncluder);
        Step := 'InterlockedIncrement(FParsedItems);';
        InterlockedIncrement(FParsedItems);
        Step := 'Parser.Process';
        Item := Parser.Process;
        if Item <> nil then
          FFindUnits.Add(Item);
      except
        on e: exception do
          Logger.Error('TParserWorker.ParseFiles[%s]: %s', [Step, e.Message]);
      end;
    finally
      Parser.Free;
    end;
  end;
end;

procedure TParserWorker.ParseFilesParallel;
var
  ResultList: TThreadList<TPasFile>;
  PasValue: TPasFile;
begin
  ResultList := TThreadList<TPasFile>.Create;
  FParsedItems := 0;

  Logger.Debug('TParserWorker.ParseFiles: Starting parseing files.');

  TParallel.&For(0, FPasFiles.Count -1,
      procedure (index: Integer)
      var
        Parser: TPasFileParser;
        Item: TPasFile;
        Step: string;
      begin
        try
          Parser := nil;
          try
            Step := 'InterlockedIncrement(FParsedItems);';
            InterlockedIncrement(FParsedItems);
            Step := 'Create';
            Parser := TPasFileParser.Create(FPasFiles[index]);
            Step := 'Parser.SetIncluder(FIncluder)';
            Parser.SetIncluder(FIncluder);
            Step := 'Parser.Process';
            Item := Parser.Process;
            if Item <> nil then
              ResultList.Add(Item);
          except
            on e: exception do
              Logger.Error('TParserWorker.ParseFiles[%s]: %s', [Step, e.Message]);
          end;
        finally
        end;
      end
    );

  Logger.Debug('TParserWorker.ParseFiles: Put results together.');
  for PasValue in ResultList.LockList do
    FFindUnits.Add(PasValue);

  Logger.Debug('TParserWorker.ParseFiles: Finished.');

  ResultList.Free;
end;

procedure TParserWorker.RemoveDcuFromExistingPasFiles;
var
  PasFilesName: TStringList;
  I: Integer;
  PasFile: string;
  DcuFile: string;
begin
  if FDcuFiles.Count = 0 then
    Exit;

  PasFilesName := TStringList.Create;
  PasFilesName.Sorted := True;
  PasFilesName.Duplicates := dupIgnore;
  try
    for I := 0 to FPasFiles.Count -1 do
    begin
      PasFile := FPasFiles[i];
      PasFile := UpperCase(ExtractFileName(PasFile));
      PasFile := StringReplace(PasFile, '.PAS', '', [rfReplaceAll]);
      PasFilesName.Add(PasFile);
    end;

    for I := FDcuFiles.Count -1 downto 0 do
    begin
      DcuFile := FDcuFiles[i];
      DcuFile:= UpperCase(ExtractFileName(DcuFile));
      DcuFile := StringReplace(DcuFile, '.DCU', '', [rfReplaceAll]);

      if PasFilesName.IndexOf(DcuFile) > -1 then
        FDcuFiles.Delete(I);
    end;
  finally
    PasFilesName.Free;
  end;
end;

function TParserWorker.Start: TObjectList<TPasFile>;
begin
  RunTasks;
  Result := FFindUnits;
end;

procedure TParserWorker.RunTasks;
var
  Step: string;

  procedure OutPutStep(CurStep: string);
  begin
    Step := CurStep;
    Logger.Debug('TParserWorker.RunTasks: ' + CurStep);
  end;
begin
  try
    OutPutStep('FIncluder.Process');
    TIncludeHandlerInc(FIncluder).Process;
    OutPutStep('ListPasFiles');
    ListPasFiles;
    OutPutStep('ListDcuFiles');
    ListDcuFiles;
    OutPutStep('RemoveDcuFromExistingPasFiles');
    RemoveDcuFromExistingPasFiles;
    OutPutStep('GeneratePasFromDcus');
    GeneratePasFromDcus;
    OutPutStep('ParseFiles');
    ParseFilesParallel;
    OutPutStep('Finished');
  except
    on E: exception do
      Logger.Error('TParserWorker.RunTasks[%s]: %s ',[Step, e.Message]);
  end;
end;

procedure TParserWorker.Start(CallBack: TOnFinished);
begin
  RunTasks;
  CallBack(FFindUnits);
end;

end.

