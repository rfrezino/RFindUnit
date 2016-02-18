unit FindUnit.Worker;

interface

uses
  OtlCommonFU, OtlTaskFU, OtlThreadPoolFU, OtlParallelFU, OtlCollectionsFU,
  Classes,FindUnit.PasParser, Generics.Collections, FindUnit.IncluderHandlerInc, Log4Pascal;

type
  TOnFinished = procedure(FindUnits: TObjectList<TPasFile>) of object;

  TParserWorker = class(TObject)
  private
    FDirectoriesPath: TStringList;
    FOnFinished: TOnFinished;
    FPasFiles: TStringList;
    FFindUnits: TObjectList<TPasFile>;
    FIncluder: TIncludeHandlerInc;
    FParsedItems: Integer;

    FDcuFiles: TStringList;
    FParseDcuFile: Boolean;

    procedure ListPasFiles;
    procedure ListDcuFiles;

    procedure RemoveDcuFromExistingPasFiles;
    procedure GeneratePasFromDcus;
    procedure ParseFiles;
    function GetItemsToParse: Integer;
  public
    constructor Create(DirectoriesPath: TStringList; Files: TStringList);
    destructor Destroy; override;

    procedure Start(CallBack: TOnFinished);

    property ItemsToParse: Integer read GetItemsToParse;
    property ParsedItems: Integer read FParsedItems;

    property ParseDcuFile: Boolean read FParseDcuFile write FParseDcuFile;
  end;

implementation

uses
  FindUnit.Utils, SysUtils, Windows, FindUnit.DcuDecompiler;

{ TParserWorker }

constructor TParserWorker.Create(DirectoriesPath: TStringList; Files: TStringList);
begin
  FDirectoriesPath := TStringList.Create;
  if DirectoriesPath <> nil then
    FDirectoriesPath.Text := TPathConverter.ConvertPathsToFullPath(DirectoriesPath.Text);
  DirectoriesPath.Free;

  FPasFiles := TStringList.Create;
  FPasFiles.Sorted := True;
  FPasFiles.Duplicates := dupIgnore;
  if Files <> nil then
    FPasFiles.AddStrings(Files);
  Files.Free;

  FDcuFiles := TStringList.Create;
  FDcuFiles.Sorted := True;
  FDcuFiles.Duplicates := dupIgnore;

  FFindUnits := TObjectList<TPasFile>.Create;

  FIncluder := TIncludeHandlerInc.Create(FDirectoriesPath.Text);
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
  DcuDecompiler := TDcuDecompiler.Create(FPasFiles, FindUnitDir);
  try

  finally
    DcuDecompiler.Free;
  end;
end;

procedure TParserWorker.ListDcuFiles;
var
  ResultList: TOmniBlockingCollection;
  RetunrValue: TObject;
  DcuFile: TOmniValue;
begin
  if not FParseDcuFile then
    Exit;

  FDirectoriesPath.Add(DirRealeaseWin32);

  ResultList := TOmniBlockingCollection.Create;

  Parallel.ForEach(0, FDirectoriesPath.Count -1)
    .Into(ResultList)
    .Execute(
      procedure (const index: Integer; var result: TOmniValue)
      var
      DcuFiles: TStringList;
      DcuFile: string;
      begin
        try
          DcuFiles := GetAllDcuFilesFromPath(FDirectoriesPath[index]);
          try
            for DcuFile in DcuFiles do
              ResultList.Add(DcuFile);
          finally
            DcuFiles.Free;
          end;
        except
          on E: exception do
            Logger.Error('TParserWorker.ListDcuFiles: ' + e.Message);
        end;
      end
    );

  while ResultList.Take(DcuFile) do
    FDcuFiles.Add(DcuFile.AsString);
end;

procedure TParserWorker.ListPasFiles;
var
  ResultList: TOmniBlockingCollection;
  RetunrValue: TObject;
  PasValue: TOmniValue;
begin
  //DEBUG
//  FPasFiles.Add('C:\Program Files (x86)\Embarcadero\RAD Studio\8.0\source\rtl\common\Classes.pas');
//  Exit;

  if FPasFiles.Count > 0 then
    Exit;

  ResultList := TOmniBlockingCollection.Create;

  Parallel.ForEach(0, FDirectoriesPath.Count -1)
    .Into(ResultList)
    .Execute(
      procedure (const index: Integer; var result: TOmniValue)
      var
      PasFiles: TStringList;
      PasFile: string;
      begin
        try
          PasFiles := GetAllPasFilesFromPath(FDirectoriesPath[index]);
          try
            for PasFile in PasFiles do
              ResultList.Add(PasFile);
          finally
            PasFiles.Free;
          end;
        except
          on E: exception do
            Logger.Error('TParserWorker.ListPasFiles: ' + e.Message);
        end;
      end
    );

  while ResultList.Take(PasValue) do
    FPasFiles.Add(PasValue.AsString);
end;

procedure TParserWorker.ParseFiles;
var
  ResultList: TOmniBlockingCollection;
  PasValue: TOmniValue;
begin
  ResultList := TOmniBlockingCollection.Create;
  FParsedItems := 0;

  Parallel.ForEach(0, FPasFiles.Count -1)
    .Into(ResultList)
    .Execute(
      procedure (const index: Integer; var result: TOmniValue)
      var
        Parser: TPasFileParser;
        Item: TPasFile;
      begin
        Parser := TPasFileParser.Create(FPasFiles[index]);
        try
          try
            Parser.SetIncluder(FIncluder);
            InterlockedIncrement(FParsedItems);
            Item := Parser.Process;
            if Item <> nil then
              ResultList.Add(Item);
          except
            on e: exception do
              Logger.Error('TParserWorker.ParseFiles: ' + e.Message);
          end;
        finally
          Parser.Free;
        end;
      end
    );

  while ResultList.Take(PasValue) do
    FFindUnits.Add(TPasFile(PasValue.AsObject));
end;

procedure TParserWorker.RemoveDcuFromExistingPasFiles;
var
  DcuFilesName: TStringList;
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
      PasFile := StringReplace(PasFile, '.DCU', '', [rfReplaceAll]);

      if FPasFiles.IndexOf(DcuFile) > -1 then
        FDcuFiles.Delete(I);
    end;
  finally
    PasFilesName.Free;
  end;
end;

procedure TParserWorker.Start;
begin
  FIncluder.Process;
  ListPasFiles;
  ListDcuFiles;
  RemoveDcuFromExistingPasFiles;
  GeneratePasFromDcus;
  ParseFiles;
  CallBack(FFindUnits);
end;

end.
