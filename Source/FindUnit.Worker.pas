unit FindUnit.Worker;

interface

uses
  OtlCommFU, OtlCommonFU, OtlTaskFU, OtlThreadPoolFU, OtlParallelFU, OtlCollectionsFU,
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

    procedure ListPasFiles;
    procedure ParseFiles;
  public
    constructor Create(DirectoriesPath: TStringList; Files: TStringList);
    destructor Destroy; override;

    procedure Start(CallBack: TOnFinished);
  end;

implementation

uses
  FindUnit.Utils, SysUtils, Windows;

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

  FFindUnits := TObjectList<TPasFile>.Create;

  FIncluder := TIncludeHandlerInc.Create(FDirectoriesPath.Text);
end;

destructor TParserWorker.Destroy;
begin
  FPasFiles.Free;
  FDirectoriesPath.Free;
  FIncluder.Free;
  inherited;
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

procedure TParserWorker.Start;
begin
  FIncluder.Process;
  ListPasFiles;
  ParseFiles;
  CallBack(FFindUnits);
end;

end.
