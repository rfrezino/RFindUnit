unit FindUnit.Worker;

interface

uses
  OtlComm, OtlCommon, OtlTask, OtlThreadPool, OtlParallel, OtlCollections,
  Classes,FindUnit.Parser, Generics.Collections;

type
  TOnFinished = procedure(FindUnits: TObjectList<TFindUnitItem>) of object;

  TParserWorker = class(TObject)
  private
    FDirectoriesPath: TStringList;
    FOnFinished: TOnFinished;
    FPasFiles: TStringList;
    FFindUnits: TObjectList<TFindUnitItem>;

    procedure ListPasFiles;
    procedure ParseFiles;
  public
    constructor Create(DirectoriesPath: TStringList);
    destructor Destroy; override;

    procedure Start(CallBack: TOnFinished);
  end;

implementation

uses
  FindUnit.Utils, SysUtils, Windows;

{ TParserWorker }

constructor TParserWorker.Create(DirectoriesPath: TStringList);
begin
  FDirectoriesPath := DirectoriesPath;

  FPasFiles := TStringList.Create;
  FPasFiles.Sorted := True;
  FPasFiles.Duplicates := dupIgnore;

  FFindUnits := TObjectList<TFindUnitItem>.Create;
end;

destructor TParserWorker.Destroy;
begin
  FPasFiles.Free;
  FDirectoriesPath.Free;
  inherited;
end;

procedure TParserWorker.ListPasFiles;
var
  ResultList: TOmniBlockingCollection;
  RetunrValue: TObject;
  PasValue: TOmniValue;
begin
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
        Parser: TFindUnitParser;
        Item: TFindUnitItem;
      begin
        Parser := TFindUnitParser.Create(FPasFiles[index]);
        try
          try
            Item := Parser.Process;
            if Item <> nil then
              ResultList.Add(Item);
          except
          end;
        finally
          Parser.Free;
        end;
      end
    );

  while ResultList.Take(PasValue) do
    FFindUnits.Add(TFindUnitItem(PasValue.AsObject));
end;

procedure TParserWorker.Start;
begin
  ListPasFiles;
  ParseFiles;
  CallBack(FFindUnits);
end;

end.
