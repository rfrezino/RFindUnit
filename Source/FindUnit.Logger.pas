unit FindUnit.Logger;

interface

uses
  SysUtils, Controls, Classes, SyncObjs, Forms, Dialogs;

type
  TLogType = (
    ltInfo,
    ltDebug,
    ltWarn,
    ltError,
    ltDBError,
    ltDBWarn,
    ltWeb,
    ltBenchMark,
    ltUnitTest,
    ltRelatorio,
    ltDebugViagem,
    ltDev,
    ltLoad,
    ltInt
  );

  TWriteType = set of TLogType;
  TOnWriteLog = procedure(AType: TLogType; AMsg: string; ADateTime: TDateTime);

  const
    KBYTE = 1024;
    MBYTE = 1024 * KBYTE;

  type
  TThreadWriteLogOnDisk = class(TThread)
  const
    WRITE_FORCED_DELAY = 20000; // Corresponde a 20 segundos
  private
    FRCMessages: TCriticalSection;
    //Arquivo onde vai ser escrito o log
    FLogFile: TFileStream;
    //Lista de mensagens pendentes para escrita no HD
    FPendingMessages: TStringStream;
    //Lista de mensagens que estão sendo escritas, para não travar a região critica
    FWrittingMessages: TStringStream;
    //Nos 20 segundos iniciais escreve tudo no log pra caso o servidor feche derrepente e não consigamos identificar a mensagem
    FDateTimeInicialization: TDateTime;

    procedure WriteLogsOnDisk;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetLogFile(var ALogFile: TFileStream);
    procedure WriteLog(AMensagem: string; AForceWrite: Boolean);
  end;

  THelperLog = class
  const
    FILE_MAX_SIZE = 100 * MBYTE;
  private
    FLastLogType: TLogType;
    FCountMsg: array[TLogType] of NativeUInt;
    FCurrentDate: TDate;
    FPath: string;
    FFilename: string;
    FFileStream: TFileStream;
    FStoreDays: Integer;
    FWriteType: TWriteType;
    FWriteStandardOutput: Boolean;
    FOnWriteLog: TOnWriteLog;
    FThreadWriteLogOnDisk: TThreadWriteLogOnDisk;
    FisDelphiRunning: Boolean;
    procedure DeleteOldFiles;
    function GetDateFromFilename(AFilename: string): TDateTime;
    // controle de escrita de mensagens no arquivo de log
    procedure ChangeLogFile;
    procedure ClearCounters;
    procedure WriteLog(AType: TLogType; AMsg: string);
    // controle de acesso as properties
    function GetCountMsg(Index: TLogType): Integer;
  protected
  public
    // controle da classe
    constructor Create(APathLog: string);
    destructor Destroy; override;
    // controle de escrita de mensagens no arquivo de log
    procedure Info(AMsg: string); overload;
    procedure Info(const AMsg: string; const AArgs: array of const); overload;
    procedure Dev(AMsg: string); overload;
    procedure Dev(const AMsg: string; const AArgs: array of const); overload;
    procedure Debug(AMsg: string); overload;
    procedure Debug(const AMsg: string; const AArgs: array of const); overload;
    procedure Warn(AMsg: string); overload;
    procedure Warn(const AMsg: string; const AArgs: array of const); overload;
    procedure Error(AMsg: string; E: Exception = nil); overload;
    procedure Error(const AMsg: string; const AArgs: array of const; E: Exception = nil); overload;
    procedure DBWarn(AMsg: string); overload;
    procedure DBWarn(const AMsg: string; const AArgs: array of const); overload;
    procedure DBError(AMsg: string); overload;
    procedure DBError(const AMsg: string; const AArgs: array of const); overload;
    procedure UnitTest(AMsg: string); overload;
    procedure UnitTest(AMsg: string; const AArgs: array of const); overload;
    procedure BenchMark(AMsg: string); overload;
    procedure BenchMark(AMsg: string; const AArgs: array of const); overload;

    // properties
    property CountMsg[Index: TLogType]: Integer read GetCountMsg;
    property Path: string read FPath;
    property Filename: string read FFilename;
    property WriteType: TWriteType read FWriteType write FWriteType;
    property WriteStandardOutput: Boolean read FWriteStandardOutput write FWriteStandardOutput;
    property StoreDays: Integer read FStoreDays write FStoreDays;
    property OnWriteLog: TOnWriteLog read FOnWriteLog write FOnWriteLog;
  end;

var
  Logger: THelperLog;

implementation

uses

  Windows, IniFiles, StrUtils, DateUtils, FindUnit.Utils;

var
  strTLogType: array[TLogType] of string;
  RCHelperLog: TCriticalSection;

function Format(const AFormat: string; const Args: array of const): string;
begin
  try
    Result := SysUtils.Format(AFormat, Args, FormatSettings);
  except
    on E: exception do
      Logger.Error('THelperLog.Format[%s]: Error of parameters.', [AFormat], E);
  end;
end;

{ THelperLog }
procedure THelperLog.BenchMark(AMsg: string);
begin
  WriteLog(ltBenchMark, AMsg);
end;

procedure THelperLog.BenchMark(AMsg: string; const AArgs: array of const);
begin
  BenchMark(Format(AMsg, AArgs));
end;

procedure THelperLog.ChangeLogFile;
var
  LOldFilename: string;
  LFileTime: TTime;
  LZipFileName: string;
  LFileNew: string;

  function SetFileName: string;
  begin
    LFileNew := Format('%slog_%s_%d-%d.txt', [FPath, FormatDateTime('yyyy-mm-dd', FCurrentDate), HourOf(LFileTime), MinuteOf(LFileTime)]);
    LZipFileName := StringReplace(LFileNew, '.txt', '.zip', [rfReplaceAll]);
    Result := LFileNew;
  end;

begin
  if (FCurrentDate = Date) and (FFileStream.Size < FILE_MAX_SIZE) then
    Exit;

  LOldFilename := FFilename;
  // atualiza a data atual e o nome do arquivo de log
  FCurrentDate := DateOf(Now);

  LFileTime := Now;
  FFilename := Format('%slog_%s.txt', [FPath, FormatDateTime('yyyy-mm-dd', FCurrentDate)]);

  if FFileStream <> nil then
  begin
    while FileExists(SetFileName) or FileExists(LZipFileName) do
      LFileTime := IncMinute(LFileTime);

    FreeAndNil(FFileStream);
    RenameFile(FFilename, LFileNew);
  end;

  // cria o arquivo de log se necessário
  if not FileExists(FFilename) then
    FileClose(FileCreate(FFilename));
  // carrega o arquivo e posiciona o cursor no final

  FFileStream := TFileStream.Create(FFilename, fmOpenWrite + fmShareDenyNone);
  FFileStream.Position := FFileStream.Size;
  FThreadWriteLogOnDisk.SetLogFile(FFileStream);
  ClearCounters;

  DeleteOldFiles;
end;

procedure THelperLog.ClearCounters;
var
  i: TLogType;
begin
  for i := Low(TLogType) to High(TLogType) do
    FCountMsg[i] := 0;
end;

function DelphiIsRunning: Boolean;
begin
  Result := {$IFDEF DEBUG} True or {$ENDIF} (FindWindowA('TAppBuilder', nil) <> 0);
end;

constructor THelperLog.Create(APathLog: string);
begin
  // inicializa thread que escreve efetivamente no disco
  FThreadWriteLogOnDisk := TThreadWriteLogOnDisk.Create;
  FThreadWriteLogOnDisk.Resume;

  // inicializa os atributos da classe
  ClearCounters;
  FLastLogType := ltInfo;
  FCurrentDate := 0;
  FPath := APathLog;
  FFilename := '';
  FWriteType := [Low(TLogType)..High(TLogType)];
  FWriteStandardOutput := True;
  FStoreDays := 14;
  FOnWriteLog := nil;

  // cria o caminho do log se não existir
  if not DirectoryExists(FPath) then
    CreateDir(FPath);

  // carrega o arquivo de log
  ChangeLogFile;
  FisDelphiRunning := DelphiIsRunning;
end;

procedure THelperLog.DBError(const AMsg: string; const AArgs: array of const);
begin
  DBError(Format(AMsg, AArgs));
end;

procedure THelperLog.DBWarn(AMsg: string);
begin
  WriteLog(ltDBWarn, AMsg);
end;

procedure THelperLog.DBWarn(const AMsg: string; const AArgs: array of const);
begin
  DBWarn(Format(AMsg, AArgs));
end;

procedure THelperLog.DBError(AMsg: string);
begin
  WriteLog(ltDBError, AMsg);
end;

procedure THelperLog.Debug(AMsg: string);
begin
  WriteLog(ltDebug, AMsg);
end;

procedure THelperLog.Debug(const AMsg: string; const AArgs: array of const);
begin
  Debug(Format(AMsg, AArgs));
end;

procedure THelperLog.DeleteOldFiles;
var
  i: Integer;
  LListFiles: TStringList;
  LDataLimite: TDateTime;
  LDateFile: TDateTime;
begin
  LDataLimite := IncDay(Now, -FStoreDays);
  // obtém a lista de arquivos da pasta de log
  LListFiles := GetAllFilesFromPath(FPath, '*.zip');
  try
    for i := 0 to LListFiles.Count - 1 do
    begin
      // procura somente pelos arquivos de log zipados
      if Pos('log_', LListFiles.Strings[i]) > 0 then
      begin
        LDateFile := GetDateFromFilename(LListFiles.Strings[i]);
        if (LDateFile <> 0) and (LDateFile < LDataLimite) then
          DeleteFile(PWideChar(LListFiles.Strings[i]));
      end;
    end;
  finally
    LListFiles.Free;
  end;
end;

destructor THelperLog.Destroy;
begin
  FThreadWriteLogOnDisk.Terminate;
  FFileStream.Free;
  inherited;
end;

procedure THelperLog.Dev(AMsg: string);
begin
  if not FisDelphiRunning then
    Exit;

  WriteLog(ltDev, AMsg);
end;

procedure THelperLog.Dev(const AMsg: string; const AArgs: array of const);
begin
  if not FisDelphiRunning then
    Exit;
  Dev(Format(AMsg, AArgs));
end;

procedure THelperLog.Error(AMsg: string; E: Exception);
begin
  if E <> nil then
    AMsg := AMsg + ' | ' + E.ClassName + ': ' + e.Message;
  WriteLog(ltError, AMsg);
end;

procedure THelperLog.Error(const AMsg: string; const AArgs: array of const; E: Exception);
begin
  Error(Format(AMsg, AArgs), E);
end;

function THelperLog.GetCountMsg(Index: TLogType): Integer;
begin
  Result := FCountMsg[Index];
end;

function THelperLog.GetDateFromFilename(AFilename: string): TDateTime;
var
  LFormatSettings: TFormatSettings;
begin
  // formato da data de log
  LFormatSettings.DateSeparator := '-';
  LFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  // obtém somente o nome do arquivo sem a extensão
  AFilename := ExtractFileName(AFilename);
  Fetch(AFilename, 'log_');
  AFilename := Fetch(AFilename, '.');
  // converte convete para data o nome do arquivo
  try
    Result := StrToDateTime(AFilename, LFormatSettings);
  except
    Result := 0;
  end;
end;

procedure THelperLog.Info(const AMsg: string; const AArgs: array of const);
begin
  Info(Format(AMsg, AArgs));
end;

procedure THelperLog.UnitTest(AMsg: string);
begin
  WriteLog(ltUnitTest, AMsg);
end;

procedure THelperLog.UnitTest(AMsg: string; const AArgs: array of const);
begin
  UnitTest(Format(AMsg, AArgs));
end;

procedure THelperLog.Info(AMsg: string);
begin
  WriteLog(ltInfo, AMsg);
end;

procedure THelperLog.Warn(const AMsg: string; const AArgs: array of const);
begin
  Warn(Format(AMsg, AArgs));
end;

procedure THelperLog.Warn(AMsg: string);
begin
  WriteLog(ltWarn, AMsg);
end;

const
  MAX_COUNTER = 4294967295;

procedure THelperLog.WriteLog(AType: TLogType; AMsg: string);
var
  LMessage: string;
begin
  if Application.Terminated then
    Exit;

  try
    // formata a mensagem quer será escrita no log e no standard output
    LMessage := Format('%s | %d | %s | %s', [
      FormatDateTime('dd.mm.yyyy hh":"nn":"ss"."zzz', Now),
      GetCurrentThreadId,
      strTLogType[AType],
      AMsg
    ]);
  except
    On E: Exception do
    begin
      Error('THelperLog.WriteLog', E);
      LMessage := AMsg;
    end;
  end;

  RCHelperLog.Acquire;
  try
    // verifica se é necessário trocar o arquivo de log
    ChangeLogFile;

    // só escreve a mensagem no log se o tipo estiver dentro do conjunto definido no config
    if AType in FWriteType then
    begin
      // escreve a mensagem no standard output
      {$IFDEF CONSOLE}
      if FWriteStandardOutput then
      begin
        if FLastLogType <> AType then
        begin
          case AType of
            ltInfo,
            ltDebug,
            ltDebugViagem,
            ltRelatorio:  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
            ltWarn:       SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN);
            ltError:      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED);
            ltDBError:    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_INTENSITY);
            ltDBWarn:     SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY);
            ltLoad,
            ltInt:        SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_GREEN);
            ltUnitTest:   SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_GREEN);
            ltBenchMark:  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_GREEN or FOREGROUND_INTENSITY);
          end;
          FLastLogType := AType;
        end;
      Writeln(LMessage);
      end;
      {$ENDIF}

      // escreve a mensagem no arquivo de log
      FThreadWriteLogOnDisk.WriteLog(LMessage, AType in [ltError]);
    end;

    // incrementa o contador de mensagens por tipo
    if MAX_COUNTER = FCountMsg[AType] then
      FCountMsg[AType] := 0;

    Inc(FCountMsg[AType]);

    // dispara o evento de escrita no log
    if Assigned(FOnWriteLog) then
      FOnWriteLog(AType, AMsg, Now);

  finally
    RCHelperLog.Release;
  end;
end;

{ TThreadWriteLogOnDisk }

constructor TThreadWriteLogOnDisk.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;

  FRCMessages := TCriticalSection.Create;
  FPendingMessages := TStringStream.Create;
  FWrittingMessages := TStringStream.Create;
  NameThreadForDebugging('TThreadWriteLogOnDisk', Self.ThreadID);
end;

destructor TThreadWriteLogOnDisk.Destroy;
begin
  FreeAndNil(FRCMessages);
  FreeAndNil(FWrittingMessages);
  FreeAndNil(FPendingMessages);
  inherited;
end;

procedure TThreadWriteLogOnDisk.Execute;
begin
  FDateTimeInicialization := Now;
  while not Terminated do
  begin
    Sleep(1000);
    if not Terminated  then
      WriteLogsOnDisk;
  end;
end;

procedure TThreadWriteLogOnDisk.SetLogFile(var ALogFile: TFileStream);
begin
  FLogFile := ALogFile;
end;

procedure TThreadWriteLogOnDisk.WriteLog(AMensagem: string; AForceWrite: Boolean);
begin
  FRCMessages.Acquire;
  try
    AMensagem := AMensagem + #13#10;
    FPendingMessages.Position := FPendingMessages.Size;
    FPendingMessages.WriteString(AMensagem);

    if AForceWrite then
      WriteLogsOnDisk;
  finally
    FRCMessages.Release;
  end;

  if AForceWrite or ((Now - FDateTimeInicialization) < WRITE_FORCED_DELAY) then
    WriteLogsOnDisk;
end;

procedure TThreadWriteLogOnDisk.WriteLogsOnDisk;
begin
  try
    if (FPendingMessages.Size = 0) or (FLogFile = nil) then
      Exit;

    FRCMessages.Acquire;
    try
      FPendingMessages.Position := 0;
      FWrittingMessages.CopyFrom(FPendingMessages, FPendingMessages.Size);
      FPendingMessages.Clear;
    finally
      FRCMessages.Release;
    end;

    FWrittingMessages.Position := 0;
    FLogFile.Position := FLogFile.Size;
    FLogFile.WriteBuffer(FWrittingMessages.Memory^, FWrittingMessages.Size);
    FWrittingMessages.Clear;
  except
  //aqui apresenta exceção ao fechar o Cliente
  end;
end;


initialization
  RCHelperLog := TCriticalSection.Create;

  strTLogType[ltInfo]         := 'INFO';
  strTLogType[ltDebug]        := 'DEBUG';
  strTLogType[ltWarn]         := 'WARN';
  strTLogType[ltError]        := 'ERROR';
  strTLogType[ltDBError]      := 'DBERROR';
  strTLogType[ltDBWarn]       := 'DBWARN';
  strTLogType[ltWeB]          := 'WEB';
  strTLogType[ltUnitTest]     := 'UNITTEST';
  strTLogType[ltBenchMark]    := 'BENCHMARK';
  strTLogType[ltRelatorio]    := 'RELATORIO';
  strTLogType[ltDebugViagem]  := 'VIAGEM';
  strTLogType[ltLoad]         := 'LOAD';
  strTLogType[ltInt]          := 'INTEGRACAO';

finalization
  RCHelperLog.Free;

end.
