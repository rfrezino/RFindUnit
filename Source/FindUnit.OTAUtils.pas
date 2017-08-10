unit FindUnit.OTAUtils;

interface

uses
  FindUnit.Header,
  FindUnit.Utils,
  System.Generics.Collections, ToolsAPI, System.Classes, Winapi.ActiveX,
  Winapi.ShellAPI, Winapi.ShlObj;

function GetVolumeLabel(const DriveChar: string): string;
function BrowseURL(const URL: string): boolean;
function GetEditView(var ASourceEditor: IOTASourceEditor; var AEditView: IOTAEditView): boolean;
function EditorAsString(SourceEditor: IOTASourceEditor): string;
function ActiveSourceEditor: IOTASourceEditor;
function SourceEditor(Module: IOTAMOdule): IOTASourceEditor;

// GeExperts
function GxOtaGetCurrentModule: IOTAMOdule;
function GxOtaGetFileEditorForModule(Module: IOTAMOdule; Index: Integer): IOTAEditor;
function GxOtaGetSourceEditorFromModule(Module: IOTAMOdule; const FileName: string = ''): IOTASourceEditor;
function GetCurrentProject: IOTAProject;
function OtaGetCurrentSourceEditor: IOTASourceEditor;
function GetSelectedTextFromContext(Context: IOTAKeyContext): TStringPosition;
function GetErrorListFromActiveModule: TOTAErrors;
procedure GetLibraryPath(Paths: TStrings; PlatformName: string);

function GetAllFilesFromProjectGroup: TDictionary<string, TFileInfo>;

function GetWordAtCursor(DeltaCharPosition: Integer = 0): TStringPosition;

var
  PathUserDir: string;

implementation

uses
  System.SysUtils, System.IOUtils, System.Win.Registry, Winapi.Windows;

function SourceEditor(Module: IOTAMOdule): IOTASourceEditor;
var
  iFileCount: Integer;
  i: Integer;
begin
  Result := nil;
  if Module = nil then
    Exit;
  with Module do
  begin
    iFileCount := GetModuleFileCount;
    for i := 0 To iFileCount - 1 do
      if GetModuleFileEditor(i).QueryInterface(IOTASourceEditor, Result) = S_OK then
        Break;
  end;
end;

function ActiveSourceEditor: IOTASourceEditor;
var
  CM: IOTAMOdule;
begin
  Result := Nil;
  if BorlandIDEServices = nil then
    Exit;
  CM := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  Result := SourceEditor(CM);
end;

function GetErrorListFromActiveModule: TOTAErrors;
var
  ModuleErrors: IOTAModuleErrors;
begin
  ModuleErrors := GxOtaGetCurrentModule as IOTAModuleErrors;
  Result := ModuleErrors.GetErrors(ActiveSourceEditor.FileName);
end;

function GetAllFilesFromProjectGroup: TDictionary<string, TFileInfo>;
var
  ModServices: IOTAModuleServices;
  Module: IOTAMOdule;
  ProjectGroup: IOTAProjectGroup;
  iMod: Integer;
  FileDesc: string;
  iProj: Integer;
  CurProject: IOTAProject;
  iFile: Integer;
  FileInfo: TFileInfo;
begin
  Result := TDictionary<string, TFileInfo>.Create;

  ModServices := BorlandIDEServices as IOTAModuleServices;
  if ModServices = nil then
    Exit;

  if ModServices.ModuleCount = 0 then
    Exit;
  
  for iMod := 0 to ModServices.ModuleCount - 1 do
  begin
    Module := ModServices.Modules[iMod];
    if Supports(Module, IOTAProjectGroup, ProjectGroup) then
    begin
      for iProj := 0 to ProjectGroup.ProjectCount -1 do
      begin
        CurProject := ProjectGroup.Projects[iProj];
        for iFile := 0 to CurProject.GetModuleCount -1 do
        begin
          FileDesc := CurProject.GetModule(iFile).FileName;
          if FileDesc = '' then
            Continue;

          FileInfo.Path := FileDesc;
          if FileExists(FileDesc) then
            FileInfo.LastAccess := System.IOUtils.TFile.GetLastWriteTime(FileDesc)
          else
            FileInfo.LastAccess := 0;
          Result.AddOrSetValue(FileInfo.Path, FileInfo);
        end;
      end;
    end;
  end;
end;

procedure GetLibraryPath(Paths: TStrings; PlatformName: string);
var
  Svcs: IOTAServices;
  Options: IOTAEnvironmentOptions;
  Text: string;
  List: TStrings;
  ValueCompiler: string;
  RegRead: TRegistry;
begin
  Svcs := BorlandIDEServices as IOTAServices;
  if not Assigned(Svcs) then Exit;
  Options := Svcs.GetEnvironmentOptions;
  if not Assigned(Options) then Exit;

  ValueCompiler := Svcs.GetBaseRegistryKey;

  RegRead := TRegistry.Create;
  List := TStringList.Create;
  try
    if PlatformName = '' then
      Text := Options.GetOptionValue('LibraryPath')
    else
    begin
      RegRead.RootKey := HKEY_CURRENT_USER;
      RegRead.OpenKey(ValueCompiler + '\Library\' + PlatformName, False);
      Text := RegRead.GetDataAsString('Search Path');
    end;

    List.Text := StringReplace(Text, ';', #13#10, [rfReplaceAll]);
    Paths.AddStrings(List);

    if PlatformName = '' then
      Text := Options.GetOptionValue('BrowsingPath')
    else
    begin
      RegRead.RootKey := HKEY_CURRENT_USER;
      RegRead.OpenKey(ValueCompiler + '\Library\' + PlatformName, False);
      Text := RegRead.GetDataAsString('Browsing Path');
    end;

    List.Text := StringReplace(Text, ';', #13#10, [rfReplaceAll]);
    Paths.AddStrings(List);
  finally
    RegRead.Free;
    List.Free;
  end;
end;

function GetWordAtCursor(DeltaCharPosition: Integer): TStringPosition;
const
  strIdentChars = ['a' .. 'z', 'A' .. 'Z', '_', '0' .. '9'];
var
  SourceEditor: IOTASourceEditor;
  EditPos: TOTAEditPos;
  iPosition: Integer;
  Content: TStringList;
  ContentTxt: string;
begin
  try
    ContentTxt := '';
    SourceEditor := ActiveSourceEditor;
    EditPos := SourceEditor.EditViews[0].CursorPos;
    Content := TStringList.Create;
    try
      Content.Text := EditorAsString(SourceEditor);
      ContentTxt := Content[Pred(EditPos.Line)];
      iPosition := EditPos.Col + DeltaCharPosition;
      if (iPosition > 0) And (Length(ContentTxt) >= iPosition) and CharInSet(ContentTxt[iPosition], strIdentChars) then
      begin
        while (iPosition > 1) And (CharInSet(ContentTxt[Pred(iPosition)], strIdentChars)) do
          Dec(iPosition);
        Delete(ContentTxt, 1, Pred(iPosition));
        iPosition := 1;
        while CharInSet(ContentTxt[iPosition], strIdentChars) do
          Inc(iPosition);
        Delete(ContentTxt, iPosition, Length(ContentTxt) - iPosition + 1);
        if CharInSet(ContentTxt[1], ['0' .. '9']) then
          ContentTxt := '';
      end
      else
        ContentTxt := '';

      Result.Value := ContentTxt;
      Result.Line := EditPos.Line;
    finally
      Content.Free;
    end;
  except
    on E: exception do
    begin
      Result.Value := '';
      Result.Line := -1;
    end;
  end;

  if (DeltaCharPosition = 0) and (Result.Value = '') then
    Result := GetWordAtCursor(-1);
end;

function GetSelectedTextFromContext(Context: IOTAKeyContext): TStringPosition;
var
  Editor: IOTAEditBuffer;
  CurSourceEditor: IOTASourceEditor;
begin
  CurSourceEditor := ActiveSourceEditor;
  Result.Line := CurSourceEditor.EditViews[0].CursorPos.Line;
  Result.Value := '';
  if Context = nil then
    Exit;

  Editor := Context.EditBuffer;
  if Editor = nil then
    Exit;

  Result.Value := Trim(Editor.EditBlock.Text);
end;

function GetCurrentProject: IOTAProject;
var
  ModServices: IOTAModuleServices;
  Module: IOTAMOdule;
  Project: IOTAProject;
  ProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;
  ModServices := BorlandIDEServices as IOTAModuleServices;
  for i := 0 to ModServices.ModuleCount - 1 do
  begin
    Module := ModServices.Modules[i];
    if Supports(Module, IOTAProjectGroup, ProjectGroup) then
    begin
      Result := ProjectGroup.ActiveProject;
      Exit;
    end
    else if Supports(Module, IOTAProject, Project) then
    begin // In the case of unbound packages, return the 1st
      if Result = nil then
        Result := Project;
    end;
  end;
end;

// Credits to GXExperts
function GxOtaGetCurrentModule: IOTAMOdule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));
  Result := ModuleServices.CurrentModule;
end;

// Credits to GXExperts
function GxOtaGetFileEditorForModule(Module: IOTAMOdule; Index: Integer): IOTAEditor;
begin
  Assert(Assigned(Module));
  Result := Module.GetModuleFileEditor(Index);
end;

// Credits to GXExperts
function GxOtaGetSourceEditorFromModule(Module: IOTAMOdule; const FileName: string = ''): IOTASourceEditor;
var
  i: Integer;
  IEditor: IOTAEditor;
  ISourceEditor: IOTASourceEditor;
begin
  Result := nil;
  if not Assigned(Module) then
    Exit;

  for i := 0 to Module.GetModuleFileCount - 1 do
  begin
    IEditor := GxOtaGetFileEditorForModule(Module, i);

    if Supports(IEditor, IOTASourceEditor, ISourceEditor) then
    begin
      if Assigned(ISourceEditor) then
      begin
        if (FileName = '') or SameFileName(ISourceEditor.FileName, FileName) then
        begin
          Result := ISourceEditor;
          Break;
        end;
      end;
    end;
  end;
end;

function OtaGetCurrentSourceEditor: IOTASourceEditor;
var
  LEditorServices: IOTAEditorServices;
  LEditBuffer: IOTAEditBuffer;
begin
  Result := nil;

  LEditorServices := (BorlandIDEServices as IOTAEditorServices);
  LEditBuffer := LEditorServices.TopBuffer;

  if Assigned(LEditBuffer) and (LEditBuffer.FileName <> '') then
    Result := GxOtaGetSourceEditorFromModule(GxOtaGetCurrentModule, LEditBuffer.FileName);

  if Result = nil then
    Result := GxOtaGetSourceEditorFromModule(GxOtaGetCurrentModule);

end;

function GetEditView(var ASourceEditor: IOTASourceEditor; var AEditView: IOTAEditView): boolean;
begin
  Result := False;
  ASourceEditor := OtaGetCurrentSourceEditor;
  if not Assigned(ASourceEditor) then
    Exit;
  AEditView := ASourceEditor.GetEditView(0);
  Result := Assigned(AEditView);
end;

type
  TBrowserInformation = record
    Name: String;
    Path: String;
    Version: String;
  end;

function GetDefaultBrowser: TBrowserInformation;
var
  tmp: PChar;
  res: PChar;

begin
  tmp := StrAlloc(255);
  res := StrAlloc(255);
  try
    GetTempPath(255, tmp);
    FileCreate(tmp + 'htmpl.htm');
    FindExecutable('htmpl.htm', tmp, res);
    Result.Name := ExtractFileName(res);
    Result.Path := ExtractFilePath(res);
    System.SysUtils.DeleteFile(tmp + 'htmpl.htm');
  finally
    StrDispose(tmp);
    StrDispose(res);
  end;
end;

function EditorAsString(SourceEditor: IOTASourceEditor): string;
const
  iBufferSize: Integer = 1024;
Var
  Reader: IOTAEditReader;
  iRead: Integer;
  iPosition: Integer;
  strBuffer: AnsiString;
begin
  Result := '';

  Reader := SourceEditor.CreateReader;
  try
    iPosition := 0;
    repeat
      SetLength(strBuffer, iBufferSize);
      iRead := Reader.GetText(iPosition, PAnsiChar(strBuffer), iBufferSize);
      SetLength(strBuffer, iRead);
      Result := Result + String(strBuffer);
      Inc(iPosition, iRead);
    until iRead < iBufferSize;
  finally
    Reader := Nil;
  end;
end;

function LongPathName(const ShortPathName: string): string;
var
  PIDL: PItemIDList;
  Desktop: IShellFolder;
  WidePathName: WideString;
  AnsiPathName: AnsiString;
begin
  Result := ShortPathName;
  if Succeeded(SHGetDesktopFolder(Desktop)) then
  begin
    WidePathName := ShortPathName;
    if Succeeded(Desktop.ParseDisplayName(0, nil, PWideChar(WidePathName), ULONG(nil^), PIDL, ULONG(nil^))) then

      try
        SetLength(AnsiPathName, MAX_PATH);
        SHGetPathFromIDList(PIDL, PChar(AnsiPathName));
        Result := PChar(AnsiPathName);

      finally
        CoTaskMemFree(PIDL);
      end;
  end;
end;

function GetEnvVarValue(const AVarName: string): string;
var
  LBufSize: Integer;
begin
  LBufSize := GetEnvironmentVariable(PChar(AVarName), nil, 0);
  if LBufSize > 0 then
  begin
    SetLength(Result, LBufSize - 1);
    GetEnvironmentVariable(PChar(AVarName), PChar(Result), LBufSize);
  end
  else
    Result := '';
end;

function GetVolumeLabel(const DriveChar: string): string;
var
  NotUsed: DWORD;
  VolumeFlags: DWORD;
  VolumeInfo: array [0 .. MAX_PATH] of Char;
  VolumeSerialNumber: DWORD;
  Buf: array [0 .. MAX_PATH] of Char;
begin
  GetVolumeInformation(PChar(DriveChar), Buf, SizeOf(VolumeInfo), @VolumeSerialNumber, NotUsed, VolumeFlags, nil, 0);

  SetString(Result, Buf, StrLen(Buf)); { Set return result }
  Result := AnsiUpperCase(Result);
end;

function BrowseURL(const URL: string): boolean;
var
  LBrowserInformation: TBrowserInformation;
begin
  Result := False;
  LBrowserInformation := GetDefaultBrowser;
  Result := ShellExecute(0, 'open', PChar(LBrowserInformation.Path + LBrowserInformation.Name), PChar(URL), nil,
    SW_SHOW) > 32;
end;

initialization

PathUserDir := GetEnvVarValue('APPDATA') + '\RfUtils';
CreateDir(PathUserDir);

end.
