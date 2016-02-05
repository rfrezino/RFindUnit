unit FindUnit.OTAUtils;

interface

uses
  ToolsAPI;

  function GetVolumeLabel(const DriveChar: string): string;
  function BrowseURL(const URL: string) : boolean;
  function GetEditView(var ASourceEditor: IOTASourceEditor; var AEditView: IOTAEditView): boolean;
  function EditorAsString(SourceEditor : IOTASourceEditor): string;

  //GeExperts
  function GxOtaGetCurrentModule: IOTAModule;
  function GxOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;
  function GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;
  function GetCurrentProject: IOTAProject;
  function OtaGetCurrentSourceEditor: IOTASourceEditor;
  function GetSelectedTextFromContext(Context: IOTAKeyContext): string;

var
  PathUserDir: string;

implementation

uses
  Windows, ShellAPI, ShlObj, ActiveX, SysUtils;

function GetSelectedTextFromContext(Context: IOTAKeyContext): string;
var
  Editor: IOTAEditBuffer;
  EdtPosition: IOTAEditPosition;
begin
  Result := '';
  if Context = nil then
    Exit;

  Editor := Context.EditBuffer;
  if Editor= nil then
    Exit;

  EdtPosition := Editor.EditPosition;
  Result := Trim(Editor.EditBlock.Text);
end;

function GetCurrentProject: IOTAProject;  
var  
  ModServices: IOTAModuleServices;  
  Module: IOTAModule;  
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

//Credits to GXExperts
function GxOtaGetCurrentModule: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));
  Result := ModuleServices.CurrentModule;
end;

//Credits to GXExperts
function GxOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;
begin
  Assert(Assigned(Module));
  Result := Module.GetModuleFileEditor(Index);
end;

//Credits to GXExperts
function GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;
var
  i: Integer;
  IEditor: IOTAEditor;
  ISourceEditor: IOTASourceEditor;
begin
  Result := nil;
  if not Assigned(Module) then
    Exit;

  for i := 0 to Module.GetModuleFileCount-1 do
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
  LFileName: string;
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
    Name     : String;
    Path     : String;
    Version  : String;
  end;

function GetDefaultBrowser : TBrowserInformation;
var
  tmp : PChar;
  res : PChar;

begin
  tmp := StrAlloc(255);
  res := StrAlloc(255);
  try
    GetTempPath(255,tmp);
    FileCreate(tmp+'htmpl.htm');
    FindExecutable('htmpl.htm',tmp,Res);
    Result.Name := ExtractFileName(res);
    Result.Path := ExtractFilePath(res);
    SysUtils.DeleteFile(tmp+'htmpl.htm');
  finally
    StrDispose(tmp);
    StrDispose(res);
  end;
end;

function EditorAsString(SourceEditor : IOTASourceEditor): string;
const
  iBufferSize : Integer = 1024;
Var
  Reader : IOTAEditReader;
  iRead : Integer;
  iPosition : Integer;
  strBuffer : AnsiString;
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
    if Succeeded(Desktop.ParseDisplayName(0, nil, PWideChar(WidePathName),
      ULONG(nil^), PIDL, ULONG(nil^))) then

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
    GetEnvironmentVariable(PChar(AVarName),PChar(Result), LBufSize);
  end
  else
    Result := '';
end;

function GetVolumeLabel(const DriveChar: string): string;
var
  NotUsed:     DWORD;
  VolumeFlags: DWORD;
  VolumeInfo:  array[0..MAX_PATH] of Char;
  VolumeSerialNumber: DWORD;
  Buf: array [0..MAX_PATH] of Char;
begin
    GetVolumeInformation(PChar(DriveChar),
    Buf, SizeOf(VolumeInfo), @VolumeSerialNumber, NotUsed,
    VolumeFlags, nil, 0);
 
    SetString(Result, Buf, StrLen(Buf));   { Set return result }
    Result:=AnsiUpperCase(Result);
end;

function BrowseURL(const URL: string) : boolean;
var
  LBrowserInformation: TBrowserInformation;
begin
  Result := False;
  LBrowserInformation := GetDefaultBrowser;
  Result := ShellExecute(0, 'open', PChar(LBrowserInformation.Path + LBrowserInformation.Name), PChar(URL), nil, SW_SHOW) > 32;
end;

initialization
  PathUserDir := GetEnvVarValue('APPDATA') + '\RfUtils';
  CreateDir(PathUserDir);

end.
