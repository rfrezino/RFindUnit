unit FindUnit.DelphiVlcWrapper;

interface

uses
	Windows;

type
  TDelphiVLCWrapper = class(TObject)
  private
    class procedure FindEditorHandle;
  public
    class function GetEditorRect: TRect;
  end;

implementation

uses
  Messages;

var
  FFound: Boolean;
  FEditHandler: Cardinal;

{ TDelphiVLCWrapper }

function EnumChildProc(AHandle: THandle; Params: integer): BOOL; stdcall;
var
  Buffer: array[0..255] of Char;
  Caption: array[0..255] of Char;
  Item: string;
begin
  if FFound then
    Exit(False);

  Result := True;
  GetClassName(AHandle, Buffer, SizeOf(Buffer)-1);
  SendMessage(AHandle, WM_GETTEXT, 256, Integer(@Caption));

  Item := Buffer;

  if Item = 'TEditControl' then
  begin
    FEditHandler := AHandle;
    FFound := True;
    Exit;
  end;

  EnumChildWindows(AHandle, @EnumChildProc, 0)
end;

class procedure TDelphiVLCWrapper.FindEditorHandle;
var
  DelphiHand: Cardinal;
begin
  FFound := False;
  DelphiHand := FindWindow('TAppBuilder', nil);
  EnumChildWindows(DelphiHand, @EnumChildProc, 0);
  FFound := True;
end;

class function TDelphiVLCWrapper.GetEditorRect: TRect;
begin
  if not FFound then
    FindEditorHandle;
  if not Windows.GetWindowRect(FEditHandler, Result) then
  begin
    Result.Left := 20;
    Result.Top := 20;
    Result.Right := 20;
  end;
end;

end.

