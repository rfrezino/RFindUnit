unit FindUnit.Header;

interface

const
  VERSION: array[0..2] of Word = (1,2,0);//(MAJOR, RELEASE, BUILD)

type
  TListType = (ltClasses = 0,
    ltProcedures = 1,
    ltFunctions = 2,
    ltContants = 3,
    ltVariables = 4,
    ltClassFunctions = 5,
    ltClassProcedures = 6,
    ltEnumeratores = 7,
    ltInterfaces = 8,
    ltReferences = 9,
    ltRecords = 10);

  TStringPosition = record
    Value: string;
    Line: Integer;
  end;

  TFilePath = string;

var
  strListTypeDescription: array[TListType] of string;
  VERSION_STR: string;
  vSystemRunning: Boolean;

const
   MAX_RETURN_ITEMS = 200;
   AUTO_IMPORT_FILE = 'memoryconfig.ini';

implementation

uses
  System.SysUtils;

procedure LoadConts;
begin
  strListTypeDescription[ltClasses] := '';
  strListTypeDescription[ltProcedures] := ' - Procedure';
  strListTypeDescription[ltFunctions] := ' - Function';
  strListTypeDescription[ltContants] := ' - Constant';
  strListTypeDescription[ltVariables] := ' - Variable';
  strListTypeDescription[ltClassFunctions] := ' - Class Function';
  strListTypeDescription[ltClassProcedures] := ' - Class Procedure';

  VERSION_STR := Format('%d.%d.%d', [VERSION[0], VERSION[1], VERSION[2]]);
end;

initialization
  LoadConts;
  vSystemRunning := True;

finalization
  vSystemRunning := False;


end.
