unit FindUnit.Header;

interface

type
  TListType = (ltClasses = 0,
    ltProcedures = 1,
    ltFunctions = 2,
    ltContants = 3,
    ltVariables = 4);

var
  strListTypeDescription: array[TListType] of string;


const
   MAX_RETURN_ITEMS = 200;

implementation

procedure LoadConts;
begin
  strListTypeDescription[ltClasses] := '';
  strListTypeDescription[ltProcedures] := ' - Procedure';
  strListTypeDescription[ltFunctions] := ' - Function';
  strListTypeDescription[ltContants] := ' - Constant';
  strListTypeDescription[ltVariables] := ' - Variable';

end;

initialization
  LoadConts;

end.
