unit BaseUnusedUnitsSetAllUse;

interface

uses
  System.StrUtils,
  System.Generics.Collections,
  System.SysUtils,
  Types;

type
  TClassUnused = class
    function TestUsesOfNonClassMethod: string;
    procedure TestUsesOfCollections;
    function Rect: TRect;
  end;

implementation

{ TClassUnused }

function TClassUnused.Rect: TRect;
begin

end;

procedure TClassUnused.TestUsesOfCollections;
var
  List: TList<Integer>;
  Dir: TDictionary<string, string>;
begin
  List := TList<Integer>.Create;
  List.Add(1);
  List.Free;

  Dir := TDictionary<string, string>.Create;
end;

function TClassUnused.TestUsesOfNonClassMethod: string;
begin
  Result := ReverseString('TestItem %d');

  Result := Format(Result, [1]);
end;

end.
