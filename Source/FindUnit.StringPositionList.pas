unit FindUnit.StringPositionList;

interface

uses
  Classes,

  FindUnit.Header,

  Generics.Collections,
  System.SysUtils;

type
  TStringPositionList = class(TList<TStringPosition>)
  private
    FDuplicates: TDuplicates;

    function IsDuplicated(Item: TStringPosition): Boolean;
  public
    function Add(const Value: TStringPosition): Integer;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

implementation

{ TStringPositionList }

function TStringPositionList.Add(const Value: TStringPosition): Integer;
begin
  Result := 0;
  case Duplicates of
    dupIgnore:
    begin
      if not IsDuplicated(Value) then
        Result := inherited Add(Value);
    end;
    dupAccept: Result := inherited Add(Value);
    dupError:
    begin
      if IsDuplicated(Value) then
        raise Exception.Create('Duplicated item');
      Result := inherited Add(Value);
    end;
  end;
end;

function TStringPositionList.IsDuplicated(Item: TStringPosition): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count -1 do
    if Item.Value = Items[i].Value then
    begin
      Result := True;
      Exit;
    end;
end;

end.
