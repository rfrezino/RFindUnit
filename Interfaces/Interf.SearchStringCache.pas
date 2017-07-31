unit Interf.SearchStringCache;

interface

uses
  System.Classes;

type
  ISearchStringCache = interface
    ['{4819D4F0-383A-401A-B2A4-80286ABA5D1D}']
    function GetMatch(const SearchString: string; out FoundList: TStringList): Boolean;
    function AddItemOnCache(const SearchString: string; var FoundList: TStringList): Boolean;
  end;

implementation

end.
