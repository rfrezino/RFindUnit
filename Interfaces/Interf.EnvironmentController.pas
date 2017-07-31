unit Interf.EnvironmentController;

interface

uses
  System.Classes;

type
  IRFUEnvironmentController = interface
    ['{A53012B2-303C-4E12-8891-B8B18DAB4487}']
    function GetFullMatch(const SearchString: string): TStringList;
    function AreDependenciasReady: Boolean;
    procedure ForceRunDependencies;
  end;

implementation

end.
