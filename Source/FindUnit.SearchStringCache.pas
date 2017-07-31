unit FindUnit.SearchStringCache;

interface

uses
  Classes,

  Interf.SearchStringCache,

  System.Generics.Collections,
  System.SyncObjs;

type
  TSearchStringCache = class(TInterfacedObject, ISearchStringCache)
  private
    FFullMatchCache: TObjectDictionary<string, TStringList>;
    FRcFullCache: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    function GetMatch(const SearchString: string; out FoundList: TStringList): Boolean;
    function AddItemOnCache(const SearchString: string; var FoundList: TStringList): Boolean;
  end;

implementation

{ TSearchStringCache }

function TSearchStringCache.AddItemOnCache(const SearchString: string; var FoundList: TStringList): Boolean;
begin
  FRcFullCache.Acquire;
  try
    FFullMatchCache.AddOrSetValue(SearchString, FoundList);
  finally
    FRcFullCache.Release;
  end;
end;

constructor TSearchStringCache.Create;
begin
  FFullMatchCache := TObjectDictionary<string, TStringList>.Create([doOwnsValues]);
  FRcFullCache := TCriticalSection.Create;
end;

destructor TSearchStringCache.Destroy;
begin
  FFullMatchCache.Free;
  FRcFullCache.Free;
  inherited;
end;

function TSearchStringCache.GetMatch(const SearchString: string; out FoundList: TStringList): Boolean;
var
  Local: TStringList;
begin
  FRcFullCache := TCriticalSection.Create;
  try
    Result := FFullMatchCache.TryGetValue(SearchString, Local);
    if Result then
    begin
      FoundList := TStringList.Create;
      FoundList.Text := Local.Text;
    end;
  finally
    FRcFullCache.Release;
  end;
end;

end.
