{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{ Copyright(c) 2012-2015 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit AV_Case1;

interface

uses
  Vcl.Forms, System.Rtti,
  System.SysUtils, System.Classes, System.Generics.Collections, Vcl.ImgList;

type
  IOTADataExplorerItem = Interface;
  IOTADataExplorerCommand = Interface;

  TScope = record
  private
    FObject: TObject;
    FName: string;
  public
    constructor Create(const AName: string; AObject: TObject);
    property Obj: TObject read FObject;
    property Name: string read FName;
  end;
  TScopesArray = TArray<TScope>;

  TDEValueRec = record
    Value: string;
    ValueObj: TObject;
    constructor Create(AValueStr: string); overload;
    constructor Create(AValueObj: TObject); overload;
  end;

  TDEEnumerableWrapper = class(TInterfacedObject, IEnumerator)
  private
    FContext: TRttiContext;
    FEnumerator: TObject;
    function GetCurrent: TObject;
  public
    constructor Create(AEnumerator: TObject);
    function MoveNext: Boolean;
    property Current: TObject read GetCurrent;
    procedure Reset;
  end;

  TDEItemsArray = TArray<IOTADataExplorerItem>;
  TDECommandsArray = TArray<IOTADataExplorerCommand>;
  IOTADataExplorerItem = Interface
    ['{3EDCE923-F52C-4E2D-ADBB-4162D7593D93}']
    function IsRoot: Boolean;
    function GetEnabled(AScopesArray: TScopesArray): Boolean;
    function GetEnabledValue: TDEValueRec;
    function GetNameValue: TDEValueRec;
    function GetIdentityValue: TDEValueRec;
    function GetCommandsArray: TDECommandsArray;
    function GetItemsArray: TDEItemsArray;
    function GetItemCount: Integer;
    function GetScopesArray: TScopesArray;
    function EvaluateValueRec(const AValueRec: TDEValueRec; AScopes: TScopesArray): TValue;
    property CommandsArray: TDECommandsArray read GetCommandsArray;
    property ItemsArray: TDEItemsArray read GetItemsArray;
    property ItemCount: Integer read GetItemCount;
    property NameValue: TDEValueRec read GetNameValue;
    property IdentityValue: TDEValueRec read GetIdentityValue;
  end;

  IOTADataExplorerRootItem = Interface
    ['{59F144E3-9AFC-4790-BCDF-45878B6A373D}']
    function IsRoot: Boolean;
  End;

  IOTADataExplorerCommand = Interface
    ['{550248E8-60B5-4E63-9515-FD8A4CA563AE}']
    procedure Execute(AItem: IOTADataExplorerItem; AScopes: TScopesArray);
    procedure Update(AItem: IOTADataExplorerItem; AScopes: TScopesArray);
    function GetCaption: string;
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    property Caption: string read GetCaption;
    property Enabled: Boolean read GetEnabled;
    property Visible: Boolean read GetVisible;
  End;

  IOTADataExplorerComponentStream = Interface
    ['{C5CFDAFD-C665-4E80-B5ED-AE0AC73E3590}']
    function GetFormat: Word;
    function GetStream(AItem: IOTADataExplorerItem; AScopes: TScopesArray; AStream: TStream): Boolean;
    property Format: Word read GetFormat;
  End;
  TDEComponentStreamArray = TArray<IOTADataExplorerComponentStream>;

  IOTADataExplorerStaticNodeItem = Interface(IOTADataExplorerItem)
    ['{31AC3616-D72F-4D51-B435-5CC71E6481E9}']
    function GetImageIndex: Integer;
    function GetDisplayNameValue: TDEValueRec;
    function GetImageIndexValue: TDEValueRec;
    function GetObjectValue: TDEValueRec;
    function GetComponentStreamsArray: TDEComponentStreamArray;
    property ImageIndex: Integer read GetImageIndex;
    property DisplayNameValue: TDEValueRec read GetDisplayNameValue;
    property ImageIndexValue: TDEValueRec read GetImageIndexValue;
    property ObjectValue: TDEValueRec read GetObjectValue;
    property ComponentStreamsArray: TDEComponentStreamArray read GetComponentStreamsArray;
  End;

  IOTADataExplorerNodeCollectionItem = Interface(IOTADataExplorerItem)
    ['{50ED24EB-2FF9-44A5-98A5-CEEFBD9D00B9}']
    function GetEnumeratorValue: TDEValueRec;
    function GetEnumeratorScopeName: string;
    function GetRefreshValue: TDEValueRec;
    property EnumeratorValue: TDEValueRec read GetEnumeratorValue;
    property RefreshValue: TDEValueRec read GetRefreshValue;
    property EnumeratorScopeName: string read GetEnumeratorScopeName;
  End;

  IDataExplorerFrame = Interface
    ['{676E80A1-CAB4-4E27-8CB2-75F6A135DAA1}']
//    function GetEditState: TEditState;
//    function EditAction(Action: TEditAction): Boolean;
  End;

  TFrameClass = class of TFrame;
  IOTADataExplorerService = Interface
    ['{51DC9F09-3DF5-4FB2-8DC1-A9BA137FA6BD}']
    procedure DeleteGroup(const AGroupNum: Integer);
    procedure RegisterRootNode(ARootItem: IOTADataExplorerItem);
    procedure UnregisterRootNode(ARootItem: IOTADataExplorerItem);
    function GetRootNodes: TArray<IOTADataExplorerItem>;
    procedure RefreshNode(AIdentity: TObject; AItem: IOTADataExplorerItem; AChildren: Boolean);
    procedure DeleteNode(AIdentity: TObject);
    procedure SelectNewChildNode(AParentIdentity: TObject; AItem: IOTADataExplorerItem; const AName: string);
    procedure RefreshChildNode(AParentIdentity: TObject; AItem: IOTADataExplorerItem; const AName: string; AChildren: Boolean);
    function AddImages(const Images: TCustomImageList): Integer;

    procedure AddTab(const ATabCaption: string; AFrameClass: TFrameClass; AFrameCreatedProc: TProc<TFrame>);
    procedure RenameTab(const AOldCaption, ANewCaption: string);
    procedure RemoveTab(const ATabCaption: string);
    function HasTab(const ATabCaption: string): Boolean;
    procedure ActivateTab(const ATabCaption: string);
    procedure ObjInspectorSelect(const AObj: TPersistent);
  End;

  IDataExplorerService = IOTADataExplorerService deprecated 'Use IOTADataExplorerService';

var
  { Offsets into the image list
  for these particular "standard" images. }
  piClosedFolder, piOpenFolder, piConnection, piDatabase,
  piFields, piIndexes, piPackages, piStoredProc, piSynonyms,
  piTables, piViews, piNoConnection, piServerMethods: Integer;

implementation

{ TScope }

constructor TScope.Create(const AName: string; AObject: TObject);
begin
  inherited;
  FName := AName;
  FObject := AObject;
end;

{ TDEEnumerableWrapper }

constructor TDEEnumerableWrapper.Create(AEnumerator: TObject);
begin
  FEnumerator := AEnumerator;
end;

procedure TDEEnumerableWrapper.Reset;
begin
  assert(False);
end;

function TDEEnumerableWrapper.GetCurrent: TObject;
var
  LProperty: TRttiProperty;
  LValue: TValue;
  LRttiType: TRttiType;
  LEnumerator: TObject;
begin
  Result := nil;
  try
    LEnumerator := FEnumerator;
    if LEnumerator = nil then
      Exit(nil);
    LRttiType := FContext.GetType(LEnumerator.ClassType);
    LProperty := LRttiType.GetProperty('Current');
    if Assigned(LProperty) then
    begin
      LValue := LProperty.GetValue(LEnumerator);
      if LValue.IsType<TObject>  then
        Result := LValue.AsObject
      else
        Result := nil;
    end
    else
      Result := nil;
  finally
    Assert(Result <> nil);
  end;
end;

function TDEEnumerableWrapper.MoveNext: Boolean;
var
  LMethod: TRttiMethod;
  LArgs: TArray<TValue>;
  LValue: TValue;
  LRttiType: TRttiType;
  LEnumerator: TObject;
begin
  LEnumerator := FEnumerator;
  if LEnumerator = nil then
    Exit(False);
  LRttiType := FContext.GetType(LEnumerator.ClassType);
  LMethod := LRttiType.GetMethod('MoveNext');
  if Assigned(LMethod) then
  begin
    LValue := LMethod.Invoke(LEnumerator, LArgs);
    if LValue.IsType<Boolean>  then
      Result := LValue.AsBoolean
    else
      Result := False;
  end
  else
    Result := False;
end;

{ TDEValueRec }

constructor TDEValueRec.Create(AValueStr: string);
begin
  Value := AValueStr;
  ValueObj := nil;
end;

constructor TDEValueRec.Create(AValueObj: TObject);
begin
  Value := '';
  ValueObj := AValueObj;
end;

end.

