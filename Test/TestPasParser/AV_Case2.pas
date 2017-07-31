{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013-2015 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
unit AV_Case2;
/// <summary>
/// REST.JsonReflect originates from Data.DBXJSONReflect, but is more light weight
/// and most importanly dos not depend on meta data injected in JSON objects.<br/>
/// Its purpose is to "reflect" Json properties into TObject ones and vise versa.<br/>
/// All JSON objects created or processed here are treated in a "plain" way.
/// The implementation found here is still "rough" from a design pov, but will
/// improve over time.<br/>
///  Important: RTTI is heavily involved thus may not be disabled.
/// Currently it is not recommended to use any of this unit directly, as
/// its interface may change over time. Use REST.TJson instead (or Data.DBXJSONReflect).
// The interfaces in REST.Json can be considered stable.
/// </summary>


interface

uses
  System.SysUtils,
  System.StrUtils,
  System.JSON,
  System.Generics.Collections,
  System.TypInfo,
  System.Rtti,
  System.Classes,
  REST.Json.Types;

type
  /// <summary> Exception thrown when conversion or reversion process cannot complete
  /// </summary>
  /// <remarks>
  /// Most of these exception are thrown by the internal converter/reverter
  /// code. User code can trigger then when assumed pre-conditions are not met, such
  /// as a field converter transforms the value into a string but the reverter
  /// is defined for an array.
  /// </remarks>
  EConversionError = class(Exception);

  /// <summary>
  /// Base converter class
  /// </summary>
  /// <remarks>
  /// Any serializer needs to inherit from this class. It provides the events
  /// API used by a marshaller class to serialize an user object.
  ///
  /// The serialization process is considered to be successfull if IsConsistent
  /// returns true.
  /// </remarks>
  TConverter<TSerial> = class abstract
  protected
    /// <summary> Returns the serialized object </summary>
    /// <returns> Serialized object </returns>
    function GetSerializedData: TSerial; virtual; abstract;
  public
    constructor Create; virtual;
    /// <summary> Resets the instance state </summary>
    /// <remarks>
    /// Clears all residual data so the instance can be reused for a new
    /// conversion.
    /// </remarks>
    procedure Clear; virtual; abstract;
    /// <summary> Event called for pre-visited instance </summary>
    /// <remarks> It is the marshal class that provides the functionality of
    /// detecting circuits in the serialization process. The un-marshal code
    /// will use the id to restore the actual pointer </remarks>
    /// <param name="TypeName"> User object type name </param>
    /// <param name="id"> The pre-visited instance id </param>
    procedure OnRefType(TypeName: string; id: Integer); virtual; abstract;
    /// <summary> Event called for each new object instance </summary>
    /// <param name="TypeName"> user object type name </param>
    procedure OnTypeStart(TypeName: string); virtual; abstract;
    /// <summary> Event called when a new user object processing ends </summary>
    /// <remarks> All fields are processed at this time </remarks>
    /// <param name="TypeName"> user object type name, matching a previous
    /// OnTypeStart event</param>
    procedure OnTypeEnd(TypeName: string); virtual; abstract;
    /// <summary> Event called for each field of an object </summary>
    /// <remarks> The field value is provided by one of the events OnString,
    /// OnNumber, OnBoolean, OnNull, OnListStart.</remarks>
    /// <param name="Field"> field name </param>
    procedure OnFieldStart(Field: TRttiField); virtual; abstract;
    /// <summary> Event called for each field immediately after its value was
    /// processed. </summary>
    /// <param name="Field">Field name matching a previous ObFieldStart</param>
    procedure OnFieldEnd(Field: TRttiField); virtual; abstract;
    /// <summary> Event called when a field value is a list of values </summary>
    /// <remarks> This event may be followed by a number of OnString, OnNumber
    /// OnBoolean, OnNull or even imbricated OnListStart/End events </remarks>
    procedure OnListStart; virtual; abstract;
    /// <summary> Event marking the processing of the last value of a list</summary>
    /// <remarks> The event matches a previous OnListStart event</remarks>
    procedure OnListEnd; virtual; abstract;
    /// <summary> String value event </summary>
    /// <remarks> The event was precedeed by a OnFieldStart (eventually an
    /// OnListStart) open event.</remarks>
    /// <param name="Data">Field or array element value as a string</param>
    procedure OnString(Data: string); virtual; abstract;
    /// <summary> Number value event </summary>
    /// <remarks> The event was precedeed by a OnFieldStart (eventually an
    /// OnListStart) open event.</remarks>
    /// <param name="Data">Field or array element value as a number</param>
    procedure OnNumber(Data: string); virtual; abstract;
    /// <summary> Boolean value event </summary>
    /// <remarks> The event was precedeed by a OnFieldStart (eventually an
    /// OnListStart) open event. Boolean are treated a special case of
    /// enumerations. </remarks>
    /// <param name="Data">Field or array element value as a boolean</param>
    procedure OnBoolean(Data: Boolean); virtual; abstract;
    /// <summary> Nil value event </summary>
    /// <remarks> The event was precedeed by a OnFieldStart (eventually an
    /// OnListStart) open event.</remarks>
    procedure OnNull; virtual; abstract;
    /// <summary> IsConsistent marks the successfull object serialization </summary>
    /// <remarks> By returning true it ensures that no open event exists and the
    /// serialized value can be used to restore the original value. </remarks>
    /// <returns>true if the process will return a consistent serialized object</returns>
    function IsConsistent: Boolean; virtual; abstract;
    /// <summary> Bypass for value events </summary>
    /// <remarks> Sets the expected serialized value directly </remarks>
    /// <param name="Data"> field serialized value </param>
    procedure SetCurrentValue(Data: TSerial); virtual; abstract;
    /// <summary> Serialized value, that can be used if ISConsistent is true</summary>
    property SerializedData: TSerial read GetSerializedData;
  end;

  TListOfObjects = array of TObject;
  TListOfStrings = array of string;

  // / <summary>Type for field converters that transform a field value into an
  // / array of objects</summary>
  // / <param name="Data">Current object instance being serialized</param>
  // / <param name="Field">Field name</param>
  // / <result> an array of serializable objects </result>
  TObjectsConverter = reference to function(Data: TObject; Field: string): TListOfObjects;
  // / <summary>Type for field converters that transform a field value into an
  // / array of strings</summary>
  // / <param name="Data">Current object instance being serialized</param>
  // / <param name="Field">Field name</param>
  // / <result> an array of strings </result>
  TStringsConverter = reference to function(Data: TObject; Field: string): TListOfStrings;
  // / <summary>Type for type converters that transform any field value of the
  // / registered type into an array of objects</summary>
  // / <param name="Data">Current field object value</param>
  // / <result> an array of serializable objects </result>
  TTypeObjectsConverter = reference to function(Data: TObject): TListOfObjects;
  // / <summary>Type for type converters that transform any field value of the
  // / registered type into an array of strings</summary>
  // / <param name="Data">Current field object value</param>
  // / <result> an array of serializable strings </result>
  TTypeStringsConverter = reference to function(Data: TObject): TListOfStrings;

  // / <summary>Type for field converters that transform a field value into an
  // / object</summary>
  // / <param name="Data">Current object instance being serialized</param>
  // / <param name="Field">Field name</param>
  // / <result> a serializable object </result>
  TObjectConverter = reference to function(Data: TObject; Field: string): TObject;
  // / <summary>Type for field converters that transform a field value into an
  // / string</summary>
  // / <param name="Data">Current object instance being serialized</param>
  // / <param name="Field">Field name</param>
  // / <result> a string </result>
  TStringConverter = reference to function(Data: TObject; Field: string): string;
  // / <summary>Type for type converters that transform any field value of the
  // / registered type into an object</summary>
  // / <param name="Data">Current field object value</param>
  // / <result> a serializable object </result>
  TTypeObjectConverter = reference to function(Data: TObject): TObject;
  // / <summary>Type for type converters that transform any field value of the
  // / registered type into a string</summary>
  // / <param name="Data">Current field object value</param>
  // / <result> a string </result>
  TTypeStringConverter = reference to function(Data: TObject): string;

  /// <summary>Converter types</summary>
  TConverterType = (ctObjects, ctStrings, ctTypeObjects, ctTypeStrings, ctObject, ctString, ctTypeObject, ctTypeString);

  // / <summary>Type for field reverters that sets field to a value based on
  // / an array of objects</summary>
  // / <param name="Data">Current object instance being serialized</param>
  // / <param name="Field">Field name</param>
  // / <param name="Args"> an array of objects </param>
  TObjectsReverter = reference to procedure(Data: TObject; Field: string; Args: TListOfObjects);
  // / <summary>Type for field reverters that sets field to a value based on
  // / an array of strings</summary>
  // / <param name="Data">Current object instance being serialized</param>
  // / <param name="Field">Field name</param>
  // / <param name="Args"> an array of strings </param>
  TStringsReverter = reference to procedure(Data: TObject; Field: string; Args: TListOfStrings);
  // / <summary>Type for type reverters that create a value based on
  // / an array of objects</summary>
  // / <param name="Data">array of objects</param>
  // / <returns>object that will be set to any field of registered type</returns>
  TTypeObjectsReverter = reference to function(Data: TListOfObjects): TObject;
  // / <summary>Type for type reverters that create a value based on
  // / an array of strings</summary>
  // / <param name="Data">array of strings</param>
  // / <returns>object that will be set to any field of registered type</returns>
  TTypeStringsReverter = reference to function(Data: TListOfStrings): TObject;

  // / <summary>Type for field reverters that sets field to a value based on
  // / an object</summary>
  // / <param name="Data">Current object instance being serialized</param>
  // / <param name="Field">Field name</param>
  // / <param name="Arg"> an object </param>
  TObjectReverter = reference to procedure(Data: TObject; Field: string; Arg: TObject);
  // / <summary>Type for field reverters that sets field to a value based on
  // / a string</summary>
  // / <param name="Data">Current object instance being serialized</param>
  // / <param name="Field">Field name</param>
  // / <param name="Arg"> a string </param>
  TStringReverter = reference to procedure(Data: TObject; Field: string; Arg: string);
  // / <summary>Type for type reverters that create a value based on
  // / an object</summary>
  // / <param name="Data">an object</param>
  // / <returns>object that will be set to any field of registered type</returns>
  TTypeObjectReverter = reference to function(Data: TObject): TObject;
  // / <summary>Type for type reverters that create a value based on
  // / a string</summary>
  // / <param name="Data">a string</param>
  // / <returns>object that will be set to any field of registered type</returns>
  TTypeStringReverter = reference to function(Data: string): TObject;

  /// <summary>Indicate the type of items that the event or interceptor reverts.</summary>
  TReverterType = (rtObjects, rtStrings, rtTypeObjects, rtTypeStrings, rtObject, rtString, rtTypeObject, rtTypeString);

  /// <summary>Converter event class</summary>
  /// <remarks>Using the appropriate property the event type is used by the
  /// marshalling code to invoke the corresponding converter</remarks>
  TConverterEvent = class
  private
    FFieldClassType: TClass;
    FFieldName: string;
    FConverterType: TConverterType;
    FObjectsConverter: TObjectsConverter;
    FStringsConverter: TStringsConverter;
    FTypeObjectsConverter: TTypeObjectsConverter;
    FTypeStringsConverter: TTypeStringsConverter;
    FObjectConverter: TObjectConverter;
    FStringConverter: TStringConverter;
    FTypeObjectConverter: TTypeObjectConverter;
    FTypeStringConverter: TTypeStringConverter;
  protected
    procedure SetObjectsConverter(Converter: TObjectsConverter);
    procedure SetStringsConverter(Converter: TStringsConverter);
    procedure SetTypeObjectsConverter(Converter: TTypeObjectsConverter);
    procedure SetTypeStringsConverter(Converter: TTypeStringsConverter);
    procedure SetObjectConverter(Converter: TObjectConverter);
    procedure SetStringConverter(Converter: TStringConverter);
    procedure SetTypeObjectConverter(Converter: TTypeObjectConverter);
    procedure SetTypeStringConverter(Converter: TTypeStringConverter);
  public
    constructor Create; overload;
    constructor Create(AFieldClassType: TClass; AFieldName: string); overload;

    function IsTypeConverter: Boolean;

    property ConverterType: TConverterType read FConverterType;
    property ObjectsConverter: TObjectsConverter read FObjectsConverter write SetObjectsConverter;
    property StringsConverter: TStringsConverter read FStringsConverter write SetStringsConverter;
    property TypeObjectsConverter: TTypeObjectsConverter read FTypeObjectsConverter write SetTypeObjectsConverter;
    property TypeStringsConverter: TTypeStringsConverter read FTypeStringsConverter write SetTypeStringsConverter;
    property ObjectConverter: TObjectConverter read FObjectConverter write SetObjectConverter;
    property StringConverter: TStringConverter read FStringConverter write SetStringConverter;
    property TypeObjectConverter: TTypeObjectConverter read FTypeObjectConverter write SetTypeObjectConverter;
    property TypeStringConverter: TTypeStringConverter read FTypeStringConverter write SetTypeStringConverter;
    property FieldClassType: TClass read FFieldClassType;
    property FieldName: string read FFieldName;
  end;

  TReverterEvent = class
  private
    FFieldClassType: TClass;
    FFieldName: string;
    FReverterType: TReverterType;
    FObjectsReverter: TObjectsReverter;
    FStringsReverter: TStringsReverter;
    FTypeObjectsReverter: TTypeObjectsReverter;
    FTypeStringsReverter: TTypeStringsReverter;
    FObjectReverter: TObjectReverter;
    FStringReverter: TStringReverter;
    FTypeObjectReverter: TTypeObjectReverter;
    FTypeStringReverter: TTypeStringReverter;
  protected
    procedure SetObjectsReverter(Reverter: TObjectsReverter);
    procedure SetStringsReverter(Reverter: TStringsReverter);
    procedure SetTypeObjectsReverter(Reverter: TTypeObjectsReverter);
    procedure SetTypeStringsReverter(Reverter: TTypeStringsReverter);
    procedure SetObjectReverter(Reverter: TObjectReverter);
    procedure SetStringReverter(Reverter: TStringReverter);
    procedure SetTypeObjectReverter(Reverter: TTypeObjectReverter);
    procedure SetTypeStringReverter(Reverter: TTypeStringReverter);
  public
    constructor Create; overload;
    constructor Create(AFieldClassType: TClass; AFieldName: string); overload;

    function IsTypeReverter: Boolean;

    property ReverterType: TReverterType read FReverterType;
    property ObjectsReverter: TObjectsReverter read FObjectsReverter write SetObjectsReverter;
    property StringsReverter: TStringsReverter read FStringsReverter write SetStringsReverter;
    property TypeObjectsReverter: TTypeObjectsReverter read FTypeObjectsReverter write SetTypeObjectsReverter;
    property TypeStringsReverter: TTypeStringsReverter read FTypeStringsReverter write SetTypeStringsReverter;
    property ObjectReverter: TObjectReverter read FObjectReverter write SetObjectReverter;
    property StringReverter: TStringReverter read FStringReverter write SetStringReverter;
    property TypeObjectReverter: TTypeObjectReverter read FTypeObjectReverter write SetTypeObjectReverter;
    property TypeStringReverter: TTypeStringReverter read FTypeStringReverter write SetTypeStringReverter;

    property FieldClassType: TClass read FFieldClassType;
    property FieldName: string read FFieldName;
  end;

  TJSONCanPopulateProc = TFunc<TObject, TRttiField, Boolean>;

  TJSONPopulationCustomizer = class
  private
    FCanPopulate: TJSONCanPopulateProc;
  protected
    function CanPopulate(Data: TObject; rttiField: TRttiField): Boolean; virtual;
    procedure PrePopulateObjField(Data: TObject; rttiField: TRttiField); virtual;
    procedure DoFieldPopulated(Data: TObject; rttiField: TRttiField); virtual;
  public
    constructor Create(ACanPopulate: TJSONCanPopulateProc);
    /// <summary>Customizer to alter an unmarshalled object instance before
    /// populating fields</summary>
    /// <param name="Data">Current object instance being serialized</param>
    /// <param name="rttiContext">RTTI context for field reflection</param>
    procedure PrePopulate(Data: TObject; rttiContext: TRttiContext); virtual;
    /// <summary>Customizer to alter an unmarshalled object instance after
    /// populating fields</summary>
    /// <param name="Data">Current object instance being serialized</param>
    procedure PostPopulate(Data: TObject); virtual;
  end;

  TJSONInterceptor = class
  private
    FConverterType: TConverterType;
    FReverterType: TReverterType;
    FObjectType: TClass;
  public
    /// <summary>Converters that transforms a field value into an
    /// array of objects</summary>
    /// <param name="Data">Current object instance being serialized</param>
    /// <param name="Field">Field name</param>
    /// <result> an array of serializable objects </result>
    function ObjectsConverter(Data: TObject; Field: string): TListOfObjects; virtual;
    /// <summary>Converter that transforms a field value into an
    /// array of strings</summary>
    /// <param name="Data">Current object instance being serialized</param>
    /// <param name="Field">Field name</param>
    /// <result> an array of strings </result>
    function StringsConverter(Data: TObject; Field: string): TListOfStrings; virtual;
    /// <summary>Converter that transforms any object into an array of intermediate
    /// objects</summary>
    /// <param name="Data">Current object instance</param>
    /// <result> an array of serializable objects </result>
    function TypeObjectsConverter(Data: TObject): TListOfObjects; virtual;
    /// <summary>Converter that transforms an object instance into an array of
    /// strings</summary>
    /// <param name="Data">Current object instance</param>
    /// <result> an array of strings </result>
    function TypeStringsConverter(Data: TObject): TListOfStrings; virtual;
    /// <summary>Converters that transforms a field value into an
    /// intermediate object</summary>
    /// <param name="Data">Current object instance being serialized</param>
    /// <param name="Field">Field name</param>
    /// <result> a serializable object </result>
    function ObjectConverter(Data: TObject; Field: string): TObject; virtual;
    /// <summary>Converters that transforms a field value into an
    /// string</summary>
    /// <param name="Data">Current object instance being serialized</param>
    /// <param name="Field">Field name</param>
    /// <result> a string </result>
    function StringConverter(Data: TObject; Field: string): string; virtual;
    /// <summary>Converter that transforms an object into an equivalent
    /// that can be eventually marshaled</summary>
    /// <param name="Data">Current object instance</param>
    /// <result> an intermediate object </result>
    function TypeObjectConverter(Data: TObject): TObject; virtual;
    /// <summary>Converter for an object instance into a string</summary>
    /// <param name="Data">Current object</param>
    /// <result>string equivalent</result>
    function TypeStringConverter(Data: TObject): string; virtual;
    /// <summary>Field reverter that sets an object field to a value based on
    /// an array of intermediate objects</summary>
    /// <param name="Data">Current object instance being populated</param>
    /// <param name="Field">Field name</param>
    /// <param name="Args"> an array of objects </param>
    procedure ObjectsReverter(Data: TObject; Field: string; Args: TListOfObjects); virtual;
    /// <summary>Reverter that sets an object field to a value based on
    /// an array of strings</summary>
    /// <param name="Data">Current object instance being populated</param>
    /// <param name="Field">Field name</param>
    /// <param name="Args"> an array of strings </param>
    procedure StringsReverter(Data: TObject; Field: string; Args: TListOfStrings); virtual;
    /// <summary>Reverter that creates an object instance based on
    /// an array of intermediate objects</summary>
    /// <param name="Data">array of intermediate objects</param>
    /// <returns>object that will be set to any field of registered type</returns>
    function TypeObjectsReverter(Data: TListOfObjects): TObject; virtual;
    /// <summary>Reverter that creates an object instance from string array</summary>
    /// <param name="Data">array of strings</param>
    /// <returns>object that will be set to any field of registered type</returns>
    function TypeStringsReverter(Data: TListOfStrings): TObject; virtual;
    /// <summary>Reverter that sets an object field to a value based on
    /// an intermediate object</summary>
    /// <param name="Data">Current object instance being populated</param>
    /// <param name="Field">Field name</param>
    /// <param name="Arg"> intermediate object </param>
    procedure ObjectReverter(Data: TObject; Field: string; Arg: TObject); virtual;
    /// <summary>Reverter that sets an object field to a value from
    /// a string</summary>
    /// <param name="Data">Current object instance being populated</param>
    /// <param name="Field">Field name</param>
    /// <param name="Arg">serialized value as a string </param>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); virtual;
    /// <summary>Reverter that creates an object from an intermediate serialized representation</summary>
    /// <param name="Data">TObject - intermediate object</param>
    /// <returns>object created from the serialized representation</returns>
    function TypeObjectReverter(Data: TObject): TObject; virtual;
    /// <summary>Creates an instance based from a string</summary>
    /// <param name="Data">String - string value</param>
    /// <returns>TObject - object that will be set to any field of registered type</returns>
    function TypeStringReverter(Data: string): TObject; virtual;

    function IsTypeConverter: Boolean;
    function IsTypeReverter: Boolean;

    property ConverterType: TConverterType read FConverterType write FConverterType;
    property ReverterType: TReverterType read FReverterType write FReverterType;

    /// <summary>
    ///   Specifies an explicit class type, if the payload object is different from the field type it gets converted from / reverted to. Only
    ///   applies to Object type interceptors. If nil, then field and payload object need to be of same type.
    ///   Used for example for TDatetime, where a date might be represented as {"$date": 123456789}
    /// </summary>
    property ObjectType: TClass read FObjectType write FObjectType;
  end;

    type
  /// <summary>Attribute that defines the interceptor used to marshal/un-marshal
  /// data. It also used to control the life cycle of the intermediate objects
  /// that may be generated by the marshalling process.
  /// A value can be marshalled in various ways and this is the order in which is done:
  /// - a registered field event takes precedence
  /// - a registered type event
  /// - an interceptor defined by a field attribute
  /// - an interceptor defined by a type attribute
  /// - default marshal/un-marshal
  /// </summary>
  JsonReflectAttribute = class(TCustomAttribute)
  private
    FMarshalOwner: Boolean;
    FConverterType: TConverterType;
    FReverterType: TReverterType;
    FInterceptor: TClass;
    FPopulationCustomizer: TClass;
  public
    constructor Create(IsMarshalOwned: Boolean); overload;
    constructor Create(ConverterType: TConverterType; ReverterType: TReverterType; InterceptorType: TClass = nil;
      PopulationCustomizerType: TClass = nil; IsMarshalOwned: Boolean = false); overload;

    /// <summary>Creates a TJSONInterceptor instance from the current definition.
    /// </summary>
    /// <remarks>The caller takes ownership of that object</remarks>
    /// <returns>TJSONInterceptor - interceptor instance</returns>
    function JSONInterceptor: TJSONInterceptor;

    /// <summary>Creates a TJSONPopulationCustomizer instance from the current
    /// definition.</summary>
    /// <remarks>The caller takes ownership of that object</remarks>
    /// <returns>TJSONPopulationCustomizer - population customizer instance</returns>
    function JSONPopulationCustomizer: TJSONPopulationCustomizer;

    /// <summary> If true, the intermediate objects created during marshalling are freed</summary>
    property MarshalOwner: Boolean read FMarshalOwner;
  end;

  TMarshalUnmarshalBase = class
  private
    FMarshalled: TDictionary<string, Boolean>;
    procedure RegisterJSONMarshalled(AComposeKey: string; Marshal: Boolean); overload;

  public
    /// <summary>static function for key generation used in dictionary lookups</summary>
    /// <param name="clazz">a meta class</param>
    /// <param name="field">field name</param>
    /// <returns>dictionary key</returns>
    class function ComposeKey(clazz: TClass; Field: string): string; overload;

    /// <summary>Registers whether a field or type should be
    /// marshalled/unmarshalled. This takes priority over the
    /// JSONMarshalled attribute, which defaults to true.
    /// If Marshal is false, the field/type will be skipped during the marshalling or
    /// unmarshalling process</summary>
    /// <remarks>This takes priority over the JSONMarshalled attribute</remarks>
    /// <param name="clazz">object metaclass</param>
    /// <param name="field">field name</param>
    /// <param name="marshal">marshal flag</param>
    procedure RegisterJSONMarshalled(clazz: TClass; Field: string; Marshal: Boolean); overload;
    /// <summary>Unregisters whether a field or type should be
    /// marshalled/unmarshalled. This clears the existing registration
    /// and defaults back to the JSONMarshalled attribute (if present), and true
    /// if no attribute is available</summary>
    /// <remarks>Clears the existing JSONMarshalled registration if set</remarks>
    /// <param name="clazz">object metaclass</param>
    /// <param name="field">field name</param>
    procedure UnregisterJSONMarshalled(clazz: TClass; Field: string);
    /// <summary>Checks whether or not an object field should be marshalled
    /// based on the JSONMarshalled registration and attribute</summary>
    /// <remarks>The JSONMarshalled registration takes priority over the attribute,
    /// and both default to True</remarks>
    /// <param name="Data">Data to be marshalled</param>
    /// <param name="rttiField">TRTTIField - rtti instance associated with the field</param>
    /// <returns>whether or not to marshal</returns>
    function ShouldMarshal(Data: TObject; rttiField: TRttiField): Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary> Marshalling parent class</summary>
  /// <remarks> Implements visitor patttern, has object id-marker support,
  /// converter registration support. A specialization of this class usually
  /// provides the serialization type. The constructor accepts the converter
  /// event handling</remarks>
  TTypeMarshaller<TSerial: class> = class(TMarshalUnmarshalBase)
  private
    FConverter: TConverter<TSerial>;
    FOwnConverter: Boolean;
    FConverters: TDictionary<string, TConverterEvent>;
    FDateFormat: TJsonDateFormat;
    FDateTimeIsUTC: Boolean;
    FShareConverters: Boolean;
    FRTTICtx: TRttiContext;

  private
    function MarshalSimpleField(rttiField: TRttiField; Data: Pointer): Boolean;
  protected
    /// <summary>composes the type name by qualifying the class name with the
    /// unit name</summary>
    /// <param name="Data">non-nil object instance</param>
    class function ComposeTypeName(Data: TObject): string;
    /// <summary>restores the unit name and the class name from a type name</summary>
    /// <param name="TypeName">Type name generated by ComposeTypeName</param>
    /// <param name="UnitName">Type unit name</param>
    /// <param name="ClassName">Type class name</param>
    class procedure DecomposeTypeName(TypeName: string; out UnitName: string; out ClassName: string);
    /// <summary>Returns a previously-registered converter for the specified field name of the specified class, or nil if there is no matching converter</summary>
    function Converter(clazz: TClass; Field: string): TConverterEvent;
    /// <summary>Checks for the existance of a converter for given meta class and field</summary>
    /// <param name="clazz">Object type</param>
    /// <param name="field">field name</param>
    /// <returns>true if there is a converter associated with type and field</returns>
    function HasConverter(clazz: TClass; Field: string): Boolean;
    /// <summary>Returns the attribute interceptor defined with a class type</summary>
    /// <remarks>Returns nil if there is no attribute defined with the type</remarks>
    /// <param name="clazz">TClass - class type</param>
    /// <returns>TJSONInterceptor instance</returns>
    function GetTypeConverter(clazz: TClass): TJSONInterceptor; overload;
    /// <summary>Returns the attribute interceptor defined with a class type using the class type RTTI instance</summary>
    /// <remarks>Returns nil if there is no attribute defined with the type. It is
    /// expected that the attribute defines a type interceptor
    /// </remarks>
    /// <param name="rttiType">TRTTIType - rtti instance associated with the user class type</param>
    /// <returns>TJSONInterceptor instance</returns>
    function GetTypeConverter(rttiType: TRttiType): TJSONInterceptor; overload;
    /// <summary>Returns the attribute interceptor defined with a field using the field RTTI instance</summary>
    /// <remarks>Returns nil if there is no attribute defined with the field. It is
    /// expected that the attribute defines a value interceptor
    /// </remarks>
    /// <param name="rttiField">TRTTIField - rtti instance associated with the field</param>
    /// <returns>TJSONInterceptor instance</returns>
    function GetTypeConverter(rttiField: TRttiField): TJSONInterceptor; overload;
    /// / <summary>Returns true if there is an attribute interceptor defined for a given field</summary>
    /// <param name="rttiField">TRTTIField - rtti instance associated with the field</param>
    /// <returns>boolean - true if there is an interceptor defined</returns>
    function HasInterceptor(rttiField: TRttiField): Boolean;
    /// <summary>Marshal argument using default converters if user converters are
    /// not defined</summary>
    /// <remarks>If no user converters are defined, it tries to use the default
    /// ones. If a type converter exists then that one is used. If a field converter
    /// is used that the field converter is used. The field converter has precedence
    /// over the type one. </remarks>
    /// <param name="Data">Data to be marshelled</param>
    procedure MarshalData(Data: TObject);
    /// <summary>Marshal argument independent of field name or type</summary>
    /// <param name="Value">Data to be marshelled</param>
    /// <param name="fieldRTTI">TRTTIField - rtti instance associated with the field</param>
    procedure MarshalValue(Value: TValue; fieldRTTI: TRttiField = nil);
    /// <summary>Marshal argument using a registered field converter</summary>
    /// <param name="Data">Data to be marshalled</param>
    /// <param name="field">field name</param>
    procedure MarshalConverter(Data: TObject; Field: string); overload;
    /// <summary>Marshal argument using a field converter</summary>
    /// <param name="Data">Data to be marshalled</param>
    /// <param name="field">field name</param>
    /// <param name="ConverterEvent">user converter registered with this instance</param>
    procedure MarshalConverter(Data: TObject; Field: string; ConverterEvent: TConverterEvent); overload;
    /// <summary>Marshal argument using an attribute interceptor</summary>
    /// <param name="Data">Data to be marshalled</param>
    /// <param name="field">field name</param>
    /// <param name="ConverterEvent">interceptor defined through the attribute</param>
    procedure MarshalConverter(Data: TObject; Field: string; ConverterEvent: TJSONInterceptor); overload;
    /// <summary>Marshal argument using a type converter</summary>
    /// <param name="Data">Data to be marshalled</param>
    /// <param name="field">field name</param>
    /// <param name="ConverterEvent">type converter</param>
    procedure MarshalTypeConverter(Data: TObject; Field: string; ConverterEvent: TConverterEvent); overload;
    /// <summary>Marshal argument using a user interceptor</summary>
    /// <param name="Data">Data to be marshalled</param>
    /// <param name="field">field name</param>
    /// <param name="ConverterEvent">user interceptor instance</param>
    procedure MarshalTypeConverter(Data: TObject; Field: string; ConverterEvent: TJSONInterceptor); overload;
    procedure SetDateTimeIsUTC(const Value: Boolean);
    function GetFieldType(Data: TObject; Field: string): TRttiField;
  public
    /// <summary>Marshal constructor for a given converter.</summary>
    /// <remarks> The converter is freed if second parameter is true (default)</remarks>
    /// <param name="Converter">implementation for On* events</param>
    /// <param name="OwnConverter">If true (default) it takes ownership of the
    /// converter</param>
    constructor Create(Converter: TConverter<TSerial>; OwnConverter: Boolean = true); overload; virtual;
    constructor Create(Converter: TConverter<TSerial>; OwnConverter: Boolean;
      Converters: TObjectDictionary<string, TConverterEvent>); overload; virtual;
    destructor Destroy; override;

    /// <summary>Marshals an object into an equivalent representation.</summary>
    /// <remarks>It uses the converter passed in the constructor</remarks>
    /// <param name="Data">Object instance to be serialized</param>
    /// <returns> the serialized equivalent</returns>
    function Marshal(Data: TObject): TSerial; virtual;

    /// <summary>Registers a user converter event</summary>
    /// <remarks>The converter event will be released by the destructor</remarks>
    /// <param name="clazz">object metaclass</param>
    /// <param name="field">field name</param>
    /// <param name="converter">converter event implementation</param>
    procedure RegisterConverter(clazz: TClass; Field: string; Converter: TConverterEvent); overload;
    /// <summary> Convenience user converter registration for object list</summary>
    /// <remarks> The event converter instance is created behind the scene</remarks>
    /// <param name="clazz">meta class</param>
    /// <param name="field">field name</param>
    /// <param name="func"> object list converter</param>
    procedure RegisterConverter(clazz: TClass; Field: string; func: TObjectsConverter); overload;
    /// <summary> Convenience user defined converter registration for object</summary>
    /// <remarks> The converter event is created behind the scene</remarks>
    /// <param name="clazz">meta class</param>
    /// <param name="field">field name</param>
    /// <param name="func"> object converter</param>
    procedure RegisterConverter(clazz: TClass; Field: string; func: TObjectConverter); overload;
    /// <summary> Convenience user defined converter registration for string array</summary>
    /// <remarks> A converter event is created behind the scene</remarks>
    /// <param name="clazz">meta class</param>
    /// <param name="field">field name</param>
    /// <param name="func"> user converter for string array</param>
    procedure RegisterConverter(clazz: TClass; Field: string; func: TStringsConverter); overload;
    /// <summary> Convenience user defined converter registration for string</summary>
    /// <param name="clazz">meta class</param>
    /// <param name="field">field name</param>
    /// <param name="func"> user defined string converter</param>
    procedure RegisterConverter(clazz: TClass; Field: string; func: TStringConverter); overload;
    /// <summary> Convenience user converter registration for object list</summary>
    /// <param name="clazz">meta class</param>
    /// <param name="func"> user defined converter for string array</param>
    procedure RegisterConverter(clazz: TClass; func: TTypeObjectsConverter); overload;
    /// <summary> Convenience user defined converter registration for object</summary>
    /// <param name="clazz">meta class</param>
    /// <param name="func"> user defined converter for object</param>
    procedure RegisterConverter(clazz: TClass; func: TTypeObjectConverter); overload;
    /// <summary> Convenience user defined converter registration for string array</summary>
    /// <param name="clazz">meta class</param>
    /// <param name="func"> type converter for string array</param>
    procedure RegisterConverter(clazz: TClass; func: TTypeStringsConverter); overload;
    /// <summary> Convenience user defined converter registration for string</summary>
    /// <param name="clazz">meta class</param>
    /// <param name="func"> type converter for string</param>
    procedure RegisterConverter(clazz: TClass; func: TTypeStringConverter); overload;
    property DateFormat: TJsonDateFormat read FDateFormat write FDateFormat;
    property DateTimeIsUTC: Boolean read FDateTimeIsUTC write SetDateTimeIsUTC;
  end;

  TJSONConverter = class(TConverter<TJSONValue>)
  private
    FRoot: TJSONValue;
    FStack: TStack<TJSONAncestor>;
  protected
    function GetSerializedData: TJSONValue; override;
    procedure ProcessData(Data: TJSONAncestor); virtual;
    function GetCurrent: TJSONAncestor;
    property Current: TJSONAncestor read GetCurrent;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    class function ConvertFieldNameToJson(const AField: TRttiField): string; overload;
    class function ConvertFieldNameToJson(AObject:TObject; const AFieldName:string): string; overload;
    procedure OnRefType(TypeName: string; id: Integer); override;
    procedure OnTypeStart(TypeName: string); override;
    procedure OnTypeEnd(TypeName: string); override;
    procedure OnFieldStart(Field: TRttiField); override;
    procedure OnFieldEnd(Field: TRttiField); override;
    procedure OnListStart; override;
    procedure OnListEnd; override;
    procedure OnString(Data: string); override;
    procedure OnNumber(Data: string); override;
    procedure OnBoolean(Data: Boolean); override;
    procedure OnNull; override;
    function IsConsistent: Boolean; override;
    procedure SetCurrentValue(Data: TJSONValue); override;
  end;

  TJSONMarshal = class(TTypeMarshaller<TJSONValue>)
  private
  public
    constructor Create; overload;
    constructor Create(Converter: TConverter<TJSONValue>; OwnConverter: Boolean = true); overload; override;
    constructor Create(Converter: TConverter<TJSONValue>; OwnConverter: Boolean;
      Converters: TObjectDictionary<string, TConverterEvent>); overload; override;
  end;

  /// <summary>Un-marshalling class for JSON objects</summary>
  /// <remarks>It is assumed that the JSON object was created by an instance of
  /// TJSONMarshal and proper reverters are defined with the instance.
  /// </remarks>
  TJSONUnMarshal = class(TMarshalUnmarshalBase)
  private
    FDateFormat: TJsonDateFormat;
    FDateTimeIsUTC: Boolean;
    FObjectHash: TDictionary<string, TObject>;
    FReverters: TDictionary<string, TReverterEvent>;
    FRTTICtx: TRttiContext;
    FShareReverters: Boolean;
    class function ObjectType(Ctx: TRttiContext; TypeName: string): TRttiType;
                                                       
    // class function InnerGenericType(const ATypeName: string): string; static;
    function ConvertFieldNameFromJson(AObject: TObject; const AFieldName: string): string;
    procedure SetDateTimeIsUTC(const Value: Boolean);
  public
    /// <summary>Creates a new instance of an object based on type name
    /// </summary>
    /// <remarks>It is assumed the object has a no-parameter Create constructor
    /// </remarks>
    /// <param name="Ctx">runtime context information instance</param>
    /// <param name="TypeName">object type as generated by marshal ComposeKey</param>
    /// <returns>object instance or nil if creation fails</returns>
    class function ObjectInstance(Ctx: TRttiContext; TypeName: string): TObject;

  protected

    /// <summary>returns true if a reverter matching the given key was registered
    /// </summary>
    /// <param name="key">reverter key, as generated by ComposeKey</param>
    /// <returns>true if a reverter is available</returns>
    function HasReverter(key: string): Boolean;
    /// <summary>Returns the interceptor responsible for reverting a field, if any</summary>
    /// <param name="field">TRTTIField instance associated with the field to be reverted</param>
    /// <returns>TReverterEvent instance defined by the attribute, nil there is no JsonReflect attribute</returns>
    function FieldReverter(Field: TRttiField): TJSONInterceptor; overload;
    /// <summary>Returns the interceptor responsible for reverting a field, if any</summary>
    /// <param name="Data">TObject - current data instance</param>
    /// <param name="Field">String - field name where an JsonReflect attribute was defined</param>
    /// <returns>TReverterEvent instance defined by the attribute, nil there is no JsonReflect attribute</returns>
    function FieldReverter(Data: TObject; Field: string): TJSONInterceptor; overload;
    function FieldTypeReverter(ctxType: TRttiType): TJSONInterceptor; overload;
    function FieldTypeReverter(Data: TObject; Field: string): TJSONInterceptor; overload;
    /// <summary>Returns the reverter registered with the given key</summary>
    /// <param name="key">reverter key</param>
    /// <returns>reverter event instance</returns>
    function Reverter(key: string): TReverterEvent;
    /// <summary>Returns the meta-class of a field</summary>
    /// <param name="Data">object instance</param>
    /// <param name="Field">object field name</param>
    /// <param name="AConvertName"></param>
    /// <returns>meta class instance</returns>
    function ClassTypeOf(Data: TObject; Field: string; AConvertName: boolean = true): TClass;

    /// <summary>returns true if the object id identifies a created object
    /// </summary>
    /// <param name="ObjId">object id</param>
    /// <returns>true if there is an object with given id</returns>
    function HasObject(ObjId: string): Boolean;
    /// <summary>returns a stored object based on its id</summary>
    /// <param name="ObjId">object key</param>
    function GetObject(ObjId: string): TObject;
    /// <summary>returns field's RTTI info</summary>
    /// <param name="Data">object instance</param>
    /// <param name="Field">field name</param>
    /// <returns>TRTTIField - RTTI field instance</returns>
    function GetFieldType(Data: TObject; Field: string): TRttiField;
    /// <summary>populates the instance fields with values from the JSON
    /// serialized representation</summary>
    /// <param name="JsonFields">serialized fields object</param>
    /// <param name="Data">object instance</param>
    /// <param name="JsonCustomizer">population customizer instance</param>
    procedure PopulateFields(JsonFields: TJSONObject; Data: TObject; JsonCustomizer: TJSONPopulationCustomizer);
    /// <summary>transforms a JSON array into an array of objects</summary>
    /// <param name="AClass">Type of objects expected to be in the JSON Array.</param>
    /// <param name="JsonArray">JSON array for a list of objects</param>
    /// <returns>list of objects</returns>
    function GetArgObjects(AClass: TClass; JsonArray: TJSONArray): TListOfObjects;
    /// <summary>transforms a JSON array into an array of strings</summary>
    /// <param name="JsonArray">JSON array for a list of strings</param>
    /// <returns>list of objects of strings</returns>
    function GetArgStrings(JsonArray: TJSONArray): TListOfStrings;
    /// <summary> Converts a JSON value into its TValue equivalent based
    /// on given type info
    /// </summary>
    /// <remarks>Throws exception if the conversion is not possible</remarks>
    /// <param name="JsonValue">JSON value</param>
    /// <param name="rttiType">type to be converted into</param>
    /// <returns>TValue equivalent of JsonValue</returns>
    function JSONToTValue(JsonValue: TJSONValue; rttiType: TRttiType): TValue;

    /// <summary> Marshal a string into a TValue based on type info
    /// </summary>
    /// <remarks>Throws exception if the conversion is not possible</remarks>
    /// <param name="Value">string value</param>
    /// <param name="typeInfo">type to be converted into</param>
    /// <returns>TValue equivalent of Value</returns>
    function StringToTValue(Value: string; typeInfo: PTypeInfo): TValue;
    /// <summary> Invokes the reverter event for a given field. As an end result
    /// the field is populated with a value from its JSON representation
    /// </summary>
    /// <param name="recField">TRTTIField - RTTI field to be populated</param>
    /// <param name="Instance">Pointer - current object address</param>
    /// <param name="revEv">TReverterEvent - user reverter that will generate
    /// the field value </param>
    /// <param name="jsonFieldVal">TJSONValue - JSON value used to populate user's value</param>
    procedure RevertType(recField: TRttiField; Instance: Pointer; revEv: TReverterEvent;
      jsonFieldVal: TJSONValue); overload;
    /// <summary> Invokes the interceptor for a given field. As an end result
    /// the field is populated with a value from its JSON representation. The
    /// interceptor is defined through an attribute
    /// </summary>
    /// <param name="recField">TRTTIField - RTTI field to be populated</param>
    /// <param name="Instance">Pointer - current object address</param>
    /// <param name="revEv">TJSONInterceptor - instance of the user interceptor that will generate
    /// the field value. The interceptor is specified through an attribute </param>
    /// <param name="jsonFieldVal">TJSONValue - JSON value used to populate user's value</param>
    procedure RevertType(recField: TRttiField; Instance: Pointer; revEv: TJSONInterceptor;
      jsonFieldVal: TJSONValue); overload;
  public
    constructor Create; overload; virtual;
    constructor Create(Reverters: TObjectDictionary<string, TReverterEvent>); overload; virtual;
    destructor Destroy; override;

    ///	<summary>
    ///	  creates an object based on a serialized JSON representation
    ///	</summary>
    ///	<param name="AClass">
    ///	  Type of the object to be returned
    ///	</param>
    ///	<param name="JsonObj">
    ///	  JSON object instance
    ///	</param>
    ///	<param name="AObject">
    ///	  If AObject holds an instance of AClass, then this object will be filled from the values in JsonObj. I.e. no
    ///	  new instance will be created.
    ///	</param>
    ///	<returns>
    ///	  object instance
    ///	</returns>
    function CreateObject(AClass: TClass; JsonObj: TJSONObject; AObject:TObject=nil): TObject;

    /// <summary>creates an object based on a serialized JSON representation
    /// returns false isnstead of raising an exception, if the object instance cannnot be created.
    /// </summary>
    /// <param name="AClass">Type of the object to be returned by AObject</param>
    /// <param name="JsonObj">JSON object instance</param>
    /// <param name="AObject">object instance</param>
    /// <returns></returns>
    function TryCreateObject(AClass: TClass; JsonObj: TJSONObject; out AObject: TObject): Boolean;

    /// <summary>Registers a user reverter event</summary>
    /// <remarks>The reverter event object will be released by the destructor</remarks>
    /// <param name="clazz">object metaclass</param>
    /// <param name="field">field name</param>
    /// <param name="reverter">reverter event implementation</param>
    procedure RegisterReverter(clazz: TClass; Field: string; Reverter: TReverterEvent); overload;
    /// <summary> Convenience method for user revertor registration for an object list</summary>
    /// <remarks> The event reverter instance is created behind the scene</remarks>
    /// <param name="clazz">meta class</param>
    /// <param name="field">field name</param>
    /// <param name="func"> object list reverter</param>
    procedure RegisterReverter(clazz: TClass; Field: string; func: TObjectsReverter); overload;
    /// <summary> Convenience method for user revertor registration for an object instance</summary>
    /// <remarks> The event reverter instance is created behind the scene</remarks>
    /// <param name="clazz">meta class</param>
    /// <param name="field">field name</param>
    /// <param name="func"> object instance reverter</param>
    procedure RegisterReverter(clazz: TClass; Field: string; func: TObjectReverter); overload;
    /// <summary> Convenience method for user revertor registration for an string list</summary>
    /// <remarks> The event reverter instance is created behind the scene</remarks>
    /// <param name="clazz">meta class</param>
    /// <param name="field">field name</param>
    /// <param name="func">string list reverter</param>
    procedure RegisterReverter(clazz: TClass; Field: string; func: TStringsReverter); overload;
    /// <summary> Convenience method for user revertor registration for a string</summary>
    /// <remarks> The event reverter instance is created behind the scene</remarks>
    /// <param name="clazz">meta class</param>
    /// <param name="field">field name</param>
    /// <param name="func"> string generated by a converter</param>
    procedure RegisterReverter(clazz: TClass; Field: string; func: TStringReverter); overload;
    /// <summary> Convenience method for user type revertor registration for an object list</summary>
    /// <remarks> The event reverter instance is created behind the scene</remarks>
    /// <param name="clazz">meta class</param>
    /// <param name="func"> object list reverter</param>
    procedure RegisterReverter(clazz: TClass; func: TTypeObjectsReverter); overload;
    /// <summary> Convenience method for user type revertor registration for an object</summary>
    /// <remarks> The event reverter instance is created behind the scene</remarks>
    /// <param name="clazz">meta class</param>
    /// <param name="func"> object list reverter</param>
    procedure RegisterReverter(clazz: TClass; func: TTypeObjectReverter); overload;
    /// <summary> Convenience method for user type revertor registration for a string list</summary>
    /// <remarks> The event reverter instance is created behind the scene</remarks>
    /// <param name="clazz">meta class</param>
    /// <param name="func">string list reverter</param>
    procedure RegisterReverter(clazz: TClass; func: TTypeStringsReverter); overload;
    /// <summary> Convenience method for user type revertor registration for a string</summary>
    /// <remarks> The event reverter instance is created behind the scene</remarks>
    /// <param name="clazz">meta class</param>
    /// <param name="func">string generated by a type converter</param>
    procedure RegisterReverter(clazz: TClass; func: TTypeStringReverter); overload;
    /// <summary>sets an object field with given string value</summary>
    /// <param name="Data">object instance</param>
    /// <param name="Field">field name</param>
    /// <param name="Value">field value</param>
    procedure SetField(Data: TObject; Field: string; Value: string); overload;
    /// <summary>sets an object field with given boolean value</summary>
    /// <param name="Data">object instance</param>
    /// <param name="Field">field name</param>
    /// <param name="Value">field value</param>
    procedure SetField(Data: TObject; Field: string; Value: Boolean); overload;
    /// <summary>sets an object field with given object value</summary>
    /// <param name="Data">object instance</param>
    /// <param name="Field">field name</param>
    /// <param name="Value">field value</param>
    procedure SetField(Data: TObject; Field: string; Value: TObject); overload;
    /// <summary>sets a field of array type
    /// </summary>
    /// <param name="Data">object instance</param>
    /// <param name="Field">field name</param>
    /// <param name="Value">field value</param>
    procedure SetFieldArray(Data: TObject; Field: string; Value: TJSONArray);
    /// <summary>sets an object field to nil</summary>
    /// <param name="Data">object instance</param>
    /// <param name="Field">field name</param>
    procedure SetFieldNull(Data: TObject; Field: string);
    class function TValueToJson(JsonValue: TValue): TJSONValue; virtual;
    property DateFormat: TJsonDateFormat read FDateFormat write FDateFormat;
    property DateTimeIsUTC: Boolean read FDateTimeIsUTC write SetDateTimeIsUTC;
  end;

  /// <summary>Serializable TStringList item</summary>
  TSerStringItem = class
  private
    FString: string;
    [Weak]
    FObject: TObject;
  public
    constructor Create(AString: string; AObject: TObject);
  end;

  /// <summary>Serializable TStringList object</summary>
  [JsonReflect(true)]
  TSerStringList = class
  private
    FSerStringItemList: array of TSerStringItem;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
  public
    constructor Create(Source: TStringList);
    destructor Destroy; override;
    function AsStringList: TStringList;
  end;

  TStringListInterceptor = class(TJSONInterceptor)
  public
    function TypeObjectConverter(Data: TObject): TObject; override;
    function TypeObjectReverter(Data: TObject): TObject; override;
  end;

  TJSONConverters = class
  private
    class var
      CFRegConverters: TObjectDictionary<string, TConverterEvent>;
      CFRegReverters: TObjectDictionary<string, TReverterEvent>;
      CFRegMarshal: TDictionary<string, Boolean>;
  public
    class constructor Create;
    class destructor Destroy;
    class function GetJSONMarshaler: TJSONMarshal;
    class function GetJSONUnMarshaler: TJSONUnMarshal;

    class procedure AddConverter(event: TConverterEvent);
    class procedure AddReverter(event: TReverterEvent);
    class procedure AddMarshalFlag(AClass: TClass; AField: string; Marshal: Boolean);
    class procedure ClearMarshalFlag(AClass: TClass; AField: string);
  end;

  /// <summary>Converts a TStringList into a TSerStringList</summary>
function StringListConverter(Data: TObject): TObject;
/// <summary>Reverts a TSerStringList into a TStringList</summary>
function StringListReverter(Ser: TObject): TObject;
/// converts the pair list of a JSON Object into a serializable structure
function JSONObjectPairListConverter(Data: TObject; Field: string): TListOfObjects;
function JSONArrayElementsConverter(Data: TObject; Field: string): TListOfObjects;
procedure JSONObjectPairListReverter(Data: TObject; Field: string; Args: TListOfObjects);
procedure JSONArrayElementsReverter(Data: TObject; Field: string; Args: TListOfObjects);

/// <summary>Returns the value of a boolean attribute on an RTTI object or
/// DefaultValue if the attribute is not set. </summary>
/// <param name="rttiObject">Object to check for attribute value</param>
/// <param name="AttributeClass">Class of boolean attribute to check (must
/// extend JSONBooleanAttribute)</param>
/// <param name="DefaultValue">Value to return if the attribute is not
/// present on rttiObject</param>
/// <returns>Value of boolean attribute or DefaultValue if the attribute
/// is not set</returns>
function JSONBooleanAttributeValue(rttiObject: TRttiNamedObject; AttributeClass: TClass;
  DefaultValue: Boolean = false): Boolean;

type
  TInternalJSONPopulationCustomizer = class(TJSONPopulationCustomizer)
  private
    FBackupCache: TDictionary<TRttiField, TObject>;
    procedure Cleanup;
  protected
    procedure PrePopulateObjField(Data: TObject; rttiField: TRttiField); override;
    procedure DoFieldPopulated(Data: TObject; rttiField: TRttiField); override;
  public
    constructor Create(ACanPopulate: TJSONCanPopulateProc);
    destructor Destroy; override;
    procedure PostPopulate(Data: TObject); override;
  end;

implementation

uses
  System.JSONConsts,
{$IFDEF MACOS}
    Macapi.CoreFoundation,
{$ENDIF MACOS}
  REST.Json.Interceptors;

const
  FIELD_ANY = '*';
  SEP_DOT = '.';

  /// converts the pair list of a JSON Object into a serializable structure
function JSONObjectPairListConverter(Data: TObject; Field: string): TListOfObjects;
var
  I: Integer;
begin
  Assert(Data is TJSONObject);
  SetLength(Result, TJSONObject(Data).Count);

  for I := 0 to TJSONObject(Data).Count - 1 do
    Result[I] := TJSONObject(Data).Pairs[I]
end;

function JSONArrayElementsConverter(Data: TObject; Field: string): TListOfObjects;
var
  I: Integer;
begin
  Assert(Data is TJSONArray);
  SetLength(Result, TJSONArray(Data).Count);
  for I := 0 to TJSONArray(Data).Count - 1 do
    Result[I] := TJSONArray(Data).Items[I]
end;

procedure JSONObjectPairListReverter(Data: TObject; Field: string; Args: TListOfObjects);
var
  I: Integer;
begin
  Assert(Data is TJSONObject);
  TJSONObject(Data).SetPairs(TList<TJSONPair>.Create);
  for I := 0 to Length(Args) - 1 do
  begin
    Assert(Args[I] <> nil);
    Assert(Args[I] is TJSONPair);
    TJSONObject(Data).AddPair(TJSONPair(Args[I]));
  end;
end;

procedure JSONArrayElementsReverter(Data: TObject; Field: string; Args: TListOfObjects);
var
  I: Integer;
begin
  Assert(Data is TJSONArray);
  TJSONArray(Data).SetElements(TList<TJSONValue>.Create);
  for I := 0 to Length(Args) - 1 do
  begin
    Assert(Args[I] <> nil);
    Assert(Args[I] is TJSONValue);
    TJSONArray(Data).AddElement(TJSONValue(Args[I]));
  end;
end;

// Provide converter and reverter because TRTTIField.FieldType is nil for TStringBuilder.FData
procedure StringBuilderReverter(Data: TObject; Field: string; Arg: string);
begin
  Assert(Data is TStringBuilder);
  TStringBuilder(Data).Clear;
  TStringBuilder(Data).Append(Arg);
end;

function StringBuilderConverter(Data: TObject; Field: string): string;
begin
  Assert(Data is TStringBuilder);
  Result := TStringBuilder(Data).ToString;
end;

{ TJSONConverter }

constructor TConverter<TSerial>.Create;
begin

end;

procedure TJSONConverter.Clear;
begin
  FStack.Clear;
  FRoot := nil;
end;

constructor TJSONConverter.Create;
begin
  FStack := TStack<TJSONAncestor>.Create;
end;

destructor TJSONConverter.Destroy;
begin
  // it is normally an error to have an non-empty stack at this point
  FreeAndNil(FStack);
  inherited;
end;

function TJSONConverter.GetCurrent: TJSONAncestor;
begin
  if FStack.Count = 0 then
    Result := nil
  else
    Result := FStack.Peek
end;

function TJSONConverter.GetSerializedData: TJSONValue;
begin
  Result := FRoot;
end;

function TJSONConverter.IsConsistent: Boolean;
begin
  Result := (FRoot <> nil) and (FStack.Count = 0)
end;

procedure TJSONConverter.OnBoolean(Data: Boolean);
begin
  if Data then
    ProcessData(TJSONTrue.Create)
  else
    ProcessData(TJSONFalse.Create)
end;

procedure TJSONConverter.OnFieldEnd(Field: TRttiField);
var
  LFieldName: string;
begin
  LFieldName := ConvertFieldNameToJson(Field);
  if (Current is TJSONPair) and (TJSONPair(Current).JsonString.Value = LFieldName) then
  begin
    if TJSONPair(Current).JsonValue = nil then
      raise EConversionError.Create(Format(SFieldValueMissing, [LFieldName]));
    FStack.Pop;
  end
  else
    raise EConversionError.Create(Format(SFieldExpected, [LFieldName]));
end;

procedure TJSONConverter.OnFieldStart(Field: TRttiField);
var
  LFieldName: string;
begin
  LFieldName := ConvertFieldNameToJson(Field);
  ProcessData(TJSONPair.Create(LFieldName, nil));
end;

procedure TJSONConverter.OnListEnd;
begin
  if Current is TJSONArray then
    FStack.Pop
  else if Current = nil then
    raise EConversionError.Create(Format(SArrayExpected, ['nil']))
  else
    raise EConversionError.Create(SNoArray)
end;

procedure TJSONConverter.OnListStart;
begin
  ProcessData(TJSONArray.Create);
end;

procedure TJSONConverter.OnNull;
begin
  ProcessData(TJSONNull.Create);
end;

procedure TJSONConverter.OnNumber(Data: string);
begin
  ProcessData(TJSONNumber.Create(Data));
end;

procedure TJSONConverter.OnRefType(TypeName: string; id: Integer);
begin
  ProcessData(TJSONObject.Create);
  FStack.Pop;
end;

procedure TJSONConverter.OnString(Data: string);
begin
  ProcessData(TJSONString.Create(Data));
end;

procedure TJSONConverter.OnTypeEnd(TypeName: string);
begin
  FStack.Pop;
end;

procedure TJSONConverter.OnTypeStart(TypeName: string);
begin
  ProcessData(TJSONObject.Create);
end;

procedure TJSONConverter.ProcessData(Data: TJSONAncestor);
begin
  // setup the root
  if FRoot = nil then
    if Data is TJSONValue then
      FRoot := TJSONValue(Data)
    else
      raise EConversionError.Create(Format(SValueExpected, [Data.ClassName]));
  // update current
  if Current <> nil then
  begin
    // pair for an object?
    if Data is TJSONPair then
      if Current is TJSONObject then
        TJSONObject(Current).AddPair(TJSONPair(Data))
      else
        raise EConversionError.Create(SObjectExpectedForPair)
    else if not(Data is TJSONValue) then
      raise EConversionError.Create(Format(SValueExpected, [Data.ClassName]))
    else
      SetCurrentValue(TJSONValue(Data));
  end
  else if Data is TJSONPair then
    raise EConversionError.Create(SInvalidContextForPair);
  // push into the stack
  if (Data is TJSONObject) or (Data is TJSONPair) or (Data is TJSONArray) then
    FStack.Push(Data);
end;

class function TJSONConverter.ConvertFieldNameToJson(AObject:TObject; const AFieldName:string): string;
var
  LRTTICtx: TRttiContext;
  rType: TRttiType;
  LField: TRttiField;
begin
  LRTTICtx := TRttiContext.Create;
  rType := LRTTICtx.GetType(AObject.ClassType);
  LField := rType.GetField(AFieldName);
  if not assigned(LField) then
    LField := rType.GetField('F' + AFieldName);   //If property name was given instead of field name
  result := ConvertFieldNameToJson(LField);
end;

class function TJSONConverter.ConvertFieldNameToJson(const AField: TRttiField): string;
var
  LFieldName: string;
  LAttribute: TCustomAttribute;
begin
  //First check if JsonNameAttribute is applied. Take without conversion
  LFieldName := '';
  for LAttribute in AField.GetAttributes do
  begin
    if LAttribute is JsonNameAttribute then
    begin
      result := JsonNameAttribute(LAttribute).Name;
      break;
    end;
  end;
  //No Name Attribute found, regular rules apply
  if result = '' then
  begin
    // Delphi Fieldname usually start with an "F", which we don't want in JSON.
    // Also Javascript (i.e. JSON) defaults to lower Camel case, i.e. first letter lower case
    // FFullName = 'Elmo'  => {"fullName":"Elmo"}
    LFieldName := AField.Name;
    if LFieldName.StartsWith('F', true) then
      LFieldName := LFieldName.Remove(0, 1);
    LFieldName := LowerCase(LFieldName.Chars[0]) + LFieldName.Substring(1);
    Result := LFieldName;
  end;
end;

procedure TJSONConverter.SetCurrentValue(Data: TJSONValue);
begin
  // data for a pair or an array
  if Current is TJSONPair then
    TJSONPair(Current).JsonValue := TJSONValue(Data)
  else if Current is TJSONArray then
    TJSONArray(Current).AddElement(TJSONValue(Data))
  else
    raise EConversionError.Create(SInvalidContext);
end;

{ TTypeMarshaller<TDataType, TSerial> }

class function TTypeMarshaller<TSerial>.ComposeTypeName(Data: TObject): string;
begin
  Result := Data.UnitName + SEP_DOT + Data.ClassName;
end;

function TTypeMarshaller<TSerial>.Converter(clazz: TClass; Field: string): TConverterEvent;
var
  key: string;
  Value: TConverterEvent;
begin
  key := ComposeKey(clazz, Field);
  TMonitor.Enter(FConverters);
  try
    FConverters.TryGetValue(key, Result);
  finally
    TMonitor.Exit(FConverters);
  end;
end;

constructor TTypeMarshaller<TSerial>.Create(Converter: TConverter<TSerial>; OwnConverter: Boolean;
  Converters: TObjectDictionary<string, TConverterEvent>);
begin
  inherited Create;
  FConverter := Converter;
  FOwnConverter := OwnConverter;
  FConverters := Converters;
  FShareConverters := true;
  FRTTICtx.GetType(TObject);
end;

constructor TTypeMarshaller<TSerial>.Create(Converter: TConverter<TSerial>; OwnConverter: Boolean = true);
begin
  inherited Create;
  FConverters := TObjectDictionary<string, TConverterEvent>.Create([doOwnsValues]);
  FShareConverters := false;
  FConverter := Converter;
  FOwnConverter := OwnConverter;
  FRTTICtx.GetType(TObject);
end;

class procedure TTypeMarshaller<TSerial>.DecomposeTypeName(TypeName: string; out UnitName, ClassName: string);
var
  DotPos: Integer;
begin
  // find the last .
  DotPos := TypeName.LastDelimiter(SEP_DOT) + 1;
  if DotPos > 0 then
  begin
    UnitName := AnsiLeftStr(TypeName, DotPos - 1);
    ClassName := AnsiRightStr(TypeName, TypeName.Length - DotPos);
  end;
end;

destructor TTypeMarshaller<TSerial>.Destroy;
begin
  if not FShareConverters then
    FreeAndNil(FConverters);
  if FOwnConverter then
    FreeAndNil(FConverter);
  inherited;
end;

function TTypeMarshaller<TSerial>.GetTypeConverter(clazz: TClass): TJSONInterceptor;
begin
  Result := GetTypeConverter(FRTTICtx.GetType(clazz));
end;

function TTypeMarshaller<TSerial>.GetFieldType(Data: TObject;  Field: string): TRttiField;
var
  rType: TRttiType;
begin
  rType := FRTTICtx.GetType(Data.ClassType);
  Result := rType.GetField(Field);
end;

function TTypeMarshaller<TSerial>.GetTypeConverter(rttiField: TRttiField): TJSONInterceptor;
var
  attr: TCustomAttribute;
begin
  Result := nil;
  if rttiField <> nil then
  begin
    try
      for attr in rttiField.GetAttributes do
        if attr is JsonReflectAttribute then
          result := JsonReflectAttribute(attr).JSONInterceptor;

    //Now check for specific Converters
    if result = nil then
    begin
      // TDateTime
      if (rttiField.FieldType <> nil) and (rttiField.FieldType.IsPublicType) and
         (rttiField.FieldType.QualifiedName = 'System.TDateTime') then // Do not localize
      begin
        case DateFormat of
          jdfISO8601: Result := TJSONInterceptor(TISODateTimeInterceptor.Create(DateTimeIsUTC));
          jdfUnix: Result := TJSONInterceptor(TUnixDateTimeInterceptor.Create(DateTimeIsUTC));
          jdfMongo: Result := TJSONInterceptor(TMongoDateTimeInterceptor.Create(DateTimeIsUTC));
          jdfParse: Result := TJSONInterceptor(TParseDateTimeInterceptor.Create(DateTimeIsUTC));
        end;
      end;
     end;
    except
    end;
  end;
end;

function TTypeMarshaller<TSerial>.GetTypeConverter(rttiType: TRttiType): TJSONInterceptor;
var
  attr: TCustomAttribute;
begin
  try
    for attr in rttiType.GetAttributes do
      if attr is JsonReflectAttribute then
        Exit(JsonReflectAttribute(attr).JSONInterceptor);
  except
  end;
  Result := nil;
end;

function TTypeMarshaller<TSerial>.HasConverter(clazz: TClass; Field: string): Boolean;
begin
  TMonitor.Enter(FConverters);
  try
    Result := FConverters.ContainsKey(ComposeKey(clazz, Field));
  finally
    TMonitor.Exit(FConverters);
  end;
end;

//This function has little value, is expensive and should thus not be used
function TTypeMarshaller<TSerial>.HasInterceptor(rttiField: TRttiField): Boolean;
var
  LConverter: TJSONInterceptor;
begin
  LConverter := GetTypeConverter(rttiField);
  result := LConverter <> nil;
  FreeAndNil(LConverter);
end;

function TTypeMarshaller<TSerial>.Marshal(Data: TObject): TSerial;
begin
  try
    MarshalData(Data);
    if FConverter.IsConsistent then
      Result := FConverter.GetSerializedData
    else
      raise EConversionError.Create(SInconsistentConversion)
  finally
    begin
      FConverter.Clear;
    end;
  end;
end;

procedure TTypeMarshaller<TSerial>.MarshalConverter(Data: TObject; Field: string; ConverterEvent: TConverterEvent);
var
  ObjItem: TObject;
  StrItem: string;
  NbrItem: string;
begin
  FConverter.OnFieldStart(GetFieldType(Data, Field));
  case ConverterEvent.ConverterType of
    ctObjects:
      begin
        FConverter.OnListStart;
        for ObjItem in ConverterEvent.ObjectsConverter(Data, Field) do
          MarshalData(ObjItem);
        FConverter.OnListEnd;
      end;
    ctStrings:
      begin
        FConverter.OnListStart;
        for StrItem in ConverterEvent.StringsConverter(Data, Field) do
          FConverter.OnString(StrItem);
        FConverter.OnListEnd;
      end;
    ctObject:
      begin
        ObjItem := ConverterEvent.ObjectConverter(Data, Field);
        MarshalData(ObjItem);
      end;
    ctString:
      FConverter.OnString(ConverterEvent.StringConverter(Data, Field));
  else
    raise EConversionError.Create(Format(SNoConversionForType,
      [GetEnumName(typeInfo(TConverterType), Integer(ConverterEvent.ConverterType))]));
  end;
  FConverter.OnFieldEnd(GetFieldType(Data, Field));
end;

procedure TTypeMarshaller<TSerial>.MarshalConverter(Data: TObject; Field: string; ConverterEvent: TJSONInterceptor);
var
  ObjItem: TObject;
  StrItem: string;
  NbrItem: string;
begin
  case ConverterEvent.ConverterType of
    ctObjects:
      begin
        FConverter.OnListStart;
        for ObjItem in ConverterEvent.ObjectsConverter(Data, Field) do
          MarshalData(ObjItem);
        FConverter.OnListEnd;
      end;
    ctStrings:
      begin
        FConverter.OnListStart;
        for StrItem in ConverterEvent.StringsConverter(Data, Field) do
          FConverter.OnString(StrItem);
        FConverter.OnListEnd;
      end;
    ctObject:
      begin
        ObjItem := ConverterEvent.ObjectConverter(Data, Field);
        MarshalData(ObjItem);
      end;
    ctString:
      FConverter.OnString(ConverterEvent.StringConverter(Data, Field));
  else
    raise EConversionError.Create(Format(SNoConversionForType,
      [GetEnumName(typeInfo(TConverterType), Integer(ConverterEvent.ConverterType))]));
  end;
end;

procedure TTypeMarshaller<TSerial>.MarshalTypeConverter(Data: TObject; Field: string; ConverterEvent: TConverterEvent);
var
  ObjItem: TObject;
  StrItem: string;
  NbrItem: string;
begin
  case ConverterEvent.ConverterType of
    ctTypeObjects:
      begin
        FConverter.OnListStart;
        for ObjItem in ConverterEvent.TypeObjectsConverter(Data) do
          MarshalData(ObjItem);
        FConverter.OnListEnd;
      end;
    ctTypeStrings:
      begin
        FConverter.OnListStart;
        for StrItem in ConverterEvent.TypeStringsConverter(Data) do
          FConverter.OnString(StrItem);
        FConverter.OnListEnd;
      end;
    ctTypeObject:
      begin
        ObjItem := ConverterEvent.TypeObjectConverter(Data);
        MarshalData(ObjItem);
      end;
    ctTypeString:
      FConverter.OnString(ConverterEvent.TypeStringConverter(Data));
  else
    raise EConversionError.Create(Format(SNoConversionForType,
      [GetEnumName(typeInfo(TConverterType), Integer(ConverterEvent.ConverterType))]));
  end;
end;

procedure TTypeMarshaller<TSerial>.MarshalTypeConverter(Data: TObject; Field: string; ConverterEvent: TJSONInterceptor);
var
  ObjItem: TObject;
  StrItem: string;
  NbrItem: string;
begin
  case ConverterEvent.ConverterType of
    ctTypeObjects:
      begin
        FConverter.OnListStart;
        for ObjItem in ConverterEvent.TypeObjectsConverter(Data) do
          MarshalData(ObjItem);
        FConverter.OnListEnd;
      end;
    ctTypeStrings:
      begin
        FConverter.OnListStart;
        for StrItem in ConverterEvent.TypeStringsConverter(Data) do
          FConverter.OnString(StrItem);
        FConverter.OnListEnd;
      end;
    ctTypeObject:
      begin
        ObjItem := ConverterEvent.TypeObjectConverter(Data);
        MarshalData(ObjItem);
      end;
    ctTypeString:
      FConverter.OnString(ConverterEvent.TypeStringConverter(Data));
  else
    raise EConversionError.Create(Format(SNoConversionForType,
      [GetEnumName(typeInfo(TConverterType), Integer(ConverterEvent.ConverterType))]));
  end;
end;

procedure TTypeMarshaller<TSerial>.MarshalValue(Value: TValue; fieldRTTI: TRttiField);
var
  I, Len: Integer;
  rttiType: TRttiType;
  rttiField: TRttiField;
  convEv: TJSONInterceptor;
  Data: TObject;
begin
  if Value.IsEmpty then
    FConverter.OnNull
  else
    case Value.Kind of
      TTypeKind.tkInteger:
        FConverter.OnNumber(IntToStr(Value.AsInteger));
      TTypeKind.tkInt64:
        FConverter.OnNumber(IntToStr(Value.AsInt64));
      TTypeKind.tkFloat:
        FConverter.OnNumber(FloatToJson(Value.AsExtended));
      TTypeKind.tkChar:
        if Value.AsType<char> = #0 then
          FConverter.OnString('')
        else
          FConverter.OnString(Value.AsString);
      TTypeKind.tkWChar:
        if Value.AsType<widechar> = #0 then
          FConverter.OnString('')
        else
          FConverter.OnString(Value.AsString);
      TTypeKind.tkString, TTypeKind.tkLString, TTypeKind.tkWString, TTypeKind.tkUString:
        FConverter.OnString(Value.AsString);
      TTypeKind.tkEnumeration:
        if ((fieldRTTI <> nil) and (string.Compare('Boolean', fieldRTTI.FieldType.Name, true) = 0)) or
          ((fieldRTTI = nil) and (string.Compare('Boolean', Value.typeInfo.NameFld.ToString, true) = 0)) then
          FConverter.OnBoolean(Value.AsBoolean)
        else
          FConverter.OnString(GetEnumName(Value.typeInfo, TValueData(Value).FAsSLong));
      TTypeKind.tkDynArray, TTypeKind.tkArray:
        begin
          FConverter.OnListStart;
          Len := Value.GetArrayLength;
          for I := 0 to Len - 1 do
            MarshalValue(Value.GetArrayElement(I));
          FConverter.OnListEnd
        end;
      TTypeKind.tkClass:
        begin
          Data := Value.AsObject;
          if (Data <> nil) then
            if HasConverter(Data.ClassType, FIELD_ANY) then
              MarshalTypeConverter(Data, EmptyStr, Converter(Data.ClassType, FIELD_ANY))
            else
            begin
              convEv := GetTypeConverter(Data.ClassType);
              if (convEv = nil) and (fieldRTTI <> nil) then
                convEv := GetTypeConverter(fieldRTTI);
              if convEv <> nil then
                try
                  MarshalTypeConverter(Data, EmptyStr, convEv)
                finally
                  convEv.Free
                end
              else
                MarshalData(Data);
            end
          else
            MarshalData(nil);
        end;
      TTypeKind.tkRecord:
        begin
          FConverter.OnListStart;

          // get the type definition
          rttiType := FRTTICtx.GetType(Value.typeInfo);
          if (rttiType.Name = 'TListHelper') and
            (Length(rttiType.GetFields) > 1) and
            (rttiType.GetFields[0].Name = 'FCount')  then
          begin
            // Special handling for TList<T>.FListHelper.  Marshal FCount.
            rttiField := rttiType.GetFields[0];
            MarshalValue(rttiField.GetValue(Value.GetReferenceToRawData), rttiField);
          end
          else
          begin
            // get the record fields
            for rttiField in rttiType.GetFields do
            begin
              MarshalValue(rttiField.GetValue(Value.GetReferenceToRawData), rttiField);
            end;
          end;

          FConverter.OnListEnd
        end;
      TTypeKind.tkPointer:
        raise EConversionError.Create(Format(STypeNotSupported, ['tkPointer']));
      TTypeKind.tkSet:
        raise EConversionError.Create(Format(STypeNotSupported, ['tkSet']));
      TTypeKind.tkMethod:
        raise EConversionError.Create(Format(STypeNotSupported, ['tkMethod']));
      TTypeKind.tkVariant:
        raise EConversionError.Create(Format(STypeNotSupported, ['tkVariant']));
      TTypeKind.tkInterface:
        raise EConversionError.Create(Format(STypeNotSupported, ['tkInterface']));
      TTypeKind.tkClassRef:
        raise EConversionError.Create(Format(STypeNotSupported, ['tkClassRef']));
      TTypeKind.tkProcedure:
        raise EConversionError.Create(Format(STypeNotSupported, ['tkProcedure']));
    else
      raise EConversionError.Create(Format(STypeNotSupported,
        [GetEnumName(Value.typeInfo, TValueData(Value).FAsSLong)]));
    end;
end;

procedure TTypeMarshaller<TSerial>.MarshalConverter(Data: TObject; Field: string);
begin
  MarshalConverter(Data, Field, Converter(Data.ClassType, Field));
end;

procedure TTypeMarshaller<TSerial>.MarshalData(Data: TObject);
var
  rttiType: TRttiType;
  rttiField: TRttiField;
  convEv: TJSONInterceptor;
  lConverter: TConverterEvent;
begin
  if Data = nil then
  begin
    FConverter.OnNull;
  end
  else
  begin
    FConverter.OnTypeStart(ComposeTypeName(Data));
    // marshall the fields
    rttiType := FRTTICtx.GetType(Data.ClassType);
    for rttiField in rttiType.GetFields do
    begin
      if not ShouldMarshal(Data, rttiField) then
        continue;
      if HasConverter(Data.ClassType, rttiField.Name) then
      begin
        lConverter := Converter(Data.ClassType, rttiField.Name);
        if lConverter.IsTypeConverter then
          if (rttiField.FieldType <> nil) and (rttiField.FieldType.TypeKind = tkClass) and
            (rttiField.GetValue(Data).AsObject <> nil) then
          begin
            FConverter.OnFieldStart(rttiField);
            MarshalTypeConverter(rttiField.GetValue(Data).AsObject, rttiField.Name, lConverter);
            FConverter.OnFieldEnd(rttiField)
          end
          else
            raise EConversionError.Create(Format(SNoTypeInterceptorExpected, [rttiField.FieldType.Name]))
        else
          MarshalConverter(Data, rttiField.Name);
      end
      else if HasInterceptor(rttiField) then
      begin
        convEv := GetTypeConverter(rttiField);
        try
          if convEv.IsTypeConverter then
            if (rttiField.FieldType <> nil) and (rttiField.FieldType.TypeKind = tkClass) and
              (rttiField.GetValue(Data).AsObject <> nil) then
            begin
              FConverter.OnFieldStart(rttiField);
              MarshalTypeConverter(rttiField.GetValue(Data).AsObject, rttiField.Name, convEv);
              FConverter.OnFieldEnd(rttiField)
            end
            else
              raise EConversionError.Create(Format(SNoTypeInterceptorExpected, [rttiField.FieldType.Name]))
          else
          begin
            FConverter.OnFieldStart(rttiField);
            MarshalConverter(Data, rttiField.Name, convEv);
            FConverter.OnFieldEnd(rttiField)
          end
        finally
          convEv.Free
        end
      end
      else if (rttiField.FieldType <> nil) and (rttiField.FieldType.TypeKind = tkClass) and (rttiField.GetValue(Data).AsObject <> nil) then
      begin
        if HasConverter(rttiField.GetValue(Data).AsObject.ClassType, FIELD_ANY) then
        begin
          FConverter.OnFieldStart(rttiField);
          MarshalTypeConverter(rttiField.GetValue(Data).AsObject, rttiField.Name,
            Converter(rttiField.GetValue(Data).AsObject.ClassType, FIELD_ANY));
          FConverter.OnFieldEnd(rttiField)
        end
        else
        begin
          convEv := GetTypeConverter(rttiField);
          if convEv <> nil then
            try
              FConverter.OnFieldStart(rttiField);
              if convEv.IsTypeConverter then
                MarshalTypeConverter(rttiField.GetValue(Data).AsObject, rttiField.Name, convEv)
              else
                MarshalConverter(rttiField.GetValue(Data).AsObject, rttiField.Name, convEv);
              FConverter.OnFieldEnd(rttiField)
            finally
              convEv.Free
            end
          else
          begin
            convEv := GetTypeConverter(rttiField.GetValue(Data).AsObject.ClassType);
            if convEv <> nil then
            begin
              try
                FConverter.OnFieldStart(rttiField);
                MarshalTypeConverter(rttiField.GetValue(Data).AsObject, rttiField.Name, convEv);
                FConverter.OnFieldEnd(rttiField)
              finally
                convEv.Free
              end;
            end
            else
            begin
              MarshalSimpleField(rttiField, Data)
            end;
          end;

        end;
      end
      else
        MarshalSimpleField(rttiField, Data)
    end;
    FConverter.OnTypeEnd(ComposeTypeName(Data));

  end;
end;

function TTypeMarshaller<TSerial>.MarshalSimpleField(rttiField: TRttiField; Data: Pointer): Boolean;
var
  fieldValue: TValue;
begin
  if rttiField.FieldType = nil then
    Exit(False);
  case rttiField.FieldType.TypeKind of
    TTypeKind.tkInteger, TTypeKind.tkInt64, TTypeKind.tkChar, TTypeKind.tkWChar, TTypeKind.tkString,
      TTypeKind.tkLString, TTypeKind.tkWString, TTypeKind.tkUString, TTypeKind.tkFloat, TTypeKind.tkClass,
      TTypeKind.tkDynArray, TTypeKind.tkArray:
      begin
        FConverter.OnFieldStart(rttiField);
        MarshalValue(rttiField.GetValue(Data));
        FConverter.OnFieldEnd(rttiField);
      end;
    TTypeKind.tkEnumeration:
      begin
        fieldValue := rttiField.GetValue(Data);
        FConverter.OnFieldStart(rttiField);
        // if fieldValue.IsType<Boolean> then
        if string.Compare('Boolean', rttiField.FieldType.Name, true) = 0 then
          // JSON has boolean value types
          FConverter.OnBoolean(fieldValue.AsBoolean)
        else
          MarshalValue(rttiField.GetValue(Data));
        FConverter.OnFieldEnd(rttiField);
      end;
    TTypeKind.tkRecord:
      begin
        FConverter.OnFieldStart(rttiField);
        MarshalValue(rttiField.GetValue(Data), rttiField);
        FConverter.OnFieldEnd(rttiField);
      end;
    TTypeKind.tkSet, TTypeKind.tkMethod, TTypeKind.tkVariant, TTypeKind.tkInterface, TTypeKind.tkPointer,
      TTypeKind.tkClassRef, TTypeKind.tkProcedure:
      begin
        Exit(false);
      end;
  else
    raise EConversionError.Create(Format(STypeNotSupported, [rttiField.FieldType.Name]));
  end;
  Exit(true);
end;

procedure TTypeMarshaller<TSerial>.RegisterConverter(clazz: TClass; Field: string; func: TStringsConverter);
var
  ConverterEvent: TConverterEvent;
begin
  ConverterEvent := TConverterEvent.Create;
  ConverterEvent.StringsConverter := func;
  RegisterConverter(clazz, Field, ConverterEvent);
end;

procedure TTypeMarshaller<TSerial>.RegisterConverter(clazz: TClass; Field: string; func: TObjectConverter);
var
  ConverterEvent: TConverterEvent;
begin
  ConverterEvent := TConverterEvent.Create;
  ConverterEvent.ObjectConverter := func;
  RegisterConverter(clazz, Field, ConverterEvent);
end;

procedure TTypeMarshaller<TSerial>.RegisterConverter(clazz: TClass; Field: string; func: TStringConverter);
var
  ConverterEvent: TConverterEvent;
begin
  ConverterEvent := TConverterEvent.Create;
  ConverterEvent.StringConverter := func;
  RegisterConverter(clazz, Field, ConverterEvent);
end;

procedure TTypeMarshaller<TSerial>.RegisterConverter(clazz: TClass; Field: string; func: TObjectsConverter);
var
  ConverterEvent: TConverterEvent;
begin
  ConverterEvent := TConverterEvent.Create;
  ConverterEvent.ObjectsConverter := func;
  RegisterConverter(clazz, Field, ConverterEvent);
end;

procedure TTypeMarshaller<TSerial>.RegisterConverter(clazz: TClass; Field: string; Converter: TConverterEvent);
begin
  TMonitor.Enter(FConverters);
  try
    FConverters.AddOrSetValue(ComposeKey(clazz, Field), Converter);
  finally
    TMonitor.Exit(FConverters);
  end;
end;

procedure TTypeMarshaller<TSerial>.RegisterConverter(clazz: TClass; func: TTypeStringsConverter);
var
  ConverterEvent: TConverterEvent;
begin
  ConverterEvent := TConverterEvent.Create;
  ConverterEvent.TypeStringsConverter := func;
  RegisterConverter(clazz, FIELD_ANY, ConverterEvent);
end;

procedure TTypeMarshaller<TSerial>.RegisterConverter(clazz: TClass; func: TTypeObjectConverter);
var
  ConverterEvent: TConverterEvent;
begin
  ConverterEvent := TConverterEvent.Create;
  ConverterEvent.TypeObjectConverter := func;
  RegisterConverter(clazz, FIELD_ANY, ConverterEvent);
end;

procedure TTypeMarshaller<TSerial>.RegisterConverter(clazz: TClass; func: TTypeObjectsConverter);
var
  ConverterEvent: TConverterEvent;
begin
  ConverterEvent := TConverterEvent.Create;
  ConverterEvent.TypeObjectsConverter := func;
  RegisterConverter(clazz, FIELD_ANY, ConverterEvent);
end;

procedure TTypeMarshaller<TSerial>.RegisterConverter(clazz: TClass; func: TTypeStringConverter);
var
  ConverterEvent: TConverterEvent;
begin
  ConverterEvent := TConverterEvent.Create;
  ConverterEvent.TypeStringConverter := func;
  RegisterConverter(clazz, FIELD_ANY, ConverterEvent);
end;

procedure TTypeMarshaller<TSerial>.SetDateTimeIsUTC(const Value: Boolean);
begin
  if Value <> FDateTimeIsUTC then
  begin
    FDateTimeIsUTC := Value;
  end;
end;

{ TConverter<TSerial> }

{ TConverterEvent }

procedure TConverterEvent.SetTypeObjectConverter(Converter: TTypeObjectConverter);
begin
  FConverterType := ctTypeObject;
  FTypeObjectConverter := Converter;
end;

procedure TConverterEvent.SetTypeObjectsConverter(Converter: TTypeObjectsConverter);
begin
  FConverterType := ctTypeObjects;
  FTypeObjectsConverter := Converter;
end;

procedure TConverterEvent.SetTypeStringConverter(Converter: TTypeStringConverter);
begin
  FConverterType := ctTypeString;
  FTypeStringConverter := Converter;
end;

procedure TConverterEvent.SetTypeStringsConverter(Converter: TTypeStringsConverter);
begin
  FConverterType := ctTypeStrings;
  FTypeStringsConverter := Converter;
end;

constructor TConverterEvent.Create;
begin
  inherited;
end;

constructor TConverterEvent.Create(AFieldClassType: TClass; AFieldName: string);
begin
  inherited Create;

  FFieldClassType := AFieldClassType;
  FFieldName := AFieldName;
end;

function TConverterEvent.IsTypeConverter: Boolean;
begin
  Result := FConverterType in [ctTypeObjects, ctTypeStrings, ctTypeObject, ctTypeString];
end;

procedure TConverterEvent.SetObjectConverter(Converter: TObjectConverter);
begin
  FConverterType := ctObject;
  FObjectConverter := Converter;
end;

procedure TConverterEvent.SetObjectsConverter(Converter: TObjectsConverter);
begin
  FConverterType := ctObjects;
  FObjectsConverter := Converter;
end;

procedure TConverterEvent.SetStringConverter(Converter: TStringConverter);
begin
  FConverterType := ctString;
  FStringConverter := Converter;
end;

procedure TConverterEvent.SetStringsConverter(Converter: TStringsConverter);
begin
  FConverterType := ctStrings;
  FStringsConverter := Converter;
end;

{ TReverterEvent }

constructor TReverterEvent.Create;
begin
  inherited;
end;

constructor TReverterEvent.Create(AFieldClassType: TClass; AFieldName: string);
begin
  inherited Create;

  FFieldClassType := AFieldClassType;
  FFieldName := AFieldName;
end;

function TReverterEvent.IsTypeReverter: Boolean;
begin
  Result := FReverterType in [rtTypeObjects, rtTypeStrings, rtTypeObject, rtTypeString];
end;

procedure TReverterEvent.SetObjectReverter(Reverter: TObjectReverter);
begin
  FReverterType := rtObject;
  FObjectReverter := Reverter;
end;

procedure TReverterEvent.SetObjectsReverter(Reverter: TObjectsReverter);
begin
  FReverterType := rtObjects;
  FObjectsReverter := Reverter;
end;

procedure TReverterEvent.SetStringReverter(Reverter: TStringReverter);
begin
  FReverterType := rtString;
  FStringReverter := Reverter;
end;

procedure TReverterEvent.SetStringsReverter(Reverter: TStringsReverter);
begin
  FReverterType := rtStrings;
  FStringsReverter := Reverter;
end;

procedure TReverterEvent.SetTypeObjectReverter(Reverter: TTypeObjectReverter);
begin
  FReverterType := rtTypeObject;
  FTypeObjectReverter := Reverter;
end;

procedure TReverterEvent.SetTypeObjectsReverter(Reverter: TTypeObjectsReverter);
begin
  FReverterType := rtTypeObjects;
  FTypeObjectsReverter := Reverter;
end;

procedure TReverterEvent.SetTypeStringReverter(Reverter: TTypeStringReverter);
begin
  FReverterType := rtTypeString;
  FTypeStringReverter := Reverter;
end;

procedure TReverterEvent.SetTypeStringsReverter(Reverter: TTypeStringsReverter);
begin
  FReverterType := rtTypeStrings;
  FTypeStringsReverter := Reverter;
end;

{ TJSONUnMarshal }

function TJSONUnMarshal.FieldReverter(Data: TObject; Field: string): TJSONInterceptor;
begin
  Result := FieldReverter(GetFieldType(Data, Field));
end;

function TJSONUnMarshal.FieldTypeReverter(ctxType: TRttiType): TJSONInterceptor;
var
  attr: TCustomAttribute;
begin
  if ctxType <> nil then
  begin
    try
      for attr in ctxType.GetAttributes do
        if attr is JsonReflectAttribute then
          Exit(JsonReflectAttribute(attr).JSONInterceptor);
    except

    end;
  end;
  Result := nil;
end;

function TJSONUnMarshal.FieldTypeReverter(Data: TObject; Field: string): TJSONInterceptor;
var
  LField: TRttiField;
begin
  LField := GetFieldType(Data, Field);
  if LField <> nil then
    Result := FieldTypeReverter(LField.FieldType)
  else
    Result := nil;
end;

function TJSONUnMarshal.ClassTypeOf(Data: TObject; Field: string; AConvertName: boolean = true): TClass;
var
  tRtti: TRttiType;
  fRtti: TRttiField;
  LFieldName: string;
begin
  Result := nil;
  tRtti := FRTTICtx.GetType(Data.ClassType);
  if tRtti <> nil then
  begin
    LFieldName := Field;
    if AConvertName then
      LFieldName := ConvertFieldNameFromJson(Data, LFieldName);
    fRtti := tRtti.GetField(LFieldName);
    if (fRtti <> nil) and (fRtti.FieldType.IsInstance) then
      Result := fRtti.FieldType.AsInstance.MetaclassType;
  end;
end;

constructor TJSONUnMarshal.Create;
begin
  inherited Create;
  FObjectHash := TDictionary<string, TObject>.Create;
  FReverters := TObjectDictionary<string, TReverterEvent>.Create([doOwnsValues]);
  FShareReverters := false;
  FRTTICtx.GetType(TObject);

  DateTimeIsUTC := true;
  DateFormat := TJsonDateFormat.jdfISO8601;

  // JSON reverters
  RegisterReverter(TJSONObject, 'FMembers', JSONObjectPairListReverter);
  RegisterReverter(TJSONArray, 'FElements', JSONArrayElementsReverter);
  RegisterReverter(TStringBuilder, 'FData', StringBuilderReverter);

end;

constructor TJSONUnMarshal.Create(Reverters: TObjectDictionary<string, TReverterEvent>);
begin
  inherited Create;
  FObjectHash := TDictionary<string, TObject>.Create;
  FReverters := Reverters;
  FShareReverters := true;
  FRTTICtx.GetType(TObject);
end;

function TJSONUnMarshal.TryCreateObject(AClass: TClass; JsonObj: TJSONObject; out AObject: TObject): Boolean;
begin
  if not Assigned(AClass) then
      result := false
  else
    try
      AObject := CreateObject(AClass, JsonObj);
      Result := true;
    except
      Result := false;
    end;
end;

function TJSONUnMarshal.CreateObject(AClass: TClass; JsonObj: TJSONObject; AObject:TObject=nil): TObject;
var
  LObjectType: string;
  LObject: TObject;
  LRttiType: TRttiType;
  LAttribute: TCustomAttribute;
  LCustomizer: TJSONPopulationCustomizer;
begin
  Assert(JsonObj <> nil);
  if not Assigned(AClass) then
    result := nil
  else
  begin
    LObjectType := AClass.QualifiedClassName; // RttiTypeOf(T).QualifiedName;
    if not Assigned(AObject) then
      LObject := ObjectInstance(FRTTICtx, LObjectType)
    else
      LObject := AObject;
    if LObject = nil then
      raise EConversionError.Create(Format(SCannotCreateType, [LObjectType]));

    LCustomizer := nil;
    LRttiType := ObjectType(FRTTICtx, LObjectType);
    if LRttiType <> nil then
      for LAttribute in LRttiType.GetAttributes do
        if LAttribute is JsonReflectAttribute then
        begin
          LCustomizer := JsonReflectAttribute(LAttribute).JSONPopulationCustomizer;
        end;
    if LCustomizer = nil then // Use default population customizer to prevent leaked memory
      LCustomizer := TInternalJSONPopulationCustomizer.Create(
        function(AData: TObject; AField: TRttiField): Boolean
        begin
          Result := ShouldMarshal(AData, AField);
        end);
    try
      try
      LCustomizer.PrePopulate(LObject, FRTTICtx);
      PopulateFields(JsonObj, LObject, LCustomizer);
      LCustomizer.PostPopulate(LObject);
      except
        if LObject <> AObject then
          LObject.Free;
        raise
      end;
    finally
      LCustomizer.Free;
    end;
    Result := LObject;
  end;
end;

destructor TJSONUnMarshal.Destroy;
begin
  FreeAndNil(FObjectHash);
  if not FShareReverters then
    FreeAndNil(FReverters);

  inherited;
end;

function TJSONUnMarshal.FieldReverter(Field: TRttiField): TJSONInterceptor;
var
  fieldAttr: TCustomAttribute;
begin
  Result := nil;
  if Field <> nil then
  begin
    // First check for individual Attribute
    try
      for fieldAttr in Field.GetAttributes do
        if fieldAttr is JsonReflectAttribute then
          Result := JsonReflectAttribute(fieldAttr).JSONInterceptor;

      // Now check for specific types
      if Result = nil then
      begin
        // TDateTime
        if (Field.FieldType.IsPublicType) and
           (Field.FieldType.QualifiedName = 'System.TDateTime') then // Do not localize
        begin
          case DateFormat of
            jdfISO8601: Result := TJSONInterceptor(TISODateTimeInterceptor.Create(DateTimeIsUTC));
            jdfUnix: Result := TJSONInterceptor(TUnixDateTimeInterceptor.Create(DateTimeIsUTC));
            jdfMongo: Result := TJSONInterceptor(TMongoDateTimeInterceptor.Create(DateTimeIsUTC));
          end;
        end;
      end;
    except

    end;
  end;
end;

function TJSONUnMarshal.GetArgObjects(AClass: TClass; JsonArray: TJSONArray): TListOfObjects;
var
  I, Count: Integer;
  jsonVal: TJSONValue;
begin
  Count := JsonArray.Count;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
  begin
    jsonVal := JsonArray.Items[I];
    if jsonVal is TJSONObject then
      Result[I] := CreateObject(AClass, TJSONObject(jsonVal))
    else
      raise EConversionError.Create(Format(SObjectExpectedInArray, [I, JsonArray.ToString]));
  end;
end;

function TJSONUnMarshal.GetArgStrings(JsonArray: TJSONArray): TListOfStrings;
var
  I, Count: Integer;
  jsonVal: TJSONValue;
begin
  Count := JsonArray.Count;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
  begin
    jsonVal := JsonArray.Items[I];
    if jsonVal is TJSONString then
      Result[I] := TJSONString(jsonVal).Value
    else
      raise EConversionError.Create(Format(SStringExpectedInArray, [I, JsonArray.ToString]))
  end;
end;

function TJSONUnMarshal.GetFieldType(Data: TObject; Field: string): TRttiField;
var
  rType: TRttiType;
  LField: string;
begin
  LField := ConvertFieldNameFromJson(Data, Field);
  rType := FRTTICtx.GetType(Data.ClassType);
  Result := rType.GetField(LField);
end;

function TJSONUnMarshal.GetObject(ObjId: string): TObject;
begin
  Result := FObjectHash.Items[ObjId];
end;

function TJSONUnMarshal.HasObject(ObjId: string): Boolean;
begin
  Result := FObjectHash.ContainsKey(ObjId);
end;

function TJSONUnMarshal.HasReverter(key: string): Boolean;
begin
  TMonitor.Enter(FReverters);
  try
    Exit(FReverters.ContainsKey(key));
  finally
    TMonitor.Exit(FReverters);
  end;
end;

function TJSONUnMarshal.ConvertFieldNameFromJson(AObject: TObject; const AFieldName: string): string;
var
  LFieldName: string;
  LRTTICtx: TRttiContext;
  LRTTIType: TRttiType;
  LRTTIField: TRttiField;
  LAttribute: TCustomAttribute;
begin
  result := '';
  //First check if any of the fields in AObject has a JSONName field name mapping
  LRTTICtx := TRttiContext.Create;

  LRTTIType := LRTTICtx.GetType(AObject.ClassType);
  for LRTTIField in LRTTIType.GetFields do
  begin
    for LAttribute in LRTTIField.GetAttributes do
    begin
       if LAttribute is JSONNameAttribute then
       begin
         if AFieldName = JSONNameAttribute(LAttribute).Name then
          result := LRTTIField.Name; 
          Break;
       end;
    end;
  end;


  if result = '' then begin
    // Delphi Fieldname usually start with an "F", which we don't have in JSON:
    // FName = 'Elmo'  => {"Name":"Elmo"}
                                        
    LFieldName := 'F' + AFieldName;
    Result := LFieldName;
  end;
end;

procedure TJSONUnMarshal.RevertType(recField: TRttiField; Instance: Pointer; revEv: TReverterEvent;
jsonFieldVal: TJSONValue);
begin
  case revEv.ReverterType of
    rtTypeObjects:
      begin
                                                
        if jsonFieldVal is TJSONArray then
          recField.SetValue(Instance, revEv.TypeObjectsReverter(GetArgObjects(revEv.FFieldClassType,
            TJSONArray(jsonFieldVal))))
        else if jsonFieldVal is TJSONNull then
          recField.SetValue(Instance, TValue.Empty)
        else
          raise EConversionError.Create(Format(SArrayExpectedForField, [recField.Name, jsonFieldVal.ToString]));
      end;
    rtTypeStrings:
      begin
        if jsonFieldVal is TJSONArray then
          recField.SetValue(Instance, revEv.TypeStringsReverter(GetArgStrings(TJSONArray(jsonFieldVal))))
        else if jsonFieldVal is TJSONNull then
          recField.SetValue(Instance, TValue.Empty)
        else
          raise EConversionError.Create(Format(SArrayExpectedForField, [recField.Name, jsonFieldVal.ToString]));
      end;
    rtTypeObject:
      begin
        if jsonFieldVal is TJSONObject then
        begin
          recField.SetValue(Instance, revEv.TypeObjectReverter(CreateObject(revEv.FFieldClassType,
            TJSONObject(jsonFieldVal))))
        end
        else if jsonFieldVal is TJSONNull then
          recField.SetValue(Instance, TValue.Empty)
        else
          raise EConversionError.Create(Format(SObjectExpectedForField, [recField.Name, jsonFieldVal.ToString]));
      end;
    rtTypeString:
      begin
        if jsonFieldVal is TJSONString then
          recField.SetValue(Instance, revEv.TypeStringReverter(TJSONString(jsonFieldVal).Value))
        else if jsonFieldVal is TJSONNull then
          recField.SetValue(Instance, TValue.Empty)
        else
          raise EConversionError.Create(Format(SObjectExpectedForField, [recField.Name, jsonFieldVal.ToString]));
      end
  else
    raise EConversionError.Create(Format(SNoConversionForType,
      [GetEnumName(typeInfo(TReverterType), Integer(revEv.ReverterType))]));
  end;
end;

procedure TJSONUnMarshal.RevertType(recField: TRttiField; Instance: Pointer; revEv: TJSONInterceptor;
jsonFieldVal: TJSONValue);
var
  LFieldType: TClass;
begin
  case revEv.ReverterType of
    rtTypeObjects:
      begin
        if jsonFieldVal is TJSONArray then
        begin
                                                  
          LFieldType := recField.FieldType.AsInstance.MetaclassType;
          recField.SetValue(Instance, revEv.TypeObjectsReverter(GetArgObjects(LFieldType, TJSONArray(jsonFieldVal))))
        end
        else if jsonFieldVal is TJSONNull then
          recField.SetValue(Instance, TValue.Empty)
        else
          raise EConversionError.Create(Format(SArrayExpectedForField, [recField.Name, jsonFieldVal.ToString]));
      end;
    rtTypeStrings:
      begin
        if jsonFieldVal is TJSONArray then
          recField.SetValue(Instance, revEv.TypeStringsReverter(GetArgStrings(TJSONArray(jsonFieldVal))))
        else if jsonFieldVal is TJSONNull then
          recField.SetValue(Instance, TValue.Empty)
        else
          raise EConversionError.Create(Format(SArrayExpectedForField, [recField.Name, jsonFieldVal.ToString]));
      end;
    rtTypeObject:
      begin
        if jsonFieldVal is TJSONObject then
        begin
          LFieldType := recField.FieldType.AsInstance.MetaclassType;
          recField.SetValue(Instance, revEv.TypeObjectReverter(CreateObject(LFieldType, TJSONObject(jsonFieldVal))))
        end
        else if jsonFieldVal is TJSONNull then
          recField.SetValue(Instance, TValue.Empty)
        else
          raise EConversionError.Create(Format(SObjectExpectedForField, [recField.Name, jsonFieldVal.ToString]));
      end;
    rtTypeString:
      begin
        if jsonFieldVal is TJSONString then
          recField.SetValue(Instance, revEv.TypeStringReverter(TJSONString(jsonFieldVal).Value))
        else if jsonFieldVal is TJSONNull then
          recField.SetValue(Instance, TValue.Empty)
        else
          raise EConversionError.Create(Format(SObjectExpectedForField, [recField.Name, jsonFieldVal.ToString]));
      end
  else
    raise EConversionError.Create(Format(SNoConversionForType,
      [GetEnumName(typeInfo(TReverterType), Integer(revEv.ReverterType))]));
  end;
end;

function TJSONUnMarshal.JSONToTValue(JsonValue: TJSONValue; rttiType: TRttiType): TValue;
var
  tvArray: array of TValue;
  Value: string;
  I: Integer;
  elementType: TRttiType;
  Data: TValue;
  recField: TRttiField;
  attrRev: TJSONInterceptor;
  jsonFieldVal: TJSONValue;
  ClassType: TClass;
  Instance: Pointer;
  LFieldType: TClass;
begin
  // null or nil returns empty
  if (JsonValue = nil) or (JsonValue is TJSONNull) then
    Exit(TValue.Empty);

  // for each JSON value type
  if JsonValue is TJSONNumber then
    // get data "as is"
    Value := TJSONNumber(JsonValue).ToString
  else if JsonValue is TJSONString then
    Value := TJSONString(JsonValue).Value
  else if JsonValue is TJSONTrue then
    Exit(true)
  else if JsonValue is TJSONFalse then
    Exit(false)
  else if JsonValue is TJSONObject then
  // object...
  begin
    LFieldType := rttiType.AsInstance.MetaclassType;
    Exit(CreateObject(LFieldType, TJSONObject(JsonValue)))
  end
  else
  begin
    case rttiType.TypeKind of
      TTypeKind.tkDynArray, TTypeKind.tkArray:
        begin
          // array
          SetLength(tvArray, TJSONArray(JsonValue).Count);
          if rttiType is TRttiArrayType then
            elementType := TRttiArrayType(rttiType).elementType
          else
            elementType := TRttiDynamicArrayType(rttiType).elementType;
          for I := 0 to Length(tvArray) - 1 do
            tvArray[I] := JSONToTValue(TJSONArray(JsonValue).Items[I], elementType);
          Exit(TValue.FromArray(rttiType.Handle, tvArray));
        end;
      TTypeKind.tkRecord:
        begin
          TValue.Make(nil, rttiType.Handle, Data);
          // match the fields with the array elements
          I := 0;
          for recField in rttiType.GetFields do
          begin
            Instance := Data.GetReferenceToRawData;
            jsonFieldVal := TJSONArray(JsonValue).Items[I];
            // check for type reverter
            ClassType := nil;
            if recField.FieldType.IsInstance then
              ClassType := recField.FieldType.AsInstance.MetaclassType;
            if (ClassType <> nil) then
            begin
              if HasReverter(ComposeKey(ClassType, FIELD_ANY)) then
                RevertType(recField, Instance, Reverter(ComposeKey(ClassType, FIELD_ANY)), jsonFieldVal)
              else
              begin
                attrRev := FieldTypeReverter(recField.FieldType);
                if attrRev = nil then
                  attrRev := FieldReverter(recField);
                if attrRev <> nil then
                  try
                    RevertType(recField, Instance, attrRev, jsonFieldVal)
                  finally
                    attrRev.Free
                  end
                else
                  recField.SetValue(Instance, JSONToTValue(jsonFieldVal, recField.FieldType));
              end
            end
            else
              recField.SetValue(Instance, JSONToTValue(jsonFieldVal, recField.FieldType));
            Inc(I);
          end;
          Exit(Data);
        end;
    end;
  end;

  // transform value string into TValue based on type info
  Exit(StringToTValue(Value, rttiType.Handle));
end;

class function TJSONUnMarshal.ObjectType(Ctx: TRttiContext; TypeName: string): TRttiType;
begin
  // type name is qualified at this point (UnitName.TypeName)
  Result := Ctx.FindType(TypeName);
end;

class function TJSONUnMarshal.ObjectInstance(Ctx: TRttiContext; TypeName: string): TObject;
var
  rType: TRttiType;
  mType: TRTTIMethod;
  metaClass: TClass;
begin
  rType := ObjectType(Ctx, TypeName);
  if (rType <> nil) then
    for mType in rType.GetMethods do
    begin
      if mType.HasExtendedInfo and mType.IsConstructor then
      begin
        if Length(mType.GetParameters) = 0 then
        begin
          // invoke
          metaClass := rType.AsInstance.MetaclassType;
          Exit(mType.Invoke(metaClass, []).AsObject);
        end;
      end;
    end;
  Exit(nil);
end;

procedure TJSONUnMarshal.PopulateFields(JsonFields: TJSONObject; Data: TObject;
JsonCustomizer: TJSONPopulationCustomizer);
var
  FieldName: string;
  jsonFieldVal: TJSONValue;
  revEv: TReverterEvent;
  revAttr: TJSONInterceptor;
  ClassType: TClass;
  JsonPairField: TJSONPair;
  LObject: TObject;
  LPopulated: Boolean;
  LObjectType: TClass;
begin
  revEv := nil;
  for JsonPairField in JsonFields do
  begin
    LPopulated := true;
    FieldName := JsonPairField.JsonString.Value;
    jsonFieldVal := JsonPairField.JsonValue;
    ClassType := Data.ClassType;
    // check for reverters
    if HasReverter(ComposeKey(ClassType, FieldName)) then
    begin
      revEv := Reverter(ComposeKey(ClassType, FieldName));
      case revEv.ReverterType of
        rtTypeObjects:
          begin
            if jsonFieldVal is TJSONArray then
            begin
                                                      
              SetField(Data, FieldName, revEv.TypeObjectsReverter(GetArgObjects(revEv.FFieldClassType,
                TJSONArray(jsonFieldVal))));
            end
            else if jsonFieldVal is TJSONNull then
              SetField(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SArrayExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end;
        rtTypeStrings:
          begin
            if jsonFieldVal is TJSONArray then
              SetField(Data, FieldName, revEv.TypeStringsReverter(GetArgStrings(TJSONArray(jsonFieldVal))))
            else if jsonFieldVal is TJSONNull then
              SetField(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SArrayExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end;
        rtTypeObject:
          begin
            if jsonFieldVal is TJSONObject then
              SetField(Data, FieldName, revEv.TypeObjectReverter(CreateObject(revEv.FFieldClassType,
                TJSONObject(jsonFieldVal))))
            else if jsonFieldVal is TJSONNull then
              SetField(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SObjectExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end;
        rtTypeString:
          begin
            if jsonFieldVal is TJSONString then
              SetField(Data, FieldName, revEv.TypeStringReverter(TJSONString(jsonFieldVal).Value))
            else if jsonFieldVal is TJSONNull then
              SetField(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SObjectExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end;
        rtObjects:
          begin
            if jsonFieldVal is TJSONArray then
                                                      
              revEv.ObjectsReverter(Data, FieldName, GetArgObjects(revEv.FFieldClassType, TJSONArray(jsonFieldVal)))
            else if jsonFieldVal is TJSONNull then
              revEv.ObjectsReverter(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SArrayExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end;
        rtStrings:
          begin
            if jsonFieldVal is TJSONArray then
              revEv.StringsReverter(Data, FieldName, GetArgStrings(TJSONArray(jsonFieldVal)))
            else if jsonFieldVal is TJSONNull then
              revEv.ObjectsReverter(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SArrayExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end;
        rtObject:
          begin
            if jsonFieldVal is TJSONObject then
              revEv.ObjectReverter(Data, FieldName, CreateObject(revEv.FFieldClassType, TJSONObject(jsonFieldVal)))
            else if jsonFieldVal is TJSONNull then
              revEv.ObjectsReverter(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SObjectExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end;
        rtString:
          begin
            if jsonFieldVal is TJSONString then
              revEv.StringReverter(Data, FieldName, TJSONString(jsonFieldVal).Value)
            else if jsonFieldVal is TJSONNull then
              revEv.ObjectsReverter(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SObjectExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end;
      else
        raise EConversionError.Create(Format(SNoConversionForType,
          [GetEnumName(typeInfo(TReverterType), Integer(revEv.ReverterType))]));
      end;
    end
    else if HasReverter(ComposeKey(ClassTypeOf(Data, FieldName), FIELD_ANY)) then
    begin
      revEv := Reverter(ComposeKey(ClassTypeOf(Data, FieldName), FIELD_ANY));
      case revEv.ReverterType of
        rtTypeObjects:
          begin
            if jsonFieldVal is TJSONArray then
              SetField(Data, FieldName, revEv.TypeObjectsReverter(GetArgObjects(revEv.FFieldClassType,
                TJSONArray(jsonFieldVal))))
            else if jsonFieldVal is TJSONNull then
              SetField(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SArrayExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end;
        rtTypeStrings:
          begin
            if jsonFieldVal is TJSONArray then
              SetField(Data, FieldName, revEv.TypeStringsReverter(GetArgStrings(TJSONArray(jsonFieldVal))))
            else if jsonFieldVal is TJSONNull then
              SetField(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SArrayExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end;
        rtTypeObject:
          begin
            if jsonFieldVal is TJSONObject then
              SetField(Data, FieldName, revEv.TypeObjectReverter(CreateObject(revEv.FFieldClassType,
                TJSONObject(jsonFieldVal))))
            else if jsonFieldVal is TJSONNull then
              SetField(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SObjectExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end;
        rtTypeString:
          begin
            if jsonFieldVal is TJSONString then
              SetField(Data, FieldName, revEv.TypeStringReverter(TJSONString(jsonFieldVal).Value))
            else if jsonFieldVal is TJSONNull then
              SetField(Data, FieldName, nil)
            else
              raise EConversionError.Create(Format(SObjectExpectedForField, [FieldName, jsonFieldVal.ToString]));
          end
      else
        raise EConversionError.Create(Format(SNoConversionForType,
          [GetEnumName(typeInfo(TReverterType), Integer(revEv.ReverterType))]));
      end;
    end
    else
    begin
      revAttr := FieldReverter(Data, FieldName);
      if revAttr = nil then
        revAttr := FieldTypeReverter(Data, FieldName);
      if revAttr <> nil then
        try
          // Reverters may be implemented elsewhere and don't know about our fieldName Mapping rules
          FieldName := ConvertFieldNameFromJson(Data, FieldName);
          if assigned(revAttr.ObjectType) then
            LObjectType := revAttr.ObjectType
          else
            LObjectType := ClassTypeOf(Data, FieldName, false);
          case revAttr.ReverterType of
            rtTypeObjects:
              begin
                if jsonFieldVal is TJSONArray then
                begin
                                    
                  raise Exception.Create('NOT IMPLEMENTED');
                  SetField(Data, FieldName, revAttr.TypeObjectsReverter(GetArgObjects(revEv.FFieldClassType,
                    TJSONArray(jsonFieldVal))))
                end
                else if jsonFieldVal is TJSONNull then
                  SetField(Data, FieldName, nil)
                else
                  raise EConversionError.Create(Format(SArrayExpectedForField, [FieldName, jsonFieldVal.ToString]));
              end;
            rtTypeStrings:
              begin
                if jsonFieldVal is TJSONArray then
                  SetField(Data, FieldName, revAttr.TypeStringsReverter(GetArgStrings(TJSONArray(jsonFieldVal))))
                else if jsonFieldVal is TJSONNull then
                  SetField(Data, FieldName, nil)
                else
                  raise EConversionError.Create(Format(SArrayExpectedForField, [FieldName, jsonFieldVal.ToString]));
              end;
            rtTypeObject:
              begin
                if jsonFieldVal is TJSONObject then
                  SetField(Data, FieldName, revAttr.TypeObjectReverter(CreateObject(LObjectType, TJSONObject(jsonFieldVal))))
                else if jsonFieldVal is TJSONNull then
                  SetField(Data, FieldName, nil)
                else
                  raise EConversionError.Create(Format(SObjectExpectedForField, [FieldName, jsonFieldVal.ToString]));
              end;
            rtTypeString:
              begin
                if jsonFieldVal is TJSONString then
                  SetField(Data, FieldName, revAttr.TypeStringReverter(TJSONString(jsonFieldVal).Value))
                else if jsonFieldVal is TJSONNull then
                  SetField(Data, FieldName, nil)
                else
                  raise EConversionError.Create(Format(SObjectExpectedForField, [FieldName, jsonFieldVal.ToString]));
              end;
            rtObjects:
              begin
                if jsonFieldVal is TJSONArray then
                  revAttr.ObjectsReverter(Data, FieldName, GetArgObjects(revEv.FFieldClassType,
                    TJSONArray(jsonFieldVal)))
                else if jsonFieldVal is TJSONNull then
                  revAttr.ObjectsReverter(Data, FieldName, nil)
                else
                  raise EConversionError.Create(Format(SArrayExpectedForField, [FieldName, jsonFieldVal.ToString]));
              end;
            rtStrings:
              begin
                if jsonFieldVal is TJSONArray then
                  revAttr.StringsReverter(Data, FieldName, GetArgStrings(TJSONArray(jsonFieldVal)))
                else if jsonFieldVal is TJSONNull then
                  revAttr.ObjectsReverter(Data, FieldName, nil)
                else
                  raise EConversionError.Create(Format(SArrayExpectedForField, [FieldName, jsonFieldVal.ToString]));
              end;
            rtObject:
              begin
                if jsonFieldVal is TJSONObject then
                  revAttr.ObjectReverter(Data, FieldName, CreateObject(LObjectType, TJSONObject(jsonFieldVal)))
                else if jsonFieldVal is TJSONNull then
                  revAttr.ObjectReverter(Data, FieldName, nil)
                else
                  raise EConversionError.Create(Format(SObjectExpectedForField, [FieldName, jsonFieldVal.ToString]));
              end;
            rtString:
              begin
                if (jsonFieldVal is TJSONString) or (jsonFieldVal is TJSONNumber) then
                  revAttr.StringReverter(Data, FieldName, jsonFieldVal.Value)
                else if jsonFieldVal is TJSONNull then
                  revAttr.ObjectsReverter(Data, FieldName, nil)
                else
                  raise EConversionError.Create(Format(SObjectExpectedForField, [FieldName, jsonFieldVal.ToString]));
              end
          else
            raise EConversionError.Create(Format(SNoConversionForType,
              [GetEnumName(typeInfo(TReverterType), Integer(revAttr.ReverterType))]));
          end
        finally
          revAttr.Free
        end
      else
      begin
        if jsonFieldVal is TJSONNumber then
          SetField(Data, FieldName, jsonFieldVal.ToString)
        else if jsonFieldVal is TJSONString then
          SetField(Data, FieldName, jsonFieldVal.Value)
        else if jsonFieldVal is TJSONTrue then
          SetField(Data, FieldName, true)
        else if jsonFieldVal is TJSONFalse then
          SetField(Data, FieldName, false)
        else if jsonFieldVal is TJSONNull then
          SetFieldNull(Data, FieldName)
        else if jsonFieldVal is TJSONObject then
        begin
          // object...
          if TryCreateObject(ClassTypeOf(Data, FieldName), TJSONObject(jsonFieldVal), LObject) then
            SetField(Data, FieldName, LObject)
          else
            LPopulated := false;
        end
        else if jsonFieldVal is TJSONArray then
          SetFieldArray(Data, FieldName, TJSONArray(jsonFieldVal))
        else
          raise EConversionError.Create(Format(SInvalidJSONFieldType, [FieldName, Data.ClassName]));
      end
    end;
    if LPopulated then
      JsonCustomizer.DoFieldPopulated(Data, GetFieldType(Data, FieldName));
  end
end;

procedure TJSONUnMarshal.RegisterReverter(clazz: TClass; Field: string; func: TObjectReverter);
var
  ReverterEvent: TReverterEvent;
begin
  ReverterEvent := TReverterEvent.Create;
  ReverterEvent.ObjectReverter := func;
  RegisterReverter(clazz, Field, ReverterEvent);
end;

procedure TJSONUnMarshal.RegisterReverter(clazz: TClass; Field: string; func: TStringsReverter);
var
  ReverterEvent: TReverterEvent;
begin
  ReverterEvent := TReverterEvent.Create;
  ReverterEvent.StringsReverter := func;
  RegisterReverter(clazz, Field, ReverterEvent);
end;

procedure TJSONUnMarshal.RegisterReverter(clazz: TClass; Field: string; Reverter: TReverterEvent);
begin
  TMonitor.Enter(FReverters);
  try
    FReverters.AddOrSetValue(ComposeKey(clazz, Field), Reverter);
  finally
    TMonitor.Exit(FReverters);
  end;
end;

procedure TJSONUnMarshal.RegisterReverter(clazz: TClass; Field: string; func: TObjectsReverter);
var
  ReverterEvent: TReverterEvent;
begin
  ReverterEvent := TReverterEvent.Create;
  ReverterEvent.ObjectsReverter := func;
  RegisterReverter(clazz, Field, ReverterEvent);
end;

procedure TJSONUnMarshal.RegisterReverter(clazz: TClass; Field: string; func: TStringReverter);
var
  ReverterEvent: TReverterEvent;
begin
  ReverterEvent := TReverterEvent.Create;
  ReverterEvent.StringReverter := func;
  RegisterReverter(clazz, Field, ReverterEvent);
end;

procedure TJSONUnMarshal.RegisterReverter(clazz: TClass; func: TTypeStringsReverter);
var
  ReverterEvent: TReverterEvent;
begin
  ReverterEvent := TReverterEvent.Create;
  ReverterEvent.TypeStringsReverter := func;
  RegisterReverter(clazz, FIELD_ANY, ReverterEvent);
end;

procedure TJSONUnMarshal.RegisterReverter(clazz: TClass; func: TTypeStringReverter);
var
  ReverterEvent: TReverterEvent;
begin
  ReverterEvent := TReverterEvent.Create;
  ReverterEvent.TypeStringReverter := func;
  RegisterReverter(clazz, FIELD_ANY, ReverterEvent);
end;

procedure TJSONUnMarshal.RegisterReverter(clazz: TClass; func: TTypeObjectsReverter);
var
  ReverterEvent: TReverterEvent;
begin
  ReverterEvent := TReverterEvent.Create;
  ReverterEvent.TypeObjectsReverter := func;
  RegisterReverter(clazz, FIELD_ANY, ReverterEvent);
end;

procedure TJSONUnMarshal.RegisterReverter(clazz: TClass; func: TTypeObjectReverter);
var
  ReverterEvent: TReverterEvent;
begin
  ReverterEvent := TReverterEvent.Create;
  ReverterEvent.TypeObjectReverter := func;
  RegisterReverter(clazz, FIELD_ANY, ReverterEvent);
end;

function TJSONUnMarshal.Reverter(key: string): TReverterEvent;
begin
  TMonitor.Enter(FReverters);
  try
    Exit(FReverters.Items[key]);
  finally
    TMonitor.Exit(FReverters);
  end;
end;

procedure TJSONUnMarshal.SetDateTimeIsUTC(const Value: Boolean);
begin
  if Value <> FDateTimeIsUTC then
  begin
    FDateTimeIsUTC := Value;
  end;
end;

procedure TJSONUnMarshal.SetField(Data: TObject; Field: string; Value: TObject);
var
  LField: TRttiField;
begin
  LField := GetFieldType(Data, Field);
  if LField <> nil then
    LField.SetValue(Data, Value);
end;

procedure TJSONUnMarshal.SetField(Data: TObject; Field, Value: string);
var
  rField: TRttiField;
begin
  rField := GetFieldType(Data, Field);
  // if the field does not exist, then we just ignore it silently
  if rField <> nil then
    case rField.FieldType.TypeKind of
      TTypeKind.tkString, TTypeKind.tkWString, TTypeKind.tkLString, TTypeKind.tkUString, TTypeKind.tkFloat,
        TTypeKind.tkInteger, TTypeKind.tkInt64, TTypeKind.tkChar, TTypeKind.tkWChar, TTypeKind.tkEnumeration:
        rField.SetValue(Data, StringToTValue(Value, rField.FieldType.Handle));
    else
      raise EConversionError.Create(Format(SNoValueConversionForField, [Value, Field, Data.ClassName]));
    end;
end;

procedure TJSONUnMarshal.SetField(Data: TObject; Field: string; Value: Boolean);
var
  LField: TRttiField;
begin
  LField := GetFieldType(Data, Field);
  if LField <> nil then
    LField.SetValue(Data, Value);
end;

procedure TJSONUnMarshal.SetFieldArray(Data: TObject; Field: string; Value: TJSONArray);
var
  rField: TRttiField;
  LValue: TValue;
  LFields: TArray<TRttiField>;
begin
  rField := GetFieldType(Data, Field);
  if rField <> nil then
    case rField.FieldType.TypeKind of
      TTypeKind.tkArray, TTypeKind.tkDynArray:
        rField.SetValue(Data, JSONToTValue(Value, rField.FieldType));
    TTypeKind.tkRecord:
    begin
      LFields := rField.FieldType.GetFields;
      // Special handling for TList<T>.FListHelper.  Unmashal FCount.  Preserve other FTypeInfo, FNotify, FCompare.
      if (rField.Name = 'FListHelper') and
        (Value.Count = 1) and
        (Length(LFields) > 1) and
        (LFields[0].Name = 'FCount')  then
      begin
        LValue := rField.GetValue(Data); // Get FListHelper
        LFields[0].SetValue(LValue.GetReferenceToRawData, JSONToTValue(Value.Items[0], LFields[0].FieldType)); // Update FCount
        rField.SetValue(Data, LValue); // Set FListHelper
      end
      else
        rField.SetValue(Data, JSONToTValue(Value, rField.FieldType));
    end
    else
      raise EConversionError.Create(Format(SInvalidTypeForField, [Field, rField.FieldType.Name]));
    end;
end;

procedure TJSONUnMarshal.SetFieldNull(Data: TObject; Field: string);
var
  LField: TRttiField;
begin
  LField := GetFieldType(Data, Field);
  if LField <> nil then
    LField.SetValue(Data, TValue.Empty);
end;

function TJSONUnMarshal.StringToTValue(Value: string; typeInfo: PTypeInfo): TValue;
var
  vChar: char;
  vWChar: widechar;
  FValue: TValue;
  enumVal: Integer;
  LResultDouble: Double;
begin
  case typeInfo.Kind of
{$IFNDEF NEXTGEN}
    TTypeKind.tkString:
      Exit(TValue.From<ShortString>(ShortString(Value)));
    TTypeKind.tkWString:
      Exit(TValue.From<WideString>(WideString(Value)));
{$ELSE}
    TTypeKind.tkString, TTypeKind.tkWString,
{$ENDIF !NEXTGEN}
    TTypeKind.tkLString, TTypeKind.tkUString:
      Exit(Value);
    TTypeKind.tkFloat:
      if System.Json.TryJsonToFloat(Value, LResultDouble) then
        exit(LResultDouble)
      else
        exit(0.0);
    TTypeKind.tkInteger:
      Exit(StrToIntDef(Value, 0));
    TTypeKind.tkInt64:
      Exit(StrToInt64Def(Value, 0));
    TTypeKind.tkChar:
      begin
        if Value = '' then
          vChar := #0
        else
          vChar := Value.Chars[0];
        TValue.Make(@vChar, typeInfo, FValue);
        Exit(FValue);
      end;
    TTypeKind.tkWChar:
      begin
        if Value = '' then
          vWChar := #0
        else
          vWChar := Value.Chars[0];
        TValue.Make(@vWChar, typeInfo, FValue);
        Exit(FValue);
      end;
    TTypeKind.tkEnumeration:
      begin
        enumVal := GetEnumValue(typeInfo, Value);
        TValue.Make(@enumVal, typeInfo, FValue);
        Exit(FValue);
      end
  else
    raise EConversionError.Create(Format(SNoConversionAvailableForValue, [Value, typeInfo.NameFld.ToString]));
  end;
end;

class function TJSONUnMarshal.TValueToJson(JsonValue: TValue): TJSONValue;
begin
  case JsonValue.Kind of
    tkInteger, tkInt64: result := TJSONNumber.Create(JsonValue.AsInt64);
    tkFloat: result := TJSONNumber.Create(JsonValue.AsExtended);
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString: result := TJSONString.Create(JsonValue.AsString);
    //tkArray, tkDynArray: ;
  else
    begin
       result := TJSONString.Create(JsonValue.ToString);
    end;
  end;
end;

{ TSerStringItem }

constructor TSerStringItem.Create(AString: string; AObject: TObject);
begin
  FString := AString;
  FObject := AObject;
end;

{ TSerStringList }

function TSerStringList.AsStringList: TStringList;
var
  item: TSerStringItem;
begin
  Result := TStringList.Create;
  for item in FSerStringItemList do
    Result.AddObject(item.FString, item.FObject);
  Result.Duplicates := FDuplicates;
  Result.Sorted := FSorted;
  Result.CaseSensitive := FCaseSensitive
end;

constructor TSerStringList.Create(Source: TStringList);
var
  I: Integer;
begin
  SetLength(FSerStringItemList, Source.Count);
  for I := 0 to Source.Count - 1 do
    FSerStringItemList[I] := TSerStringItem.Create(Source[I], Source.Objects[I]);
  FCaseSensitive := Source.CaseSensitive;
  FSorted := Source.Sorted;
  FDuplicates := Source.Duplicates;
end;

destructor TSerStringList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FSerStringItemList) - 1 do
    FSerStringItemList[I].Free;
  inherited;
end;

{ StringListConverter }

function StringListConverter(Data: TObject): TObject;
begin
  if Data = nil then
    Exit(nil);
  Exit(TSerStringList.Create(TStringList(Data)));
end;

{ StringListReverter }

function StringListReverter(Ser: TObject): TObject;
begin
  if Ser = nil then
    Exit(nil);
  try
    Exit(TSerStringList(Ser).AsStringList);
  finally
    Ser.Free;
  end;
end;

function JSONBooleanAttributeValue(rttiObject: TRttiNamedObject; AttributeClass: TClass; DefaultValue: Boolean = false): Boolean;
var
  rttiAttrib: TCustomAttribute;
begin
  for rttiAttrib in rttiObject.GetAttributes do
    if rttiAttrib is AttributeClass then
      Exit(JSONBooleanAttribute(rttiAttrib).Value);
  Exit(DefaultValue);
end;

{ TJSONPopulationCustomizer }

procedure TJSONPopulationCustomizer.PrePopulate(Data: TObject; rttiContext: TRttiContext);
var
  rttiType: TRttiType;
  rttiField: TRttiField;
begin
  // Free any initialized fields before population
  rttiType := rttiContext.GetType(Data.ClassType);
  for rttiField in rttiType.GetFields do
  begin
    if not CanPopulate(Data, rttiField) then
      continue;
    if (rttiField.FieldType <> nil) and (rttiField.FieldType.TypeKind = tkClass) and
      JSONBooleanAttributeValue(rttiField, JSONOwnedAttribute, true)
    // and JSONBooleanAttributeValue(rttiField,JSONMarshalled,true)
      and (rttiField.GetValue(Data).AsObject <> nil) then
      PrePopulateObjField(Data, rttiField);
  end;
end;

procedure TJSONPopulationCustomizer.PrePopulateObjField(Data: TObject; rttiField: TRttiField);
var
  Value: TObject;
begin
  if rttiField <> nil then
  begin
    if not CanPopulate(Data, rttiField) then
      Exit;
    Value := rttiField.GetValue(Data).AsObject;
    Value.Free;
    rttiField.SetValue(Data, TValue.Empty);
  end;
end;

function TJSONPopulationCustomizer.CanPopulate(Data: TObject; rttiField: TRttiField): Boolean;
begin
  if Assigned(FCanPopulate) then
    Result := FCanPopulate(Data, rttiField)
  else
    Result := JSONBooleanAttributeValue(rttiField, JSONMarshalledAttribute, true);
end;

constructor TJSONPopulationCustomizer.Create(ACanPopulate: TJSONCanPopulateProc);
begin
  inherited Create;
  FCanPopulate := ACanPopulate;
end;

procedure TJSONPopulationCustomizer.DoFieldPopulated(Data: TObject; rttiField: TRttiField);
begin
  // No customization by default
end;

procedure TJSONPopulationCustomizer.PostPopulate(Data: TObject);
begin
  // No customization by default
end;

{ TJSONInterceptor }

function TJSONInterceptor.IsTypeConverter: Boolean;
begin
  Result := FConverterType in [ctTypeObjects, ctTypeStrings, ctTypeObject, ctTypeString];
end;

function TJSONInterceptor.IsTypeReverter: Boolean;
begin
  Result := FReverterType in [rtTypeObjects, rtTypeStrings, rtTypeObject, rtTypeString];
end;

function TJSONInterceptor.ObjectConverter(Data: TObject; Field: string): TObject;
begin
  Result := nil;
end;

procedure TJSONInterceptor.ObjectReverter(Data: TObject; Field: string; Arg: TObject);
begin

end;

function TJSONInterceptor.ObjectsConverter(Data: TObject; Field: string): TListOfObjects;
begin
  Result := nil;
end;

procedure TJSONInterceptor.ObjectsReverter(Data: TObject; Field: string; Args: TListOfObjects);
begin

end;

function TJSONInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := EmptyStr;
end;

procedure TJSONInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin

end;

function TJSONInterceptor.StringsConverter(Data: TObject; Field: string): TListOfStrings;
begin
  Result := nil;
end;

procedure TJSONInterceptor.StringsReverter(Data: TObject; Field: string; Args: TListOfStrings);
begin

end;

function TJSONInterceptor.TypeObjectConverter(Data: TObject): TObject;
begin
  Result := nil;
end;

function TJSONInterceptor.TypeObjectReverter(Data: TObject): TObject;
begin
  Result := nil;
end;

function TJSONInterceptor.TypeObjectsConverter(Data: TObject): TListOfObjects;
begin
  Result := nil;
end;

function TJSONInterceptor.TypeObjectsReverter(Data: TListOfObjects): TObject;
begin
  Result := nil;
end;

function TJSONInterceptor.TypeStringConverter(Data: TObject): string;
begin
  Result := EmptyStr;
end;

function TJSONInterceptor.TypeStringReverter(Data: string): TObject;
begin
  Result := nil;
end;

function TJSONInterceptor.TypeStringsConverter(Data: TObject): TListOfStrings;
begin
  Result := nil;
end;

function TJSONInterceptor.TypeStringsReverter(Data: TListOfStrings): TObject;
begin
  Result := nil;
end;

{ TStringListInterceptor }

function TStringListInterceptor.TypeObjectConverter(Data: TObject): TObject;
begin
  Result := StringListConverter(Data);
end;

function TStringListInterceptor.TypeObjectReverter(Data: TObject): TObject;
begin
  Result := StringListReverter(Data);
end;

{ TJSONMarshal }

constructor TJSONMarshal.Create;
begin
  inherited Create(TJSONConverter.Create, true);

  // JSON converters
  RegisterConverter(TJSONObject, 'FMembers', JSONObjectPairListConverter);
  RegisterConverter(TJSONArray, 'FElements', JSONArrayElementsConverter);
  RegisterConverter(TStringBuilder, 'FData', StringBuilderConverter);
end;

constructor TJSONMarshal.Create(Converter: TConverter<TJSONValue>; OwnConverter: Boolean);
begin
  inherited Create(Converter, OwnConverter);

end;

constructor TJSONMarshal.Create(Converter: TConverter<TJSONValue>; OwnConverter: Boolean;
Converters: TObjectDictionary<string, TConverterEvent>);
begin
  inherited Create(Converter, OwnConverter, Converters);

end;

{ TConverters }

class constructor TJSONConverters.Create;
begin
  CFRegConverters := TObjectDictionary<string, TConverterEvent>.Create([doOwnsValues]);
  CFRegReverters := TObjectDictionary<string, TReverterEvent>.Create([doOwnsValues]);
  CFRegMarshal := TDictionary<string, Boolean>.Create;
end;

class destructor TJSONConverters.Destroy;
begin
  FreeAndNil(CFRegMarshal);
  FreeAndNil(CFRegConverters);
  FreeAndNil(CFRegReverters);
end;

class function TJSONConverters.GetJSONMarshaler: TJSONMarshal;
var
  LKey: string;
begin
  Result := TJSONMarshal.Create(TJSONConverter.Create, true, CFRegConverters);
  TMonitor.Enter(CFRegMarshal);
  try
    for LKey in CFRegMarshal.Keys do
      Result.RegisterJSONMarshalled(LKey, CFRegMarshal.Items[LKey]);
  finally
    TMonitor.Exit(CFRegMarshal);
  end;
  // add JSON converters
  Result.RegisterConverter(TJSONObject, 'FMembers', JSONObjectPairListConverter);
  Result.RegisterConverter(TJSONArray, 'FElements', JSONArrayElementsConverter);
  Result.RegisterConverter(TStringBuilder, 'FData', StringBuilderConverter);
end;

class function TJSONConverters.GetJSONUnMarshaler: TJSONUnMarshal;
var
  LKey: string;
begin
  Result := TJSONUnMarshal.Create(CFRegReverters);
  TMonitor.Enter(CFRegMarshal);
  try
    for LKey in CFRegMarshal.Keys do
      Result.RegisterJSONMarshalled(LKey, CFRegMarshal.Items[LKey]);
  finally
    TMonitor.Exit(CFRegMarshal);
  end;
  // add JSON reverters
  Result.RegisterReverter(TJSONObject, 'FMembers', JSONObjectPairListReverter);
  Result.RegisterReverter(TJSONArray, 'FElements', JSONArrayElementsReverter);
  Result.RegisterReverter(TStringBuilder, 'FData', StringBuilderReverter);
end;

class procedure TJSONConverters.AddConverter(event: TConverterEvent);
begin
  TMonitor.Enter(CFRegConverters);
  try
    CFRegConverters.Add(TJSONMarshal.ComposeKey(event.FieldClassType, event.FieldName), event);
  finally
    TMonitor.Exit(CFRegConverters);
  end;
end;

class procedure TJSONConverters.AddMarshalFlag(AClass: TClass; AField: string; Marshal: Boolean);
begin
  TMonitor.Enter(CFRegMarshal);
  try
    CFRegMarshal.AddOrSetValue(TJSONMarshal.ComposeKey(AClass, AField), Marshal);
  finally
    TMonitor.Exit(CFRegMarshal);
  end;
end;

class procedure TJSONConverters.ClearMarshalFlag(AClass: TClass; AField: string);
var
  LKey: string;
begin
  TMonitor.Enter(CFRegMarshal);
  try
    LKey := TJSONMarshal.ComposeKey(AClass, AField);
    if CFRegMarshal.ContainsKey(LKey) then
      CFRegMarshal.Remove(LKey);
  finally
    TMonitor.Exit(CFRegMarshal);
  end;
end;

class procedure TJSONConverters.AddReverter(event: TReverterEvent);
begin
  TMonitor.Enter(CFRegReverters);
  try
    CFRegReverters.Add(TJSONMarshal.ComposeKey(event.FieldClassType, event.FieldName), event);
  finally
    TMonitor.Exit(CFRegReverters);
  end;
end;

{ TMarshalUnmarshalBase }

constructor TMarshalUnmarshalBase.Create;
begin
  inherited Create;
  FMarshalled := TDictionary<string, Boolean>.Create;
end;

destructor TMarshalUnmarshalBase.Destroy;
begin
  FMarshalled.Free;
  inherited;
end;

class function TMarshalUnmarshalBase.ComposeKey(clazz: TClass; Field: string): string;
begin
  if clazz <> nil then
    Result := clazz.UnitName + SEP_DOT + clazz.ClassName + SEP_DOT + Field
  else
    Result := '';
end;

procedure TMarshalUnmarshalBase.RegisterJSONMarshalled(AComposeKey: string; Marshal: Boolean);
begin
  FMarshalled.AddOrSetValue(AComposeKey, Marshal);
end;

procedure TMarshalUnmarshalBase.RegisterJSONMarshalled(clazz: TClass; Field: string; Marshal: Boolean);
begin
  FMarshalled.AddOrSetValue(ComposeKey(clazz, Field), Marshal);
end;

procedure TMarshalUnmarshalBase.UnregisterJSONMarshalled(clazz: TClass; Field: string);
var
  LKey: string;
begin
  LKey := ComposeKey(clazz, Field);
  if FMarshalled.ContainsKey(LKey) then
    FMarshalled.Remove(LKey);
end;

function TMarshalUnmarshalBase.ShouldMarshal(Data: TObject; rttiField: TRttiField): Boolean;
var
  LKey: string;
begin
  Assert(Data <> nil);
  // Under ARC there is always a refCount field which we DO NOT WANT serialized
  if rttiField.Name = 'FRefCount' then
  begin
    Result := false;
  end
  else
  begin
    LKey := ComposeKey(Data.ClassType, rttiField.Name);
    if FMarshalled.ContainsKey(LKey) then
      Exit(FMarshalled.Items[LKey]);
    Result := JSONBooleanAttributeValue(rttiField, JSONMarshalledAttribute, true);
  end;
end;

{ TInternalJSONPopulationCustomizer }

procedure TInternalJSONPopulationCustomizer.Cleanup;
begin
  FBackupCache.Clear;
end;

constructor TInternalJSONPopulationCustomizer.Create(ACanPopulate: TJSONCanPopulateProc);
begin
  inherited Create(ACanPopulate);
  FBackupCache := TObjectDictionary<TRttiField, TObject>.Create([doOwnsValues]);
end;

destructor TInternalJSONPopulationCustomizer.Destroy;
begin
  FBackupCache.Free;
  inherited;
end;

procedure TInternalJSONPopulationCustomizer.DoFieldPopulated(Data: TObject; rttiField: TRttiField);
begin
  if FBackupCache.ContainsKey(rttiField) then
    FBackupCache.Remove(rttiField);
end;

procedure TInternalJSONPopulationCustomizer.PostPopulate(Data: TObject);
var
  LRttiField: TRttiField;
  LPair: TPair<TRttiField, TObject>;
begin
  for LRttiField in FBackupCache.Keys do
  begin
    Assert(LRttiField.GetValue(Data).AsObject = nil);
    LPair := FBackupCache.ExtractPair(LRttiField);
    LPair.key.SetValue(Data, TValue.From<TObject>(LPair.Value));
  end;
  Cleanup;
end;

procedure TInternalJSONPopulationCustomizer.PrePopulateObjField(Data: TObject; rttiField: TRttiField);
begin
  if rttiField <> nil then
  begin
    FBackupCache.AddOrSetValue(rttiField, rttiField.GetValue(Data).AsObject);
    rttiField.SetValue(Data, TValue.Empty);
  end;
end;

{ JSONReflect }

constructor JsonReflectAttribute.Create(IsMarshalOwned: Boolean);
begin
  FMarshalOwner := IsMarshalOwned;
  inherited Create;
end;

constructor JsonReflectAttribute.Create(ConverterType: TConverterType; ReverterType: TReverterType; InterceptorType: TClass;
PopulationCustomizerType: TClass; IsMarshalOwned: Boolean);
begin
  FMarshalOwner := IsMarshalOwned;
  FReverterType := ReverterType;
  FConverterType := ConverterType;
  FInterceptor := InterceptorType;
  FPopulationCustomizer := PopulationCustomizerType;
  inherited Create;
end;

function JsonReflectAttribute.JSONInterceptor: TJSONInterceptor;
begin
  Result := FInterceptor.NewInstance as TJSONInterceptor;
  Result.ConverterType := FConverterType;
  Result.ReverterType := FReverterType;
end;

function JsonReflectAttribute.JSONPopulationCustomizer: TJSONPopulationCustomizer;
begin
  if FPopulationCustomizer <> nil then
    Result := FPopulationCustomizer.NewInstance as TJSONPopulationCustomizer
  else
    Result := nil;
end;




end.


