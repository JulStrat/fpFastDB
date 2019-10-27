{-< FastDbVar.pas >------------------------------------------------*
  FastDbSession Version 1.0
    (c) 2002 Serge Aleynikov (serge@hq.idt.net)
  Main Memory Database Management System
  Created:     11/11/2002 Serge Aleynikov (serge@hq.idt.net)
  Last update:
    9/12/2003  Fixed a problem with TFastDbField.Assign() missing
               indexes.
    5/11/2003  Fixed a problem with GetArrayAsSingle/Double fetching
               wrong data from array and occasuonally causing index
               out of bounds error.
    5/09/2003  Fixed a compilation issue on Linux/Kylix3
               It turns out that Kylix3 doesn't like Pointer of Object
               declarations. 
    5/8/2003   Added TFieldBufferItem.FetchData property to control
               conditional fetching of Array/inverseRef fields
               during selects.
               Added TFieldBufferItem.Capacity property to be able
               to reserve a large pool of memory for speeding up
               inverse reference array copying without requiring
               to reallocate memory.
    4/2/2003   Fixed an AV bug in
               Fields[4].asArraySingle[]
               Fields[4].asArrayInteger[]
    12/06/2002 Fixed the field type conversion in BindToStatement()
               which caused the error in working with ctDateTime
               field type.
-------------------------------------------------------------------*
  Field structures for column/parameter binding
-------------------------------------------------------------------*}
unit FastDbVar;
{$ifdef FPC}{$mode delphi}{$endif}

{$I FastDbConfig.inc}

interface
uses
  FastDbCLI,
  TypInfo,
  Classes,
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils;

const
  FastDbUnilitializedHandle = -1;  // Value assigned to session/statement/field handles

  DATE_FORMAT_STRING        = 'mm/dd/yyyy hh:nn:ss';
  {$IFDEF MSWINDOWS}
  EOL   = #10;
  {$ELSE}
  EOL   = #13#10;
  {$ENDIF}

type
{$Align Off}
  TProcedureOfObjectThunk = record
    PopEAX:    Byte;
    PushImm:   Byte; InstPtr: Pointer;
    PushEAX:   Byte;
    MovEAXImm: Byte; ProcPtr: Pointer;
    JmpEAX:    Word;
  end;
{$Align On}

  TFieldBufferItem    = class;
  TArrayFieldSetValue = function(Sender: TFieldBufferItem;
                                 DestinationPtr: Pointer;
                                 const SourcePtr: Pointer;
                                 Len: Integer;
                                 const Statement: Integer
                                //): Pointer of Object;  // Note: for some reason Kylix3 compliler crashes when it sees declaration of Pointer of Object
                                ): Integer of Object;
  TArrayFieldGetValue = function(Sender: TFieldBufferItem;
                                 DestinationPtr: Pointer;
                                 var Len: Integer;
                                 const Statement: Integer
                                //): Pointer of Object;  // Note: for some reason Kylix3 compliler crashes when it sees declaration of Pointer of Object
                                ): Integer of Object;

  TFieldBufferItem=class(TCollectionItem)
  private
    FPData           : Pointer;
    FData            : array of Char;
    FDataSize        : Integer;
    FCapacity        : Integer;
    FName            : string;
    FFieldType       : TCliVarType;
    FBoundToStatement: Integer;
    FDateFormat      : string;
    FFetchData       : Boolean;
    FOnArraySetValue : TArrayFieldSetValue;
    FOnArrayGetValue : TArrayFieldGetValue;

    function  GetFieldTypeName: string;
    function  GetFieldSize: Integer;
    procedure SetFieldSize(const Value: Integer);

    function  GetAsPointer: Pointer;
    function  GetAsBoolean: Boolean;
    function  GetAsInteger(const Index: Integer): Integer;
    function  GetAsInt64: TCliInt8;
    function  GetAsSingle: Single;
    function  GetAsDouble(const Index: Integer): Double;
    function  GetAsString: string;

    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsInteger(const Index, Value: Integer);
    procedure SetAsSingle(const Value: Single);
    procedure SetAsDouble(const Index: Integer; const Value: Double);
    procedure SetAsString(const Value: string); virtual;
    procedure SetAsInt64(const Value: TCliInt8);

    procedure SetFieldType(Value: TCliVarType); virtual;
    procedure SetCapacity(const Value: Integer);
  protected
    procedure SetBufferTypeAndSize(const NewType: TCliVarType; const NewSize: Integer=0; const CopyExact: Boolean=False);
    function  GetDisplayName: string; override;
    property  OnArraySetValue: TArrayFieldSetValue read FOnArraySetValue write FOnArraySetValue;
    property  OnArrayGetValue: TArrayFieldGetValue read FOnArrayGetValue write FOnArrayGetValue;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure  Assign(Source: TPersistent); override;
    procedure  ReadFromStream(Stream: TStream); overload; virtual;
    procedure  ReadFromStream(Stream: TStream; const AJustData: Boolean); overload; virtual;
    procedure  WriteToStream(Stream: TStream); overload; virtual;
    procedure  WriteToStream(Stream: TStream; const AJustData: Boolean); overload; virtual;

    procedure FreeBuffer;
    property  FieldSize: Integer              read GetFieldSize;
    property  FieldTypeName: string           read GetFieldTypeName;
    procedure SetValue(const AValue: Pointer; const Size: Integer; const CopyExact: Boolean=False);
    procedure ClearValue;
    function  BindToStatement(const AStatement: Integer): Boolean; virtual;
    procedure UnBindFromStatement;
    procedure CopyTypeAndValue(FromField: TFieldBufferItem);

    property asByte:    Integer  index 1 read GetAsInteger write SetAsInteger;
    property asOID:     Integer  index 2 read GetAsInteger write SetAsInteger;
    property asInteger: Integer  index 3 read GetAsInteger write SetAsInteger;
    property asInt8:    TCliInt8         read GetAsInt64   write SetAsInt64;
    property asString:  string           read GetAsString  write SetAsString;
    property asSingle:  Single           read GetAsSingle  write SetAsSingle;
    property asDouble:  Double   index 1 read GetAsDouble  write SetAsDouble;
    property asDateTime:Double   index 2 read GetAsDouble  write SetAsDouble;
    property asPointer: Pointer          read GetAsPointer;
    property asBoolean: Boolean          read GetAsBoolean write SetAsBoolean;
  published
    property FieldType: TCliVarType  read FFieldType  write SetFieldType;
    property Name: string            read FName       write FName;
    property DateFormat: string      read FDateFormat write FDateFormat;
    property Capacity:  Integer      read FCapacity   write SetCapacity;
    property FetchData: Boolean      read FFetchData  write FFetchData;
  end;

  TFastDbField=class(TFieldBufferItem)
  private
    FRefTable       : string;
    FInverseRefField: string;
    FIndexType     : TIndexTypes;

    procedure SetFieldFlags(const Value: Integer);
    function  GetFieldFlags: Integer;
    function  GetArraySize: Integer;
    procedure SetArraySize(const Value: Integer);

    function  GetArrayAsBoolean(Idx: Integer): Boolean;
    function  GetArrayAsInteger(Idx: Integer; Index: Integer): Integer;
    function  GetArrayAsInt64(Idx: Integer): TCliInt8;
    function  GetArrayAsSingle(Idx: Integer): Single;
    function  GetArrayAsDouble(Idx: Integer; Index: Integer): Double;
    function  GetArrayAsString(Idx: Integer): string;

    procedure SetArrayAsBoolean(Idx: Integer; const Value: Boolean);
    procedure SetArrayAsInteger(Idx: Integer; Index: Integer; const Value: Integer);
    procedure SetArrayAsInt64(Idx: Integer; const Value: TCliInt8);
    procedure SetArrayAsSingle(Idx: Integer; const Value: Single);
    procedure SetArrayAsDouble(Idx: Integer; Index: Integer; const Value: Double);
    procedure SetArrayAsString(Idx: Integer; const Value: string);
  public
    function BindToStatement(const AStatement: Integer): Boolean; override;
    property FieldFlags: Integer read GetFieldFlags write SetFieldFlags;
    property ArraySize: Integer  read GetArraySize  write SetArraySize;
    procedure Assign(Source: TPersistent); override;

    property asArrayByte[Idx: Integer]:    Integer  index 1 read GetArrayAsInteger write SetArrayAsInteger;
    property asArrayOID[Idx: Integer]:     Integer  index 2 read GetArrayAsInteger write SetArrayAsInteger;
    property asArrayInteger[Idx: Integer]: Integer  index 3 read GetArrayAsInteger write SetArrayAsInteger;
    property asArrayInt8[Idx: Integer]:    TCliInt8         read GetArrayAsInt64   write SetArrayAsInt64;
    property asArrayString[Idx: Integer]:  string           read GetArrayAsString  write SetArrayAsString;
    property asArraySingle[Idx: Integer]:  Single           read GetArrayAsSingle  write SetArrayAsSingle;
    property asArrayDouble[Idx: Integer]:  Double   index 1 read GetArrayAsDouble  write SetArrayAsDouble;
    property asArrayDateTime[Idx: Integer]:Double   index 2 read GetArrayAsDouble  write SetArrayAsDouble;
    property asArrayBoolean[Idx: Integer]: Boolean          read GetArrayAsBoolean write SetArrayAsBoolean;
  published
    property IndexType: TIndexTypes  read FIndexType       write FIndexType;
    property RefTable: string        read FRefTable        write FRefTable;
    property InverseRefField: string read FInverseRefField write FInverseRefField;
    property OnArraySetValue;
    property OnArrayGetValue;
  end;

  TFastDbVariable=class(TFieldBufferItem)
  private
    procedure SetAsString(const Value: string); override;
  public
    // OnSubstitutedValueChange is for internal use to notify a Query component that
    // SQL changed.
    function GetSubstitudeValue: string;
    function DisplayString(AQuery: TObject): string;
    function BindToStatement(const AStatement: Integer): Boolean; override;
  end;

  TFieldBufferCollection = class(TOwnedCollection)
  protected
    function Add(const AName: string; AType: TCliVarType): TFieldBufferItem;
  public
    procedure Delete(const AName: string);
    procedure UnBindFromStatement;
    function  asText: string;
    procedure ClearValues;
    procedure ReadFromStream(Stream: TStream); virtual;
    procedure WriteToStream(Stream: TStream); virtual;
  end;

  TFastDbFields = class(TFieldBufferCollection)
  private
    function  GetField(Index: Integer): TFastDbField;
    procedure SetField(Index: Integer; const Value: TFastDbField);
  public
    constructor Create(AOwner: TComponent);
    function Add(const AName: string;
                 const AType: TCliVarType;
                 const AIndexType: TIndexTypes=[];
                 const ARefTable: string='';
                 const AInverseRefField: string=''): TFastDbField;
    property Fields[Index: Integer]: TFastDbField read GetField write SetField; default;
  end;

  TFastDbVariables = class(TFieldBufferCollection)
  private
    function  GetVariable(Index: Integer): TFastDbVariable;
    procedure SetVariable(Index: Integer; const Value: TFastDbVariable);
  public
    constructor Create(AOwner: TComponent);
    function Add(const AName: string; const AType: TCliVarType; AValue: Pointer=nil): TFastDbVariable;
    property Variables[Index: Integer]: TFastDbVariable read GetVariable write SetVariable; default;
  end;

  function CreateProcedureOfObjectThunk (Instance, Proc: Pointer): TProcedureOfObjectThunk;
  function CliTypeToOrd(const CliType: TCliVarType): Integer;
  function OrdToCliType(const CliOrdType: Integer): TCliVarType;
  function CliVarTypeAsStr(const CliType: TCliVarType; const ExtendedSyntax: Boolean=False): string;
  function FieldFlagsToIndexTypes(const FieldFlags: Integer): TIndexTypes;
  function IndexTypesToFieldFlags(const IndexTypes: TIndexTypes): Integer;
  function FastDbFieldToFieldDescriptor(Field: TFastDbField): TCliFieldDescriptor;

  function IsArrayType(const CliType: TCliVarType): Boolean;
  function iif(b: Boolean; true, false: string): string;

implementation
  uses FastDbSession, FastDbQuery;

  //function GetValueAsStr(const CliType: TCliVarType; Value: Pointer): string;

//---------------------------------------------------------------------------
function iif(b: Boolean; true, false: string): string;
begin
  if b then Result := true else Result := false;
end;

//---------------------------------------------------------------------------
function CreateProcedureOfObjectThunk (Instance, Proc: Pointer): TProcedureOfObjectThunk;
begin
  with result do begin
    PopEAX    := $58;                      { Pop EAX }
    PushImm   := $68;  InstPtr:= Instance; { Push Instance }
    PushEAX   := $50;                      { Push EAX }
    MovEAXImm := $B8;  ProcPtr:= Proc;     { Mov EAX, Proc }
    JmpEAX    := $E0FF;                    { Jmp EAX }
  end
end;

//---------------------------------------------------------------------------
function CliVarTypeAsStr(const CliType: TCliVarType; const ExtendedSyntax: Boolean=False): string;
var
  ct : TCliVarType;
begin
  Result := '';
  ct := CliType;
  if ExtendedSyntax and (CliType in [ctArrayOfOID..ctArrayOfString]) then
    begin
      Result := 'array of ';
      ct := TCliVarType(Ord(ct) - Ord(ctArrayOfOID));
    end;

  case ct of
    ctOID          : Result := Result + iif(ExtendedSyntax, 'reference', '(oid)');
    ctBOOL         : Result := Result + iif(ExtendedSyntax, 'int1', 'Boolean');
    ctInt1         : Result := Result + iif(ExtendedSyntax, 'int1', 'Byte');
    ctInt2         : Result := Result + iif(ExtendedSyntax, 'int2', 'SmallInt');
    ctAutoInc,
    ctInt4         : Result := Result + iif(ExtendedSyntax, 'int4', 'Integer');
    ctInt8         : Result := Result + iif(ExtendedSyntax, 'int8', 'Int64');
    ctReal4        : Result := Result + iif(ExtendedSyntax, 'real4', 'Single');
    ctDateTime     : Result := Result + iif(ExtendedSyntax, 'real8', 'DateTime');
    ctReal8        : Result := Result + iif(ExtendedSyntax, 'real8', 'Double');
    ctString       ,
    ctPString      : Result := Result + iif(ExtendedSyntax, 'string', 'String');
  else
    if not ExtendedSyntax then
      begin
        Result := GetEnumName(TypeInfo(TCliVarType), Ord(ct));
        Delete(Result, 1, 2);
      end;
  end;
end;

//---------------------------------------------------------------------------
function IsArrayType(const CliType: TCliVarType): Boolean;
begin
  Result := not (CliType in [ctOID..ctPString, ctDateTime, ctSubst]);
end;

//---------------------------------------------------------------------------
function CliTypeToOrd(const CliType: TCliVarType): Integer;
begin
  case CliType of
    ctDateTime : Result := Ord(ctReal8);
    ctString   : Result := Ord(ctPString);
  else
    Result := Ord(CliType);
  end;
end;

//---------------------------------------------------------------------------
function OrdToCliType(const CliOrdType: Integer): TCliVarType;
begin
  case CliOrdType of
    cli_asciiz : Result := ctPString;
  else
    Result := TCliVarType(CliOrdType);
  end;
end;

//---------------------------------------------------------------------------
function FieldFlagsToIndexTypes(const FieldFlags: Integer): TIndexTypes;
begin
  Result := [];
  if (FieldFlags and cli_hashed)  > 0             then Include(Result, itHash);
  if (FieldFlags and cli_indexed) > 0             then Include(Result, itTree);
  {$IFDEF GIGABASE}
  if (FieldFlags and cli_case_insensitive) > 0    then Include(Result, itCaseInsensitive);
  if (FieldFlags and cli_unique) > 0              then Include(Result, itUnique);
  if (FieldFlags and cli_optimize_duplicates) > 0 then Include(Result, itOptimizeDuplicates);
  {$ENDIF}
end;

//---------------------------------------------------------------------------
function IndexTypesToFieldFlags(const IndexTypes: TIndexTypes): Integer;
begin
  Result := 0;
  if itHash in IndexTypes               then Result := cli_hashed;
  if itTree in IndexTypes               then Result := Result or cli_indexed;
  {$IFDEF GIGABASE}
  if itCaseInsensitive in IndexTypes    then Result := Result or cli_case_insensitive;
  if itUnique in IndexTypes             then Result := Result or cli_unique;
  if itOptimizeDuplicates in IndexTypes then Result := Result or cli_optimize_duplicates;
  {$ENDIF}
end;

//---------------------------------------------------------------------------
function FastDbFieldToFieldDescriptor(Field: TFastDbField): TCliFieldDescriptor;
begin
  with Result do begin
    FieldType           := CliTypeToOrd(Field.FieldType);
    Flags               := Field.FieldFlags;
    name                := PChar(Field.Name);
    if Field.RefTable = '' then
      refTableName := nil
    else
      refTableName := PChar(Field.RefTable);
    if Field.InverseRefField = '' then
      inverseRefFieldName := nil
    else
      inverseRefFieldName := PChar(Field.InverseRefField);
  end;
end;

//---------------------------------------------------------------------------
function GetValueAsStr(const CliType: TCliVarType; Value: Pointer): string;
begin
  case CliType of
    ctOID          : Result := IntToStr(TCliOid(Value^));
    ctBOOL         : if Byte(Value^) <> 0 then Result := '1' else Result := '0';
    ctInt1         : Result := IntToStr(Byte(Value^));
    ctInt2         : Result := IntToStr(SmallInt(Value^));
    ctAutoInc,
    ctInt4         : Result := IntToStr(TCliInt4(Value^));
    ctInt8         : Result := IntToStr(TCliInt8(Value^));
    ctReal4        : Result := FloatToStr(Single(Value^));
    ctDateTime     : Result := FormatDateTime('mm/dd/yyyy hh:nn:ss', TDateTime(Value^));
    ctReal8        : Result := FloatToStr(Double(Value^));
    ctString       : Result := PChar(Value^);
    ctPString      : Result := PChar(Pointer(Value^)^);
  else
    Result := '';
  end;
end;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
function GetFieldColumn(const ColumnType: Integer;
  varPtr: Pointer; var Len: Integer; const ColumnName: PChar;
  const Statement: Integer; const UserData: Pointer): Pointer; cdecl;
var
  fld : TFieldBufferItem absolute UserData;
begin
  Assert(fld <> nil, 'UserData must be assigned a value of TFieldBufferItem object!');
  if Assigned(fld.OnArrayGetValue) then
    Result := Pointer(fld.OnArrayGetValue(fld, varPtr, Len, statement))
  else if IsArrayType(fld.FieldType) then
    begin
      Len    := fld.FieldSize div SizeOfCliType[TCliVarType(Ord(fld.FieldType) - Ord(ctArrayOfOID))];
      Result := fld.asPointer;
    end
  else if fld.FieldType in [ctString, ctPString] then
    begin
      Len    := fld.FieldSize-1;
      Result := fld.asPointer;
    end
  else
    begin
      Len    := fld.FieldSize;
      Result := fld.asPointer;
    end;
end;

//---------------------------------------------------------------------------
function SetFieldColumn(const ColumnType: Integer;
  varPtr: Pointer; Len: Integer; const ColumnName: PChar;
  const Statement: Integer; const SourcePtr: Pointer;
  const UserData: Pointer): Pointer; cdecl;
var
  fld : TFieldBufferItem absolute UserData;
begin
  Assert(fld <> nil, 'UserData must be assigned a value of TFieldBufferItem object!');
  if Assigned(fld.OnArraySetValue) then
    Result := Pointer(fld.OnArraySetValue(fld, varPtr, SourcePtr, Len, statement))
  else if fld.FieldType in [ctString, ctPString] then
    begin
      fld.SetFieldSize(len);
      Result := PPChar(fld.asPointer)^;
    end
  else if IsArrayType(fld.FieldType) then
    if fld.FetchData then
      begin
        fld.SetFieldSize(len*SizeOfCliType[TCliVarType(Ord(fld.FieldType) - Ord(ctArrayOfOID))]);
        //SetValue(varPtr, Len);  // FastDB copies data automatically
        Result := fld.asPointer;
      end
    else
      Result := nil               // FastDB won't fetch a field if we return nil
  else // sanity check
    raise EFastDbError.Create(cli_unsupported_type);
end;

//---------------------------------------------------------------------------
// TFastDbField
//---------------------------------------------------------------------------
constructor TFieldBufferItem.Create(Collection: TCollection);
begin
  inherited;
  FFieldType := ctUnknown;
  FFetchData := True;
  FBoundToStatement := FastDbUnilitializedHandle;  // Note: since 0 is a valid statement we initialize this to a negative value
end;

//---------------------------------------------------------------------------
destructor TFieldBufferItem.Destroy;
begin
  FreeBuffer;
  inherited;
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.Assign(Source: TPersistent);
begin
  if Source is TFieldBufferItem then
    begin
      FBoundToStatement := TFieldBufferItem(Source).FBoundToStatement;
      //FBoundToStatement := FastDbUnilitializedHandle;
      CopyTypeAndValue(TFieldBufferItem(Source));
    end
  else
    inherited Assign(Source);
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.CopyTypeAndValue(FromField: TFieldBufferItem);
begin
  FName             := FromField.FName;
  //FBoundToStatement := TFieldBufferItem(Source).FBoundToStatement;
  //FBoundToStatement := FastDbUnilitializedHandle;
  FDateFormat       := FromField.FDateFormat;
  FOnArraySetValue  := FromField.FOnArraySetValue;
  FOnArrayGetValue  := FromField.FOnArrayGetValue;

  SetBufferTypeAndSize(FromField.FFieldType, FromField.FieldSize, True);

  SetValue(FromField.FData, FromField.FieldSize, True);
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.FreeBuffer;
begin
  FData := nil;
  FDataSize := 0;
end;

//---------------------------------------------------------------------------
function TFieldBufferItem.GetAsBoolean: Boolean;
begin
  Result := GetAsInt64 <> 0;
end;

//---------------------------------------------------------------------------
function TFieldBufferItem.GetAsDouble(const Index: Integer): Double;
begin
  case FFieldType of
    ctOID,
    ctInt4:     Result := PCliInt4(FData)^;
    ctBOOL,
    ctInt1:     Result := PByte(FData)^;
    ctInt2:     Result := PCliInt2(FData)^;
    ctInt8:     Result := PCliInt8(FData)^;
    ctReal4:    Result := PCliReal4(FData)^;
    ctDateTime,
    ctReal8:    Result := PCliReal8(FData)^;
    ctString:   begin
                  Assert(FData <> nil, 'String not assigned!');
                  if not TryStrToFloat(PChar(FData), Result) then Result := 0;
                end;
    ctPString:  begin
                  Assert(FPData <> nil, 'String not assigned!');
                  if not TryStrToFloat(PChar(FPData^), Result) then Result := 0;
                end;
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
function TFieldBufferItem.GetAsInt64: Int64;
begin
  case FFieldType of
    ctOID,
    ctInt4:     Result := PCliInt4(FData)^;
    ctBOOL,
    ctInt1:     Result := PByte(FData)^;
    ctInt2:     Result := PCliInt2(FData)^;
    ctInt8:     Result := PCliInt8(FData)^;
    ctReal4:    Result := Trunc(PCliReal4(FData)^);
    ctDateTime,
    ctReal8:    Result := Trunc(PCliReal8(FData)^);
    ctString:   begin
                  Assert(FData <> nil, 'String not assigned!');
                  if not TryStrToInt64(PChar(FData), Result) then Result := 0;
                end;
    ctPString:  begin
                  Assert(FPData <> nil, 'String not assigned!');
                  if not TryStrToInt64(PChar(FPData^), Result) then Result := 0;
                end;
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
function TFieldBufferItem.GetAsInteger(const Index: Integer): Integer;
begin
  Result := GetAsInt64;
end;

//---------------------------------------------------------------------------
function TFieldBufferItem.GetAsPointer: Pointer;
begin
  if FFieldType in [ctString, ctPString] then
    Result := FPData
  else
    Result := FData;
end;

//---------------------------------------------------------------------------
function TFieldBufferItem.GetAsSingle: Single;
begin
  Result := GetAsDouble(1);
end;

//---------------------------------------------------------------------------
function TFieldBufferItem.GetAsString: string;
begin
  case FFieldType of
    ctBOOL:     if GetAsInt64 <> 0 then Result := 'True' else Result := 'False';
    ctOID,
    ctInt8,
    ctInt4,
    ctInt1,
    ctInt2:     Result := IntToStr(GetAsInt64);
    ctDateTime: if FDateFormat <> '' then
                  Result := FormatDateTime(FDateFormat, GetAsDouble(2))
                else
                  Result := FormatDateTime(DATE_FORMAT_STRING, GetAsDouble(2));
    ctReal4,
    ctReal8:    Result := FloatToStr(GetAsDouble(2));
    ctString:   Result := PChar(FData);
    ctPString:  begin
                  if FPData = nil then
                    Result := ''
                  else
                    Result := string(Pointer(FPData^));
                end;
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.SetAsDouble(const Index: Integer; const Value: Double);
begin
  case FFieldType of
    ctOID,
    ctInt4:      PCliInt4(FData)^ := Trunc(Value);
    ctBOOL,
    ctInt1:      PByte(FData)^ := Trunc(Value);
    ctInt2:      PCliInt2(FData)^ := Trunc(Value);
    ctInt8:      PCliInt8(FData)^ := Trunc(Value);
    ctReal4:     PCliReal4(FData)^ := Value;
    ctDateTime,
    ctReal8:     PCliReal8(FData)^ := Value;
    ctString,
    ctPString:   SetAsString(FloatToStr(Value));
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.SetAsInt64(const Value: Int64);
begin
  case FFieldType of
    ctOID,
    ctInt4:      PCliInt4(FData)^  := Value;
    ctBOOL,
    ctInt1:      PByte(FData)^     := Value;
    ctInt2:      PCliInt2(FData)^  := Value;
    ctInt8:      PCliInt8(FData)^  := Value;
    ctReal4:     PCliReal4(FData)^ := Value;
    ctDateTime,
    ctReal8:     PCliReal8(FData)^ := Value;
    ctString,
    ctPString:   SetAsString(FloatToStr(Value));
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.SetAsString(const Value: string);
var s : string;
begin
  case FFieldType of
    ctOID,
    ctInt4:      PCliInt4(FData)^  := StrToInt(Value);
    ctInt1:      PByte(FData)^     := StrToInt(Value);
    ctInt2:      PCliInt2(FData)^  := StrToInt(Value);
    ctInt8:      PCliInt8(FData)^  := StrToInt(Value);
    ctBOOL:      begin
                   s := Trim(Value); if s <> '' then s := UpCase(s[1]) else s := ' ';
                   PByte(FData)^ := Ord(s[1] in ['T', 'Y', '1']);
                 end;
    ctReal4:     PCliReal4(FData)^ := StrToFloat(Value);
    ctDateTime,
    ctReal8:     PCliReal8(FData)^ := StrToFloat(Value);
    ctString,
    ctPString:   begin
                   SetBufferTypeAndSize(FFieldType, Length(Value));
                   Move(Value[1], FData[0], Length(Value)+1);
                   //FPData := @FData;
                 end;
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.ClearValue;
begin
  case FFieldType of
    ctOID..ctReal8, ctAutoInc, ctDateTime:
      SetAsInt64(0);
    ctSubst,
    ctString,
    ctPString,
    ctArrayOfOID..ctArrayOfString:
      SetBufferTypeAndSize(FFieldType, 0);
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.SetFieldType(Value: TCliVarType);
begin
  if FFieldType <> Value then
    SetBufferTypeAndSize(Value);
end;

//---------------------------------------------------------------------------
function TFieldBufferItem.GetFieldSize: Integer;
begin
  Result := FDataSize; // Length(FData);
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.SetCapacity(const Value: Integer);
begin
  if Value <> FCapacity then
    begin
      FCapacity := Value;
      SetLength(FData, FCapacity);
      FPData := @FData;
    end;
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.SetBufferTypeAndSize(const NewType: TCliVarType;
  const NewSize: Integer; const CopyExact: Boolean);
var n : Integer;
begin
  if CopyExact or IsArrayType(NewType) then
    n := NewSize
  else if NewType in [ctString, ctPString] then
    n := NewSize+1
  else
    n := SizeOfCliType[NewType];

  if (FCapacity <> 0) then
    begin
      if n > FCapacity then
        begin
          SetLength(FData, n);
          FCapacity := n;
          FPData := @FData;
        end
      // Note: if n <= FCapacity then the FData buffer was set by the SetCapacity() handler  
    end
  else
    if FDataSize <> n then
      begin
        SetLength(FData, n);
        FPData := @FData;
      end;

  FDataSize := n;

  if FFieldType <> NewType then
    FFieldType := NewType;
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.SetAsBoolean(const Value: Boolean);
begin
  SetAsInteger(4, Ord(Value));
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.SetAsInteger(const Index, Value: Integer);
begin
  SetAsInt64(Value);
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.SetAsSingle(const Value: Single);
begin
  SetAsDouble(1, Value);
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.SetFieldSize(const Value: Integer);
begin
  SetBufferTypeAndSize(FFieldType, Value);
end;

//---------------------------------------------------------------------------
function TFieldBufferItem.GetFieldTypeName: string;
begin
  Result := CliVarTypeAsStr(FFieldType);
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.SetValue(const AValue: Pointer; const Size: Integer;
  const CopyExact: Boolean=False);
begin
  SetBufferTypeAndSize(FFieldType, Size, CopyExact);
  if Size > 0 then
    if CopyExact then
      Move(AValue^, FData[0], Size)
    else
      begin
        if IsArrayType(FFieldType) then begin
          Assert(AValue <> nil, FName + '.SetValue() - Value pointer is nil');
          Move(AValue^, FData[0], Size)
        end else if FFieldType in [ctString, ctPString] then begin
          Assert((Size+1) = FDataSize, Format('%s.SetValue(%d) - Data Size Mismatch. Expected %d bytes', [FName, Size, FDataSize]));
          StrPCopy(PChar(FData), PChar(AValue));
        end else begin
          Assert(Size = FDataSize, Format('%s.SetValue(%d) - Data Size Mismatch. Expected %d bytes', [FName, Size, FDataSize]));
          Move(AValue^, FData[0], Size);
        end;
      end;
end;

//---------------------------------------------------------------------------
function TFieldBufferItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := Name
  else
    Result := Format('%s[%d]', [ClassName, Index]);
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.UnBindFromStatement;
begin
  FBoundToStatement := -1;  // Note: since 0 is a valid statement we initialize this to a negative value
end;

//---------------------------------------------------------------------------
function TFieldBufferItem.BindToStatement(const AStatement: Integer): Boolean;
begin
  Result := FBoundToStatement <> AStatement;
  if Result then
    FBoundToStatement := AStatement;
end;

//---------------------------------------------------------------------------
{function TFieldBufferItem.RequireStringAllocation(const CliType: TCliVarType;
  const NewSize: Integer): Boolean;
begin
  Result := (CliType in [ctPString, ctString]) and (NewSize >= FDataSize);
end;}

//---------------------------------------------------------------------------
procedure TFieldBufferItem.ReadFromStream(Stream: TStream);
var
  n : Integer;
  ft : TCliVarType;
begin
  Stream.Read(ft, SizeOf(TCliVarType));
  Stream.Read(n, SizeOf(Integer));
  if n > 0 then
    begin
      SetLength(FName, n);
      Stream.Read(FName[1], n);
    end
  else
    FName := '';

  Stream.Read(n, SizeOf(Integer));
  SetBufferTypeAndSize(ft, n);
  if n > 0 then
    Stream.Read(FData[0], n);

  n := Length(FDateFormat);
  if n > 0 then
    begin
      SetLength(FDateFormat, n);
      Stream.Read(FDateFormat[1], n);
    end
  else
    FDateFormat := '';
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.ReadFromStream(Stream: TStream; const AJustData: Boolean);
var
  n : Integer;
  ft: TCliVarType;
begin
  if AJustData then
    begin
      Stream.Read(FCapacity, SizeOf(Integer));
      Stream.Read(ft, SizeOf(TCliVarType));
      SetBufferTypeAndSize(ft, n);
      Stream.Read(n, SizeOf(Integer));
      SetLength(FName, n);
      Stream.Read(FName[1], n);
      if n > 0 then
        Stream.Read(FData[0], n);
    end
  else
    ReadFromStream(Stream);
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.WriteToStream(Stream: TStream);
var n : Integer;
begin
  Stream.Write(FCapacity, SizeOf(Integer));
  Stream.Write(FFieldType, SizeOf(TCliVarType));
  n := Length(FName);
  Stream.Write(n, SizeOf(Integer));
  if n > 0 then
    Stream.Write(FName[1], Length(FName));
  Stream.Write(FDataSize, SizeOf(Integer));
  if n > 0 then
    Stream.Write(FData[0], FDataSize);
  n := Length(FDateFormat);
  if n > 0 then
    Stream.Write(FDateFormat[1], Length(FDateFormat));
end;

//---------------------------------------------------------------------------
procedure TFieldBufferItem.WriteToStream(Stream: TStream; const AJustData: Boolean);
var n : Integer;
begin
  if AJustData then
    begin
      Stream.Write(FFieldType, SizeOf(TCliVarType));
      n := FDataSize;
      Stream.Write(n, SizeOf(Integer));
      if n > 0 then
        Stream.Write(FData[0], FDataSize);
    end
  else
    WriteToStream(Stream);
end;

//---------------------------------------------------------------------------
// TFastDbField
//---------------------------------------------------------------------------
procedure TFastDbField.SetFieldFlags(const Value: Integer);
begin
  FIndexType := FieldFlagsToIndexTypes(Value);
end;

//---------------------------------------------------------------------------
function TFastDbField.GetFieldFlags: Integer;
begin
  Result := IndexTypesToFieldFlags(FIndexType);
end;

//---------------------------------------------------------------------------
function TFastDbField.GetArraySize: Integer;
begin
  if IsArrayType(FFieldType) then
    Result := FDataSize div SizeOfCliType[TCliVarType(Ord(FFieldType) - Ord(ctArrayOfOID))]
  else
    Result := 1;
end;

//---------------------------------------------------------------------------
procedure TFastDbField.SetArraySize(const Value: Integer);
var n, nNew : Integer;
begin
  n    := SizeOfCliType[TCliVarType(Ord(FFieldType) - Ord(ctArrayOfOID))];
  nNew := n * Value;
  if IsArrayType(FFieldType) then
    begin
      if FDataSize <> nNew then
        SetBufferTypeAndSize(FFieldType, nNew);
        //SetLength(FData, nNew);
    end
  else
    raise EFastDbError.Create(cli_unsupported_type);
end;

//---------------------------------------------------------------------------
function TFastDbField.BindToStatement(const AStatement: Integer): Boolean;
var
  n : Integer;
  p : Pointer;
begin
  Result := inherited BindToStatement(AStatement);
  if Result then
    begin
      p := asPointer;
      n := FDataSize;
      if IsArrayType(FFieldType) or (FFieldType in [ctString, ctPString]) then begin
        {$IFDEF CLI_DEBUG}
        TraceDebugProcedure(Format('cli_array_column_ex(%d, "%s", %d, 0x%p, '#10+
                                   '                        (*set)0x%p, (*get)0x%p)', [AStatement, FName, CliTypeToOrd(FieldType), p, @SetFieldColumn, @GetFieldColumn]), True);
        {$ENDIF}
        n := cli_array_column_ex(AStatement, PChar(FName), CliTypeToOrd(FieldType), p, @SetFieldColumn, @GetFieldColumn, Self);
      end else begin
        {$IFDEF CLI_DEBUG}
        TraceDebugProcedure(Format('cli_column(%d, "%s", %d, %d, '#10+
                                   '               (*set)0x%p, (*get)0x%p)', [AStatement, FName, CliTypeToOrd(FieldType), FDataSize, @SetFieldColumn, @GetFieldColumn]), True);
        {$ENDIF}
        n := cli_column(AStatement, PChar(FName), CliTypeToOrd(FieldType), @n, p);
      end;
      {$IFDEF CLI_DEBUG}
      TraceDebugProcedure(Format('%d', [n]), False);
      {$ENDIF}
      CliCheck(n);
    end;
end;

//---------------------------------------------------------------------------
procedure TFastDbField.Assign(Source: TPersistent);
begin
  if Source is TFastDbField then
    begin
      inherited Assign(Source);
      FRefTable        := TFastDbField(Source).FRefTable;
      FInverseRefField := TFastDbField(Source).FInverseRefField;
      FIndexType       := TFastDbField(Source).FIndexType;
    end
  else
    inherited Assign(Source);
end;
//---------------------------------------------------------------------------
function TFastDbField.GetArrayAsBoolean(Idx: Integer): Boolean;
begin
  Assert((Idx >= 0) and (Idx < ArraySize));
  Result := Byte(TCliBoolArray(FData)[Idx]) <> 0;
end;

//---------------------------------------------------------------------------
function TFastDbField.GetArrayAsInteger(Idx: Integer; Index: Integer): Integer;
begin
  Result := GetArrayAsInt64(Idx);
end;

//---------------------------------------------------------------------------
function TFastDbField.GetArrayAsInt64(Idx: Integer): TCliInt8;
begin
  Assert((Idx >= 0) and (Idx < ArraySize), Format('Array index out of bounds (%d/%d)!', [Idx, ArraySize]));
  case FFieldType of
    ctArrayOfOID,
    ctArrayOfInt4:    Result := TCliInt4Array(FData)[Idx];
    ctArrayOfBool,
    ctArrayOfInt1:    Result := Byte(TCliInt1Array(FData)[Idx]);
    ctArrayOfInt2:    Result := TCliInt2Array(FData)[Idx];
    ctArrayOfInt8:    Result := TCliInt8Array(FData)[Idx];
    ctArrayOfReal4:   Result := Trunc(TCliReal4Array(FData)[Idx]);
    ctArrayOfReal8:   Result := Trunc(TCliReal8Array(FData)[Idx]);
    ctArrayOfString:  begin
                        Assert(PChar(TCliInt4Array(FData)[Idx]) <> nil, 'String not assigned!');
                        if not TryStrToInt64(PChar(TCliInt4Array(FData)[Idx]), Result) then Result := 0;
                      end;
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
function TFastDbField.GetArrayAsSingle(Idx: Integer): Single;
begin
  Result := GetArrayAsDouble(1, Idx);
end;

//---------------------------------------------------------------------------
function TFastDbField.GetArrayAsDouble(Idx: Integer; Index: Integer): Double;
begin
  Assert((Index >= 0) and (Index < ArraySize), 'Array index out of bounds!');
  case FFieldType of
    ctArrayOfOID,
    ctArrayOfInt4:    Result := TCliInt4Array(FData)[Index];
    ctArrayOfBool,
    ctArrayOfInt1:    Result := Byte(TCliInt1Array(FData)[Index]);
    ctArrayOfInt2:    Result := TCliInt2Array(FData)[Index];
    ctArrayOfInt8:    Result := TCliInt8Array(FData)[Index];
    ctArrayOfReal4:   Result := TCliReal4Array(FData)[Index];
    ctArrayOfReal8:   Result := TCliReal8Array(FData)[Index];
    ctArrayOfString:  begin
                        Assert(PChar(TCliInt4Array(FData)[Index]) <> nil, 'String not assigned!');
                        if not TryStrToFloat(PChar(TCliInt4Array(FData)[Index]), Result) then Result := 0;
                      end;
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
function TFastDbField.GetArrayAsString(Idx: Integer): string;
begin
  Assert((Idx >= 0) and (Idx < ArraySize), 'Array index out of bounds!');
  case FFieldType of
    ctArrayOfBool:    if GetArrayAsInt64(Idx) <> 0 then Result := 'True' else Result := 'False';
    ctArrayOfOID,
    ctArrayOfInt1,
    ctArrayOfInt2,
    ctArrayOfInt4,
    ctArrayOfInt8:    Result := IntToStr(GetArrayAsInt64(Idx));
    ctArrayOfReal4,
    ctArrayOfReal8:   Result := FloatToStr(GetArrayAsDouble(1, Idx));
    ctArrayOfString:  Result := PChar(TCliInt4Array(FData)[Idx]);  //!!! Needs testing!
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
procedure TFastDbField.SetArrayAsBoolean(Idx: Integer; const Value: Boolean);
begin
  SetArrayAsInteger(Idx, 4, Ord(Value));
end;

//---------------------------------------------------------------------------
procedure TFastDbField.SetArrayAsDouble(Idx: Integer; Index: Integer; const Value: Double);
begin
  Assert((Idx >= 0) and (Idx < ArraySize), 'Array index out of bounds!');
  case FFieldType of
    ctArrayOfOID,
    ctArrayOfInt4:   TCliInt4Array(FData)[Idx] := Trunc(Value);
    ctArrayOfBool,
    ctArrayOfInt1:   Byte(TCliInt1Array(FData)[Idx])  := Trunc(Value);
    ctArrayOfInt2:   TCliInt2Array(FData)[Idx]  := Trunc(Value);
    ctArrayOfInt8:   TCliInt8Array(FData)[Idx]  := Trunc(Value);
    ctArrayOfReal4:  TCliReal4Array(FData)[Idx] := Value;
    ctArrayOfReal8:  TCliReal8Array(FData)[Idx] := Value;
    ctArrayOfString: SetArrayAsString(Idx, FloatToStr(Value));
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
procedure TFastDbField.SetArrayAsInt64(Idx: Integer; const Value: TCliInt8);
begin
  Assert((Idx >= 0) and (Idx < ArraySize), 'Array index out of bounds!');
  case FFieldType of
    ctArrayOfOID,
    ctArrayOfInt4:   Integer(TCliInt4Array(FData)[Idx])  := Value;
    ctArrayOfBool,
    ctArrayOfInt1:   Byte(TCliInt1Array(FData)[Idx])  := Value;
    ctArrayOfInt2:   TCliInt2Array(FData)[Idx]  := Value;
    ctArrayOfInt8:   TCliInt8Array(FData)[Idx]  := Value;
    ctArrayOfReal4:  TCliReal4Array(FData)[Idx] := Value;
    ctArrayOfReal8:  TCliReal8Array(FData)[Idx] := Value;
    ctArrayOfString: SetArrayAsString(Idx, IntToStr(Value));
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
procedure TFastDbField.SetArrayAsInteger(Idx: Integer; Index: Integer; const Value: Integer);
begin
  SetArrayAsInt64(Idx, Value);
end;

//---------------------------------------------------------------------------
procedure TFastDbField.SetArrayAsSingle(Idx: Integer; const Value: Single);
begin
  SetArrayAsDouble(Idx, 1, Value);
end;

//---------------------------------------------------------------------------
procedure TFastDbField.SetArrayAsString(Idx: Integer; const Value: string);
var s : string;
begin
  Assert((Idx >= 0) and (Idx < ArraySize), 'Array index out of bounds!');
  case FFieldType of
    ctArrayOfOID,
    ctArrayOfInt4:      TCliInt4Array(FData)[Idx]  := StrToInt(Value);
    ctArrayOfBool:      begin
                          s := Trim(Value); if s <> '' then s := UpCase(s[1]) else s := ' ';
                          Byte(TCliInt1Array(FData)[Idx]) := Ord(s[1] in ['T', 'Y', '1']);
                        end;
    ctArrayOfInt1:      Byte(TCliInt1Array(FData)[Idx])  := StrToInt(Value);
    ctArrayOfInt2:      TCliInt2Array(FData)[Idx]  := StrToInt(Value);
    ctArrayOfInt8:      TCliInt8Array(FData)[Idx]  := StrToInt(Value);
    ctArrayOfReal4:     TCliReal4Array(FData)[Idx] := StrToFloat(Value);
    ctArrayOfReal8:     TCliReal8Array(FData)[Idx] := StrToFloat(Value);
    ctArrayOfString:    TCliInt4Array(FData)[Idx]  := Integer(PChar(Value));  //!!! Needs testing!
  else
    raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
  end;
end;

//---------------------------------------------------------------------------
// TFastDbVariable
//---------------------------------------------------------------------------
function TFastDbVariable.DisplayString(AQuery: TObject): string;
begin
  Result := CliVarTypeAsStr(FieldType);
end;

//---------------------------------------------------------------------------
function TFastDbVariable.GetSubstitudeValue: string;
begin
  Result := GetValueAsStr(FFieldType, asPointer);
end;

//---------------------------------------------------------------------------
procedure TFastDbVariable.SetAsString(const Value: string);
var sOld : string;
begin
  sOld := GetAsString;
  inherited;
  // Notify the query that SQL changed.
  if (FFieldType = ctSubst) and (Value <> sOld)
     and (Collection <> nil)
     and (Collection.Owner <> nil)
     and (Collection.Owner is TFastDbQuery) then
    TFastDbQuery(Collection.Owner).SqlChanged := True;
end;

//---------------------------------------------------------------------------
function TFastDbVariable.BindToStatement(const AStatement: Integer): Boolean;
var rc : Integer;
begin
  Result := inherited BindToStatement(AStatement);
  if Result then
    begin
      if IsArrayType(FFieldType) then
        raise EFastDbError.Create(cli_unsupported_type, Format('Variable[%s] %s', [FName, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
      {$IFDEF CLI_DEBUG}
      TraceDebugProcedure(Format('cli_parameter(%d, "%s", %d, 0x%p)', [AStatement, PARAM_CHAR+FName, CliTypeToOrd(FieldType), asPointer]), True);
      {$ENDIF}
      rc := cli_parameter(AStatement, PChar(PARAM_CHAR+FName), CliTypeToOrd(FFieldType), asPointer);
      {$IFDEF CLI_DEBUG}
      TraceDebugProcedure(Format('%d', [rc]), False);
      {$ENDIF}
      CliCheck(rc, 'cli_parameter failed');
    end;
end;

//---------------------------------------------------------------------------
// TDataCollection
//---------------------------------------------------------------------------
function TFieldBufferCollection.Add(const AName: string; AType: TCliVarType): TFieldBufferItem;
var i : Integer;
begin
  for i:=0 to Count-1 do
    with TFieldBufferItem(Items[i]) do
      if StrIComp(PChar(FName), PChar(AName)) = 0 then   // Note: StrIComp() works faster than SameText()
        raise EFastDbError.Create(cli_item_already_defined);
  Result := TFieldBufferItem(inherited Add);
  Result.FName := AName;
  Result.FieldType := AType;
end;

//---------------------------------------------------------------------------
procedure TFieldBufferCollection.Delete(const AName: string);
var i : Integer;
begin
  for i:=0 to Count-1 do
    with TFieldBufferItem(Items[i]) do
      if StrIComp(PChar(FName), PChar(AName)) = 0 then   // Note: StrIComp() works faster than SameText()
        begin
          Free;
          exit;
        end;
end;

//---------------------------------------------------------------------------
procedure TFieldBufferCollection.UnBindFromStatement;
var i : Integer;
begin
  for i:=0 to Count-1 do
    TFieldBufferItem(Items[i]).UnBindFromStatement;
end;

//---------------------------------------------------------------------------
function TFieldBufferCollection.asText: string;
var i : Integer;
begin
  Result := '';
  for i:=0 to Count-1 do
    with TFieldBufferItem(Items[i]) do
      if not IsArrayType(FFieldType) then
        Result := Format('%s%s=%s'+EOL, [Result, FName, asString]);
end;

//---------------------------------------------------------------------------
procedure TFieldBufferCollection.ClearValues;
var i : Integer;
begin
  for i:=0 to Count-1 do
    TFieldBufferItem(Items[i]).ClearValue;
end;

//---------------------------------------------------------------------------
procedure TFieldBufferCollection.ReadFromStream(Stream: TStream);
var i : Integer;
begin
  for i:=0 to Count-1 do
    TFieldBufferItem(Items[i]).ReadFromStream(Stream);
end;

//---------------------------------------------------------------------------
procedure TFieldBufferCollection.WriteToStream(Stream: TStream);
var i : Integer;
begin
  for i:=0 to Count-1 do
    TFieldBufferItem(Items[i]).WriteToStream(Stream);
end;

//---------------------------------------------------------------------------
// TFastDbFields
//---------------------------------------------------------------------------
constructor TFastDbFields.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TFastDbField);
end;

//---------------------------------------------------------------------------
function TFastDbFields.Add(const AName: string; const AType: TCliVarType;
  const AIndexType: TIndexTypes; const ARefTable,
  AInverseRefField: string): TFastDbField;
begin
  Result := TFastDbField(inherited Add(AName, AType));
  Result.IndexType  := AIndexType;
  Result.RefTable   := ARefTable;
  Result.InverseRefField := AInverseRefField;
end;

//---------------------------------------------------------------------------
function TFastDbFields.GetField(Index: Integer): TFastDbField;
begin
  Result := TFastDbField(Items[Index]);
end;

//---------------------------------------------------------------------------
procedure TFastDbFields.SetField(Index: Integer; const Value: TFastDbField);
begin
  TFastDbField(Items[Index]).Assign(Value);
end;

//---------------------------------------------------------------------------
// TFastDbVariables
//---------------------------------------------------------------------------
constructor TFastDbVariables.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TFastDbVariable);
end;

//---------------------------------------------------------------------------
function TFastDbVariables.Add(const AName: string;
  const AType: TCliVarType; AValue: Pointer=nil): TFastDbVariable;
begin
  if AType in [ctArrayOfOID .. ctArrayOfString, ctUnknown] then
    raise EFastDbError.Create(cli_unsupported_type);
  Result := TFastDbVariable(inherited Add(AName, AType));
  Result.Name := AName;
  Result.FieldType := AType;
  if AValue <> nil then
    Result.SetValue(AValue, Result.FieldSize);
end;

//---------------------------------------------------------------------------
function TFastDbVariables.GetVariable(Index: Integer): TFastDbVariable;
begin
  Result := TFastDbVariable(Items[Index]);
end;

//---------------------------------------------------------------------------
procedure TFastDbVariables.SetVariable(Index: Integer; const Value: TFastDbVariable);
begin
  TFastDbVariable(Items[Index]).Assign(Value);
end;

end.
