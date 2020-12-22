unit FastDbDataSet;

{$I FastDbConfig.inc}

interface

uses
  Classes, SysUtils,
  {$IFDEF MSWINDOWS}
  Windows, Forms,
  {$ENDIF}
  Contnrs,
  FastDbSession, FastDbQuery, FastDbVar, FastDbCLI,
  DB, Variants, TypInfo;

type
  EMdDataSetError = class (Exception);

  TMdRecInfo = record
    Bookmark: Longint;
    BookmarkFlag: TBookmarkFlag;
  end;
  PMdRecInfo = ^TMdRecInfo;

  TFastDbDataSet = class (TDataSet)
  private
    FReadOnly   : Boolean;
    FRecIdField : TField;
    FFilterList : TList;

    function PrepareUpdateQuery(const AUpdateType: TUpdateKind; const CurrentOID: TCliOID): TCliOID;

    function  GetSql: string;
    procedure SetSql(const Value: string);
    function  GetSession: TFastDbSession;
    procedure SetSession(const Value: TFastDbSession);
    function  GetFields: TFastDbFields;
    function  GetVariables: TFastDbVariables;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetVariables(const Value: TFastDbVariables);

    procedure CreateRecIDField;
  protected
    // the list holding the data
    FQuery       : TFastDbQuery;
    FUpdateQuery : TFastDbQuery;   // Query used for updates
    FUpdateFields: TFastDbFields;       // Fields used to hold inserted/updated values

    FIsTableOpen: Boolean;

    // record data
    FRecordSize, // the size of the actual data
    FRecordBufferSize, // data + housekeeping (TRecInfo)
    FCurrentRecord, // current record (0 to FRecordCount - 1)
    BofCrack, // before the first record (crack)
    EofCrack: Integer; // after the last record (crack)

    // create, close, and so on
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function  IsCursorOpen: Boolean; override;

    // custom functions
    procedure InternalLoadCurrentRecord(Buffer: PChar); virtual;

    // memory management
    function  AllocRecordBuffer: PChar; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    function  GetRecordSize: Word; override;

    // movement and optional navigation (used by grids)
    function  GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    function  GetRecNo: Longint; override;
    function  GetRecordCount: Longint; override;
    procedure SetRecNo(Value: Integer); override;

    // bookmarks
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    function  GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;

    // custom dataset virtual methods
    procedure InternalCancel;  override;
    procedure InternalEdit;    override;
    procedure InternalInsert;  override;
    procedure InternalRefresh; override;

    // edit support
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalPost;    override;
    procedure InternalDelete;  override;
    // fields
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;

    // TDataSet virtual methdos
    procedure InternalInitFieldDefs; override;
    function  GetCanModify: Boolean; override;
    procedure InternalHandleException; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    property FilterList: TList read FFilterList;
    property RecIdField : TField read FRecIdField;
  published
    property Session: TFastDbSession read GetSession write SetSession;
    property SQL: string read GetSql write SetSql;
    property Fields:    TFastDbFields    read GetFields;
    property Variables: TFastDbVariables read GetVariables write SetVariables;
    property ReadOnly: Boolean           read FReadOnly    write SetReadOnly;

    // redeclared data set properties
    property Active;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

implementation

var
  DsFieldTypeOfCliType : array[TCliVarType] of TFieldType = (
    ftInteger,         //ctOID
    ftBoolean,         //ctBoolean
    ftBytes,           //ctInt1
    ftSmallint,        //ctInt2
    ftInteger,         //ctInt4
    ftLargeint,        //ctInt8
    ftFloat,           //ctReal4
    ftFloat,           //ctReal8
    ftInteger,         //ctDecimal,
    ftString,          // cli_asciiz,
    ftString,          // cli_pasciiz,
    ftString,          // cli_cstring
    ftArray,           // cli_array_of_oid,
    ftArray,           // cli_array_of_bool,
    ftArray,           // cli_array_of_int1,
    ftArray,           // cli_array_of_int2,
    ftArray,           // cli_array_of_int4,
    ftArray,           // cli_array_of_int8,
    ftArray,           // cli_array_of_real4,
    ftArray,           // cli_array_of_real8,
    ftArray,           // cli_array_of_decimal
    ftArray,           //ctArrayOfString
    ftUnknown,         //ctAny,
    ftDateTime,        //ctDateTime
    ftAutoInc,         //ctAutoInc
    ftUnknown,         //ctRectangle
    ftUnknown,         //ctUnknown
    ftUnknown          //ctSubst
  );

/////////////////////////////////////////////////
////// Part I:
////// Initialization, opening, and closing
/////////////////////////////////////////////////

// I: open the dataset
procedure TFastDbDataSet.InternalOpen;
begin
  FRecordSize := 0;
  FQuery.Execute;  // The internal query is always executed in ReadOnly mode!
                   // A separate query is used for insert/update

  // initialize the field definitions
  // (another virtual abstract method of TDataSet)
  InternalInitFieldDefs;

  // if there are no persistent field objects,
  // create the fields dynamically
  if DefaultFields then
    CreateFields;
  // connect the TField objects with the actual fields
  BindFields (True);

  // sets cracks and record position and size
  BofCrack := -1;
  EofCrack := GetRecordCount;
  FCurrentRecord := BofCrack;
  FRecordBufferSize := FRecordSize + sizeof (TMdRecInfo);
  BookmarkSize := sizeOf (Integer);

  // everything OK: table is now open
  FIsTableOpen := True;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.InternalClose;
begin
  // disconnet field objects
  BindFields (False);
  // destroy field object (if not persistent)
  if DefaultFields then
    DestroyFields;

  FQuery.Close;

  // close the file
  FIsTableOpen := False;
end;

// I: is table open
function TFastDbDataSet.IsCursorOpen: Boolean;
begin
  Result := FIsTableOpen;
end;

////////////////////////////////////////
////// Part II:
////// Bookmarks management and movement
////////////////////////////////////////

// II: set the requested bookmark as current record
procedure TFastDbDataSet.InternalGotoBookmark (Bookmark: Pointer);
var
  ReqBookmark: Integer;
begin
  ReqBookmark := PInteger (Bookmark)^;
  if (ReqBookmark >= 0) and (ReqBookmark < GetRecordCount) then
    FCurrentRecord := ReqBookmark
  else
    raise EMdDataSetError.Create ('Bookmark ' +
      IntToStr (ReqBookmark) + ' not found');
end;

// II: same as above (but passes a buffer)
procedure TFastDbDataSet.InternalSetToRecord (Buffer: PChar);
var
  ReqBookmark: Integer;
begin
  ReqBookmark := PMdRecInfo(Buffer + FRecordSize).Bookmark;
  InternalGotoBookmark (@ReqBookmark);
end;

// II: retrieve bookmarks flags from buffer
function TFastDbDataSet.GetBookmarkFlag (
  Buffer: PChar): TBookmarkFlag;
begin
  Result := PMdRecInfo(Buffer + FRecordSize).BookmarkFlag;
end;

// II: change the bookmark flags in the buffer
procedure TFastDbDataSet.SetBookmarkFlag (Buffer: PChar;
  Value: TBookmarkFlag);
begin
  PMdRecInfo(Buffer + FRecordSize).BookmarkFlag := Value;
end;

// II: Go to a special position before the first record
procedure TFastDbDataSet.InternalFirst;
begin
  FCurrentRecord := BofCrack;
end;

// II: Go to a special position after the last record
procedure TFastDbDataSet.InternalLast;
begin
  EofCrack := GetRecordCount;
  FCurrentRecord := EofCrack;
end;

//---------------------------------------------------------------------------
// II: read the bookmark data from record buffer
procedure TFastDbDataSet.GetBookmarkData (Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PMdRecInfo(Buffer + FRecordSize).Bookmark;
end;

//---------------------------------------------------------------------------
// II: set the bookmark data in the buffer
procedure TFastDbDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PMdRecInfo(Buffer + FRecordSize).Bookmark := PInteger(Data)^;
end;

//---------------------------------------------------------------------------
// II (optional): Record count
function TFastDbDataSet.GetRecordCount: Longint;
begin
  //CheckActive;
  Result := FQuery.RowCount;
end;

// II (optional): Get the number of the current record
function TFastDbDataSet.GetRecNo: Longint;
begin
  UpdateCursorPos;
  if FCurrentRecord < 0 then
    Result := 1
  else
    Result := FCurrentRecord + 1;
end;

// II (optional): Move to the given record number
procedure TFastDbDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value >= 1) and (Value <= GetRecordCount) then
  begin
    FCurrentRecord := Value - 1;
    Resync([]);
  end;
end;

//////////////////////////////////////////
////// Part III:
////// Record buffers and field management
//////////////////////////////////////////

// III: Retrieve data for current, previous, or next record
// (eventually moving to it) and return the status
function TFastDbDataSet.GetRecord(Buffer: PChar;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grOK; // default
  case GetMode of
    gmNext: // move on
      if FCurrentRecord < GetRecordCount - 1 then
        Inc (FCurrentRecord)
      else
        Result := grEOF; // end of file
    gmPrior: // move back
      if FCurrentRecord > 0 then
        Dec (FCurrentRecord)
      else
        Result := grBOF; // begin of file
    gmCurrent: // check if empty
      if FCurrentRecord >= GetRecordCount then
        Result := grError;
  end;
  // load the data
  if Result = grOK then
    InternalLoadCurrentRecord (Buffer)
  else
    if (Result = grError) and DoCheck then
      raise EMdDataSetError.Create ('GetRecord: Invalid record');
end;

// III: Initialize the record (set to 0)
procedure TFastDbDataSet.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^, FRecordBufferSize, 0);
end;

// III: Free the buffer
procedure TFastDbDataSet.FreeRecordBuffer (var Buffer: PChar);
begin
  FreeMem (Buffer);
end;

/// III: Determine the size of each record buffer in memory
function TFastDbDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize; // data only
end;

/// III: Allocate a buffer for the record
function TFastDbDataSet.AllocRecordBuffer: PChar;
begin
  GetMem (Result, FRecordBufferSize);
end;

//---------------------------------------------------------------------------
// TFastDbDataSet
//---------------------------------------------------------------------------
constructor TFastDbDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FQuery       := TFastDbQuery.Create(Self);
  FUpdateQuery := TFastDbQuery.Create(Self);
  FUpdateFields:= TFastDbFields.Create(Self);
  FReadOnly    := True;
  CreateRecIDField;
end;

//---------------------------------------------------------------------------
destructor TFastDbDataSet.Destroy;
begin
  FQuery.Free;
  FUpdateQuery.Free;
  FUpdateFields.Free;
  inherited;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.InternalLoadCurrentRecord(Buffer: PChar);
begin
  FQuery.Skip(FCurrentRecord - Integer(FQuery.RecNo));
  //PInteger (Buffer)^ := FCurrentRecord;
  with PMdRecInfo(Buffer + FRecordSize)^ do
  begin
    BookmarkFlag := bfCurrent;
    Bookmark := FCurrentRecord;
  end;
end;

//---------------------------------------------------------------------------
function TFastDbDataSet.GetSql: string;
begin
  Result := FQuery.Sql;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.SetSql(const Value: string);
begin
  FQuery.Sql := Value;
end;

//---------------------------------------------------------------------------
function TFastDbDataSet.GetFields: TFastDbFields;
begin
  Result := FQuery.Fields;
end;

//---------------------------------------------------------------------------
function TFastDbDataSet.GetSession: TFastDbSession;
begin
  Result := FQuery.Session;
end;

//---------------------------------------------------------------------------
function TFastDbDataSet.GetVariables: TFastDbVariables;
begin
  Result := FQuery.Variables;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.SetVariables(const Value: TFastDbVariables);
begin
  FQuery.Variables.Assign(Value);
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.SetSession(const Value: TFastDbSession);
begin
  FQuery.Session := Value;
  FUpdateQuery.Session := Value;
end;

//---------------------------------------------------------------------------
function TFastDbDataSet.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.InternalHandleException;
begin
  // special purpose exception handling
  // do nothing
end;

//---------------------------------------------------------------------------
function TFastDbDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  nCurRec : Integer;
  flds : TFastDbFields;
  fld  : TFastDbField;
begin
  Result := False;
  if not IsEmpty and (Field.FieldNo > 0) then
    begin
      nCurRec := PMdRecInfo(ActiveBuffer + FRecordSize)^.Bookmark;
      FQuery.Skip(nCurRec - Integer(FQuery.RecNo));
      Result := True;

      case State of
        dsEdit, dsInsert:
          flds := FUpdateFields;
      else
        flds := FQuery.Fields;
      end;

      fld := flds[Field.FieldNo-1];

      if Assigned (Buffer) then
        if fld.FieldType = ctString then begin
          StrPCopy(PChar(Buffer), fld.asString);
          Result := True;
        end else if not IsArrayType(fld.FieldType) then begin
          Move(fld.asPointer^, Buffer^, fld.FieldSize);
          Result := True;
        end else
          Result := False;
      if (Field is TDateTimeField) and (fld.asDateTime = 0) then
        Result := False;
    end;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.InternalInitFieldDefs;
var
  i   : Integer;
  nSize: Integer;
begin
  FieldDefs.Clear;
  for i:=0 to FQuery.Fields.Count-1 do begin
    case DsFieldTypeOfCliType[FQuery.Fields[i].FieldType] of
      ftBytes : nSize := 1;  //ctInt1
      ftArray : nSize := FQuery.Fields[i].ArraySize;
      ftString: nSize := 80; // Use some default value
    else
      nSize := 0;
    end;
    FieldDefs.Add(FQuery.Fields[i].Name,
                  DsFieldTypeOfCliType[FQuery.Fields[i].FieldType],
                  nSize, True);
  end;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  nCurRec : Integer;
  i : Integer;
begin
  if Field.FieldNo >= 0 then
    begin
      nCurRec := PMdRecInfo(ActiveBuffer + FRecordSize)^.Bookmark;
      FQuery.Skip(nCurRec - Integer(FQuery.RecNo));  // Position the record

      if Assigned (Buffer) then
        with FUpdateFields[Field.FieldNo-1] do
          case DsFieldTypeOfCliType[FieldType] of
            ftBytes:  SetValue(Buffer, TBytesField(Field).Size);
            ftString: asString := PChar(Buffer);
            {$IF NOT Defined(FPC)}
            ftArray : for i:=0 to TArrayField(Field).Size-1 do
                        case FieldType of
                          ctArrayOfOID,
                          ctArrayOfInt4,
                          ctArrayOfBool,
                          ctArrayOfInt1,
                          ctArrayOfInt2,
                          ctArrayOfInt8:
                            asArrayInt8[i]   := VarAsType(TArrayField(Field).FieldValues[i], varInt64);
                          ctArrayOfReal4,
                          ctArrayOfReal8:
                            asArrayDouble[i] := VarAsType(TArrayField(Field).FieldValues[i], varDouble);
                          ctArrayOfString:
                            asArrayString[i] := VarAsType(TArrayField(Field).FieldValues[i], varString);
                        else
                          raise EFastDbError.Create(cli_unsupported_type, Format('Field[%s] %s', [Name, GetEnumName(TypeInfo(TCliVarType), Ord(FieldType))]));
                        end;
            {$ENDIF}
          else
            SetValue(Buffer, FieldSize);
          end;

      DataEvent (deFieldChange, Longint(Field));
    end;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.SetReadOnly(const Value: Boolean);
begin
  if FQuery.IsOpen then
    raise EFastDbQuery.Create('Cannot modify property of an active dataset!');
  FReadOnly := Value;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  // always append at the end
  InternalLast;

  FUpdateQuery.Fields.ClearValues;
  {with TFastDbQuery.Create(nil) do
  try
    Fields.Assign(FQuery.Fields);
    Sql := 'insert into '+FQuery.TableName;
    Session := FQuery.Session;
    FQuery.Freeze;
    Insert;
  finally
    Free;
    FQuery.UnFreeze;
  end;}
end;

//---------------------------------------------------------------------------
function TFastDbDataSet.PrepareUpdateQuery(const AUpdateType: TUpdateKind;
  const CurrentOID: TCliOID): TCliOID;

  procedure CopyValues(AFrom, ATo: TFastDbFields);
  var i : Integer;
  begin
    for i:=0 to AFrom.Count-1 do
      ATo[i].CopyTypeAndValue(AFrom[i]);
  end;
begin
  FUpdateQuery.ClearVariables;

  case AUpdateType of
    ukInsert:
      begin
        FUpdateQuery.Sql := Format('insert into %s', [FQuery.TableName]);


        CopyValues(FUpdateFields, FUpdateQuery.Fields);
        Result := FUpdateQuery.Insert;
      end;
    ukModify:
      begin
        FUpdateQuery.Sql := Format('select * from %s where current = %%oid', [FQuery.TableName]);
        FUpdateQuery.Variables.Add('oid', ctOid, @CurrentOID);

        if FUpdateQuery.Execute(False) = 1 then
          begin
            CopyValues(FUpdateFields, FUpdateQuery.Fields);
            FUpdateQuery.Update;
          end;

        Result := CurrentOID;
      end;
    ukDelete:
      begin
        FQuery.Next;
        Result := FQuery.OID;
        FQuery.Prev;

        FUpdateQuery.Sql := Format('select * from %s where current = %%oid', [FQuery.TableName]);
        FUpdateQuery.Variables.Add('oid', ctOid, @CurrentOID);

        if FUpdateQuery.Execute(False) = 1 then
          FUpdateQuery.Delete;
      end;
  else
    raise EFastDbError.Create('Unknown update type!');
  end;

  FUpdateQuery.Close;
end;

//---------------------------------------------------------------------------
// III: Delete the current record
procedure TFastDbDataSet.InternalDelete;
var
  oid : TCliOID;

begin
  CheckActive;
  DisableControls;
  try
    InternalLoadCurrentRecord(ActiveBuffer);      //!!! Investigate the need for this one

    oid := PrepareUpdateQuery(ukDelete, FQuery.OID);
    FQuery.Close;
    FQuery.Session.Commit(True);
    FQuery.Execute;

    FCurrentRecord := FQuery.Seek(oid);
    InternalLoadCurrentRecord(ActiveBuffer);
  finally
    EnableControls;
  end;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.InternalPost;
var
  oid : TCliOID;
begin
  CheckActive;
  DisableControls;
  try
    InternalLoadCurrentRecord(ActiveBuffer);      //!!! Investigate the need for this one

    if State = dsEdit then
      begin
        oid := PrepareUpdateQuery(ukModify, FQuery.OID);

        //FQuery.RefreshRecord;

        FQuery.Close;
        FQuery.Session.Commit(True);
        FQuery.Execute;
        //FQuery.Freeze;
        //FQuery.Unfreeze;
        FCurrentRecord := FQuery.Seek(oid);
        InternalLoadCurrentRecord(ActiveBuffer);
      end
    else
      begin
        // always append
        InternalLast;

        FUpdateQuery.Sql := Format('insert into %s', [FQuery.TableName]);
        FUpdateQuery.Fields.Assign(FUpdateFields);
        FUpdateQuery.ClearVariables;

        FUpdateQuery.Insert;

        FQuery.Close;
        FQuery.Execute;
        FQuery.Last;
      end;
  finally
    EnableControls;
  end;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.InternalRefresh;
begin
  FQuery.Close;
  FQuery.Execute;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.InternalCancel;
begin
  FUpdateFields.ClearValues;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.InternalEdit;
var nCurRec : Integer;
begin
  // Ensure we are positioned at the right record
  nCurRec := PMdRecInfo(ActiveBuffer + FRecordSize)^.Bookmark;
  FQuery.Skip(nCurRec - Integer(FQuery.RecNo));

  // Buffer current record values
  FUpdateFields.Assign(FQuery.Fields);
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.InternalInsert;
begin
  // Get current field metadata
  FUpdateFields.Assign(FQuery.Fields);
  FUpdateFields.ClearValues;
end;

//---------------------------------------------------------------------------
procedure TFastDbDataSet.CreateRecIDField;
begin
  if (FRecIdField <> nil) then exit;
  FRecIdField := TIntegerField.Create(self);
  with FRecIdField do
  begin
    FieldName := 'RecID';
    DataSet := self;
    Name := self.Name + FieldName;
    Calculated := True;
    Visible := False;
  end;
end;

end.
