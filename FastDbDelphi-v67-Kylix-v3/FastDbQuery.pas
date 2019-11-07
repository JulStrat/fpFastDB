{-< FastDbQuery.pas >----------------------------------------------*
  FastDbQuery Version 1.0
    (c) 2002 Serge Aleynikov (serge@hq.idt.net)
  Main Memory Database Management System
  Created:     11/11/2002 Serge Aleynikov (serge@hq.idt.net)
  Last update:
    8/20/2003  Added TFastDbQuery.Variable() method
    8/19/2003  Added DeleteAllRecords()
    4/8/2003   Added additional debugging details for cli_insert
    1/22/2003  Added support for cli_attach and cli_detach
    12/06/2002 Serge Aleynikov

-------------------------------------------------------------------*
  Database Query component
-------------------------------------------------------------------*}
unit FastDbQuery;
{$ifdef FPC}{$mode delphi}{$endif}
{$I FastDbConfig.inc}

interface

uses
  SysUtils, Classes, Math, StrUtils, FastDbCLI, FastDbSession, FastDbVar
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF LINUX}
  {$ifndef FPC}
  , Libc
  {$endif}
  {$ENDIF}
  ;

type
  TFastDbQuery = class;

  TOnDescribeFieldEvent = procedure(Sender: TFastDbField; var GetProc: TArrayFieldGetValue; var SetProc: TArrayFieldSetValue) of object;

  EFastDbQuery = class(Exception);

  TFastDbQuery = class(TComponent)
  private
    FVariables       : TFastDbVariables;
    FFieldList       : TFastDbFields;
    FSession         : TFastDbSession;
    FStatement       : Integer;
    FSQL             : string;
    FBof             : Boolean;
    FEof             : Boolean;
    FDebug           : Boolean;
    FRowCount        : LongWord;
    FRecNo           : LongWord;
    FBeforeQuery     : TNotifyEvent;      // Gets called on an Execute
    FAfterQuery      : TNotifyEvent;
    FOnDescribeField : TOnDescribeFieldEvent;
    FSqlChanged      : Boolean;
    FTableName       : string;
    FInsertQuery     : Boolean;
    FDescribed       : Boolean;
    FReadOnly        : Boolean;

    procedure SetSession(ASession: TFastDbSession);
    procedure SetSQL(Value: string);
    //procedure SetVariables(Value: TFastDbVariables);
    //procedure SetFields(const Value: TFastDbFields);
    function  GetTableName: string;
    //function  FindVariable(var AName: string): Integer;
    procedure FindVariables(var SQL: string; var Vars: TStringList);
    procedure ReplaceSubstVariables(var s: string);
    function  GetOID: TCliOid;
    procedure InternalDescribe;
    procedure InternalBindFields;
    procedure InternalBindVariables;
    procedure FreeStatement(const CheckError: Boolean=True);
    //function  ReplaceVariables(S: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Close;
    procedure Describe;
    function  IsOpen: Boolean;
    property  Handle: Integer read FStatement;
    property  ReadOnly: Boolean read FReadOnly;

    function  Execute(const AReadOnly: Boolean=True): LongWord; virtual;
    function  Insert(const IntoTableName: string=''): TCliOid;
    procedure Update;    // updates currently selected record.  Execute must be done qtForUpdate
    procedure Delete;    // deletes all selected records.       Execute must be done qtForUpdate
    procedure RefreshRecord;
    procedure Freeze;    // freezes the statement so that it is possible to issue a session.commit and to preserve the statement's state
    procedure UnFreeze;  // Unfreezes the statement frozen with Freeze

    property  SqlChanged: Boolean read FSqlChanged write FSqlChanged;  // set to True to force Describe before an Execute.

    procedure First;
    procedure Last;
    function  Next: Boolean;
    function  Prev: Boolean;
    procedure Skip(const Records: Integer; const RefreshOnNoSkip: Boolean=False);
    function  Seek(const AOid: TCliOID): Integer;   // returns new RecNo
    property  Eof: Boolean read FEof;
    property  Bof: Boolean read FBof;

    property  RowCount: LongWord read FRowCount;
    property  RecNo: LongWord    read FRecNo;
    property  OID: TCliOid read GetOID;
    function  RecordSize: Integer;

    function  Field(const FieldId: Integer): TFastDbField; overload;
    function  Field(const Field: string): TFastDbField;    overload;

    function  Variable(const Index: Integer): TFastDbVariable; overload;
    function  Variable(const Name: string):   TFastDbVariable; overload;

    function  FieldIndex(const Field: string): Integer;
    function  VariableIndex(AName: string): Integer;

    procedure Clear;
    procedure ClearVariables;

    function  SubstitutedSQL: string;
  published
    property SQL: string read FSQL write SetSQL;
    property Session: TFastDbSession read FSession write SetSession;
    property Debug: Boolean read FDebug write FDebug;

    property Fields:    TFastDbFields    read FFieldList{ write SetFields};
    property Variables: TFastDbVariables read FVariables{ write SetVariables};

    property TableName: string read GetTableName;

    property OnBeforeQuery: TNotifyEvent read FBeforeQuery write FBeforeQuery;
    property OnAfterQuery: TNotifyEvent read FAfterQuery write FAfterQuery;
    property OnDescribeField: TOnDescribeFieldEvent read FOnDescribeField write FOnDescribeField;
  end;

  // Detele all records from a table
  TCommitType = (ctNone, ctPreCommit, ctCommit);
  procedure DeleteAllRecords(ASession: TFastDbSession; const ATableName: string; const ACommit: TCommitType=ctNone);

implementation

const
  SFieldDoesntExist = 'Field %s does not exist';
  SVarDoesntExist   = 'Variable %s does not exist';

//---------------------------------------------------------------------------
procedure DeleteAllRecords(ASession: TFastDbSession; const ATableName: string; const ACommit: TCommitType);
begin
  with TFastDbQuery.Create(nil) do
  try
    Session := ASession;
    Sql     := 'select * from ' + ATableName;
    if Execute(False) > 0 then
      Delete;
  finally
    Free;
    case ACommit of
      ctPreCommit: ASession.Commit(False);
      ctCommit:    ASession.Commit(True);
    end;
  end;
end;

//---------------------------------------------------------------------------
// TFastDbQuery
//---------------------------------------------------------------------------
constructor TFastDbQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSession   := nil;
  FStatement := FastDbUnilitializedHandle;
  FDebug     := False;
  FFieldList := TFastDbFields.Create(Self);
  FVariables := TFastDbVariables.Create(Self);
end;

//---------------------------------------------------------------------------
destructor TFastDbQuery.Destroy;
begin
  Close;
  FFieldList.Free;
  FVariables.Free;
  inherited;
end;


//---------------------------------------------------------------------------
procedure TFastDbQuery.Clear;
begin
  Close;
  FVariables.Clear;
  FFieldList.Clear;
  FSql := '';
  FSqlChanged := True;
  FTableName  := '';
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.ClearVariables;
begin
  FVariables.Clear;
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.FindVariables(var SQL: string; var Vars: TStringList);
begin
  FastDbSession.FindVariables(SQL, False, Vars);
end;

//---------------------------------------------------------------------------
// Replace substitution variables
{
function TFastDbQuery.ReplaceVariables(S: string): string;
begin
var Vars: TStringList;
    i, n, p: Integer;
    Value: string;
begin
  Vars := TStringList.Create;
  try
    FindVariables(S, Vars);
    for i := Vars.Count - 1 downto 0 do begin
      n := FindVariable(Vars[i]);
      if n >= 0 then
        Value := Copy(FVariables[i], Length(Vars[i]) + 2, Length(FVariables[i]));
      else
        Value := '';
      p := Integer(Vars.Objects[i]);
      Delete(S, p, Length(Vars[i]) + 1);
      if (Length(S) > p) and (S[p] = '.') then Delete(S, p, 1);
      Insert(Value, S, p); *)
    end;
    Result := S;
  finally
    Vars.Free;
  end;
end;}

//---------------------------------------------------------------------------
// Replace substitution variables by the actual values
procedure TFastDbQuery.ReplaceSubstVariables(var s: string);
var vi, sv{, vp}: Integer;
//    ss: string;
    Ready: Boolean;
    VarList: TStringList;
    VarName: string;
begin
  // First check if there are any substitution variables
  Ready := True;
  for vi := 0 to Variables.Count - 1 do
    if Variables[vi].FieldType = ctSubst then Ready := False;
  // If so, replace them with the values
  if not Ready then
  begin
    VarList := TStringList.Create;
    try
      FindVariables(s, VarList);
      for sv := VarList.Count-1 downto 0 do begin
        VarName := VarList[sv];
        vi := VariableIndex(VarName);
        if (vi >= 0) and (Variables[vi].FieldType = ctSubst) then
          begin
            (*vv := GetVariable(VarName);
            !!!!!!!!!!!
            if VarIsNull(vv) or VarIsEmpty(vv) then ss := '' else ss := vv;
            vp := Integer(VarList.Objects[sv]);
            Delete(s, vp, Length(VarName) + 1);
            Insert(ss, s, vp);*)
          end;
      end;
    finally
      VarList.Free;
    end;
  end;
end;

//---------------------------------------------------------------------------
// Return the SQL with substitution variables substituted
function TFastDbQuery.SubstitutedSQL: string;
var i : Integer;
begin
  Result := FSql; //FSql.Text;
  for i:=Length(Result) downto 1 do
    if Result[i] in [#10,#13] then
      System.Delete(Result, i, 1);
  ReplaceSubstVariables(Result);
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.Close;
begin
  FreeStatement(True);
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.Describe;
var
  s : string;
begin
  if FDescribed then exit;

  if FSession = nil then
    raise EFastDbError.Create(cli_session_not_assigned)
  else if Trim(FSQL) = '' then
    raise EFastDbError.Create(cli_empty_query);

  FreeStatement(False);

  if FFieldList.Count = 0 then
    InternalDescribe;

  s := SubstitutedSQL;
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_statement(%d, "%s")', [FSession.Handle, s]), True);
  {$ENDIF}
  FStatement := cli_statement(FSession.Handle, PChar(s));
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [FStatement]), False);
  {$ENDIF}
  FSession.CliCheck(FStatement, 'cli_statement failed');

  InternalBindFields;
  InternalBindVariables;

  FDescribed := True;
end;

//---------------------------------------------------------------------------
function TFastDbQuery.Execute(const AReadOnly: Boolean=True): LongWord;
var QueryType: Integer;
begin
  if not AReadOnly and not (oaReadWrite in FSession.OpenAttributes) then
    raise EFastDbQuery.Create('Cannot execute a writable query in a read-only session!');

  // For threaded access attach this thread to the database
  if FSession.Threaded then
    FSession.Attach;

  Describe;

  FReadOnly := AReadOnly;
  if AReadOnly then QueryType := 0 else QueryType := 1;

  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_fetch(%d, %d)', [FStatement, QueryType]), True);
  {$ENDIF}
  FRowCount := cli_fetch(FStatement, QueryType);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [FRowCount]), False);
  {$ENDIF}
  FRowCount := FSession.CliCheck(FRowCount, 'cli_fetch failed');

  FRecNo    := 0;
  FEof   := FRowCount <= 0;
  Result := FRowCount;
  if not FEof then
    First
  else
    FBof := False;
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.RefreshRecord;
var rc : Integer;
begin
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_skip(%d, 0)', [FStatement, 0]), True);
  {$ENDIF}
  rc := cli_skip(FStatement, 0);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  FSession.CliCheck(rc);
end;

//---------------------------------------------------------------------------
function TFastDbQuery.Insert(const IntoTableName: string=''): TCliOid;
var
  rc : Integer;
  {$IFDEF CLI_DEBUG}
  i : Integer;
  {$ENDIF}
  s : string;

  function ParamStr: string;
  var
    i, nLen1 : Integer;
    s1 : string;
  begin
    nLen1 := 0;
    Result := '';
    for i:=0 to Fields.Count-1 do
      nLen1 := Max(nLen1, Length(Fields[i].name));
    for i:=0 to Fields.Count-1 do begin
      if IsArrayType(Fields[i].FieldType) then
        if Fields[i].ArraySize = 0 then
          s1 := '[]'
        else
          s1 := Format('[...] (Length=%d)', [Fields[i].ArraySize])
      else if Fields[i].FieldType = ctString then
        s1 := '"'+ Fields[i].asString +'"'
      else if Fields[i].FieldType = ctOID then
        s1 := Format('(0x%x)', [Fields[i].asOID])
      else
        s1 := Fields[i].asString;
      Result := Result + Format('%*s%s%*s= %s'#10,    [8, ' ', Fields[i].name, nLen1+1-Length(Fields[i].name), ' ', s1]);
    end;
  end;
begin
  if (IntoTableName <> '') then
    begin
      s := 'insert into ' + IntoTableName;
      if not SameText(FSql, s) then
        FSql := s;
    end;

  Describe;

  // For threaded access attach this thread to the database
  if FSession.Threaded then
    FSession.Attach;

  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_insert(%d, @oid)'#10, [FStatement])+ParamStr, True);
  {$ENDIF}

  try
    rc := cli_insert(FStatement, @Result);
  except
    on e: Exception do
      raise EFastDbError.Create(cli_access_violation, ' cli_insert failed!'#10+ParamStr+#10+e.message);
  end;
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d (oid=0x%x)', [rc, Result]), False);
  {$ENDIF}

  FSession.CliCheck(rc, 'cli_insert failed');

  if FSession.Threaded then
    FSession.Detach;
  //Inc(FRecNo);
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.InternalDescribe;
var
  flds: TFieldDescriptors;
  i,nFields : Integer;
  FieldData : TFastDbField;
  GetProc: TArrayFieldGetValue;
  SetProc: TArrayFieldSetValue;
begin
  FFieldList.Clear;

  nFields := FSession.DescribeTable(GetTableName, flds);
  try
    for i:=0 to nFields-1 do begin // Build the FieldList
      FieldData := TFastDbField.Create(FFieldList);
      with FieldData do begin
        Name := string(flds[i].name);
        FieldType  := TCliVarType(flds[i].FieldType);
        FieldFlags := flds[i].flags;
        RefTable   := string(flds[i].refTableName);
        InverseRefField := string(flds[i].inverseRefFieldName);

        if Assigned(FOnDescribeField) then
          begin
            FOnDescribeField(FieldData, GetProc, SetProc);
            OnArrayGetValue := GetProc;
            OnArraySetValue := SetProc;
          end;
      end;
    end;
  finally
    SetLength(flds, 0);
  end;
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.FreeStatement(const CheckError: Boolean);
var rc : Integer;
begin
  if FStatement > FastDbUnilitializedHandle then
    begin
      // For threaded access detach this thread from the database
      if FSession.Threaded then
        FSession.Detach;

      {$IFDEF CLI_DEBUG}
      TraceDebugProcedure(Format('cli_free(%d)', [FStatement]), True);
      {$ENDIF}
      rc := cli_free(FStatement);
      {$IFDEF CLI_DEBUG}
      TraceDebugProcedure(Format('%d', [rc]), False);
      {$ENDIF}
      if CheckError then
        FSession.CliCheck(rc, 'cli_free failed');

      Fields.UnBindFromStatement;
      Variables.UnBindFromStatement;
      FStatement := FastDbUnilitializedHandle;
      FDescribed := False;
    end;
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.InternalBindFields;
var i: Integer;
begin
  for i:=0 to FFieldList.Count-1 do
    FFieldList[i].BindToStatement(FStatement);
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.InternalBindVariables;
var i: Integer;
begin
  for i:=0 to FVariables.Count-1 do
    FVariables[i].BindToStatement(FStatement);
end;

//---------------------------------------------------------------------------
function TFastDbQuery.Next: Boolean;
var n : Integer;
begin
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_get_next(%d)', [FStatement]), True);
  {$ENDIF}
  n := cli_get_next(FStatement);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [n]), False);
  {$ENDIF}
  FEof := n <> cli_ok;
  FBof := False;
  Result := not FEof;
  if Result then
    Inc(FRecNo);
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.SetSession(ASession: TFastDbSession);
begin
  FSession := ASession;
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.SetSQL(Value: string);
begin
  if Value <> FSql then
    begin
      FSql := Trim(Value);
      FDescribed := False;
      FSqlChanged := True;
    end;
end;

//---------------------------------------------------------------------------
{procedure TFastDbQuery.SetVariables(Value: TFastDbVariables);
begin
  FVariables.Assign(Value);
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.SetFields(const Value: TFastDbFields);
begin
  FFieldList.Assign(Value);
end;
}
//---------------------------------------------------------------------------
function TFastDbQuery.VariableIndex(AName: string): Integer;
var i: Integer;
begin
  for i:=0 to FVariables.Count-1 do
    // Note: StrIComp() works much faster than SameText()
    if StrIComp(PChar(TFastDbVariable(FVariables[i]).Name), PChar(AName)) = 0 then
      begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.First;
var rc : Integer;
begin
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_get_first(%d)', [FStatement]), True);
  {$ENDIF}
  rc := cli_get_first(FStatement);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  FSession.CliCheck(rc, 'cli_get_first failed');
  FBof := True;
  FEof := False;
  FRecNo := 0;
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.Last;
var rc : Integer;
begin
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_get_last(%d)', [FStatement]), True);
  {$ENDIF}
  rc := cli_get_last(FStatement);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  FSession.CliCheck(rc, 'cli_get_last failed');
  FBof := False;
  FEof := True;
  FRecNo := FRowCount;
end;

//---------------------------------------------------------------------------
function TFastDbQuery.GetOID: TCliOid;
begin
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_get_oid(%d)', [FStatement]), True);
  {$ENDIF}
  Result := cli_get_oid(FStatement);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [Result]), False);
  {$ENDIF}
end;

//---------------------------------------------------------------------------
function TFastDbQuery.Prev: Boolean;
var rc : Integer;
begin
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_get_prev(%d)', [FStatement]), True);
  {$ENDIF}
  rc := cli_get_prev(FStatement);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  FBof := rc <> cli_ok;
  FEof := False;
  Result := not FBof;
  if Result then
    Dec(FRecNo);
end;

//---------------------------------------------------------------------------
function TFastDbQuery.Seek(const AOid: TCliOID): Integer;   // returns new RecNo
begin
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_seek(%d)', [FStatement]), True);
  {$ENDIF}
  FRecNo := cli_seek(FStatement, AOid);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [FRecNo]), False);
  {$ENDIF}
  Result := FSession.CliCheck(FRecNo, 'cli_seek failed');
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.Skip(const Records: Integer; const RefreshOnNoSkip: Boolean=False);
var n : Integer;
begin
  if (Records = 0) and not RefreshOnNoSkip then exit;

  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_skip(%d, %d)', [FStatement, Records]), True);
  {$ENDIF}
  n := cli_skip(FStatement, Records);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [n]), False);
  {$ENDIF}
  if n = cli_not_found then
    if Records >= 0 then
      begin
        FEof := True;
        FBof := False;
        FRecNo := FRowCount;
      end
    else
      begin
        FBof := True;
        FEof := False;
        FRecNo := 0;
      end
  else  // raise error if n <> cli_ok
    begin
      FSession.CliCheck(n, 'cli_skip failed');
      Inc(FRecNo, Records);
    end;
end;

//---------------------------------------------------------------------------
function TFastDbQuery.FieldIndex(const Field: string): Integer;
var i: Integer;
begin
  for i := 0 to FFieldList.Count - 1 do
    if StrIComp(PChar(Field), PChar(FFieldList[i].Name)) = 0 then
      begin
        Result := i;
        Exit;
      end;
  Result := -1;
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.Delete;
var rc : Integer;
begin
  // For threaded access attach this thread to the database
  if FSession.Threaded then
    FSession.Attach;

  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_remove(%d)', [FStatement]), True);
  {$ENDIF}
  rc := cli_remove(FStatement);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}

  if FSession.Threaded then
    FSession.Detach;

  FSession.CliCheck(rc, 'cli_remove failed');
  FRecNo := 0;
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.Update;
var rc : Integer;
begin
  // For threaded access attach this thread to the database
  if FSession.Threaded then
    FSession.Attach;

  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_update(%d)', [FStatement]), True);
  {$ENDIF}
  rc := cli_update(FStatement);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}

  FSession.CliCheck(rc, 'cli_update failed');

  if FSession.Threaded then
    FSession.Detach;
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.Freeze;
var rc : Integer;
begin
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_freeze(%d)', [FStatement]), True);
  {$ENDIF}
  rc := cli_freeze(FStatement);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  FSession.CliCheck(rc, 'cli_freeze failed');
end;

//---------------------------------------------------------------------------
procedure TFastDbQuery.UnFreeze;
var rc : Integer;
begin
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_unfreeze(%d)', [FStatement]), True);
  {$ENDIF}
  rc := cli_unfreeze(FStatement);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  FSession.CliCheck(rc, 'cli_unfreeze failed');
end;

//---------------------------------------------------------------------------
// Determine the name of the updating table, used by DML statements
function TFastDbQuery.GetTableName: string;
var s, s1: string;
    i, From: Integer;
    InlineQuery: Boolean;
    {$IFNDEF VER160}
    function PosEx(subStr, str: string; position: Integer): integer;
    begin
      Result := Pos(subStr, Copy(str, position, MaxInt));
      if (Result > 0) and (position > 0) then
        Inc(Result, position-1);
    end;
    {$ENDIF}
begin
  // Was it already determined?
  if not FSqlChanged then
    begin
      Result := FTableName;
      Exit;
    end
  else
    Result := '';

  if FSql = '' then Exit;

  // The tablename is the first identifier after FROM
  s  := RemoveSQLComment(SubstitutedSQL);
  s1 := AnsiLowerCase(s);
  for i := 1 to Length(s) do if not (s[i] in Identifiers + ['(']) then s[i] := ' ';
  i := 1;
  repeat
    From := PosEx(' from ', s1, i);
    if From = 0 then From := PosEx(' from(', s1, i);
    if From = 0 then From := PosEx(' into ', s1, i);
    if From = 0 then Exit;
    FInsertQuery := SameText('insert', Copy(s1, i, 6));
    Inc(From, 5);
    while (From <= Length(s)) and (s[From] = ' ') do Inc(From);
    i := From;
    InlineQuery := (i <= Length(s)) and (s[i] = '(');
    if not InlineQuery then
      begin
        while (i <= Length(s)) and (s[i] in Identifiers) do Inc(i);
        s := Copy(s, From, i - From);
      end;
  until not InlineQuery;

  // Remove quotes and convert to lower case
  Result := StringReplace(s, '"', '', []);
  FTableName := Result;
end;

//---------------------------------------------------------------------------
function TFastDbQuery.Field(const FieldId: Integer): TFastDbField;
begin
  if (FieldId < 0) or (FieldId > FFieldList.Count - 1) then
    raise EFastDbQuery.Create(Format(SFieldDoesntExist, [IntToStr(FieldId)]))
  else
    Result := FFieldList[FieldId];
end;

//---------------------------------------------------------------------------
function TFastDbQuery.Field(const Field: string): TFastDbField;
var i: Integer;
begin
  i := FieldIndex(Field);
  if i = -1 then
    raise EFastDbQuery.Create(Format(SFieldDoesntExist, [Field]))
  else
    Result := FFieldList[i];
end;

//---------------------------------------------------------------------------
function TFastDbQuery.Variable(const Index: Integer): TFastDbVariable;
begin
  if (Index < 0) or (Index > FVariables.Count - 1) then
    raise EFastDbQuery.Create(Format(SVarDoesntExist, [IntToStr(Index)]))
  else
    Result := FVariables[Index];
end;

//---------------------------------------------------------------------------
function TFastDbQuery.Variable(const Name: string): TFastDbVariable;
var i: Integer;
begin
  i := VariableIndex(Name);
  if i = -1 then
    raise EFastDbQuery.Create(Format(SVarDoesntExist, [Name]))
  else
    Result := FVariables[i];
end;

//---------------------------------------------------------------------------
function TFastDbQuery.RecordSize: Integer;
var i : Integer;
begin
  Result := 0;
  for i:=0 to Fields.Count-1 do
    Result := Result + Fields[i].FieldSize;
end;

//---------------------------------------------------------------------------
function TFastDbQuery.IsOpen: Boolean;
begin
  Result := FStatement > FastDbUnilitializedHandle;
end;

end.

