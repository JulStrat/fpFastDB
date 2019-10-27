{-< FastDbSession.pas >---------------------------------------------*
  FastDbSession Version 1.0
    (c) 2002 Serge Aleynikov (serge@hq.idt.net)
  Main Memory Database Management System
  Created:     11/11/2002 Serge Aleynikov (serge@hq.idt.net)
  Last update:
    12/22/2003 Added AlterTable()
    6/4/2003   Added SetTraceFunction()
               Changed TFastDbSessionEvent to TDbSessionEvent type
               Changed TFastDbErrorHandler to TDbErrorHandler type
    5/23/2003  Added GetDatabaseState() method
    4/7/2002   Fixed a bug related to inverse references
    4/2/2003   Added additional debug logging in CreateTable()
    2/4/2002   Added TFastDbSession.Threaded method that controls
               multithreaded access to FastDb Session
    1/22/2003  Added support for cli_attach and cli_detach
    12/24/2002 fixed bug in cli_set_error_handler()
    11/20/2002 First release created and tested.

-------------------------------------------------------------------*
  Database connectivity component
-------------------------------------------------------------------*}
unit FastDbSession;
{$ifdef FPC}{$mode delphi}{$endif}
{$I FastDbConfig.inc}

interface

uses
  SysUtils, Classes, StrUtils, Math, FastDbVar, FastDbCLI,
  {$IFDEF LINUX}
  Types, Libc
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows
  {$ENDIF}
  ;

const
  Identifiers = ['a'..'z', 'A'..'Z', '0'..'9', '_', '#', '$', '.', '"'{, '@', #128..#255}];
  PARAM_CHAR = '%';

type
  TFastDbSession  = class;
  TDbSessionEvent = procedure(Sender: TFastDbSession) of Object;
  TDbTraceEvent   = procedure(Sender: TFastDbSession; Msg: string) of Object;
  TDbErrorHandler = procedure(Sender: TFastDbSession; const ErrorClassCode: Integer;
                                  const Msg: string; const MsgArg: Integer) of Object;

  PContextEntry = ^TContextEntry;
  TContextEntry = record
    ThreadID : DWord;
    RefCount : Integer;
  end;

  TFastDbSession = class(TComponent)
  private
    FLastErrorCode         : Integer;
    FRollbackOnDisconnect  : Boolean;
    FUsername              : string;
    FPassword              : string;
    FDatabase              : string;
    FDatabasePath          : string;
    FHost                  : string;
    FPort                  : Integer;
    FMaxConnectRetries     : Integer;
    FReconnectTimeout      : Integer;
    FInitDatabaseSize      : Integer;
    FInitIndexSize         : Integer;
    FExtensionQuantum      : Integer;
    FFileSizeLimit         : Integer;
    FAutoCommit            : Boolean;
    FOnChange              : TDbSessionEvent;
    FBeforeLogOn           : TDbSessionEvent;
    FAfterLogOn            : TDbSessionEvent;
    FHandle                : Integer;
    FThreadID              : DWord;
    FThreaded              : Boolean;

    FContextList           : TThreadList;

    FReplicationSupport    : Boolean;
    FNodeID                : Integer;
    FNodeNames             : TStrArray;

    FOpenAttributes        : TCliOpenAttributes;
    FTransactionCommitDelay: Integer;
    FTraceHandlerThunk     : TProcedureOfObjectThunk;
    FAssignedErrorHandler  : Boolean;
    FOldErrorHandler       : TCliErrorHandler;
    FOnSessionError        : TDbErrorHandler;
    FOnTraceEvent          : TDbTraceEvent;

    procedure CheckHandle;
    procedure SessionTraceHandler(Msg: PChar); cdecl;
    procedure SetConnected(const Value: Boolean);
    procedure SetLogonUsername(const Value: string);
    procedure SetLogonPassword(const Value: string);
    procedure SetDatabase(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetAutoCommit(const Value: Boolean);
    function  GetConnected: Boolean;
    procedure SetDatabasePath(const Value: string);
    procedure SetHost(const Value: string);
    procedure SetTransactionCommitDelay(const Value: Integer);
    procedure SetMaxConnectRetries(const Value: Integer);
    procedure SetReconnectTimeout(const Value: Integer);
    procedure SetInitDatabaseSize(const Value: Integer);
    procedure InternalOpenDatabase(const AConnectLocal: Boolean);
    procedure SetOnTraceEvent(const Value: TDbTraceEvent);
  protected
    procedure Loaded; override;
    procedure DoBeforeLogon; virtual;
    procedure DoAfterLogon; virtual;
    function  DumpFields(const Fields: TFieldDescriptors; const LeftOffset: Integer=0): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure OpenDatabase(
                    const AServerHost: string='';
                    const AServerPort: Integer=0;
                    const AMaxConnectRetries: Integer=0;
                    const AReconnectTimeout: Integer=FastDbDefReconnectTimeoutSec);

    procedure CreateDatabase(
                    const ADatabaseName: string='';
                    const AFilePath: string='';
                    const AInitDatabaseSize: Integer=FastDbDefaultInitDatabaseSize;
                    const AOpenAttrs: TCliOpenAttributes=[oaReadWrite];
                    const AInitIndexSize: Integer=FastDbDefaultInitIndexSize;
                    const AExtensionQuantum: Integer=FastDbDefaultExtensionQuantum;
                    const AFileSizeLimit: Integer=0;
                    const ATransactionCommitDelay: Word=0);

    procedure CreateReplicatedDatabase(
                    const ANodeID: Integer;
                    const ANodeNames: TStrArray;
                    const ADatabaseName: string='';
                    const AFilePath: string='';
                    const AInitDatabaseSize: Integer=FastDbDefaultInitDatabaseSize;
                    const AOpenAttrs: TCliOpenAttributes=[oaReadWrite];
                    const AInitIndexSize: Integer=FastDbDefaultInitIndexSize;
                    const AExtensionQuantum: Integer=FastDbDefaultExtensionQuantum;
                    const AFileSizeLimit: Integer=0);

    procedure CloseDatabase(const RaiseError: Boolean=True);

    procedure Commit(const Flush: Boolean);
    procedure Rollback;

    procedure ListTables(List: TStringList);
    function  TableExists(const Table: string): Boolean;
    function  DescribeTable(const Table: string; var Fields: TFieldDescriptors; RaiseError: Boolean=True): Integer; overload; // returns field count
    function  DescribeTable(const Table: string; Fields: TFastDbFields; RaiseError: Boolean=True) : Integer; overload; // returns field count
    function  CreateTable(const Table: string; var Fields: TFieldDescriptors; RefCheck: Boolean=False)  : Boolean; overload;
    function  CreateTable(const Table: string; const Fields: TFastDbFields; RefCheck: Boolean=False)          : Boolean; overload;
    procedure DropTable(Table: string);
    procedure AlterIndex(const Table, Field: string; const NewFlags: TIndexTypes=[]);
    function  AlterTable(const Table: string; var Fields: TFieldDescriptors)  : Boolean; overload;
    function  AlterTable(const Table: string; const Fields: TFastDbFields)    : Boolean; overload;
    function  ExtractTableDDL(const TableName: string): string;
    procedure SaveDDLtoFile(FileName: string);

    function  GetDatabaseState: TCliDatabaseMonitor;  // Obtain database status record

    property  Handle: Integer  read FHandle;
    property  ThreadID: DWord  read FThreadID;   // thread which opened the database
    function  CliCheck(const Code: Integer; Msg: string=''; const RaiseError: Boolean=True): Integer;
    function  ErrorMessage(ErrorCode: Integer): string;
    function  ServerVersion: string;

    // Threading support
    procedure Attach;
    procedure Detach(ADetachMode: TDetachModes=[dtPreCommit, dtDestroyContext]);

    {$IFDEF GIGABASE}
    procedure ClearConnectionPool;    // Close all released connection in connection pool
    {$ENDIF}

    // Replication support properties
    property  ReplicationSupport: Boolean read FReplicationSupport;
    property  NodeID: Integer read FNodeID;
  published
    property OnChange   : TDbSessionEvent        read FOnChange          write FOnChange;
    property OnSessionError: TDbErrorHandler     read FOnSessionError    write FOnSessionError;
    property OnTraceEvent: TDbTraceEvent         read FOnTraceEvent      write SetOnTraceEvent;
    property BeforeLogOn: TDbSessionEvent        read FBeforeLogOn       write FBeforeLogOn;
    property AfterLogOn : TDbSessionEvent        read FAfterLogOn        write FAfterLogOn;
    property LogonUsername: string               read FUsername          write SetLogonUsername;
    property LogonPassword: string               read FPassword          write SetLogonPassword;

    property Database: string                    read FDatabase          write SetDatabase;
    property Host: string                        read FHost              write SetHost;
    property Port: Integer                       read FPort              write SetPort;
    property DatabasePath: string                read FDatabasePath      write SetDatabasePath;
    property MaxConnectRetries: Integer          read FMaxConnectRetries write SetMaxConnectRetries;
    property ReconnectTimeout: Integer           read FReconnectTimeout  write SetReconnectTimeout;
    property InitDatabaseSize: Integer           read FInitDatabaseSize  write SetInitDatabaseSize;
    property InitIndexSize: Integer              read FInitIndexSize     write FInitIndexSize;
    property ExtensionQuantum: Integer           read FExtensionQuantum  write FExtensionQuantum;
    property FileSizeLimit: Integer              read FFileSizeLimit     write FFileSizeLimit;
    property TransactionCommitDelay: Integer     read FTransactionCommitDelay write SetTransactionCommitDelay;
    property OpenAttributes: TCliOpenAttributes  read FOpenAttributes    write FOpenAttributes;

    property Connected: Boolean                  read GetConnected       write SetConnected;
    property RollbackOnDisconnect: Boolean       read FRollbackOnDisconnect write FRollbackOnDisconnect;
    property AutoCommit: Boolean                 read FAutoCommit        write SetAutoCommit Stored False;
    property LastErrorCode: Integer              read FLastErrorCode     write FLastErrorCode;
    property Threaded: Boolean                   read FThreaded          write FThreaded;
  end;

  ENotImplemented = class(Exception);

  procedure FindVariables(const SQL: string; IncludeDuplicates: Boolean; var Vars: TStringList);
  function  RemoveSQLComment(const ASQL: string): string;
  procedure SplitSelect(Select: string;
                        var BeforeWhere, WhereClause, AfterWhere, WhereWord: string);
  function AppName: string;

implementation

//---------------------------------------------------------------------------
function AppName: string;
begin
  Result := LowerCase(ExtractFileName(ParamStr(0)));
end;

const
  EOL = {$IFDEF MSWINDOWS}#13#10{$ENDIF}{$IFDEF LINUX}#10{$ENDIF};
  SMultiThreadedAttach = 'In a multi-threaded application %s() method must be called from a thread different from the one that created the FastDB session!';
var
  CNullBuf : array[0..0] of Char = (#0);

//---------------------------------------------------------------------------
procedure FindVariables(const SQL: string; IncludeDuplicates: Boolean; var Vars: TStringList);
var s: string;
    i: Integer;
    Mode: char;
    VarName, EndC: string;
    VarPos: Integer;
begin
  s := SQL + EOL;
  Mode := 'S';
  EndC := '';
  VarPos := 0;
  for i := 1 to Length(s) do
  begin
    case Mode of
      'S' : begin
             if s[i] = PARAM_CHAR then
              begin
                Mode    := 'V';
                VarName := '';
                VarPos  := i;
              end;
              if (S[i] = '''') then
              begin
                Mode := 'Q';
                EndC := '''';
              end;
              if (S[i] = '/') and (S[i + 1] = '*') then
              begin
                Mode := 'C';
                EndC := '*/';
              end;
              if (S[i] = '-') and (S[i + 1] ='-') then
              begin
                Mode := 'C';
                EndC := EOL;
              end;
            end;
      'V' : begin
              if not (s[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '#', '$', #128..#255]) then
              begin
                VarName := LowerCase(VarName);
                if (VarName <> '') and (IncludeDuplicates or (Vars.IndexOf(VarName) < 0)) then
                  Vars.AddObject(VarName, TObject(VarPos));
                Mode := 'S';
              end else
                VarName := VarName + s[i];
            end;
      'C' : if (S[i] = EndC[1]) and (S[i + 1] = EndC[2]) then mode := 'S';
      'Q' : if (S[i] = EndC[1]) then mode := 'S';
    end;
  end;
end;

// Remove SQL Comment from a string
function RemoveSQLComment(const ASQL: string): string;
var i, l: Integer;
    c1, c2, Mode: char;
begin
  Result := '';
  l := Length(ASQL);
  i := 1;
  Mode := 'N';
  while i <= l do begin
    c1 := ASQL[i];
    if c1 = '''' then
    begin
      if Mode = 'Q' then
        Mode := 'N'
      else if Mode = 'N' then
        Mode := 'Q';
    end;
    if Mode = 'Q' then Result := Result + c1;
    if i < l then c2 := ASQL[i + 1] else c2 := #0;
    if Mode = 'N' then
    begin
      if (c1 = '/') and (c2 = '*') then Mode := '*';
      if (c1 = '-') and (c2 = '-') then Mode := '-';
      if Mode = 'N' then Result := Result + c1;
    end;
    if ((Mode = '*') and (c1 = '*') and (c2 = '/')) or
       ((Mode = '-') and (c1 in [#13, #10])) then
    begin
      Mode := 'N';
      Inc(i);
    end;
    Inc(i);
  end;
  Result := Trim(Result);
end;

function RemoveParenthesisAndQuotes(const ASQL: string): string;
var i, pLevel: Integer;
    qMode: Boolean;
begin
  Result := ASQL;
  // Discard text between parnthesis and quotes
  pLevel := 0;
  qMode := False;
  for i := 1 to Length(Result) do
  begin
    if Result[i] = '(' then Inc(pLevel);
    if Result[i] = ')' then Dec(pLevel);
    if Result[i] = '''' then qMode := not qMode;
    if ((pLevel > 0) or qMode) and not (Result[i] in ['(', ')', '''']) then
      Result[i] := '_';
  end;
end;

// Find a whole word in a string
function WordPos(const AWord, AString: string): Integer;
var s: string;
    i, p: Integer;
begin
  s := ' ' + LowerCase(AString) + ' ';
  for i := 1 to Length(s) do if not (s[i] in Identifiers) then s[i] := ' ';
  p := Pos(' ' + LowerCase(AWord) + ' ', s);
  Result := p;
end;

// Split the select statement into 3 parts: select_part1 where_part2 order_by_part3
procedure SplitSelect(Select: string;
                      var BeforeWhere, WhereClause, AfterWhere, WhereWord: string);
var p0, p1, p2: Integer;
    ncSelect, ucSelect: string;
begin
  // Remove comments
  ncSelect := RemoveSQLComment(Select);
  // Convert to LowerCase
  ucSelect := LowerCase(ncSelect);
  // Discard text between parnthesis and quotes
  ucSelect := RemoveParenthesisAndQuotes(ucSelect);
  p0 := WordPos('group', ucSelect);
  if p0 > 0 then WhereWord := 'having' else WhereWord := 'where';
  p1 := WordPos(LowerCase(WhereWord), ucSelect);
  p2 := WordPos('order', ucSelect);
  if p2 <= 0 then p2 := WordPos('for', ucSelect);
  if p2 > 0 then
    AfterWhere := Copy(ncSelect, p2, Length(ncSelect))
  else begin
    AfterWhere := '';
    p2 := Length(ncSelect) + 1;
  end;
  if p1 > 0 then
  begin
    BeforeWhere := Copy(ncSelect, 1, p1 - 1);
    WhereClause := Copy(ncSelect, p1 + Length(WhereWord), p2 - p1 - Length(WhereWord));
  end else begin
    BeforeWhere := Copy(ncSelect, 1, p2 - 1);
    WhereClause := '';
  end;
  BeforeWhere := Trim(BeforeWhere);
  WhereClause := Trim(WhereClause);
  AfterWhere  := Trim(AfterWhere);
end;

//---------------------------------------------------------------------------
procedure SessionErrorHandler(ErrorClassCode: Integer;
  const Msg: PChar; MsgArg: Integer; const UserData: Pointer); cdecl;
var
  s : TFastDbSession absolute UserData;
begin
  Assert(s <> nil, 'UserData must be assign TFastDbSession value!');
  if Assigned(s.OnSessionError) then
    s.OnSessionError(s, ErrorClassCode-100, string(Msg), MsgArg);
  // This procedure must raise an error to unwind the stack
  raise EFastDbError.Create(ErrorClassCode-100, string(Msg)+Format(' (%d)', [MsgArg]));
end;

//---------------------------------------------------------------------------
// TFastDbSession
//---------------------------------------------------------------------------
constructor TFastDbSession.Create(AOwner: TComponent);
begin
  inherited;
  FTraceHandlerThunk    := CreateProcedureOfObjectThunk(Self, @TFastDbSession.SessionTraceHandler);

  FRollbackOnDisconnect := True;
  if not (csDesigning in ComponentState) then
    begin
      FDatabase         := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
      FDatabasePath     := ExtractFilePath(ParamStr(0));
    end;
  FHost                 := 'localhost';
  FPort                 := FastDbDefaultDatabasePort;
  FMaxConnectRetries    := 5;
  FInitDatabaseSize     := FastDbDefaultInitDatabaseSize;
  FInitIndexSize        := FastDbDefaultInitIndexSize;
  FExtensionQuantum     := FastDbDefaultExtensionQuantum;
  FFileSizeLimit        := 0;

  {$WARNINGS OFF}
  FReconnectTimeout     := FastDbDefReconnectTimeoutSec;
  {$WARNINGS ON}
  FHandle               := FastDbUnilitializedHandle;

  FAutoCommit             := True;
  FOpenAttributes         := [oaReadWrite];
  FTransactionCommitDelay := 0;

  FContextList := TThreadList.Create;
end;

//---------------------------------------------------------------------------
destructor TFastDbSession.Destroy;
var i : Integer;
begin
  with FContextList.LockList do
  try
    for i:=Count-1 downto 0 do
      Dispose(PContextEntry(Items[i]));
    Clear;
  finally
    FContextList.UnlockList;
    FContextList.Free;
  end;

  if FHandle <> FastDbUnilitializedHandle then
    CloseDatabase(False);
  inherited;
end;

//---------------------------------------------------------------------------
function TFastDbSession.CliCheck(const Code: Integer; Msg: string='';
  const RaiseError: Boolean=True): Integer;
begin
  Result := Code;
  if Code < 0 then
    begin
      FLastErrorCode := Code;
      if RaiseError then
        raise EFastDbError.Create(Code, Msg);
    end;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.Commit(const Flush: Boolean);
var rc : Integer;
begin
  CheckHandle;
  {$IFDEF CLI_DEBUG}
  if Flush then TraceDebugProcedure(Format('cli_commit(%d)', [FHandle]), True)
           else TraceDebugProcedure(Format('cli_precommit(%d)', [FHandle]), True);
  {$ENDIF}
  if Flush then rc := cli_commit(FHandle)
           else rc := cli_precommit(FHandle);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  CliCheck(rc, 'cli_commit failed');
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.DoAfterLogon;
begin
  if Assigned(FAfterLogOn) then
    FAfterLogOn(Self);
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.DoBeforeLogon;
begin
  if Assigned(FBeforeLogOn) then
    FBeforeLogOn(Self);
end;

//---------------------------------------------------------------------------
function TFastDbSession.ErrorMessage(ErrorCode: Integer): string;
begin
  Result := CliErrorToStr(ErrorCode);
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.Loaded;
begin
  inherited;
  if FHandle > FastDbUnilitializedHandle then
    begin
      DoAfterLogon;
      if Assigned(FOnChange) then FOnChange(Self);
    end;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.CloseDatabase(const RaiseError: Boolean=True);
var rc : Integer;
begin
  if FHandle > FastDbUnilitializedHandle then
    try
      if FAssignedErrorHandler then
        try
          cli_set_error_handler(FHandle, FOldErrorHandler, Self);
        finally
          FAssignedErrorHandler := False;
        end;

      {$IFDEF CLI_DEBUG}
      TraceDebugProcedure(Format('cli_close(%d)', [FHandle]), True);
      {$ENDIF}
      rc := cli_close(FHandle);
      {$IFDEF CLI_DEBUG}
      TraceDebugProcedure(Format('%d', [rc]), False);
      {$ENDIF}
      CliCheck(rc, 'cli_close failed', RaiseError);
    finally
      FHandle := FastDbUnilitializedHandle;
    end;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.InternalOpenDatabase(const AConnectLocal: Boolean);
var n : Integer;
begin
  if FHandle > FastDbUnilitializedHandle then
    begin
      {$IFDEF CLI_DEBUG}
      TraceDebugProcedure(Format('cli_close(%d)', [FHandle]), True);
      n :=
      {$ENDIF}
      cli_close(FHandle);
      {$IFDEF CLI_DEBUG}
      TraceDebugProcedure(Format('%d', [n]), False);
      {$ENDIF}
      FHandle := FastDbUnilitializedHandle;
    end;

  DoBeforeLogon;

  if not AConnectLocal then
    n := cli_open(Format('%s:%d', [FHost, FPort]), FMaxConnectRetries, FReconnectTimeout)
  else
    n := cli_bad_address;

  if n = cli_bad_address then
    if FReplicationSupport then
      n := CliCheck(cli_create_replication_node(
                                     FNodeID,
                                     Length(FNodeNames),
                                     FNodeNames,
                                     FDatabase,
                                     FDatabasePath,
                                     FInitDatabaseSize,
                                     FOpenAttributes,
                                     FInitIndexSize,
                                     FExtensionQuantum,
                                     FFileSizeLimit), 'cli_create_replication_node failed')
    else
      n := CliCheck(cli_create(FDatabase,
                                     FDatabasePath,
                                     FInitDatabaseSize,
                                     FTransactionCommitDelay,
                                     FOpenAttributes,
                                     FInitIndexSize,
                                     FExtensionQuantum,
                                     FFileSizeLimit), 'cli_create failed')
  else if n < 0 then
    raise EFastDbError.Create(FHandle, 'cli_open failed');

  FHandle := n;
  FThreadID := GetCurrentThreadID;

  if {Assigned(FOnSessionError) and } not FAssignedErrorHandler then
    begin
      FOldErrorHandler := cli_set_error_handler(FHandle, @SessionErrorHandler, Self);
      FAssignedErrorHandler := True;
    end;

  // Turn on tracing if it is assigned
  if Assigned(FOnTraceEvent) then
    SetOnTraceEvent(FOnTraceEvent);

  DoAfterLogon;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.OpenDatabase(const AServerHost: string='';
  const AServerPort: Integer=0; const AMaxConnectRetries: Integer=0;
  const AReconnectTimeout: Integer=FastDbDefReconnectTimeoutSec);
begin
  if FHandle > FastDbUnilitializedHandle then // already connected
    raise EFastDbError.Create(cli_database_already_open)
  else
    begin
      if AServerHost <> ''                                  then FHost              := AServerHost;
      if AServerPort <> 0                                   then FPort              := AServerPort;
      if AMaxConnectRetries <> 0                            then FMaxConnectRetries := AMaxConnectRetries;
      if AReconnectTimeout  <> FastDbDefReconnectTimeoutSec then FReconnectTimeout  := AReconnectTimeout;

      FReplicationSupport := False;

      InternalOpenDatabase(False);
    end;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.CreateDatabase(const ADatabaseName: string;
  const AFilePath: string;
  const AInitDatabaseSize: Integer;
  const AOpenAttrs: TCliOpenAttributes;
  const AInitIndexSize: Integer;
  const AExtensionQuantum: Integer;
  const AFileSizeLimit: Integer;
  const ATransactionCommitDelay: Word
  );
begin
  if FHandle > FastDbUnilitializedHandle then // already connected
    raise EFastDbError.Create(cli_database_already_open)
  else
    begin
      if ADatabaseName <> ''                                then FDatabase               := ADatabaseName;
      if AFilePath <> ''                                    then FDatabasePath           := AFilePath;
      if AInitDatabaseSize <> FastDbDefaultInitDatabaseSize then FInitDatabaseSize       := AInitDatabaseSize;
      if ATransactionCommitDelay <> 0                       then FTransactionCommitDelay := ATransactionCommitDelay;
      if AOpenAttrs <> [oaReadWrite]                        then FOpenAttributes         := AOpenAttrs;
      if AInitIndexSize <> FastDbDefaultInitIndexSize       then FInitIndexSize          := AInitIndexSize;
      if AExtensionQuantum <> FastDbDefaultExtensionQuantum then FExtensionQuantum       := AExtensionQuantum;
      if AFileSizeLimit <> 0                                then FFileSizeLimit          := AFileSizeLimit;

      FReplicationSupport := False;

      InternalOpenDatabase(True);
    end;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.CreateReplicatedDatabase(const ANodeID: Integer;
  const ANodeNames: TStrArray;
  const ADatabaseName, AFilePath: string; const AInitDatabaseSize: Integer;
  const AOpenAttrs: TCliOpenAttributes; const AInitIndexSize,
  AExtensionQuantum, AFileSizeLimit: Integer);
begin
  if FHandle > FastDbUnilitializedHandle then // already connected
    raise EFastDbError.Create(cli_database_already_open)
  else
    begin
      if ADatabaseName <> ''                                then FDatabase         := ADatabaseName;
      if AFilePath <> ''                                    then FDatabasePath     := AFilePath;
      if AInitDatabaseSize <> FastDbDefaultInitDatabaseSize then FInitDatabaseSize := AInitDatabaseSize;
      if AOpenAttrs <> [oaReadWrite]                        then FOpenAttributes   := AOpenAttrs;
      if AInitIndexSize <> FastDbDefaultInitIndexSize       then FInitIndexSize    := AInitIndexSize;
      if AExtensionQuantum <> FastDbDefaultExtensionQuantum then FExtensionQuantum := AExtensionQuantum;
      if AFileSizeLimit <> 0                                then FFileSizeLimit    := AFileSizeLimit;

      FNodeID := ANodeID;
      FNodeNames := ANodeNames;
      FTransactionCommitDelay := 0;   // TransactionCommitDelay is not supported in the replicated database
      FReplicationSupport := True;

      InternalOpenDatabase(True);
    end;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.Rollback;
var rc : Integer;
begin
  CheckHandle;
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_abort(%d)', [FHandle]), True);
  {$ENDIF}
  rc := cli_abort(FHandle);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  CliCheck(rc, 'cli_abort failed');
end;

//---------------------------------------------------------------------------
function TFastDbSession.ServerVersion: string;
begin
  Result := FastDbCli.Version;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetAutoCommit(const Value: Boolean);
begin
  if FAutoCommit <> Value then
    FAutoCommit := Value;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetConnected(const Value: Boolean);
begin
  if Connected <> Value then
    if Value then
      InternalOpenDatabase(True)
    else
      CloseDatabase;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetDatabase(const Value: string);
begin
  if FDatabase <> Value then
    FDatabase := Value;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetPort(const Value: Integer);
begin
  if FPort <> Value then
    FPort := Value;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetLogonPassword(const Value: string);
begin
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetLogonUsername(const Value: string);
begin
end;

//---------------------------------------------------------------------------
function TFastDbSession.GetConnected: Boolean;
begin
  Result := FHandle > FastDbUnilitializedHandle;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetDatabasePath(const Value: string);
begin
  if FDatabasePath <> Value then
    FDatabasePath := Value;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetHost(const Value: string);
begin
  if FHost <> Value then
    FHost := Value;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetTransactionCommitDelay(const Value: Integer);
begin
  if FTransactionCommitDelay <> Value then
    FTransactionCommitDelay := Value;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetMaxConnectRetries(const Value: Integer);
begin
  if FMaxConnectRetries <> Value then
    FMaxConnectRetries := Value;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetReconnectTimeout(const Value: Integer);
begin
  if FReconnectTimeout <> Value then
    FReconnectTimeout := Value;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetInitDatabaseSize(const Value: Integer);
begin
  if FInitDatabaseSize <> Value then
    FInitDatabaseSize := Value;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.ListTables(List: TStringList);
var
  i, n : Integer;
  tbl, tables: PCliTableDescriptor;
  {$IFDEF CLI_DEBUG}
  s : string;
  {$ENDIF}
begin
  List.Clear;
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_show_tables(%d, &tables)', [FHandle]), True);
  s := '';
  {$ENDIF}
  n := cli_show_tables(FHandle, tables);
  try
    CliCheck(n, 'cli_show_tables failed');
    tbl := tables;
    for i:=0 to n-1 do begin
      List.Add(tbl^.name);
      {$IFDEF CLI_DEBUG}
      s := s + Format(',"%s"', [tbl^.name]);
      {$ENDIF}
      Inc(tbl);
    end;
  finally
    if n > 0 then
      cli_free_memory(FHandle, tables);
  end;
  {$IFDEF CLI_DEBUG}
  if s <> '' then Delete(s, 1, 1);
  TraceDebugProcedure(Format('%d tables=0x%p (%s)', [n, tables, s]), False);
  {$ENDIF}
end;

//---------------------------------------------------------------------------
function TFastDbSession.TableExists(const Table: string): Boolean;
var
  str : TStringList;
begin
  str := TStringList.Create;
  try
    ListTables(str);
    Result := str.IndexOf(Table) <> -1;
  finally
    str.Free;
  end;
end;

//---------------------------------------------------------------------------
function TFastDbSession.DumpFields(const Fields: TFieldDescriptors;
  const LeftOffset: Integer): string;
var
  m, n, k, nLen1, nLen2, nLen3 : Integer;
  s2, s3: string;
  function NullQQ(str: string): string;
  begin
    if str='' then Result := 'NULL' else Result := '"'+str+'"';
  end;

  function NilLen(s: PChar; Default: Integer): Integer;
  begin
    if s = nil then
      Result := Default
    else
      Result := strlen(s)+2;
  end;
begin
  Result := '';
  if (Fields = nil) then exit;
  nLen1 := 0;
  nLen2 := 0;
  nLen3 := 0;
  for n:=0 to High(Fields) do begin
    nLen1 := Max(nLen1, Length(CliVarTypeAsStr(OrdToCliType(Fields[n].FieldType))));
    nLen2 := Max(nLen2, NilLen(Fields[n].name,4));
    nLen3 := Max(nLen3, NilLen(Fields[n].refTableName,4));
  end;
  for n:=0 to High(Fields) do begin
    s3 := NullQQ(Fields[n].inverseRefFieldName);
    s2 := NullQQ(Fields[n].refTableName);
    m := Max(0, NilLen(Fields[n].name,4));
    k := Max(0, NilLen(Fields[n].refTableName,4));
    Result := Result +
            Format('%*s{%-2d/*%s*/%*s %d,"%s"%*s%s%*s%s}%s', [
                     LeftOffset, ' ',
                     Fields[n].FieldType, CliVarTypeAsStr(OrdToCliType(Fields[n].FieldType)),
                     nLen1+1-Length(CliVarTypeAsStr(OrdToCliType(Fields[n].FieldType))), ',',
                     Fields[n].flags,
                     Fields[n].name, nLen2+1-m, ',',
                     s2, nLen3+1-k, ',',
                     s3,
                     ifthen(n=High(Fields), '})', #10)
                   ]);
  end;
end;

//---------------------------------------------------------------------------
function TFastDbSession.DescribeTable(const Table: string; var Fields: TFieldDescriptors;
  RaiseError: Boolean=True): Integer;
var
  p : PCliFieldDescriptor;
{$IFDEF CLI_DEBUG}
  s : string;
{$ENDIF}
begin
  CheckHandle;
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_describe(%d, "%s", [...])', [FHandle, Table]), True);
  {$ENDIF}

  Result := cli_describe(FHandle, PChar(Table), @p);

  if RaiseError then
    CliCheck(Result, 'cli_describe failed');
  if Result > 0 then
    try
      SetLength(Fields, Result);
      Move(p^, Fields[0], Result*SizeOf(TCliFieldDescriptor));
      {$IFDEF CLI_DEBUG}
      s := #10+DumpFields(Fields, 4);
      {$ENDIF}
    finally
      cli_free_memory(FHandle, p);
    end
  else
    begin
      //SetLength(Fields, 0);
      {$IFDEF CLI_DEBUG}
      s := '';
      {$ENDIF}
    end;

  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d %s', [Result, s]), False);
  {$ENDIF}
end;

//---------------------------------------------------------------------------
function TFastDbSession.DescribeTable(const Table: string; Fields: TFastDbFields;
  RaiseError: Boolean=True): Integer;
var
  flds : TFieldDescriptors;
  i    : Integer;
begin
  Result := DescribeTable(Table, flds, RaiseError);
  try
    Fields.Clear;
    for i:=0 to Result-1 do begin
      Fields.Add(string(flds[i].name),
                 OrdToCliType(flds[i].FieldType),
                 FieldFlagsToIndexTypes(flds[i].flags),
                 string(flds[i].refTableName),
                 string(flds[i].inverseRefFieldName));
    end;
  finally
    if Result > 0 then
      SetLength(flds, 0);
  end;
end;

//---------------------------------------------------------------------------
function TFastDbSession.CreateTable(const Table: string;
  var Fields: TFieldDescriptors; RefCheck: Boolean=False): Boolean;
var
  n,m : Integer;
  s : PChar;
  bHasReferences : Boolean;
  flds: TFieldDescriptors;
  bOk : Boolean;
  {$IFDEF CLI_DEBUG}
  s1 : string;
  {$ENDIF}
begin
  CheckHandle;

  s := PChar(Table);
  bHasReferences := False;

  // The following check is performed to ensure that inverseRefFieldNames for
  // non-existing tables are NULL'ed.  This is a FastDB requirement.
  flds := nil;
  try
    for n:=0 to High(Fields) do
      if Fields[n].refTableName <> '' then
        begin
          bHasReferences := True;
          if Fields[n].inverseRefFieldName <> '' then begin
            if RefCheck and (DescribeTable(Fields[n].refTableName, flds, False) > 0) then
              begin
                bOk := False;
                for m:=0 to High(flds) do
                  if SameText(flds[m].name, Fields[n].inverseRefFieldName) then
                    begin
                      bOk := SameText(flds[m].refTableName, Table) and
                             SameText(flds[m].inverseRefFieldName, Fields[n].name);
                      if not bOk then
                        raise EFastDbError.Create(cli_wrong_inverse_reference,
                                                  Format('%s.%s[%s.%s] <-mismatch-> %s.%s[%s.%s]', [
                                                     Table, Fields[n].name,
                                                         Fields[n].refTableName, Fields[n].inverseRefFieldName,
                                                     Fields[n].refTableName, flds[m].name,
                                                         flds[m].refTableName, flds[m].inverseRefFieldName
                                                  ]));
                      break;
                    end;
                if not bOk then
                  raise EFastDbError.Create(cli_wrong_inverse_reference,
                                                  Format('%0:s.%1:s[%2:s.%3:s] inverse reference not found in %2:s.%3:s', [
                                                     Table, Fields[n].name,
                                                         Fields[n].refTableName, Fields[n].inverseRefFieldName
                                                  ]));
              end;
          end;
        end;
  finally
    flds := nil;
  end;

  {$IFDEF CLI_DEBUG}
  s1 := Format('cli_create_table(%d, "%s", %d, {'#10, [FHandle, s, Length(Fields)]) +
        DumpFields(Fields, 4);
  TraceDebugProcedure(s1, True);
  {$ENDIF}
  FLastErrorCode := cli_create_table(FHandle, s, Length(Fields), @Fields[0]);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [FLastErrorCode]), False);
  {$ENDIF}
  if (FLastErrorCode = cli_ok) or (FLastErrorCode = cli_table_not_found) then
    Result := True
  else if bHasReferences and (FLastErrorCode = cli_table_not_found) then
    // This is a special case when an inverse reference is declared and no table being referenced is
    // yet defined.  This is not considered an error.
    Result := True
  else if (FLastErrorCode = cli_table_already_exists) {or (FLastErrorCode = cli_not_implemented)} then
    Result := False
  else
    raise EFastDbError.Create(FLastErrorCode, 'cli_create_table failed');
end;

//---------------------------------------------------------------------------
function TFastDbSession.CreateTable(const Table: string; const Fields: TFastDbFields; RefCheck: Boolean=False): Boolean;
var
  fld : TFieldDescriptors;
  i : Integer;
begin
  if Fields.Count = 0 then
    raise EFastDbError.Create(cli_no_fields_defined, 'Fields parameter has no elements!');

  SetLength(fld, Fields.Count);
  try
    for i:=0 to High(fld) do
      fld[i] := FastDbFieldToFieldDescriptor(Fields[i]);
    Result := CreateTable(Table, fld, RefCheck);
  finally
    SetLength(fld, 0);
  end;
end;

//---------------------------------------------------------------------------
function TFastDbSession.AlterTable(const Table: string; var Fields: TFieldDescriptors): Boolean;
  {$IFDEF CLI_DEBUG}
var
  s : string;
  {$ENDIF}
begin
  CheckHandle;

  {$IFDEF CLI_DEBUG}
  s := Format('cli_alter_table(%d, "%s", %d, {'#10, [FHandle, Table, Length(Fields)]) + DumpFields(Fields, 4);
  TraceDebugProcedure(s, True);
  {$ENDIF}
  FLastErrorCode := cli_alter_table(FHandle, PChar(Table), Length(Fields), @Fields[0]);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [FLastErrorCode]), False);
  {$ENDIF}
  if (FLastErrorCode = cli_ok) then
    Result := True
  else
    raise EFastDbError.Create(FLastErrorCode, 'cli_create_table failed');
end;

//---------------------------------------------------------------------------
function TFastDbSession.AlterTable(const Table: string; const Fields: TFastDbFields): Boolean;
var
  fld : TFieldDescriptors;
  i : Integer;
begin
  if Fields.Count = 0 then
    raise EFastDbError.Create(cli_no_fields_defined, 'Fields parameter has no elements!');

  SetLength(fld, Fields.Count);
  try
    for i:=0 to High(fld) do
      fld[i] := FastDbFieldToFieldDescriptor(Fields[i]);
    Result := AlterTable(Table, fld);
  finally
    SetLength(fld, 0);
  end;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.DropTable(Table: string);
var rc : Integer;
begin
  CheckHandle;
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_drop_table(%d, "%s")', [FHandle, Table]), True);
  {$ENDIF}
  rc := cli_drop_table(FHandle, PChar(Table));
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  CliCheck(rc, 'cli_drop_table failed');
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.AlterIndex(const Table, Field: string;
  const NewFlags: TIndexTypes);
var
  rc,n : Integer;
begin
  CheckHandle;
  n := IndexTypesToFieldFlags(NewFlags);

  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_alter_index(%d, "%s", "%s", %d)', [FHandle, Table, Field, n]), True);
  {$ENDIF}
  rc := cli_alter_index(FHandle, PChar(Table), PChar(Field), n);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  CliCheck(rc, 'cli_alter_index failed');
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.Attach;
var
  rc: Integer;
{  i : Integer;
  bFound : Boolean;
  p : PContextEntry;
  nThreadID : DWord;}
begin
  {nThreadID := GetCurrentThreadID;

  if nThreadID = ThreadID then
    raise EFastDbError.CreateFmt(SMultiThreadedAttach, ['Attach']);

  with FContextList.LockList do
  try
    bFound := False;
    if FThreaded then
      for i:=0 to Count-1 do
        with PContextEntry(Items[i])^ do
          if ThreadID = nThreadID then
            begin
              Inc(RefCount);
              bFound := True;
              break;
            end;

    if not bFound then
      begin
        if FThreaded then
          begin
            New(p);
            p^.ThreadID := nThreadID;
            p^.RefCount := 1;
            Add(p);
          end;
   }
        {$IFDEF CLI_DEBUG}
        TraceDebugProcedure(Format('cli_attach(%d)', [FHandle]), True);
        {$ENDIF}
        rc := cli_attach(FHandle);
        {$IFDEF CLI_DEBUG}
        TraceDebugProcedure(Format('%d', [rc]), False);
        {$ENDIF}
        CliCheck(rc, 'cli_attach failed');
  {    end;
  finally
    FContextList.UnlockList;
  end;}
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.Detach(ADetachMode: TDetachModes);
//var
  //i  : Integer;
  //bDoDetach : Boolean;
  //nThreadID: DWord;
begin
  {nThreadID := GetCurrentThreadID;

  if nThreadID = ThreadID then
    raise EFastDbError.CreateFmt(SMultiThreadedAttach, ['Detach']);

  with FContextList.LockList do
  try
    nDisconnectMode := 0; // cli_commit_on_detach;  <-- This one does hard commit, 0 does precommit;
    bDoDetach := not FThreaded;

    if FThreaded then
      for i:=0 to Count-1 do
        with PContextEntry(Items[i])^ do
          if ThreadID = nThreadID then
            begin
              Dec(RefCount);
              if RefCount <= 0 then
                begin
                  Dispose(PContextEntry(Items[i]));
                  Delete(i);
                  nDisconnectMode := cli_destroy_context_on_detach or nDisconnectMode;
                  bDoDetach := True;
                end;
              break;
            end;

    if bDoDetach then
      begin}
        cli_detach(FHandle, ADetachMode);
{      end;
  finally
    FContextList.UnlockList;
  end;}
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.CheckHandle;
begin
  if FHandle = -1 then
    raise EFastDbError.Create(cli_session_not_assigned);
end;

//---------------------------------------------------------------------------
function TFastDbSession.ExtractTableDDL(const TableName: string): string;
const
  IDX_STR = 'create %s on %s.%s;'#10;
var
  Fields: TFieldDescriptors;
  i,n,nLen : Integer;
  it : TIndexTypes;
begin
  SetLength(Result, 20*1048);
  SetLength(Fields, 100);
  n := DescribeTable(TableName, Fields);
  Result := 'create table '+TableName + '('#10;
  nLen := 0;
  for i:=0 to n-1 do
    nLen := Max(nLen, Length(Fields[i].name));
  for i:=0 to n-1 do begin
    Result := Result + Format('%*s%-*s %s%s%s'#10,
                                [10, ' ', nLen, Fields[i].name,
                                 CliVarTypeAsStr(OrdToCliType(Fields[i].FieldType), True),
                                 iif(Fields[i].refTableName <> '', ' to '+Fields[i].refTableName, ''),
                                 iif(i=(n-1), '', ',')]);
  end;
  Result := Result + ');'#10;
  for i:=0 to n-1 do begin
    it := FieldFlagsToIndexTypes(Fields[i].flags);
    if itHash in it then Result := Result + Format(IDX_STR, ['hash',  TableName, Fields[i].name]);
    if itTree in it then Result := Result + Format(IDX_STR, ['index', TableName, Fields[i].name]);
  end;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SaveDDLtoFile(FileName: string);
var
  str : TStringList;
  s   : string;
  i   : Integer;
begin
  with TFileStream.Create(FileName, fmCreate) do
  try
    str := TStringList.Create;
    try
      ListTables(str);
      s := Format('open ''%s'';'#10, [FDatabase]);
      Write(s[1], Length(s));
      for i:=0 to str.Count-1 do begin
        s := ExtractTableDDL(str[i]);
        Write(s[1], Length(s));
      end;
      s := 'commit;'#10;
      Write(s[1], Length(s));
      s := 'exit;'#10;
      Write(s[1], Length(s));
    finally
      str.Free;
    end;
  finally
    Free;
  end;
end;

//---------------------------------------------------------------------------
function TFastDbSession.GetDatabaseState: TCliDatabaseMonitor;
var rc : Integer;
begin
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_get_database_state', [FHandle]), True);
  {$ENDIF}
  rc := cli_get_database_state(FHandle, @Result);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  CliCheck(rc, 'cli_get_database_state failed');
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SetOnTraceEvent(const Value: TDbTraceEvent);
begin
  FOnTraceEvent := Value;
  if FHandle <> -1 then
    begin
      {$IFDEF CLI_DEBUG}
      TraceDebugProcedure(Format('cli_set_trace_function', [FHandle]), True);
      {$ENDIF}
      if Assigned(Value) then
        cli_set_trace_function(@FTraceHandlerThunk)
      else
        cli_set_trace_function(nil);
      {$IFDEF CLI_DEBUG}
      TraceDebugProcedure(Format('%d', [cli_ok]), False);
      {$ENDIF}
    end;
end;

//---------------------------------------------------------------------------
procedure TFastDbSession.SessionTraceHandler(Msg: PChar);
begin
  if Assigned(FOnTraceEvent) then
    FOnTraceEvent(Self, string(Msg));
end;

{$IFDEF GIGABASE}
//---------------------------------------------------------------------------
procedure TFastDbSession.ClearConnectionPool;
var rc : Integer;
begin
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_clear_connection_pool', [FHandle]), True);
  {$ENDIF}
  rc := cli_clear_connection_pool;
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [rc]), False);
  {$ENDIF}
  CliCheck(rc, 'cli_clear_connection_pool failed');
end;
{$ENDIF}

end.


