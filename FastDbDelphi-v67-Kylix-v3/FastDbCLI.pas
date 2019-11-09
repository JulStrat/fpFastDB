{-< CLI.H >--------------------------------------------------------*
  Main Memory Database Management System

  FastDbCLI - Delphi/Kylix FastDB API
    Call level interface to FastDB Main Memory DBMS
    Serge Aleynikov <serge@hq.idt.net>

  Created:
    11/11/2002 Serge Aleynikov (serge@hq.idt.net)
  Last update:
    Version 3.01
    6/24/2004  Verified compliance with FastDB 2.88
               Added a context parameter to callback functions of
               cli_column_set_ex, cli_column_get_ex
    Version 2.88
    1/08/2004  Verified compliance with FastDB 2.88
               Added cli_get_field_size(), cli_get_field_offset()
    Version 2.86
    12/22/2003 Verified compliance with FastDB 2.86
               Added cli_alter_table(), TFastDbSession.AlterTable()
    Version 2.74
    8/8/2003   Verified compliance with FastDB 2.74
    Version 2.72
    7/14/2003  Verified compliance with FastDB 2.72
               Added functions: cli_prepare_query(),
                                cli_execute_query(),
                                cli_insert_struct()
    Version 2.69
    6/20/2003  Verified compliance with FastDB 2.69
    Version 2.68
    6/12/2003  Added compliance with FastDB 2.68 and Gigabase 2.90
    Version 2.66
    6/4/2003   Added compliance with FastDB 2.65
               Introduced cli_set_trace_function()
    Version 2.65
    5/28/2003  Added compliance with FastDB 2.65
               Introduced cli_get_database_state
    Version 2.61.2:
    5/11/2003  Fixed a problem with
               TFastDbVar:TFieldBufferItem.GetArrayAsSingle/Double
               fetching wrong data from array and occasuonally
               causing index out of bounds error.
               Also, TFieldBufferItem.FetchData now defaults to True
    5/09/2003  Fixed a compilation issue on Linux/Kylix3
               It turns out that Kylix3 doesn't like Pointer of Object
               declarations.
               Fixed an AV when using library with a FastDb DLL of wrong
               version.
    Version 2.61:
    4/20/2003  Added compatibility with FastDB 2.61
    3/31/2003  Added compatibility with GigaBase
    3/30/2003  Added compatibility with FastDB 2.59
               Fixed an AV in case of loading a DLL that's not found in the path.
    2/4/2003   Added EFastDbError.Create(Msg: string) constructor
    1/22/2003  Added support for cli_attach and cli_detach
    1/3/2003   Fixed the Access Violation problem with
               cli_set_error_handler
    12/24/2002 fixed bug in cli_set_error_handler()
    12/20/2002 Added new FastDb 2.56 features.
               cli_freeze()
               cli_unfreeze()
    11/20/2002 Included all new FastDb 2.54 features. Changes:
               cli_create_replication_node()
               cli_create()
               cli_skip()

  FastDB Version 2.59 (c) 1999 GARRET
  Konstantin Knizhnik <knizhnik@garret.ru>
-------------------------------------------------------------------*}
unit FastDbCLI;
{$ifdef FPC}{$mode delphi}{$endif}
{$I FastDbConfig.inc}

interface

uses
  {$IFDEF CLR}Borland.Delphi.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF CODESITE_DEBUG}
  CSIntf,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  {$IFDEF CLR}Borland.Win32.Windows{$ELSE}Windows{$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
  {$ifdef FPC}
  dynlibs
  {$else}
  Libc, Types
  {$endif}
  {$ENDIF}
  ;

const
  Version = '2.88';

  {$IFDEF GIGABASE}
    {$IFDEF LINUX}
    libname = 'libgigabase_r.so';
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    libname = 'GigaBase.dll';
    {$ENDIF}
  {$ELSE}
    {$IFDEF LINUX}
    libname = 'libfastdb.so'; { v3.75 Compiled on Raspberry Pi }
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    libname = 'FastDB.dll'; { old version }
    {$ENDIF}
  {$ENDIF}

  FastDbDefaultInitDatabaseSize  = 4*1024*1024; // Default initial db size (number of objects)
  FastDbDefaultInitIndexSize     = 512*1024;    // Default initial index size (number of objects)
  FastDbDefaultExtensionQuantum  = 4*1024*1024; // Quantum of extension of allocated memory
  FastDbMaxParallelSearchThreads = 64;          // Maximal number of threads which can be spawned to perform parallel sequentila search
  FastDbDefaultDatabasePort      = 6010;
  FastDbDefReconnectTimeoutSec   = 120;         // Reconnect timeout seconds

  //-----------------------------------------
  // cli_result_code
  //-----------------------------------------
  cli_ok                         = 0;
  cli_bad_address                = -1;
  cli_connection_refused         = -2;
  cli_database_not_found         = -3;
  cli_bad_statement              = -4;
  cli_parameter_not_found        = -5;
  cli_unbound_parameter          = -6;
  cli_column_not_found           = -7;
  cli_incompatible_type          = -8;
  cli_network_error              = -9;
  cli_runtime_error              = -10;
  cli_bad_descriptor             = -11;
  cli_unsupported_type           = -12;
  cli_not_found                  = -13;
  cli_not_update_mode            = -14;
  cli_table_not_found            = -15;
  cli_not_all_columns_specified  = -16;
  cli_not_fetched                = -17;
  cli_already_updated            = -18;
  cli_table_already_exists       = -19;
  cli_not_implemented            = -20;
  {$IFDEF GIGABASE}
  cli_login_failed               = -21;
  cli_empty_parameter            = -22;
  cli_closed_connection          = -23;
  {$ENDIF}

//-----------------------------------------
// cli_error_class
// Note: When calling CliErrorToStr in TCliErrorHandler subtract 100 from the
//       code passed as ErrorClassCode in order to get correct description
//-----------------------------------------
  cli_query_error                = 1  -100;
  cli_arithmetic_error           = 2  -100;
  cli_index_out_of_range_error   = 3  -100;
  cli_database_open_error        = 4  -100;
  cli_file_error                 = 5  -100;
  cli_out_of_memory_error        = 6  -100;
  cli_deadlock                   = 7  -100;
  cli_null_reference_error       = 8  -100;
  cli_lock_revoked               = 9  -100;
  cli_file_limit_exeeded         = 10 -100;

  //-----------------------------------------
  // Extended Error Codes
  //-----------------------------------------
  cli_error_loading_library      = -200;
  cli_session_not_assigned       = -201;
  cli_database_already_open      = -202;
  cli_invalid_field_size         = -203;
  cli_empty_query                = -204;
  cli_item_already_defined       = -205;
  cli_wrong_inverse_reference    = -206;
  cli_no_fields_defined          = -207;
  cli_access_violation           = -208;

  //-----------------------------------------
  // Field Types
  //-----------------------------------------
  cli_oid                        = 0;
  cli_bool                       = 1;
  cli_int1                       = 2;
  cli_int2                       = 3;
  cli_int4                       = 4;
  cli_int8                       = 5;
  cli_real4                      = 6;
  cli_real8                      = 7;
  cli_decimal                    = 8;  { not supported in FastDB }
  cli_asciiz                     = 9;  { zero terminated string (Get/Set function can be used) }
  cli_pasciiz                    = 10; { pointer to zero terminated string }
  cli_cstring                    = 11; { not supported in FastDB }
  cli_array_of_oid               = 12;
  cli_array_of_bool              = 13;
  cli_array_of_int1              = 14;
  cli_array_of_int2              = 15;
  cli_array_of_int4              = 16;
  cli_array_of_int8              = 17;
  cli_array_of_real4             = 18;
  cli_array_of_real8             = 19;
  cli_array_of_decimal           = 20; { not supported in FastDB }
  cli_array_of_string            = 21;
  cli_any                        = 22; { not supported in FastDB }
  cli_datetime                   = 23; { not supported in FastDB }
  cli_autoincrement              = 24;
  cli_rectangle                  = 25; { not supported in FastDB }
  cli_unknown                    = 26;

  function CliErrorToStr(Code: Integer): string;             // translate error code to string
  function CliCheck(Code: Integer; Msg: string=''): Integer; // raises an exception in case of an error result
{$IFDEF CLI_DEBUG}
type
  TTraceDebugProcedure = procedure(Message: string; const BeforeCall: Boolean) of object;
var
  TraceDebugProcedure: TTraceDebugProcedure;
{$ENDIF}

type
  TStrArray = array of string;

  EFastDbError = class(Exception)
  public
    ErrorCode: Integer;
    constructor Create(Code: Integer; Msg: string=''); overload;
    constructor Create(Msg: string=''); overload;
  end;

  size_t = LongWord;

  TCliVarType = (
    ctOID,
    ctBOOL,
    ctInt1,
    ctInt2,
    ctInt4,
    ctInt8,
    ctReal4,
    ctReal8,
    ctDecimal,        // not supported in FastDB
    ctString,
    ctPString,
    ctCString,        // not supported in FastDB
    ctArrayOfOID,
    ctArrayOfBool,
    ctArrayOfInt1,
    ctArrayOfInt2,
    ctArrayOfInt4,
    ctArrayOfInt8,
    ctArrayOfReal4,
    ctArrayOfReal8,
    ctArrayOfDemical, // not supported in FastDB
    ctArrayOfString,
    ctAny,            // not supported in FastDB
    ctDateTime,
    ctAutoInc,
    ctRectangle,      // not supported in FastDB
    ctUnknown,
    //--- Custom types not supported by the database directly -----
    ctSubst           // Reserved for substitution variables
  );

type
  cli_bool_t  = Char;
  cli_int1_t  = Char;
  cli_int2_t  = SmallInt;
  cli_int4_t  = Integer;
  cli_real4_t = Single;
  cli_real8_t = Double;
  cli_int8_t  = INT64;
  cli_oid_t   = LongInt;

  TClibool  = cli_bool_t;
  TCliInt1  = cli_int1_t;
  TCliInt2  = cli_int2_t;
  TCliInt4  = cli_int4_t;
  TCliReal4 = cli_real4_t;
  TCliReal8 = cli_real8_t;
  TCliInt8  = cli_int8_t;
  TCliOid   = cli_oid_t;

  PCliOid   = ^cli_oid_t;
  PCliReal4 = ^cli_real4_t;
  PCliReal8 = ^cli_real8_t;
  PCliBool  = ^cli_bool_t;
  PCliInt1  = ^cli_int1_t;
  PCliInt2  = ^cli_int2_t;
  PCliInt4  = ^cli_int4_t;
  PCliInt8  = ^cli_int8_t;

  TCliBoolArray  = array of TCliBool;
  TCliOidArray   = array of TCliOid;
  TCliInt1Array  = array of TCliInt1;
  TCliInt2Array  = array of TCliInt2;
  TCliInt4Array  = array of TCliInt4;
  TCliInt8Array  = array of TCliInt8;
  TCliReal4Array = array of TCliReal4;
  TCliReal8Array = array of TCliReal8;

var
  SizeOfCliType : array[TCliVarType] of Integer = (
    sizeof(cli_oid_t),
    sizeof(cli_bool_t),
    sizeof(cli_int1_t),
    sizeof(cli_int2_t),
    sizeof(cli_int4_t),
    sizeof(cli_int8_t),
    sizeof(cli_real4_t),
    sizeof(cli_real8_t),
    0, // cli_decimal
    0, // cli_asciiz,
    0, // cli_pasciiz,
    0, // cli_cstring,
    0, // cli_array_of_oid,
    0, // cli_array_of_bool,
    0, // cli_array_of_int1,
    0, // cli_array_of_int2,
    0, // cli_array_of_int4,
    0, // cli_array_of_int8,
    0, // cli_array_of_real4,
    0, // cli_array_of_real8,
    0, // cli_array_of_decimal,
    0, // cli_array_of_string,
    0, // cli_any,
    sizeof(cli_real8_t), // cli_datetime,
    4, // cli_autoincrement,
    0, // cli_rectangle,
    0, // cli_unknown
    0  // ctSubst
  );

const
  cli_view_only  = 0;
  cli_for_update = 1;


type
  TCliQueryType = (qtViewOnly, qtForUpdate);

{********************************************************************
  cli_open
  Establish connection with the server
  Parameters:
  server_url - zero terminated string with server address and port,
               for example "localhost:5101", "195.239.208.240:6100",...
  max_connect_attempts - number of attempts to establish connection
  reconnect_timeout_sec - timeput in seconds between connection attempts

  Gigabase users:
  ===============
  user_name - user name for login
  password  - password for login
  pooled_connection - if not 0, then connection will be allocated from the connection pool

  Returns:
    >= 0 - connectiondescriptor to be used in all other cli calls
    < 0 - error code as described in cli_result_code enum
}
function cli_open(const ServerURL: string;
                  const MaxConnectAttempts: Integer;
                  const ReconnectTimeoutSec: Integer
                  {$IFDEF GIGABASE}
				  ; UserName, Password: string;
                  PooledConnection: Boolean
                  {$ENDIF}
                 ): Integer;
type
  TCliOpenAttribute  = (oaReadWrite, oaReadOnly, oaTruncate, oaOpenConcurrent);
  TCliOpenAttributes = set of TCliOpenAttribute;

var
  cli_open_attributes: array[TCliOpenAttribute] of Integer=(
    $0, //cli_open_default
    $1, //cli_open_readonly
    $2, //cli_open_truncate
    $4  //cli_open_concurrent
  );

{********************************************************************
  cli_create
  Create conecntion to the local database
  Parameters:
  databaseName - name of the database
  fileName - path to the database file
  transactionCommitDelay - trasnaction commit delay (specify 0 to disable)
  openAttr - mask of cli_open_attributes
  initDatabaseSize - initial size of the database
  extensionQuantum - database extension quantum
  initIndexSize - initial size of object index
  fileSizeLimit - limit for file size (0 - unlimited)

  Gigabase:
  ========
  poolSize - size of page pool (in pages), specify 0 to let GigaBASE automaticaly detect pool size

  Returns:
    >= 0 - connection descriptor to be used in all other cli calls
    < 0 - error code as described in cli_result_code enum
--------------------------------------------------------------------}
{$IFDEF GIGABASE}
function cli_create(const DatabasePath: string;
                    const TransactionCommitDelay: Word=0;
                    const OpenAttr: TCliOpenAttributes = [oaReadWrite];
                    const PoolSize: Integer=0
                    ): Integer;
{$ELSE}
function cli_create(const DatabaseName: string;
                    const FilePath: string;
                    const InitDatabaseSize: Integer=FastDbDefaultInitDatabaseSize;
                    const TransactionCommitDelay: Word=0;
                    const OpenAttr: TCliOpenAttributes = [oaReadWrite];
                    const InitIndexSize: Integer=FastDbDefaultInitIndexSize;
                    const ExtensionQuantum: Integer=FastDbDefaultExtensionQuantum;
                    const FileSizeLimit: Integer=0
                    ): Integer;
{$ENDIF}

{$IFDEF GIGABASE}
{*********************************************************************
  cli_clear_connection_pool
      Close all released connections in the connection pool
--------------------------------------------------------------------}
var
  cli_clear_connection_pool: function: Integer cdecl;
{$ENDIF}


{********************************************************************
  cli_create_replication_node
  Create conecntion to the local database
  Parameters:
  databaseName - name of the database
  fileName - path to the database file
  transactionCommitDelay - trasnaction commit delay (specify 0 to disable)
  openAttr - mask of cli_open_attributes
  initDatabaseSize - initial size of the database
  extensionQuantum - database extension quantum
  initIndexSize - initial size of object index
  fileSizeLimit - limit for file size (0 - unlimited)
  Returns:
    >= 0 - connection descriptor to be used in all other cli calls
    < 0 - error code as described in cli_result_code enum
--------------------------------------------------------------------}
function cli_create_replication_node(const NodeID: Integer;
                                     const ServerCount: Integer;
                                     const NodeNames: TStrArray;
                                     const DatabaseName: string;
                                     const FilePath: string;
                                     const InitDatabaseSize: Integer=FastDbDefaultInitDatabaseSize;
                                     //const TransactionCommitDelay: Word=0;
                                     const OpenAttr: TCliOpenAttributes = [oaReadWrite];
                                     const InitIndexSize: Integer=FastDbDefaultInitIndexSize;
                                     const ExtensionQuantum: Integer=FastDbDefaultExtensionQuantum;
                                     const FileSizeLimit: Integer=0
                                    ): Integer;

{********************************************************************
  cli_close
  Close session
  Parameters:
  session - session descriptor returned by cli_open
  Returns:
  result code as described in cli_result_code enum
--------------------------------------------------------------------}
var
  cli_close: function(session: Integer): Integer cdecl;

{********************************************************************
  cli_statement
  Specify SunSQL statement to be executed at server
  Binding to the parameters and columns can be established
  Parameters:
  session - session descriptor returned by cli_open
  stmt - zero terminated string with SubSQL statement
  Returns:
  >= 0 - statement descriptor
  < 0 - error code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_statement: function(session: Integer;
                          const stmt: PChar): Integer cdecl;

{********************************************************************
  cli_parameter
  Bind parameter to the statement
  Parameters:
  statement - statememt descriptor returned by cli_statement
  param_name - zero terminated string with parameter name
  Paramter name should start with '%'
  var_type - type of variable as described in TCliVarType enum.
  Only scalar and zero terminated string types are supported.
  var_ptr - pointer to the variable
  Returns:
  result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_parameter: function(statement: Integer;
                          const param_name: PChar;
                          var_type: Integer;
                          var_ptr: Pointer): Integer cdecl;

{********************************************************************
  cli_column
  Bind extracted column of select or insert statement
  Parameters:
  statement - statememt descriptor returned by cli_statement
  column_name - zero terminated string with column name
  var_type - type of variable as described in TCliVarType enum
  var_len - pointer to the variable to hold length of array variable.
  This variable should be assigned the maximal length
  of the array/string buffer, pointed by var_ptr.
  After the execution of the statement it is assigned the
  real length of the fetched array/string. If it is large
  than length of the buffer, then only part of the array
  will be placed in the buffer, but var_len still will
  contain the actual array length.
  var_ptr - pointer to the variable
  Returns:
  result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_column: function(statement: Integer;
                       const column_name: PChar;
                       var_type: Integer;
                       var_len: PInteger;
                       var_ptr: Pointer): Integer cdecl;


type
  //TCliColumnSet   = function(var_type: Integer; var_ptr: Pointer; len: Integer): Pointer; cdecl;
  //TCliColumnGet   = function(var_type: Integer; var_ptr: Pointer; var len: Integer): Pointer; cdecl;
  TCliColumnSetEx = function(const ColumnType: Integer;
                             varPtr: Pointer;
                             Len: Integer;
                             const ColumnName: PChar;
                             const Statement: Integer;
                             const SourcePtr: Pointer;
                             const UserData: Pointer): Pointer; cdecl;
  TCliColumnGetEx = function(const ColumnType: Integer;
                             varPtr: Pointer;
                             var Len: Integer;
                             const ColumnName: PChar;
                             const Statement: Integer;
                             const UserData: Pointer): Pointer; cdecl;

{********************************************************************
  cli_array_column
    Specify get/set functions for the array column
  Parameters:
    statement - statememt descriptor returned by cli_statement
    column_name - zero terminated string with column name
    var_type - type of variable as described in TCliVarType enum
    var_ptr - pointer to the variable
    set - function which will be called to construct fetched
          field. It receives pointer to the variable,
          length of the fetched array and returns pointer to th
          array's elements
    get - function which will be called to update the field in the
          database. Given pointer to the variable, it should return
          pointer to the array elements and store length of the
          array to the variable pointer by len parameter
    user_data   - pointer to user specific data passed to get and set functions
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
{var
  cli_array_column: function(statement: Integer;
                             const column_name: PChar;
                             var_type: Integer;
                             var_ptr: Pointer;
                             setCol: TCliColumnSet;
                             getCol: TCliColumnGet): Integer cdecl;
}
var
  cli_array_column_ex: function(statement: Integer;
                                const column_name: PChar;
                                varType: Integer;
                                varPtr: Pointer;
                                setCol: TCliColumnSetEx;
                                getCol: TCliColumnGetEx;
                                user_data: Pointer): Integer cdecl;


{********************************************************************
  cli_fetch
    Execute select statement.
  Parameters:
    statement - statememt descriptor returned by cli_statement
    for_update - not zero if fetched rows will be updated
          cli_view_only  = 0
          cli_for_update = 1

  Returns:
    >= 0 - success, for select statements number of fetched rows is returned
    < 0 - error code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_fetch: function(statement: Integer;
                      for_update: Integer): Integer cdecl;

{********************************************************************
  cli_insert
    Execute insert statement.
  Parameters:
    statement - statememt descriptor returned by cli_statement
    oid - object identifier of created record.
  Returns:
    status code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_insert: function(statement: Integer;
                       oid: PCliOid): Integer cdecl;

{********************************************************************
  cli_get_first
    Get first row of the selection.
  Parameters:
    statement - statememt descriptor returned by cli_statement
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_get_first: function(statement: Integer): Integer cdecl;

{********************************************************************
  cli_get_last
    Get last row of the selection.
  Parameters:
    statement - statememt descriptor returned by cli_statement
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_get_last: function(statement: Integer): Integer cdecl;

{********************************************************************
  cli_get_next
    Get next row of the selection. If get_next records is called
    exactly after cli_fetch function call, is will fetch the first record
    in selection.
  Parameters:
    statement - statememt descriptor returned by cli_statement
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_get_next: function(statement: Integer): Integer cdecl;

{********************************************************************
  cli_get_prev
    Get previous row of the selection. If get_next records is called
    exactly after cli_fetch function call, is will fetch the last record
    in selection.
  Parameters:
    statement - statememt descriptor returned by cli_statement
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_get_prev: function(statement: Integer): Integer cdecl;

{*********************************************************************
 * cli_skip
 *     Skip specified number of rows.
 * Parameters:
 *     statement  - statememt descriptor returned by cli_statement
 *     n          - number of objects to be skipped
 *                - if "n" is positive, then this function has the same effect as
 *                     executing cli_get_next() function "n" times.
 *                - if "n" is negative, then this function has the same effect as
 *                     executing cli_get_prev() function "-n" times.
 *                - if "n"  is zero, this method has no effect
 * Returns:
 *     result code as described in cli_result_code enum
 *}
  cli_skip: function(statement: Integer; n: Integer): Integer cdecl;

{*********************************************************************
 * cli_seek
 *    Position cursor to the record with specified OID
 * Parameters:
 *     statement   - statememt descriptor returned by cli_statement
 *     oid         - object identifier of the record to which cursor should be positioned
 * Returns:
 *     >= 0 - success, position of the record in the selection
 *     <  0 - error code as described in cli_result_code enum
 *}
  cli_seek: function(statement: Integer; oid: cli_oid_t): Integer cdecl;

{********************************************************************
  cli_get_oid
    Get object identifier of the current record
  Parameters:
    statement - statememt descriptor returned by cli_statement
  Returns:
    object identifier or 0 if no object is seleected
--------------------------------------------------------------------}
  cli_get_oid: function(statement: Integer): cli_oid_t cdecl;

{********************************************************************
  cli_update
    Update the current row in the selection. You have to set
    for_update parameter of cli_fetch to 1 in order to be able
    to perform updates. Updated value of row fields will be taken
    from bound column variables.
  Parameters:
    statement - statememt descriptor returned by cli_statement
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_update: function(statement: Integer): Integer cdecl;

{********************************************************************
  cli_remove
    Remove all selected records. You have to set
    for_update parameter of cli_fetch to 1 in order to be able
    to remove records.
  Parameters:
    statement - statememt descriptor returned by cli_statement
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_remove: function(statement: Integer): Integer cdecl;

{********************************************************************
  cli_free
    Deallocate statement and all associated data
  Parameters:
    statement - statememt descriptor returned by cli_statement
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_free: function(statement: Integer): Integer cdecl;

{********************************************************************
  cli_commit
    Commit current database transaction
  Parameters:
    session - session descriptor as returned by cli_open
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_commit: function(session: Integer): Integer cdecl;

{********************************************************************
  cli_precommit
    Release all locks set by transaction. This methods allows other clients
    to proceed, but it doesn't flush transaction to the disk.
  Parameters:
    session - session descriptor as returned by cli_open
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_precommit: function(session: Integer): Integer cdecl;

{********************************************************************
  cli_abort
    Abort current database transaction
  Parameters:
    session - session descriptor as returned by cli_open
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_abort: function(session: Integer): Integer cdecl;

{*********************************************************************
 * cli_freeze
 *    Freeze cursor
 * Parameters:
 *     statement   - statememt descriptor returned by cli_statement
 * Returns:
 *     result code as described in cli_result_code enum
 *}
  cli_freeze: function(Statement: Integer): Integer; cdecl;

{*********************************************************************
 * cli_unfreeze
 *    Unfreeze cursor
 * Parameters:
 *     statement   - statememt descriptor returned by cli_statement
 * Returns:
 *     result code as described in cli_result_code enum
 *}
  cli_unfreeze: function(Statement: Integer): Integer; cdecl;

{*********************************************************************
 * cli_attach
 *    Attach thread to the database. Each thread except one opened the database should first
 *    attach to the database before any access to the database, and detach after end of the work with database
 * Parameters:
 *     session - session descriptor returned by cli_open
 * Returns:
 *     result code as described in cli_result_code enum
 *}
var
  cli_attach: function(session: Integer): Integer; cdecl;

{*********************************************************************
 * cli_detach
 *    Detach thread from the database. Each thread except one opened the database should perform
 *    attach to the database before any access to the database, and detach after end of the work with database
 * Parameters:
 *     session - session descriptor returned by cli_open
 *     detach_mode - bit mask representing detach mode
 * Returns:
 *     result code as described in cli_result_code enum
 *}
const
  cli_commit_on_detach          = 1;
  cli_destroy_context_on_detach = 2;

type
  TDetachMode  = (dtPreCommit, dtCommit, dtDestroyContext);
  TDetachModes = set of TDetachMode;

function cli_detach(session: Integer; DetachMode: TDetachModes): Integer;

const
  cli_hashed  = 1; { field should be indexed usnig hash table }
  cli_indexed = 2; { field should be indexed using B-Tree }
  {$IFDEF GIGABASE}
  cli_case_insensitive    = 8;  { index is case insensitive }
  cli_unique              = 16; { index containing unique keys }
  cli_optimize_duplicates = 64; { index with lot of duplicate key values }
  {$ENDIF}

type
  TIndexType = (itHash, itTree {$IFDEF GIGABASE}, itCaseInsensitive, itUnique, itOptimizeDuplicates{$ENDIF});
  TIndexTypes = set of TIndexType;

  //PCliFieldDescriptor = ^cli_field_descriptor;
  cli_field_descriptor = record
    FieldType: Integer;
    flags: Integer;
    name: PCHAR;
    refTableName: PCHAR;
    inverseRefFieldName: PCHAR;
  end {cli_field_descriptor};
  TCliFieldDescriptor  = cli_field_descriptor;
  PCliFieldDescriptor  = ^TCliFieldDescriptor;
  PPCliFieldDescriptor = ^PCliFieldDescriptor;
  TFieldDescriptors    = array of TCliFieldDescriptor;
  //PPACliFieldDescriptor = TACliFieldDescriptor;

{********************************************************************
  cli_describe
    Describe fileds of specified table
  Parameters:
    session - session descriptor as returned by cli_open
    table - name of the table
    fields - adress of the pointer to the array of fields descriptors,
    this array should be later deallocated by application by cli_free_memory()
  Returns:
    >= 0 - number of fields in the table
    < 0 - result code as described in cli_result_code enum
--------------------------------------------------------------------}
var
  cli_describe: function(session: Integer;
                         const table: PChar;
                         fields: PPCliFieldDescriptor): Integer cdecl;


{*********************************************************************
 * cli_get_field_size
 *     Calculate field size
 * Parameters:
 *     fields   - array with fields descriptors obtained using cli_describe function
 *     field_no - number of the field
 *}
var
  cli_get_field_size: function(fields: PPCliFieldDescriptor; field_no: Integer): Integer; cdecl;

{*********************************************************************
 * cli_get_field_offset
 *     Calculate offset of the field
 * Parameters:
 *     fields   - array with fields descriptors obtained using cli_describe function
 *     field_no - number of the field
 *}
var
  cli_get_field_offset: function(fields: PPCliFieldDescriptor; field_no: Integer): Integer; cdecl;

type
  cli_table_descriptor = record
    name: PCHAR;
  end {cli_table_descriptor};
  PCliTableDescriptor = ^cli_table_descriptor;
  //PACliTableDescriptor = array of PCliTableDescriptor;
  //PPACliTableDescriptor = ^PACliTableDescriptor;

{********************************************************************
  cli_show_tables
    Show all tables of specified database
  Parameters:
    session - session descriptor as returned by cli_open
    tables - address of the pointer to the array of tables descriptors,
             this array should be later deallocated by application by
             cli_free_memory()
  Returns:
    >= 0 - number of tables in the database (Metatable is not returned/counted)
    < 0 - result code as described in cli_result_code enum
--------------------------------------------------------------------}
var
  cli_show_tables: function(session: Integer;
                            var tables: PCliTableDescriptor): Integer cdecl;


{********************************************************************
  cli_create_table
    Create new table
  Parameters:
    session - session descriptor as returned by cli_open
    tableName - name of new table
    nFields - number of columns in the table
    fields - array with table columns descriptors
             this array should be later deallocated by application by
             __free() or SysFreeMem()
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_create_table: function(session: Integer;
                             const tableName: PChar;
                             nFields: Integer;
                             fields: PCliFieldDescriptor): Integer cdecl;

{*********************************************************************
  cli_alter_table
      Change table format
  Parameters:
      session   - session descriptor as returned by cli_open
      tableName - name of existing table
      nFields   - number of columns in the table
      fields    - array with new table columns descriptors
  Returns:
      result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_alter_table: function(session: Integer;
                            const tableName: PChar;
                            nFields: Integer;
                            fields: PCliFieldDescriptor): Integer cdecl;

{********************************************************************
  cli_drop_table
    drop the table
  Parameters:
    session - session descriptor as returned by cli_open
    tableName - name of deleted table
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_drop_table: function(session: Integer;
                           const tableName: PChar): Integer cdecl;


{********************************************************************
  cli_alter_index
    add or remove column index
  Parameters:
    session - session descriptor as returned by cli_open
    tableName - name of the table
    fieldName - name of field
    newFlags - new flags of the field, if index exists for this field,
               but is not specified in newFlags mask, then it will be
               removed; if index not exists, but is specified in
               newFlags mask, then it will be created.
  Returns:
    result code as described in cli_result_code enum
--------------------------------------------------------------------}
  cli_alter_index: function(session: Integer;
                            const tableName: PChar;
                            const fieldName: PChar;
                            newFlags: Integer): Integer cdecl;

{*********************************************************************
 * cli_free_memory
 *    Free memory allocated by cli_show_tables and cli_describe
 * Parameters:
 *     session - session descriptor returned by cli_open
 *     ptr - pointer to the allocated buffer
 *}
  cli_free_memory: procedure(session: Integer; Memory: Pointer); cdecl;

type
  TCliErrorHandler = procedure(ErrorClassCode: Integer; const Msg: PChar; MsgArg: Integer); cdecl;

{*********************************************************************
 * cli_set_error_handler
 *     Set FastDB erro handler. Handler should be no-return function which perform stack unwind.
 * Parameters:
 *     session   - session descriptor as returned by cli_open
 *     handler   - error handler
 *     UserData  - error handler context: pointer to the user specific data
 *                  which will be passed to thr handler
 * Returns:
 *     previous handler
 *}
function cli_set_error_handler(const session: Integer; NewHandler: TCliErrorHandler; const UserData: Pointer): TCliErrorHandler;

{*********************************************************************
 * cli_get_database_state
 *    Obtain information about current state of the database
 * Parameters:
 *     session - session descriptor returned by cli_open
 *     monitor - pointer to the monitor structure. The folloing fields are set:
 *       n_readers: number of granted shared locks
 *       n_writers: number of granted exclusive locks
 *       n_blocked_reader: number of threads which shared lock request was blocked
 *       n_blocked_writers: number of threads which exclusive lock request was blocked
 *       n_users: number of processes openned the database
 * Returns:
 *     result code as described in cli_result_code enum
 *}
type
  PCliDatabaseMonitor = ^TCliDatabaseMonitor;
  TCliDatabaseMonitor = record
    n_readers          : Integer;
    n_writers          : Integer;
    n_blocked_readers  : Integer;
    n_blocked_writers  : Integer;
    n_users            : Integer;
  end;

var
  cli_get_database_state: function(const session: Integer; Monitor: PCliDatabaseMonitor): Integer; cdecl;

{*********************************************************************
 * cli_set_trace_function
 *    Set trace function which will be used to output FastDB trace messages
 * Parameters:
 *     func - pointer to trace function which receives trace message terminated with new line character
 *}
type
  TCliTraceFunction = procedure(Msg: PChar); cdecl;
var
  cli_set_trace_function: procedure(TraceFunction: TCliTraceFunction); cdecl;


{*********************************************************************
 * cli_prepare_query
 *     Prepare SubSQL query statement.
 * Parameters:
 *     session - session descriptor returned by cli_open
 *     query   - query string with optional parameters. Parameters are specified
 *               as '%T' where T is one or two character code of parameter type using the same notation
 *               as in printf: %d or %i - int, %f - float or double, %ld - int8, %s - string, %p - oid...
 * Returns:
 *     >= 0 - statement descriptor
 *     <  0 - error code as described in cli_result_code enum
 *}
var
  cli_prepare_query : function(session: Integer; query: PChar): Integer; cdecl;

{*********************************************************************
 * cli_execute_query
 *     Execute query previously prepared by cli_prepare_query
 * Parameters:
 *     statement - statement descriptor returned by cli_prepare_query
 *     for_update - not zero if fetched rows will be updated
 *     record_struct - structure to receive selected record fields
 *     ...     - varying list of query parameters
 * Returns:
 *     >= 0 - success, for select statements number of fetched rows is returned
 *     <  0 - error code as described in cli_result_code enum
 *}
var
  cli_execute_query: function(statement: Integer; for_update: Integer; record_struct: Pointer; var_param: array of const): Integer; cdecl;

{*********************************************************************
 * cli_insert_struct
 *     Insert new record represented as C structure
 * Parameters:
 *     session - session descriptor returned by cli_open
 *     table_name - name of the destination table
 *     record_struct - structure specifying value of record fields
 *     oid - pointer to the location to receive OID of created record (may be NULL)
 * Returns:
 *     result code as described in cli_result_code enum
 *}
var
{  cli_insert_struct: function(session: Integer; table_name: PChar; record_struct: Pointer; var oid: TCliOid): Integer; cdecl; }
  cli_insert_struct: function(session: Integer; table_name: PChar; record_struct: Pointer; poid: Pointer): Integer; cdecl;

implementation

{$IFDEF CLI_DEBUG}

type
  TDebugTrace = class
  private
    FCritSect: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DefaultTraceDebugProcedure(Message: string; const BeforeCall: Boolean);
  end;

var
  DebugTrace: TDebugTrace;

  constructor TDebugTrace.Create;
  begin
    InitializeCriticalSection(FCritSect);
  end;

  destructor TDebugTrace.Destroy;
  begin
    DeleteCriticalSection(FCritSect);
    inherited;
  end;

  procedure TDebugTrace.DefaultTraceDebugProcedure(Message: string; const BeforeCall: Boolean);
  begin
    EnterCriticalSection(FCritSect);
    try
      {$IFDEF CODESITE_DEBUG}
      if BeforeCall then CodeSite.SendMsg(Format({$IFDEF USE_DEBUG_THREAD_PREFIX}'[%d]: '+{$ENDIF}'%s ',     [{$IFDEF USE_DEBUG_THREAD_PREFIX}GetCurrentThreadID,{$ENDIF} message]))
                    else CodeSite.SendMsg(Format({$IFDEF USE_DEBUG_THREAD_PREFIX}'[%d]: '+{$ENDIF}'  -> %s', [{$IFDEF USE_DEBUG_THREAD_PREFIX}GetCurrentThreadID,{$ENDIF} message]));
      {$ELSE}
      if BeforeCall then writeln(Format({$IFDEF USE_DEBUG_THREAD_PREFIX}'[%d]: '+{$ENDIF}'%s ',     [{$IFDEF USE_DEBUG_THREAD_PREFIX}GetCurrentThreadID,{$ENDIF} message]))
                    else writeln(Format({$IFDEF USE_DEBUG_THREAD_PREFIX}'[%d]: '+{$ENDIF}'  -> %s', [{$IFDEF USE_DEBUG_THREAD_PREFIX}GetCurrentThreadID,{$ENDIF} message]));
      {$ENDIF}
    finally
      LeaveCriticalSection(FCritSect);
    end;
  end;
{$ENDIF}

function OpenAttrToInt(OpenAttr: TCliOpenAttributes): Integer;
var oa : TCliOpenAttribute;
begin
  Result := 0;
  for oa:=Low(TCliOpenAttribute) to High(TCliOpenAttribute) do
    if oa in OpenAttr then
      Result := Result or cli_open_attributes[oa];
end;

var
  __cli_open: function(const server_url     : PChar;
                       max_connect_attempts : Integer;
                       reconnect_timeout_sec: Integer
                       {$IFDEF GIGABASE}
                       ; UserName, Password: PChar;
                       PooledConnection: Integer
                       {$ENDIF}
                       ): Integer cdecl;

  {$IFDEF GIGABASE}
  __cli_create: function(const databasePath     : PChar;
                         transactionCommitDelay : Word;
                         openAttr               : Integer;
                         PoolSize               : SIZE_T
                         )                      : Integer cdecl;
  {$ELSE}
  __cli_create: function(const databaseName     : PChar;
                         const filePath         : PChar;
                         transactionCommitDelay : Word;
                         openAttr               : Integer;
                         initDatabaseSize       : SIZE_T;
                         extensionQuantum       : SIZE_T;
                         initIndexSize          : SIZE_T;
                         fileSizeLimit          : SIZE_T
                         )                      : Integer cdecl;
  {$ENDIF}

  __cli_create_replication_node:
                function(nodeId                 : Integer;
                         nServers               : Integer;
                         nodeNames              : Pointer;  // array of PChar
                         const databaseName     : PChar;
                         const filePath         : PChar;
                         openAttr               : Integer;
                         initDatabaseSize       : SIZE_T;
                         extensionQuantum       : SIZE_T;
                         initIndexSize          : SIZE_T;
                         fileSizeLimit          : SIZE_T
                         )                      : Integer cdecl;
  __cli_set_error_handler: function(const session: Integer; NewHandler: TCliErrorHandler; const UserData: Pointer): TCliErrorHandler; cdecl;

  __cli_detach: function(session: Integer; detach_mode: Integer): Integer; cdecl;

{ ECliError }
constructor EFastDbError.Create(Code: Integer; Msg: string='');
begin
  ErrorCode := Code;
  if Msg <> '' then Msg := '. ' + Msg;
  inherited Create(CliErrorToStr(Code)+Msg);
end;

constructor EFastDbError.Create(Msg: string);
begin
  ErrorCode := -999;
  inherited Create(Msg);
end;

var
  //SaveExit: pointer;
  DLLHandle: THandle = 0;

  procedure DLLExit;
  begin
    //ExitProc := SaveExit;
    if DLLHandle <> 0 then
      try
        FreeLibrary(DLLHandle);
      finally
        DLLHandle := 0;
      end;
  end {DLLExit};

//---------------------------------------------------------------------------
procedure LoadDLL;
  //+++++++++++++++++
  procedure GetAddr(var ProcAddr: Pointer; ProcName: string; AssertCheck: Boolean=True);
  begin
    ProcAddr := GetProcAddress(DLLHandle,PChar({$IFDEF USE_UNDERSCORE_PREFIX}'_'+{$ENDIF}ProcName));
    if AssertCheck then
      Assert(ProcAddr <> nil, Format('%s procedure not found in "%s"!', [ProcName, libname]));
  end;
  //+++++++++++++++++
begin
  if DLLHandle > 0 then
    Exit
  else begin
    try
      DLLHandle := LoadLibrary(libname);
    except
      on e: Exception do
        begin
          DLLHandle := 0;
          raise EFastDbError.Create(cli_error_loading_library, Format('(%s)'#10'%s', [libname, e.message]));
        end;
    end;

    if DLLHandle <= 0 then
      raise EFastDbError.CreateFmt('Library "%s" not found!', [libname])
    else  // library loaded successfully
      try
        //SaveExit := ExitProc;
        GetAddr(@__cli_open                    , 'cli_open');
        GetAddr(@__cli_create                  , 'cli_create');
        GetAddr(@__cli_create_replication_node , 'cli_create_replication_node', False);
        GetAddr(@cli_close                     , 'cli_close');
        GetAddr(@cli_statement                 , 'cli_statement');
        GetAddr(@cli_parameter                 , 'cli_parameter');
        GetAddr(@cli_column                    , 'cli_column');
        //GetAddr(@cli_array_column           , 'cli_array_column');  This procedure is not needed.
        GetAddr(@cli_array_column_ex           , 'cli_array_column_ex');
        GetAddr(@cli_fetch                     , 'cli_fetch');
        GetAddr(@cli_insert                    , 'cli_insert');
        GetAddr(@cli_get_first                 , 'cli_get_first');
        GetAddr(@cli_get_last                  , 'cli_get_last');
        GetAddr(@cli_get_next                  , 'cli_get_next');
        GetAddr(@cli_get_prev                  , 'cli_get_prev');
        GetAddr(@cli_get_oid                   , 'cli_get_oid');
        GetAddr(@cli_skip                      , 'cli_skip');
        GetAddr(@cli_seek                      , 'cli_seek');
        GetAddr(@cli_update                    , 'cli_update');
        GetAddr(@cli_remove                    , 'cli_remove');
        GetAddr(@cli_free                      , 'cli_free');
        GetAddr(@cli_commit                    , 'cli_commit');
        GetAddr(@cli_precommit                 , 'cli_precommit');
        GetAddr(@cli_abort                     , 'cli_abort');
        GetAddr(@cli_describe                  , 'cli_describe');
        GetAddr(@cli_get_field_size            , 'cli_get_field_size');
        GetAddr(@cli_get_field_offset          , 'cli_get_field_offset');
        GetAddr(@cli_show_tables               , 'cli_show_tables');
        GetAddr(@cli_create_table              , 'cli_create_table');
        GetAddr(@cli_alter_table               , 'cli_alter_table');
        GetAddr(@cli_drop_table                , 'cli_drop_table');
        GetAddr(@cli_alter_index               , 'cli_alter_index');
        GetAddr(@__cli_set_error_handler       , 'cli_set_error_handler');
        GetAddr(@cli_freeze                    , 'cli_freeze');
        GetAddr(@cli_unfreeze                  , 'cli_unfreeze');
        GetAddr(@cli_attach                    , 'cli_attach');
        GetAddr(@__cli_detach                  , 'cli_detach');
        GetAddr(@cli_free_memory               , 'cli_free_memory');
        GetAddr(@cli_get_database_state        , 'cli_get_database_state');
        GetAddr(@cli_set_trace_function        , 'cli_set_trace_function');
        GetAddr(@cli_prepare_query             , 'cli_prepare_query');
        GetAddr(@cli_execute_query             , 'cli_execute_query');
        GetAddr(@cli_insert_struct             , 'cli_insert_struct');

        {$IFDEF GIGABASE}
        GetAddr(@cli_clear_connection_pool     , 'cli_clear_connection_pool');
        {$ENDIF}
        
        //ExitProc := @DLLExit;
      except
        on e: Exception do
          begin
            FreeLibrary(DLLHandle);
            DLLHandle := 0;
            raise;
          end
      end;
  end;
end; {LoadDLL}

//--------------------------------------------------------------
function cli_open(const ServerURL: string;
                  const MaxConnectAttempts: Integer;
                  const ReconnectTimeoutSec: Integer
                  {$IFDEF GIGABASE}
				  ; UserName, Password: string;
                  PooledConnection: Boolean
                  {$ENDIF}
                  ): Integer;
{$IFDEF GIGABASE}
var
  n : Integer;
{$ENDIF}
begin
  LoadDLL;
  {$IFDEF GIGABASE}
  if PooledConnection then n := 1 else n := 0;
  {$ENDIF}
  {$IFDEF CLI_DEBUG}
    {$IFDEF GIGABASE}
    TraceDebugProcedure(Format('cli_open(%s, %d, %d, "%s", "%s", %d)', [ServerURL, MaxConnectAttempts, ReconnectTimeoutSec, Username, Password, n]), True);
    {$ELSE}
    TraceDebugProcedure(Format('cli_open(%s, %d, %d)', [ServerURL, MaxConnectAttempts, ReconnectTimeoutSec]), True);
    {$ENDIF}
  {$ENDIF}
  Result := __cli_open(PChar(ServerURL), MaxConnectAttempts, ReconnectTimeoutSec
                       {$IFDEF GIGABASE}
                       , Username, Password, n
                       {$ENDIF}
                      );
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [Result]), False);
  {$ENDIF}
end;

{$IFDEF GIGABASE}
function cli_create(const DatabasePath: string;
                    const TransactionCommitDelay: Word=0;
                    const OpenAttr: TCliOpenAttributes = [oaReadWrite];
                    const PoolSize: Integer=0
                    ): Integer;
{$ELSE}
//---------------------------------------------------------------------------
function cli_create(const DatabaseName: string;
                    const FilePath: string;
                    const InitDatabaseSize: Integer=FastDbDefaultInitDatabaseSize;
                    const TransactionCommitDelay: Word=0;
                    const OpenAttr: TCliOpenAttributes = [oaReadWrite];
                    const InitIndexSize: Integer=FastDbDefaultInitIndexSize;
                    const ExtensionQuantum: Integer=FastDbDefaultExtensionQuantum;
                    const FileSizeLimit: Integer=0
                    ): Integer;
{$ENDIF}
begin
  LoadDLL;

  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(
                      {$IFDEF GIGABASE}
                      Format('cli_create("%s",%d,%d,%d)', [
                                 DatabasePath,
                                 TransactionCommitDelay,
                                 OpenAttrToInt(OpenAttr),
                                 PoolSize])
                      {$ELSE}
                      Format('cli_create("%s","%s",%d,%d,%d,%d,%d,%d)', [
                                 DatabaseName,
                                 FilePath,
                                 TransactionCommitDelay,
                                 OpenAttrToInt(OpenAttr),
                                 InitDatabaseSize,
                                 ExtensionQuantum,
                                 InitIndexSize,
                                 FileSizeLimit])
                      {$ENDIF}
                     , True);
  {$ENDIF}
  try
    {$IFDEF GIGABASE}
    Result := __cli_create(PChar(DatabasePath), TransactionCommitDelay, OpenAttrToInt(OpenAttr), PoolSize);
    {$ELSE}
    Result := __cli_create(PChar(DatabaseName), PChar(FilePath), TransactionCommitDelay,
                           OpenAttrToInt(OpenAttr), InitDatabaseSize, ExtensionQuantum,
                           InitIndexSize, FileSizeLimit);
    {$ENDIF}
  except
    on e: Exception do
      raise EFastDbError.Create(cli_database_open_error, 'cli_create failed. '+e.message);
  end;
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [Result]), False);
  {$ENDIF}
end;

//---------------------------------------------------------------------------
function cli_create_replication_node(const NodeID: Integer;
                                     const ServerCount: Integer;
                                     const NodeNames: TStrArray;
                                     const DatabaseName: string;
                                     const FilePath: string;
                                     const InitDatabaseSize: Integer=FastDbDefaultInitDatabaseSize;
                                     //const TransactionCommitDelay: Word=0;
                                     const OpenAttr: TCliOpenAttributes = [oaReadWrite];
                                     const InitIndexSize: Integer=FastDbDefaultInitIndexSize;
                                     const ExtensionQuantum: Integer=FastDbDefaultExtensionQuantum;
                                     const FileSizeLimit: Integer=0
                                    ): Integer;
{$IFDEF CLI_DEBUG}
var
  i : Integer;
  s : string;
{$ENDIF}
begin
  {$IFDEF CLI_DEBUG}
  for i:=0 to Length(NodeNames)-1 do
    s := s + Format(',"%s"', [NodeNames[i]]);
  if s <> '' then Delete(s, 1, 1);
  TraceDebugProcedure(Format('cli_create_replication_node(%d,%d,(%s),"%s","%s",%d,%d,%d,%d,%d)', [
                                 NodeID,
                                 ServerCount,
                                 s,
                                 DatabaseName,
                                 FilePath,
                                 OpenAttrToInt(OpenAttr),
                                 InitDatabaseSize,
                                 ExtensionQuantum,
                                 InitIndexSize,
                                 FileSizeLimit]), True);
  {$ENDIF}
  Result := __cli_create_replication_node(NodeID, ServerCount, @NodeNames[0], PChar(DatabaseName),
                         PChar(FilePath), OpenAttrToInt(OpenAttr),
                         InitDatabaseSize, ExtensionQuantum, InitIndexSize, FileSizeLimit);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [Result]), False);
  {$ENDIF}
end;

//---------------------------------------------------------------------------
function cli_set_error_handler(const session: Integer;
  NewHandler: TCliErrorHandler; const UserData: Pointer): TCliErrorHandler;
begin
  LoadDLL;
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_set_error_handler(%d, 0x%p)', [session, @NewHandler]), True);
  {$ENDIF}
  Result := __cli_set_error_handler(session, NewHandler, UserData);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('0x%p', [@Result]), False);
  {$ENDIF}
end;

//--------------------------------------------------------------
function cli_detach(session: Integer; DetachMode: TDetachModes): Integer;
begin
  Result := 0;
  if dtCommit         in DetachMode then Inc(Result, cli_commit_on_detach);
  if dtDestroyContext in DetachMode then Inc(Result, cli_destroy_context_on_detach);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('cli_detach(%d, %d)', [session, Result]), True);
  {$ENDIF}
  Result := __cli_detach(session, Result);
  {$IFDEF CLI_DEBUG}
  TraceDebugProcedure(Format('%d', [Result]), False);
  {$ENDIF}
  CliCheck(Result, 'cli_detach failed');
end;

//--------------------------------------------------------------
function CliCheck(Code: Integer; Msg: string=''): Integer;      // raises an exception in case of an error result
begin
  if Code < 0 then
    raise EFastDbError.Create(Code, Msg)
  else
    Result := Code;
end;

//--------------------------------------------------------------
function CliErrorToStr(Code: Integer): string;
begin
  if Code >= 0 then
    Result := ''
  else begin
    case Code of
      cli_bad_address                : Result := 'Invalid format of server URL';
      cli_connection_refused         : Result := 'Connection with server could not be established';
      cli_database_not_found         : Result := 'Database cannot be found';
      cli_bad_statement              : Result := 'Text of SQL statement is not correct';
      cli_parameter_not_found        : Result := 'Parameter was not found in statement';
      cli_unbound_parameter          : Result := 'Parameter was not specified';
      cli_column_not_found           : Result := 'No such colunm in the table';
      cli_incompatible_type          : Result := 'Conversion between application and database type is not possible';
      cli_network_error              : Result := 'Connection with server is broken';
      cli_runtime_error              : Result := 'Error during query execution';
      cli_bad_descriptor             : Result := 'Invalid statement/session description';
      cli_unsupported_type           : Result := 'Unsupported type for parameter or column';
      cli_not_found                  : Result := 'Record was not found';
      cli_not_update_mode            : Result := 'Attempt to update records selected by view only cursor';
      cli_table_not_found            : Result := 'There is no table with specified name in the database';
      cli_not_all_columns_specified  : Result := 'Insert statement doesn''t specify values for all table columns';
      cli_not_fetched                : Result := 'cli_fetch method was not called';
      cli_already_updated            : Result := 'cli_update method was invoked more than once for the same record';
      cli_table_already_exists       : Result := 'Attempt to create existing table';
      cli_not_implemented            : Result := 'Function is not implemented';
      //----- Severe Error Class Codes---------
      cli_query_error                : Result := 'Query error';
      cli_arithmetic_error           : Result := 'Arithmetic error';
      cli_index_out_of_range_error   : Result := 'Index out of range';
      cli_database_open_error        : Result := 'Database open error';
      cli_file_error                 : Result := 'File error';
      cli_out_of_memory_error        : Result := 'Out of memory';
      cli_deadlock                   : Result := 'Deadlock detected';
      cli_null_reference_error       : Result := 'Null reference';
      cli_lock_revoked               : Result := 'Lock revoked';
      cli_file_limit_exeeded         : Result := 'File limit exeeded';
      //----- Custom Error Codes---------------
      cli_error_loading_library      : Result := 'Error loading library';
      cli_session_not_assigned       : Result := 'Session not assigned or not connected';
      cli_database_already_open      : Result := 'Database already open';
      cli_invalid_field_size         : Result := 'Invalid field size';
      cli_empty_query                : Result := 'Query SQL text is not assigned';
      cli_item_already_defined       : Result := 'Field/Variable is already defined';
      cli_wrong_inverse_reference    : Result := 'Wrong inverse reference';
      cli_no_fields_defined          : Result := 'No fields defined';
      cli_access_violation           : Result := 'Access Violation';
    else
      Result := Format('Unknown code -%d', [Code]);
    end;
  end;
end;

initialization
  DLLHandle := 0;
  {$IFDEF CLI_DEBUG}
  DebugTrace := TDebugTrace.Create;
  TraceDebugProcedure := DebugTrace.DefaultTraceDebugProcedure;
  {$ENDIF}
  {$IFDEF CODESITE_DEBUG}
  CodeSite.Clear;
  {$ENDIF}

finalization
  {$IFDEF CLI_DEBUG}
  DebugTrace.Free;
  {$ENDIF}
  DLLExit;

end.



