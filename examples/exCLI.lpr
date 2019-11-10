program exCLI;

{$IFDEF FPC}
  {$MODE Delphi}
//  {$PACKRECORDS C}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  {$ifdef FPC}
  //CTypes,
  {$endif}
  SysUtils,
  Classes,
  FastDbCLI in 'FastDbCLI.pas';

type
  TCliOidArray = array[0..0] of TCliOid;

  PPerson = ^TPerson;
  TPerson = record
    name        : array[0..63] of Char;
    salary      : TCliInt8;
    address     : PChar;
    weight      : TCliReal8;
  end;

procedure SessionErrorHandler(ErrorClassCode: Integer; const Msg: PChar; MsgArg: Integer); cdecl;
begin
  writeln(Format('Error: %d. %s (%d).', [ErrorClassCode, Msg, MsgArg]));
end;

const
  person_descriptor: array[0..3] of TCliFieldDescriptor = (
    (FieldType:cli_asciiz;  Flags:0; Name:'name';    refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_int8;    Flags:0; Name:'salary';  refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_pasciiz; Flags:0; Name:'address'; refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_real8;   Flags:0; Name:'weight';  refTableName:nil; inverseRefFieldName:nil));

var
  serverURL    : string = 'localhost:6100';
  databaseName : string = 'excli';
  filePath     : string = 'excli.fdb';
  session, statement, rc, len : Integer;
  oid : TCliOid;
  p : TPerson;

begin
  session := cli_open(serverURL, 10, 1);
  // len := 256;
  session := cli_create(databaseName, filePath, 1024*1024);

  cli_create_table(
    session,
    'persons',
    sizeof(person_descriptor) div sizeof(cli_field_descriptor),
    @person_descriptor);

  statement := CliCheck(cli_statement(session, 'insert into persons'), 'cli_statement failed');
  CliCheck(cli_column(statement, 'name',    Ord(cli_asciiz), nil, @p.name), 'cli_column 1 failed');
  CliCheck(cli_column(statement, 'salary',  Ord(cli_int8), nil, @p.salary), 'cli_column 2 failed');
  CliCheck(cli_column(statement, 'address', Ord(cli_pasciiz), @len, @p.address), 'cli_column 3 failed');
  CliCheck(cli_column(statement, 'weight',  Ord(cli_real8), nil, @p.weight), 'cli_column 4 failed');

  p.name := 'John Smith';
  p.salary := 75000;
  p.address := '1 Guildhall St., Cambridge CB2 3NH, UK';
  p.weight := 80.3;

  CliCheck(cli_insert(statement, @oid), 'cli_insert failed');

  p.name := 'Joe Cooker';
  p.salary := 100000;
  p.address := 'Outlook drive, 15/3';
  p.weight := 80.3;

  CliCheck(cli_insert(statement, @oid), 'cli_insert failed');
{  cli_insert_struct(session, 'persons', @p, @oid); }
  CliCheck(cli_commit(session), 'cli_commit failed');
  CliCheck(cli_close(session), 'cli_close failed');
end.
