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
//  Strings,
  FastDbCLI in 'FastDbCLI.pas';

type
  TCliOidArray = array[0..0] of TCliOid;

  PPerson = ^TPerson;
  TPerson = record
    name        : PChar;
    salary      : TCliInt8;
    address     : PChar;
    weight      : TCliReal8;
  end;

  TRstr = String[27];

procedure SessionErrorHandler(ErrorClassCode: Integer; const Msg: PChar; MsgArg: Integer); cdecl;
begin
  writeln(Format('Error: %d. %s (%d).', [ErrorClassCode, Msg, MsgArg]));
end;

const
  person_descriptor: array[0..3] of TCliFieldDescriptor = (
    (FieldType:cli_pasciiz; Flags:0; Name:'name';    refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_int8;    Flags:0; Name:'salary';  refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_pasciiz; Flags:0; Name:'address'; refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_real8;   Flags:0; Name:'weight';  refTableName:nil; inverseRefFieldName:nil));

var
  serverURL    : string = 'localhost:6100';
  databaseName : string = 'excli';
  filePath     : string = 'excli.fdb';
  session, statement, rc, len, i, tc : Integer;
  oid : TCliOid;
  p : TPerson;
  rname: TRstr;
  raddr: TRstr;

procedure mkRandStr(var s: TRstr);
var
  i: Integer;
begin
  for i := Low(s) to High(s) - 1 do
    s[i] := Chr(Ord('a') + Random(25));
  //s[High(s)] := Chr(0);
end;

begin
  session := cli_open(serverURL, 10, 1);
  session := cli_create(databaseName, filePath, 1024*1024);
  cli_create_table(
    session,
    'persons',
    sizeof(person_descriptor) div sizeof(cli_field_descriptor),
    @person_descriptor);

  p.name := @rname;
  p.address := @raddr;

  tc := GetTickCount;
  for i := 1 to 1024*1024 do
  begin
    mkRandStr(rname);
    mkRandStr(raddr);
    p.salary := Random(120);
    CliCheck(cli_insert_struct(session, 'persons', @p, @oid), 'cli_insert_struct failed');
  end;
  WriteLn('Inserted - ', 1024*1024, ' records. Time - ', GetTickCount - tc, ' ticks.');
  CliCheck(cli_commit(session), 'cli_commit failed');
  CliCheck(cli_close(session), 'cli_close failed');
  ReadLn;
end.
