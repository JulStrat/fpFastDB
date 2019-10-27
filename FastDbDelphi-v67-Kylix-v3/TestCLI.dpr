{-< TestCLI.dpr >--------------------------------------------------*
  Test Delphi Implementation of FastDB
  10/21/2002 Serge Aleynikov (serge@hq.idt.net)

 (code based on testcli C program from FastDB)
-------------------------------------------------------------------*}
program TestCLI;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  FastDbCLI in 'FastDbCLI.pas',
  FastDbSession in 'FastDbSession.pas';

type
  TCliOidArray = array[0..0] of TCliOid;

  PPerson = ^TPerson;
  TPerson = record
    name        : array[0..63] of Char;
    salary      : TCliInt8;
    address     : PChar;
    weight      : TCliReal8;
    n_subordinates: TCliInt4;
    subordinates  : PCliOid;
  end;

function set_subordinates(const ColumnType: Integer;
                          varPtr: Pointer;
                          Len: Integer;
                          const ColumnName: PChar;
                          const Statement: Integer;
                          const SourcePtr: Pointer): Pointer; cdecl;
var
  p: PPerson absolute varPtr;
begin
  if PChar(p^.subordinates) <> nil then
    FreeMem(p^.subordinates);

  p^.n_subordinates := len;
  GetMem(p^.subordinates, len*sizeof(TCliOid));
  Result := p^.subordinates;
end;

function get_subordinates(const ColumnType: Integer;
                          varPtr: Pointer;
                          var Len: Integer;
                          const ColumnName: PChar;
                          const Statement: Integer): Pointer; cdecl;
var
  p: PPerson absolute varPtr;
begin
  len := p^.n_subordinates;
  Result := p^.subordinates;
end;

procedure SessionErrorHandler(ErrorClassCode: Integer; const Msg: PChar; MsgArg: Integer); cdecl;
begin
  writeln(Format('Error: %d. %s (%d).', [ErrorClassCode, Msg, MsgArg]));
end;

const
  person_descriptor: array[0..4] of TCliFieldDescriptor = (
    (FieldType:cli_asciiz;       Flags:cli_hashed;  Name:'name';    refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_int8;         Flags:cli_indexed; Name:'salary';  refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_pasciiz;      Flags:0;           Name:'address'; refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_real8;        Flags:0;           Name:'weight';  refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_array_of_oid; Flags:0;           Name:'subordinates'; refTableName:'persons'; inverseRefFieldName:nil));

var
  serverURL    : string = 'localhost:6100';
  databaseName : string = 'clitest';
  filePath     : string = 'clitest.fdb';
  session, statement, statement2, rc, len : Integer;
  i, n, salary: Integer;
  table_created : Boolean = False;
  name : array[0..256] of Char;
  address: array[0..256] of Char;
  oid : TCliOid;
  p : TPerson;
  p1: Pointer;
  fld, fields : PCliFieldDescriptor;
  tbl, tables: PCliTableDescriptor;
  ptr : TCliErrorHandler;
  database: TFastDbSession;
  str : TStringList;
  s : string;
begin
  session := cli_open(serverURL, 10, 1);
  if (session = cli_bad_address) then begin
    session := cli_create(databaseName, filePath, 1*1024*1024);
  end;
  if (session < 0) then begin
    writeln(Format('cli_open failed with code (%d) - %s', [session, CliErrorToStr(session)]));
    exit;
  end;

  //ptr := cli_set_error_handler(session, @SessionErrorHandler);
  //writeln(Format('cli_set_error_handler -> 0x%p', [@ptr]));

  try
    try
      rc := cli_create_table(session, 'persons', sizeof(person_descriptor) div sizeof(cli_field_descriptor),
                @person_descriptor);
      if rc = cli_ok then begin
        table_created := True;
      end
      else if (rc <> cli_table_already_exists) and (rc <> cli_not_implemented) then begin
        writeln(Format('cli_create_table failed with code %d', [rc]));
        exit;
      end;

      Tables := nil;
      n := cli_show_tables(session, Tables);
      p1 := Tables^.name;
      writeln(Format('cli_show_tables(%d, &tables)', [session]));
      writeln(Format('  -> %d &table->name=0x%p tables->name="%s"', [n, p1, Tables^.name]));

      if Tables <> nil then
        SysFreeMem(Tables);

      Tables := nil;
      n := cli_show_tables(session, Tables);
      p1 := Tables^.name;
      writeln(Format('cli_show_tables(%d, &tables)', [session]));
      writeln(Format('  -> %d &table->name=0x%p tables->name="%s"', [n, p1, Tables^.name]));

      if Tables <> nil then
        SysFreeMem(Tables);

      //writeln(Format('cli_describe(%d, "%s") -> %d', [session, 'persons', cli_describe(session, PChar('persons'), fields)]));

      n := cli_show_tables(session, tables);
      writeln(Format('show_tables -> %d', [n]));
      tbl := tables;
      for i:=0 to n-1 do begin
        s := tbl^.name;
        writeln(Format('  %-12s', [s]));
        writeln(Format('cli_describe -> %d', [cli_describe(session, PChar(s), @fields)]));
        try
          //for j:=0 to High(fields) do
          //  writeln(Format(#9'%-12s'#9'%-15s'#9'%d', [string(fields[j].name), GetEnumName(TypeInfo(TCliVarType), fields[j].FieldType), fields[j].Flags]));
        finally
          SysFreeMem(fields);
        end;
      end;

      statement := CliCheck(cli_statement(session, 'insert into persons'), 'cli_statement failed');

      CliCheck(cli_column(statement, 'name',    Ord(cli_asciiz), nil, @p.name), 'cli_column 1 failed');
      CliCheck(cli_column(statement, 'salary',  Ord(cli_int8), nil, @p.salary), 'cli_column 1 failed');
      CliCheck(cli_column(statement, 'address', Ord(cli_pasciiz), @len, @p.address), 'cli_column 1 failed');
      CliCheck(cli_column(statement, 'weight',  Ord(cli_real8), nil, @p.weight), 'cli_column 1 failed');
      CliCheck(cli_array_column_ex(statement, 'subordinates', Ord(cli_array_of_oid), @p, set_subordinates, get_subordinates), 'cli_column 1 failed');

      p.name := 'John Smith';
      p.salary := 75000;
      p.address := '1 Guildhall St., Cambridge CB2 3NH, UK';
      p.weight := 80.3;
      p.n_subordinates := 0;
      p.subordinates := nil;
      CliCheck(cli_insert_struct(session, 'persons', @p, oid));
      //CliCheck(cli_insert(statement, @oid), 'cli_insert failed');

      p.name := 'Joe Cooker';
      p.salary := 100000;
      p.address := 'Outlook drive, 15/3';
      p.weight := 80.3;
      p.n_subordinates := 1;
      p.subordinates := @oid;
      rc := cli_insert(statement, nil);
      if (rc <> cli_ok) then begin
          writeln(Format('cli_insert 2 failed with code %d', [rc]));
          exit;
      end;

      rc := cli_get_oid(statement);

      rc := cli_free(statement);
      if (rc <> cli_ok) then begin
          writeln(Format('cli_free failed with code %d', [rc]));
          exit;
      end;

      writeln;
      writeln('Executing: "select * from persons');
      writeln('             where length(subordinates) < %sub and salary > %sal"');
      writeln;

      p.subordinates := nil;
      statement := cli_statement(session,
                                'select * from persons where ' +
                                'length(subordinates) < %subordinates and salary > %salary');
      if (statement < 0) then begin
          writeln(Format('cli_statement 2 failed with code %d', [rc]));
          exit;
      end;
      p.address := address;
      len := sizeof(address);

      CliCheck(cli_column(statement, 'name',    Ord(cli_asciiz), nil, @p.name), 'cli_column 2 failed');
      CliCheck(cli_column(statement, 'salary',  Ord(cli_int8), nil, @p.salary), 'cli_column 2 failed');
      CliCheck(cli_column(statement, 'address', Ord(cli_pasciiz), @len, @p.address), 'cli_column 2 failed');
      CliCheck(cli_column(statement, 'weight',  Ord(cli_real8), nil, @p.weight), 'cli_column 2 failed');
      CliCheck(cli_array_column_ex(statement, 'subordinates', Ord(cli_array_of_oid), @p, set_subordinates, get_subordinates), 'cli_column 2 failed');

      CliCheck(cli_parameter(statement, '%subordinates', Ord(cli_int4), @n), 'cli_parameter failed');
      CliCheck(cli_parameter(statement, '%salary',       Ord(cli_int4), @salary), 'cli_parameter failed');

      n := 2;
      salary := 90000;
      rc := cli_fetch(statement, cli_view_only);
      if (rc <> 1) then begin
          writeln(Format('cli_fetch 1 returns %d instead of 1', [rc]));
          exit;
      end;
      rc := cli_get_oid(statement);
      n := 10;
      salary := 50000;
      rc := cli_fetch(statement, cli_for_update);
      if (rc <> 2) then begin
        writeln(Format('cli_fetch 2 returns %d instead of 2', [rc]));
        exit;
      end;

      statement2 := CliCheck(cli_statement(session, 'select * from persons where current = %oid'), 'cli_statement 3 failed');

      CliCheck(cli_column(statement2, 'name', Ord(cli_asciiz), nil, @name[0]), 'cli_column 3 failed');

      CliCheck(cli_parameter(statement2, '%oid', Ord(cli_oid), @oid), 'cli_parameter 3 failed');

      rc := cli_get_next(statement);

      while rc = cli_ok do begin
        writeln(Format('%s'#9'%d'#9'%n'#9'%s', [p.name, p.salary, p.weight, p.address]));
        if (p.n_subordinates > 0) then begin
            writeln('Manages:');
            for i:=0 to p.n_subordinates-1 do begin
              oid := TCliOidArray(p.subordinates^)[i];
              rc := cli_fetch(statement2, cli_view_only);
              if (rc <> 1) then begin
                writeln(Format('cli_fetch by oid failed with code %d', [rc]));
                exit;
              end;
              rc := cli_get_first(statement2);
              if (rc <> cli_ok) then begin
                writeln(Format('cli_get_first failed with code %d', [rc]));
                exit;
              end;
              writeln(Format(#9'%s', [name]));
            end;
        end;
        p.salary := p.salary*90 div 100;
        CliCheck(cli_update(statement), 'cli_update failed');
        rc := cli_get_next(statement);
      end;

      if (rc <> cli_not_found) then begin
          writeln(Format('cli_get_next failed with code %d', [rc]));
          exit;
      end;

      CliCheck(cli_free(statement), 'cli_free 2 failed');
      CliCheck(cli_free(statement2), 'cli_free 2 failed');

      CliCheck(cli_commit(session), 'cli_commit failed');

      writeln;
      writeln('Executing: "select * from persons order by salary"');

      statement := CliCheck(cli_statement(session, 'select * from persons order by salary'), 'cli_statement 4 failed');

      CliCheck(cli_column(statement, 'salary', Ord(cli_int4), nil, @salary), 'cli_column 4 failed');

      rc := cli_fetch(statement, cli_for_update);
      if (rc <> 2) then begin
        writeln(Format('cli_fetch 4 failed with code %d', [rc]));
        exit;
      end;
      writeln('New salaries:');

      rc := cli_get_prev(statement);
      while (rc = cli_ok) do begin
        writeln(Format(#9'%d', [salary]));
        rc := cli_get_prev(statement);
      end;
      if (rc <> cli_not_found) then begin
        writeln(Format('cli_get_prev failed with code %d', [rc]));
        exit;
      end;

      CliCheck(cli_free(statement), 'cli_free 3 failed');

      if table_created then begin
        rc := CliCheck(cli_show_tables(session, tables), 'cli_show_tables failed');
        try
          writeln('Tables:');
          tbl := tables;
          for i:=0 to rc-1 do begin
            writeln(Format(#9'%-12s', [tbl.name]));
            Inc(tbl);
          end;
        finally
          SysFreeMem(tables);
        end;

        rc := CliCheck(cli_describe(session, 'persons', @fields), Format('cli_describe failed with code %d', [rc]));
        try
          writeln('Fields:');
          fld := fields;
          for i:=0 to rc-1 do begin
            writeln(Format(#9'%-12s'#9'%d'#9'%d', [fld.name, Ord(fld.FieldType), fld.Flags]));
            Inc(fld);
          end;
        finally
          SysFreeMem(fields);
        end;
      end;

      writeln('CLI test sucessfully passed!');
    except
      on e : Exception do
        writeln(e.message);
    end;
  finally
    if table_created then
      CliCheck(cli_drop_table(session, 'persons'), 'cli_drop_table failed');

    try
      CliCheck(cli_close(session), 'cli_close failed');
    finally
      writeln('Press <Enter> to continue...');
      readln;
    end;
  end;

end.
