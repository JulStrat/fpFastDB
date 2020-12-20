program TestFastDB;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  TypInfo,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  FastDbCLI in 'FastDbCLI.pas',
  FastDbSession in 'FastDbSession.pas',
  FastDbVar in 'FastDbVar.pas',
  FastDbQuery in 'FastDbQuery.pas';

const
  serverURL        = 'localhost:6100';
  DatabaseName     = 'clitest';
  filePath         = DatabaseName + '.fdb';
  csTableName      = 'persons';
  DEF_INSERT_COUNT = 3;

const
  person_descriptor: array[0..4] of TCliFieldDescriptor = (
    (FieldType:cli_asciiz;       Flags:cli_hashed;  Name:'name';    refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_int8;         Flags:cli_indexed; Name:'salary';  refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_pasciiz;      Flags:0;           Name:'address'; refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_real8;        Flags:0;           Name:'weight';  refTableName:nil; inverseRefFieldName:nil),
    (FieldType:cli_array_of_oid; Flags:0;           Name:'subordinates'; refTableName:'persons'; inverseRefFieldName:nil));

type
  TTestThread = class(TThread)
  private
    FDatabase: TFastDbSession;
  public
    constructor Create(Database: TFastDbSession);
    procedure Execute; override;
  end;

  TTestFastDb = class
  private
    procedure OnTraceEvent(Sender: TFastDbSession; Msg: string);
  public
    procedure DoMain;
  end;

  //---------------------------------------------------------------------------
  procedure TTestFastDb.DoMain;
  var
    rc : Integer;
    i, j, n: Integer;
    table_created : Boolean;
    oid : TCliOid;
    fields : TFieldDescriptors;
    Database : TFastDbSession;
    Query    : TFastDbQuery;
    Query2   : TFastDbQuery;
    str : TStringList;
    s1, s2: string;

    procedure DisplayResult(AQuery: TFastDbQuery; All: Boolean=True);
    begin
      with AQuery do
        if All then
          writeln(Format('(%4d) %-23s %7d %-3.1n (%d) %-28s', [
              OID,
              Field('name').asString,
              Field('salary').asInteger,
              Field('weight').asSingle,
              Field('subordinates').ArraySize,
              Field('address').asString]))
        else
          writeln(Format('(%4d) %-23s %-28s', [
              OID,
              Field('name').asString,
              Field('address').asString]));
    end;

    procedure DisplayTable;
    var Q: TFastDbQuery;
    begin
      Q := TFastDbQuery.Create(nil);
      try
        Q.Session := Database;
        //SQL := 'select * from persons where (5/0 = 1)';
        Q.SQL := 'select * from '+LowerCase(csTableName);
        writeln('Table '+csTableName+':');
        writeln('--OID-- ---------Name---------- -Wage- -Wt-- ----------Address-----------');

        Q.Execute;

        while not Q.Eof do begin
          DisplayResult(Q);
          Q.Next;
        end;
        writeln('-------------------------------------------------------------------------');
      finally
        Q.Free;
      end;
    end;

  begin
    Database := TFastDbSession.Create(nil);
    try
      Database.Host         := serverURL;
      Database.DatabasePath := FilePath;
      Database.Database     := databaseName;
      Database.InitDatabaseSize := 64*1024;
      Database.InitIndexSize    := 10*1024;
      Database.ExtensionQuantum := 20*1024;
      Database.OnTraceEvent     := OnTraceEvent;
      //Database.OpenAttributes := [oaReadOnly];
      try
        Database.Connected  := True;
      except
        on e: Exception do
          begin
            writeln(e.message);
            exit;
          end;
      end;

      str   := TStringList.Create;
      Query := TFastDbQuery.Create(nil);
      try
        Query.Session  := Database;
        //Query.OnDescribeField := DescribeQuery;

        Query.Fields.Add('name',         ctString, [itHash]);
        Query.Fields.Add('salary',       ctInt8,    [itTree]);
        Query.Fields.Add('address',      ctString);
        Query.Fields.Add('weight',       ctReal8);
        Query.Fields.Add('subordinates', ctArrayOfOID, [], csTableName);

        try
          table_created := Database.CreateTable(csTableName, Query.Fields);

          if table_created then
            writeln('Table "'+csTableName+'" created');
        except
          on e: Exception do
            begin
              writeln(e.message);
              exit;
            end;
        end;

        Query.Fields.Clear;
        Query.Fields.Add('name',         ctString, [itHash]);
        Query.Fields.Add('salary',       ctInt4,    [itTree]);
        Query.Fields.Add('address',      ctString);
        Query.Fields.Add('weight',       ctReal4);
        Query.Fields.Add('subordinates', ctArrayOfOID, [], csTableName);

        try
          Query.Session.AlterTable(csTableName, Query.Fields);
        except
          on e: Exception do
            begin
              writeln(e.message);
              exit;
            end;
        end;

        (*
        {Database.DescribeTable(csTableName, fields);
        if fields <> nil then
          SysFreeMem(fields);
        }
        pTables := nil;
        n := cli_show_tables(Database.Handle, pTables);
        p := pTables^.name;
        writeln(Format('[%d]: cli_show_tables(%d, &tables)', [GetCurrentThreadID, Database.Handle]));
        writeln(Format('[%d]    -> %d &table->name=0x%p tables->name="%s"', [GetCurrentThreadID, n, p, pTables^.name]));

        //if pTables <> nil then
        //  SysFreeMem(pTables);

        pTables := nil;
        n := cli_show_tables(Database.Handle, pTables);
        p := pTables^.name;
        writeln(Format('[%d]: cli_show_tables(%d, &tables)', [GetCurrentThreadID, Database.Handle]));
        writeln(Format('[%d]    -> %d &table->name=0x%p tables->name="%s"', [GetCurrentThreadID, n, p, pTables^.name]));

        //if pTables <> nil then
        //  SysFreeMem(pTables);
        *)
        Database.ListTables(str);

        writeln('Tables:');
        fields := nil;

        for i:=0 to str.Count-1 do begin
          writeln(Format('  %-12s', [str[i]]));
          writeln(Format('  %-12s', [StringOfChar('=', Length(str[i]))]));
          try
            Database.DescribeTable(str[i], fields);
            for j:=0 to High(fields) do
              writeln(Format(#9'%-12s'#9'%-15s'#9'%d', [string(fields[j].name), GetEnumName(TypeInfo(TCliVarType), fields[j].FieldType), fields[j].Flags]));
          finally
            fields := nil;
          end;
        end;

        Randomize;

        writeln('-------Performing Insert---------');
        Query.SQL := 'insert into '+csTableName;

        n := DEF_INSERT_COUNT;
        oid := 0;

        for i:=0 to n-1 do begin
          case (i mod 3) of
            1:  begin
                  s1 := 'John Smith';
                  s2 := '%d Guildhall St, %dNH';
                end;
            2:  begin
                  s1 := 'Joe Franklinstain';
                  s2 := '%d Outlook Dr #%d';
                end;
          else
                  s1 := 'Sam Ash';
                  s2 := '%d River Dr, %d/3';
          end;
          Query.Field(0).asString  := Format(s1+' [%d]', [Random(100)]);
          Query.Field(1).asInt8    := Random(100000);
          Query.Field(2).asString  := Format(s2, [Random(20), Random(50)]);
          Query.Field(3).asDouble  := Random(130);
          if oid <> 0 then
            begin
              Query.Field(4).ArraySize := 1;
              Query.Field('subordinates').asArrayOID[0] := oid;
            end
          else
            Query.Field(4).ArraySize := 0;

          //Query.Describe;  // needs to be called in order to set up the internal statement

          oid := Query.Insert;
        end;

        writeln(Format('inserted %d records', [n]));

        Database.Commit(True);

        // Store the OID of the 3rd record and try to do a Seek() later to find it
        Query.Clear;
        Query.SQL := 'select * from '+csTableName;
        if Query.Execute > 0 then
          begin
            Query.Skip(3);
            oid := Query.OID;
            Query.Close;
          end
        else
          oid := 0;

        if Query.Execute > 0 then
          begin
            // Test Query.Seek()
            if oid > 0 then
              Query.Seek(oid);

            DisplayTable;

            Query.First;    writeln(Format('cli_first()  -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Skip(3);  writeln(Format('cli_skip(3)  -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Skip(-2); writeln(Format('cli_skip(-2) -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Skip(3);  writeln(Format('cli_skip(3)  -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Last;     writeln(Format('cli_last()   -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Prev;     writeln(Format('cli_prev()   -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.First;    writeln(Format('cli_first()  -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Next;     writeln(Format('cli_next()   -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);

            writeln(Format('==>Found %3d records<==', [Query.RowCount]));
          end;

        Query.Variables.Clear;

        Query.Variables.Add('subordinates', ctInt4);
        Query.Variables.Add('salary',       ctInt8);
        Query.Variables[0].asInteger := 2;
        Query.Variables[1].asInteger := 90000;

        Query.SQL := 'select * from '+csTableName+' '#10'where length(subordinates) < %subordinates and salary > %salary';

        writeln('-------Executing:----------------');
        writeln(Query.Sql);
        writeln('Parameters:');
        writeln('===========');
        writeln(Query.Variables.asText);

        rc := Query.Execute;
        writeln(Format('==>Found %3d records<==', [rc]));
        {if (rc <> 1) then begin
            writeln(Format('cli_fetch 1 returns %d instead of 1', [rc]));
            //exit;
        end;}

        Query.Variables[0].asInteger := 2;
        Query.Variables[1].asInteger := 74999;

        writeln('-------Executing for update-----');
        writeln(Query.Sql);
        writeln('Parameters:');
        writeln('===========');
        writeln(Query.Variables.asText);

        rc := Query.Execute(False);

        writeln(Format('==>Found %3d records<==', [rc]));
        {if (rc <> 2) then begin
          writeln(Format('cli_fetch 2 returns %d instead of 2', [rc]));
        end;}

        Query2:= TFastDbQuery.Create(nil);
        try
          Query2.Session := Database;
          Query2.SQL := 'select * from '+csTableName+' where current = %oid';

          Query2.Fields.Add('name', ctString);
          Query2.Variables.Add('oid', ctOid);

          while not Query.Eof do begin
            try
              DisplayResult(Query);
              n := Query.Field('subordinates').ArraySize;

              for i:=0 to n-1 do begin
                if (i = 0) then writeln(#9'Manages:');

                // Note: The array access method below is the fastest
                // Query2.Variables[0].asOID := PIntegerArray(Query.Field('subordinates').asPointer)^[i];
                // We'll use the asArrayOID[] method instead, which is more elegant
                Query2.Variables[0].asOID := Query.Field('subordinates').asArrayOID[i];
                rc := Query2.Execute;
                if (rc <> 1) then begin
                  writeln(Format('cli_fetch by oid failed with code %d', [rc]));
                  exit;
                end;
                writeln(Format(#9'  (%d) %s', [Query2.OID, Query2.Field('name').asString]));
                Query2.Next;
              end;
            except
              on e: Exception do
                writeln('Error Subquery: '+ e.message);
            end;

            with Query.Field('salary') do asInteger := asInteger*90 div 100;

            Query.Update;
            writeln(Format('-----Record %4d updated-------', [Query.OID]));

            Query.Freeze;
            //Query2.Freeze;
            Database.Commit(True);
            //Query2.UnFreeze;
            Query.UnFreeze;

            Query.Next;
          end;

          writeln('Query.Eof reached');
        finally
          Query2.Free;
        end;

        Query.Close;

        Database.Commit(False);

        DisplayTable;

        writeln;
        writeln('--------Executing:--------------');
        writeln('select * from '+csTableName+' order by salary');
        writeln('---------------------------------');

        Query.SQL := 'select * from '+csTableName+' order by salary desc';
        Query.ClearVariables;

        Query.Execute;

        writeln('New salaries:');

        while not Query.Eof do begin
          writeln(Format('(%5d) %-23s %6d', [Query.OID, Query.Field('name').asString, Query.Field('salary').asInteger]));
          Query.Next;
        end;

      finally
        str.Free;
        Query.Free;
      end;

      with TTestThread.Create(Database) do
      try
        WaitFor;
      finally
        Free;
      end;

      writeln('CLI test sucessfully passed!');
    finally
      //if table_created then
      //  Database.DropTable(csTableName);

      Database.Free;
    end;
  end;

  //---------------------------------------------------------------------------
  // TTestThread
  //---------------------------------------------------------------------------
  constructor TTestThread.Create(Database: TFastDbSession);
  begin
    FDatabase := Database;
    FreeOnTerminate := False;
    inherited Create(False);
  end;

  //---------------------------------------------------------------------------
  procedure TTestThread.Execute;
  var
    q : TFastDbQuery;
    rc : Integer;
  begin
    FDatabase.Attach;
    q := TFastDbQuery.Create(nil);
    try
      q.Session := FDatabase;
      q.SQL := 'select * from '+csTableName+' order by salary';
      rc := q.Execute;
      writeln(Format('===>Threaded query returned %d records', [rc]));
    finally
      q.Free;
      FDatabase.Detach([dtDestroyContext]);
    end;
  end;

  //---------------------------------------------------------------------------
  procedure TTestFastDb.OnTraceEvent(Sender: TFastDbSession; Msg: string);
  begin
    writeln(Msg);
  end;

begin

  with TTestFastDb.Create do
  try
    try

      DoMain;
    except
      on e: Exception do
        writeln(e.message);
    end;

    {$IFDEF MSWINDOWS}
    writeln('Press <Enter> to continue...');
    readln;
    {$ENDIF}
  finally
    Free;
  end;

end.
