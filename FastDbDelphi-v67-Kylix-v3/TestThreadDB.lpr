program TestThreadDB;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}
{$I FastDbConfig.inc}

uses
  SysUtils,
  Classes,
  TypInfo,
  {$IFDEF MSWINDOWS}
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  {$IFDEF CODESITE_DEBUG}
  CSIntf,
  {$ENDIF}
  FastDbCLI     in 'FastDbCLI.pas',
  FastDbSession in 'FastDbSession.pas',
  FastDbVar     in 'FastDbVar.pas',
  FastDbQuery   in 'FastDbQuery.pas';

const
  serverURL        = 'localhost:6100';
  DatabaseName     = 'clitest';
  filePath         = DatabaseName + '.fdb';
  csTableName      = 'persons';
var
  DEF_INSERT_COUNT : Integer = 100000;
  DEF_THREAD_COUNT : Integer = 1;
  DEF_DATABASE_SIZE: Integer = 48*1024*1024;

type
  TTestFastDb = class
  public
    ActiveThreads: Integer;
    procedure Main;
    procedure OnTerminateThread(Sender: TObject);
  end;

  TTestThread = class(TThread)
  private
    FDatabase: TFastDbSession;
  public
    constructor Create(Database: TFastDbSession; TerminationEvent: TNotifyEvent);
    procedure Execute; override;
  end;

  procedure TTestFastDb.OnTerminateThread(Sender: TObject);
  begin
    Dec(ActiveThreads);
  end;

  procedure DoOutput(Msg: string='');
  begin
    {$IFDEF CODESITE_DEBUG}
    if Msg <> '' then CodeSite.SendMsg(Msg);
    {$ELSE}
    writeln(Msg);
    {$ENDIF}
  end;

  procedure TTestFastDb.Main;
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
          DoOutput(Format('(%4d) %-23s %7d %-3.1n (%d) %-28s', [
              OID,
              Field('name').asString,
              Field('salary').asInteger,
              Field('weight').asSingle,
              Field('subordinates').ArraySize,
              Field('address').asString]))
        else
          DoOutput(Format('(%4d) %-23s %-28s', [
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
        DoOutput('Table '+csTableName+':');
        DoOutput('--OID-- ---------Name---------- -Wage- -Wt-- ----------Address-----------');

        Q.Execute;

        while not Q.Eof do begin
          DisplayResult(Q);
          Q.Next;
        end;
        DoOutput('-------------------------------------------------------------------------');
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
      Database.InitDatabaseSize := DEF_DATABASE_SIZE;
      //Database.InitIndexSize    := 10*1024;
      //Database.ExtensionQuantum := 20*1024;
      {Database.InitDatabaseSize := 128*1024*1024;
      Database.InitIndexSize    := 4*1024*1024;
      Database.ExtensionQuantum := 64*1024*1024;}
      Database.Threaded := False;
      //Database.OpenAttributes := [oaReadOnly];
      try
        Database.Connected  := True;
      except
        on e: Exception do
          begin
            DoOutput(e.message);
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
            DoOutput('Table "'+csTableName+'" created');
        except
          on e: Exception do
            begin
              DoOutput(e.message);
              exit;
            end;
        end;

        Database.ListTables(str);
        
        Database.ListTables(str);

        DoOutput('Tables:');
        fields := nil;

        for i:=0 to str.Count-1 do begin
          DoOutput(Format('  %-12s', [str[i]]));
          DoOutput(Format('  %-12s', [StringOfChar('=', Length(str[i]))]));
          try
            Database.DescribeTable(str[i], fields);
            for j:=0 to High(fields) do
              DoOutput(Format(#9'%-12s'#9'%-15s'#9'%d', [string(fields[j].name), GetEnumName(TypeInfo(TCliVarType), fields[j].FieldType), fields[j].Flags]));
          finally
            fields := nil;
          end;
        end;

        Randomize;

        Database.Commit(False);

        n := GetTickCount;

        ActiveThreads := DEF_THREAD_COUNT;

        for i:=0 to DEF_THREAD_COUNT-1 do
          TTestThread.Create(Database, OnTerminateThread);

        while ActiveThreads > 0 do begin
          CheckSynchronize;
          sleep(10);
        end;

        n := GetTickCount - n;
        DoOutput(Format('Execution time for %d threads (%.0n inserts): %n sec (%n rec/sec)', [DEF_THREAD_COUNT, DEF_THREAD_COUNT*DEF_INSERT_COUNT/1, n/1000, DEF_THREAD_COUNT*DEF_INSERT_COUNT*1000/n]));
        n := GetTickCount;
        Database.Commit(True);
        n := GetTickCount - n;
        DoOutput(Format('Commit time: %n sec', [n/1000]));

        Database.Threaded := False;

        exit;

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

            Query.First;    DoOutput(Format('cli_first()  -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Skip(3);  DoOutput(Format('cli_skip(3)  -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Skip(-2); DoOutput(Format('cli_skip(-2) -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Skip(3);  DoOutput(Format('cli_skip(3)  -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Last;     DoOutput(Format('cli_last()   -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Prev;     DoOutput(Format('cli_prev()   -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.First;    DoOutput(Format('cli_first()  -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);
            Query.Next;     DoOutput(Format('cli_next()   -> OID: %d', [Query.OID]));
            DisplayResult(Query, False);

            DoOutput(Format('==>Found %3d records<==', [Query.RowCount]));
          end;

        Query.Variables.Clear;

        Query.Variables.Add('subordinates', ctInt4);
        Query.Variables.Add('salary',       ctInt8);
        Query.Variables[0].asInteger := 2;
        Query.Variables[1].asInteger := 90000;

        Query.SQL := 'select * from '+csTableName+' '#10'where length(subordinates) < %subordinates and salary > %salary';

        DoOutput('-------Executing:----------------');
        DoOutput(Query.Sql);
        DoOutput('Parameters:');
        DoOutput('===========');
        DoOutput(Query.Variables.asText);

        rc := Query.Execute;
        DoOutput(Format('==>Found %3d records<==', [rc]));
        {if (rc <> 1) then begin
            DoOutput(Format('cli_fetch 1 returns %d instead of 1', [rc]));
            //exit;
        end;}

        Query.Variables[0].asInteger := 2;
        Query.Variables[1].asInteger := 74999;

        DoOutput('-------Executing for update-----');
        DoOutput(Query.Sql);
        DoOutput('Parameters:');
        DoOutput('===========');
        DoOutput(Query.Variables.asText);

        rc := Query.Execute(False);

        DoOutput(Format('==>Found %3d records<==', [rc]));
        {if (rc <> 2) then begin
          DoOutput(Format('cli_fetch 2 returns %d instead of 2', [rc]));
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
                if (i = 0) then DoOutput(#9'Manages:');

                // Note: The array access method below is the fastest
                // Query2.Variables[0].asOID := PIntegerArray(Query.Field('subordinates').asPointer)^[i];
                // We'll use the asArrayOID[] method instead, which is more elegant
                Query2.Variables[0].asOID := Query.Field('subordinates').asArrayOID[i];
                rc := Query2.Execute;
                if (rc <> 1) then begin
                  DoOutput(Format('cli_fetch by oid failed with code %d', [rc]));
                  exit;
                end;
                DoOutput(Format(#9'  (%d) %s', [Query2.OID, Query2.Field('name').asString]));
                Query2.Next;
              end;
            except
              on e: Exception do
                DoOutput('Error Subquery: '+ e.message);
            end;

            with Query.Field('salary') do asInteger := asInteger*90 div 100;

            Query.Update;
            DoOutput(Format('-----Record %4d updated-------', [Query.OID]));

            Query.Freeze;
            //Query2.Freeze;
            Database.Commit(True);
            //Query2.UnFreeze;
            Query.UnFreeze;

            Query.Next;
          end;

          DoOutput('Query.Eof reached');
        finally
          Query2.Free;
        end;

        Query.Close;

        Database.Commit(False);

        DisplayTable;

        DoOutput;
        DoOutput('--------Executing:--------------');
        DoOutput('select * from '+csTableName+' order by salary');
        DoOutput('---------------------------------');

        Query.SQL := 'select * from '+csTableName+' order by salary desc';
        Query.ClearVariables;

        Query.Execute;

        DoOutput('New salaries:');

        while not Query.Eof do begin
          DoOutput(Format('(%5d) %-23s %6d', [Query.OID, Query.Field('name').asString, Query.Field('salary').asInteger]));
          Query.Next;
        end;

      finally
        str.Free;
        Query.Free;
      end;

      DoOutput('CLI test sucessfully passed!');
    finally
      //if table_created then
      //  Database.DropTable(csTableName);

      Database.Free;
    end;
  end;

  //---------------------------------------------------------------------------
  // TTestThread
  //---------------------------------------------------------------------------
  constructor TTestThread.Create(Database: TFastDbSession; TerminationEvent: TNotifyEvent);
  begin
    FDatabase := Database;
    FreeOnTerminate := True;
    OnTerminate := TerminationEvent;
    inherited Create(False);
  end;

  procedure TTestThread.Execute;
  var
    q : TFastDbQuery;
    oid : Integer;
    i,n  : Integer;
    s1, s2: string;
  begin
    DoOutput(Format('====>Thread %d<======', [ThreadID]));
    FDatabase.Attach;
    q := TFastDbQuery.Create(nil);
    try
      q.Session := FDatabase;
      q.Fields.Add('name',         ctString, [itHash]);
      q.Fields.Add('salary',       ctInt8,    [itTree]);
      q.Fields.Add('address',      ctString);
      q.Fields.Add('weight',       ctReal8);
      q.Fields.Add('subordinates', ctArrayOfOID, [], csTableName);

      q.SQL := 'insert into '+csTableName;
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
        q.Field(0).asString  := Format(s1+' [%d]', [Random(100)]);
        q.Field(1).asInt8    := Random(100000);
        q.Field(2).asString  := Format(s2, [Random(20), Random(50)]);
        q.Field(3).asDouble  := Random(130);
        if oid <> 0 then
          begin
            q.Field(4).ArraySize := 1;
            q.Field('subordinates').asArrayOID[0] := oid;
          end
        else
          q.Field(4).ArraySize := 0;

        FDatabase.attach;
        oid := q.Insert;
        FDatabase.Detach;
      end;
    finally
      q.Free;
      FDatabase.Detach([dtDestroyContext]);
    end;
  end;

begin

  with TTestFastDb.Create do
  try
    if ParamCount > 0 then
      DEF_THREAD_COUNT := StrToInt(ParamStr(1));
    if ParamCount > 1 then
      DEF_INSERT_COUNT := StrToInt(ParamStr(2));

    try
      Main;
    except
      on e: Exception do
        DoOutput(e.message);
    end;
    DoOutput('Press <Enter> to continue...');
    readln;
  finally
    Free;
  end;
end.


