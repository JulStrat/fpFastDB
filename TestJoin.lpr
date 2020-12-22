program TestJoin;
{$IF Defined(FPC)}
{$MODE Delphi}
{$PACKRECORDS C}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  FastDbCLI,
  FastDbVar,
  FastDbQuery,
  FastDbSession;

var
  session : TFastDbSession;
  query, qAccount, qCode, qLocation: TFastDbQuery;
  oid1, oid2 : TCliOID;
  n : Integer;
begin
  session := TFastDbSession.Create(nil);
  query     := TFastDbQuery.Create(nil);
  qAccount  := TFastDbQuery.Create(nil);
  qCode     := TFastDbQuery.Create(nil);
  qLocation := TFastDbQuery.Create(nil);
  query.Session     := session;
  qAccount.Session  := session;
  qCode.Session     := session;
  qLocation.Session := session;

  if (ParamCount = 0) and DeleteFile('Test.fdb') then   // Don't reinitialize the database is a parameter is passed
    writeln('Database deleted');

  session.Database         := 'Test';
  session.DatabasePath     := 'Test.fdb';
  session.InitDatabaseSize := 4*1024;
  session.InitIndexSize    := 1*1024;
  {$IF NOT Defined(FPC)}
  session.InverseReferenceCheck := False;
  {$ENDIF}
  session.Connected := True;

  if (ParamCount = 0) then
    begin
      // This example illustrates the following relationships using reverse references:
      // Location  one-to-many    DialCode
      // Account   many-to-many   DialCode
      //-------------------------
      //---- DialCode -----------
      //-------------------------
      qCode.Fields.Add('Code',      ctString,          [itHash, itTree]);
      qCode.Fields.Add('Location',  ctOID,             [], 'Location',  'Codes');
      qCode.Fields.Add('Accounts',  ctArrayOfOID,      [], 'Account',   'Codes');
      session.CreateTable('DialCode', qCode.Fields);

      //-------------------------
      //---- Location -----------
      //-------------------------
      qLocation.Fields.Add('LocationID', ctInt4,       [itTree]);
      qLocation.Fields.Add('Codes',      ctArrayOfOID, [], 'DialCode',  'Location');
      qLocation.Fields.Add('Accounts',   ctArrayOfOID, [], 'Account',   'Locations');
      session.CreateTable('Location', qLocation.Fields);

      //-------------------------
      //---- Account ------------
      //-------------------------
      qAccount.Fields.Add('AccountID',   ctInt4,       [itTree]);
      qAccount.Fields.Add('Codes',       ctArrayOfOID, [], 'DialCode',  'Accounts');
      qAccount.Fields.Add('Locations',   ctArrayOfOID, [], 'Location',  'Accounts');
      session.CreateTable('Account', qAccount.Fields);

      qLocation.SQL := 'insert into Location';
      qLocation.Field('LocationID').asInteger := 15;
      oid1 := qLocation.Insert;

      qAccount.SQL := 'insert into Account';
      qAccount.Field('AccountID').asInteger  := 10;
      qAccount.Field('Locations').ArraySize  := 1;
      qAccount.Field('Locations').asArrayOID[0] := oid1;
      oid2 := qAccount.Insert;

      qCode.SQL := 'insert into DialCode';
      qCode.Fields[0].asString := '123';
      qCode.Fields[1].asOID    := oid1;
      qCode.Fields[2].ArraySize  := 1;
      qCode.Fields[2].asArrayOID[0] := oid2;
      qCode.Insert;

      session.Commit(True);
    end;

  // Test DialCode<-Location reference
  query.Clear;
  query.SQL := 'select * from DialCode where Location.LocationID = 15';
  n := query.Execute;
  writeln(Format('Found %d records', [n]));

  query.Clear;
  //query.SQL := 'select * from Location where exists i: (Codes[i].Code = ''123'')';
  query.SQL := 'select * from Account where exists i: (current in Codes[i].Accounts)';

  n := query.Execute;
  writeln(Format('Found %d records', [n]));

  query.Clear;
  query.SQL := 'select * from DialCode where length(Accounts) > 0';
  n := query.Execute;
  writeln(Format('Found %d records', [n]));

  query.SQL := 'select * from DialCode where length(Accounts) > 0 and exists i: (Accounts[i].AccountID = 10)';
  n := query.Execute;
  writeln(Format('Found %d records', [n]));

  session.Rollback;

  // Free resources
  query.Free;
  qAccount.Free;
  qCode.Free;
  qLocation.Free;
  session.Free;
end.
