program TestRefTables;
{$IF Defined(FPC)}
{$MODE Delphi}
{$PACKRECORDS C}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  FastDbSession,
  FastDbQuery,
  FastDbVar,
  FastDbCLI;

var
  session : TFastDbSession;
  q       : TFastDbQuery;
  flds    : TFastDbFields;
  oidSwitch, oidSwitch2, oidLocation, oidCarrier : Integer;
  buffer: array[0..1] of TCliOID;
  bRefCheck : Boolean;
  bOrder    : Boolean;
  bUseIndex : Boolean;
  n         : Integer;
  f         : TFieldDescriptors;
begin
  if ParamCount < 2 then
    begin
      writeln('Usage: '+AppName+' InitDatabaseSize InitIndexSize [-describe] [-order]');
      writeln('                   (sizes are in kBytes)');
      writeln('                   -describe forces a check of an inverse reference');
      writeln('                   -order    changes field creationi order in Demand');
      writeln('                   -index    builds a hash index on Demand.SwitchLocationID');
      writeln('Example: '+AppName+' 10 100 -describe');
      halt;
    end;

  bRefCheck := FindCmdLineSwitch('describe', ['-'], True);
  bOrder    := FindCmdLineSwitch('order',    ['-'], True);
  bUseIndex := FindCmdLineSwitch('index',    ['-'], True);

  session := TFastDbSession.Create(nil);
  q       := TFastDbQuery.Create(nil);
  flds    := TFastDbFields.Create(nil);
  session.Database         := 'Test';
  session.DatabasePath     := 'Test.fdb';
  session.InitDatabaseSize := StrToInt(ParamStr(1))*1024;
  session.InitIndexSize    := StrToInt(ParamStr(2))*1024;

  DeleteFile(session.DatabasePath);

  session.Connected := True;
  q.Session := session;

  //------------------------------
  //-----QualityGroup-------------
  //------------------------------
  q.Clear;
  q.fields.Add('QualityID',    ctInt2,  [itHash]);
  q.fields.Add('QualityGroup', ctString);
  q.fields.Add('MinQuality',   ctInt1);
  q.fields.Add('MaxQuality',   ctInt1);
  q.fields.Add('Demands',      ctArrayOfOID, [], 'Demand', 'QualityGroups');
  session.CreateTable('QualityGroup', q.fields, True);

  //------------------------------
  //-----Demand-------------------
  //------------------------------
  q.Clear;
  if bOrder then
    q.fields.Add('QualityGroups',    ctArrayOfOID,  [], 'QualityGroup',  'Demands');
  q.fields.Add('Switch',           ctOID,         [], 'Switch',   'Demands');
  q.fields.Add('Location',         ctOID,         [], 'Location', 'Demands');
  if not bOrder then
    q.fields.Add('QualityGroups',    ctArrayOfOID,  [], 'QualityGroup',  'Demands');
  if bUseIndex then
    q.fields.Add('SwitchLocationID', ctInt8, [{itHash}])
  else
    q.fields.Add('SwitchLocationID', ctInt8, [itHash]);
  q.fields.Add('Demands',          ctArrayOfInt4);

//  for n:=0 to q.Fields.Count-1 do
//    if q.Fields[n].RefTable <> '' then
//      session.DescribeTable(q.Fields[n].RefTable, flds, False);

  session.CreateTable('Demand', q.fields, False);

  //------------------------------
  //-----Switch-------------------
  //------------------------------
  q.Clear;
  q.fields.Add('SwitchID',           ctInt2,   [itHash]);
  q.fields.Add('SwitchName',         ctString);
  q.fields.Add('Carriers',           ctArrayOfOID, [], 'Carrier', 'Switch');
  q.fields.Add('Demands',            ctArrayOfOID, [], 'Demand',  'Switch');
  q.Fields.Add('Rates',              ctArrayOfOID, [], 'Rate',    'Switch');
  session.CreateTable('Switch', q.fields, bRefCheck);

  //------------------------------
  //-----Location-----------------
  //------------------------------
  q.Clear;
  q.fields.Add('LocationID',       ctInt4,  [itHash]);
  q.fields.Add('LocationName',     ctString);
  q.fields.Add('Rates',            ctArrayOfOID, [], 'Rate',    'Location');
  q.fields.Add('Demands',          ctArrayOfOID, [], 'Demand',  'Location');
  session.CreateTable ('Location', q.fields, bRefCheck);

  //q.Clear;
  //session.DescribeTable('Switch', q.Fields);

  //session.Commit(True);
  //------------------------------
  //-----Carrier------------------
  //------------------------------
  q.Clear;
  //q.fields.Add('Switch',            ctOID,        [], 'Switch',  'Carriers');
  q.fields.Add('Switch',            ctArrayOfOID, [], 'Switch',  'Carriers');
  q.fields.Add('CarrierID',         ctInt4,       [itHash]);
  q.fields.Add('SwitchCarrierID',   ctInt8,       [itHash]);
  q.fields.Add('CarrierName',       ctString);
  q.fields.Add('Rates',             ctArrayOfOID, [], 'Rate',    'Carrier');
  q.fields.Add('TotalCapacity',     ctInt4);
  q.fields.Add('AvailableCapacity', ctInt4);
  q.fields.Add('TotalLines',        ctInt2);
  q.fields.Add('AvailableLines',    ctInt2);
  session.CreateTable('Carrier', q.fields, bRefCheck);

  //------------------------------
  //-----Rate---------------------
  //------------------------------
  q.Clear;
  q.fields.Add('CarrierLocationID',    ctInt8,       [itHash]);
  q.fields.Add('Switch',               ctOID,        [], 'Switch',   'Rates');
  q.fields.Add('Location',             ctOID,        [], 'Location', 'Rates');
  q.fields.Add('Carrier',              ctOID,        [], 'Carrier',  'Rates');
  q.fields.Add('Quality',              ctInt1);
  q.fields.Add('Rate',                 ctReal4);
  q.fields.Add('RealDemand',           ctInt4);
  session.CreateTable('Rate', q.fields, bRefCheck);

  q.Clear;
  q.SQL := 'insert into Switch';
  q.Describe;
  q.Fields[0].asInteger := 1;
  q.Fields[1].asString  := 'Test Sw1';
  oidSwitch := q.Insert;
  q.Fields[0].asInteger := 2;
  q.Fields[1].asString  := 'Test Sw2';
  oidSwitch2 := q.Insert;

  q.Clear;
  q.SQL := 'insert into Location';
  q.Describe;
  q.Fields[0].asInteger := 10;
  q.Fields[1].asString  := 'Test Loc';
  oidLocation := q.Insert;

  q.Clear;
  q.SQL := 'insert into Carrier';
  q.Describe;
  q.Fields[0].ArraySize := 2;
  buffer[0] := oidSwitch;
  buffer[1] := oidSwitch2;
  q.Fields[0].SetValue(@buffer[0], 2*SizeOf(TCliOID));
  //q.Fields[0].asArrayOID[0] := oidSwitch;
  //q.Fields[0].asArrayOID[1] := oidSwitch2;
  //q.Fields[0].asOID     := oidSwitch;
  q.Fields[1].asInteger := 100;
  q.Fields[3].asString  := 'Test Carrier';
  oidCarrier := q.Insert;

  q.Clear;
  q.SQL := 'insert into Rate';
  q.Describe;
  q.Field('Switch').asOID   := oidSwitch;
  q.Field('Location').asOID := oidLocation;
  q.Field('Carrier').asOID  := oidCarrier;
  q.Field('Rate').asSingle  := 345.25;
  oidCarrier := q.Insert;

  q.Clear;
  q.SQL := 'select * from Rate where Switch.SwitchID = %d';
  q.Variables.Add('d', ctInt4);
  q.Variables[0].asInteger := 1;
  n := q.Execute;
  if n <> 1 then
    writeln('Error: "select * from Rate" returned '+IntToStr(n)+' records!');

  session.Commit(True);
  writeln('Success.  Press <Enter> to exit...');
  readln;
end.
