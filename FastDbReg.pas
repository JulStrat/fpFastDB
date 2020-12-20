unit FastDbReg;

interface
  uses Classes, FastDbSession, FastDbQuery;

procedure Register;

implementation
{$R FastDB.dcr}

procedure Register;
begin
  RegisterComponents('Data Access', [TFastDbSession, TFastDbQuery]);
end;


end.
