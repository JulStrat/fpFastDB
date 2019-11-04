program ExtractDDL;

{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  FastDbSession;

var
  session : TFastDbSession;
begin
  if ParamCount < 1 then
    begin
      writeln('Usage: '+LowerCase(ExtractFileName(ParamStr(0)))+' datafile.fdb');
      halt;
    end;
  if not FileExists(ParamStr(1)) then
    begin
      writeln('File '+ParamStr(1)+' not found!');
      halt;
    end;

  session := TFastDbSession.Create(nil);
  session.DatabasePath := ParamStr(1);
  session.Database     := ChangeFileExt(ExtractFileName(ParamStr(1)), '');
  session.Connected    := True;
  try
    session.SaveDDLtoFile(ChangeFileExt(ParamStr(1), '.sql'));
  finally
    session.Connected    := False;
  end;
end.
 