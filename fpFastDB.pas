{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fpFastDB;

{$warn 5023 off : no warning about unused units}
interface

uses
  FastDbCLI, FastDbQuery, FastDbReg, FastDbSession, FastDbVar, FastDbDataSet, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FastDbReg', @FastDbReg.Register);
end;

initialization
  RegisterPackage('fpFastDB', @Register);
end.
