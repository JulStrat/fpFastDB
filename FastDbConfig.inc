//---------------------------------------------------------------------------
// This is a configuration file for FastDb VCL library
//---------------------------------------------------------------------------
{$ASSERTIONS ON}
{$D+}

(*
{$IFDEF MSWINDOWS}
  {$DEFINE USE_UNDERSCORE_PREFIX}    // Use '_' for DLL procedure names
{$ENDIF}
*)
//---------------------------------------------------------------------------
// Debug tracing
//---------------------------------------------------------------------------
{.$DEFINE CLI_DEBUG}
{.$DEFINE CODESITE_DEBUG}

{.$DEFINE USE_DEBUG_THREAD_PREFIX}
{$IF Defined(FPC)}
{$MODE Delphi}
{$PACKRECORDS C}
{$ENDIF}
