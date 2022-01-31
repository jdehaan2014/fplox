unit uCommon;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  UINT8_COUNT = UINT8.MaxValue + 1; //Byte.MaxValue + 1;

  MaxCases = 256; // max no of cases in switch statement

  IntSize = SizeOf(Integer);
  ByteSize = SizeOf(Byte);
  CharSize = SizeOf(Char);

type
  size_t = DWord;


procedure WriteFmt(const Fmt: String; const Args: array of const);
procedure WriteLnFmt(const Fmt: String; const Args: array of const);

//{$DEFINE DEBUG_PRINT_CODE}
{$DEFINE DEBUG_TRACE_EXECUTION}
//{$DEFINE DEBUG_STRESS_GC}
//{$DEFINE DEBUG_LOG_GC}

var
  DebugPrintCode: Boolean = {$ifdef DEBUG_PRINT_CODE}true{$else}false{$endif};
  DebugTraceExecution: Boolean = {$ifdef DEBUG_TRACE_EXECUTION}true{$else}false{$endif};
  DebugStressGC: Boolean = {$ifdef DEBUG_STRESS_GC}true{$else}false{$endif};
  DebugLogGC: Boolean = {$ifdef DEBUG_LOG_GC}true{$else}false{$endif};

implementation

procedure WriteFmt(const Fmt: String; const Args: array of const);
begin
  Write(Format(Fmt, Args));
end;

procedure WriteLnFmt(const Fmt: String; const Args: array of const);
begin
  WriteLn(Format(Fmt, Args));
end;

end.

