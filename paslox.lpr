program paslox;

{$mode objfpc}{$H+}

uses
  SysUtils, uChunk, uMemory, uValue, uDebug, uVM;

  procedure Repl;
  var
    Line: String;
  begin
    while True do
      begin
        Write('paslox > ');
        ReadLn(Line);
        if Line = '' then Break;
        Line := Line;
        Interpret(Line);
      end;
  end;


  function ReadFile(const FileName: TFileName): String;
  var
    InputFile: THandle;
    FileSize, BytesRead: Integer;
    Buffer: String;
  begin
    try
      InputFile := FileOpen(FileName, fmOpenRead);
      if InputFile = -1 then
        begin
          WriteLn(Format('Error opening file "%s".', [FileName]));
          Halt(74);
        end;

      FileSize := FileSeek(InputFile, 0, fsFromEnd);
      SetLength(Buffer, FileSize);
      FileSeek(InputFile, 0, fsFromBeginning);

      BytesRead := FileRead(InputFile, Buffer[1], FileSize);

      if BytesRead < FileSize then
        begin
          WriteLn(Format('Error reading file "%s".', [FileName]));
          Halt(74);
        end;

      Result := Buffer;
    finally
      FileClose(InputFile);
    end;
  end;

  procedure RunFile(const Path: String);
  var
    Source: String;
    InterpretResult: TInterpretResult;
  begin
    Source := ReadFile(Path);
    InterpretResult := Interpret(Source);

    if InterpretResult = irCompileError then Halt(65);
    if InterpretResult = irRuntimeError then Halt(70);
  end;

begin
  InitVM;

  case ParamCount of
    0: Repl;
    1: RunFile(ParamStr(1));
    else
      WriteLn('Usage: paslox [path]');
      Halt(64);
  end;

  FreeVM;
end.

