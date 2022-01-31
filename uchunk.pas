unit uChunk;

{$mode delphi}{$H+}
{$modeswitch typehelpers}
{$PointerMath ON}

interface

uses
  SysUtils,
  uMemory, uValue;

type

  TOpcode = (
    op_Constant,
    op_ConstantLong,
    op_Nil, op_True, op_False,
    op_Pop,
    op_Dup,
    op_Get_Local, op_Set_Local,
    op_Get_Global, op_Define_Global, op_Set_Global,
    op_Get_UpValue, op_Set_UpValue,
    op_Get_Property, op_Set_Property,
    op_Get_Super,
    op_Equal, op_Greater, op_Less,
    op_Add, op_Subtract, op_Multiply, op_Divide,
    op_Not, op_Negate,
    op_Print,
    op_Jump, op_Jump_If_False, op_Loop,
    op_Call,
    op_Invoke,
    op_Super_Invoke,
    op_Closure,
    op_Close_UpValue,
    op_Return,
    op_Class,
    op_Inherit,
    op_Method
  );

  TOpCodeHelper = type helper for TOpcode
    function toStr: String;
  end;

  PChunk = ^TChunk;
  TChunk = record
    Code: PByte;  // holds dynamic array of byte
    Count,
    Capacity: Longint;
    Constants: TValueArray;
    Lines: PInteger;
  end;

procedure InitChunk(Chunk: PChunk);
procedure WriteChunk(Chunk: PChunk; const AByte: Byte; const Line: Integer); overload;
procedure WriteChunk(Chunk: PChunk; const OpCode: TOpcode; const Line: Integer); overload;
procedure WriteConstant(Chunk: PChunk; const Value: TValue; const Line: Integer);
function AddConstant(Chunk: PChunk; Value: TValue): Byte;
procedure FreeChunk(Chunk: PChunk);

implementation
uses uVM, uCommon;

procedure InitChunk(Chunk: PChunk);
begin
  Chunk^.Count := 0;
  Chunk^.Capacity := 0;
  Chunk^.Code := Nil;
  Chunk^.Lines := Nil;
  InitValueArray(@Chunk^.Constants);
end;

procedure WriteChunk(Chunk: PChunk; const AByte: Byte; const Line: Integer);
var
  OldCapacity: LongInt;
begin
  if Chunk^.Capacity < Chunk^.Count + 1 then
    begin
      OldCapacity := Chunk^.Capacity;
      Chunk^.Capacity := Grow_Capacity(OldCapacity);

      Chunk^.Code := PByte(Reallocate(
        Chunk^.Code, ByteSize*OldCapacity, ByteSize*Chunk^.Capacity));

      Chunk^.Lines := PInteger(Reallocate(
        Chunk^.Lines, IntSize*OldCapacity, IntSize*Chunk^.Capacity));
    end;
  Chunk^.Code[Chunk^.Count] := AByte;
  Chunk^.Lines[Chunk^.Count] := Line;
  Inc(Chunk^.Count);
end;

procedure WriteChunk(Chunk: PChunk; const OpCode: TOpcode; const Line: Integer);
begin
  WriteChunk(Chunk, Ord(OpCode), Line);
end;

procedure WriteConstant(Chunk: PChunk; const Value: TValue; const Line: Integer);
var
  Index: Integer;
begin
  Index := AddConstant(Chunk, Value);
  if Index < 256 then
    begin
      WriteChunk(Chunk, op_Constant, Line);
      WriteChunk(Chunk, Byte(Index), Line);
    end
  else
    begin
      WriteChunk(Chunk, op_ConstantLong, Line);
      WriteChunk(Chunk, Byte(Index and $FF), Line);
      WriteChunk(Chunk, Byte((Index shr 8) and $FF), Line);
      WriteChunk(Chunk, Byte((Index shr 16) and $FF), Line);
    end;
end;

function AddConstant(Chunk: PChunk; Value: TValue): Byte;
begin
  Push(Value);
  WriteValueArray(@Chunk^.Constants, Value);
  Pop;
  Result := Chunk^.Constants.Count - 1;
end;


procedure FreeChunk(Chunk: PChunk);
begin
  Reallocate(Chunk^.Code, ByteSize*Chunk^.Capacity, 0); // Free_Array
  Reallocate(Chunk^.Lines, IntSize*Chunk^.Capacity, 0); // Free_Array

  FreeValueArray(@Chunk^.Constants);
  InitChunk(Chunk);
end;

{ TOpCodeHelper }

function TOpCodeHelper.toStr: String;
begin
  WriteStr(Result, Self);
  Result := Uppercase(Result);
end;

end.

