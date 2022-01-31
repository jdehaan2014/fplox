unit uDebug;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, uChunk, uValue;

procedure DisassembleChunk(Chunk: PChunk; const Name: String);
function DisassembleInstruction(Chunk: PChunk; Offset: Longint): Longint;

implementation
uses uCommon, uObject;

procedure DisassembleChunk(Chunk: PChunk; const Name: String);
var
  Offset: Longint = 0;
begin
  WriteLnFmt('== %s ==', [Name]);
  while Offset < Chunk^.Count do
    Offset := DisassembleInstruction(Chunk, Offset);
end;

function SimpleInstruction(const Name: String; Offset: Longint): Longint;
begin
  WriteLnFmt('%s', [Name]);
  Result := Offset+1;
end;

function ConstantInstruction(const Name: String; Chunk: PChunk; Offset: Longint): Longint;
var
  Constant: Byte;
begin
  Constant := Chunk^.Code[Offset+1];
  WriteFmt('%-16s %4d "', [Name, Constant]);
  PrintValue(Chunk^.Constants.Values[Constant]);
  Writeln('"');
  Result := Offset+2;
end;

function LongConstantInstruction(const Name: String; Chunk: PChunk; Offset: Longint): Longint;
var
  Constant: DWord;
begin
  Constant := Chunk^.Code[Offset+1] or
              (Chunk^.Code[Offset+2] << 8) or
              (Chunk^.Code[Offset+3] << 16);
  WriteFmt('%-16s %4d "', [Name, Constant]);
  PrintValue(Chunk^.Constants.Values[Constant]);
  Writeln('"');
  Result := Offset+4;
end;

function InvokeInstruction(const Name: String; Chunk: PChunk; Offset: Longint): Longint;
var
  Constant, ArgCount: Byte;
begin
  Constant := Chunk^.Code[Offset+1];
  ArgCount := Chunk^.Code[Offset+2];
  WriteFmt('%-5s (%d args) %2d "', [Name, ArgCount, Constant]);
  PrintValue(Chunk^.Constants.Values[Constant]);
  WriteLn('"');
  Result := Offset + 3;
end;

function ByteInstruction(const Name: String; Chunk: PChunk; Offset: Longint): Longint;
var
  Slot: UInt8;
begin
  Slot := Chunk^.Code[Offset+1];
  WriteLnFmt('%-16s %4d', [Name, Slot]);
  Result := Offset + 2;
end;

function JumpInstruction(const Name: String; Sign: Integer; Chunk: PChunk;
  Offset: Longint): Integer;
var
  Jump: UInt16;
begin
  Jump := UInt16(Chunk^.Code[Offset+1] shl 8);
  Jump := Jump or Chunk^.Code[Offset+2];
  WriteLnFmt('%-16s %4d -> %d', [Name, Offset, Offset+3 + Sign * Jump]);
  Result := Offset + 3;
end;

function DisassembleInstruction(Chunk: PChunk; Offset: Longint): Longint;
var
  Instruction: TOpCode;
  Constant, isLocal, Index: Byte;
  Func: PObjFunc;
  j: Integer;
begin
  WriteFmt('%.4d ', [Offset]);
  if (Offset > 0) and (Chunk^.Lines[Offset] = Chunk^.Lines[Offset-1]) then
    Write('   | ')
  else
    Write(Format('%4d ', [Chunk^.Lines[Offset]]));

  Instruction := TOpCode(Chunk^.Code[Offset]);
  case Instruction of
    op_Constant:     Exit(ConstantInstruction(Instruction.toStr, Chunk, Offset));
    op_ConstantLong: Exit(LongConstantInstruction(Instruction.toStr, Chunk, Offset));
    op_Nil, op_True, op_False: Exit(SimpleInstruction(Instruction.toStr, Offset));
    op_Pop: Exit(SimpleInstruction(Instruction.toStr, Offset));
    op_Dup: Exit(SimpleInstruction(Instruction.toStr, Offset));
    op_Get_Local, op_Set_Local:
      Exit(ByteInstruction(Instruction.toStr, Chunk, Offset));
    op_Get_Global, op_Define_Global, op_Set_Global:
      Exit(ConstantInstruction(Instruction.toStr, Chunk, Offset));
    op_Get_UpValue, op_Set_UpValue:
      Exit(ByteInstruction(Instruction.toStr, Chunk, Offset));
    op_Get_Property, op_Set_Property:
      Exit(ConstantInstruction(Instruction.toStr, Chunk, Offset));
    op_Get_Super: Exit(ConstantInstruction(Instruction.toStr, Chunk, Offset));
    op_Equal, op_Greater, op_Less: Exit(SimpleInstruction(Instruction.toStr, Offset));
    op_Add, op_Subtract, op_Multiply, op_Divide:
      Exit(SimpleInstruction(Instruction.toStr, Offset));
    op_Not, op_Negate: Exit(SimpleInstruction(Instruction.toStr, Offset));
    op_Print: Exit(SimpleInstruction(Instruction.toStr, Offset));
    op_Jump, op_Jump_If_False:
      Exit(JumpInstruction(Instruction.toStr, 1, Chunk, Offset));
    op_Loop:
      Exit(JumpInstruction(Instruction.toStr, -1, Chunk, Offset));
    op_Call: Exit(ByteInstruction(Instruction.toStr, Chunk, Offset));
    op_Invoke: Exit(InvokeInstruction(Instruction.toStr, Chunk, Offset));
    op_Super_Invoke: Exit(InvokeInstruction(Instruction.toStr, Chunk, Offset));
    op_Closure:
      begin
        Inc(Offset);
        Constant := Chunk^.Code[Offset];
        Inc(Offset);
        WriteFmt('%-16s %4d ', [Instruction.toStr, Constant]);
        PrintValue(Chunk^.Constants.Values[Constant]);
        WriteLn;

        Func := asFunc(Chunk^.Constants.Values[Constant]);
        for j := 0 to Func^.UpValueCount-1 do
          begin
            isLocal := Chunk^.Code[Offset];
            Inc(Offset);
            Index := Chunk^.Code[Offset];
            Inc(Offset);
            WriteLnFmt('%.4d    |                     %s %d',
              [Offset-2, specialize IfThen<String>(isLocal=1, 'local', 'upvalue'), Index]);
          end;

        Result := Offset;
      end;
    op_Close_UpValue: Exit(SimpleInstruction(Instruction.toStr, Offset));
    op_Return: Exit(SimpleInstruction(Instruction.toStr, Offset));
    op_Class: Exit(ConstantInstruction(Instruction.toStr, Chunk, Offset));
    op_Inherit: Exit(SimpleInstruction(Instruction.toStr, Offset));
    op_Method: Exit(ConstantInstruction(Instruction.toStr, Chunk, Offset));
    otherwise
      WriteLnFmt('Unknown opcode %d', [Instruction]);
      Exit(Offset+1);
  end;
end;


end.

