unit uVM;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, uChunk, uValue, uObject, uDebug, uCompiler, uCommon, uTable;

const
  Frames_Max = 128;
  Stack_Max = Frames_Max * UINT8_COUNT;

type

  PCallFrame = ^TCallFrame;
  TCallFrame = record
    Closure: PObjClosure;
    IP: PByte;
    Slots: PValue;
  end;

  TInterpretResult = (irInterpretOK, irCompileError, irRuntimeError);

  TVM = record
    Frames: array[0..Frames_Max-1] of TCallFrame;
    FrameCount: Integer;

    Stack: array[0..Stack_Max-1] of TValue;
    StackTop: PValue;
    Globals: TTable;
    Strings: TTable;
    InitString: PObjString;
    OpenUpValues: PObjUpValue;

    BytesAllocated: size_t;
    NextGC: size_t;

    Objects: PObj;
    GrayCount: Integer;
    GrayCapacity: Integer;
    GrayStack: PPObj;
  end;

procedure InitVM;
procedure FreeVM;

function Interpret(const Source: String): TInterpretResult;

procedure Push(Value: TValue);
function Pop: TValue;

var
  VM: TVM;

implementation
uses uMemory, uNative;

procedure ResetStack;
begin
  VM.StackTop := @VM.Stack[0];
  VM.FrameCount := 0;
  VM.OpenUpValues := Nil;
end;

function RuntimeError(const Format: String; Args: array of const): TInterpretResult;
var
  Instruction: size_t;
  i: Integer;
  Frame: PCallFrame;
  Func: PObjFunc;
begin
  WriteLnFmt(Format, Args);

  for i := VM.FrameCount-1 downto 0 do
    begin
      Frame := @VM.Frames[i];
      Func := Frame^.Closure^.Func;
      // -1 because the IP is sitting on the next instruction to be executed.
      Instruction := Frame^.IP - PByte(Func^.Chunk.Code) - 1;
      WriteFmt('[line %d] in ', [Func^.Chunk.Lines[Instruction]]);
      if Func^.Name = Nil then
        WriteLn('script')
      else
        WriteLnFmt('%s()', [Func^.Name^.Chars]);
    end;

  ResetStack;
  Result := irRuntimeError;
end;

procedure DefineNative(const Name: PChar; Func: TNativeFn);
begin
  Push(ObjVal(CopyString(Name, Integer(strlen(Name)))));
  Push(ObjVal(NewNative(Func)));
  TableSet(@VM.Globals, asString(VM.Stack[0]), VM.Stack[1]);
  Pop;
  Pop;
end;

procedure DefineNatives;
begin
  DefineNative('pi', @PiNative);
  DefineNative('sqrt', @SqrtNative);
  DefineNative('sqr', @SqrNative);
  DefineNative('trunc', @TruncNative);
  DefineNative('round', @RoundNative);
  DefineNative('abs', @AbsNative);
  DefineNative('sin', @SinNative);
  DefineNative('cos', @CosNative);
  DefineNative('exp', @ExpNative);
  DefineNative('ln', @LnNative);
  DefineNative('frac', @FracNative);
  DefineNative('arctan', @ArcTanNative);
  DefineNative('milliseconds', @MilliSecondsNative);
  DefineNative('date', @DateNative);
  DefineNative('time', @TimeNative);
  DefineNative('now', @NowNative);
  DefineNative('today', @TodayNative);
  DefineNative('random', @RandomNative);
  DefineNative('randomize', @RandomizeNative);
  DefineNative('length', @LengthNative);
  DefineNative('floor', @FloorNative);
  DefineNative('ceil', @CeilNative);
  DefineNative('readln', @ReadlnNative);
  DefineNative('toNum', @ToNumNative);
  DefineNative('toStr', @ToStrNative);
  DefineNative('hasField', @HasFieldNative);
  DefineNative('getField', @GetFieldNative);
  DefineNative('setField', @SetFieldNative);
  DefineNative('delField', @DelFieldNative);
  DefineNative('error', @ErrorNative);
end;

procedure InitVM;
begin
  ResetStack;
  VM.Objects := Nil;
  VM.BytesAllocated := 0;
  VM.NextGC := 1024*1024;
  VM.GrayCount := 0;
  VM.GrayCapacity := 0;
  VM.GrayStack := Nil;
  InitTable(@VM.Globals);
  InitTable(@VM.Strings);
  VM.InitString := Nil;
  VM.InitString := CopyString('init', 4);
  DefineNatives;
end;

procedure FreeVM;
begin
  FreeTable(@VM.Globals);
  FreeTable(@VM.Strings);
  VM.InitString := Nil;
  FreeObjects;
end;

procedure Push(Value: TValue);
begin
  VM.StackTop^ := Value;
  Inc(VM.StackTop);
end;

function Pop: TValue;
begin
  Dec(VM.StackTop);
  Result := VM.StackTop^;
end;

function Peek(Distance: Integer): TValue;
begin
  Result := VM.StackTop[-1-Distance];
end;

function Call(Closure: PObjClosure; const ArgCount: Integer): Boolean;
var
  Frame: PCallFrame;
begin
  if ArgCount <> Closure^.Func^.Arity then
    begin
      RuntimeError('Expected %d arguments but got %d.',
        [Closure^.Func^.Arity, ArgCount]);
      Exit(False);
    end;

  if VM.FrameCount = Frames_Max then
    begin
      RuntimeError('Stack overflow.', []);
      Exit(False);
    end;

  Frame := @VM.Frames[VM.FrameCount];
  Inc(VM.FrameCount);
  Frame^.Closure := Closure;
  Frame^.IP := Closure^.Func^.Chunk.Code;

  Frame^.Slots := VM.StackTop-ArgCount-1;
  Result := True;
end;

function CallValue(Callee: TValue; const ArgCount: Integer): Boolean;
var
  Native: TNativeFn;
  Klass: PObjClass;
  Bound: PObjBoundMethod;
  Initializer: TValue;
begin
  if isObj(Callee) then
    case Obj_Typ(Callee) of
      obj_Bound_Method:
        begin
          Bound := asBoundMethod(Callee);
          VM.StackTop[-ArgCount - 1] := Bound^.Receiver;
          Exit(Call(Bound^.Method, ArgCount));
        end;
      obj_Class:
        begin
          Klass := asClass(Callee);
          VM.StackTop[-ArgCount-1] := ObjVal(NewInstance(Klass));
          if TableGet(@Klass^.Methods, VM.InitString, Initializer) then
            Exit(Call(asClosure(Initializer), ArgCount))
          else if ArgCount <> 0 then
            begin
              RuntimeError('Expected 0 arguments but got %d.', [ArgCount]);
              Exit(False);
            end;
          Exit(True);
        end;
      obj_Closure: Result := Call(asClosure(Callee), ArgCount);
      obj_Native:
        begin
          Native := asNative(Callee);
          Result := Native(ArgCount, VM.StackTop-ArgCount);
          if Result then
            Dec(VM.StackTop, ArgCount)
          else
            RuntimeError(asString(VM.StackTop[-ArgCount-1])^.Chars, []);
        end
      else // Non-callable object type.
    end
  else
    begin
      RuntimeError('Can only call functions and classes.', []);
      Result := False;
    end;
end;

function InvokeFromClass(Klass: PObjClass; Name: PObjString; const ArgCount: Integer): Boolean;
var
  Method: TValue;
begin
  if not TableGet(@Klass^.Methods, Name, Method) then
    begin
      RuntimeError('Undefined property "%s".', [Name^.Chars]);
      Exit(False);
    end;

  Result := Call(asClosure(Method), ArgCount);
end;

function Invoke(Name: PObjString; const ArgCount: Integer): Boolean;
var
  Receiver, Value: TValue;
  Instance: PObjInstance;
begin
  Receiver := Peek(ArgCount);

  if not isInstance(Receiver) then
    begin
      RuntimeError('Only instances have methods.', []);
      Exit(False);
    end;

  Instance := asInstance(Receiver);

  if TableGet(@Instance^.Fields, Name, Value) then
    begin
      VM.StackTop[-ArgCount - 1] := Value;
      Exit(CallValue(Value, ArgCount));
    end;

  Result := InvokeFromClass(Instance^.Klass, Name, ArgCount);
end;

function BindMethod(Klass: PObjClass; Name: PObjString): Boolean;
var
  Method: TValue;
  Bound: PObjBoundMethod;
begin
  if not TableGet(@Klass^.Methods, Name, Method) then
    begin
      RuntimeError('Undefined property "%s".', [Name^.Chars]);
      Exit(False);
    end;

  Bound := NewBoundMethod(Peek(0), asClosure(Method));
  Pop;
  Push(ObjVal(Bound));
  Result := True;
end;

function CaptureUpValue(Local: PValue): PObjUpValue;
var
  CreatedUpValue, PrevUpValue, UpValue: PObjUpValue;
begin
  PrevUpValue := Nil;
  UpValue := VM.OpenUpValues;

  while (UpValue <> Nil) and (UpValue^.Location > Local) do
    begin
      PrevUpValue := UpValue;
      UpValue := UpValue^.Next;
    end;

  if (UpValue <> Nil) and (UpValue^.Location = Local) then
    Exit(UpValue);

  CreatedUpValue := NewUpValue(Local);
  CreatedUpValue^.Next := UpValue;

  if PrevUpValue = Nil then
    VM.OpenUpValues := CreatedUpValue
  else
    PrevUpValue^.Next := CreatedUpValue;

  Result := CreatedUpValue;
end;

procedure CloseUpValues(Last: PValue);
var
  UpValue: PObjUpValue;
begin
  while (VM.OpenUpValues <> Nil) and (VM.OpenUpValues^.Location >= Last) do
    begin
      UpValue := VM.OpenUpValues;
      UpValue^.Closed := UpValue^.Location^;
      UpValue^.Location := @UpValue^.Closed;
      VM.OpenUpValues := UpValue^.Next;
    end;
end;

procedure DefineMethod(Name: PObjString);
var
  Method: TValue;
  Klass: PObjClass;
begin
  Method := Peek(0);
  Klass := asClass(Peek(1));
  TableSet(@Klass^.Methods, Name, Method);
  Pop;
end;

function isFalsey(Value: TValue): Boolean;
begin
  Result := isNil(Value) or (isBool(Value) and not asBool(Value));
end;

procedure Concatenate;
var
  a, b, Result: PObjString;
  Length: LongInt;
  Chars: PChar;
begin
  b := asString(Peek(0));
  a := asString(Peek(1));
  Length := a^.Length + b^.Length;
  Chars := StrAlloc(Length+1);
  StrMove(Chars, a^.Chars, a^.Length);
  StrCat(Chars, b^.Chars);
  Result := TakeString(Chars, Length);
  Pop;
  Pop;
  Push(ObjVal(PObj(Result)));
end;

function Run: TInterpretResult;
var
  Instruction: TOpcode;
  Constant, Value, FuncResult, SuperClass: TValue;
  Slot: PValue;
  SlotNum, isLocal, Index: Byte;
  ArgCount, i: Integer;
  Name, Method: PObjString;
  OffSet: UInt16;
  Frame: PCallFrame;
  a, b: Double;
  Func: PObjFunc;
  Closure: PObjClosure;
  Instance: PObjInstance;
  SubClass, Super: PObjClass;

  function ReadByte: Byte; inline;
  begin
    Result := Frame^.IP^;
    Inc(Frame^.IP); // point to next instruction
  end;

  function ReadShort: UInt16; inline;
  begin
    Inc(Frame^.IP, 2);
    Result := UInt16((Frame^.IP[-2] shl 8) or Frame^.IP[-1]);
  end;

  function ReadConstant: TValue; inline;
  begin
    Result := Frame^.Closure^.Func^.Chunk.Constants.Values[ReadByte];
  end;

  function ReadString: PObjString; inline;
  begin
    Result := asString(ReadConstant);
  end;

  procedure GetAB(var a, b: Double);
  begin
    b := asNumber(Pop);
    a := asNumber(Pop);
  end;

begin
  Frame := @VM.Frames[VM.FrameCount-1];
  while True do
    begin
      if DebugTraceExecution then // set in unit uCommon
        begin
          Write('          ');
          Slot := @VM.Stack[0];
          while Slot < VM.StackTop do
            begin
              Write('[ '); PrintValue(Slot^); Write(' ]');
              Inc(Slot);
            end;
          Writeln;
          DisassembleInstruction(@Frame^.Closure^.Func^.Chunk,
            LongInt(Frame^.IP - PByte(Frame^.Closure^.Func^.Chunk.Code)));
        end;

      Instruction := TOpCode(ReadByte);
      case Instruction of
        op_Constant:
          begin
            Constant := ReadConstant;
            Push(Constant);
          end;

        op_ConstantLong: begin {ToDo} end;

        op_Nil: Push(NilVal);
        op_True: Push(BoolVal(True));
        op_False: Push(BoolVal(False));

        op_Pop: Pop;

        op_Dup: Push(Peek(0));

        op_Get_Local:
          begin
            SlotNum := ReadByte;
            Push(Frame^.Slots[SlotNum]);
          end;

        op_Set_Local:
          begin
            SlotNum := ReadByte;
            Frame^.Slots[SlotNum] := Peek(0);
          end;

        op_Get_Global:
          begin
            Name := ReadString;
            if not TableGet(@VM.Globals, Name, Value) then
              Exit(RuntimeError('Undefined variable "%s".', [Name^.Chars]));
            Push(Value);
          end;

        op_Define_Global:
          begin
            Name := ReadString;
            TableSet(@VM.Globals, Name, Peek(0));
            Pop;
          end;

        op_Set_Global:
          begin
            Name := ReadString;
            if TableSet(@VM.Globals, Name, Peek(0)) then
              begin
                TableDelete(@VM.Globals, Name);
                Exit(RuntimeError('Undefined variable "%s".', [Name^.Chars]));
              end;
          end;

        op_Get_UpValue:
          begin
            SlotNum := ReadByte;
            Push(Frame^.Closure^.UpValues[SlotNum]^.Location^);
          end;

        op_Set_UpValue:
          begin
            SlotNum := ReadByte;
            Frame^.Closure^.UpValues[SlotNum]^.Location^ := Peek(0);
          end;

        op_Get_Property:
          begin
            if not isInstance(Peek(0)) then
              begin
                RuntimeError('Only instances have properties.', []);
                Exit(irRuntimeError);
              end;

            Instance := asInstance(Peek(0));
            Name := ReadString;

            if TableGet(@Instance^.Fields, Name, Value) then
              begin
                Pop;  // instance;
                Push(Value);
              end
            else if not BindMethod(Instance^.Klass, Name) then
              Exit(irRuntimeError);
          end;

        op_Set_Property:
          begin
            if not isInstance(Peek(1)) then
              begin
                RuntimeError('Only instances have properties.', []);
                Exit(irRuntimeError);
              end;

            Instance := asInstance(Peek(1));
            TableSet(@Instance^.Fields, ReadString, Peek(0));

            Value := Pop;
            Pop;
            Push(Value);
          end;

        op_Get_Super:
          begin
            Name := ReadString;
            Super := asClass(Pop);
            if not BindMethod(Super, Name) then
              Exit(irRuntimeError);
          end;

        op_Equal: Push(BoolVal(ValuesEqual(Pop, Pop)));

        op_Add:
          if isNumber(Peek(0)) and isNumber(Peek(1)) then
            begin
              GetAB(a, b);
              Push(NumberVal(a+b));
            end
          else if (isString(Peek(0)) and isString(Peek(1))) or
             (isString(Peek(0)) and isNumber(Peek(1)))  or
             (isNumber(Peek(0)) and isString(Peek(1))) then
            Concatenate
          else
            Exit(RuntimeError('Operands must be two numbers or two strings.', []));
        op_Subtract:
          if isNumber(Peek(0)) and isNumber(Peek(1)) then
            begin
              GetAB(a, b);
              Push(NumberVal(a-b));
            end
          else
            Exit(RuntimeError('Operands must be two numbers.', []));
        op_Multiply:
          if isNumber(Peek(0)) and isNumber(Peek(1)) then
            begin
              GetAB(a, b);
              Push(NumberVal(a*b));
            end
          else
            Exit(RuntimeError('Operands must be two numbers.', []));
        op_Divide:
          if isNumber(Peek(0)) and isNumber(Peek(1)) then
            begin
              GetAB(a, b);
              Push(NumberVal(a/b));
            end
          else
            Exit(RuntimeError('Operands must be two numbers.', []));
        op_Greater:
          if isNumber(Peek(0)) and isNumber(Peek(1)) then
            begin
              GetAB(a, b);
              Push(BoolVal(a>b));
            end
          else
            Exit(RuntimeError('Operands must be two numbers.', []));
        op_Less:
          if isNumber(Peek(0)) and isNumber(Peek(1)) then
            begin
              GetAB(a, b);
              Push(BoolVal(a<b));
            end
          else
            Exit(RuntimeError('Operands must be two numbers.', []));

        op_Not: Push(BoolVal(isFalsey(Pop)));
        op_Negate: //Push(-Pop);
          begin
            if not isNumber(Peek(0)) then
              begin
                RuntimeError('Operand must be a number.', []);
                Exit(irRuntimeError);
              end;
            Push(NumberVal(-asNumber(Pop())));
          end;

        op_Print:
          begin
            PrintValue(Pop);
            WriteLn;
          end;

        op_Jump:
          begin
            OffSet := ReadShort;
            Inc(Frame^.IP, OffSet);
          end;

        op_Jump_If_False:
          begin
            OffSet := ReadShort;
            if isFalsey(Peek(0)) then Inc(Frame^.IP, OffSet);
          end;

        op_Loop:
          begin
            OffSet := ReadShort;
            Dec(Frame^.IP, OffSet);
          end;

        op_Call:
          begin
            ArgCount := ReadByte;
            if not CallValue(Peek(ArgCount), ArgCount) then
              Exit(irRuntimeError);
            Frame := @VM.Frames[VM.FrameCount-1];
          end;

        op_Invoke:
          begin
            Method := ReadString;
            ArgCount := ReadByte;
            if not Invoke(Method, ArgCount) then
              Exit(irRuntimeError);
            Frame := @VM.Frames[VM.FrameCount - 1];
          end;

        op_Super_Invoke:
          begin
            Method := ReadString;
            ArgCount := ReadByte;
            Super := asClass(Pop);
            if not InvokeFromClass(Super, Method, ArgCount) then
              Exit(irRuntimeError);
            Frame := @VM.Frames[VM.FrameCount - 1];
          end;

        op_Closure:
          begin
            Func := asFunc(ReadConstant);
            Closure := NewClosure(Func);
            Push(ObjVal(Closure));
            for i := 0 to Closure^.UpValueCount-1 do
              begin
                isLocal := ReadByte;
                Index := ReadByte;
                if isLocal = 1 then
                  Closure^.UpValues[i] := CaptureUpValue(Frame^.Slots + Index)
                else
                  Closure^.UpValues[i] := Frame^.Closure^.UpValues[Index];
              end;
          end;

        op_Close_UpValue:
          begin
            CloseUpValues(VM.StackTop-1);
            Pop;
          end;

        op_Return:
          begin
            FuncResult := Pop;

            CloseUpValues(Frame^.Slots);

            Dec(VM.FrameCount);
            if VM.FrameCount = 0 then
              begin
                Pop;
                Exit(irInterpretOK);
              end;

            VM.StackTop := Frame^.Slots;
            Push(FuncResult);

            Frame := @VM.Frames[VM.FrameCount-1];
          end;

        op_Class: Push(ObjVal(NewClass(ReadString)));

        op_Inherit:
          begin
            SuperClass := Peek(1);
            if not isClass(SuperClass) then
              begin
                RuntimeError('Superclass must be a class.', []);
                Exit(irRuntimeError);
              end;

            SubClass := asClass(Peek(0));
            TableAddAll(@asClass(SuperClass)^.Methods, @SubClass^.Methods);
            Pop; // Subclass
          end;

        op_Method: DefineMethod(ReadString);

        otherwise
          WriteLn(Format('VM: Unknown opcode %d', [Instruction]));
          Exit(irRuntimeError);
      end;
    end;
end;

function Interpret(const Source: String): TInterpretResult;
var
  Func: PObjFunc;
  Closure: PObjClosure;
begin
  Func := Compile(Source);
  if Func = Nil then Exit(irCompileError);

  Push(ObjVal(Func));
  Closure := NewClosure(Func);
  Pop;
  Push(ObjVal(Closure));
  CallValue(ObjVal(Closure), 0);

  Result := Run;
end;

end.

