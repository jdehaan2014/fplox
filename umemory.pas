unit uMemory;

{$mode delphi}{$H+}

interface

uses
  SysUtils, uValue, uCommon;


function Grow_Capacity(OldCapacity: LongInt): LongInt;
function Reallocate(Previous: Pointer; OldSize, NewSize: size_t): Pointer;
procedure MarkObject(Obj: Pointer);
procedure MarkValue(Value: TValue);
procedure CollectGarbage;
procedure FreeObjects;

// not for pointer types, use reallocate in that case directly
// function Grow_Array<T>(Previous: Pointer; const OldSize, NewSize: size_t): Pointer;
// procedure Free_Array<T>(Ptr: Pointer; OldCount: LongInt);

implementation
uses uVM, uChunk, uObject, uCompiler, uTable;

const
  GC_HEAP_GROW_FACTOR = 2;

function Grow_Capacity(OldCapacity: LongInt): LongInt;
begin
  if OldCapacity < 8 then
    Result := 8
  else
    Result := OldCapacity * 2;
end;

// function Grow_Array<T>(Previous: Pointer; const OldSize, NewSize: size_t): Pointer;
//begin
//  Result := Reallocate(Previous, SizeOf(T)*OldSize, SizeOf(T)*NewSize);
//end;

function Reallocate(Previous: Pointer; OldSize, NewSize: size_t): Pointer;
begin
  Inc(VM.BytesAllocated, NewSize - OldSize);

  if NewSize > OldSize then
    begin
      if DebugStressGC then
        CollectGarbage;

      if VM.BytesAllocated > VM.NextGC then
        CollectGarbage;
    end;

  if NewSize = 0 then
    begin
      FreeMem(Previous);
      Exit(Nil);
    end;

  Result := ReAllocMem(Previous, NewSize); // FPC heapmanager auto keeps track of old size.
end;

procedure Free<T>(ptr: Pointer);
begin
  Reallocate(ptr, SizeOf(T), 0);
end;

procedure FreeObject(Obj: PObj);
var
  ObjString: PObjString;
  Func: PObjFunc;
  Closure: PObjClosure;
  Instance: PObjInstance;
  Klass: PObjClass;
begin
  if DebugLogGC then
    WriteLnFmt('%p free type %s', [Pointer(Obj), Obj^.Typ.AsString]);

  case Obj^.Typ of
    obj_Bound_Method: Free<TObjBoundMethod>(Obj);
    obj_Class:
      begin
        Klass := PObjClass(Obj);
        FreeTable(@Klass^.Methods);
        Free<TObjClass>(Obj);
      end;
    obj_Closure:
      begin
        Closure := PObjClosure(Obj);
        Reallocate(Closure^.UpValues, SizeOf(PObjUpValue)*Closure^.UpValueCount, 0);
        Free<TObjClosure>(Obj);
      end;
    obj_Func:
      begin
        Func := PObjFunc(Obj);
        FreeChunk(@Func^.Chunk);
        Free<TObjFunc>(Obj);
      end;
    obj_Instance:
      begin
        Instance := PObjInstance(Obj);
        FreeTable(@Instance^.Fields);
        Free<TObjInstance>(Obj);
      end;
    obj_Native: Free<TObjNative>(Obj);
    obj_String:
      begin
        ObjString := PObjString(Obj);
        StrDispose(ObjString^.Chars);
        Free<TObjString>(Obj);
      end;
    obj_UpValue:
      begin
        Free<TObjUpValue>(Obj);
      end;
  end;
end;

procedure MarkObject(Obj: Pointer);
var
  TheObj: PObj;
begin
  if Obj = Nil then Exit;
  TheObj := PObj(Obj);
  if TheObj^.isMarked then Exit;

  if DebugLogGC then
    begin
      WriteFmt('%p mark ', [Pointer(TheObj)]);
      PrintValue(ObjVal(TheObj));
      WriteLn;
    end;

  TheObj^.isMarked := True;

  if VM.GrayCapacity < VM.GrayCount + 1 then
    begin
      VM.GrayCapacity := Grow_Capacity(VM.GrayCapacity);
      VM.GrayStack := PPObj(ReAllocMem(VM.GrayStack, SizeOf(PObj)*VM.GrayCapacity));
    end;

  VM.GrayStack[VM.GrayCount] := TheObj;
  Inc(VM.GrayCount);
end;

procedure MarkValue(Value: TValue);
begin
  if not isObj(Value) then Exit;
  MarkObject(asObj(Value));
end;

procedure MarkArray(var ValueArray: TValueArray);
var
  i: Integer;
begin
  for i := 0 to ValueArray.Count-1 do
    MarkValue(ValueArray.Values[i]);
end;

procedure BlackenObject(Obj: PObj);
var
  Func: PObjFunc;
  Closure: PObjClosure;
  i: Integer;
  Klass: PObjClass;
  Instance: PObjInstance;
  Bound: PObjBoundMethod;
begin
  if DebugLogGC then
    begin
      WriteFmt('%p blacken ', [Obj]);
      PrintValue(ObjVal(Obj));
      WriteLn;
    end;

  case Obj^.Typ of
    obj_Bound_Method:
      begin
        Bound := PObjBoundMethod(Obj);
        MarkValue(Bound^.Receiver);
        MarkObject(PObj(Bound^.Method));
      end;
    obj_Class:
      begin
        Klass := PObjClass(Obj);
        MarkObject(PObj(Klass^.Name));
        MarkTable(@Klass^.Methods);
      end;
    obj_Closure:
      begin
        Closure := PObjClosure(Obj);
        MarkObject(PObj(Closure^.Func));
        for i := 0 to Closure^.UpValueCount-1 do
          MarkObject(PObj(Closure^.UpValues[i]));
      end;
    obj_Func:
      begin
        Func := PObjFunc(Obj);
        MarkObject(PObj(Func^.Name));
        MarkArray(Func^.Chunk.Constants);
      end;
    obj_Instance:
      begin
        Instance := PObjInstance(Obj);
        MarkObject(PObj(Instance^.Klass));
        MarkTable(@Instance^.Fields);
      end;
    obj_UpValue: MarkValue(PObjUpValue(Obj)^.Closed);
    obj_Native,
    obj_String:;
  end;
end;

procedure MarkRoots;
var
  Slot: PValue;
  i: Integer;
  UpValue: PObjUpValue;
begin
  Slot := @VM.Stack[0];
  while Slot < VM.StackTop do
    begin
      MarkValue(Slot^);
      Inc(Slot);
    end;

  for i := 0 to VM.FrameCount-1 do
    MarkObject(PObj(VM.Frames[i].Closure));

  UpValue := VM.OpenUpValues;
  while UpValue <> Nil do
    begin
      MarkObject(PObj(UpValue));
      UpValue := UpValue^.Next;
    end;

  MarkTable(@VM.Globals);
  MarkCompilerRoots;
  MarkObject(PObj(VM.InitString));
end;

procedure TraceReferences;
var
  Obj: PObj;
begin
  while VM.GrayCount > 0 do
    begin
      Dec(VM.GrayCount);
      Obj := VM.GrayStack[VM.GrayCount];
      BlackenObject(Obj);
    end;
end;

procedure Sweep;
var
  Obj, Unreached: PObj;
  Previous: PObj = Nil;
begin
  Obj := VM.Objects;
  while Obj <> Nil do
    begin
      if Obj^.isMarked then
        begin
          Obj^.isMarked := False;
          Previous := Obj;
          Obj := Obj^.Next;
        end
      else
        begin
          Unreached := Obj;

          Obj := Obj^.Next;
          if Previous <> Nil then
            Previous^.Next := Obj
          else
            VM.Objects := Obj;

          FreeObject(Unreached);
        end;
    end;
end;

procedure CollectGarbage;
var
  Before: size_t;
begin
  if DebugLogGC then
    begin
      WriteLn('-- gc begin');
      Before := VM.BytesAllocated;
    end;

  MarkRoots;
  TraceReferences;
  TableRemoveWhite(@VM.Strings);
  Sweep;

  VM.NextGC := VM.BytesAllocated * GC_HEAP_GROW_FACTOR;
  if DebugLogGC then
    begin
      WriteLn('-- gc end');
      WriteLnFmt('   collected %ld bytes (from %ld to %ld) next at %ld',
        [Before - VM.BytesAllocated, Before, VM.BytesAllocated, VM.NextGC]);
    end;
end;

procedure FreeObjects;
var
  Obj, Next: PObj;
begin
  Obj := VM.Objects;
  while Obj <> Nil do
    begin
      Next := Obj^.Next;
      FreeObject(Obj);
      Obj := Next;
    end;

  FreeMem(VM.GrayStack);
end;

// procedure Free_Array<T>(Ptr: Pointer; OldCount: LongInt);
//begin
//  Reallocate(Ptr, SizeOf(T)*OldCount, 0);
//end;

end.

