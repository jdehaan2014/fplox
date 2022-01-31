unit uObject;

{$mode delphi}{$H+}
{$modeswitch typehelpers}
{$POINTERMATH ON}

interface

uses
  SysUtils, uValue, uChunk, uCommon;

type

  PObjString = ^TObjString;

//|============================================================================|
//|Due to unit circular reference, the Table types are moveed here from uTable.|
//|============================================================================|

  TEntry = record
    Key: PObjString;
    Value: TValue;
  end;

  PEntry = ^TEntry;

  TTable = record
    Count,
    Capacity: LongInt;
    Entries: PEntry;
  end;

  PTable = ^TTable;

//|============================================================================|
//|End of Table types                                                          |
//|============================================================================|

  TObjTyp = (obj_Bound_Method, obj_Class, obj_Closure, obj_Func, obj_Instance,
             obj_Native, obj_String, obj_UpValue);

  { TObjTypHelper }

  TObjTypHelper = type helper for TObjTyp
    function AsString: String;
  end;

  PObj = ^TObj;

  TObj = record
    Typ: TObjTyp;
    isMarked: Boolean;
    Next: PObj;
  end;

  PPObj = ^PObj;

  TObjString = record
    Obj: TObj;
    Length: LongInt;
    Chars: PChar;
    Hash: LongWord;   // uint32_t  0 .. 4294967295 -> 4 bytes
  end;

  PObjFunc = ^TObjFunc;
  TObjFunc = record
    Obj: TObj;
    Arity: Integer;
    UpValueCount: Integer;
    Chunk: TChunk;
    Name: PObjString;
  end;

  TNativeFn = function(const ArgCount: Integer; Args: PValue): Boolean;

  PObjNative = ^TObjNative;
  TObjNative = record
    Obj: TObj;
    Func: TNativeFn;
  end;

  PObjUpValue = ^TObjUpValue;
  TObjUpValue = record
    Obj: TObj;
    Location: PValue;
    Closed: TValue;
    Next: PObjUpValue;
  end;

  PPObjUpValue = ^PObjUpValue;

  PObjClosure = ^TObjClosure;
  TObjClosure = record
    Obj: TObj;
    Func: PObjFunc;
    UpValues: PPObjUpValue;
    UpValueCount: Integer;
  end;

  PObjClass = ^TObjClass;
  TObjClass = record
    Obj: TObj;
    Name: PObjString;
    Methods: TTable;
  end;

  PObjInstance = ^TObjInstance;
  TObjInstance = record
    Obj: TObj;
    Klass: PObjClass;
    Fields: TTable;
  end;

  PObjBoundMethod = ^TObjBoundMethod;
  TObjBoundMethod = record
    Obj: TObj;
    Receiver: TValue;
    Method: PObjClosure;
  end;

  function isObjType(Value: TValue; ObjTyp: TObjTyp): Boolean; inline;

  function NewFunc: PObjFunc;
  function isFunc(Value: TValue): Boolean;
  function asFunc(Value: TValue): PObjFunc;

  function NewNative(Func: TNativeFn): PObjNative;
  function isNative(Value: TValue): Boolean;
  function asNative(Value: TValue): TNativeFn;

  function NewClosure(Func: PObjFunc): PObjClosure;
  function isClosure(Value: TValue): Boolean;
  function asClosure(Value: TValue): PObjClosure;

  function NewUpValue(Slot: PValue): PObjUpValue;

  function NewClass(Name: PObjString): PObjClass;
  function isClass(Value: TValue): Boolean;
  function asClass(Value: TValue): PObjClass;

  function NewInstance(Klass: PObjClass): PObjInstance;
  function isInstance(Value: TValue): Boolean;
  function asInstance(Value: TValue): PObjInstance;

  function NewBoundMethod(Receiver: TValue; Method: PObjClosure): PObjBoundMethod;
  function isBoundMethod(Value: TValue): Boolean;
  function asBoundMethod(Value: TValue): PObjBoundMethod;

  function isString(Value: TValue): Boolean;
  function asString(Value: TValue): PObjString;
  function asCString(Value: TValue): PChar;
  function CopyString(Chars: PChar; const Length: LongInt): PObjString;
  function TakeString(Chars: PChar; const Length: LongInt): PObjString;
  function AllocateString(Chars: PChar; const Length: LongInt; const Hash: LongWord): PObjString;

  function Obj_Typ(Value: TValue): TObjTyp;
  function Allocate_Obj<T>(const Typ: TObjTyp): Pointer;
  function AllocateObject(const Size: size_t; const Typ: TObjTyp): PObj;
  procedure PrintObject(Value: TValue);
  procedure WriteStrObj(var Result: String; Value: TValue);

implementation
uses uVM, uTable, uMemory;

function isObjType(Value: TValue; ObjTyp: TObjTyp): Boolean; inline;
begin
  Result := isObj(Value) and (PObj(asObj(Value))^.Typ = ObjTyp);
end;

function NewFunc: PObjFunc;
var
  Func: PObjFunc;
begin
  Func := PObjFunc(Allocate_Obj<TObjFunc>(obj_Func));
  Func^.Arity := 0;
  Func^.UpValueCount := 0;
  Func^.Name := Nil;
  InitChunk(@Func^.Chunk);
  Result := Func;
end;

function isFunc(Value: TValue): Boolean;
begin
  Result := isObjType(Value, obj_Func);
end;

function asFunc(Value: TValue): PObjFunc;
begin
  Result := PObjFunc(asObj(Value));
end;

function NewNative(Func: TNativeFn): PObjNative;
begin
  Result := PObjNative(Allocate_Obj<TObjNative>(obj_Native));
  Result^.Func := Func;
end;

function isNative(Value: TValue): Boolean;
begin
  Result := isObjType(Value, obj_Native);
end;

function asNative(Value: TValue): TNativeFn;
begin
  Result := PObjNative(asObj(Value))^.Func;
end;

function NewClosure(Func: PObjFunc): PObjClosure;
var
  Upvalues: PPObjUpValue;
  i: Integer;
begin
  Upvalues := PPObjUpValue(Reallocate(Nil, 0, sizeof(PObjUpValue) * Func^.UpValueCount));
  for i := 0 to Func^.UpValueCount-1 do
    UpValues[i] := Nil;

  Result := PObjClosure(Allocate_Obj<TObjClosure>(obj_Closure));
  Result^.Func := Func;
  Result^.UpValues := UpValues;
  Result^.UpValueCount := Func^.UpValueCount;
end;

function isClosure(Value: TValue): Boolean;
begin
  Result := isObjType(Value, obj_Closure);
end;

function asClosure(Value: TValue): PObjClosure;
begin
  Result := PObjClosure(asObj(Value));
end;

function NewUpValue(Slot: PValue): PObjUpValue;
begin
  Result := PObjUpValue(Allocate_Obj<TObjUpValue>(obj_UpValue));
  Result^.Closed := NilVal;
  Result^.Location := Slot;
  Result^.Next := Nil;
end;

function NewClass(Name: PObjString): PObjClass;
var
  Klass: PObjClass;
begin
  Klass := PObjClass(Allocate_Obj<TObjClass>(obj_Class));
  Klass^.Name := Name;
  InitTable(@Klass^.Methods);
  Result := Klass;
end;

function isClass(Value: TValue): Boolean;
begin
  Result := isObjType(Value, obj_Class);
end;

function asClass(Value: TValue): PObjClass;
begin
  Result := PObjClass(asObj(Value));
end;

function NewInstance(Klass: PObjClass): PObjInstance;
var
  Instance: PObjInstance;
begin
  Instance := PObjInstance(Allocate_Obj<TObjInstance>(obj_Instance));
  Instance^.Klass := Klass;
  InitTable(@Instance^.Fields);
  Result := Instance
end;

function isInstance(Value: TValue): Boolean;
begin
  Result := isObjType(Value, obj_Instance);
end;

function asInstance(Value: TValue): PObjInstance;
begin
  Result := PObjInstance(asObj(Value));
end;

function NewBoundMethod(Receiver: TValue; Method: PObjClosure): PObjBoundMethod;
var
  Bound: PObjBoundMethod;
begin
  Bound := PObjBoundMethod(Allocate_Obj<TObjBoundMethod>(obj_Bound_Method));
  Bound^.Receiver := Receiver;
  Bound^.Method := Method;
  Result := Bound;
end;

function isBoundMethod(Value: TValue): Boolean;
begin
  Result := isObjType(Value, obj_Bound_Method);
end;

function asBoundMethod(Value: TValue): PObjBoundMethod;
begin
  Result := PObjBoundMethod(asObj(Value));
end;

function isString(Value: TValue): Boolean;
begin
  Result := isObjType(Value, obj_String);
end;

function HashString(const Key: PChar; const Length: Integer): LongWord;
var
  i: Integer = 0;
  Hash: LongWord = 2166136261;
begin
  while i < Length do
    begin
      Hash := (Hash xor Ord(Key[i])) * 16777619;
      Inc(i);
    end;

  Result := Hash;
end;

function asString(Value: TValue): PObjString;
var
  NumberAsStr: String;
  Hash: LongWord;
begin
  if isNumber(Value)  then // added for concat of string and number
    begin
      NumberAsStr := Value.ToString;
      Hash := HashString(PChar(NumberAsStr), NumberAsStr.Length);
      Result := AllocateString(PChar(NumberAsStr), NumberAsStr.Length, Hash);
    end
  else
    Result := PObjString(asObj(Value));
end;

//function asString(Value: TValue): PObjString;
//begin
//  Result := PObjString(asObj(Value));
//end;

function asCString(Value: TValue): PChar;
begin
  Result := PObjString(asObj(Value))^.Chars;
end;

function Obj_Typ(Value: TValue): TObjTyp;
begin
  Result := PObj(asObj(Value))^.Typ;
end;

function AllocateString(Chars: PChar; const Length: LongInt;
  const Hash: LongWord): PObjString;
begin
  Result := PObjString(Allocate_Obj<TObjString>(obj_String));
  Result^.Obj.Typ := obj_String;
  Result^.Length := Length;
  Result^.Chars := Chars;
  Result^.Hash := Hash;
  Push(ObjVal(Result));
  TableSet(@VM.Strings, Result, NilVal);
  Pop;
end;


function CopyString(Chars: PChar; const Length: LongInt): PObjString;
var
  HeapChars: PChar;
  Hash: LongWord;
  Interned: PObjString;
begin
  Hash := HashString(Chars, Length);
  Interned := TableFindString(@VM.Strings, Chars, Length, Hash);
  if Interned <> Nil then
    Exit(Interned);

  HeapChars := StrAlloc(Length+1);
  HeapChars := StrLCopy(HeapChars, Chars, Length);

  Result := AllocateString(HeapChars, Length, Hash);
end;

function TakeString(Chars: PChar; const Length: LongInt): PObjString;
var
  Hash: LongWord;
  Interned: PObjString;
begin
  Hash := HashString(Chars, Length);
  Interned := TableFindString(@VM.Strings, Chars, Length, Hash);
  if Interned <> Nil then
    begin
      StrDispose(Chars);
      Exit(Interned);
    end;
  Result := AllocateString(Chars, Length, Hash);
end;

function FuncToStr(Func: PObjFunc): String;
begin
  if Func^.Name = Nil then
    Result := '<script>'
  else
    Result := Format('<fn %s>', [Func^.Name^.Chars]);
end;


procedure PrintFunc(Func: PObjFunc);
begin
  Write(FuncToStr(Func));
end;

function Allocate_Obj<T>(const Typ: TObjTyp): Pointer;
begin
  Result := PObj(AllocateObject(SizeOf(T), Typ));
end;

function AllocateObject(const Size: size_t; const Typ: TObjTyp): PObj;
begin
  Result := PObj(Reallocate(Nil, 0, Size));
  Result^.Typ := Typ;
  Result^.isMarked := False;
  Result^.Next := VM.Objects;
  VM.Objects := Result;

  if DebugLogGC then
    WriteLnFmt('%p allocate %d for %s', [Pointer(Result), Size, Typ.AsString]);
end;


procedure PrintObject(Value: TValue);
begin
  case Obj_Typ(Value) of
    obj_Bound_Method: PrintFunc(asBoundMethod(Value)^.Method^.Func);
    obj_Class: WriteFmt('%s', [asClass(Value)^.Name^.Chars]);
    obj_Closure: PrintFunc(asClosure(Value)^.Func);
    obj_Func: PrintFunc(asFunc(Value));
    obj_Instance: WriteFmt('%s instance', [asInstance(Value)^.Klass^.Name^.Chars]);
    obj_Native: Write('<native fn>');
    obj_String: WriteFmt('%s', [asCString(Value)]);
    obj_UpValue: Write('upvalue');
  end;
end;

procedure WriteStrObj(var Result: String; Value: TValue);
begin
  case Obj_Typ(Value) of
    obj_Bound_Method: Result := FuncToStr(asBoundMethod(Value)^.Method^.Func);
    obj_Class: Result := Format('%s', [asClass(Value)^.Name^.Chars]);
    obj_Closure: Result := FuncToStr(asClosure(Value)^.Func);
    obj_Func: Result := FuncToStr(asFunc(Value));
    obj_Instance: Result := Format('%s instance', [asInstance(Value)^.Klass^.Name^.Chars]);
    obj_Native: Result := '<native fn>';
    obj_String: Result := Format('%s', [asCString(Value)]);
    obj_UpValue: Result := 'upvalue';
  end;
end;

{ TObjTypHelper }

function TObjTypHelper.AsString: String;
begin
  case Self of
    obj_Class: Result := 'Class';
    obj_Closure: Result := 'Closure';
    obj_Func: Result := 'Function';
    obj_Instance: Result := 'Instance';
    obj_Native: Result := 'Native';
    obj_String: Result := 'String';
    obj_UpValue: Result := 'UpValue';
  end;
end;




end.

