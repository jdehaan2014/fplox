unit uValue;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}
{$DEFINE NAN_BOXING}

interface

uses
  SysUtils;

{$ifdef NAN_BOXING}
const
  SIGN_BIT: UInt64 = UInt64($8000000000000000);
  QNAN:     UInt64 = UInt64($7FFC000000000000);

  TagNil = 1;
  TagFalse = 2;
  TagTrue = 3;
{$endif}

type

  TValueType = (valBool, valNil, valNumber, valObj);


  PValue = ^TValue;

  {$ifdef NAN_BOXING}

  TUnion = packed record
    case Byte of
      0: (Bits: QWord);
      1: (Number: Double);
  end;

  TValue = type UInt64; // QWord

  {$else}

  TValue = record
    case Typ: TValueType of
      valBool:   (Bool: Boolean);
      valNumber: (Number: Double);
      valObj:    (Obj: Pointer);
  end;

  {$endif}

  TValueHelper = type helper for TValue
    function toString: String;
  end;

  PValueArray = ^TValueArray;
  TValueArray = record
    Values: PValue;
    Count,
    Capacity: Longint;
  end;

  // functions to check if a value is a certain type
  function isBool(Value: TValue): Boolean;
  function isNil(Value: TValue): Boolean;
  function isNumber(Value: TValue): Boolean;
  function isObj(Value: TValue): Boolean;

  // Conversion from TValue to Pascal value
  function asBool(Value: TValue): Boolean;
  function asNumber(Value: TValue): Double;
  function asObj(Value: TValue): Pointer;

  // Conversion functions Pascal value to TValue
  function BoolVal(const Value: Boolean): TValue;
  function NilVal: TValue;
  function NumberVal(const Value: Double): TValue;
  function ObjVal(Obj: Pointer): TValue;

  function ValuesEqual(a, b: TValue): Boolean;

procedure InitValueArray(Arr: PValueArray);
procedure WriteValueArray(Arr: PValueArray; const AValue: TValue);
procedure FreeValueArray(Arr: PValueArray);
procedure PrintValue(Value: TValue);

implementation
uses uCommon, uObject, uMemory;

{$ifdef NAN_BOXING}

function FalseVal: TValue;
begin
  Result := UInt64(QNAN or TagFalse);
end;

function TrueVal: TValue;
begin
  Result := UInt64(QNAN or TagTrue);
end;


// functions to check if a value is a certain type

function isBool(Value: TValue): Boolean;
begin
  Result := (Value and FalseVal) = FalseVal;
end;

function isNil(Value: TValue): Boolean;
begin
  Result := Value = NilVal;
end;

function isNumber(Value: TValue): Boolean;
begin
  Result := (Value and QNAN) <> QNAN;
end;

function isObj(Value: TValue): Boolean;
begin
  Result := (Value and (QNAN or SIGN_BIT)) = (QNAN or SIGN_BIT);
end;


// Conversion functions: promotion of Pascal value to TValue

function BoolVal(const Value: Boolean): TValue;
begin
  Result := specialize IfThen<TValue>(Value, TrueVal, FalseVal);
end;

function NilVal: TValue;
begin
  Result := UInt64(QNAN or TagNil);
end;

function NumberVal(const Value: Double): TValue;
var
  Data: TUnion;
begin
  Data.Number := Value;
  Result := Data.Bits;
end;

function ObjVal(Obj: Pointer): TValue;
begin
  Result := TValue(SIGN_BIT or QNAN or UInt64(PUInt64(Obj))); // PUInt64(Obj)
end;

// Conversion functions: TValue to Pascal value

function asBool(Value: TValue): Boolean;
begin
  Result := Value = TrueVal;
end;

function asNumber(Value: TValue): Double;
var
  Data: TUnion;
begin
  Data.Bits := Value;
  Result := Data.Number;
end;

function asObj(Value: TValue): Pointer;
begin
  Result := PUInt64(Value and not(SIGN_BIT or QNAN));
end;

{$else}

// functions to check if a value is a certain type

function isBool(Value: TValue): Boolean;
begin
  Result := Value.Typ = valBool;
end;

function isNil(Value: TValue): Boolean;
begin
  Result := Value.Typ = valNil;
end;

function isNumber(Value: TValue): Boolean;
begin
  Result := Value.Typ = valNumber;
end;

function isObj(Value: TValue): Boolean;
begin
  Result := Value.Typ = valObj;
end;

// Conversion functions: TValue to Pascal value

function asBool(Value: TValue): Boolean;
begin
  Result := Value.Bool;
end;

function asNumber(Value: TValue): Double;
begin
  Result := Value.Number;
end;

function asObj(Value: TValue): Pointer;
begin
  Result := Value.Obj;
end;

// Conversion functions: promotion of Pascal value to TValue

function BoolVal(const Value: Boolean): TValue;
begin
  Result.Typ := valBool;
  Result.Bool := Value;
end;

function NilVal: TValue;
begin
  Result.Typ := valNil;
  Result.Number := 0;
end;

function NumberVal(const Value: Double): TValue;
begin
  Result.Typ := valNumber;
  Result.Number := Value;
end;

function ObjVal(Obj: Pointer): TValue;
begin
  Result.Typ := valObj;
  Result.Obj := Obj;
end;

{$endif}

function ValuesEqual(a, b: TValue): Boolean;
begin
  {$ifdef NAN_BOXING}
  Result := a = b;
  {$else}
  if a.Typ <> b.Typ then
    Exit(False);
  case a.Typ of
    valBool: Result := asBool(a) = asBool(b);
    valNil: Result := True;
    valNumber: Result := asNumber(a) = asNumber(b);
    valObj: Result := asObj(a) = asObj(b);
  end;
  {$endif}
end;

procedure InitValueArray(Arr: PValueArray);
begin
  Arr^.Count := 0;
  Arr^.Capacity := 0;
  Arr^.Values := Nil;
end;

procedure WriteValueArray(Arr: PValueArray; const AValue: TValue);
var
  OldCapacity: LongInt;
begin
  if Arr^.Capacity < Arr^.Count + 1 then
    begin
      OldCapacity := Arr^.Capacity;
      Arr^.Capacity := Grow_Capacity(OldCapacity);
      Arr^.Values := PValue(Reallocate(
        Arr^.Values, SizeOf(TValue)*OldCapacity, SizeOf(TValue)*Arr^.Capacity));
    end;
  Arr^.Values[Arr^.Count] := AValue;
  Inc(Arr^.Count);
end;

procedure FreeValueArray(Arr: PValueArray);
begin
  Reallocate(Arr^.Values, SizeOf(TValue)*Arr^.Capacity, 0); // Free_Array
  InitValueArray(Arr);
end;

{$ifdef NAN_BOXING}

procedure PrintValue(Value: TValue);
begin
  if isBool(Value) then
    Write(specialize IfThen<String>(asBool(Value), 'true', 'false'))
  else if isNil(Value) then
    Write('nil')
  else if isNumber(Value) then
    WriteFmt('%g', [asNumber(Value)])
  else if isObj(Value) then
    PrintObject(value);
end;

{ TValueHelper }

function TValueHelper.toString: String;
begin
  if isBool(Self) then
    WriteStr(Result, asBool(Self))
  else if isNil(Self) then
    Result := 'nil'
  else if isNumber(Self) then
    Result := Format('%g', [asNumber(Self)])
  else if isObj(Self) then
    WriteStrObj(Result, Self);
end;

{$else}

procedure PrintValue(Value: TValue);
begin
  case Value.Typ of
    valBool: Write(specialize IfThen<String>(asBool(Value), 'true', 'false'));
    valNil: Write('nil');
    valNumber: WriteFmt('%g', [asNumber(Value)]);
    valObj: PrintObject(value);
  end;
end;

{ TValueHelper }

function TValueHelper.toString: String;
begin
  case Value.Typ of
    valBool: WiteStr(Result, asBool(Value));
    valNil: Result := 'nil';
    valNumber: Result := Format('%g', [asNumber(Value)]);
    valObj: Result := WriteStrObj(Value);
  end;
end;

{$endif}

end.

