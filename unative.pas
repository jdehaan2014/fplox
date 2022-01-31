unit uNative;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uObject, uValue;

function PiNative(const ArgCount: Integer; Args: PValue): Boolean;
function SqrtNative(const ArgCount: Integer; Args: PValue): Boolean;
function SqrNative(const ArgCount: Integer; Args: PValue): Boolean;
function TruncNative(const ArgCount: Integer; Args: PValue): Boolean;
function RoundNative(const ArgCount: Integer; Args: PValue): Boolean;
function AbsNative(const ArgCount: Integer; Args: PValue): Boolean;
function SinNative(const ArgCount: Integer; Args: PValue): Boolean;
function CosNative(const ArgCount: Integer; Args: PValue): Boolean;
function ExpNative(const ArgCount: Integer; Args: PValue): Boolean;
function LnNative(const ArgCount: Integer; Args: PValue): Boolean;
function FracNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArcTanNative(const ArgCount: Integer; Args: PValue): Boolean;
function MilliSecondsNative(const ArgCount: Integer; Args: PValue): Boolean;
function DateNative(const ArgCount: Integer; Args: PValue): Boolean;
function TimeNative(const ArgCount: Integer; Args: PValue): Boolean;
function NowNative(const ArgCount: Integer; Args: PValue): Boolean;
function TodayNative(const ArgCount: Integer; Args: PValue): Boolean;
function RandomNative(const ArgCount: Integer; Args: PValue): Boolean;
function RandomizeNative(const ArgCount: Integer; Args: PValue): Boolean;
function LengthNative(const ArgCount: Integer; Args: PValue): Boolean;
function FloorNative(const ArgCount: Integer; Args: PValue): Boolean;
function CeilNative(const ArgCount: Integer; Args: PValue): Boolean;
function ReadLnNative(const ArgCount: Integer; Args: PValue): Boolean;
function ToNumNative(const ArgCount: Integer; Args: PValue): Boolean;
function ToStrNative(const ArgCount: Integer; Args: PValue): Boolean;

function HasFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
function GetFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
function DelFieldNative(const ArgCount: Integer; Args: PValue): Boolean;

function ErrorNative(const ArgCount: Integer; Args: PValue): Boolean;

implementation
uses math, uTable;

function ReturnError(const Msg: PChar): TValue;
begin
  Result := ObjVal(PObj(CopyString(Msg, StrLen(Msg))));
end;

function GetString(const Msg: PChar): TValue;
begin
  Result := ObjVal(PObj(CopyString(Msg, StrLen(Msg)+1)));
end;

function PiNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := NumberVal(Pi);
  Result := True;
end;

function SqrtNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Sqrt(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function SqrNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Sqr(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function TruncNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Trunc(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function RoundNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Round(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function AbsNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Abs(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function SinNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Sin(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function CosNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Cos(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function ExpNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Exp(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function LnNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Ln(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function FracNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Frac(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function ArcTanNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(ArcTan(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function MilliSecondsNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  TS: TTimeStamp;
begin
  TS := DateTimeToTimeStamp(Now);
  Args[-1] := NumberVal(Double(TS.Time));
  Result := True;
end;

function DateNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := GetString(PChar(DateToStr(Date)));
  Result := True;
end;

function TimeNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := GetString(PChar(TimeToStr(Time)));
  Result := True;
end;

function NowNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  ST: TSystemTime;
  S: String;
begin
  DateTimeToSystemTime(Now, ST);
  with ST do
    WriteStr(S, Hour:2, ':', Minute:2, ':', Second:2, '.', MilliSecond);
  Args[-1] := GetString(PChar(S));
  Result := True;
end;

function TodayNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  S: String;
begin
  S := FormatSettings.LongDayNames[DayOfWeek(Date)];
  Args[-1] := GetString(PChar(S));
  Result := True;
end;

function RandomNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := NumberVal(Random);
  Result := True;
end;

function RandomizeNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Randomize;
  Args[-1] := NilVal;
  Result := True;
end;

function LengthNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isString(Args[0]) then
    begin
      Args[-1] := NumberVal(StrLen(asCString(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a string.');
  Result := False;
end;

function FloorNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Floor(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function CeilNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if isNumber(Args[0]) then
    begin
      Args[-1] := NumberVal(Ceil(asNumber(Args[0])));
      Exit(True);
    end;

  Args[-1] := ReturnError('Argument must be a number.');
  Result := False;
end;

function ReadLnNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Input: String;
begin
  try
    Readln(Input);
    Args[-1] := GetString(PChar(Input));
    Result := True;
  except
    Args[-1] := NilVal;
    Result := False;
  end;
end;

function ToNumNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Success: Boolean = False;
  Value: Double;
begin
  try
    Success := TryStrToFloat(String(asCString(Args[0])), Value);
    if Success then
      Args[-1] := NumberVal(Value)
    else
      Args[-1] := ReturnError('Could not convert string to double.');
  except
    Args[-1] := ReturnError('Could not convert string to double.');
    Success := False;
  end;
  Result := Success;
end;

function ToStrNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Chars: PChar;
  Num: Double;
begin
  try
    Num := asNumber(Args[0]);
    Chars := PChar(Num.ToString);
    Args[-1] := GetString(Chars);
    Result := True;
  except
    Args[-1] := ReturnError('Could not convert value to string.');
    Result := False;
  end;
end;

function HasFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Instance: PObjInstance;
  Dummy: TValue;
  Correct: Boolean = True;
begin
  if (ArgCount <> 2) or not isInstance(Args[0]) or not isString(Args[1]) then
    Correct := False;

  if not Correct then
    begin
      Args[-1] := ReturnError('Wrong number/type of arguments.');
      Exit(False);
    end
  else
    begin
      Instance := asInstance(Args[0]);
      Args[-1] := BoolVal(TableGet(@Instance^.Fields, asString(Args[1]), Dummy));
    end;

  Result := True;
end;

function GetFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Instance: PObjInstance;
  Value: TValue;
  Correct: Boolean = True;
begin
  if (ArgCount <> 2) or not isInstance(Args[0]) or not isString(Args[1]) then
    Correct := False;

  if not Correct then
    begin
      Args[-1] := ReturnError('Wrong number/type of arguments.');
      Exit(False);
    end
  else
    begin
      Instance := asInstance(Args[0]);
      if TableGet(@Instance^.Fields, asString(Args[1]), Value) then
        Args[-1] := Value
      else
        Args[-1] := NilVal;
    end;

  Result := True;
end;

function SetFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Instance: PObjInstance;
  Correct: Boolean = True;
begin
  if (ArgCount <> 3) or not isInstance(Args[0]) or not isString(Args[1]) then
    Correct := False;

  if not Correct then
    begin
      Args[-1] := ReturnError('Wrong number/type of arguments.');
      Exit(False);
    end
  else
    begin
      Instance := asInstance(Args[0]);
      TableSet(@Instance^.Fields, asString(Args[1]), Args[2]);
      Args[-1] := Args[2]
    end;

  Result := True;
end;

function DelFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Instance: PObjInstance;
  Correct: Boolean = True;
begin
  if (ArgCount <> 2) or not isInstance(Args[0]) or not isString(Args[1]) then
    Correct := False;

  if not Correct then
    begin
      Args[-1] := ReturnError('Wrong number/type of arguments.');
      Exit(False);
    end
  else
    begin
      Instance := asInstance(Args[0]);
      TableDelete(@Instance^.Fields, asString(Args[1]));
      Args[-1] := NilVal;
    end;

  Result := True;
end;

function ErrorNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := ObjVal(PObj(CopyString('Error!', 6)));
  Result := False;
end;

end.

