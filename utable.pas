unit uTable;

{$mode delphi}{$H+}

interface

uses
  SysUtils, uObject, uValue;

const
  TABLE_MAX_LOAD = 0.75;

// MOVED TO uObject.pas to prevent circular reference
//type
//
//  TEntry = record
//    Key: PObjString;
//    Value: TValue;
//  end;
//
//  {$POINTERMATH ON}
//  PEntry = ^TEntry;
//  {$POINTERMATH OFF}
//
//  TTable = record
//    Count,
//    Capacity: LongInt;
//    Entries: PEntry;
//  end;
//
//  {$POINTERMATH ON}
//  PTable = ^TTable;
//  {$POINTERMATH OFF}

procedure InitTable(Table: PTable);
procedure FreeTable(Table: PTable);
function TableGet(Table: PTable; Key: PObjString; var Value: TValue): Boolean;
function TableSet(Table: PTable; Key: PObjString; Value: TValue): Boolean;
function TableDelete(Table: PTable; Key: PObjString): Boolean;
procedure TableAddAll(TableFrom, TableTo: PTable);
function TableFindString(Table: PTable; Chars: PChar; const Length: Integer;
  const Hash: Longword): PObjString;

procedure TableRemoveWhite(Table: PTable);
procedure MarkTable(Table: PTable);

implementation
uses uMemory;

procedure InitTable(Table: PTable);
begin
  Table^.Count := 0;
  Table^.Capacity := -1;
  Table^.Entries := Nil;
end;

procedure FreeTable(Table: PTable);
begin
  Reallocate(Table^.Entries, SizeOf(TEntry)*(Table^.Capacity+1), 0); // Free_Array
  InitTable(Table);
end;

function FindEntry(Entries: PEntry; const Capacity: LongInt; Key: PObjString): PEntry;
var
  Index: LongWord;
  Entry, Tombstone: PEntry;
begin
  Index := Key^.Hash and Capacity;
  Tombstone := Nil;
  while True do
    begin
      Entry := @Entries[Index];

      if Entry^.Key = Nil then
        begin
          if isNil(Entry^.Value) then
            Exit(IfThen<PEntry>(Tombstone<>Nil, Tombstone, Entry))
          else
            if Tombstone = Nil then Tombstone := Entry;
        end
      else if Entry^.Key = Key then
        Exit(Entry);
      Index := (Index+1) and Capacity;
    end;
end;

procedure AdjustCapacity(Table: PTable; const Capacity: LongInt);
var
  Entries, Entry, Dest: PEntry;
  i: Integer;
begin
  Entries := PEntry(Reallocate(NiL, 0, SizeOf(TEntry) * (Capacity+1)));
  for i := 0 to Capacity do
    begin
      Entries[i].Key := Nil;
      Entries[i].Value := NilVal;
    end;

  Table^.Count := 0;
  for i := 0 to Table^.Capacity do
    begin
      Entry := @Table^.Entries[i];
      if Entry^.Key = Nil then Continue;
      Dest := FindEntry(Entries, Capacity, Entry^.Key);
      Dest^.Key := Entry^.Key;
      Dest^.Value := Entry^.Value;
      Inc(Table^.Count);
    end;

  ReAllocMem(Table^.Entries, 0);
  Table^.Entries := Entries;
  Table^.Capacity := Capacity;
end;

function TableGet(Table: PTable; Key: PObjString; var Value: TValue): Boolean;
var
  Entry: PEntry;
begin
  if Table^.Count = 0 then Exit(False);

  Entry := FindEntry(Table^.Entries, Table^.Capacity, Key);
  if Entry^.Key = Nil then Exit(False);

  Value := Entry^.Value;
  Result := True;
end;

function TableSet(Table: PTable; Key: PObjString; Value: TValue): Boolean;
var
  Entry: PEntry;
  isNewKey: Boolean;
  Capacity: LongInt;
begin
  if (Table^.Count + 1) > (Table^.Capacity+1) * TABLE_MAX_LOAD then
    begin
      Capacity := Grow_Capacity(Table^.Capacity+1) - 1;
      AdjustCapacity(Table, Capacity);
    end;
  Entry := FindEntry(Table^.Entries, Table^.Capacity, Key);
  isNewKey := Entry^.Key = Nil;
  if isNewKey and (isNil(Entry^.Value)) then Inc(Table^.Count);
  Entry^.Key := Key;
  Entry^.Value := Value;
  Exit(isNewKey);
end;

function TableDelete(Table: PTable; Key: PObjString): Boolean;
var
  Entry: PEntry;
begin
  if Table^.Count = 0 then Exit(False);

  // find the entry
  Entry := FindEntry(Table^.Entries, Table^.Capacity, Key);
  if Entry^.Key = Nil then Exit(False);

  // Place a tombstone in the entry.
  Entry^.Key := Nil;
  Entry^.Value := BoolVal(True);

  Result := True;
end;

procedure TableAddAll(TableFrom, TableTo: PTable);
var
  i: Integer;
  Entry: PEntry;
begin
  for i := 0 to TableFrom^.Capacity do
    begin
      Entry := @TableFrom^.Entries[i];
      if Entry^.Key <> Nil then
        TableSet(TableTo, Entry^.Key, Entry^.Value);
    end;
end;

function TableFindString(Table: PTable; Chars: PChar; const Length: Integer;
  const Hash: Longword): PObjString;
var
  Index: LongWord;
  Entry: PEntry;
begin
  if Table^.Count = 0 then Exit(Nil);

  Index := Hash and Table^.Capacity;

  while True do
    begin
      Entry := @Table^.Entries[Index];
      if Entry^.Key = Nil then
        begin
          // Stop if we find an empty non-tombstone entry.
          if isNil(Entry^.Value) then Exit(Nil);
        end
      else if (Entry^.Key^.Length = Length) and (Entry^.Key^.Hash = Hash) and
              (StrLComp(Entry^.Key^.Chars, Chars, Length) = 0) then
        Exit(Entry^.Key);

      Index := (Index + 1) and Table^.Capacity;
    end;
end;

procedure TableRemoveWhite(Table: PTable);
var
  i: Integer;
  Entry: PEntry;
begin
  for i := 0 to Table^.Capacity-1 do
    begin
      Entry := @Table^.Entries[i];
      if (Entry^.Key <> Nil) and (not Entry^.Key^.Obj.isMarked) then
        TableDelete(Table, Entry^.Key);
    end;
end;

procedure MarkTable(Table: PTable);
var
  i: Integer;
  Entry: PEntry;
begin
  for i := 0 to Table^.Capacity do
    begin
      Entry := @Table^.Entries[i];
      MarkObject(PObj(Entry^.Key));
      MarkValue(Entry^.Value);
    end;
end;

end.

