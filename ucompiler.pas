unit uCompiler;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, uScanner, uChunk, uObject;

function Compile(const Source: String): PObjFunc;
procedure MarkCompilerRoots;

implementation
uses uCommon, uValue, uDebug, uMemory;

{ The C library function double strtod(const char *str, char **endptr) converts
  the string pointed to by the argument str to a floating-point number (type double).
  If endptr is not NULL, a pointer to the character after the last character used
  in the conversion is stored in the location referenced by endptr.}
function strtod(s1: PChar; s2: PPChar): Double; cdecl; external;

type
  TPrecedence = (
    precNone,
    precAssignment,  // :=
    precConditional, // ?:
    precOr,          // or
    precAnd,         // and
    precEquality,    // = <>
    precComparison,  // < > <= >=
    precTerm,        // + -
    precFactor,      // * /
    precUnary,       // ! -
    precCall,        // . () []
    precPrimary
  );

  TParseFn = procedure(const canAssign: Boolean);

  TParseRule = record
    Prefix: TParseFn;
    Infix: TParseFn;
    Precedence: TPrecedence;
  end;

  TParseRules = array[TTokenTyp] of TParseRule;

  PLocal = ^TLocal;
  TLocal = record
    Name: TToken;
    Depth: Integer;
    isCaptured: Boolean;
  end;

  PUpValue = ^TUpValue;
  TUpValue = record
    Index: Byte;
    isLocal: Boolean;
  end;

  TFuncTyp = (typ_Anonym, typ_Func, typ_Initializer, typ_Method, typ_Script);

  PCompiler = ^TCompiler;
  TCompiler = record
    Enclosing: PCompiler;
    Func: PObjFunc;
    Typ: TFuncTyp;
    Locals: array[0..UINT8_COUNT-1] of TLocal;
    LocalCount: Integer;
    UpValues: array[0..UINT8_COUNT-1] of TUpValue;
    ScopeDepth: Integer;
  end;

  PClassCompiler = ^TClassCompiler;
  TClassCompiler = record
    Enclosing: PClassCompiler;
    Name: TToken;
    hasSuperClass: Boolean;
  end;

  TParser = record
    Current,
    Previous: TToken;
    hadError,
    PanicMode: Boolean;
  end;


var
  Parser: TParser;
  Current: PCompiler=Nil;
  CurrentClass: PClassCompiler=Nil;

  InnermostLoopStart: Integer = -1;
  InnermostLoopScopeDepth: Integer = 0;
  InnermostLoopEnd: Integer = -1;

function CurrentChunk: PChunk;
begin
  Result := @Current^.Func^.Chunk;
end;

// ERROR HANDLING

procedure ErrorAt(Token: TToken; const Message: PChar);
begin
  if Parser.PanicMode then Exit;
  Parser.PanicMode := True;

  WriteFmt('[line %d] Error', [Token.Line]);

  if Token.Typ = TOKEN_EOF then
    Write(' at end')
  else if Token.Typ = TOKEN_ERROR then
    // nothing
  else
    WriteFmt(' at "%.*s"', [Token.Length, Token.Start]);

  WriteLnFmt(': %s', [Message]);
  Parser.hadError := True;
end;

procedure ErrorAtCurrent(const Message: PChar);
begin
  ErrorAt(Parser.Current, Message);
end;

procedure Error(const Message: PChar);
begin
  ErrorAt(Parser.Previous, Message);
end;

procedure Advance;
begin
  Parser.Previous := Parser.Current;

  while True do
    begin
      Parser.Current := ScanToken;
      if Parser.Current.Typ <> TOKEN_ERROR then Break;

      ErrorAtCurrent(Parser.Current.Start);
    end;
end;

procedure Consume(const Typ: TTokenTyp; const Message: PChar);
begin
  if Parser.Current.Typ = Typ then
    Advance
  else
    ErrorAtCurrent(Message);
end;

function Check(const Typ: TTokenTyp): Boolean;
begin
  Result := Parser.Current.Typ = Typ;
end;

function Match(const Typ: TTokenTyp): Boolean;
begin
  if not Check(Typ) then Exit(False);
  Advance;
  Result := True;
end;

procedure EmitByte(const AByte: Byte);
begin
  WriteChunk(CurrentChunk, AByte, Parser.Previous.Line);
end;

procedure EmitByte(const Opcode: TOpcode);
begin
  WriteChunk(CurrentChunk, Opcode, Parser.Previous.Line);
end;

procedure EmitBytes(const Opcode: TOpcode; const AByte: Byte);
begin
  EmitByte(Opcode);
  EmitByte(AByte);
end;

procedure EmitBytes(const Opcode1, Opcode2: TOpcode);
begin
  EmitByte(Opcode1);
  EmitByte(Opcode2);
end;

procedure EmitLoop(const LoopStart: Integer);
var
  OffSet: Integer;
begin
  EmitByte(op_Loop);

  OffSet := CurrentChunk^.Count - LoopStart + 2;
  if OffSet > UInt16.MaxValue then
    Error('Loop body too large.');

  EmitByte((OffSet >> 8) and $FF); // >> is shr
  EmitByte(OffSet and $FF);
end;

function EmitJump(const Instruction: TOpcode): Integer;
begin
  EmitByte(Instruction);
  EmitByte($FF);
  EmitByte($FF);
  Result := CurrentChunk^.Count-2;
end;

procedure EmitReturn;
begin
  if Current^.Typ = typ_Initializer then
    EmitBytes(op_Get_Local, 0)
  else
    EmitByte(op_Nil);

  EmitByte(op_Return);
end;

function MakeConstant(Value: TValue): Byte;
var
  Constant: Integer;
begin
  Constant := AddConstant(CurrentChunk, Value);
  if Constant > Byte.MaxValue then
    begin
      Error('Too many constants in one chunk.');
      Exit(0);
    end;
  Result := Byte(Constant);
end;

procedure EmitConstant(Value: TValue);
begin
  EmitBytes(op_Constant, MakeConstant(Value));
end;

procedure PatchJump(const OffSet: Integer);
var
  Jump: Integer;
begin
  // -2 to adjust for the bytecode for the jump offset itself.
  Jump := CurrentChunk^.Count - OffSet - 2;

  if Jump > UINT16.MaxValue then
    Error('Too much code to jump over.');

  CurrentChunk^.Code[OffSet] := (Jump shr 8) and $FF;
  CurrentChunk^.Code[OffSet+1] := Jump and $FF;
end;

procedure InitCompiler(var Compiler: TCompiler; const Typ: TFuncTyp);
var
  Local: PLocal;
begin
  Compiler.Enclosing := Current;
  Compiler.Func := Nil;
  Compiler.Typ := Typ;
  Compiler.LocalCount := 0;
  Compiler.ScopeDepth := 0;
  Compiler.Func := NewFunc;
  Current := @Compiler;

  if Typ <> typ_Script then
    Current^.Func^.Name := CopyString(Parser.Previous.Start, Parser.Previous.Length);

  Local := @Current^.Locals[Current^.LocalCount];
  Inc(Current^.LocalCount);
  Local^.Depth := 0;
  Local^.isCaptured := False;
  if Typ <> typ_Func then
    begin
      Local^.Name.Start := 'this';
      Local^.Name.Length := 4;
    end
  else
    begin
      Local^.Name.Start := '';
      Local^.Name.Length := 0;
    end;
end;


function EndCompiler: PObjFunc;
var
  Func: PObjFunc;
  Chars: String = '<script>';
begin
  EmitReturn;
  Func := Current^.Func;

  if DebugPrintCode then // set in unit uCommon
    if not Parser.hadError then
      begin
        if Func^.Name <> Nil then
          Chars := Func^.Name^.Chars;
        DisassembleChunk(CurrentChunk, Chars);
      end;

  Current := Current^.Enclosing;
  Result := Func;
end;

procedure BeginScope;
begin
  Inc(Current^.ScopeDepth);
end;

procedure EndScope;
begin
  Dec(Current^.ScopeDepth);

  // Remove local variables from the stack
  while (Current^.LocalCount > 0) and
        (Current^.Locals[Current^.LocalCount-1].Depth > Current^.ScopeDepth) do
    begin
      if Current^.Locals[Current^.LocalCount-1].isCaptured then
        EmitByte(op_Close_UpValue)
      else
        EmitByte(op_Pop);
      Dec(Current^.LocalCount);
    end;
end;


procedure Expression; forward;
procedure Declaration; forward;
procedure Statement; forward;
function getRule(TokenTyp: TTokenTyp): TParseRule; forward;
procedure ParsePrecedence(Precedence: TPrecedence); forward;
function IdentifierConstant(Name: TToken): Byte; forward;
function ResolveLocal(Compiler: PCompiler; var Name: TToken): Integer; forward;
function ResolveUpValue(Compiler: PCompiler; var Name: TToken): Integer; forward;
procedure Function_(const Typ: TFuncTyp); forward;

procedure Binary(const canAssign: Boolean);
var
  OperatorTyp: TTokenTyp;
  Rule: TParseRule;
begin
  OperatorTyp := Parser.Previous.Typ;

  // Compile the right operand.
  Rule := getRule(OperatorTyp);
  ParsePrecedence(Succ(Rule.Precedence));

  // Emit the operator instruction.
  case OperatorTyp of
    TOKEN_PLUS:  EmitByte(op_Add);
    TOKEN_MINUS: EmitByte(op_Subtract);
    TOKEN_STAR:  EmitByte(op_Multiply);
    TOKEN_SLASH: EmitByte(op_Divide);
    TOKEN_BANG_EQUAL: EmitBytes(op_Equal, op_Not);
    TOKEN_EQUAL_EQUAL: EmitByte(op_Equal);
    TOKEN_GREATER: EmitByte(op_Greater);
    TOKEN_GREATER_EQUAL: EmitBytes(op_Less, op_Not);
    TOKEN_LESS: EmitByte(op_Less);
    TOKEN_LESS_EQUAL: EmitBytes(op_Greater, op_Not);
    else
      Exit;  // unreachable
  end;
end;

function ArgumentList: Byte;
var
  ArgCount: Byte = 0;
begin
  if not Check(TOKEN_RIGHT_PAREN) then
    repeat
      Expression;

      if ArgCount >= 255 then
        Error('Cannot have more than 255 arguments.');

      Inc(ArgCount);
    until not Match(TOKEN_COMMA);

  Consume(TOKEN_RIGHT_PAREN, 'Expect ")" after arguments.');
  Result := ArgCount;
end;

procedure Call(const canAssign: Boolean);
var
  ArgCount: Byte;
begin
  ArgCount := ArgumentList;
  EmitBytes(op_Call, ArgCount);
end;

procedure Dot(const canAssign: Boolean);
var
  Name, ArgCount: Byte;
begin
  Consume(TOKEN_IDENTIFIER, 'Expect property name after ".".');
  Name := IdentifierConstant(Parser.Previous);

  if canAssign and Match(TOKEN_EQUAL) then
    begin
      Expression;
      EmitBytes(op_Set_Property, Name);
    end
  else if Match(TOKEN_LEFT_PAREN) then
    begin
      ArgCount := ArgumentList;
      EmitBytes(op_Invoke, Name);
      EmitByte(ArgCount);
    end
  else
    EmitBytes(op_Get_Property, Name);
end;

procedure Literal(const canAssign: Boolean);
begin
  case Parser.Previous.Typ of
    TOKEN_FALSE: EmitByte(op_False);
    TOKEN_TRUE: EmitByte(op_True);
    TOKEN_NIL: EmitByte(op_Nil);
    else
      Exit; // unreachable
  end;
end;

procedure Grouping(const canAssign: Boolean);
begin
  Expression;
  Consume(TOKEN_RIGHT_PAREN, 'Expect ")" after expression.');
end;

procedure Number(const canAssign: Boolean);
var
  Value: Double;
begin
  Value := strtod(Parser.Previous.Start, Nil);
  EmitConstant(NumberVal(Value));
end;

procedure Strings(const canAssign: Boolean);
begin
  EmitConstant(ObjVal(PObj(CopyString(Parser.Previous.Start+1, Parser.Previous.Length-2))));
end;

procedure Interpolated(const canAssign: Boolean);
begin
  EmitConstant(ObjVal(PObj(CopyString(Parser.Previous.Start+1, Parser.Previous.Length-3))));
  Expression;
  EmitByte(op_Add);
  Advance;
  while Parser.Previous.Typ = TOKEN_INTERPOLATED do
    begin
      EmitConstant(ObjVal(PObj(CopyString(Parser.Previous.Start+1, Parser.Previous.Length-3))));
      Expression;
      EmitByte(op_Add);
      Advance;
      EmitByte(op_Add);
    end;

  if Parser.Previous.Typ = TOKEN_STRING then
    begin
      EmitConstant(ObjVal(PObj(CopyString(Parser.Previous.Start+1, Parser.Previous.Length-2))));
      EmitByte(op_Add);
    end
  else
    Error('Expected end of string interpolation.');
end;

procedure NamedVariable(Name: TToken; const canAssign: Boolean);
var
  getOp, setOp: TOpcode;
  Arg: Integer;
begin
  Arg := ResolveLocal(Current, Name);
  if Arg <> -1 then
    begin
      getOp := op_Get_Local;
      setOp := op_Set_Local;
    end
  else
    begin
      Arg := ResolveUpvalue(Current, Name);
      if Arg <> -1 then
        begin
          getOp := op_Get_UpValue;
          setOp := op_Set_UpValue;
        end
      else
        begin
          Arg := IdentifierConstant(Name);
          getOp := op_Get_Global;
          setOp := op_Set_Global;
        end;
    end;

  if canAssign and Match(TOKEN_EQUAL) then
    begin
      Expression;
      EmitBytes(setOp, UInt8(Arg));
    end
  else
    EmitBytes(getOp, UInt8(Arg));
end;

procedure Variable(const canAssign: Boolean);
begin
  NamedVariable(Parser.Previous, canAssign);
end;

procedure This(const canAssign: Boolean);
begin
  if CurrentClass = Nil then
    begin
      Error('Cannot use "this" outside of a class.');
      Exit;
    end;

  Variable(False);
end;

function SyntheticToken(Text: PChar): TToken;
begin
  Result.Start := Text;
  Result.Length := Integer(strlen(Text));
end;

procedure Super(const canAssign: Boolean);
var
  Name, ArgCount: Byte;
begin
  if CurrentClass = Nil then
    Error('Cannot use "super" outside of a class.')
  else if not CurrentClass^.hasSuperClass then
    Error('Cannot use "super" in a class with no superclass.');

  Consume(TOKEN_DOT, 'Expect "." after "super".');
  Consume(TOKEN_IDENTIFIER, 'Expect superclass method name.');
  Name := IdentifierConstant(Parser.Previous);

  NamedVariable(SyntheticToken('this'), False);
  if Match(TOKEN_LEFT_PAREN) then
    begin
      ArgCount := ArgumentList;
      NamedVariable(SyntheticToken('super'), False);
      EmitBytes(op_Super_Invoke, Name);
      EmitByte(ArgCount);
    end
  else
    begin
      NamedVariable(SyntheticToken('super'), False);
      EmitBytes(op_Get_Super, Name);
    end;
end;

procedure Unary(const canAssign: Boolean);
var
  OperatorTyp: TTokenTyp;
begin
  OperatorTyp := Parser.Previous.Typ;

  // Compile the operand.
  ParsePrecedence(precUnary);

  // Emit the operator instruction.
  case OperatorTyp of
    TOKEN_MINUS: EmitByte(op_Negate);
    TOKEN_PLUS: begin {do nothing} end;
    TOKEN_BANG: EmitByte(op_Not);
    else
      Exit; // unreachable
  end;
end;

procedure Conditional(const canAssign: Boolean);
var
  IfJump, ElseJump: Integer;
begin
  // Jump to else branch if condition is false
  IfJump := EmitJump(op_Jump_If_False);
  // Compile the then branch.
  ParsePrecedence(precConditional);

  Consume(TOKEN_COLON, 'Expect ":" after then branch of conditional operator.');

  // Jump over the else branch when the if branch is taken.
  ElseJump := EmitJump(op_Jump);

  // Compile the else branch.
  PatchJump(IfJump);
  ParsePrecedence(precAssignment);

  // Patch the jump over the else.
  PatchJump(ElseJump);
end;

procedure AndThen(const canAssign: Boolean);
var
  EndJump: Integer;
begin
  EndJump := EmitJump(op_Jump_If_False);

  EmitByte(op_Pop);
  ParsePrecedence(precAnd);

  PatchJump(EndJump);
end;

procedure OrElse(const canAssign: Boolean);
var
  ElseJump, EndJump: Integer;
begin
  ElseJump := EmitJump(op_Jump_If_False);
  EndJump := EmitJump(op_Jump);

  PatchJump(ElseJump);
  EmitByte(op_Pop);

  ParsePrecedence(precOr);
  PatchJump(EndJump);
end;

procedure Lambda(const canAssign: Boolean);
begin
  Function_(typ_Anonym);
end;

const Rules: TParseRules = (
  (Prefix: @Grouping;     Infix: @Call;        Precedence: precCall), // TOKEN_LEFT_PAREN
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_RIGHT_PAREN
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_LEFT_BRACE
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_RIGHT_BRACE
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_COMMA
  (Prefix: Nil;           Infix: @Dot;         Precedence: precCall), // TOKEN_DOT
  (Prefix: @Unary;        Infix: @Binary;      Precedence: precTerm), // TOKEN_MINUS
  (Prefix: @Unary;        Infix: @Binary;      Precedence: precTerm), // TOKEN_PLUS
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_SEMICOLON
  (Prefix: Nil;           Infix: @Binary;      Precedence: precFactor), // TOKEN_SLASH
  (Prefix: Nil;           Infix: @Binary;      Precedence: precFactor), // TOKEN_STAR
  (Prefix: Nil;           Infix: @Conditional; Precedence: precConditional), // TOKEN_QUESTION
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_COLON
  (Prefix: @Unary;        Infix: Nil;          Precedence: precNone), // TOKEN_BANG
  (Prefix: Nil;           Infix: @Binary;      Precedence: precEquality), // TOKEN_BANG_EQUAL
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_EQUAL
  (Prefix: Nil;           Infix: @Binary;      Precedence: precEquality), // TOKEN_EQUAL_EQUAL
  (Prefix: Nil;           Infix: @Binary;      Precedence: precComparison), // TOKEN_GREATER
  (Prefix: Nil;           Infix: @Binary;      Precedence: precComparison), // TOKEN_GREATER_EQUAL
  (Prefix: Nil;           Infix: @Binary;      Precedence: precComparison), // TOKEN_LESS
  (Prefix: Nil;           Infix: @Binary;      Precedence: precComparison), // TOKEN_LESS_EQUAL
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_ARROW
  (Prefix: @Variable;     Infix: Nil;          Precedence: precNone), // TOKEN_IDENTIFIER
  (Prefix: @Strings;      Infix: Nil;          Precedence: precNone), // TOKEN_STRING
  (Prefix: @Interpolated; Infix: Nil;          Precedence: precNone), // TOKEN_INTERPOLATED
  (Prefix: @Number;       Infix: Nil;          Precedence: precNone), // TOKEN_NUMBER
  (Prefix: Nil;           Infix: @AndThen;     Precedence: precAnd), // TOKEN_AND
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_BREAK
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_CASE
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_CLASS
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_CONTINUE
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_DEFAULT
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_ELSE
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_ENSURE
  (Prefix: @Literal;      Infix: Nil;          Precedence: precNone), // TOKEN_FALSE
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_FOR
  (Prefix: @Lambda;       Infix: Nil;          Precedence: precNone), // TOKEN_FUN
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_IF
  (Prefix: @Literal;      Infix: Nil;          Precedence: precNone), // TOKEN_NIL
  (Prefix: Nil;           Infix: @OrElse;      Precedence: precOr), // TOKEN_OR
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_PRINT
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_RETURN
  (Prefix: @Super;        Infix: Nil;          Precedence: precNone), // TOKEN_SUPER
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_SWITCH
  (Prefix: @This;         Infix: Nil;          Precedence: precNone), // TOKEN_THIS
  (Prefix: @Literal;      Infix: Nil;          Precedence: precNone), // TOKEN_TRUE
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_VAR
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_WHILE
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone), // TOKEN_ERROR
  (Prefix: Nil;           Infix: Nil;          Precedence: precNone)  // TOKEN_EOF
);

function getRule(TokenTyp: TTokenTyp): TParseRule;
begin
  Result := Rules[TokenTyp];
end;


procedure ParsePrecedence(Precedence: TPrecedence);
var
  PrefixRule, InfixRule: TParseFn;
  canAssign: Boolean;
begin
  Advance;
  PrefixRule := getRule(Parser.Previous.Typ).Prefix;
  if PrefixRule = Nil then
    begin
      Error('Expected expression.');
      Exit;
    end;

  canAssign := Precedence <= precAssignment;
  PrefixRule(canAssign);

  while Precedence <= getRule(Parser.Current.Typ).Precedence do
    begin
      Advance;
      InfixRule := getRule(Parser.Previous.Typ).Infix;
      InfixRule(canAssign);
    end;

  if canAssign and Match(TOKEN_EQUAL) then
    Error('Invalid assignment target.');
end;

function IdentifierConstant(Name: TToken): Byte;
begin
  Result := MakeConstant(ObjVal(PObj(CopyString(Name.Start, Name.Length))));
end;

function IdentifiersEqual(a, b: TToken): Boolean;
begin
  if a.Length <> b.Length then Exit(False);
  Result := StrLComp(a.Start, b.Start, a.Length) = 0
end;

function ResolveLocal(Compiler: PCompiler; var Name: TToken): Integer;
var
  i: Integer;
  Local: PLocal;
begin
  for i := Compiler^.LocalCount-1 downto 0 do
    begin
      Local := @Compiler^.Locals[i];
      if IdentifiersEqual(Name, Local^.Name) then
        begin
          if Local^.Depth = -1 then
            Error('Cannot read local variable in its own initializer.');;
          Exit(i);
        end;
    end;
  Result := -1;
end;

function AddUpValue(Compiler: PCompiler; Index: Byte; isLocal: Boolean): Integer;
var
  UpValueCount, i: Integer;
  UpValue: PUpValue;
begin
  UpValueCount := Compiler^.Func^.UpValueCount;

  for i:=0 to UpValueCount-1 do
    begin
      UpValue := @Compiler^.UpValues[i];
      if (UpValue^.Index = Index) and (UpValue^.isLocal = isLocal) then
        Exit(i);
    end;

  if UpValueCount = UINT8_COUNT then
    begin
      Error('Too many closure variables in function.');
      Exit(0);
    end;

  Compiler^.UpValues[UpValueCount].isLocal := isLocal;
  Compiler^.UpValues[UpValueCount].Index := Index;
  Result := Compiler^.Func^.UpValueCount;
  Inc(Compiler^.Func^.UpValueCount);
end;

function ResolveUpValue(Compiler: PCompiler; var Name: TToken): Integer;
var
  Local, UpValue: Integer;
begin
  if Compiler^.Enclosing = Nil then Exit(-1);

  Local := ResolveLocal(Compiler^.Enclosing, Name);
  if Local <> -1 then
    begin
      Compiler^.Enclosing^.Locals[Local].isCaptured := True;
      Exit(AddUpValue(Compiler, Byte(Local), True));
    end;

  UpValue := ResolveUpValue(Compiler^.Enclosing, Name);
  if UpValue <> -1 then
    Exit(AddUpValue(Compiler, Byte(UpValue), False));

  Result := -1;
end;

procedure AddLocal(Name: TToken);
var
  Local: PLocal;
begin
  if Current^.LocalCount = UINT8_COUNT then
    begin
      Error('Too many local variables in function.');
      Exit;
    end;

  Local := @Current^.Locals[Current^.LocalCount];
  Inc(Current^.LocalCount);
  Local^.Name := Name;
  Local^.Depth := -1;
  Local^.isCaptured := False;
end;

procedure DeclareVariable;
var
  Name: TToken;
  i: Integer;
  Local: PLocal;
begin
  // Global variables are implicitly declared.
  if Current^.ScopeDepth = 0 then Exit;

  Name := Parser.Previous;
  for i := Current^.LocalCount-1 downto 0 do
    begin
      Local := @Current^.Locals[i];
      if (Local^.Depth <> -1) and (Local^.Depth < Current^.ScopeDepth) then
        Break;

      if IdentifiersEqual(Name, Local^.Name) then
        Error('Variable with this name already declared in this scope.');
    end;
  AddLocal(Name);
end;

function ParseVariable(const ErrorMessage: PChar): Byte;
begin
  Consume(TOKEN_IDENTIFIER, ErrorMessage);

  DeclareVariable;
  if Current^.ScopeDepth > 0 then Exit(0);

  Result := IdentifierConstant(Parser.Previous);
end;

procedure MarkInitialized;
begin
  if Current^.ScopeDepth = 0 then Exit;
  Current^.Locals[Current^.LocalCount-1].Depth := Current^.ScopeDepth;
end;

procedure DefineVariable(const Global: UInt8);
begin
  if Current^.ScopeDepth > 0 then
    begin
      MarkInitialized;
      Exit;
    end;
  EmitBytes(op_Define_Global, Global);
end;

procedure Expression;
begin
  ParsePrecedence(precAssignment);
end;

procedure Block;
begin
  while not Check(TOKEN_RIGHT_BRACE) and not Check(TOKEN_EOF) do
    Declaration;

  Consume(TOKEN_RIGHT_BRACE, 'Expect "}" after block.');
end;

procedure Function_(const Typ: TFuncTyp);
var
  Compiler: TCompiler;
  Func: PObjFunc;
  ParamConstant: Byte;
  i: Integer;
begin
  InitCompiler(Compiler, Typ);
  BeginScope;

  // Compile the parameter list
  Consume(TOKEN_LEFT_PAREN, 'Expect "(" after function name.');
  if not Check(TOKEN_RIGHT_PAREN) then
    repeat
      Inc(Current^.Func^.Arity);
      if Current^.Func^.Arity > 255 then
        ErrorAtCurrent('Cannot have more than 255 parameters.');

      ParamConstant := ParseVariable('Expect parameter name.');
      DefineVariable(ParamConstant)
    until not Match(TOKEN_COMMA);
  Consume(TOKEN_RIGHT_PAREN, 'Expect ")" after parameters.');

  // is it an arrow func?
  if Match(TOKEN_ARROW) then
    begin
      if Current^.Typ = typ_Initializer then
        Error('Cannot return a value from an initializer.');

      Expression;
      EmitByte(op_Return);
    end
  else  // The body
    begin
      Consume(TOKEN_LEFT_BRACE, 'Expect "{" before function body.');
      Block;
    end;

  // Create the function object
  Func := EndCompiler;
  EmitBytes(op_Closure, MakeConstant(ObjVal(Func)));

  for i := 0 to Func^.UpValueCount-1 do
    begin
      if Compiler.UpValues[i].isLocal then
        EmitByte(1)
      else
        EmitByte(0);
      EmitByte(Compiler.UpValues[i].Index);
    end;
end;

procedure Method;
var
  Constant: Byte;
  Typ: TFuncTyp;
begin
  Consume(TOKEN_IDENTIFIER, 'Expect method name.');
  Constant := IdentifierConstant(Parser.Previous);

  Typ := typ_Method;
  if (Parser.Previous.Length = 4) and
     (StrLComp(Parser.Previous.Start, 'init', 4) = 0) then
    Typ := typ_Initializer;

  Function_(Typ);
  EmitBytes(op_Method, Constant);
end;

procedure FunDeclaration;
var
  Global: UInt8;
begin
  Global := ParseVariable('Expect function name.');
  MarkInitialized;
  Function_(typ_Func);
  DefineVariable(Global);
end;

procedure VarDeclaration;
var
  Global: Byte;
begin
  Global := ParseVariable('Expect variable name.');

  if Match(TOKEN_EQUAL) then
    Expression
  else
    EmitByte(op_Nil);
  Consume(TOKEN_SEMICOLON, 'Expect ";" after variable declaration.');

  DefineVariable(Global);
end;

procedure ClassDeclaration;
var
  NameConstant: Byte;
  ClassName: TToken;
  ClassCompiler: TClassCompiler;
begin
  Consume(TOKEN_IDENTIFIER, 'Expect class name.');
  ClassName := Parser.Previous;
  NameConstant := IdentifierConstant(Parser.Previous);
  DeclareVariable;

  EmitBytes(op_Class, NameConstant);
  DefineVariable(NameConstant);

  ClassCompiler.Name := Parser.Previous;
  ClassCompiler.hasSuperClass := False;
  ClassCompiler.Enclosing := CurrentClass;
  CurrentClass := @ClassCompiler;

  if Match(TOKEN_LESS) then
    begin
      Consume(TOKEN_IDENTIFIER, 'Expect superclass name.');
      Variable(False);

      if IdentifiersEqual(ClassName, Parser.Previous) then
        Error('A class cannot inherit from itself.');

      BeginScope;
      AddLocal(SyntheticToken('super'));
      DefineVariable(0);

      NamedVariable(ClassName, False);
      EmitByte(op_Inherit);
      ClassCompiler.hasSuperClass := True;
    end;

  NamedVariable(ClassName, False);
  Consume(TOKEN_LEFT_BRACE, 'Expect "{" before class body.');
  while not Check(TOKEN_RIGHT_BRACE) and not Check(TOKEN_EOF) do
    Method;
  Consume(TOKEN_RIGHT_BRACE, 'Expect "}" after class body.');
  EmitByte(op_Pop);

  if ClassCompiler.hasSuperClass then
    EndScope;

  CurrentClass := CurrentClass^.Enclosing;
end;

procedure ExpressionStatement;
begin
  Expression;
  Consume(TOKEN_SEMICOLON, 'Expect ";" after value.');
  EmitByte(op_Pop);
end;

procedure ForStatement;
var
  ExitJump, BodyJump: Integer;
  IncrementStart: Integer;
  SurroundingLoopStart, SurroundingLoopScopeDepth: Integer;
begin
  BeginScope;

  Consume(TOKEN_LEFT_PAREN, 'Expect "(" after "for".');
  if Match(TOKEN_SEMICOLON) then
    // No initializer
  else if Match(TOKEN_VAR) then
    VarDeclaration
  else
    ExpressionStatement;

  SurroundingLoopStart := InnermostLoopStart;
  SurroundingLoopScopeDepth := InnermostLoopScopeDepth;
  InnermostLoopStart := CurrentChunk^.Count;
  InnermostLoopScopeDepth := Current^.ScopeDepth;

  ExitJump := -1;
  if not Match(TOKEN_SEMICOLON) then
    begin
      Expression;
      Consume(TOKEN_SEMICOLON, 'Expect ";" after loop condition.');

      // Jump out of the loop if condition is false.
      ExitJump := EmitJump(op_Jump_If_False);

      EmitByte(op_Pop); // condition
    end;

  if not Match(TOKEN_RIGHT_PAREN) then
    begin
      BodyJump := EmitJump(op_Jump);

      IncrementStart := CurrentChunk^.Count;
      Expression;
      EmitByte(op_Pop);
      Consume(TOKEN_RIGHT_PAREN, 'Expect ")" after for clauses.');

      EmitLoop(InnermostLoopStart);
      InnermostLoopStart := IncrementStart;
      PatchJump(BodyJump);
   end;

  Statement;

  EmitLoop(InnermostLoopStart);

  if ExitJump <> -1 then
    begin
      PatchJump(ExitJump);
      EmitByte(op_Pop);
    end;

  InnermostLoopStart := SurroundingLoopStart;
  InnermostLoopScopeDepth := SurroundingLoopScopeDepth;

  EndScope;

  if InnermostLoopEnd <> -1 then
    PatchJump(InnermostLoopEnd);
end;

procedure IfStatement;
var
  ThenJump, ElseJump: Integer;
  VarDeclared: Boolean = False;
begin
  Consume(TOKEN_LEFT_PAREN, 'Expect "(" after "if".');

  if Match(TOKEN_VAR) then
    begin
      BeginScope;
      VarDeclaration;
      VarDeclared := True;
    end;

  Expression;
  Consume(TOKEN_RIGHT_PAREN, 'Expect ")" after condition.');

  ThenJump := EmitJump(op_Jump_If_False);
  EmitByte(op_Pop);
  Statement;

  ElseJump := EmitJump(op_Jump);
  PatchJump(ThenJump);
  EmitByte(op_Pop);

  if Match(TOKEN_ELSE) then
    Statement;
  PatchJump(ElseJump);

  if VarDeclared then
    EndScope;
end;

procedure PrintStatement;
begin
  Expression;
  Consume(TOKEN_SEMICOLON, 'Expect ";" after value.');
  EmitByte(op_Print);
end;

procedure ReturnStatement;
begin
  if Current^.Typ = typ_Script then
    Error('Cannot return from top-level code.');

  if Match(TOKEN_SEMICOLON) then
    EmitReturn
  else
    begin
      if Current^.Typ = typ_Initializer then
        Error('Cannot return a value from an initializer.');

      Expression;
      Consume(TOKEN_SEMICOLON, 'Expect ";" after return value.');
      EmitByte(op_Return);
    end;
end;

procedure WhileStatement;
var
  ExitJump, LoopStart: Integer;
  VarDeclared: Boolean = False;
begin
  LoopStart := CurrentChunk^.Count;

  Consume(TOKEN_LEFT_PAREN, 'Expect "(" after "while".');

  if Match(TOKEN_VAR) then
    begin
      BeginScope;
      VarDeclaration;
      VarDeclared := True;
    end;

  Expression;
  Consume(TOKEN_RIGHT_PAREN, 'Expect ")" after condition.');

  ExitJump := EmitJump(op_Jump_If_False);

  EmitByte(op_Pop);

  Statement;

  EmitLoop(LoopStart);

  PatchJump(ExitJump);
  EmitByte(op_Pop);

  if VarDeclared then
    EndScope;
end;

procedure SwitchStatement;
var
  State: Integer = 0;  // 0: before all cases, 1: before default, 2: after default.
  CaseEnds: array[0..MaxCases-1] of Integer;
  CaseCount: Integer = 0;
  PreviousCaseSkip: Integer = -1;
  CaseType: TTokenTyp;
  i: Integer;
begin
  Consume(TOKEN_LEFT_PAREN, 'Expect "(" after "switch".');
  Expression;
  Consume(TOKEN_RIGHT_PAREN, 'Expect ")" after value.');
  Consume(TOKEN_LEFT_BRACE, 'Expect "{" before switch cases.');

  while not Match(TOKEN_RIGHT_BRACE) and not Check(TOKEN_EOF) do
    begin
      if Match(TOKEN_CASE) or Match(TOKEN_DEFAULT) then
        begin
          CaseType := Parser.Previous.Typ;

          if State = 2 then
            Error('Cannot have another case or default after the default case.');

          if State = 1 then
            begin
             // At the end of the previous case, jump over the others.
              CaseEnds[CaseCount] := EmitJump(op_Jump);
              Inc(CaseCount);

              // Patch its condition to jump to the next case (this one).
              PatchJump(PreviousCaseSkip);
              EmitByte(op_Pop);
            end;

          if CaseType = TOKEN_CASE then
            begin
              State := 1;
              // See if the case is equal to the value.
              EmitByte(op_Dup);
              Expression;
              Consume(TOKEN_COLON, 'Expect ":" after case value.');

              EmitByte(op_Equal);
              PreviousCaseSkip := EmitJump(OP_JUMP_IF_FALSE);

              // Pop the comparison result.
              EmitByte(op_Pop);
            end
          else
            begin
              State := 2;
              Consume(TOKEN_COLON, 'Expect ":" after default.');
              PreviousCaseSkip := -1;
            end;
        end
      else
        begin
          // Otherwise, it's a statement inside the current case.
          if State = 0 then
            Error('Cannot have statements before any case.');
          Statement;
        end;
    end;

  // If we ended without a default case, patch its condition jump.
  if State = 1 then
    begin
      PatchJump(PreviousCaseSkip);
      EmitByte(op_Pop);
    end;

  // Patch all the case jumps to the end.
  for i := 0 to CaseCount-1 do
    PatchJump(CaseEnds[i]);

  EmitByte(op_Pop);
end;

procedure Synchronize;
begin
  Parser.PanicMode := False;

  while Parser.Current.Typ <> TOKEN_EOF do
    begin
      if parser.Previous.Typ = TOKEN_SEMICOLON then Exit;

      case Parser.Current.Typ of
        TOKEN_CLASS,
        TOKEN_FUN,
        TOKEN_VAR,
        TOKEN_FOR,
        TOKEN_IF,
        TOKEN_WHILE,
        TOKEN_PRINT,
        TOKEN_RETURN: Exit;
      end;
      Advance;
    end;
end;

procedure Declaration;
begin
  if Match(TOKEN_CLASS) then
    ClassDeclaration
  else if Match(TOKEN_FUN) then
    FunDeclaration
  else if Match(TOKEN_VAR) then
    VarDeclaration
  else
    Statement;

  if Parser.PanicMode then Synchronize;
end;

procedure ContinueStatement;
var
  i: Integer;
begin
  if InnermostLoopStart = -1 then
    Error('Cannot use "continue" outside of a loop.');

  Consume(TOKEN_SEMICOLON, 'Expect ";" after "continue".');

  // Discard any locals created inside the loop.
  i := Current^.LocalCount - 1;
  while (i >= 0) and (Current^.Locals[i].Depth > InnermostLoopScopeDepth) do
    begin
      EmitByte(op_Pop);
      Dec(i);
    end;

  // Jump to top of current innermost loop.
  EmitLoop(InnermostLoopStart);
end;

procedure BreakStatement;
var
  i: Integer;
begin
  if InnermostLoopStart = -1 then
    Error('Cannot use "break" outside of a loop.');

  Consume(TOKEN_SEMICOLON, 'Expect ";" after "break".');

  // Discard any locals created inside the loop.
  i := Current^.LocalCount - 1;
  while (i >= 0) and (Current^.Locals[i].Depth > InnermostLoopScopeDepth) do
    begin
      EmitByte(op_Pop);
      Dec(i);
    end;

  // Jump to past end of current innermost loop.
  InnermostLoopEnd := EmitJump(op_Jump);
end;

procedure EnsureStatement;
var
  OKJump: Integer;
begin
  if Match(TOKEN_VAR) then
    VarDeclaration;

  Expression;
  EmitByte(op_Not); // test for inequality

  OKJump := EmitJump(op_Jump_If_False);
  EmitByte(op_Pop);

  Consume(TOKEN_ELSE, 'Expect "else" after condition.');
  Statement;

  PatchJump(OKJump);
end;

procedure Statement;
begin
  if Match(TOKEN_PRINT) then
    PrintStatement
  else if Match(TOKEN_FOR) then
    ForStatement
  else if Match(TOKEN_IF) then
    IfStatement
  else if Match(TOKEN_RETURN) then
    ReturnStatement
  else if Match(TOKEN_WHILE) then
    WhileStatement
  else if Match(TOKEN_LEFT_BRACE) then
    begin
      BeginScope;
      Block;
      EndScope;
    end
  else if Match(TOKEN_SWITCH) then
    SwitchStatement
  else if Match(TOKEN_CONTINUE) then
    ContinueStatement
  else if Match(TOKEN_BREAK) then
    BreakStatement
  else if Match(TOKEN_ENSURE) then
    EnsureStatement
  else
    ExpressionStatement;
end;

function Compile(const Source: String): PObjFunc;
var
  Compiler: TCompiler;
  Func: PObjFunc;
begin
  InitScanner(Source);
  InitCompiler(Compiler, typ_Script);

  Parser.hadError := False;
  Parser.PanicMode := False;

  Advance;

  while not Match(TOKEN_EOF) do
    Declaration;

  Func :=  EndCompiler;

  if Parser.hadError then
    Result := Nil
  else
    Result := Func;
end;

procedure MarkCompilerRoots;
var
  Compiler: PCompiler;
begin
  Compiler := Current;
  while Compiler <> Nil do
    begin
      MarkObject(PObj(Compiler^.Func));
      Compiler := Compiler^.Enclosing;
    end;
end;

end.

