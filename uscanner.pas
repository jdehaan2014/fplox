unit uScanner;

{$mode delphi}{$H+}

interface

uses
  SysUtils;

type

  TTokenTyp = (
    // Single-character tokens.
    TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
    TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
    TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,
    TOKEN_QUESTION, TOKEN_COLON,

    // One or two character tokens.
    TOKEN_BANG, TOKEN_BANG_EQUAL,
    TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER, TOKEN_GREATER_EQUAL,
    TOKEN_LESS, TOKEN_LESS_EQUAL,
    TOKEN_ARROW,

    // Literals.
    TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_INTERPOLATED, TOKEN_NUMBER,

    // Keywords.
    TOKEN_AND, TOKEN_BREAK, TOKEN_CASE, TOKEN_CLASS, TOKEN_CONTINUE, TOKEN_DEFAULT,
    TOKEN_ELSE, TOKEN_ENSURE, TOKEN_FALSE, TOKEN_FOR, TOKEN_FUN, TOKEN_IF,
    TOKEN_NIL, TOKEN_OR, TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_SWITCH,
    TOKEN_THIS, TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE,

    TOKEN_ERROR,
    TOKEN_EOF
  );

  TScanner = record
    Start: PChar;
    Current: PChar;
    Line: Integer;
    OpenParens: Integer;
  end;

  TToken = record
    Typ: TTokenTyp;
    Start: PChar;
    Length,
    Line: Integer;
  end;


procedure InitScanner(const Source: String);
function ScanToken: TToken;

var
  Scanner: TScanner;

implementation

procedure InitScanner(const Source: String);
begin
  Scanner.Start := @Source[1];
  Scanner.Current := @Source[1];
  Scanner.Line := 1;
  Scanner.OpenParens := 0;
end;

function IsDigit(const ch: Char): Boolean;
begin
  Result := ch in ['0'..'9'];
end;

function IsAlpha(const ch: Char): Boolean;
begin
  Result := (ch in ['a'..'z']) or (ch in ['A'..'Z']) or (ch = '_');
end;

function IsAtEnd: Boolean;
begin
  Result := Scanner.Current^ = #0;
end;

function Advance: Char;
begin
  Result := Scanner.Current^;
  Inc(Scanner.Current);
end;

function Match(const Expected: Char): Boolean;
begin
  if IsAtEnd then Exit(False);
  if Scanner.Current^ <> Expected then Exit(False);

  Inc(Scanner.Current);
  Result := True;
end;

function Peek: Char;
begin
  Result := Scanner.Current^;
end;

function PeekNext: Char;
begin
  if IsAtEnd then Exit(#0);
  Result := Scanner.Current[1];
end;

function MakeToken(const Typ: TTokenTyp): TToken;
begin
  Result.Typ := Typ;
  Result.Start := Scanner.Start;
  Result.Length := Integer(Scanner.Current - Scanner.Start);
  Result.Line := Scanner.Line;
end;

function ErrorToken(const Message: PChar): TToken;
begin
  Result.Typ := TOKEN_ERROR;
  Result.Start := Message;
  Result.Length := Integer(strlen(Message));
  Result.Line := Scanner.Line;
end;

procedure SkipWhiteSpace;
var
  ch: Char;
begin
  while True do
    begin
      ch := Peek;
      case ch of
        #9, #13, #32: Advance;  // Tab, Return, Space
        LineEnding:
          begin
            Inc(Scanner.Line);
            Advance;
          end;
        '/':
          begin
            if PeekNext = '/' then // till end of line
              while (Peek <> LineEnding) and not IsAtEnd do Advance
            else
              Break;
          end
        else
          Break;
      end;
    end;
end;

function CheckKeyWord(const Start, Length: Integer; Rest: PChar; Typ: TTokenTyp): TTokenTyp;
begin
  if ((Scanner.Current - Scanner.Start) = (Start + Length)) and
     (StrLComp(Scanner.Start + Start, Rest, Length) = 0) then // compare memory
    Exit(Typ);
  Result := TOKEN_IDENTIFIER;
end;

function IdentifierType: TTokenTyp;
begin
  case Scanner.Start[0] of
    'a': Exit(CheckKeyWord(1, 2, 'nd', TOKEN_AND));
    'b': Exit(CheckKeyWord(1, 4, 'reak', TOKEN_BREAK));
    'c': if Scanner.Current - Scanner.Start > 1 then
      case Scanner.Start[1] of
        'a': Exit(CheckKeyword(2, 2, 'se', TOKEN_CASE));
        'l': Exit(CheckKeyword(2, 3, 'ass', TOKEN_CLASS));
        'o': Exit(CheckKeyword(2, 6, 'ntinue', TOKEN_CONTINUE));
      end;
    'd': Exit(CheckKeyWord(1, 6, 'efault', TOKEN_DEFAULT));
    //'e': Exit(CheckKeyWord(1, 3, 'lse', TOKEN_ELSE));
    'e': if Scanner.Current - Scanner.Start > 1 then
           case Scanner.Start[1] of
             'l': Exit(CheckKeyword(2, 2, 'se', TOKEN_ELSE));
             'n': Exit(CheckKeyword(2, 4, 'sure', TOKEN_ENSURE));
           end;
    'f': if Scanner.Current - Scanner.Start > 1 then
           case Scanner.Start[1] of
             'a': Exit(CheckKeyword(2, 3, 'lse', TOKEN_FALSE));
             'o': Exit(CheckKeyword(2, 1, 'r', TOKEN_FOR));
             'u': Exit(CheckKeyword(2, 1, 'n', TOKEN_FUN));
           end;
    'i': Exit(CheckKeyWord(1, 1, 'f', TOKEN_IF));
    'n': Exit(CheckKeyWord(1, 2, 'il', TOKEN_NIL));
    'o': Exit(CheckKeyWord(1, 1, 'r', TOKEN_OR));
    'p': Exit(CheckKeyWord(1, 4, 'rint', TOKEN_PRINT));
    'r': Exit(CheckKeyWord(1, 5, 'eturn', TOKEN_RETURN));
    's': if Scanner.Current - Scanner.Start > 1 then
           case Scanner.Start[1] of
             'u': Exit(CheckKeyword(2, 3, 'per', TOKEN_SUPER));
             'w': Exit(CheckKeyword(2, 4, 'itch', TOKEN_SWITCH));
           end;
    't': if Scanner.Current - Scanner.Start > 1 then
           case Scanner.Start[1] of
             'h': Exit(CheckKeyword(2, 2, 'is', TOKEN_THIS));
             'r': Exit(CheckKeyword(2, 2, 'ue', TOKEN_TRUE));
           end;
    'v': Exit(CheckKeyWord(1, 2, 'ar', TOKEN_VAR));
    'w': Exit(CheckKeyWord(1, 4, 'hile', TOKEN_WHILE));
  end;
  Result := TOKEN_IDENTIFIER;
end;


function Identifier: TToken;
begin
  while IsAlpha(Peek) or IsDigit(Peek) do Advance;
  Result := MakeToken(IdentifierType);
end;

function Number: TToken;
begin
  while IsDigit(Peek) do Advance;

  // Look for a fractional part
  if (Peek = '.') and IsDigit(PeekNext) then
    begin
      Advance;   // Consume the '.'
      while IsDigit(Peek) do Advance;
    end;

  Result := MakeToken(TOKEN_NUMBER);
end;

function Strings: TToken;
var
  Typ: TTokenTyp = TOKEN_STRING;
  c: Char = #0;
begin
  while True do
    begin
      c := Advance;

      if c = '"' then Break;

      if IsAtEnd then
        if Scanner.OpenParens > 0 then
          Exit(ErrorToken('Expected closing paren ")".'))
        else
          Exit(ErrorToken('Unterminated string.'));

      if c = '$' then
        begin
          if Peek = '(' then
            begin
              Typ := TOKEN_INTERPOLATED;
              Scanner.OpenParens := 1;
              Advance;
              Break;
            end;
        end;

    end;

  Result := MakeToken(Typ);
end;

function ScanToken: TToken;
var
  c: char;
begin
  SkipWhitespace;

  Scanner.Start := Scanner.Current;

  if IsAtEnd then Exit(MakeToken(TOKEN_EOF));

  c := Advance;

  if IsAlpha(c) then Exit(Identifier);
  if IsDigit(c) then Exit(Number);

  case c of
    '(': // If we are inside an interpolated expression, count the unmatched "(".
      begin
        if Scanner.OpenParens > 0 then Inc(Scanner.OpenParens);
        Exit(MakeToken(TOKEN_LEFT_PAREN));
      end;

    ')': // If we are inside an interpolated expression, count the ")".
      if Scanner.OpenParens > 0 then
        begin
          Dec(Scanner.OpenParens);
          if Scanner.OpenParens = 0 then
             // This is the final ")", so the interpolation expression has ended.
            Exit(Strings)
          else
            Exit(MakeToken(TOKEN_RIGHT_PAREN));
        end
      else
        Exit(MakeToken(TOKEN_RIGHT_PAREN));

    '{': Exit(MakeToken(TOKEN_LEFT_BRACE));
    '}': Exit(MakeToken(TOKEN_RIGHT_BRACE));
    ':': Exit(MakeToken(TOKEN_COLON));
    ';': Exit(MakeToken(TOKEN_SEMICOLON));
    ',': Exit(MakeToken(TOKEN_COMMA));
    '.': Exit(MakeToken(TOKEN_DOT));
    '-': Exit(MakeToken(IfThen<TTokenTyp>(Match('>'), TOKEN_ARROW, TOKEN_MINUS)));
    '+': Exit(MakeToken(TOKEN_PLUS));
    '/': Exit(MakeToken(TOKEN_SLASH));
    '*': Exit(MakeToken(TOKEN_STAR));
    '!': Exit(MakeToken(IfThen<TTokenTyp>(Match('='), TOKEN_BANG_EQUAL, TOKEN_BANG)));
    '=': Exit(MakeToken(IfThen<TTokenTyp>(Match('='), TOKEN_EQUAL_EQUAL, TOKEN_EQUAL)));
    '<': Exit(MakeToken(IfThen<TTokenTyp>(Match('='), TOKEN_LESS_EQUAL, TOKEN_LESS)));
    '>': Exit(MakeToken(IfThen<TTokenTyp>(Match('='), TOKEN_GREATER_EQUAL, TOKEN_GREATER)));
    '?': Exit(MakeToken(TOKEN_QUESTION));

    '"': Exit(Strings);
  end;

  Result := ErrorToken('Unexpected character.');
end;


end.

