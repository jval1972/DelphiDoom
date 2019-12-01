unit vxe_script;

interface

uses
  Classes;

//
// JVAL
//
// Script Engine
//

// TScriptEngine
const
  MAX_STRING_SIZE = 1024;

type
  TScriptEngine = class
  private
    sc_String: PChar;
    sc_Integer: integer;
    sc_Line: integer;
    sc_End: boolean;
    fNewLine: boolean;
    fBracketLevel: integer;
    fParenthesisLevel: integer;
    ScriptBuffer: PChar;
    ScriptPtr: PChar;
    ScriptEndPtr: PChar;
    StringBuffer: array [0..MAX_STRING_SIZE] of char;
    ScriptSize: integer;
    AlreadyGot: boolean;
    lasterror: string;
  protected
    function fToken: string;
  public
    constructor Create(const tx: string); virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure SetText(const tx: string); virtual;
    procedure ScriptError(const err: string); overload;
    procedure ScriptError(const Fmt: string; const Args: array of const); overload;
    function GetString: boolean;
    function GetStringEOL: string;
    procedure MustGetString;
    procedure MustGetStringName(const name: string);
    function GetInteger: boolean;
    procedure MustGetInteger;
    procedure UnGet;
    function MatchString(const strs: TStringList): integer; overload;
    function MatchString(const str: string): boolean; overload;
    function MatchPosString(const str: string): boolean;
    function MustMatchString(strs: TStringList): integer;
    function Compare(const txt: string): boolean;
    function GetLastError: string;
    property _Integer: integer read sc_Integer;
    property _String: string read fToken;
    property _Finished: boolean read sc_End;
    property _Line: integer read sc_Line;
    property NewLine: boolean read fNewLine;
    property BracketLevel: integer read fBracketLevel;
    property ParenthesisLevel: integer read fParenthesisLevel;
  end;


implementation

uses
  SysUtils;

const
  ASCII_QUOTE = '"';
  ASCII_COMMENT1 = '/';
  ASCII_COMMENT = $2F2F; // = '//'


// TScriptEngine
constructor TScriptEngine.Create(const tx: string);
begin
  Inherited Create;
  lasterror := '';
  fBracketLevel := 0;
  fParenthesisLevel := 0;
  fNewLine := false;
  Clear;
  SetText(tx);
end;

destructor TScriptEngine.Destroy;
begin
  Clear;
  Inherited;
end;

function TScriptEngine.fToken: string;
begin
  result := sc_String;
end;

procedure TScriptEngine.Clear;
begin
  if ScriptSize > 0 then
    FreeMem(ScriptBuffer, ScriptSize);

  ScriptBuffer := nil;
  ScriptSize := 0;
  ScriptPtr := ScriptBuffer;
  ScriptEndPtr := ScriptPtr + ScriptSize;
  sc_Line := 1;
  sc_End := false;
  sc_String := @StringBuffer[0];
  AlreadyGot := false;
end;

procedure TScriptEngine.SetText(const tx: string);
var
  p: Pointer;
  size: integer;
begin

  size := Length(tx);
  GetMem(p, size);
  Move(tx[1], p^, size);

  Clear;
  ScriptBuffer := p;
  ScriptSize := size;
  ScriptPtr := ScriptBuffer;
  ScriptEndPtr := ScriptPtr + ScriptSize;
  sc_Line := 1;
  sc_End := false;
  sc_String := @StringBuffer[0];
  AlreadyGot := false;
end;

procedure TScriptEngine.ScriptError(const err: string);
begin
  lasterror := err;
end;

procedure TScriptEngine.ScriptError(const fmt: string; const Args: array of const);
var
  s: string;
begin
  s := Format(fmt, Args);
  ScriptError(s);
end;


procedure TScriptEngine.MustGetString;
begin
  if not GetString then
    ScriptError('TScriptEngine.MustGetString(): Missing string at Line %d', [sc_Line]);
end;

procedure TScriptEngine.MustGetStringName(const name: string);
begin
  MustGetString;
  if not Compare(name) then
    ScriptError('TScriptEngine.MustGetStringName(): "%s" expected at Line %d', [name, sc_Line]);
end;

function TScriptEngine.GetInteger: boolean;
var
  code: integer;
begin
  if GetString then
  begin
    val(sc_String, sc_Integer, code);
    if code <> 0 then
    begin
      ScriptError(
          'TScriptEngine.GetInteger(): Bad numeric constant "%s" at Line %d',
          [sc_String, sc_Line]);
      result := false;
      exit;
    end;
    result := true;
  end
  else
    result := true;
end;

procedure TScriptEngine.MustGetInteger;
begin
  if not GetInteger then
    ScriptError('TScriptEngine.MustGetInteger(): Missing integer at Line %d', [sc_Line]);
end;

procedure TScriptEngine.UnGet;
// Assumes there is a valid string in sc_String.
begin
  AlreadyGot := true;
end;

function TScriptEngine.MatchString(const str: string): boolean;
begin
  result := Compare(str);
end;

function TScriptEngine.MatchString(const strs: TStringList): integer;
// Returns the index of the first match to sc_String from the passed
// array of strings, or -1 if not found.
var
  i: integer;
begin
  for i := 0 to strs.Count - 1 do
  begin
    if Compare(strs.Strings[i]) then
    begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;

function TScriptEngine.MatchPosString(const str: string): boolean;
begin
  result := Pos(UpperCase(str), strupper(sc_String)) > 0;
end;

function TScriptEngine.MustMatchString(strs: TStringList): integer;
var
  i: integer;
begin
  i := MatchString(strs);
  if i = -1 then
    ScriptError('TScriptEngine.MustMatchString(): List'#13#10'%s'#13#10'expected at Line %d',  [strs.Text, sc_Line]);

  result := i;
end;

function TScriptEngine.Compare(const txt: string): boolean;
begin
  result := UpperCase(txt) = strupper(sc_String);
end;

function TScriptEngine.GetLastError: string;
begin
  result := lasterror;
end;

function TScriptEngine.GetString: boolean;
var
  txt: PChar;
  foundToken: boolean;
begin
  if AlreadyGot then
  begin
    AlreadyGot := false;
    result := true;
    exit;
  end;

  fNewLine := false;
  foundToken := false;
  if ScriptPtr >= ScriptEndPtr then
  begin
    sc_End := true;
    result := false;
    exit;
  end;

  while not foundToken do
  begin
    while (ScriptPtr^ <= Chr(32)) or (ScriptPtr^ in ['{', '}', '(', ')', ',']) do
    begin
      if ScriptPtr >= ScriptEndPtr then
      begin
        sc_End := true;
        result := false;
        exit;
      end;
      if ScriptPtr^ = '{' then
        inc(fBracketLevel)
      else if ScriptPtr^ = '}' then
      begin
        dec(fBracketLevel);
        if fBracketLevel < 0 then
          ScriptError('TScriptEngine.GetString(): Closing bracket "}" found at line %d without opening bracket "{"', [sc_Line]);
      end
      else if ScriptPtr^ = '(' then
        inc(fParenthesisLevel)
      else if ScriptPtr^ = ')' then
      begin
        dec(fParenthesisLevel);
        if fParenthesisLevel < 0 then
          ScriptError('TScriptEngine.GetString(): Closing parenthesis ")" found at line %d without opening parenthesis "("', [sc_Line]);
      end
      else if ScriptPtr^ = Chr(10) then
      begin
        inc(sc_Line);
        fNewLine := true;
      end;
      inc(ScriptPtr);
    end;

    if ScriptPtr >= ScriptEndPtr then
    begin
      sc_End := true;
      result := false;
      exit;
    end;

    if ScriptPtr^ <> ASCII_COMMENT1 then
    begin // Found a token
      foundToken := true;
    end
    else
    begin // Skip comment

      if ScriptPtr >= ScriptEndPtr then
      begin
        sc_End := true;
        result := false;
        exit;
      end;
      inc(ScriptPtr);
      if ScriptPtr^ = ASCII_COMMENT1 then
      begin
        while (ScriptPtr^ <> Chr(13)) and (ScriptPtr^ <> Chr(10)) do
        begin
          if ScriptPtr >= ScriptEndPtr then
          begin
            sc_End := true;
            result := false;
            exit;
          end;
          inc(ScriptPtr);
        end;
        inc(sc_Line);
        fNewLine := true;
      end;
    end;
  end;

  txt := sc_String;
  if ScriptPtr^ = ASCII_QUOTE then
  begin // Quoted string
    inc(ScriptPtr);
    while ScriptPtr^ <> ASCII_QUOTE do
    begin
      txt^ := ScriptPtr^;
      inc(txt);
      inc(ScriptPtr);
      if (ScriptPtr = ScriptEndPtr) or
         (txt = @sc_String[MAX_STRING_SIZE - 1]) then
        break;
    end;
    inc(ScriptPtr);
  end
  else
  begin // Normal string
    while (ScriptPtr^ > Chr(32)) and (ScriptPtr < ScriptEndPtr) and
          (PWord(ScriptPtr)^ <> ASCII_COMMENT) and (not (ScriptPtr^ in ['{', '}', '(', ')', ','])) do
    begin
      txt^ := ScriptPtr^;
      inc(txt);
      inc(ScriptPtr);
      if (ScriptPtr = ScriptEndPtr) or
         (txt = @sc_String[MAX_STRING_SIZE - 1]) then
        break;

    end;
  end;
  txt^ := Chr(0);
  result := true;
end;

function TScriptEngine.GetStringEOL: string;
begin
  result := '';
  if not GetString then
    exit;

  if fNewLine then
  begin
    AlreadyGot := true;
    exit;
  end;
  result := sc_string;
  while not sc_End and not fNewLine do
  begin
    GetString;
    if fNewLine then
    begin
      AlreadyGot := true;
      exit;
    end;
    result := result + ' ' + sc_string;
  end;
end;

end.


