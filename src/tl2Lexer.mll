{
  open Lexing
  open Tl2Parser
  exception LexError of string

  let lex_error s =
    raise (LexError s)

  let char_of_hex_escape a b =
    let int_of_digit c =
      let c' = Char.code c in
      if c' >= 48 && c' <= 57 then c' - 48 (* 0-9 *)
      else if c' >= 97 && c' <= 102 then 10 + c' - 97 (* a-f *)
      else if c' >= 65 && c' <= 70 then 10 + c' - 65 (* A-F *)
      else lex_error "Invalid numeric character escape."
    in
    let a' = int_of_digit a in
    let b' = int_of_digit b in
    let code = a' * 16 + b' in
    Char.chr code

  let line_col lexbuf =
    let pos = lexbuf.lex_start_p in
    let line_num = pos.pos_lnum in
    let col_num = pos.pos_cnum - pos.pos_bol + 1 in
    line_num, col_num
}

let whitespace = [' ' '\t']
let newline = '\n'
let not_newline = [^ '\n']

let integer = '0' | ['1'-'9']['0'-'9''_']*
let real = integer '.' integer?

let ident = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*

rule read = parse
  | whitespace+
    { read lexbuf }

  | "true"
    { BOOL true }
  | "false"
    { BOOL false }
  | integer as i
    { INT (int_of_string i) }
  | real as r
    { REAL (float_of_string r) }
  | '"'
    { STRING (read_string (Buffer.create 17) lexbuf) }
	| "``"
    { CODE (None, read_code (Buffer.create 17) lexbuf) }
	| (ident as tag) "``"
    { CODE (Some tag, read_code (Buffer.create 17) lexbuf) }

  | ident as id
    { IDENT id }
  | (('!' | '@' | '$' | '%' | '&') as sigil) (ident as id)
    { SIGILED (sigil, id) }

  | ':' { COLON }
  | ',' { COMMA }
  | '.' { DOT }

  | '(' { LPN }
  | ')' { RPN }
  | '{' { LBR }
  | '}' { RBR }
  | '[' { LSQ }
  | ']' { RSQ }

  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | '/' { DIV }
  | '^' { EXP }
  | '%' { MOD }

  | '#' not_newline* newline
    { new_line lexbuf ; read lexbuf }

  | newline
    { new_line lexbuf ; read lexbuf }
  | eof { EOF }

  | _
    { lex_error ("Unexpected character: '" ^ Lexing.lexeme lexbuf ^ "'.") }

and read_string buf =
  parse
  | '"'       { Buffer.contents buf }
  | '\\' '"'  { Buffer.add_char buf '"' ; read_string buf lexbuf }
  | '\\' '/'  { Buffer.add_char buf '/' ; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\' ; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b' ; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012' ; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n' ; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r' ; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t' ; read_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\'' ; read_string buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '"' ; read_string buf lexbuf }
  | '\\' 'x' (_ as a) (_ as b)
    { Buffer.add_char buf (char_of_hex_escape a b) ; read_string buf lexbuf }
  | [^ '"' '\\' '\n']* '\n'
    { Buffer.add_string buf (Lexing.lexeme lexbuf) ;
      new_line lexbuf ;
      read_string buf lexbuf }
  | [^ '"' '\\' '\n']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf) ;
      read_string buf lexbuf }
  (* Escape newline and subsequent whitespace like in OCaml. *)
  | '\\' newline whitespace*
    { new_line lexbuf ;
      read_string buf lexbuf }
  | '\\' (_ as x)
    { Buffer.add_char buf x ; read_string buf lexbuf }
  | _ { lex_error ("Illegal string character: '" ^ Lexing.lexeme lexbuf ^ "'.") }
  | eof { lex_error "Unterminated string literal." }

and read_code buf =
  parse
  | "''"
	  { Buffer.contents buf }
  | '\n'
    { Buffer.add_char buf '\n' ;
      new_line lexbuf ;
      read_code buf lexbuf }
  | _ as x
    { Buffer.add_char buf x ; read_code buf lexbuf }
  | eof { lex_error "Unterminated code literal." }
