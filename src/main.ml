open Lexing
open Printf

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let () =
  let filename = Sys.argv.(1) in
  let inp = open_in filename in
  let lexbuf = Lexing.from_channel inp in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
  try
    let ast = Tl2.parse lexbuf in
    print_endline (Tl2.show (fun fmt _ -> Format.fprintf fmt "α") ast)
  with
  | Tl2Lexer.LexError msg ->
    fprintf stderr "%s: Lexical error: %s\n" (print_position lexbuf) msg ;
    exit 1
  | Tl2Parser.Error ->
    fprintf stderr "%s: Syntax error.\n" (print_position lexbuf) ;
    exit 1
