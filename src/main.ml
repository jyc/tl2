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
  begin match Tl2.parse lexbuf with
  | `Ok ast -> 
    print_endline (Tl2.show (fun fmt _ -> Format.fprintf fmt "α") ast)
  | `Error s ->
    prerr_endline s ;
    exit 1
  end
