open Printf

type 'a result = [`Ok of 'a | `Error of string]

let show pos =
  let open Lexing in
  let file =
    if pos.pos_fname = "" then ""
    else pos.pos_fname ^ ":"
  in
  let inside =
    if pos.pos_lnum = -1 then ""
    else sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in
  sprintf "%s%s" file inside

let parse lexbuf =
  try
    `Ok (Tl2Parser.main Tl2Lexer.read lexbuf)
  with
  | Tl2Parser.Error -> `Error (sprintf "Parse error: %s" (show lexbuf.Lexing.lex_curr_p))
  | Tl2Lexer.LexError s -> `Error (Printf.sprintf "Lex error: %s" s)

let parse_file filename =
  let open Lexing in
  let inp = open_in filename in
  let lexbuf = Lexing.from_channel inp in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
  let ast = parse lexbuf in
  close_in inp ;
  ast

let parse_string s =
  let open Lexing in
  let lexbuf = Lexing.from_string s in
  let ast = parse lexbuf in
  ast

include Tl2Ast

let (@.) f g x = f (g x)

let to_inline_code = function
  | InlineCode (_, tag, c) -> (tag, c)
  | _ -> invalid_arg "Expected inline code."

let to_bin = function
  | Bin (_, op, l, r) -> (op, l, r)
  | _ -> invalid_arg "Expected bin."

let to_un = function
  | Un (_, op, x) -> (op, x)
  | _ -> invalid_arg "Expected un."

let to_bool = function
  | Bool (_, x) -> x
  | _ -> invalid_arg "Expected un."

let to_int = function
  | Int (_, x) -> x
  | _ -> invalid_arg "Expected int."

let to_real = function
  | Real (_, x) -> x
  | _ -> invalid_arg "Expected real."

let to_string = function
  | String (_, x) -> x
  | _ -> invalid_arg "Expected string."

let to_ident = function
  | Ident (_, x) -> x
  | _ -> invalid_arg "Expected ident."

let to_sigiled = function
  | Sigiled (_, s, x) -> (s, x)
  | _ -> invalid_arg "Expected sigiled."

let to_vector = function
  | Vector (_, tag, es) -> (tag, es)
  | _ -> invalid_arg "Expected vector."

let to_inline_complex = function
  | InlineComplex (_, decls) -> decls
  | _ -> invalid_arg "Expected inline complex."

let to_code = function
  | Code (_, tag, c) -> (tag, c)
  | _ -> invalid_arg "Expected code."

let to_enum = function
  | Enum (_, es) -> es
  | _ -> invalid_arg "Expected enum."

let to_simple = function
  | Simple (_, k, v) -> (k, v)
  | _ -> invalid_arg "Expected simple."

let to_complex = function
  | Complex (_, ks, decls) -> (ks, decls)
  | _ -> invalid_arg "Expected complex."

let simple decls key =
  let rec lookup = function
    | [] -> raise Not_found
    | Tl2Ast.Simple (_, k, v) :: rest when k = key -> v
    | _ :: rest -> lookup rest
  in lookup decls

let inline_code decls = to_inline_code @. simple decls
let bin decls = to_bin @. simple decls
let un decls = to_un @. simple decls
let bool decls = to_bool @. simple decls
let int decls = to_int @. simple decls
let real decls = to_real @. simple decls
let string decls = to_string @. simple decls
let ident decls = to_ident @. simple decls
let sigiled decls = to_sigiled @. simple decls
let vector decls = to_vector @. simple decls
let inline_complex decls = to_inline_complex @. simple decls
