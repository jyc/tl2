include module type of Tl2Ast

type 'a result = [`Ok of 'a | `Error of string]

val parse : Lexing.lexbuf -> pos t result
val parse_file : string -> pos t result
val parse_string : string -> pos t result

val to_inline_code : 'a expr -> string option * string
val to_bin : 'a expr -> binop * 'a expr * 'a expr
val to_un : 'a expr -> unop * 'a expr
val to_bool : 'a expr -> bool
val to_int : 'a expr -> int
val to_real : 'a expr -> float
val to_string : 'a expr -> string
val to_ident : 'a expr -> string
val to_sigiled : 'a expr -> char * string
val to_vector : 'a expr -> string option * 'a expr list
val to_inline_complex : 'a expr -> 'a decl list
val to_code : 'a decl -> string option * string
val to_enum : 'a decl -> string list
val to_simple : 'a decl -> string * 'a expr 
val to_complex : 'a decl -> string list * 'a decl list

val simple : 'a decl list -> string -> 'a expr

val inline_code : 'a decl list -> string -> string option * string
val bin : 'a decl list -> string -> binop * 'a expr * 'a expr
val un : 'a decl list -> string -> unop * 'a expr
val bool : 'a decl list -> string -> bool
val int : 'a decl list -> string -> int
val real : 'a decl list -> string -> float
val string : 'a decl list -> string -> string
val ident : 'a decl list -> string -> string
val sigiled : 'a decl list -> string -> char * string
val vector : 'a decl list -> string -> string option * 'a expr list
val inline_complex : 'a decl list -> string -> 'a decl list
