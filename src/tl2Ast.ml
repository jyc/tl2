type binop =
  | Add | Sub
  | Mul | Div
  | Exp | Mod
	[@@deriving show]

type unop =
  | Neg
	[@@deriving show]

type 'a expr =
  | InlineCode of 'a * string option * string

  | Bin of 'a * binop * 'a expr * 'a expr
  | Un of 'a * unop * 'a expr

  | Bool of 'a * bool
  | Int of 'a * int
  | Real of 'a * float
  | String of 'a * string

  | Ident of 'a * string
  | Sigiled of 'a * char * string

  | Vector of 'a * string option * 'a expr list
  | InlineComplex of 'a * 'a decl list
	[@@deriving show]
and 'a decl =
  | Code of 'a * string option * string
  | Enum of 'a * string list
  | Simple of 'a * string * 'a expr
  | Complex of 'a * string list * 'a decl list
	[@@deriving show]
and 'a t = 'a decl list
	[@@deriving show]

let note_of_expr = function
  | InlineCode (a, _, _) -> a
  | Bin (a, _, _, _) -> a
  | Un (a, _, _) -> a
  | Bool (a, _) -> a
  | Int (a, _) -> a
  | Real (a, _) -> a
  | String (a, _) -> a
  | Ident (a, _) -> a
  | Sigiled (a, _, _) -> a
  | Vector (a, _, _) -> a
  | InlineComplex (a, _) ->a

let note_of_decl = function
  | Code (a, _, _) -> a
  | Enum (a, _) -> a
  | Simple (a, _, _) -> a
  | Complex (a, _, _) -> a

type pos = Lexing.position * Lexing.position
