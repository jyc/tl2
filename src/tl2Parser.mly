%{
	open Tl2Ast
%}

%token <bool> BOOL
%token <int> INT
%token <float> REAL
%token <string> STRING
%token <string option * string> CODE

%token <string> IDENT
%token <char * string> SIGILED

%token COLON
%token COMMA
%token DOT

%token LPN
%token RPN
%token LBR
%token RBR
%token LSQ
%token RSQ

%token ADD
%token SUB
%token MUL
%token DIV
%token EXP
%token MOD

%token EOF

%left ADD SUB
%left MUL DIV
%left EXP MOD

%start <Tl2Ast.pos Tl2Ast.t> main
%%

expr:
	| c = CODE
		{ let (tag, code) = c in
      InlineCode (($symbolstartpos, $endpos), tag, code) }
	| a = expr; ADD; b = expr
	  { Bin (($symbolstartpos, $endpos), Add, a, b) }
	| a = expr; SUB; b = expr
	  { Bin (($symbolstartpos, $endpos), Sub, a, b) }
	| a = expr; MUL; b = expr
	  { Bin (($symbolstartpos, $endpos), Mul, a, b) }
	| a = expr; DIV; b = expr
	  { Bin (($symbolstartpos, $endpos), Div, a, b) }
	| a = expr; EXP; b = expr
	  { Bin (($symbolstartpos, $endpos), Exp, a, b) }
	| a = expr; MOD; b = expr
	  { Bin (($symbolstartpos, $endpos), Mod, a, b) }
	| SUB; a = expr
	  { Un (($symbolstartpos, $endpos), Neg, a) }
	| b = BOOL
	  { Bool (($symbolstartpos, $endpos), b) }
	| i = INT
		{ Int (($symbolstartpos, $endpos), i) }
	| r = REAL
	  { Real (($symbolstartpos, $endpos), r) }
	| s = STRING
	  { String (($symbolstartpos, $endpos), s) }
	| id = IDENT
		{ Ident (($symbolstartpos, $endpos), id) }
  | sg = SIGILED
    { let (sigil, id) = sg in
      Sigiled (($symbolstartpos, $endpos), sigil, id) }

  | tag = IDENT?; LSQ; es = separated_list(COMMA, expr); RSQ
	  { Vector (($symbolstartpos, $endpos), tag, es) }

  | LBR; b = block; RBR
    { InlineComplex (($symbolstartpos, $endpos), b) }

	| LPN; e = expr; RPN
	  { e }

decl:
	| c = CODE
		{ let (tag, code) = c in
      Code (($symbolstartpos, $endpos), tag, code) }
	| ls = separated_nonempty_list(COMMA, IDENT); DOT
	  { Enum (($symbolstartpos, $endpos), ls) }
	| id = IDENT; COLON; e = expr
	  { Simple (($symbolstartpos, $endpos), id, e) }
	| name = nonempty_list(IDENT); LBR; b = block; RBR
	  { Complex (($symbolstartpos, $endpos), name, b) }

block:
	| ds = list(decl)
	  { ds }

main:
	| b = block; EOF
    { b }

(* vim: set ts=2 sts=2 sw=2 noexpandtab : *)
