/**************************************************************************/
/*                                                                        */
/*   Typerex Tools                                                        */
/*                                                                        */
/*   Copyright 2011-2017 OCamlPro SAS                                     */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU General Public License version 3 described in the file       */
/*   LICENSE.                                                             */
/*                                                                        */
/**************************************************************************/

/* The parser definition */

%{
  open Ocpp_version

open Location
open Ocpp_types

let mkexp d =  { desc = d; eloc = symbol_rloc() }
let mkrhs rhs pos = mkloc rhs (rhs_loc pos)
let mkoperator name pos =
  let loc = rhs_loc pos in
  { desc = Pexp_ident(mkloc name loc); eloc = loc }
let mkinfix arg1 name arg2 =
  mkexp(Pexp_apply(mkoperator name 2, [arg1;arg2]))

%}

/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
/* %token PARSER */
%token PLUS
%token PLUSDOT
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Ocpp_version.Location.t> COMMENT

%token EOL
/*
%token OCPP_INCLUDE
%token OCPP_DEFINE
%token OCPP_DEFUN
%token OCPP_UNDEF
%token OCPP_IFDEF
%token OCPP_IFNDEF
%token OCPP_IF
%token OCPP_ELSE
%token OCPP_ELIF
%token OCPP_ENDIF
%token OCPP_ERROR
%token OCPP_WARNING
%token OCPP_DBL_LBRACE
%token OCPP_DBL_RBRACE
*/

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT  /* expr (e OP e OP e) */
%left     INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT


/* Entry points */

%start ocpp_expr
%type <Ocpp_types.expression> ocpp_expr
%%

/* Entry points */

ocpp_version:
  | INT DOT ocpp_version  { string_of_int $1 :: $3 }
  | INT { [ string_of_int $1 ] }
;

ocpp_constant:
    INT                                         { Int $1 }
  | STRING                                      { String $1 }
  | TRUE                                        { Int 1 }
  | FALSE                                       { Int 0 }
;

ocpp_expr:
  | LIDENT
      { mkexp(Pexp_ident (mkrhs ($1) 1)) }
  | UIDENT
      { mkexp(Pexp_uident (mkrhs ($1) 1)) }
  | ocpp_constant
      { mkexp(Pexp_constant $1) }
  | LPAREN ocpp_expr RPAREN
      { $2 }
  | ocpp_expr PLUS ocpp_expr
      { mkinfix $1 "+" $3 }
  | ocpp_expr MINUS ocpp_expr
      { mkinfix $1 "-" $3 }
  | ocpp_expr STAR ocpp_expr
      { mkinfix $1 "*" $3 }

  | LIDENT LPAREN ocpp_expr RPAREN
      { mkexp(Pexp_apply(mkexp(Pexp_ident(mkrhs ($1) 1)), [ $3 ])) }
  | INT DOT ocpp_version
    {
      let version =
        String (String.concat "." (string_of_int $1 :: $3))
      in
      let version = mkexp(Pexp_constant version) in
      mkexp(Pexp_apply(mkexp(Pexp_ident(mkrhs ("version") 1)),
          [ version ])) }

  | ocpp_expr EQUAL ocpp_expr
      { mkinfix $1 "=" $3 }
  | ocpp_expr LESS ocpp_expr
      { mkinfix $1 "<" $3 }
  | ocpp_expr GREATER ocpp_expr
      { mkinfix $1 ">" $3 }
  | ocpp_expr INFIXOP0 ocpp_expr
      { mkinfix $1 $2 $3 }

  | BANG ocpp_expr
      { mkexp(Pexp_apply(mkoperator "!" 1, [$2])) }
  | PREFIXOP ocpp_expr
      { mkexp(Pexp_apply(mkoperator $1 1, [$2])) }

  | ocpp_expr BARBAR ocpp_expr
      { mkinfix $1 "||" $3 }
  | ocpp_expr AMPERAMPER ocpp_expr
      { mkinfix $1 "&&" $3 }
;

%%
