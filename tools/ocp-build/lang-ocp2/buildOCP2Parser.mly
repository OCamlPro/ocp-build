/******************************************************************************/
/*                                                                            */
/*                          TypeRex OCaml Tools                               */
/*                                                                            */
/*                               OCamlPro                                     */
/*                                                                            */
/*    Copyright 2011-2012 OCamlPro                                            */
/*    All rights reserved.  See accompanying files for the terms under        */
/*    which this file is distributed. In doubt, contact us at                 */
/*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         */
/*                                                                            */
/******************************************************************************/

%{

(* Precedence of operators is the same as in Javascript !! *)

  open StringCompat
  open BuildValue.Types
open BuildOCP2Tree

let symb_loc () =
  let l = {
    loc_begin = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
  } in
  l

(* Add locations ! *)
let mkstmt stmt_expr = { stmt_expr; stmt_loc = symb_loc (); }
let mkexp exp_expr = { exp_expr; exp_loc = symb_loc (); }
let mkinfix op exp = mkexp (ExprCall(mkexp (ExprIdent op), exp))
%}

%token EOF

%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <char> CHAR

%token SEMI
%token COMMA
%token DOT

%token EQUAL
%token PLUSEQUAL
%token MINUSEQUAL

%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE

%token BANG

%token GREATER
%token GREATEREQUAL
%token LESS
%token LESSEQUAL
%token LESSGREATER
%token EQUALEQUAL
%token BANGEQUAL
%token AMPERAMPER
%token BARBAR

%token BEGIN
%token END
%token FUNCTION
%token RETURN

%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE

%token OR
%token AMPER
%token XOR
%token PLUS
%token MINUS
%token STAR
%token DIVIDE
%token PERCENT

%token LESSLESS
%token GREATERGREATER


%token INCLUDE
%token IMPORT

%token TRY
%token CATCH

%token FOR
%token IN

%token <string> IDENT

%start main
%type <BuildOCP2Tree.statement> main

%%

main:
  statements EOF { $1 }
;

statements:
| statement_semi statements { mkstmt( StmtSeq($1, $2) ) }
|                           { mkstmt( StmtEmpty ) }
;

statement_semi:
| statement                         { $1 }
| statement_block                   { $1 }
;

statement:
| statement_no_semi SEMI               { $1 }
| INCLUDE expr SEMI                  { mkstmt( StmtInclude ($2,
                                                              mkstmt StmtEmpty) ) }
| INCLUDE expr ELSE statement_semi    { mkstmt( StmtInclude ($2, $4) ) }
| IF LPAREN expr RPAREN statement_no_if ELSE statement_semi
                     { mkstmt( StmtIfthenelse($3, $5, $7)) }
| IF LPAREN expr RPAREN statement_no_if_semi
                     { mkstmt( StmtIfthenelse($3, $5, mkstmt StmtEmpty) ) }
| FUNCTION lhs_expr LPAREN ident_args RPAREN LBRACE statements RBRACE
                         { mkstmt (StmtAssign ($2,
                                               mkexp( ExprFunction($4,$7) ))) }
;

statement_no_if:
| statement_no_semi         { $1 }
| statement_block           { $1 }
;

statement_no_if_semi:
| statement_no_semi SEMI    { $1 }
| statement_block           { $1 }
;

statement_block_no_try:
| LBRACE statements RBRACE  { $2 }
| BEGIN statements END      { mkstmt( StmtBlock $2 ) }
| FOR IDENT IN expr statement_block_no_try
                            { mkstmt (StmtFor($2,$4,$5)) }
;

statement_block:
| statement_block_no_try           { $1 }
| TRY statement_block_no_try catch catches
                     { mkstmt (StmtTry($2, $3 :: $4) ) }
;

catch:
| CATCH LPAREN STRING COMMA IDENT RPAREN statement_block_no_try
    { ($3, ($5,$7) ) }
;

catches:
|   {  []  }
| catch catches { $1 :: $2 }
;

statement_no_semi:
| lhs_expr assignment_op expr  {
  let lhs = $1 in
  let v = $3 in
  let v = match $2 with
    | "=" -> v
    | "+=" -> mkinfix prim_add_name [lhs; v]
    | "-=" -> mkinfix prim_sub_name [lhs; v]
    | _ -> assert false
  in
  mkstmt( StmtAssign(lhs,v) ) }
| simple_expr               { mkstmt( StmtExpr $1) }
| RETURN expr               { mkstmt( StmtReturn (Some $2) ) }
| RETURN                    { mkstmt( StmtReturn None) }
| IMPORT expr               { mkstmt( StmtImport $2 ) }
;

assignment_op:
| EQUAL                     { "=" }
| PLUSEQUAL                 { "+=" }
| MINUSEQUAL                { "-=" }
;

expr:
| tupled_expr               { $1 }
| tupled_expr COMMA expr_comma_expr  { mkexp (ExprTuple ($1 :: $3)) }
;

expr_comma_expr:
| tupled_expr               { [$1] }
| tupled_expr COMMA expr_comma_expr { $1 :: $3 }
;

tupled_expr:
| logicalOR_expr { $1 }
;

lhs_expr:
| IDENT                    { mkexp ( ExprIdent $1 ) }
| lhs_expr DOT IDENT
    { mkexp ( ExprField($1, mkexp (ExprValue (VString $3))) ) }
| lhs_expr LBRACKET expr RBRACKET
        { mkexp (ExprField($1, $3)) }
;

logicalOR_expr:
| logicalAND_expr { $1 }
| logicalOR_expr BARBAR logicalAND_expr { mkinfix prim_or_name [$1;$3] }
;

logicalAND_expr:
| bitwiseOR_expr { $1 }
| logicalAND_expr AMPERAMPER bitwiseOR_expr { mkinfix prim_and_name [$1;$3] }
;

bitwiseOR_expr:
| bitwiseXOR_expr { $1 }
| bitwiseOR_expr OR bitwiseXOR_expr { mkinfix prim_or_name [$1;$3] }
    ;

bitwiseXOR_expr:
| bitwiseAND_expr { $1 }
| bitwiseXOR_expr XOR bitwiseAND_expr { mkinfix prim_xor_name [$1;$3] }
 ;

bitwiseAND_expr:
| equality_expr { $1 }
| bitwiseAND_expr AMPER equality_expr { mkinfix prim_and_name [$1;$3] }
    ;

equality_expr:
| relational_expr { $1 }
| equality_expr equality_op relational_expr { mkinfix $2 [$1;$3] }
    ;

relational_expr:
| shift_expr { $1 }
| relational_expr relational_op shift_expr { mkinfix $2 [$1;$3] }
    ;

shift_expr:
| additive_expr { $1 }
| shift_expr shift_op additive_expr { mkinfix $2 [$1;$3] }
;

additive_expr:
| multiplicative_expr { $1 }
| additive_expr additive_op multiplicative_expr { mkinfix $2 [$1;$3] }
    ;

multiplicative_expr:
| unary_expr { $1 }
| multiplicative_expr multiplicative_op unary_expr { mkinfix $2 [$1;$3] }
    ;

unary_expr:
| unary_op unary_expr               { mkinfix $1 [$2] }
| no_operator_expr                  { $1 }
;

no_operator_expr:
| simple_expr               { $1 }
| LBRACE ident_equal_exprs_maybe_empty RBRACE { mkexp (ExprRecord $2 ) }
;

ident_equal_exprs_maybe_empty:
|                       { [] }
| IDENT EQUAL expr ident_equal_exprs { ($1,$3) :: $4 }
;

ident_equal_exprs:
|                       { [] }
| SEMI                  { [] }
| SEMI IDENT EQUAL expr ident_equal_exprs { ($2,$4) :: $5 }
;

simple_expr:
| lhs_expr                  { $1 }
| simpler_expr              { $1 }
;

simpler_expr:
| LPAREN expr RPAREN        { $2 }
| simple_expr LPAREN expr_args RPAREN     { mkexp ( ExprCall($1,$3) ) }
| simpler_expr DOT IDENT
    { mkexp (ExprField ($1, mkexp (ExprValue (VString $3)))) }
| FUNCTION LPAREN ident_args RPAREN LBRACE statements RBRACE
    { mkexp( ExprFunction($3,$6) ) }
| TRUE                      { mkexp (ExprValue (VBool true)) }
| FALSE                     { mkexp (ExprValue (VBool false)) }
| STRING                    { mkexp (ExprValue (VString $1)) }
| INT                       { mkexp (ExprValue (VInt $1)) }
| LBRACKET expr_semi_exprs_maybe_empty RBRACKET { mkexp (ExprList $2) }
| DOT                       { mkexp  ExprEnv }
;

expr_semi_exprs_maybe_empty:
|                           { [] }
| expr expr_semi_exprs { $1 :: $2 }
;
expr_semi_exprs:
| { [] }
| SEMI  { [] }
| SEMI expr expr_semi_exprs { $2 :: $3 }
    ;

comma_exprs:
|                           { [] }
| COMMA tupled_expr comma_exprs    { $2 :: $3 }
;

expr_args:
|                           { [] }
| tupled_expr comma_exprs          { $1 :: $2 }
;

comma_idents:
|                            { [] }
| COMMA IDENT comma_idents   { $2 :: $3 }
;

ident_args:
|                             { [] }
| IDENT comma_idents          { $1 :: $2 }
;
unary_op:
  | PLUS        { prim_identity_name }
  | MINUS       { prim_not_name }
  | BANG       { prim_not_name }
;

multiplicative_op:
  | STAR        { prim_mul_name }
  | DIVIDE      { prim_div_name }
  | PERCENT     { prim_mod_name }
;

additive_op:
  | PLUS        { prim_add_name }
  | MINUS       { prim_sub_name }
;

shift_op:
  | LESSLESS              { prim_lsl_name }
  | GREATERGREATER        { prim_lsr_name }
;

relational_op:
  | LESS                  { prim_lessthan_name  (* "<" *) }
  | GREATER               { prim_greaterthan_name (* ">" *) }
  | LESSEQUAL             { prim_lessequal_name (* "<=" *) }
  | GREATEREQUAL          { prim_greaterequal_name (* ">=" *) }
;
equality_op:
  | EQUALEQUAL            { prim_equal_name (* "==" *) }
  | EQUAL            { prim_equal_name (* "==" *) }
  | BANGEQUAL             { prim_notequal_name (* "<>" *) }
  | LESSGREATER             { prim_notequal_name (* "<>" *) }
;


/*
opt_semi:
| SEMI { () }
|      { () }
;
*/
%%
