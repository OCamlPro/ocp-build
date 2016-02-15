type token =
  | STRING of (string)
  | INT of (int)
  | EOF
  | FLOAT of (float)
  | CHAR of (char)
  | SEMI
  | BEGIN
  | END
  | IDENT of (string)
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | PLUSEQUAL
  | MINUSEQUAL
  | TRUE
  | FALSE
  | INCLUDE
  | INCLUDED of (BuildOCPTree.statement list)
  | OBJECTS
  | LIBRARY
  | SYNTAX
  | PROGRAM
  | CONFIG
  | RULES
  | BANG
  | EQUAL
  | LPAREN
  | RPAREN
  | TYPE
  | USE
  | PACK
  | IF
  | THEN
  | ELSE
  | NOT
  | COND_OR
  | COND_AND
  | SYNTAXES
  | CAMLP4
  | CAMLP5
  | TEST
  | PERCENT
  | GREATER
  | GREATEREQUAL
  | LESS
  | LESSEQUAL

open Parsing;;
let _ = parse_error;;
# 15 "ocp-build/buildOCPParser.mly"

open BuildValue.Types
open BuildOCPTree
open BuildOCPTypes
  (* TODO: location of the type in ocamlyacc is erroneous, for example here token "main"
   type is located in the .mli/.ml file instead of the .mly file. *)

let true_value = VBool true

# 63 "ocp-build/buildOCPParser.ml"
let yytransl_const = [|
    0 (* EOF *);
  261 (* SEMI *);
  262 (* BEGIN *);
  263 (* END *);
  265 (* LBRACKET *);
  266 (* RBRACKET *);
  267 (* LBRACE *);
  268 (* RBRACE *);
  269 (* PLUSEQUAL *);
  270 (* MINUSEQUAL *);
  271 (* TRUE *);
  272 (* FALSE *);
  273 (* INCLUDE *);
  275 (* OBJECTS *);
  276 (* LIBRARY *);
  277 (* SYNTAX *);
  278 (* PROGRAM *);
  279 (* CONFIG *);
  280 (* RULES *);
  281 (* BANG *);
  282 (* EQUAL *);
  283 (* LPAREN *);
  284 (* RPAREN *);
  285 (* TYPE *);
  286 (* USE *);
  287 (* PACK *);
  288 (* IF *);
  289 (* THEN *);
  290 (* ELSE *);
  291 (* NOT *);
  292 (* COND_OR *);
  293 (* COND_AND *);
  294 (* SYNTAXES *);
  295 (* CAMLP4 *);
  296 (* CAMLP5 *);
  297 (* TEST *);
  298 (* PERCENT *);
  299 (* GREATER *);
  300 (* GREATEREQUAL *);
  301 (* LESS *);
  302 (* LESSEQUAL *);
    0|]

let yytransl_block = [|
  257 (* STRING *);
  258 (* INT *);
  259 (* FLOAT *);
  260 (* CHAR *);
  264 (* IDENT *);
  274 (* INCLUDED *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\004\000\004\000\004\000\
\004\000\004\000\003\000\003\000\003\000\003\000\003\000\003\000\
\008\000\008\000\007\000\007\000\007\000\012\000\012\000\012\000\
\013\000\013\000\011\000\014\000\014\000\009\000\009\000\010\000\
\016\000\016\000\017\000\017\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\020\000\020\000\020\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\005\000\005\000\005\000\021\000\023\000\023\000\006\000\006\000\
\006\000\015\000\015\000\015\000\025\000\025\000\025\000\024\000\
\024\000\024\000\024\000\024\000\027\000\027\000\026\000\026\000\
\022\000\022\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\001\000\001\000\001\000\001\000\
\001\000\001\000\005\000\005\000\003\000\006\000\005\000\001\000\
\001\000\003\000\002\000\000\000\002\000\003\000\005\000\001\000\
\001\000\003\000\001\000\000\000\002\000\000\000\002\000\001\000\
\003\000\001\000\003\000\001\000\002\000\003\000\003\000\001\000\
\003\000\003\000\003\000\003\000\000\000\002\000\002\000\001\000\
\001\000\001\000\001\000\003\000\003\000\002\000\005\000\004\000\
\003\000\001\000\001\000\003\000\000\000\001\000\000\000\002\000\
\002\000\002\000\002\000\005\000\002\000\002\000\000\000\001\000\
\001\000\001\000\001\000\001\000\000\000\002\000\001\000\003\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\072\000\000\000\000\000\073\000\074\000\075\000\
\000\000\076\000\000\000\083\000\000\000\000\000\016\000\027\000\
\000\000\004\000\008\000\006\000\000\000\005\000\000\000\000\000\
\007\000\000\000\000\000\058\000\059\000\000\000\066\000\049\000\
\000\000\000\000\000\000\051\000\050\000\000\000\000\000\000\000\
\000\000\048\000\000\000\032\000\000\000\000\000\000\000\000\000\
\001\000\003\000\000\000\000\000\067\000\000\000\013\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\081\000\082\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\054\000\062\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\000\000\057\000\000\000\000\000\046\000\047\000\
\053\000\052\000\000\000\038\000\000\000\017\000\000\000\027\000\
\000\000\033\000\035\000\000\000\000\000\000\000\000\000\000\000\
\000\000\064\000\000\000\011\000\065\000\021\000\000\000\000\000\
\012\000\019\000\060\000\056\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\000\000\068\000\000\000\000\000\
\022\000\000\000\014\000\018\000\080\000\031\000\000\000\079\000\
\078\000\055\000\000\000\025\000\000\000\000\000\000\000\000\000\
\023\000\026\000\000\000\029\000"

let yydgoto = "\002\000\
\012\000\018\000\014\000\027\000\042\000\114\000\118\000\103\000\
\132\000\043\000\015\000\091\000\149\000\153\000\016\000\044\000\
\045\000\046\000\047\000\062\000\077\000\048\000\079\000\017\000\
\053\000\105\000\134\000"

let yysindex = "\018\000\
\197\000\000\000\000\000\197\000\229\255\000\000\000\000\000\000\
\008\255\000\000\047\255\000\000\021\000\197\000\000\000\000\000\
\004\255\000\000\000\000\000\000\000\000\000\000\008\255\000\000\
\000\000\016\255\008\255\000\000\000\000\071\255\000\000\000\000\
\020\255\157\000\157\000\000\000\000\000\008\255\196\255\014\255\
\196\255\000\000\026\255\000\000\024\255\031\255\137\255\020\255\
\000\000\000\000\169\000\169\000\000\000\091\001\000\000\218\000\
\020\255\091\001\157\000\157\000\213\255\065\255\081\255\044\255\
\066\255\000\000\000\000\000\000\230\000\196\255\196\255\169\000\
\169\000\169\000\169\000\169\000\000\000\000\000\099\255\020\255\
\020\255\091\001\196\255\103\255\091\001\218\000\218\000\196\255\
\104\255\000\000\218\000\000\000\088\255\110\255\000\000\000\000\
\000\000\000\000\001\001\000\000\013\001\000\000\085\255\000\000\
\092\255\000\000\000\000\020\255\020\255\020\255\020\255\020\255\
\157\000\000\000\089\255\000\000\000\000\000\000\120\255\100\255\
\000\000\000\000\000\000\000\000\197\000\085\255\013\001\133\255\
\134\255\091\001\001\001\000\000\096\001\000\000\126\255\096\001\
\000\000\040\001\000\000\000\000\000\000\000\000\091\001\000\000\
\000\000\000\000\052\001\000\000\113\255\052\001\144\255\079\001\
\000\000\000\000\218\000\000\000"

let yyrindex = "\000\000\
\148\000\000\000\000\000\003\000\151\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\003\000\000\000\000\000\
\001\000\000\000\000\000\000\000\136\255\000\000\000\000\164\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\149\255\161\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\255\252\254\048\255\152\255\
\000\000\000\000\000\000\000\000\000\000\167\255\000\000\168\255\
\000\000\148\255\172\255\059\255\059\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\
\065\000\122\255\000\000\000\000\122\255\076\255\168\255\000\000\
\000\000\000\000\076\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\174\255\000\000\125\000\000\000\
\097\000\000\000\000\000\180\255\239\255\246\255\014\000\016\000\
\149\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\174\255\125\000\174\255\000\000\
\000\000\025\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\175\255\000\000\
\000\000\000\000\177\255\000\000\185\000\177\255\000\000\000\000\
\000\000\000\000\177\255\000\000"

let yygindex = "\000\000\
\000\000\255\255\198\255\000\000\082\000\216\255\207\255\169\255\
\058\000\225\255\205\255\124\255\039\000\000\000\241\255\123\000\
\128\000\159\000\079\000\223\255\233\255\000\000\000\000\171\000\
\000\000\070\000\000\000"

let yytablesize = 640
let yytable = "\013\000\
\071\000\063\000\002\000\026\000\090\000\148\000\089\000\065\000\
\028\000\059\000\102\000\126\000\050\000\084\000\066\000\029\000\
\051\000\093\000\001\000\148\000\049\000\067\000\055\000\036\000\
\078\000\094\000\095\000\096\000\036\000\052\000\027\000\036\000\
\070\000\092\000\090\000\090\000\027\000\119\000\085\000\090\000\
\102\000\122\000\085\000\142\000\117\000\034\000\058\000\028\000\
\032\000\030\000\034\000\115\000\033\000\104\000\029\000\034\000\
\120\000\035\000\069\000\070\000\129\000\036\000\037\000\038\000\
\069\000\045\000\085\000\071\000\045\000\085\000\045\000\003\000\
\102\000\039\000\097\000\040\000\099\000\040\000\006\000\135\000\
\040\000\041\000\020\000\040\000\040\000\130\000\090\000\020\000\
\030\000\117\000\031\000\007\000\098\000\100\000\008\000\090\000\
\077\000\151\000\090\000\128\000\090\000\010\000\129\000\090\000\
\054\000\151\000\129\000\113\000\056\000\116\000\121\000\130\000\
\061\000\061\000\085\000\123\000\124\000\144\000\131\000\064\000\
\144\000\136\000\104\000\128\000\030\000\133\000\137\000\085\000\
\063\000\080\000\081\000\130\000\138\000\063\000\130\000\146\000\
\009\000\061\000\061\000\061\000\074\000\074\000\074\000\009\000\
\140\000\141\000\152\000\002\000\074\000\063\000\108\000\109\000\
\110\000\111\000\112\000\154\000\074\000\002\000\045\000\074\000\
\061\000\074\000\072\000\058\000\010\000\074\000\074\000\074\000\
\075\000\075\000\075\000\010\000\045\000\063\000\020\000\063\000\
\075\000\009\000\045\000\073\000\074\000\075\000\076\000\139\000\
\075\000\002\000\063\000\075\000\020\000\075\000\156\000\061\000\
\106\000\075\000\075\000\075\000\028\000\032\000\107\000\068\000\
\057\000\033\000\145\000\029\000\034\000\010\000\035\000\039\000\
\000\000\000\000\036\000\037\000\039\000\028\000\032\000\039\000\
\039\000\060\000\033\000\000\000\029\000\034\000\039\000\035\000\
\000\000\000\000\040\000\036\000\037\000\003\000\041\000\000\000\
\000\000\004\000\005\000\000\000\006\000\030\000\000\000\058\000\
\000\000\000\000\000\000\040\000\000\000\000\000\000\000\019\000\
\020\000\021\000\022\000\023\000\024\000\000\000\030\000\000\000\
\000\000\071\000\009\000\010\000\011\000\071\000\071\000\071\000\
\071\000\002\000\041\000\000\000\071\000\025\000\002\000\041\000\
\000\000\042\000\041\000\041\000\000\000\071\000\042\000\000\000\
\071\000\042\000\042\000\000\000\071\000\000\000\071\000\071\000\
\071\000\070\000\071\000\000\000\000\000\070\000\070\000\070\000\
\070\000\043\000\000\000\044\000\070\000\000\000\043\000\000\000\
\044\000\043\000\043\000\044\000\044\000\070\000\000\000\000\000\
\070\000\000\000\000\000\000\000\070\000\000\000\070\000\070\000\
\070\000\069\000\070\000\000\000\000\000\069\000\069\000\069\000\
\069\000\000\000\000\000\000\000\069\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\069\000\000\000\000\000\
\069\000\000\000\000\000\000\000\069\000\000\000\069\000\069\000\
\069\000\077\000\069\000\000\000\000\000\077\000\077\000\077\000\
\077\000\000\000\000\000\000\000\077\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\077\000\000\000\000\000\
\077\000\000\000\000\000\000\000\077\000\030\000\077\000\077\000\
\077\000\030\000\030\000\030\000\030\000\000\000\000\000\000\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\030\000\000\000\000\000\030\000\000\000\000\000\000\000\
\000\000\000\000\030\000\030\000\030\000\028\000\032\000\000\000\
\000\000\060\000\033\000\000\000\029\000\034\000\000\000\035\000\
\000\000\028\000\032\000\036\000\037\000\000\000\033\000\000\000\
\029\000\034\000\000\000\035\000\000\000\000\000\000\000\036\000\
\037\000\028\000\000\000\040\000\000\000\028\000\028\000\028\000\
\028\000\000\000\000\000\000\000\028\000\003\000\030\000\040\000\
\000\000\004\000\005\000\000\000\006\000\028\000\000\000\000\000\
\028\000\000\000\030\000\000\000\000\000\000\000\028\000\028\000\
\028\000\007\000\003\000\000\000\008\000\000\000\086\000\087\000\
\000\000\006\000\009\000\010\000\011\000\000\000\003\000\000\000\
\000\000\000\000\000\000\005\000\000\000\006\000\007\000\000\000\
\101\000\008\000\000\000\000\000\000\000\000\000\000\000\009\000\
\010\000\088\000\007\000\000\000\000\000\008\000\000\000\000\000\
\000\000\003\000\000\000\009\000\010\000\011\000\005\000\000\000\
\006\000\000\000\000\000\125\000\000\000\003\000\000\000\000\000\
\000\000\127\000\005\000\000\000\006\000\007\000\000\000\000\000\
\008\000\000\000\000\000\000\000\000\000\000\000\009\000\010\000\
\011\000\007\000\000\000\000\000\008\000\000\000\000\000\000\000\
\003\000\000\000\009\000\010\000\011\000\087\000\000\000\006\000\
\000\000\000\000\147\000\000\000\003\000\000\000\000\000\000\000\
\150\000\087\000\000\000\006\000\007\000\000\000\000\000\008\000\
\000\000\000\000\000\000\000\000\000\000\009\000\010\000\088\000\
\007\000\000\000\000\000\008\000\000\000\000\000\000\000\003\000\
\000\000\009\000\010\000\088\000\087\000\000\000\006\000\000\000\
\000\000\155\000\000\000\003\000\000\000\000\000\000\000\082\000\
\003\000\000\000\006\000\007\000\000\000\000\000\008\000\006\000\
\000\000\000\000\143\000\000\000\009\000\010\000\088\000\007\000\
\000\000\000\000\008\000\000\000\007\000\000\000\000\000\008\000\
\009\000\010\000\083\000\000\000\000\000\009\000\010\000\083\000"

let yycheck = "\001\000\
\000\000\035\000\000\000\005\000\056\000\138\000\056\000\039\000\
\001\001\033\000\069\000\099\000\014\000\054\000\001\001\008\001\
\013\001\058\000\001\000\152\000\000\000\008\001\007\001\028\001\
\048\000\059\000\060\000\061\000\033\001\026\001\006\001\036\001\
\000\000\057\000\086\000\087\000\012\001\087\000\054\000\091\000\
\099\000\091\000\058\000\131\000\085\000\028\001\027\001\001\001\
\002\001\042\001\033\001\083\000\006\001\069\000\008\001\009\001\
\088\000\011\001\033\001\036\001\101\000\015\001\016\001\017\001\
\000\000\007\001\082\000\037\001\010\001\085\000\012\001\001\001\
\131\000\027\001\010\001\028\001\033\001\031\001\008\001\113\000\
\033\001\035\001\007\001\036\001\037\001\101\000\138\000\012\001\
\042\001\130\000\009\000\021\001\012\001\028\001\024\001\147\000\
\000\000\147\000\150\000\101\000\152\000\031\001\143\000\155\000\
\023\000\155\000\147\000\009\001\027\000\007\001\007\001\127\000\
\034\000\035\000\130\000\028\001\007\001\133\000\034\001\038\000\
\136\000\033\001\138\000\125\000\000\000\034\001\007\001\143\000\
\007\001\051\000\052\000\147\000\033\001\012\001\150\000\010\001\
\001\001\059\000\060\000\061\000\005\001\006\001\007\001\008\001\
\012\001\012\001\034\001\000\000\013\001\028\001\072\000\073\000\
\074\000\075\000\076\000\012\001\021\001\007\001\010\001\024\001\
\009\001\026\001\026\001\027\001\001\001\030\001\031\001\032\001\
\005\001\006\001\007\001\008\001\012\001\007\001\007\001\028\001\
\013\001\042\001\007\001\043\001\044\001\045\001\046\001\126\000\
\021\001\012\001\012\001\024\001\012\001\026\001\152\000\113\000\
\070\000\030\001\031\001\032\001\001\001\002\001\071\000\041\000\
\030\000\006\001\133\000\008\001\009\001\042\001\011\001\028\001\
\255\255\255\255\015\001\016\001\033\001\001\001\002\001\036\001\
\037\001\005\001\006\001\255\255\008\001\009\001\027\001\011\001\
\255\255\255\255\031\001\015\001\016\001\001\001\035\001\255\255\
\255\255\005\001\006\001\255\255\008\001\042\001\255\255\027\001\
\255\255\255\255\255\255\031\001\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\255\255\042\001\255\255\
\255\255\001\001\030\001\031\001\032\001\005\001\006\001\007\001\
\008\001\007\001\028\001\255\255\012\001\041\001\012\001\033\001\
\255\255\028\001\036\001\037\001\255\255\021\001\033\001\255\255\
\024\001\036\001\037\001\255\255\028\001\255\255\030\001\031\001\
\032\001\001\001\034\001\255\255\255\255\005\001\006\001\007\001\
\008\001\028\001\255\255\028\001\012\001\255\255\033\001\255\255\
\033\001\036\001\037\001\036\001\037\001\021\001\255\255\255\255\
\024\001\255\255\255\255\255\255\028\001\255\255\030\001\031\001\
\032\001\001\001\034\001\255\255\255\255\005\001\006\001\007\001\
\008\001\255\255\255\255\255\255\012\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\021\001\255\255\255\255\
\024\001\255\255\255\255\255\255\028\001\255\255\030\001\031\001\
\032\001\001\001\034\001\255\255\255\255\005\001\006\001\007\001\
\008\001\255\255\255\255\255\255\012\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\021\001\255\255\255\255\
\024\001\255\255\255\255\255\255\028\001\001\001\030\001\031\001\
\032\001\005\001\006\001\007\001\008\001\255\255\255\255\255\255\
\012\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\021\001\255\255\255\255\024\001\255\255\255\255\255\255\
\255\255\255\255\030\001\031\001\032\001\001\001\002\001\255\255\
\255\255\005\001\006\001\255\255\008\001\009\001\255\255\011\001\
\255\255\001\001\002\001\015\001\016\001\255\255\006\001\255\255\
\008\001\009\001\255\255\011\001\255\255\255\255\255\255\015\001\
\016\001\001\001\255\255\031\001\255\255\005\001\006\001\007\001\
\008\001\255\255\255\255\255\255\012\001\001\001\042\001\031\001\
\255\255\005\001\006\001\255\255\008\001\021\001\255\255\255\255\
\024\001\255\255\042\001\255\255\255\255\255\255\030\001\031\001\
\032\001\021\001\001\001\255\255\024\001\255\255\005\001\006\001\
\255\255\008\001\030\001\031\001\032\001\255\255\001\001\255\255\
\255\255\255\255\255\255\006\001\255\255\008\001\021\001\255\255\
\011\001\024\001\255\255\255\255\255\255\255\255\255\255\030\001\
\031\001\032\001\021\001\255\255\255\255\024\001\255\255\255\255\
\255\255\001\001\255\255\030\001\031\001\032\001\006\001\255\255\
\008\001\255\255\255\255\011\001\255\255\001\001\255\255\255\255\
\255\255\005\001\006\001\255\255\008\001\021\001\255\255\255\255\
\024\001\255\255\255\255\255\255\255\255\255\255\030\001\031\001\
\032\001\021\001\255\255\255\255\024\001\255\255\255\255\255\255\
\001\001\255\255\030\001\031\001\032\001\006\001\255\255\008\001\
\255\255\255\255\011\001\255\255\001\001\255\255\255\255\255\255\
\005\001\006\001\255\255\008\001\021\001\255\255\255\255\024\001\
\255\255\255\255\255\255\255\255\255\255\030\001\031\001\032\001\
\021\001\255\255\255\255\024\001\255\255\255\255\255\255\001\001\
\255\255\030\001\031\001\032\001\006\001\255\255\008\001\255\255\
\255\255\011\001\255\255\001\001\255\255\255\255\255\255\005\001\
\001\001\255\255\008\001\021\001\255\255\255\255\024\001\008\001\
\255\255\255\255\011\001\255\255\030\001\031\001\032\001\021\001\
\255\255\255\255\024\001\255\255\021\001\255\255\255\255\024\001\
\030\001\031\001\032\001\255\255\255\255\030\001\031\001\032\001"

let yynames_const = "\
  EOF\000\
  SEMI\000\
  BEGIN\000\
  END\000\
  LBRACKET\000\
  RBRACKET\000\
  LBRACE\000\
  RBRACE\000\
  PLUSEQUAL\000\
  MINUSEQUAL\000\
  TRUE\000\
  FALSE\000\
  INCLUDE\000\
  OBJECTS\000\
  LIBRARY\000\
  SYNTAX\000\
  PROGRAM\000\
  CONFIG\000\
  RULES\000\
  BANG\000\
  EQUAL\000\
  LPAREN\000\
  RPAREN\000\
  TYPE\000\
  USE\000\
  PACK\000\
  IF\000\
  THEN\000\
  ELSE\000\
  NOT\000\
  COND_OR\000\
  COND_AND\000\
  SYNTAXES\000\
  CAMLP4\000\
  CAMLP5\000\
  TEST\000\
  PERCENT\000\
  GREATER\000\
  GREATEREQUAL\000\
  LESS\000\
  LESSEQUAL\000\
  "

let yynames_block = "\
  STRING\000\
  INT\000\
  FLOAT\000\
  CHAR\000\
  IDENT\000\
  INCLUDED\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'toplevel_statements) in
    Obj.repr(
# 81 "ocp-build/buildOCPParser.mly"
                        ( _1 )
# 446 "ocp-build/buildOCPParser.ml"
               : BuildOCPTree.statement list))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "ocp-build/buildOCPParser.mly"
  ( [] )
# 452 "ocp-build/buildOCPParser.ml"
               : 'toplevel_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'toplevel_statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'toplevel_statements) in
    Obj.repr(
# 86 "ocp-build/buildOCPParser.mly"
                                         ( _1 :: _2 )
# 460 "ocp-build/buildOCPParser.ml"
               : 'toplevel_statements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'toplevel_statements) in
    Obj.repr(
# 87 "ocp-build/buildOCPParser.mly"
                           ( _2 )
# 467 "ocp-build/buildOCPParser.ml"
               : 'toplevel_statements))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "ocp-build/buildOCPParser.mly"
          ( ProgramPackage )
# 473 "ocp-build/buildOCPParser.ml"
               : 'package_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "ocp-build/buildOCPParser.mly"
          ( LibraryPackage )
# 479 "ocp-build/buildOCPParser.ml"
               : 'package_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "ocp-build/buildOCPParser.mly"
          ( TestPackage )
# 485 "ocp-build/buildOCPParser.ml"
               : 'package_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "ocp-build/buildOCPParser.mly"
          ( ObjectsPackage )
# 491 "ocp-build/buildOCPParser.ml"
               : 'package_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "ocp-build/buildOCPParser.mly"
          ( SyntaxPackage )
# 497 "ocp-build/buildOCPParser.ml"
               : 'package_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "ocp-build/buildOCPParser.mly"
          ( RulesPackage )
# 503 "ocp-build/buildOCPParser.ml"
               : 'package_type))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'string_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_set_options) in
    Obj.repr(
# 100 "ocp-build/buildOCPParser.mly"
                                                         ( StmtDefineConfig (_3, _4) )
# 511 "ocp-build/buildOCPParser.ml"
               : 'toplevel_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'package_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'string_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 101 "ocp-build/buildOCPParser.mly"
                                                      ( StmtDefinePackage (_2, _3, _4) )
# 520 "ocp-build/buildOCPParser.ml"
               : 'toplevel_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'toplevel_statements) in
    Obj.repr(
# 102 "ocp-build/buildOCPParser.mly"
                                ( StmtBlock _2 )
# 527 "ocp-build/buildOCPParser.ml"
               : 'toplevel_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'string_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'one_toplevel_statement) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'maybe_else_one_toplevel_statement) in
    Obj.repr(
# 104 "ocp-build/buildOCPParser.mly"
                                                              ( StmtInclude(_3,_5,_6) )
# 536 "ocp-build/buildOCPParser.ml"
               : 'toplevel_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'one_toplevel_statement) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'maybe_else_one_toplevel_statement) in
    Obj.repr(
# 105 "ocp-build/buildOCPParser.mly"
                                                                             ( StmtIfThenElse(_2,_4,_5) )
# 545 "ocp-build/buildOCPParser.ml"
               : 'toplevel_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_statement) in
    Obj.repr(
# 106 "ocp-build/buildOCPParser.mly"
                   ( _1 )
# 552 "ocp-build/buildOCPParser.ml"
               : 'toplevel_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'toplevel_statement) in
    Obj.repr(
# 114 "ocp-build/buildOCPParser.mly"
                     ( [_1] )
# 559 "ocp-build/buildOCPParser.ml"
               : 'one_toplevel_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'toplevel_statements) in
    Obj.repr(
# 115 "ocp-build/buildOCPParser.mly"
                                    ( _2 )
# 566 "ocp-build/buildOCPParser.ml"
               : 'one_toplevel_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 119 "ocp-build/buildOCPParser.mly"
                       ( _1 :: _2 )
# 574 "ocp-build/buildOCPParser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "ocp-build/buildOCPParser.mly"
  ( [] )
# 580 "ocp-build/buildOCPParser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 121 "ocp-build/buildOCPParser.mly"
                  ( _2 )
# 587 "ocp-build/buildOCPParser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 125 "ocp-build/buildOCPParser.mly"
                       ( StmtBlock _2 )
# 594 "ocp-build/buildOCPParser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'one_statement) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'maybe_else_one_statement) in
    Obj.repr(
# 126 "ocp-build/buildOCPParser.mly"
                                                           ( StmtIfThenElse(_2, _4, _5) )
# 603 "ocp-build/buildOCPParser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_statement) in
    Obj.repr(
# 127 "ocp-build/buildOCPParser.mly"
                   ( _1 )
# 610 "ocp-build/buildOCPParser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 131 "ocp-build/buildOCPParser.mly"
            ( [_1] )
# 617 "ocp-build/buildOCPParser.ml"
               : 'one_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 132 "ocp-build/buildOCPParser.mly"
                           ( _2 )
# 624 "ocp-build/buildOCPParser.ml"
               : 'one_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set_option) in
    Obj.repr(
# 136 "ocp-build/buildOCPParser.mly"
             ( StmtOption _1 )
# 631 "ocp-build/buildOCPParser.ml"
               : 'simple_statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "ocp-build/buildOCPParser.mly"
  ( None )
# 637 "ocp-build/buildOCPParser.ml"
               : 'maybe_else_one_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'one_statement) in
    Obj.repr(
# 141 "ocp-build/buildOCPParser.mly"
                     ( Some _2 )
# 644 "ocp-build/buildOCPParser.ml"
               : 'maybe_else_one_statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "ocp-build/buildOCPParser.mly"
  ( None )
# 650 "ocp-build/buildOCPParser.ml"
               : 'maybe_else_one_toplevel_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'one_toplevel_statement) in
    Obj.repr(
# 146 "ocp-build/buildOCPParser.mly"
                              ( Some _2 )
# 657 "ocp-build/buildOCPParser.ml"
               : 'maybe_else_one_toplevel_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condition2) in
    Obj.repr(
# 150 "ocp-build/buildOCPParser.mly"
           ( _1 )
# 664 "ocp-build/buildOCPParser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'condition2) in
    Obj.repr(
# 154 "ocp-build/buildOCPParser.mly"
                                ( OrConditions (_1, _3) )
# 672 "ocp-build/buildOCPParser.ml"
               : 'condition2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condition1) in
    Obj.repr(
# 155 "ocp-build/buildOCPParser.mly"
             ( _1 )
# 679 "ocp-build/buildOCPParser.ml"
               : 'condition2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition0) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'condition1) in
    Obj.repr(
# 159 "ocp-build/buildOCPParser.mly"
                                 ( AndConditions (_1, _3) )
# 687 "ocp-build/buildOCPParser.ml"
               : 'condition1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condition0) in
    Obj.repr(
# 160 "ocp-build/buildOCPParser.mly"
             ( _1 )
# 694 "ocp-build/buildOCPParser.ml"
               : 'condition1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'condition0) in
    Obj.repr(
# 164 "ocp-build/buildOCPParser.mly"
                 ( NotCondition _2 )
# 701 "ocp-build/buildOCPParser.ml"
               : 'condition0))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'condition) in
    Obj.repr(
# 165 "ocp-build/buildOCPParser.mly"
                          ( _2 )
# 708 "ocp-build/buildOCPParser.ml"
               : 'condition0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 166 "ocp-build/buildOCPParser.mly"
                              ( IsEqual(_1, _3) )
# 716 "ocp-build/buildOCPParser.ml"
               : 'condition0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 167 "ocp-build/buildOCPParser.mly"
             ( IsNonFalse _1 )
# 723 "ocp-build/buildOCPParser.ml"
               : 'condition0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 168 "ocp-build/buildOCPParser.mly"
                                ( Greater(_1,_3) )
# 731 "ocp-build/buildOCPParser.ml"
               : 'condition0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 169 "ocp-build/buildOCPParser.mly"
                                     ( GreaterEqual(_1,_3) )
# 739 "ocp-build/buildOCPParser.ml"
               : 'condition0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 170 "ocp-build/buildOCPParser.mly"
                             ( Greater(_3,_1) )
# 747 "ocp-build/buildOCPParser.ml"
               : 'condition0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 171 "ocp-build/buildOCPParser.mly"
                                  ( GreaterEqual(_3,_1) )
# 755 "ocp-build/buildOCPParser.ml"
               : 'condition0))
; (fun __caml_parser_env ->
    Obj.repr(
# 175 "ocp-build/buildOCPParser.mly"
  ( [] )
# 761 "ocp-build/buildOCPParser.ml"
               : 'list_of_expressions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_expressions) in
    Obj.repr(
# 176 "ocp-build/buildOCPParser.mly"
                           ( _2 )
# 768 "ocp-build/buildOCPParser.ml"
               : 'list_of_expressions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_expressions) in
    Obj.repr(
# 177 "ocp-build/buildOCPParser.mly"
                                 ( _1 :: _2 )
# 776 "ocp-build/buildOCPParser.ml"
               : 'list_of_expressions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'string_expression) in
    Obj.repr(
# 181 "ocp-build/buildOCPParser.mly"
                    ( _1 )
# 783 "ocp-build/buildOCPParser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 182 "ocp-build/buildOCPParser.mly"
           ( ExprString (string_of_int  _1) )
# 790 "ocp-build/buildOCPParser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 183 "ocp-build/buildOCPParser.mly"
            ( ExprBool false )
# 796 "ocp-build/buildOCPParser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 184 "ocp-build/buildOCPParser.mly"
           ( ExprBool true )
# 802 "ocp-build/buildOCPParser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_expressions) in
    Obj.repr(
# 185 "ocp-build/buildOCPParser.mly"
                                    (
    ExprList [ ExprApply (ExprString "", [
                   OptionVariableSet ("type", ExprString "%list");
                   OptionVariableSet ("value", ExprList _2);
                 ])] )
# 813 "ocp-build/buildOCPParser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_expressions) in
    Obj.repr(
# 190 "ocp-build/buildOCPParser.mly"
                                        ( ExprList _2 )
# 820 "ocp-build/buildOCPParser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'set_option_list) in
    Obj.repr(
# 191 "ocp-build/buildOCPParser.mly"
                             ( ExprApply (_1, _2) )
# 828 "ocp-build/buildOCPParser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'packer) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'maybe_set_option_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_expressions) in
    Obj.repr(
# 194 "ocp-build/buildOCPParser.mly"
                                                                     (
  let packname = _1 in
  let pack_options = _2 in
  let packname = modname_of_fullname packname in
    ExprPrimitive ("pack",
      [
       OptionVariableSet ("to_module", ExprApply(ExprString packname, pack_options));
       OptionVariableSet ("files", ExprList _4)
      ])
)
# 846 "ocp-build/buildOCPParser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'set_option_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_expressions) in
    Obj.repr(
# 204 "ocp-build/buildOCPParser.mly"
                                                (
  ExprApply (ExprList _3, _2)
)
# 856 "ocp-build/buildOCPParser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_option_list) in
    Obj.repr(
# 210 "ocp-build/buildOCPParser.mly"
                                ( ExprPrimitive (_2, _3) )
# 864 "ocp-build/buildOCPParser.ml"
               : 'string_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 211 "ocp-build/buildOCPParser.mly"
         ( ExprString   _1 )
# 871 "ocp-build/buildOCPParser.ml"
               : 'string_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 212 "ocp-build/buildOCPParser.mly"
        ( ExprVariable _1 )
# 878 "ocp-build/buildOCPParser.ml"
               : 'string_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_set_options) in
    Obj.repr(
# 216 "ocp-build/buildOCPParser.mly"
                                    ( _2 )
# 885 "ocp-build/buildOCPParser.ml"
               : 'set_option_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 220 "ocp-build/buildOCPParser.mly"
    ( [] )
# 891 "ocp-build/buildOCPParser.ml"
               : 'maybe_set_option_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set_option_list) in
    Obj.repr(
# 221 "ocp-build/buildOCPParser.mly"
                  ( _1 )
# 898 "ocp-build/buildOCPParser.ml"
               : 'maybe_set_option_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 225 "ocp-build/buildOCPParser.mly"
  ( [] )
# 904 "ocp-build/buildOCPParser.ml"
               : 'list_of_set_options))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_set_options) in
    Obj.repr(
# 226 "ocp-build/buildOCPParser.mly"
                           ( _2 )
# 911 "ocp-build/buildOCPParser.ml"
               : 'list_of_set_options))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'set_option) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_set_options) in
    Obj.repr(
# 227 "ocp-build/buildOCPParser.mly"
                                 ( _1 :: _2 )
# 919 "ocp-build/buildOCPParser.ml"
               : 'list_of_set_options))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'string_expression) in
    Obj.repr(
# 231 "ocp-build/buildOCPParser.mly"
                        ( OptionConfigUse _2 )
# 926 "ocp-build/buildOCPParser.ml"
               : 'set_option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'set_ident) in
    Obj.repr(
# 232 "ocp-build/buildOCPParser.mly"
                  ( _2 _1 )
# 934 "ocp-build/buildOCPParser.ml"
               : 'set_option))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'one_set_option) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'maybe_else_one_set_option) in
    Obj.repr(
# 233 "ocp-build/buildOCPParser.mly"
                                                             ( OptionIfThenElse(_2, _4, _5) )
# 943 "ocp-build/buildOCPParser.ml"
               : 'set_option))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 237 "ocp-build/buildOCPParser.mly"
                   ( fun id -> OptionVariableSet (id, _2) )
# 950 "ocp-build/buildOCPParser.ml"
               : 'set_ident))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 238 "ocp-build/buildOCPParser.mly"
                       ( fun id -> OptionVariableAppend (id, _2) )
# 957 "ocp-build/buildOCPParser.ml"
               : 'set_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 239 "ocp-build/buildOCPParser.mly"
   ( fun id ->  OptionVariableSet (id, ExprBool true) )
# 963 "ocp-build/buildOCPParser.ml"
               : 'set_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 243 "ocp-build/buildOCPParser.mly"
         ( _1 )
# 970 "ocp-build/buildOCPParser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 244 "ocp-build/buildOCPParser.mly"
         ( _1 )
# 977 "ocp-build/buildOCPParser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 245 "ocp-build/buildOCPParser.mly"
         ( "syntax" )
# 983 "ocp-build/buildOCPParser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 246 "ocp-build/buildOCPParser.mly"
         ( "rules" )
# 989 "ocp-build/buildOCPParser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 247 "ocp-build/buildOCPParser.mly"
         ( "pack" )
# 995 "ocp-build/buildOCPParser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 251 "ocp-build/buildOCPParser.mly"
  ( None )
# 1001 "ocp-build/buildOCPParser.ml"
               : 'maybe_else_one_set_option))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'one_set_option) in
    Obj.repr(
# 252 "ocp-build/buildOCPParser.mly"
                      ( Some _2 )
# 1008 "ocp-build/buildOCPParser.ml"
               : 'maybe_else_one_set_option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set_option) in
    Obj.repr(
# 256 "ocp-build/buildOCPParser.mly"
             ( _1  )
# 1015 "ocp-build/buildOCPParser.ml"
               : 'one_set_option))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_set_options) in
    Obj.repr(
# 257 "ocp-build/buildOCPParser.mly"
                                    ( OptionBlock _2 )
# 1022 "ocp-build/buildOCPParser.ml"
               : 'one_set_option))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 261 "ocp-build/buildOCPParser.mly"
              ( _2 )
# 1029 "ocp-build/buildOCPParser.ml"
               : 'packer))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 262 "ocp-build/buildOCPParser.mly"
              ( let s = _2 in
                Printf.sprintf "%c%s.ml"
                  (Char.lowercase s.[0])
                  (String.sub s 1 (String.length s - 1)) )
# 1039 "ocp-build/buildOCPParser.ml"
               : 'packer))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : BuildOCPTree.statement list)
;;
