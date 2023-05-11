type token =
  | VAR of (string)
  | OBJECT of (string)
  | ROLE of (string)
  | INVROLE of (string)
  | LPAREN
  | RPAREN
  | TYPE
  | ARROW
  | QUERY
  | AND
  | EOL

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* TYPE *);
  264 (* ARROW *);
  265 (* QUERY *);
  266 (* AND *);
  267 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* OBJECT *);
  259 (* ROLE *);
  260 (* INVROLE *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\004\000\004\000\
\004\000\000\000"

let yylen = "\002\000\
\006\000\001\000\003\000\003\000\003\000\003\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\010\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\009\000\007\000\000\000\000\000\
\001\000\000\000\006\000\004\000\005\000\003\000"

let yydgoto = "\002\000\
\004\000\010\000\011\000\015\000"

let yysindex = "\006\000\
\255\254\000\000\008\255\000\000\002\255\006\255\001\255\253\254\
\253\254\007\255\005\255\000\000\000\000\000\000\004\255\011\255\
\000\000\001\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\252\255\000\000\008\000"

let yytablesize = 17
let yytable = "\012\000\
\013\000\008\000\009\000\014\000\019\000\020\000\001\000\003\000\
\005\000\006\000\007\000\021\000\017\000\022\000\018\000\002\000\
\016\000"

let yycheck = "\003\001\
\004\001\001\001\002\001\007\001\001\001\002\001\001\000\009\001\
\001\001\008\001\005\001\001\001\006\001\018\000\010\001\006\001\
\009\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  TYPE\000\
  ARROW\000\
  QUERY\000\
  AND\000\
  EOL\000\
  "

let yynames_block = "\
  VAR\000\
  OBJECT\000\
  ROLE\000\
  INVROLE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'gp) in
    Obj.repr(
# 21 "bin/myParser.mly"
                                                           (Q (V _2 , _5))
# 99 "bin/myParser.ml"
               : Lib.query))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 25 "bin/myParser.mly"
                                       (_1)
# 106 "bin/myParser.ml"
               : 'gp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'gp) in
    Obj.repr(
# 26 "bin/myParser.mly"
                                       ( CP (_1 , _3)  )
# 114 "bin/myParser.ml"
               : 'gp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'role) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 30 "bin/myParser.mly"
                                   (SP (V _1 , _2 , O _3))
# 123 "bin/myParser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'role) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 31 "bin/myParser.mly"
                                   (OP (O _1 , _2 , V _3))
# 132 "bin/myParser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'role) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 32 "bin/myParser.mly"
                                   (DP (V _1 , _2 , V _3))
# 141 "bin/myParser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "bin/myParser.mly"
                     (Type)
# 147 "bin/myParser.ml"
               : 'role))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "bin/myParser.mly"
                     (P (_1))
# 154 "bin/myParser.ml"
               : 'role))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "bin/myParser.mly"
                     (I (_1))
# 161 "bin/myParser.ml"
               : 'role))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lib.query)
;;
