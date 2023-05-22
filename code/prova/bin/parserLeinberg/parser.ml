type token =
  | VAR of (string)
  | OBJECT of (string)
  | ROLE of (string)
  | TYPE
  | ARROW
  | QUERY
  | AND
  | EOL

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Lib
# 16 "parser.ml"
let yytransl_const = [|
  260 (* TYPE *);
  261 (* ARROW *);
  262 (* QUERY *);
  263 (* AND *);
  264 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* OBJECT *);
  259 (* ROLE *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\005\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\005\000\003\000\
\002\000\004\000\000\000"

let yydgoto = "\002\000\
\004\000\009\000"

let yysindex = "\001\000\
\003\255\000\000\009\255\000\000\007\255\255\254\000\255\008\255\
\254\254\006\255\011\255\013\255\255\254\000\000\000\000\000\000\
\000\000\000\000\010\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\255"

let yygindex = "\000\000\
\000\000\002\000"

let yytablesize = 20
let yytable = "\007\000\
\008\000\001\000\010\000\011\000\013\000\014\000\015\000\016\000\
\003\000\005\000\012\000\006\000\017\000\018\000\019\000\000\000\
\013\000\000\000\000\000\006\000"

let yycheck = "\001\001\
\002\001\001\000\003\001\004\001\007\001\008\001\001\001\002\001\
\006\001\001\001\003\001\005\001\002\001\001\001\013\000\255\255\
\007\001\255\255\255\255\008\001"

let yynames_const = "\
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
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'gp) in
    Obj.repr(
# 20 "parser.mly"
                                                  ( _4 )
# 91 "parser.ml"
               : Lib.axiom list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 23 "parser.mly"
                                   ((_1 , Atomic _3) :: [] )
# 99 "parser.ml"
               : 'gp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 24 "parser.mly"
                                   ((_1 , Exist(Property _2, Atomic _3)) :: [])
# 108 "parser.ml"
               : 'gp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 25 "parser.mly"
                                   ((_3 , Exist(PropertyInverse _2, Atomic _1)) :: [])
# 117 "parser.ml"
               : 'gp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 26 "parser.mly"
                                   ((_1 , Exist(Property _2, Atomic _3)) :: 
                                    (_3 , Exist(PropertyInverse _2, Atomic _1)) :: 
                                    [])
# 128 "parser.ml"
               : 'gp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'gp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'gp) in
    Obj.repr(
# 29 "parser.mly"
                                   (List.append _1 _3 )
# 136 "parser.ml"
               : 'gp))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lib.axiom list)
;;
