type token =
  | VAR of (string)
  | OBJECT of (string)
  | ROLE of (string)
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
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* TYPE *);
  263 (* ARROW *);
  264 (* QUERY *);
  265 (* AND *);
  266 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* OBJECT *);
  259 (* ROLE *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\006\000\001\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\007\000\005\000\004\000\006\000\003\000"

let yydgoto = "\002\000\
\004\000\010\000\011\000"

let yysindex = "\005\000\
\255\254\000\000\007\255\000\000\002\255\006\255\000\255\253\254\
\008\255\009\255\004\255\003\255\010\255\014\255\000\000\000\255\
\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\001\000\000\000"

let yytablesize = 17
let yytable = "\012\000\
\008\000\009\000\013\000\017\000\018\000\001\000\003\000\005\000\
\006\000\007\000\014\000\019\000\016\000\015\000\020\000\002\000\
\021\000"

let yycheck = "\003\001\
\001\001\002\001\006\001\001\001\002\001\001\000\008\001\001\001\
\007\001\004\001\003\001\002\001\009\001\005\001\001\001\005\001\
\016\000"

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
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'gp) in
    Obj.repr(
# 20 "bin/myParser.mly"
                                                           (_5)
# 94 "bin/myParser.ml"
               : Lib.axiom list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 24 "bin/myParser.mly"
                                       (_1)
# 101 "bin/myParser.ml"
               : 'gp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'gp) in
    Obj.repr(
# 25 "bin/myParser.mly"
                                       ( List.append _1 _3 )
# 109 "bin/myParser.ml"
               : 'gp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 29 "bin/myParser.mly"
                                   ((_1 , Atomic _3) :: [] )
# 117 "bin/myParser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 30 "bin/myParser.mly"
                                   ((_1 , Exist(Property _2, Atomic _3)) :: [])
# 126 "bin/myParser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 31 "bin/myParser.mly"
                                   ((_3 , Exist(PropertyInverse _2, Atomic _1)) :: [])
# 135 "bin/myParser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 32 "bin/myParser.mly"
                                   ((_1 , Exist(Property _2, Atomic _3)) :: 
                                    (_3 , Exist(PropertyInverse _2, Atomic _1)) :: 
                                    [])
# 146 "bin/myParser.ml"
               : 'clause))
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
