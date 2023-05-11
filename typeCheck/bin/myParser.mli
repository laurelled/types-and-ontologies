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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lib.query
