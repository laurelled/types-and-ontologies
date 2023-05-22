type token =
  | VAR of (string)
  | OBJECT of (string)
  | ROLE of (string)
  | TYPE
  | ARROW
  | QUERY
  | AND
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> axiom list
