(* File lexer.mll *)
{
  open MyParser        (* The type token is defined in parser.mli *)
  exception Eof
}
  rule token = parse 
      [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]        { EOL }
(*variabili lettera singola, role lettera minuscola inizio
object lettera maiuscola 
    | ['A'-'z']+ as role {ROLE (role)} 

  
      *)
    | "query"        {QUERY}
    | "type"         {TYPE}
    | "AND"          {AND}
    | ['a'-'z'] as v    {VAR (Char.escaped v)}
    | ['a' - 'z'](['a'-'z'] | ['A'-'Z'])+ as role {ROLE (role)}
    | ['a' - 'z'](['a'-'z'] | ['A'-'Z'])+['-'] as invrole {INVROLE (invrole)}
    | ['A' - 'Z'](['a'-'z'] | ['A'-'Z'])+ as obj {OBJECT (obj)}
    | '('            {LPAREN}
    | ')'            {RPAREN}
    | "<-"           {ARROW}
    | eof            {raise Eof}