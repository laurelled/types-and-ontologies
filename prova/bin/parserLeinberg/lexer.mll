(* File lexer.mll *)
{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}
  rule token = parse 
      [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]        { EOL }
(*variabili lettera singola, role lettera minuscola inizio
object lettera maiuscola *)
    | ['a'-'z'] as v    {VAR (Char.escaped v)}
    | ['a'-'z']['A'-'z']+ as role {ROLE (role)} 
    | ['A' - 'Z']['a'-'z']+ as obj {OBJECT (obj)}   
    | "query"        {QUERY}
    | "<-"           {ARROW}
    | "type"         {TYPE}  
    | "AND"          {AND}
    | eof            {raise Eof}