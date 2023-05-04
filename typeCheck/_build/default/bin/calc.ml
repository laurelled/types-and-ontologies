(* File calc.ml *)
 let _ =
  let stramFile = open_in "exp.txt" in 
    try
      let lexbuf = Lexing.from_channel stramFile in
        let result = MyParser.main MyLexer.token lexbuf in
          Printf.printf "%s" (Lib.isHabitated result)
    with MyLexer.Eof ->
      exit 0