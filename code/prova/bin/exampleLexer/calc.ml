 (* File calc.ml *)
 let _ =
  let stramFile = open_in "exp.txt" in 
    try
      let lexbuf = Lexing.from_channel stramFile in
        let result = Parser.main Lexer.token lexbuf in
          print_int result; print_newline(); flush stdout;
    with Lexer.Eof ->
      exit 0