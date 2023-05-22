(* File calc.ml *)
 let _ =
  let stramFile = open_in "../queryFile.txt" in 
    try
      let lexbuf = Lexing.from_channel stramFile in
        let queryToken = MyParser.main MyLexer.token lexbuf in
          let axiomList = Lib.axiomizerQuery queryToken in
            let result = Lib.isHabitated axiomList in 
          Printf.printf "%s" result
    with MyLexer.Eof ->
      exit 0