
type classExpression = 
Property of string 
| PropertyInverse of string
| Atomic of string 
| Top 
| Conjunction of classExpression * classExpression 
| Exist of classExpression * classExpression

let rec stringfy (c : classExpression) : string =
  match c with 
  | Property s -> s
  | PropertyInverse s -> "INVERSE " ^ s 
  | Atomic s -> s
  | Top -> "T"
  | Conjunction (c1 , c2) -> (stringfy c1) ^ " AND " ^ (stringfy c2)
  | Exist (c1 , c2) -> (stringfy c1) ^ " SOME " ^ (stringfy c2);;
  

type axiom = string * classExpression

let rec stringfyAxiom (list : axiom list) : string =
  match list with
  | [] -> ""
  | ( (a,b) :: []) ->  a ^ " : " ^ (stringfy b)
  | ( (a,b) :: (x :: xs)) ->  a ^ " : " ^ (stringfy b) ^ " : " ^ stringfyAxiom (x :: xs);;


  let isHabitated  classEx =
  let param = stringfyAxiom classEx in
  let tmp_file = Filename.temp_file "" ".txt" in
  let _ = Sys.command @@ "java -classpath ../Reasoner/out/production/Reasoner:../Reasoner/HermiT.jar Main " ^ 
    "\"" ^ param ^ "\" >" ^ tmp_file in
  let chan = open_in tmp_file in
  let s = input_line chan in
    close_in chan;
    s;;