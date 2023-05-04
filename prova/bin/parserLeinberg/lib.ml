
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
