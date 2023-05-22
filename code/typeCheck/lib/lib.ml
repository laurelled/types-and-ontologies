type var = V of string
type obj = O of string
type role = P of string | I of string | Type

type gp =
  CP of gp * gp 
  | SP of var * role * obj
  | OP of obj * role * var
  | DP of var * role * var 

type query = 
  Q of var * gp


(****************************************************************************)  
type classExpression = 
Property of string 
| PropertyInverse of string
| Atomic of string 
| Top 
| Conjunction of classExpression * classExpression 
| Exist of classExpression * classExpression

type axiom = string * classExpression
(****************************************************************************)  

  
let stringfyRole (r : role) : string = 
  match r with 
  | P prop -> prop
  | I prop -> prop ^ "-"    
  | Type -> "type"

(*
    x hasTopping GorgonzolaTopping -> x , Ex hasTopping GorgonzolaTopping
    GorgonzolaTopping hasTopping- x -> x , Ex hasTopping-- GorgonzolaTopping
*)

let rec axiomizerGP (gp : gp) : axiom list = 
  match gp with 
  (*QT-conj*)
    | CP (g1 , g2) -> List.append (axiomizerGP g1) (axiomizerGP g2)
  (*QT-type*)
    | SP (V var , Type , O obj) -> (var , Atomic obj) :: []
  (*QT-role1*)  
    | SP (V var , P role , O obj) -> (var , Exist (Property role , Atomic obj)) :: []
    | SP (V var , I role , O obj) -> (var , Exist (PropertyInverse role , Atomic obj)) :: []    
  (*QT-role2*)  
    | OP (O obj , P role , V var) -> (var , Exist (PropertyInverse role , Atomic obj)) :: []
    | OP (O obj , I role , V var) -> (var , Exist (Property role , Atomic obj)) :: [] (*Inverse di inverse*)
  (*QT-role3*)  
    | DP (V var1 , P role , V var2) -> (var1 , Exist (Property role , Atomic var2)) :: (var2 , Exist (PropertyInverse role , Atomic var1)) :: []
    | DP (V var1 , I role , V var2) -> (var1 , Exist (PropertyInverse role , Atomic var2)) :: (var2 , Exist (Property role , Atomic var1)) :: []

    | _ -> []
 

let axiomizerQuery (q : query) : axiom list = 
  match q with
  | Q (_ , gp) -> axiomizerGP gp 
  



let rec stringfy (c : classExpression) : string =
  match c with 
  | Property s -> s
  | PropertyInverse s -> "INVERSE " ^ s 
  | Atomic s -> s
  | Top -> "T"
  | Conjunction (c1 , c2) -> (stringfy c1) ^ " AND " ^ (stringfy c2)
  | Exist (c1 , c2) -> (stringfy c1) ^ " SOME " ^ (stringfy c2);;
  


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