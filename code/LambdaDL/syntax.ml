open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)
  (* Query *)

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

  (* Concept Expression *)
type ce = 
  Property of string 
  | PropertyInverse of string
  | Atomic of string 
  | Top 
  | Conjunction of ce * ce
  | Disjunction of ce * ce
  | Exist of ce * ce

type axiom = string * ce

(* Types *)
type ty =
   TyTop (**)
  | TyBool (**)
  | TyRecord of (string * ty) list (**)
  | TyArr of ty * ty (**)
  | TyNat (**)
  | TyList of ty
  | TyConcept of ce

type info = Info

(* Terms *)
type term =
    TmVar of info * int * int (**)
  | TmTrue of info (**)
  | TmFalse of info (**)
  | TmIf of info * term * term * term (**)
  | TmRecord of info * (string * term) list (**)
  | TmProj of info * term * string (**)
  | TmAbs of info * string * ty * term (**)
  | TmApp of info * term * term (**)
  | TmLet of info * string * term * term (**)
  | TmFix of info * term (**)
  | TmZero of info (**)
  | TmSucc of info * term (**)
  | TmPred of info * term (**)
  | TmIsZero of info * term (**)
  | TmNil of info * ty (**)
  | TmCons of info * term * term (**)
  | TmIsNil of info * term (**)
  | TmHead of info * term (**)
  | TmTail of info * term  (**)
  | TmQuery of info * var * gp
  | TmRoleProj of info * term * role
  | TmEq of info * term * term
  | TmNode of info * string

type binding =
    NameBind 
  | VarBind of ty

type context = (string * binding) list


(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x,bind)::ctx

let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | (y,_)::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

let index2name fi ctx x =
  try
    let (xn,_) = List.length ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
      [] -> error fi ("Identifier " ^ x ^ " is unbound")
    | (y,_)::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

let termShift d t = 
  let rec walk c t = match t with
    TmVar(fi,x,n)       -> if x>=c then TmVar(fi,x+d,n+d)
                            else TmVar(fi,x,n+d)
  | TmTrue(fi) as t     -> t
  | TmFalse(fi) as t    -> t
  | TmIf(fi,t1,t2,t3)   -> TmIf(fi,walk c t1,walk c t2,walk c t3)
  | TmProj(fi,t1,l)     -> TmProj(fi,walk c t1,l)
  | TmRecord(fi,fields) -> TmRecord(fi,List.map (fun (li,ti) ->(li,walk c ti)) fields)
  | TmAbs(fi,x,tyT1,t2) -> TmAbs(fi,x,tyT1,walk (c+1) t2)
  | TmApp(fi,t1,t2)     -> TmApp(fi,walk c t1,walk c t2)
  | TmLet(fi,x,t1,t2)   -> TmLet(fi,x,walk c t1,walk (c+1) t2)
  | TmFix(fi,t1)        -> TmFix(fi,walk c t1)
  | TmZero(fi)          -> TmZero(fi)
  | TmSucc(fi,t1)       -> TmSucc(fi, walk c t1)
  | TmPred(fi,t1)       -> TmPred(fi, walk c t1)
  | TmIsZero(fi,t1)     -> TmIsZero(fi, walk c t1)
  | TmNil(fi,ty) as t   -> t
  | TmCons(fi,t1,t2)    -> TmCons(fi, walk c t1, walk c t2)
  | TmIsNil(fi,t1)      -> TmIsNil(fi, walk c t1)
  | TmHead(fi,t1)       -> TmHead(fi, walk c t1)
  | TmTail(fi,t1)       -> TmTail(fi, walk c t1)
  | TmQuery(fi,var,gp) as t -> t
  | TmRoleProj(fi,t1,r) -> TmRoleProj(fi,walk c t1,r) 
  | TmEq(fi,t1,t2)      -> TmEq(fi, walk c t1, walk c t2) 
  | TmNode(fi,s) as t   -> t 
  in walk 0 t
(* ---------------------------------------------------------------------- *)
(* Substitution *)

let termSubst j s t =
  let rec walk c t = match t with
    TmVar(fi,x,n)       -> if x=j+c then termShift c s else TmVar(fi,x,n)
  | TmTrue(fi) as t     -> t
  | TmFalse(fi) as t    -> t
  | TmIf(fi,t1,t2,t3)   -> TmIf(fi,walk c t1,walk c t2,walk c t3)
  | TmProj(fi,t1,l)     -> TmProj(fi,walk c t1,l)
  | TmRecord(fi,fields) -> TmRecord(fi,List.map (fun (li,ti) ->(li,walk c ti)) fields)
  | TmAbs(fi,x,tyT1,t2) -> TmAbs(fi,x,tyT1,walk (c+1) t2)
  | TmApp(fi,t1,t2)     -> TmApp(fi,walk c t1,walk c t2)
  | TmLet(fi,x,t1,t2)   -> TmLet(fi,x,walk c t1,walk (c+1) t2)
  | TmFix(fi,t1)        -> TmFix(fi,walk c t1)
  | TmZero(fi)          -> TmZero(fi)
  | TmSucc(fi,t1)       -> TmSucc(fi, walk c t1)
  | TmPred(fi,t1)       -> TmPred(fi, walk c t1)
  | TmIsZero(fi,t1)     -> TmIsZero(fi, walk c t1)
  | TmNil(fi,ty) as t   -> t
  | TmCons(fi,t1,t2)    -> TmCons(fi, walk c t1, walk c t2)
  | TmIsNil(fi,t1)      -> TmIsNil(fi, walk c t1)
  | TmHead(fi,t1)       -> TmHead(fi, walk c t1)
  | TmTail(fi,t1)       -> TmTail(fi, walk c t1)
  | TmRoleProj(fi,t1,r) -> TmRoleProj(fi,walk c t1,r) 
  | TmEq(fi,t1,t2)      -> TmEq(fi, walk c t1, walk c t2) 
  | TmQuery(fi,var,gp) as t -> t
  | TmNode(fi,s) as t   -> t 
  in walk 0 t

let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let rec getbinding fi ctx i =
  try
    let (_,bind) = List.nth ctx i in
    bind
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))
 let getTypeFromContext fi ctx i =
   match getbinding fi ctx i with
         VarBind(tyT) -> tyT
     | _ -> error fi
       ("getTypeFromContext: Wrong kind of binding for variable " 
         ^ (index2name fi ctx i)) 
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
  | TmVar(fi,_,_)   -> fi
  | TmTrue(fi)      -> fi
  | TmFalse(fi)     -> fi
  | TmIf(fi,_,_,_)  -> fi
  | TmProj(fi,_,_)  -> fi
  | TmRecord(fi,_)  -> fi
  | TmAbs(fi,_,_,_) -> fi
  | TmApp(fi, _, _) -> fi
  | TmLet(fi,_,_,_) -> fi
  | TmFix(fi,_)     -> fi
  | TmZero(fi)      -> fi
  | TmSucc(fi,_)    -> fi
  | TmPred(fi,_)    -> fi
  | TmIsZero(fi,_)  -> fi 
  | TmNil(fi,_)     -> fi 
  | TmCons(fi,_,_)  -> fi 
  | TmIsNil(fi,_)   -> fi 
  | TmHead(fi,_)    -> fi 
  | TmTail(fi,_)    -> fi
  | TmQuery(fi,_,_) -> fi
  | TmRoleProj(fi,_,_) -> fi 
  | TmEq(fi,_,_)    -> fi
  | TmNode(fi,_)    -> fi 

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

(* TO DO
let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t = 
  match t with
    TmVar(_,_,_) -> true
  | _ -> false

let rec printty_Type outer ctx tyT = match tyT with
      tyT -> printty_ArrowType outer ctx tyT

and printty_ArrowType outer ctx  tyT = match tyT with 
    TyArr(tyT1,tyT2) ->
      obox0(); 
      printty_AType false ctx tyT1;
      if outer then pr " ";
      pr "->";
      if outer then print_space() else break();
      printty_ArrowType outer ctx tyT2;
      cbox()
  | tyT -> printty_AType outer ctx tyT

and printty_AType outer ctx tyT = match tyT with
    TyString -> pr "String"
  | TyUnit -> pr "Unit"
  | TyId(b) -> pr b
  | TyBool -> pr "Bool"
  | TyRecord(fields) ->
        let pf i (li,tyTi) =
          if (li <> ((string_of_int i))) then (pr li; pr ":"); 
          printty_Type false ctx tyTi 
        in let rec p i l = match l with 
            [] -> ()
          | [f] -> pf i f
          | f::rest ->
              pf i f; pr","; if outer then print_space() else break(); 
              p (i+1) rest
        in pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | TyTop -> pr "Top"
  | TyFloat -> pr "Float"
  | TyVar(x,n) ->
      if ctxlength ctx = n then
        pr (index2name dummyinfo ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TyNat -> pr "Nat"
  | tyT -> pr "("; printty_Type outer ctx tyT; pr ")"

let printty ctx tyT = printty_Type true ctx tyT 

let rec printtm_Term outer ctx t = match t with
    TmIf(fi, t1, t2, t3) ->
       obox0();
       pr "if ";
       printtm_Term false ctx t1;
       print_space();
       pr "then ";
       printtm_Term false ctx t2;
       print_space();
       pr "else ";
       printtm_Term false ctx t3;
       cbox()
  | TmAbs(fi,x,tyT1,t2) ->
      (let (ctx',x') = (pickfreshname ctx x) in
         obox(); pr "lambda ";
         pr x'; pr ":"; printty_Type false ctx tyT1; pr ".";
         if (small t2) && not outer then break() else print_space();
         printtm_Term outer ctx' t2;
         cbox())
  | TmLet(fi, x, t1, t2) ->
       obox0();
       pr "let "; pr x; pr " = "; 
       printtm_Term false ctx t1;
       print_space(); pr "in"; print_space();
       printtm_Term false (addname ctx x) t2;
       cbox()
  | TmFix(fi, t1) ->
       obox();
       pr "fix "; 
       printtm_Term false ctx t1;
       cbox()
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmTimesfloat(_,t1,t2) ->
       pr "timesfloat "; printtm_ATerm false ctx t2; 
       pr " "; printtm_ATerm false ctx t2
  | TmApp(fi, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space();
      printtm_ATerm false ctx t2;
      cbox()
  | TmPred(_,t1) ->
       pr "pred "; printtm_ATerm false ctx t1
  | TmIsZero(_,t1) ->
       pr "iszero "; printtm_ATerm false ctx t1
  | t -> printtm_PathTerm outer ctx t

and printtm_AscribeTerm outer ctx t = match t with
    TmAscribe(_,t1,tyT1) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space(); pr "as ";
      printty_Type false ctx tyT1;
      cbox()
  | t -> printtm_ATerm outer ctx t

and printtm_PathTerm outer ctx t = match t with
    TmProj(_, t1, l) ->
      printtm_ATerm false ctx t1; pr "."; pr l
  | t -> printtm_AscribeTerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TmString(_,s) -> pr ("\"" ^ s ^ "\"")
  | TmUnit(_) -> pr "unit"
  | TmVar(fi,x,n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmRecord(fi, fields) ->
       let pf i (li,ti) =
         if (li <> ((string_of_int i))) then (pr li; pr "="); 
         printtm_Term false ctx ti 
       in let rec p i l = match l with
           [] -> ()
         | [f] -> pf i f
         | f::rest ->
             pf i f; pr","; if outer then print_space() else break(); 
             p (i+1) rest
       in pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | TmFloat(_,s) -> pr (string_of_float s)
  | TmInert(_,tyT) -> pr "inert["; printty_Type false ctx tyT; pr "]"
  | TmZero(fi) ->
       pr "0"
  | TmSucc(_,t1) ->
     let rec f n t = match t with
         TmZero(_) -> pr (string_of_int n)
       | TmSucc(_,s) -> f (n+1) s
       | _ -> (pr "(succ "; printtm_ATerm false ctx t1; pr ")")
     in f 1 t1
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 

let prbinding ctx b = match b with
    NameBind -> ()
  | TmAbbBind(t,tyT) -> pr "= "; printtm ctx t
  | VarBind(tyT) -> pr ": "; printty ctx tyT
  | TyVarBind -> ()
  | TyAbbBind(tyT) -> pr "= "; printty ctx tyT *)


