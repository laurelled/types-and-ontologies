open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval ctx t  -> true
  | TmAbs(_,_,_,_) -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
  | TmNil(_,_) -> true
  | TmCons(l,t) -> isval ctx l && isval ctx t
  | TmNode(_,_) -> true
  | _ -> false


let rec eval1 ctx t = match t with
   TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmRecord(fi,fields) ->
      let rec evalafield l = match l with 
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi -> 
          let rest' = evalafield rest in
          (l,vi)::rest'
      | (l,ti)::rest -> 
          let ti' = eval1 ctx ti in
          (l, ti')::rest
      in let fields' = evalafield fields in
      TmRecord(fi, fields')
  | TmProj(fi, TmRecord(_, fields), l) ->
      (try List.assoc l fields
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
      termSubstTop v1 t2 
  | TmLet(fi,x,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmLet(fi, x, t1', t2) 
  | TmFix(fi,v1) as t when isval ctx v1 ->
      (match v1 with
         TmAbs(_,_,_,t12) -> termSubstTop t t12
       | _ -> raise NoRuleApplies)
  | TmFix(fi,t1) ->
      let t1' = eval1 ctx t1
      in TmFix(fi,t1')
  | TmSucc(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero(fi,t1')
  | TmHead(_, TmCons(fi,t1,t2)) -> t1
  | TmHead(fi,t1) -> 
      let t1' = eval1 ctx t1 in
      TmHead(fi,t1')
  | TmTail(_, TmCons(fi,t1,t2)) -> t2 
  | TmTail(fi,t1) -> 
      let t1' = eval1 ctx t1 in
      TmTail(fi,t1')
  | TmCons(fi,t1,t2) -> 
      let t1' = eval1 ctx t1 in
      TmCons(fi,t1',t2)
  | TmCons(fi,t1,t2) -> 
      let t2' = eval1 ctx t2 in
      TmCons(fi,t1,t2')
  | TmIsNil(fi,TmNil(_,_)) -> TmTrue(fi)
  | TmIsNIl(fi,TmCons(_,_,_)) -> TmFalse(fi)
  | TmIsNil(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmIsNil(fi,t1')
  | TmRoleProj(fi,TmNode(fi, s),P(r)) -> 
      let t1' = executeQuery TmQuery(fi, V("x"), OP(O(s), P(r), V("x") )) in
      List.fold_left (fun l r -> TmCons(fi,l,TmNode(fi,r))) 
                     TmNil(fi, TyConcept(Exist(PropertyInverse(s), c)))
                     t1'
  | _ -> 
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t

(* ------------------------   SUBTYPING  ------------------------ *)

let isConcept ty1 = function
  | TyConcept(_) -> true
  | _            -> false

(* To Do *)
let conceptInclusion ce1 ce2 = true

(* Syntactic Equivalence *)
let rec ceeqv ce1 ce2 =
  match (ce1, ce2) with
    (PropertyInverse(s1),PropertyInverse(s2)) -> s1 == s2
  | (Property(s1),Property(s2)) -> s1 == s2
  | (Atomic(s1),Atomic(s2)) -> s1 == s2
  | (Top,Top) -> true
  | (Conjunction(ceS1,ceS2),Conjunction(ceT1,ceT2)) ->
      (ceeqv ceS1 ceT1) && (ceeqv ceS2 ce ceT2)
  | (Disjunction(ceS1,ceS2),Disjunction(ceT1,ceT2)) ->
      (ceeqv ceS1 ceT1) && (ceeqv ceS2 ce ceT2)
  | (Exist(ceS1,ceS2), Exist(ceT1,ceT2)) ->
      (ceeqv ceS1 ceT1) && (ceeqv ceS2 ce ceT2)

let rec tyeqv ctx tyS tyT =
  match (tyS,tyT) with
    (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | (TyTop,TyTop) -> true
  | (TyBool,TyBool) -> true
  | (TyNat,TyNat) -> true
  | (TyRecord(fields1),TyRecord(fields2)) -> 
       List.length fields1 = List.length fields2
       &&                                         
       List.for_all 
         (fun (li2,tyTi2) ->
            try let (tyTi1) = List.assoc li2 fields1 in
                tyeqv ctx tyTi1 tyTi2
            with Not_found -> false)
         fields2
  | (TyList(tyS1),TyList(tyS2)) -> tyeqv ctx tyS1 tyS2
  | (TyConcept(ceS1),TyConcept(ceT1)) -> ceeqv ceS1 ceT1
  | _ -> false

let rec subtype ctx tyS tyT =
   tyeqv ctx tyS tyT ||
   match (tyS,tyT) with
     (_,TyTop) -> 
       true
   | (TyRecord(fS), TyRecord(fT)) ->
       List.for_all
         (fun (li,tyTi) -> 
            try let tySi = List.assoc li fS in
                subtype ctx tySi tyTi
            with Not_found -> false)
         fT
   | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       (subtype ctx tyT1 tyS1) && (subtype ctx tyS2 tyT2)
   | (TyList(tyS1),TyList(tyT1)) -> subtype ctx tyS1 tyT1
   | (TyConcept(ceS1),TyConcept(ceT1)) -> conceptInclusion(ceS1,ceT1)
   | (_,_) -> 
       false

let rec join ctx tyS tyT =
  if subtype ctx tyS tyT then tyT else 
  if subtype ctx tyT tyS then tyS else
  match (tyS,tyT) with
    (TyRecord(fS), TyRecord(fT)) ->
      let labelsS = List.map (fun (li,_) -> li) fS in
      let labelsT = List.map (fun (li,_) -> li) fT in
      let commonLabels = 
        List.find_all (fun l -> List.mem l labelsT) labelsS in
      let commonFields = 
        List.map (fun li -> 
                    let tySi = List.assoc li fS in
                    let tyTi = List.assoc li fT in
                    (li, join ctx tySi tyTi))
                 commonLabels in
      TyRecord(commonFields)
  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
      (try TyArr(meet ctx tyS1 tyT1, join ctx tyS2 tyT2)
        with Not_found -> TyTop)
  | (TyList(tyS1),TyList(tyT1)) -> TyList(join ctx tyS1, tyT1)
  | (TyConcept(ceS1),TyConcept(ceT1)) -> TyConcept(Conjunction(ceS1 ceT1))
  | _ -> TyTop

and meet ctx tyS tyT =
  if subtype ctx tyS tyT then tyS else 
  if subtype ctx tyT tyS then tyT else 
  match (tyS,tyT) with
    (TyRecord(fS), TyRecord(fT)) ->
      let labelsS = List.map (fun (li,_) -> li) fS in
      let labelsT = List.map (fun (li,_) -> li) fT in
      let allLabels = 
        List.append
          labelsS 
          (List.find_all 
            (fun l -> not (List.mem l labelsS)) labelsT) in
      let allFields = 
        List.map (fun li -> 
                    if List.mem li allLabels then
                      let tySi = List.assoc li fS in
                      let tyTi = List.assoc li fT in
                      (li, meet ctx tySi tyTi)
                    else if List.mem li labelsS then
                      (li, List.assoc li fS)
                    else
                      (li, List.assoc li fT))
                 allLabels in
      TyRecord(allFields)
  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
      TyArr(join ctx tyS1 tyT1, meet ctx tyS2 tyT2)
  | (TyList(tyS1),TyList(tyT1)) -> TyList(meet ctx tyS1 tyT1) 
  | (TyConcept(ceS1),TyConcept(ceT1)) -> TyConcept(Disjunction(ceS1 ceT1))
  | _ -> 
      raise Not_found

(* ------------------------   TYPING  ------------------------ *)

let rec typeof ctx t =
  match t with
   TmTrue(fi) -> 
      TyBool
  | TmFalse(fi) -> 
      TyBool
  | TmIf(fi,t1,t2,t3) ->
      if subtype ctx (typeof ctx t1) TyBool then
        join ctx (typeof ctx t2) (typeof ctx t3)
      else error fi "guard of conditional not a boolean"
  | TmRecord(fi, fields) ->
      let fieldtys = 
        List.map (fun (li,ti) -> (li, typeof ctx ti)) fields in
      TyRecord(fieldtys)
  | TmProj(fi, t1, l) ->
      (match typeof ctx t1 with
          TyRecord(fieldtys) ->
            (try List.assoc l fieldtys
             with Not_found -> error fi ("label "^l^" not found"))
        | _ -> error fi "Expected record type")
  | TmVar(fi,i,_) -> getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT1,t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, tyT2)
  | TmApp(fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match ctx tyT1 with
          TyArr(tyT11,tyT12) ->
            if subtype ctx tyT2 tyT11 then tyT12
            else error fi "parameter type mismatch"
        | _ -> error fi "arrow type expected")
  | TmLet(fi,x,t1,t2) ->
     let tyT1 = typeof ctx t1 in
     let ctx' = addbinding ctx x (VarBind(tyT1)) in         
        typeof ctx' t2
  | TmFix(fi, t1) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
           TyArr(tyT11,tyT12) ->
             if subtype ctx tyT12 tyT11 then tyT12
             else error fi "result of body not compatible with domain"
         | _ -> error fi "arrow type expected")
  | TmZero(fi) ->
      TyNat
  | TmSucc(fi,t1) ->
      if subtype ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of succ is not a number"
  | TmPred(fi,t1) ->
      if subtype ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero(fi,t1) ->
      if subtype ctx (typeof ctx t1) TyNat then TyBool
      else error fi "argument of iszero is not a number"
  | TmNil(fi,ty) -> ty
  | TmCons(fi,tyS1,tyS2) ->
      let tyS1' = typeof ctx tyS1 in
      let tyS2' = typeof ctx tyS2 in
        join ctx tyS1' tyS2'
  | TmIsNil(fi,t1) ->
      let ty = typeof ctx t1 in
      (match ty with
          TyList(_) -> TyBool
        | _ -> error fi "argument of isNil is not a list")
  | TmHead(fi,t1) -> 
      let ty = typeof ctx t1 in
        (match ty with
            TyList(ty1) -> ty1
          | _ -> error fi "argument of head is not a list")
  | TmTail(fi,t1) ->
      let ty = typeof ctx t1 in
      (match ty with
          TyList(ty1) as ty2 -> ty2
        | _ -> error fi "argument of isNil is not a list")
  | TmQuery(fi, var, gp) -> 
        if allSatisfiable(axioms(q), var) then
        TyList(TyRecord(var, TyConcept(Atomic var))) else
            error fi "axioms unsatisibale"
  | TmRoleProj(fi, t, P(s)) -> 
        let ty1 = typeof ctx t in
        (match ty1 with
            TyConcept(c) as t -> TyConcept(Exist(PropertyInverse(s), c)))
            | _ -> error fi "argument of role projection is not a Concept Expression"
  | TmEq(fi, t1, t2) -> 
    let ty1 = typeof ctx t1 in
    let ty2 = typeof ctx t2 in
        (match (ty1, ty2) with
              (TyConcept(_), TyConcept(_)) -> TyBool
            | (ty1', ty2') -> if tyeqv ty1' ty2' then 
                    TyBool else
                    error fi "argument of = have different types"
            | _ ->  error fi "argument of = have different types")
  | _ -> error "no type found"
