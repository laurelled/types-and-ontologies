(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error


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
 
   (* Term *)
 type ty =
    TyTop (**)
   | TyBool (**)
   | TyRecord of (string * ty) list (**)
   | TyArr of ty * ty (**)
   | TyNat (**)
   | TyList of ty
   | TyConcept of ce
 
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
   | TmRoleProj of info * term * role
   | TmEq of info * term * term
   | TmQuery of info * var * gp
   | TmNode of info * string

type binding =
    NameBind 
  | VarBind of ty

(* Contexts *)
type context
val emptycontext : context 
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val getbinding : info -> context -> int -> binding
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val getTypeFromContext : info -> context -> int -> ty

(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term

(* Printing *)
(* TO DO
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val printty : context -> ty -> unit
val prbinding : context -> binding -> unit

(* Misc *)
val tmInfo: term -> info*)

