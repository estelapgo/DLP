
type ty =
    TyBool
  | TyNat
  | TyString
  | TyArr of ty * ty
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyList of ty
  | TyVariant of (string * ty) list
  | TyVar of string

;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term 
  | TmString of string 
  | TmConcat of term * term 
  | TmLength of term
  | TmTuple of term list
  | TmProjection of term * string
  | TmRecord of (string * term) list
  | TmHead of ty * term
  | TmTail of ty * term
  | TmCons of ty * term * term
  | TmNil of ty
  | TmisEmpty of ty * term
  | TmTag of string * term * ty
  | TmCase of term * (string * string * term) list
;;

type command = 
  Eval of term 
| Bind of string * term 
| TBind of string * ty
| Quit 
;;

type binding = 
  TyBind of ty 
| TyTmBind of (ty * term)
;;

type context =
  (string * binding) list
;;


val emptyctx : context;;
val addtbinding : context -> string -> ty -> context;;
val addvbinding : context -> string -> ty -> term -> context;;
val gettbinding : context -> string -> ty;;
val getvbinding : context -> string -> term;;

(* val string_of_ty : ty -> string;; *)
exception Type_error of string;;
val typeof : context -> term -> ty;;
(* 
val string_of_term : term -> string;; *)
exception NoRuleApplies;;

val eval : context -> term -> term;;
val execute : context  -> command -> context;;



