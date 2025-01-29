
(* TYPE DEFINITIONS *)
open Format

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

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addtbinding ctx s ty =
  (s, TyBind ty) :: ctx
;;

let addvbinding ctx s ty tm = 
  (s, TyTmBind (ty, tm)) :: ctx 
;;

let gettbinding ctx x =
  match List.assoc x ctx with 
   TyBind ty -> ty 
   | TyTmBind (ty, _) -> ty 
;;

let getvbinding ctx s = 
  match List.assoc s ctx with 
    TyTmBind (_, tm) -> tm 
  | _ -> raise Not_found 
;;

exception Type_error of string
;; 

let rec base_ty ctx ty = match ty with
  TyBool -> TyBool (* Directly return base type *)
  | TyNat -> TyNat
  | TyString -> TyString
  | TyArr (ty1, ty2) -> TyArr (base_ty ctx ty1, base_ty ctx ty2)
  | TyTuple tys -> TyTuple (List.map (base_ty ctx) tys)
  | TyRecord tys -> TyRecord (List.map (fun (s, ty) -> (s, base_ty ctx ty)) tys)
  | TyList (ty) -> TyList (base_ty ctx ty)
  | TyVariant tys -> TyVariant (List.map (fun (s, ty) -> (s, base_ty ctx ty)) tys)
  | TyVar s -> gettbinding ctx s
;;

(* Pretty-Printer*)
let rec print_type = function
  TyArr (t1, t2) ->
    open_box 1;
    print_atomic_type t1;
    close_box();
    print_string "->";
    print_space ();
    open_box 1;
    print_type t2;
    close_box ();
    ()
  | ty -> print_atomic_type ty;
    
  and print_atomic_type = function
    TyBool ->
        open_box 1;
        print_string "Bool";
        close_box ()
    | TyNat ->
        open_box 1;
        print_string "Nat";
        close_box ()
    | TyString ->
        open_box 1;
        print_string "String";
        close_box ()
    | TyTuple ts ->
        open_box 1;
        print_string "{";
        print_tuple ts;
        print_string "}";
        close_box ()
    | TyRecord ts ->
        open_box 1;
        print_string "{";
        print_record ts;
        print_string "}";
        close_box ()
    | TyVariant ts ->
        open_box 1;
        print_string "<";
        print_record ts;
        print_string ">";
        close_box ()
    | TyList ty ->
        open_box 1;
        print_string "List[";
        print_type ty;
        print_string "]";
        close_box ()
    | TyVar s ->
          open_box 1;
          print_string s;
          close_box ()
    | ty ->
        print_char '(';
        open_box 1;
        print_type ty;
        close_box ();
        print_char ')';
        ()

  and print_tuple = function
      [] -> ()
      | [x] -> print_type x
      | x :: xs ->
          print_type x;
          print_string ", ";
          print_space ();
          print_tuple xs

  and print_record = function
      [] -> ()
      | [(s, term)] -> 
          print_string s; 
          print_string " : "; 
          print_type term
    
      | (s, term) :: t ->
          print_string s;
          print_string " : ";
          print_type term;
          print_string ", ";
          print_space ();
          print_record t
  ;;

let rec print_term = function 
	TmIf (t1, t2, t3) ->
    open_box 1;
    print_string "if ";
    print_term t1;
    print_space ();
    print_string "then ";
    print_space ();
    print_term t2;
    print_space ();
    print_string "else ";
    print_space ();
    print_term t3;
    close_box ()

  | TmAbs (v, ty, t) ->
    open_box 1;
    print_string "lambda ";
    print_string v;
    print_string " : ";
    print_type ty;
    print_string ". ";
    print_space ();
    print_term t;
    close_box ()

  | TmLetIn (v, TmFix (TmAbs (v', ty', t')), t2) ->
			open_box 1;
			print_string "let rec ";
			print_string v;
			print_string " : ";
			print_type ty';
			print_string " = ";
			print_space ();
			print_term (TmFix (TmAbs (v', ty', t')));
			print_string " in ";
			print_space ();
			print_term t2;
			close_box ()

	| TmLetIn (v, t1, t2) ->
			open_box 1;
			print_string "let ";
			print_string v;
			print_string " = ";
			print_space ();
			print_term t1;
			print_string " in ";
			print_space ();
			print_term t2;
			close_box ()
      
  | term -> print_app_term term
    
  and print_app_term = function 
    TmSucc t ->
			open_box 1;
			let rec f n t' = match t' with
				TmZero -> print_string (string_of_int n)
				| TmSucc s -> f (n+1) s
				| _  -> print_string "succ "; 
								print_atomic_term t
			in f 1 t;
			close_box ()
	
	  | TmPred t ->
			open_box 1;
			print_string "pred ";
			print_space ();
			print_path_term t;
			close_box ()

	  | TmIsZero t ->
			open_box 1;
			print_string "iszero ";
			print_space ();
			print_path_term t;
			close_box ()

	  | TmConcat (t1, t2) ->  
			open_box 1;
			print_string "concat ";
			print_space ();
			print_path_term t1;
			print_string " ";
			print_space ();
			print_path_term t2;
			close_box ()

	  | TmFix t ->
			open_box 1;
			print_string "fix ";
			print_space ();
			print_path_term t;
			close_box ()

	  | TmApp (t1, t2) ->
			open_box 1;
			print_app_term t1;
			print_string " ";
			print_space ();
			print_path_term t2;
			close_box ()

	  | term -> print_path_term term

  and print_path_term = function
    TmProjection (t, s) ->
        open_box 1;
        print_path_term t;
        print_string ".";
        print_string s;
        close_box ()

    | term -> print_atomic_term term

and print_atomic_term = function
	TmTrue ->
			open_box 1;
			print_string "true";
			close_box ()
	
	| TmFalse ->
			open_box 1;
			print_string "false";
			close_box ()

	| TmVar v ->
			open_box 1;
			print_string v;
			close_box ()
	
	| TmZero ->
			open_box 1;
			print_string "0";
			close_box ()

	| TmString s ->
			open_box 1;
			print_string "\"";
			print_string s;
			print_string "\"";
			close_box ()
	
	| TmTuple ts ->
			open_box 1;
			print_string "{";
			print_tuple ts;
			print_string "}";
			close_box ()
	
	| TmRecord ts ->
			open_box 1;
			print_string "{";
			print_record ts;
			print_string "}";
			close_box ()

	| TmTag (s, t, ty) ->
			open_box 1;
			print_string "<";
			print_string s;
			print_string " = ";
			print_space ();
			print_term t;
			print_string ">";
			print_string " as ";
			print_type ty;
			close_box ()
	
	| TmCase (t, ts) ->
			open_box 1;
			print_string "case ";
			print_term t;
			print_string " of ";
			print_space ();
			print_cases ts;
			close_box ()
	| TmNil ty ->
			open_box 1;
			print_string "nil[";
			print_type ty;
			print_string "]";
			close_box ()

	| TmCons (ty, t1, t2) ->
        open_box 1;
        print_string "cons[";
        print_type ty;
        print_string "] ";
        print_app_term t1;
        print_string " ";
        let aux = function
            TmNil _ -> print_atomic_term t2
            | _ -> print_string"(";
                    print_atomic_term t2;
                    print_string ")"
        in aux t2;
        close_box ()

	| TmisEmpty (ty, t) ->
			open_box 1;
			print_string "isnil[";
			print_type ty;
			print_string "] ";
			print_space ();
			print_term t;
			close_box ()
	| TmHead (ty, t) ->
			open_box 1;
			print_string "head[";
			print_type ty;
			print_string "] ";
			print_space ();
			print_term t;
			close_box ()
	| TmTail (ty, t) ->
			open_box 1;
			print_string "tail[";
			print_type ty;
			print_string "] ";
			print_space ();
			print_term t;
			close_box ()
	
	| term -> 
			open_box 1;
			print_string "(";
			print_term term;
			print_string ")";
			close_box ()

and print_tuple = function
	[] -> ()
	| [x] -> print_term x
	| x :: xs ->
			print_term x;
			print_string ", ";
			print_space ();
			print_tuple xs

and print_record = function
	[] -> ()
	| [(s, t)] -> print_string s; print_string " = "; print_term t
	| (s, t) :: xs ->
			print_string s;
			print_string " = ";
			print_term t;
			print_string ", ";
			print_space ();
			print_record xs

and print_cases = function
	[] -> ()
	| [(name, v, t)] -> 
			print_string "<";
			print_string name;
			print_string " = ";
			print_string v;
			print_string "> => ";
			print_space ();
			print_term t
	| (name, v, t) :: xs ->
			print_string "<";
			print_string name;
			print_string " = ";
			print_string v;
			print_string "> => ";
			print_space ();
			print_term t;
			print_string " | ";
			print_space ();
			print_cases xs

let print_eval tm ty =
	open_box 1;
	print_string "- : ";
	print_type ty;
	print_string " = ";
	print_space ();
	print_term tm;
	close_box ();
	force_newline ();
	print_flush ()

let print_bind s tm ty =
	open_box 1;
	print_string s;
	print_string " : ";
	print_type ty;
	print_string " = ";
	print_space ();
	print_term tm;
	close_box ();
	force_newline ();
	print_flush ()

let print_tbind s ty =
	open_box 1;
	print_string "type ";
	print_string s;
	print_string " = ";
	print_space ();
	print_type ty;
	close_box ();
	force_newline ();
	print_flush ()
  

(* TYPE MANAGEMENT (TYPING) *)
(* 
let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyString ->
    "String"
  | TyTuple tyr ->
    let rec print = function
      [] -> ""
      | (ty::[]) -> (string_of_ty ty)
      | (ty::t)  -> (string_of_ty ty) ^ ", " ^ print t
    in "{" ^ (print tyr) ^ "}"
  | TyRecord tyr ->
    let rec print = function
      [] -> ""
      |((s,ty)::[]) -> s ^ ":" ^ (string_of_ty ty)
      |((s,ty)::t) -> s ^ ":" ^ (string_of_ty ty) ^ "," ^ print t
    in "{" ^ (print tyr) ^ "}" 

| TyList ty -> "[" ^ string_of_ty ty ^ "]"
  | TyVariant l -> 
    let rec loop s tup =
      match tup with
        | [] -> s
        | [(str, t)] -> (s ^ (str ^ ": " ^ string_of_ty t))
        | (str, t) :: tail -> loop (s ^ (str ^ ": " ^ string_of_ty t ^ "," )) tail
    in (loop "<" l ) ^ ">"   
    | TyVar s -> s
;; *)

(* Subtyping*)

let rec subtypeof tm1 tm2 = 
  match (tm1, tm2) with
  | (TyArr(s1, s2), TyArr(t1, t2)) -> 
      (subtypeof t1 s1) && (subtypeof s2 t2)
  | (TyRecord(l1), TyRecord(l2)) ->
      let rec contains l1 l2 = 
        match l1 with
        | [] -> true
        | (name, ty)::rest -> 
            (try 
               let ty' = List.assoc name l2 in
               subtypeof ty ty'
             with Not_found -> false)
            && contains rest l2
      in contains l1 l2
  | (tm1, tm2) -> tm1 = tm2
;;


let rec typeof ctx tm = 

  let rec obtTy = function
  TyVar t -> gettbinding ctx t
  | s -> s in

  let rec tyarrowtolarge = function
    TyArr (t1,t2) -> (TyArr(tyarrowtolarge t1,tyarrowtolarge t2))
    | TyList t -> TyList (obtTy t)
    | s -> (obtTy s) in

  match tm with

    (* T-True *)
  TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding ctx x with 
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
    | TmAbs (x, tyT1, t2) ->
      let ctx' = addtbinding ctx x tyT1 in
        let tyT1' = base_ty ctx tyT1 in
          let tyT2 = typeof ctx' t2 in
            TyArr (tyT1', tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
              if subtypeof tyT11 tyT2 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
      typeof ctx' t2

    (* T-Fix*)
  | TmFix t1 -> 
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyArr (tyT11, tyT12) ->
          if subtypeof tyT11 tyT12 then tyT12
          else raise (Type_error "result of body not compatible with domain")
      | _ -> raise (Type_error "arrow type expected"))

    (* T-String - new rule for string*)
  |TmString _-> 
    TyString 

    (* Tm-Concat - new rule for string*)
  |TmConcat (t1, t2) -> 
    if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString 
    else raise (Type_error "argument o concsat is not a string")

    (* Tm-Length - new rule for string*)
  | TmLength t1 ->
      if typeof ctx t1 = TyString then TyNat
      else raise (Type_error "argument of length is not a string")

    (* T-Tuple *)
  | TmTuple tml ->
    let rec get_types = function
        [] -> []
        | (tm::t) -> ((typeof ctx tm)::(get_types t))
    in TyTuple (get_types tml)

    (* T-Record*)  
  | TmRecord tmr ->
    let rec get_types = function 
      [] -> []
      |((s,tm)::t) -> ((s,typeof ctx tm)::(get_types t))
    in TyRecord (get_types tmr) 

    (* T-Projection *)
  | TmProjection(t, n) ->
    (match (typeof ctx t, n) with
      | (TyRecord (tyr), s) ->
        (try List.assoc s tyr with
         _ -> raise (Type_error ("cannot project "^ s ^ ",this key does not exist in the record"))) 
        | (TyTuple (tyr), s) ->
          (try List.nth tyr (int_of_string s - 1) with
             _ -> raise (Type_error ("cannot project " ^ s ^ ", this key does not exist in the tuple")))
            | _ -> raise (Type_error "Projection is only supported on tuples"))

    (* T-Cons*)
  | TmCons (ty,h,t) ->
        let tyTh = obtTy(typeof ctx h) in
          let tyTt = obtTy(typeof ctx t) in
            if ((subtypeof (tyarrowtolarge(tyTh)) (tyarrowtolarge(obtTy(ty)))) && (subtypeof (TyList(tyarrowtolarge((obtTy ty)))) (tyarrowtolarge(tyTt)))) then 
              TyList(ty) else raise (Type_error "elements of list have different types")

    (* T-Head*)
  | TmHead (ty,t) ->
      if typeof ctx t = TyList(ty) then ty
      else raise (Type_error ("argument of head is not a List"))

    (* T-Tail*)
  | TmTail (ty,t) ->
      if typeof ctx t = TyList(ty) then TyList(ty)
      else raise (Type_error ("argument of tail is not a List"))

    (* T-Nil*)
  | TmNil ty ->
      TyList ty

    (* T-isEmpty*)
  | TmisEmpty (ty,t) ->
      if typeof ctx t = TyList(ty) then TyBool
      else raise (Type_error ("argument of isempty is not a List"))

    (* T-Variant*)
  | TmTag (s, t1, ty) ->
    let tyT1 = base_ty ctx ty in
      (match tyT1 with
        TyVariant ty' ->
          if List.mem_assoc s ty' then
            if List.assoc s ty' = typeof ctx t1 then tyT1
            else raise (Type_error "field does not match")
          else raise (Type_error "field not found")
        | _ -> raise (Type_error "expected variant type"))

    (* T-Case *)
  | TmCase (tm,(nametm,p,t1)::l) ->
    let rec buscartipovariable n = function
      TyVariant [] -> raise (Type_error ("Error1"))
      | TyVariant ((name,ty1)::t) -> if (name=n) then obtTy(ty1) else (buscartipovariable n (TyVariant t))
      | TyVar s ->(buscartipovariable n (gettbinding ctx s))
      | _-> raise (Type_error ("Error2")) in
    let tipopname = buscartipovariable nametm (obtTy(typeof ctx tm)) in
    let ctx2 = (addtbinding ctx p tipopname) in
    let tyHead = typeof ctx2 t1 in
    let rec tipo t2 = function
      [] -> obtTy(t2)
      | (pname2,pname,t1)::t ->let ctx' = (addtbinding ctx pname (buscartipovariable pname2 (obtTy(typeof ctx tm)))) in if(t2=(typeof ctx' t1)) then (tipo t2 t) else (raise (Type_error ("tipos imcompatibles en el case")))
    in (tipo tyHead l)

  | TmCase (_,[]) -> (raise (Type_error ("cases required")))
 ;;

(* TERMS MANAGEMENT (EVALUATION) *)
(* 
let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      "var" ^ s
  | TmAbs (s, tyS, t) ->
      "\n(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "app(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
        "fix " ^ string_of_term t
  | TmString s -> 
      "\"" ^ s ^ "\""
  | TmConcat (t1, t2) -> 
      "(concat " ^ string_of_term t1 ^ " " ^ string_of_term t2 ^")"
  | TmTuple list ->
    let rec aux = function
      | [] -> ""
      | [h] -> string_of_term h
      | h::t -> string_of_term h ^ ", " ^ aux t
    in "{" ^ aux list ^ "}"

  | TmRecord (list) ->
      let rec aux = function
        | [] -> ""
        | [(i, h)] -> i ^ " : " ^ string_of_term h
        | (i, h)::t -> i ^ " : " ^ string_of_term h ^ ", " ^ aux t
      in "{" ^ aux list ^ "}"

  | TmProjection (t,s) ->
    string_of_term t ^ "." ^ s   

  | TmCons(ty,h,t) -> 
      "cons[" ^ string_of_ty ty ^ ":" ^ string_of_term h ^ "," ^ (string_of_term t) ^ "]"
  | TmHead (ty,t) ->
      "head[" ^ string_of_ty ty ^ "]" ^ "(" ^ string_of_term t ^ ")"

  | TmTail (ty,t) ->
      "tail[" ^ string_of_ty ty ^ "]" ^ "(" ^ string_of_term t ^ ")"
  | TmNil ty ->
      "nil[" ^ string_of_ty ty ^ "]"
  | TmisEmpty (ty,t) ->
     "isempty[" ^ string_of_ty ty ^ "]"   
  | TmTag (str, t, ty) ->
      "<" ^ str ^ " = " ^ string_of_term t ^ ">"   
  | TmCase(t, l) -> 
      let string = " case " ^ string_of_term t ^ " of " in
      let rec aux list string2 = match list with
    [] -> string2
  | (s1, s2, term)::tail -> 
    let string3 = string2 ^ "<" ^ s1 ^ "," ^ s2 ^ "> => " ^ string_of_term term in
    aux tail string3
    in aux l string  
  | TmLength t ->
      "(length " ^ string_of_term t ^ ")"
;; *)

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
        free_vars t
  |TmString _->
    []
  | TmConcat (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmLength t ->
      free_vars t
  | TmTuple t ->
      let rec aux list = match list with
        | [] -> []
        | h::[] -> free_vars h
        | h::t -> lunion (free_vars h) (aux t)
      in aux t
  | TmRecord t ->
    let rec aux list = match list with
      | (i, h)::[] -> free_vars h
      | (i, h)::t -> lunion (free_vars h) (aux t)
      | [] -> []
    in aux t
  | TmProjection (t,_) ->
      free_vars t
  | TmCons(ty,h,t) ->
      lunion (free_vars h) (free_vars t)
  | TmHead (ty,t) ->
      free_vars t
  | TmTail (ty,t) ->
      free_vars t
  | TmNil ty ->
    []
  | TmisEmpty (ty,t) ->
      free_vars t
  | TmTag (_, t, _) ->
    free_vars t
  | TmCase (t, cases) ->
    let rec get_free = function
    [] ->[]  
    | ((_,s,term)::[]) -> (ldif (free_vars term) [s])
    |((_,s,term)::termt) -> lunion (ldif (free_vars term) [s]) (get_free termt)
   in (lunion (free_vars t) (get_free cases))
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t -> 
    TmFix (subst x s t)            
  | TmString st -> 
    TmString st
  | TmConcat (t1, t2) -> 
    TmConcat (subst x s t1, subst x s t2)
  | TmLength t ->
      TmLength (subst x s t)
  | TmTuple tml ->
    let rec subs_rcd = function
      [] -> []
      |(tm::t) -> (subst x s tm)::(subs_rcd t)
    in TmTuple (subs_rcd tml)

  | TmRecord tmr ->
      let rec subst_rec = function 
        [] -> []
        |((st,tm)::t) -> (st,(subst x s tm))::(subst_rec t)
      in TmRecord (subst_rec tmr)
       
  | TmProjection(t,n) ->
        TmProjection  (subst x s t, n)
  | TmCons(ty,h,t) ->
      TmCons(ty,(subst x s h),(subst x s t))
  | TmHead (ty,t) ->
      TmHead (ty,(subst x s t))
  | TmTail (ty,t) ->
      TmTail (ty,(subst x s t))
  | TmNil ty ->
      tm
  | TmisEmpty (ty,t) ->
      TmisEmpty(ty,subst x s t)
  | TmTag (name, t, ty) ->
      TmTag (name, subst x s t, ty)
  | TmCase (t1, l) ->
        let rec sustincases = function 
        []-> []
        | ((s1,s2,ter)::t) -> ((s1,s2,(subst x s ter))::(sustincases t))
        in (TmCase ((subst x s t1), (sustincases l)))
    ;;
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | t when isnumericval t -> true
  | TmTuple list -> List.for_all(fun t -> isval(t)) list
  | TmRecord list -> List.for_all (fun t -> isval t) (List.map snd list)
  | TmCons(_,h,t) -> (isval h) && (isval t)
  | TmNil _-> true
  | TmTag (_, t, _) -> isval t
  | TmCase (t, cases) -> isval t && List.for_all (fun (_, _, t) -> isval t) cases
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12 

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)

    (* E-FixBeta*)
  | TmFix (TmAbs (x, _, t2)) -> 
      subst x tm t2 

    (*E-Fix*)
  | TmFix t1 -> 
      let t1' = eval1 ctx t1 in 
      TmFix t1' 

    (* new rule for string*)
  |TmConcat (TmString s1, TmString s2) -> 
    TmString (s1 ^ s2 )

    (* new rule for string*)
  |TmConcat (TmString s1, t2) ->
    let t2' = eval1 ctx t2 in 
    TmConcat (TmString s1, t2') 
  
    (* new rule for string*)
  | TmConcat (t1, t2) -> 
    let t1' = eval1 ctx t1 in 
    TmConcat (t1', t2)

    (* new rule for string*)
  | TmLength (TmString s1) ->
    let length = String.length s1 in
    let rec term_of_int n = 
      if n = 0 then
        TmZero
      else
        TmSucc (term_of_int (n - 1))
    in term_of_int length
  
  | TmLength t1 ->
      let t1' = eval1 ctx t1 in
      TmLength t1' 

  | TmVar s ->    (* Global context*)
    getvbinding ctx s 

    (* E-Projection *)    
  | TmProjection (TmTuple list as v, s) when isval v ->
      List.nth list (int_of_string s - 1)
    
  | TmProjection (TmRecord list as v, s) when isval v ->
      List.assoc s list
    
  | TmProjection (t, s) ->
      let t' = eval1 ctx t in TmProjection (t', s)
  
    (* E-Tuple*)
  | TmTuple tml -> 
    let rec eval_rcd = function
      [] -> raise NoRuleApplies
      |(tm::t) when isval tm -> tm::(eval_rcd t)
     |(tm::t) -> (eval1 ctx tm)::t
    in TmTuple (eval_rcd tml)

    (* E-Record*)
  | TmRecord tmr ->
      let rec eval_rec = function
        [] -> raise NoRuleApplies
        |((s,tm)::t) when isval tm -> (s,tm)::(eval_rec t)
        |((s,tm)::t) -> (s,(eval1 ctx tm))::t
      in TmRecord (eval_rec tmr)

    (* E-Cons*)
  | TmCons(ty,h,t) when isval h -> TmCons(ty,h,(eval1 ctx t))

  | TmCons(ty,h,t) -> TmCons(ty,(eval1 ctx h),t)

    (* E-Head*)
  | TmHead(ty,TmCons(_,h,_)) -> h

  | TmHead (ty,t) ->
      TmHead(ty, eval1 ctx t)

    (* E-Tail*)
  | TmTail(ty,TmCons(_,_,t)) -> t

  | TmTail (ty,t) ->
      TmTail(ty, eval1 ctx t)
    
    (*E-isEmpty*)
  | TmisEmpty(ty,TmNil(_)) -> TmTrue
  | TmisEmpty(ty,TmCons(_,_,_)) -> TmFalse
  | TmisEmpty(ty,t) -> TmisEmpty(ty,eval1 ctx t)

    (* E-Tag *)
  | TmTag (name, t, ty) ->
    let t' = eval1 ctx t in
    TmTag (name, t', ty)

    (* E-Case *)
  | TmCase ((TmTag(s, t1, ty)),l)  ->
    let rec buscar = function
    [] -> raise (Type_error ("Error"))
    | (namePos,nameVar,t2)::t -> if (namePos=s) then (nameVar,t2) else (buscar t)
    in let (nameVar,t2) = buscar l in
    (subst nameVar t1 t2) 
  | TmCase (_,_) ->raise (Type_error ("Error"))

  | _ ->
      raise NoRuleApplies
;;

let apply_ctx ctx tm =  (* Global context*)
  List.fold_left (fun t x -> subst x (getvbinding ctx x) t) tm (free_vars tm)
;;

let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;

let execute ctx = function  
  | Eval tm ->
    let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
        (* print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');*)   
        print_eval tm' tyTm;
        ctx 

  | Bind (s, tm) ->
    let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
        (* print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');*)
        print_bind s tm' tyTm;
        addvbinding ctx s tyTm tm'

  | TBind (s, ty) ->
    let bty = base_ty ctx ty in 
    (* print_endline (s ^ " : type = " ^ string_of_ty bty); *)
    print_tbind s bty;
    addtbinding ctx s bty

  | Quit -> 
    raise End_of_file
;;

  
