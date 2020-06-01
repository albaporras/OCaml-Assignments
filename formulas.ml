(** Propositional formulas *)

(** A type representing propositional formulas. *)

type formula =
  | Top
  | Var of Variable.var
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula

(**Top *)
let top (phi:formula) : formula =
	Top;;

(** Bot *)
let bot (phi:formula) : formula =
	Neg Top ;;

(** Negation. *)
let neg (phi:formula) : formula =
	Neg(phi);;

(** Conjunction. *)
let (&&&) (phi: formula) (psi:formula) : formula =
	And (phi, psi);;

(** Disjunction. *)
let (|||) (phi:formula) ( psi:formula) : formula =
	Or (phi, psi);;

(** Implication. *)
let (-->) (phi:formula) (psi:formula) :formula =
	Neg phi ||| psi;;

(** Equivalence. *)
let (<->) (phi:formula) (psi: formula) : formula =
	(phi--> psi) &&& (psi --> phi);;

(** Prints formulas. *)
let rec string_of_formula (phi : formula) : string =
  match phi with
  | Top -> "Top"
  | Var x -> string_of_int x (*for some reason I can't use the function strin_of_var *)
  | Neg psi -> "~" ^ (string_of_formula psi)
  | And (psi1, psi2) -> "(" ^ (string_of_formula psi1) ^ " & " ^ (string_of_formula psi2) ^ ")"
  | Or (psi1, psi2) -> "(" ^ (string_of_formula psi1) ^ " V " ^ (string_of_formula psi2) ^ ")"

(** is_tauto*)
let rec is_tauto (phi : formula) : bool =
  let variables : var list = get_variables phi in
  match variables with
  | [] -> eval phi
  | x :: _ ->
    is_tauto (replace x false phi) && is_tauto (replace x true phi)


let rec replace (x : var) (value : bool) (phi : formula) : formula =
match phi with
|Var s -> if (s=x) then if value then Neg Bot else Bot else Var s
|Bot -> Bot
|Neg psi -> Neg (replace x value psi)
|And (p,q) -> And((replace x value p),(replace x value q)) ;; 


let rec get_variables f = 
match f with
| Var x-> [x]
| Bot->[]
| Neg p-> (get_variables p)
| And (p,q)-> (get_variables p) @ (get_variables q);;

let rec eval (phi : formula (* without variables *)) : bool




