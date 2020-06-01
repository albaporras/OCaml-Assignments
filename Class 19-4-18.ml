type var = string

type formula =
  | Bot
  | Var of var
  | Neg of formula
  | And of formula * formula

let string_of_var (x : var) : string = x

let rec string_of_formula (phi : formula) : string =
  match phi with
  | Bot -> "Bot"
  | Var x -> string_of_var x
  | Neg psi -> "~" ^ (string_of_formula psi)
  | And (psi1, psi2) -> "(" ^ (string_of_formula psi1) ^ " & " ^ (string_of_formula psi2) ^ ")"


(*let rec is_tauto (phi : formula) : bool =
  let variables : var list = get_variables phi in
  match variables with
  | [] -> eval phi
  | x :: _ ->
    is_tauto (replace x false phi) && is_tauto (replace x true phi)


let rec replace (x : var) (value : bool) (phi : formula) : formula

let rec get_variables (phi : formula) : var list

let rec eval (phi : formula (* without variables *)) : bool*)
