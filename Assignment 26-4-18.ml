type var = string;;

type formula =
  | Bot
  | Var of var
  | Neg of formula
  | And of formula * formula;;


let rec delete_duplicates (q: 'a list) : 'a list =
match q with 
| []-> [] 
| m :: []-> m :: [] 
| m :: n :: tl ->  if m = n then delete_duplicates (n :: tl)  else m :: delete_duplicates (n :: tl);;

let rec get_variables f = 
match f with
| Var x-> [x]
| Bot->[]
| Neg p-> (get_variables p)
| And (p,q)-> (get_variables p) @ (get_variables q);;
