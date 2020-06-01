(** Propositional variables *)

(** A type representing propositional variables. *)

type var = int;;

(** Decides whether two variables are the same.*)
let eq (n:var) (m:var) =
match n with
|x -> if x =m
then true
else false;;

(** Prints a variable. *)
let string_of_var (x : var) : string = 
string_of_int x;;

(** Ouputs a new variable from a seed. 
val new_var : int -> var *)
 let new_var (seed : string) : var =
      int_of_string seed + 1;;


