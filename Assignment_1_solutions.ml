let rec total_product q = 
match q with
|[] -> 0
|k::r -> k * total_product r;;

let eq n m =
match n with
|x -> if x =m
then true
else false;;

let rec is_member n m =
match n with
|x -> List.memq x m;;

let rec join q r =
match q with
| [] -> r  
| h :: t -> h :: t@r;;

let rec reverse q=
match q with
|[] -> []
|x::t-> List.rev (x::t);;

let max n m =
if n > m then n else m;;

let rec max_element = function
|[] -> 0
|x::q -> max x (max_element q);;

let rec elim n q =
match q with
|[] -> []
|x::t-> List.filter (fun x-> (x<>n)) t;;

let rec count n m r=
match n with
|x ->if (List.mem x m)== true 
then r=r+1
else r;;

let rec last = function
|[] -> None
|[x] -> Some x
|_ :: n -> last n;;


