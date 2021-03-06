let elim (n:int) (q:'a list) : 'a list= 
let rec iter (l1: 'a list) (l2: 'a list) : 'a list=
match l1 with
|[] -> l2
|hd::tl -> if (hd <> n ) then iter tl (hd::l2) else iter tl (l2) 
in
iter (List.rev q) [] ;;

let lenght (q: int list) : int =
let rec iter (l: int list) (len: int) : int =
match l with
|[] -> len
|hd::tl -> iter tl (len+1)
in
iter q 0;;

let last (q:'a list) : 'a option =
let rec iter (l:'a list) : 'a option =
match l with
|[] -> None
|[x] -> Some x
|_ :: n -> iter n
in
iter q;;

let rec fact (x:int) : int =
match x with
|0 -> 1 
|n -> n * fact(n-1);;

let factorial (n:int) : int =
let rec iter (s:int) (r:int) : int=
match s with
|0 -> 1
|k->iter (s+1) (r*(s+1))
in 
iter 0 1;;

