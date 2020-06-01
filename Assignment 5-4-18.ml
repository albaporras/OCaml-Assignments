let total_product (w: int list) : int =
let rec iter (l : int list) (running_total: int) : int = 
match l with
|[] -> running_total
|hd:: tl -> iter tl (hd * running_total)
in
iter w 1;;

let count (w: int list) (x: int) : int =
let rec iter (l: int list) (total:int) :int =
match l with
|[] -> total
|hd:: tl -> if (hd=x) then iter tl (total +1) else iter tl (total)
in
iter w 0;;

let eq (n:int) (m:int) =
match n with
|x -> if x =m
then true
else false;;

let rec reverse (q: 'a list) : 'a list= (*I've not modified it because we corrected in class *)
match q with
|[] -> []
|x::t-> List.rev (x::t);;

let join (q: int list) (r: int list): int list=
let rec iter (l: int list) (l2: int list): int list=
match l with
|[] -> l2
|hd::tl-> iter tl (hd::l2)
in 
iter (List.rev q) r;;

let max (n: int) (m: int) =
if n > m then n else m;;

let rec max_element = function
|[] -> 0
|x::q -> max x (max_element q);; 

let rec last = function
|[] -> None
|[x]  -> Some x 
|_ :: n -> last n;;

let rec kth_element k = function
|[] -> None
|hd::tl -> if k = 1 then Some hd else kth_element  (k-1) tl;;

let split ( q: int list)(x:int) =
let rec iter n f = function
|[] -> (List.rev f), []
|hd::tl-> if n = 0 then (List.rev f), q else iter (n-1) (hd::f) tl 
in iter x [] q;;
