
let rec upTo m n =
  if m > n then
    []
  else
    m::(upTo (m+1) n);;

(* Ha due argomenti, l accumulatore e la lista*)
let flatten ll = 
let rec aux acc = function
  [] -> acc
  | x::xs -> aux (x @ acc) xs
in aux [] ll;; 

let conta e l =
let rec aux cont e l = match l with
  [] -> 0
  | x::xs -> if e=x then aux (cont+1) e l
              else
              aux cont e l
in aux 0 e l;;

let take dim l =
  let rec aux result dim l =
    if dim = 0 then List.rev result 
    else match l with
          [] -> List.rev result
          | x::rest -> aux (x::result) (dim-1) rest
  in aux [] dim l;;

(* Suppongo che in elementi non ci sono delle ripetizioni, conta anche gli elementi mancanti*)
let rec contatutti elementi listona =
  match elementi with
  [] -> []
  | x::xs -> (x, conta x listona)::contatutti xs listona;; 

(* Definisco un ordinamento per il list.sort *)

(* Prende due coppie: se n1 è più piccolo restituisce -1 se è uguale restituisce 0 sennò 1*)
let comp (v1,n1) = function
  (_, n) -> if n1<n then -1 
  else if n1 = n then 0
  else 1;;

let sort l = List.sort comp l;;

let rec primi = function
  [] -> []
  | x::xs -> (fst x)::primi(xs);;

let super estrazioni dim higher = primi(take dim (sort(contatutti(upTo 1 higher) (flatten estrazioni))));;