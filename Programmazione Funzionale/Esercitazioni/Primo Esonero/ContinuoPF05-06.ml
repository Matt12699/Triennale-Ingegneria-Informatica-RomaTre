(* Applicata a un elemento x e la rappresentazione di un insieme S, determina se x appartiene a S (è un predicato)*)

let rec mem x = function
    [] -> false
  | y::ys -> if x=y then true
             else mem x ys;;

let rec remove x = function
    []-> []
    | y::ys -> if x=y then ys
    else
      y::(remove x ys);;

let rec setDiff l1 = function
  [] -> l1
  | x::xs -> if mem x l1 then remove x l1
  else
    setDiff l1 xs;;