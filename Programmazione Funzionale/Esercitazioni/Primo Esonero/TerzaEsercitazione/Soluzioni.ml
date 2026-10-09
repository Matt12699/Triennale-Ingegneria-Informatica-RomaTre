(*let somma_ore (h1, m1) (h2, m2) =
  let is_h x = x>=0 && x<=23
in let is_m y = y>=0 && y<=59
in y (is_h h1) && (is_h h2) && (is_m m1) && (is_m m2) then
  let m_tot = m1 + m2
  in ((h1 + h2+m_tot/60) mod 24, m_tot mod 60)
  else raise NoTime*)

exception NoInput
let read_max () =
  try let m = read_int()
    in let rec aux m = 
        try let n = read_int()
            in aux (max n m)
        with _ -> m
      in aux m
    with _ -> raise NoInput

let x = read_max();;
Printf.printf "Il massimo è: %d\n" x;;

let read_max_min () =
try let y = read_int()
    in let rec read_max_min' (a, b) =
        try let x = read_int() 
      in read_max_min' ((max a x), (min b x)) 
      with _ -> (a, b)
    in read_max_min' (y, y)
with _ -> raise NoInput

let tutti_minori n =
    let rec aux b =
    try let x = read_int()
        in aux( x < n ) && b
    with _ -> b
  in aux true

  let rec tutti_minori' n = 
    try let m = read_int()
        in (tutti_minori' n) && m<n
    with _ -> true

let rec merge l1 l2 = 
  match (l1, l2) with
    ([], l) -> l
  | (l, []) -> l
  | (x::xs, y::ys) -> if x<=y then x::(merge xs (y::ys))
                      else y::(merge (x::xs) ys)

(*Le due parti devono avere la stessa lunghezza o variare di uno*)
let rec split l = 
  match l with
    [] -> ([], []) (* Lista vuota restituisco liste vuote*)
  | [x] -> ([x], []) (*Lista con un solo elemento*)
  | x::y::rest ->  let (xs, ys) = split rest (*Ci sono almeno due elementi*) (*Spacco a metà la lista srnza i primi due elementi (split rest)
                                                                               i due elementi rimasti li metto in testa alle due liste*)
                  in (x::xs, y::ys)

(*c'è un modo più semplice*)

let rec split' l = 
  match l with
    [] -> ([], []) (* Lista vuota restituisco liste vuote*)
  | x::xs -> let (cs, bs) = split xs
              in  (x::bs, cs)
(*c'è anche una versione iterativa*)

let rec mergesort l = 
  match l with
  [] -> []
  | [x] -> [x]
  | l -> let (l1, l2) = split' l
        in merge (mergesort l1) (mergesort l2)
        
let rec prod l = 
  let rec prod_aux l result = 
  match l with
        | [] -> result
        | x::rest -> prod_aux rest result*x
  in prod_aux l 1;;

(* Funzione up to mi deve restituire la lista dei valori che vanno da m a n*)

let rec upTo m n =
  if m > n then
    []
  else
    m::(upTo (m+1) n);;

let upto m n = 
  let rec aux l m' n' =
    if m' > n' then
      l
    else
      aux (n'::l) m' (n'-1)
  in aux [] m n;;
  