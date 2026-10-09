(* Problema delle N regine. 
  L'indice dell'array rappresenta la colonna, il valore la riga

  Creiamo una funzione scacco con due coppie di interi che ci dice se la prima posizione è sotto scacco della seconda
  Per vedere se è sotto scacco se i-m = j-n -> stessa diagonale discendente
                                  i+j = m+n -> stessa diagonale ascendente *)

(* scacco : int * int -> int * int -> bool *)
(* scacco (i,j) (m,n) = true se due regine, posizionate sulle caselle
(i,j) e (m,n) sono sotto scacco reciproco*)
let scacco (i,j) (m,n) =
(i=m) || (i+j=m+n) || (i-j = m-n)
 (* Ciascuna regina grazie alla rappresentazione non può trovarsi sulla stessa colonna *)

(* Creiamo una funzione che mi dice se quella posizione è salva o meno. Serve a aggiungere una nuova regina nella riga m sulla colonna nuova *)
(*Dobbiamo ora definire una funzione che, data la rappresentazione di una scacchiera
in cui sono state posizionate m regine e un intero k, determini se la m+1-esima regina
si pu`o mettere nella riga k. *)
(* Serve a associare la colonna alla riga *)
(* combine : ’a list -> ’b list -> (’a * ’b) list *)
(* combine [x1;...;xn] [y1;...;yn] = [(x1,y1);...;(xn,yn)] *)
let rec combine lst1 lst2 =
  match (lst1,lst2) with
  ([],[]) -> []
  | (x::rest1,y::rest2) -> (x,y)::combine rest1 rest2
  | _ -> failwith "combine"

let upTo n m = 
  let rec aux result n m =
    if n>m then result
    else
      aux (n::result) (n+1) m
    in aux [] n m;;

(* safe : int list -> int -> bool *)
(* safe board k = true se la prossima regina si puo’ mettere alla
riga k
(* aux: (int * int) list -> bool *)
(* funzione ausiliaria che controlla tutte le caselle della lista *)
(* aux lista_caselle = true se nessuna casella in lista_caselle mette
sotto scacco una regina nella casella
(m+1,k), dove m e’ la lunghezza di board *)*)
let safe board k =
let m = List.length board in
let rec aux = function
[] -> true
| (i,j)::rest ->
not (scacco (i,j) (k,m+1)) && aux rest
in aux (List.combine board (upTo 1 m))

(* Adesso scrivo una funzione search che mi trovi almeno una soluzione *)
exception NotFound
(* Ci manca l'informazione sulla colonna da inserire nel with *)
(* queens n = soluzione al problema delle n regine *)
(* search : int -> int -> int list -> int list *)
(* search c r board = soluzione al problema delle n regine
che estende la scacchiera board dove sono gia’ state
sistemate c-1 regine, e con la posizione della c-esima
regina in una riga maggiore o uguale a r *)
let queens n =
  let rec search c r board =
  if c > n then board
  else
  if r > n then raise NotFound
  else
  if safe board r
  then
  try search (c+1) 1 (board@[r])
  with NotFound -> search c (r+1) board
  else search c (r+1) board
  in search 1 1 []
  
