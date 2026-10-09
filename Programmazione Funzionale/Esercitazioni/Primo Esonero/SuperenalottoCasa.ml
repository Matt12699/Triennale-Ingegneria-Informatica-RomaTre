
(* Funzione per stampare una lista di interi *)
let print_list lst =
  Printf.printf "[";
  List.iter (fun x -> Printf.printf "%d; " x) lst;
  Printf.printf "]\n";;

(* Funzione per stampare una lista di coppie *)
let rec stampa_lista = function
  | [] -> ()
  | (x, n) :: rest ->
      Printf.printf "(%d, %d)\n" x n;
      stampa_lista rest;;

(* Distribuisco il problema del superenalotto in sottoproblemi *)

(* 1a Costruisco la lista che va da [1;2;...;higher] *)

let upTo n m = 
  let rec aux result n m =
    if n>m then result
    else
      aux (n::result) (n+1) m
    in aux [] n m;;
  
  (* 1.1a Rigiro la lista *)
let rev l =
  let rec aux result l = 
    match l with
    [] -> result
    | x::rest -> aux (x::result) rest
  in aux [] l;;

(* Esempio di utilizzo *)
let resultReversed = upTo 3 7;;
Printf.printf "Provo la funzione upTo\n";;
print_list resultReversed;;

let result = rev resultReversed;;
Printf.printf "Rigiro la funzione con reversed\n";;
print_list result;;

(* 1b appiattisco la lista estrazioni (Lista contenente lista di interi int list list) trasformandola in una lista di interi*)
(* NOTA: non uso l'operatore concatenazione perchè giro la lista con la funzione reverse *)
let flatten estrazioni = 
  let rec aux risultato estrazioni =
  match estrazioni with
  [] -> risultato
  | x::rest -> aux (risultato@x) rest
  in aux [] estrazioni;;

(* 1c contare le occorrenze di ciascun elemento nella lista flatten estrazioni *)
(* 1.1c contare le occorrenze di un elemento in una lista *)

let conta n lista = 
  let rec aux result n lista = match lista with
    [] -> result
    | x::rest -> if x = n then aux(result + 1) n rest else
                  aux result n rest
    in aux 0 n lista;;

let contato = conta 7 result;;
Printf.printf "Provo la funzione conta contando quante volte si ripete 7\n";;
Printf.printf "%d\n" contato;;

let rec contatutti elementi listona = 
    match elementi with
    [] -> []
    | x::rest -> (x, conta x listona)::contatutti rest listona;;

(* 2 Ordinare la lista di coppie secondo valori non decrescenti del secondo elemento *)
(* 2.1 Scrivere una funzione di ordinamento *)
let comp (v1,n1) = function
  (_, n) -> if n1<n then -1 
  else if n1 = n then 0
  else 1;;

let sort listaCoppie = List.sort comp listaCoppie;;

(* 3 Prendere le prime dim coppie della lista ordinata *)
let take dim l =
  let rec aux result dim l =
    if dim = 0 then List.rev result 
    else match l with
          [] -> List.rev result
          | x::rest -> aux (x::result) (dim-1) rest
  in aux [] dim l;;

  let elementi = [1; 2; 3];;
  Printf.printf "Elementi: \n";;
 print_list elementi;;
  let listona = [1; 1; 1; 2; 3; 3];;
  Printf.printf "Listona: \n";;
 print_list listona;;

  let contat = contatutti elementi listona;;
  Printf.printf "Stampo le coppie (ele, n) dove ele è l elemento e n sono le sue occorrenze\n";;
  stampa_lista contat;;

  let contatos = sort contat;;
  Printf.printf "Provo la funzione sort che ordina la funzione precedente in base alle occorrenze minori dei numeri\n";;
  stampa_lista contatos;;
  let risultato = take 2 contatos;;
  Printf.printf "Provo la funzione take che in questo caso prende i primi due elementi dalla lista di coppie ordinata\n";;
  stampa_lista risultato;;

(* 4 dalla lista di coppie ottenuta, estrarre la lista con solo i primi elementi di ciascuna coppia *)
let primi list =
  let rec aux result list = match list with
  | [] -> result
  | x::rest -> aux ((fst x)::result) rest
in aux [] list;;

Printf.printf "Provo la funzione primi che dalla funzione precedente estrae solo ele"
let prim = primi risultato;;
print_list prim;;

let super estrazioni dim higher = primi (take dim (sort (contatutti (rev (upTo 1 higher)) (flatten estrazioni))));;

let lista_upto = rev (upTo 1 90);;
print_list lista_upto;;
Printf.printf "\n------------------------------\n";;
Printf.printf "Prova super: \n";;
let estrazioni = [[1;2;3]; [1;2]; [3;4]; [1;5;6]];;
let super_vincente = super estrazioni 2 6;;
print_list super_vincente;;
