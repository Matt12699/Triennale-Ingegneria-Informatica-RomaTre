(* maxlist: ’a list-> ’a, che riporta il massimo elemento in una
 lista (la lista vuota non ha elementi, quindi nemmeno un massimo;
 dunque se la lista è vuota deve essere sollevata un’eccezione). *)

exception EmptyList

let maxlist l = 
  match l with 
  [] -> raise EmptyList
  | x::xs -> let rec aux m l = match l with
    [] -> m
    | x::rest -> if x > m then aux x rest
                else aux m rest
              in aux x l;;

let lista = [1; 2; 3; 0; 70];;
let x = maxlist lista;;
Printf.printf "Il numero massimo e': %d\n" x;;

(* drop: int-> ’a list-> ’a list, tale che drop n lst = lista
 che si ottiene da lst togliendone i primi n elementi. Se il numero
 di elementi di lst è minore di n (oppure uguale a n), allora drop n
 lst = []. *)

 let rec drop n = function
    [] -> []
    | x::rest -> if n<=0 then x::rest
      else
           drop (n-1) rest;;

(* append: ’a list-> ’a list-> ’a list. Se @ non fosse prede
f
 inito, come si potrebbe definire (utilizzando solo i costruttori delle
 liste)? *)

 let rec append l1 l2 = match l1 with
        [] -> l2
        | x::rest -> x::append rest l2;;

let append' l1 l2 = 
  let rec aux result a = match l1 with
      [] -> a
      | x::rest-> aux (x::a) rest 
in aux l2 (List.rev l1);;

(* nth: int-> ’a list-> ’a, tale che nth n lst = elemento di lst
 in posizione n, dove il primo elemento della lista è in posizione 0. La
 funzione solleverà un’eccezione se n è negativo o se la lista non con
tiene abbastanza elementi (Notare che il modulo List contiene una
 funzione con questo nome, ma qui si chiede di definirla per esercizio).*)
exception NegativeNumber
exception NeedMoreElements
 let rec nth n = function
    [] -> raise NeedMoreElements
    | x::rest -> if n<0 then raise NeedMoreElements
    else
      if n=0 then x else nth (n-1) rest;; 

    let x = nth 4 lista;;
    Printf.printf "Il numero in posizione 3 e': %d\n" x;;

(*  Un predicato nondec: int list-> bool che, applicato a una lista
 lst, riporti true se gli elementi di lst sono in ordine non decrescente,
 false altrimenti.
 Adesempio, nondec [1;2;3;4] = true, enondec [1;2;4;3] = false. *)

let nondec l = 
  let rec aux result l = match l with
    [] -> result
    | [x] -> result
    | x::y::rest -> if y>x then aux result (y::rest) else false
  in aux true l;;

  let x = nondec lista;;
  Printf.printf "%b" x;;

(*  Unafunzione alternate: ’a list-> ’a listche, applicataauna
 lista lst, riporti la lista contentente tutti e soli gli elementi di lst
 che si trovano in posizione dispari. Ricordiamo che, per convenzione,
 il primo elemento di una lista si trova in posizione 0, il secondo in
 posizione 1, ecc. Quindi, ad esempio, alternate [0;1;20;32;4;5]
 = [1;32;5]. *)

 (*let alternate l =
  let rec aux index l = match l with 
    [] -> index
    | x::rest -> if index mod 2 =1 then x::(aux (index+1) rest) 
                else aux (index+1) rest
  in aux 0 l;;*)

(*Una funzione min_dei_max: int list list-> int che, data una
 lista di liste di interi, riporti il valore minimo tra i massimi di ciascuna
 lista. Ad esempio, per la lista [[3;100;1;9];[2;10;20];[80;65;4]],
 si otterrà il valore 20.*)
exception NoInput

let rec minimo minProv l = match l with
[] -> minProv
| x::rest -> if x < minProv then minimo x rest
            else
              minimo minProv rest;;

let rec massimo maxProv l = match l with
    [] -> maxProv
    | x::rest -> if x > maxProv then massimo x rest
                else
                 massimo maxProv rest;;

 let min_dei_max l = 
  let rec listmax acc = function                              (* List max concatena l acc con i massimi di ciascuna lista del secondo argomento, restituisce quindi una lista dei massimi delle liste *)
    [] -> acc
    | x::rest-> listmax ((maxlist x)::acc) rest
 in let rec minlist = function
    [] -> raise NeedMoreElements
    | x::rest -> try let y = (minlist rest)
                    in min x y
                  with _ -> x
    in minlist (listmax [] l);;
    
    let sum' l =
      let rec aux tot = function
          [] -> tot
        | x::xs -> aux (tot+x) xs
      in aux 0 l
    ;;

    exception NotFound

    let search_subset set tot =
      let rec search_aux solution others tot' = 
        let s = sum' solution (* Somma degli elementi del candidato a essere una soluzione *)
        in if s=tot' then solution (* Se è uguale al totale siamo arrivati alla soluzione *)
           else if s>tot' then raise NotFound (* Se è maggiore del totale quell'insieme non può essere soluzione (La somma ha sforato)*)
           else match others with  (*we have s<tot*) (* Devo pescare un'elemento *)
                  [] -> raise NotFound (* Se non ci sono altri elementi fallimento *)
                | x::xs -> (* Se c'è un altro elemento lo pesco *)
                   try search_aux (x::solution) xs tot' (* "O faccio questo o faccio quest'altro", provo ad aggiungere l elemento*)
                   with NotFound -> search_aux (solution) xs tot'   (* Se siamo arrivati a un vicolo cieco torniamo indietro senza aggiungere x (L'ho scartato)*)      
      in search_aux [] set tot
    ;;
                    
