(* Generare tutte le liste di zeri e uni di lunghezza n *)
let rec genera_binari n = 
  if n = 0 then [[]] else
  let sottoliste = genera_binari (n-1) in
  (List.map (function x -> 0::x) sottoliste) @ (List.map (function x-> 1::x) sottoliste);;

let rec print_list' lst =
  match lst with
  | [] -> ()
  | x::xs -> Printf.printf "[";
             List.iter (fun el -> Printf.printf "%d; " el) x;
             Printf.printf "] ";
             print_list' xs;;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova genera_binari: \n";;
let test = genera_binari 3;;
print_list' test;;

(* Generare tutte le liste di numeri tra 1 e k, di lunghezza n. *)
let rec genera n k =
  if n = 0 then [[]] else
  let sottoliste = genera (n-1) (k) in
  let rec aux risultato k = match k with
    0 -> risultato
    | _ -> aux ((List.map (function x-> k::x) sottoliste)@risultato) (k-1)
  in aux [] k;;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova genera: \n";;
let test = genera 2 3;;
print_list' test;;

(* Obiettivo: Dato un numero, trova il primo indice in cui si trova. Se non esiste, solleva un'eccezione.

Idea:
Proviamo a cercare l'elemento nella lista.

Se lo troviamo, ritorniamo l'indice.

Se non lo troviamo, solleviamo un'eccezione.

Indizio:
Puoi usare un contatore (indice) e fare un controllo passo per passo con try/with. *)
exception NoNumber
let search_number n list =
  let rec aux indice list =
    match list with
    [] -> raise NoNumber
    | x::rest -> if x=n then indice else aux (indice+1) rest
  in aux 0 list;;

(* Dato un numero e una lista di liste, trovare il primo elemento che contiene quel numero. Se non lo troviamo, solleviamo un'eccezione. 
Il backtracking ti servirà per esplorare ogni lista interna.*)

let rec search_in_lists n list = 
  match list with
  [] -> raise NoNumber
  | x::rest ->
              try
              let rec aux list =
                match list with
                [] -> raise NoNumber
                | y::rest -> if y=n then x else aux rest
              in aux x
              with NoNumber -> search_in_lists n rest;;

let rec search_in_lists n list =
  match list with
  [] -> raise NoNumber
  | x::rest -> let rec aux sublist = match sublist with
                [] -> search_in_lists n rest
                | y::rest -> if y = n then x else aux rest
              in aux x;;

let esempio = [[3; 2]; [4; 4]; [9; 9]];;
let risultato = search_in_lists 3 esempio;;

let print_list lst =
  Printf.printf "[";
  List.iter (fun x -> Printf.printf "%d; " x) lst;
  Printf.printf "]\n";;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova search_in_lists: \n";;
print_list risultato;;

(* Dato un insieme S di numeri interi positivi e un intero N, determinare un sottoinsieme Y di S tale che la somma
  degli elementi di Y sia uguale a N*)

  let sumof list = 
    let rec aux risultato list =
      match list with
      [] -> risultato
      | x::rest -> aux (risultato+x) rest
    in aux 0 list;;

  exception NessunInsiemeTrovato
  let subset_search n s = 
    let rec aux solution altri =
      if sumof solution = n then solution
      else if sumof solution > n then raise NessunInsiemeTrovato
      else
        match altri with
        [] -> raise NessunInsiemeTrovato
        | x::rest -> try 
                      aux (x::solution) rest
                     with NessunInsiemeTrovato -> aux solution rest
                    in aux [] s;;

  