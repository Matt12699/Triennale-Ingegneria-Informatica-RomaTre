(* stampa di un dizionario *)
let stampa_dizionario dizionario =
   List.iter (fun (chiave, valore) -> Printf.printf "%s -> %d\n" chiave valore) dizionario;;
 

(* Dato un dizionario, rappresentato dalla lista associativa assoc_list, e una chiave k, riportare il valore associato
   a k in assoc_list, se esiste, un errore altrimenti *)

exception NotFound

let rec assoc k assoc_list = match assoc_list with
   [] -> raise NotFound
   | (k1,v)::rest ->  if k1 = k then v 
                        else
                        assoc k rest;;

let dizionario = [("pippo", 100); ("pluto", 50); ("topolino", 30)];;
stampa_dizionario dizionario;;
Printf.printf "Prova della funzione di ricerca in un dizionario\n"
let valore = assoc "pluto" dizionario;;
Printf.printf "Il valore associato a pluto e': %d\n" valore;;

(* Data una chiave k, un valore v e una lista associativa assoc_list, riportare la lista che si ottiene inserendo la coppia (k,v) in assoc_list - sostituendo (o sovrascrivendo)
   l'eventuale elemento già esistente con chiave k*)

let inserisci k v assoc_list = 
   (k,v)::assoc_list;;

Printf.printf "Inserisco la coppia (pluto, 15) nel dizionario\n";;
let dizionario = inserisci "pluto" 15 dizionario;;
stampa_dizionario dizionario;;

(* data una chiave k e una lista associativa assoc_list, riportare la lista che si ottiene cancellando da assoc_list tutte le coppie con chiave k*)
let rec cancella k assoc_list = 
      match assoc_list with 
      [] -> []
      | (k1, v)::rest -> if k1 = k then cancella k rest
                        else
                           (k1,v)::cancella k rest;;

Printf.printf "Cancello tutte le coppie con chiave pluto\n";;
let dizionario = cancella "pluto" dizionario;;
stampa_dizionario dizionario;;


