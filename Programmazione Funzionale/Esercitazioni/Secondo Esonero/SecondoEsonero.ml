(* Giusta: 7 punti *)
let from_stringlist_to_string lista = 
  let rec aux risultato lista = 
    match lista with
    [] -> risultato
    | x::rest -> if not(rest=[]) then aux (risultato^x^" ") rest else aux (risultato^x) rest
  in aux "" lista;;

let x = from_stringlist_to_string [];;
Printf.printf "%s\n" x;;
let x = from_stringlist_to_string ["ci"];;
Printf.printf "%s\n" x;;
let x = from_stringlist_to_string ["ci";"ao"];;
Printf.printf "%s\n" x;;
let x = from_stringlist_to_string ["ci";"a";"o"];;
Printf.printf "%s\n" x;;

(* Sbagliata 1-2/8*)
type 'a ntree = Tr of 'a * 'a ntree list;;
(*let lispy albero = 
  let rec aux albero = 
    match albero with
    Tr(x, []) -> [(String.make 1 x)]
    | Tr(x, figli) -> ["("]::x::(List.map (aux) figli)@[")"]
  in from_stringlist_to_string (aux albero);;*)

(* Manca la rimozione degli spazi alla fine, lo string make sulla x , il list.flatten su list.map e le concatenazioni*)
let lispy albero = 
  let rec aux albero = 
    match albero with
    Tr(x, []) -> [(String.make 1 x)]
    | Tr(x, figli) -> ["("]@[(String.make 1 x)]@(List.flatten (List.map (aux) figli))@[")"]
  in from_stringlist_to_string (List.filter (function x-> not(x=" ")) (aux albero));;

let x = lispy (Tr('a', []));;
Printf.printf "%s\n" x;;

let x = lispy (Tr('a', [Tr('b', [])]));;
Printf.printf "%s\n" x;;

let x = lispy (Tr('a', [Tr('b', [Tr('c', [])])]));;
Printf.printf "%s\n" x;;

let x = lispy (Tr('a', [Tr('b', []);Tr('c', [])]));;
Printf.printf "%s\n" x;;

type 'a graph = ('a * 'a) list;;

let graph = [ (1,2); (1,3); (1,4); (2,6); (3,5); (4,6); (5,4); (6,5); (6,7)];;

(* Giusta 4/5 *)
(* Mi sono dimenticato grafo su list exists *)
exception NoNodoNelGrafo
let degree grafo nodo = if not(List.exists (function (y,z)-> nodo=y || nodo=z) grafo) then raise NoNodoNelGrafo else List.length (List.filter (function (y,z) -> nodo=y || nodo=z) grafo);;

let x = degree graph 1;;
Printf.printf "%d\n" x;;

let x = degree graph 6;;
Printf.printf "%d\n" x;;

let rec vicini x grafo = if List.exists (function (y,z) -> x=y || z=x)  grafo then List.map (function (y,z)-> if x=y then z else y) 
(List.filter (function (y,z)-> x=y || x=z) grafo) else raise NoNodoNelGrafo;;

exception GrafoVuoto

(* Sbagliata 1/5 *)
(* Non era da punteggio pieno con l utilizzo di vicini inoltre ho dimenticato di accumulare i visitati e ho usato list.map per il fst di una coppia *)
let nodes grafo = 
  if grafo=[] then raise GrafoVuoto
  else
    let rec aux visitati pendenti = 
      match pendenti with
      [] -> visitati
      | x::rest -> if List.mem x visitati then aux visitati rest
      else aux (x::visitati) ((vicini x grafo)@rest)
    in aux [] [(fst (List.hd grafo))];;

let print_int_list lst =
  let rec aux = function
    | [] -> print_string "]\n"
    | [x] -> Printf.printf "%d]\n" x
    | x::xs -> Printf.printf "%d; " x; aux xs
  in
  print_string "["; aux lst;;

let x = nodes graph;;
print_int_list x;;

(* Giusta 4/5 *)
(* ho dimenticato grafo dentro degree*)
let nodes_with_degree grafo =    
  let nodiGrafo = nodes grafo in   
  List.combine nodiGrafo (List.map (degree grafo) nodiGrafo);;


let print_pair (a, b) = 
  Printf.printf "(%d, %d)" a b;;

let print_pairs_list lista = 
  print_string "[";
  (match lista with
   | [] -> ()
   | primo::resto -> 
       print_pair primo;
       List.iter (fun coppia -> print_string "; "; print_pair coppia) resto);
  print_string "]";
  print_newline ();;

let x = nodes_with_degree graph;;
print_pairs_list x;;

let compare (_,grado1) (_, grado2) =
  if grado1 > grado2 then -1
  else if grado1=grado2 then 0 else 1;;

(* Giusta 5/5 *)
let ordered_nodes_grafo grafo = List.map (fst) (List.sort (compare) (nodes_with_degree grafo));;

let x = ordered_nodes_grafo graph;;
print_int_list x;;

