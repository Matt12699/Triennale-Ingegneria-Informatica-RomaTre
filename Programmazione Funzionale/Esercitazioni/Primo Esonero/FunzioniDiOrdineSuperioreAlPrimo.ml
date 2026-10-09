
(*let rec inits = function 
  [] -> []
  | [x] -> [[x]]
  | x::rest -> [x]:: List.map (::) [x] (inits rest);; (* :: è un costruttore non viene considerata una funzione *)*)

  let rec inits = function 
  [] -> []
  | [x] -> [[x]]
  | x::rest -> [x]:: (List.map ((@) [x]) (inits rest));; (* :: è un costruttore non viene considerata una funzione *)

exception NoElement
let rec find f list =
  match list with
  [] -> raise NoElement
  | x::rest -> if f x then x else
                find f rest;;
let list = [1; 2; 3; 3];;
let pari x = x mod 2 = 0;;

let x = find pari list;;
Printf.printf "%d\n" x;;

let find_applicata list = find (function x-> x*x<30) list;;

let takewhile p list = 
  let rec aux risultato p list = 
    match list with
    [] -> risultato
    | x::rest -> if p x then aux (risultato@[x]) p rest else
                        aux risultato p rest
    in aux [] p list;;

let rec takewhile f = function
    [] -> []
    | x::xs -> if f x then x::(takewhile f xs)
              else [];; 

let listaPari = takewhile (function x-> x mod 2 = 0) list;;


let partition p list = 
  let rec aux veri falsi p list =
    match list with
    [] -> (veri, falsi)
    | x::rest -> if p x then aux (veri@[x]) falsi p rest
    else
      aux veri (falsi@[x]) p rest
    in aux [] [] p list;;

(* Funzione per stampare una int list *)
let rec print_int_list = function
  | [] -> print_string "[]"
  | [x] -> Printf.printf "%d" x
  | x::xs -> Printf.printf "%d; " x; print_int_list xs

(* Funzione per stampare la coppia di liste *)
let print_partition_result (l1, l2) =
print_string "Soddisfano il predicato: ["; print_int_list l1; print_string "]\n";
print_string "NON soddisfano il predicato: ["; print_int_list l2; print_string "]\n"

(* Esempio di uso *)
let predicato x = x mod 2 = 0
let lista = [1; 2; 3; 4; 5; 6]

let risultato = partition predicato lista
let () = print_partition_result risultato

let pairwith x l = List.map (function y-> (x, y)) l;;

(* Funzione per stampare una lista di coppie int * int *)
let rec print_pair_list = function
  | [] -> print_string "[]\n"
  | [(a, b)] -> Printf.printf "(%d, %d)]\n" a b
  | (a, b)::rest ->
      Printf.printf "(%d, %d); " a b;
      print_pair_list rest

(* Esempio d'uso *)
let () =
  let result = pairwith 5 [1; 2; 3] in
  print_string "[";
  print_pair_list result

let rec mem x = function
    [] -> false
  | y::ys -> if x=y then true
             else mem x ys
;;

let setdiff l1 l2 = List.filter (function x -> not(mem x l2)) l1;; (*Molto simile alla sua*)

let l1 = [1; 2; 3; 4; 5];;
let l2 = [2;4];;

let l3 = setdiff l1 l2;;

let print_list lst =
  Printf.printf "[";
  List.iter (fun x -> Printf.printf "%d; " x) lst;
  Printf.printf "]\n";;

print_list l3;;

let mem l x = List.exists ((=) x) l;;

let non p x = not (p x);;

let setdiff l1 l2 = List.filter (non (mem l2)) l1;;

let rec powerset list =
  match list with 
  [] -> [[]]
  | x::rest -> powerset rest @ List.map (function y-> x::y) [rest];;
  