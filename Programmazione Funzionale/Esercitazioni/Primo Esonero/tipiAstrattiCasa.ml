(* Funzione per stampare una lista di interi *)
let print_list lst =
  Printf.printf "[";
  List.iter (fun x -> Printf.printf "%d; " x) lst;
  Printf.printf "]\n";;

(* Applicata a un elemento x e la rappresentazione di un insieme S, determina se x appartiene a S (è un predicato)*)

let rec mem x = function
    [] -> false
  | y::ys -> if x=y then true
             else mem x ys
;;

(* Applicata a due liste che rappresentano insiemi S1 e S2, riporta una rappresentazione di S1 U S2*)
let rec union s1 s2 = match s1 with
  [] -> s2
  | x::rest -> if mem x s2 then union rest s2
              else union rest (x::s2);;

let s1 = [1 ; 2 ; 3];;
Printf.printf "La prima lista e': ";;
print_list s1;;
let s2 = [2 ; 5 ; 6];;
Printf.printf "La seconda lista e': ";;
print_list s2;;

Printf.printf "Provo la funzione unione: \n";;
let unione = union s1 s2;;
print_list unione;;

let rec intersect s1 s2 =
  let rec aux result s1 s2 = match s1 with
    [] -> result
    | x::rest -> if mem x s2 then aux (x::result) rest s2
                  else
                    aux result rest s2
                  in aux [] s1 s2;;

Printf.printf "Provo la funzione intersezione: \n";;
let intersezione = intersect s1 s2;;
print_list intersezione;;

(* Stampa al contrario, ma dato che stiamo vedendo le liste come rappresentanti di insiemi, l'ordine non conta niente. Fix: result@[x] *)
let rec setDiff s1 s2 =
  let rec aux result s1 s2 = match s1 with
    [] -> result
    | x::rest -> if mem x s2 then aux result rest s2
                  else
                    aux (x::result) rest s2
                  in aux [] s1 s2;;

Printf.printf "Provo la funzione differenza: \n";;
let differenza = setDiff s1 s2;;
print_list differenza;;

