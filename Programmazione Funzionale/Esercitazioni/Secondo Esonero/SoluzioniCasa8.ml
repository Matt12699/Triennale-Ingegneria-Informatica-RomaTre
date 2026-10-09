(*  1. Risolvere i problemi seguenti su espressioni rappresentate come alberi binari,
 mediante la dichiarazione di tipo
 type expr =
 Int of int
 | Var of string
 | Sum of expr * expr
 | Diff of expr * expr
 | Mult of expr * expr
 | Div of expr * expr
 (a) Scrivere una funzione subexpr: expr-> expr-> bool che, date due
 espressioni aritmetiche E1 e E2 determini se E2 è una sotto espressione di
 E1.
 Ad esempio, le sottoespressioni di 5 + (3 × 8) sono: 5 + (3 × 8) stessa
 (qualsiasi espressione è una sottoespressione di se stessa), 5, 3 × 8, 3 e 8.*)

type expr =
 Int of int
 | Var of string
 | Sum of expr * expr
 | Diff of expr * expr
 | Mult of expr * expr
 | Div of expr * expr;;

 let rec subexpr e1 e2 = 
  e1 = e2 ||
  match e1 with
  Int n -> false
  | Var n ->  false
  | Sum (e11, e12) -> subexpr e11 e2 || subexpr e12 e2
  | Diff (e11, e12) -> subexpr e11 e2 || subexpr e12 e2
  | Mult (e11, e12) -> subexpr e11 e2 || subexpr e12 e2
  | Div (e11, e12) -> subexpr e11 e2 || subexpr e12 e2

  (* Definizione delle espressioni *)
let e1 = Sum (Int 5, Mult (Int 3, Int 8));;
let e2 = Int 5;;
let e3 = Mult (Int 3, Int 8);;
let e4 = Int 3;;
let e5 = Int 9;; (* non presente *)
let e6 = Sum (Int 3, Int 8);; (* simile a e3 ma diversa struttura *)

(* Stampa dei risultati *)
let () =
  Printf.printf "\n=============================\n";;
  Printf.printf "Test della funzione subexpr:\n";;
  Printf.printf "e2 è sottoespressione di e1? %b\n" (subexpr e1 e2); (* true *)
  Printf.printf "e3 è sottoespressione di e1? %b\n" (subexpr e1 e3); (* true *)
  Printf.printf "e4 è sottoespressione di e1? %b\n" (subexpr e1 e4); (* true *)
  Printf.printf "e5 è sottoespressione di e1? %b\n" (subexpr e1 e5); (* false *)
  Printf.printf "e6 è sottoespressione di e1? %b\n" (subexpr e1 e6); (* false *)  
;;

(*  (b) Scrivere una funzione subst_in_expr: expr-> string-> expr-> expr
 che, data un’espressione E, il nome di una variabile x e un’espressione E′,
 riporti l’espressione che si ottiene da E sostituendo ogni occorrenza di x
 con E′. *) 

(* Funzione subst_in_expr *)
 let rec subst_in_expr e x e' = 
  match e with
  | Int n -> e
  | Var n -> if n=x then e' else e
  | Sum (e11, e12) -> Sum ((subst_in_expr e11 x e'), (subst_in_expr e12 x e'))
  | Diff (e11, e12) -> Diff ((subst_in_expr e11 x e'), (subst_in_expr e12 x e'))
  | Mult (e11, e12) -> Mult ((subst_in_expr e11 x e'), (subst_in_expr e12 x e'))
  | Div (e11, e12) -> Div ((subst_in_expr e11 x e'), (subst_in_expr e12 x e'));;

(* Funzione per convertire un'espressione in stringa *)
let rec string_of_expr e =
  match e with
  | Int n -> string_of_int n
  | Var v -> v
  | Sum (a, b) -> "(" ^ string_of_expr a ^ " + " ^ string_of_expr b ^ ")"
  | Diff (a, b) -> "(" ^ string_of_expr a ^ " - " ^ string_of_expr b ^ ")"
  | Mult (a, b) -> "(" ^ string_of_expr a ^ " * " ^ string_of_expr b ^ ")"
  | Div (a, b) -> "(" ^ string_of_expr a ^ " / " ^ string_of_expr b ^ ")"

(* Test case *)
let () =
  Printf.printf "\n=============================\n";
  Printf.printf "Test della funzione subst_in_expr:\n";

  let expr =
    Mult (
      Var "x",
      Sum (
        Var "y",
        Diff (Var "x", Int 3)
      )
    )
  in

  let subst_expr = Sum (Int 1, Var "z") in

  let result = subst_in_expr expr "x" subst_expr in

  Printf.printf "Espressione iniziale: %s\n" (string_of_expr expr);
  Printf.printf "Espressione da sostituire per 'x': %s\n" (string_of_expr subst_expr);
  Printf.printf "Espressione risultante: %s\n" (string_of_expr result)

(*Data la dichiarazione di tipo per la rappresentazione di alberi binari:
 type ’a tree = Empty | Tr of ’a * ’a tree * ’a tree
 definire le funzioni seguenti:
 (a) reflect : ’a tree-> ’a tree. Applicata a un albero binario, ne costruisce l’immagine riflessa. 
 Ad esempio, i due alberi sotto rappresentati
 sono uno l’immagine riflessa dell’altro (• rappresenta l’albero vuoto).*)

 type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

 let rec reflect albero =
  match albero with
  Empty -> Empty
  | Tr(n, t1, t2) -> Tr(n, reflect(t2), reflect(t1));;

 (* Funzione per stampare l’albero in stile "grafico" *)
let print_tree_graphic tree =
  let rec aux prefix is_left t =
    match t with
    | Empty -> ()
    | Tr (v, l, r) ->
        Printf.printf "%s%s%d\n" prefix (if is_left then "├── " else "└── ") v;
        let new_prefix = prefix ^ (if is_left then "│   " else "    ") in
        aux new_prefix true l;
        aux new_prefix false r
  in
  match tree with
  | Empty -> print_endline "."
  | Tr (v, l, r) ->
      Printf.printf "%d\n" v;
      aux "" true l;
      aux "" false r

(* Test completo *)
let () =
  Printf.printf "\n=============================\n";
  Printf.printf "Test della funzione reflect con stampa grafica:\n\n";

  let t =
    Tr (1,
        Tr (2, Tr (4, Empty, Empty), Empty),
        Tr (3, Empty, Tr (5, Empty, Empty))
    )
  in

  let reflected = reflect t in

  Printf.printf "Albero originale:\n";
  print_tree_graphic t;

  Printf.printf "\nAlbero riflesso:\n";
  print_tree_graphic reflected

(*  (b) fulltree: int->int tree.La funzione, applicata a un intero n,
riporta un albero binario completo di altezza n-1,con i nodi etichettati da interi
 come segue: la radice è etichettata da 1, i figli di un nodo etichettato da k
 sono etichettati da 2k e 2k+1. *)

 let rec altezza albero = 
  match albero with
  Empty -> -1
  | Tr(_, t1, t2) -> 1 + max (altezza t1) (altezza t2);;

 let fulltree n =
  let rec aux cont n=
    match n with
    0 -> Empty
    | k -> Tr(cont, (aux (cont*2) (n-1)), (aux ((cont*2)+1) (n-1)))
 in aux 1 n;;

  (* Test completo *)
let () =
  Printf.printf "\n=============================\n";
  Printf.printf "Test della funzione fulltree con stampa grafica:\n\n";

  let reflected = fulltree 3 in

  Printf.printf "\nAlbero completo:\n";
  print_tree_graphic reflected

  (*balanced:’a tree->bool, determina se una albero è bilanciato (un albero è bilanciato se per ogni nodo n, le altezze dei sottoalberi sinistro e
 destro di n differiscono al massimo di 1).*)

 let rec balanced albero = 
  match albero with
  Empty -> true
  | Tr(x, l, r) -> if abs((altezza l) - (altezza r))>1 then false else true;;

 