type direzioni = Su | Giu | Destra | Sinistra;;

type posizione = int * int * direzioni;; (* è semplicemente un abbreviazione di tipo *)

type azione = Gira | Avanti of int;; (* Avanti è un costruttore funzionale: restituisce azione a patto che gli passi un intero *)

let gira = function
  Su -> Destra
  | Destra-> Giu
  | Giu -> Sinistra
  | Sinistra -> Su;;

let avanti (x,y,d) n = 
  match d with
  Destra -> (x+n, y, d)
  | Giu -> (x, y-n, d)
  | Sinistra -> (x-n, y, d)
  | Su -> (x, y+n, d);;

let sposta (x,y,d) = function 
  Gira -> (x,y,gira d)
  | Avanti n -> avanti (x,y,d) n;;

let esegui pos list = List.fold_left (sposta) pos list;; 

(* Numeri interi non negativi *)
type nat = Zero | Succ of nat;;

(*Definire il prodotto sul tipo nat così definito
 type nat = Zero | Succ of nat
 usando la funzione somma definita a lezione:*)

(* Tipo per i numeri naturali *)
type nat = Zero | Succ of nat;;

(* somma : nat -> nat -> nat *)
let rec somma n m =
  match n with
  | Zero -> m
  | Succ k -> Succ (somma k m);;

(* prodotto : nat -> nat -> nat *)
let rec prod n m =
  match m with
  | Zero -> Zero
  | Succ k -> somma n (prod n k);;

(* Converte int in nat *)
let rec int_to_nat n =
  if n = 0 then Zero else Succ (int_to_nat (n - 1));;

(* Converte nat in int *)
let rec nat_to_int = function
  | Zero -> 0
  | Succ k -> 1 + nat_to_int k;;

(* Stampa un nat come int *)
let print_nat n =
  Printf.printf "%d" (nat_to_int n);;
  let test_prod a b =
    let na = int_to_nat a in
    let nb = int_to_nat b in
    let result = prod na nb in
    Printf.printf "Prodotto di %d * %d = " a b;
    print_nat result;
    Printf.printf "\n";;
  
  (* Proviamo con diversi test *)
  let () =
    Printf.printf "\n=============================\n";
    Printf.printf "Test della funzione prod:\n";
    test_prod 0 0;
    test_prod 0 5;
    test_prod 5 0;
    test_prod 2 3;
    test_prod 3 4;
    test_prod 6 6;;


(* eval env e = valore dell'espressione e nell'ambiente env. Errore se qualche variabile in e non ha un valore associato in env*)

type expr = Int of int
            | Var of string
            | Sum of expr * expr
            | Diff of expr * expr
            | Mult of expr * expr
            | Div of expr * expr;;

let rec eval env e =
  match e with
  Int n -> n
  | Var n -> List.assoc n env
  | Sum (e1,e2) -> (+) (eval env e1) (eval env e2)
  | Diff (e1,e2) -> (eval env e1) - (eval env e1);;


type 'a tree = 
    Leaf of 'a
    | One of 'a * 'a tree
    | Two of 'a * 'a tree * 'a tree;;

let rec somma albero =
  match albero with
  Leaf n -> n
  | One (n, alberello) -> n + somma(alberello)
  | Two (n, alberello1, alberello2) -> n + somma(alberello1) + somma(alberello2);;

type 'a tree = 
    Empty 
    | Tr of 'a * 'a tree * 'a tree;;

  let rec somma albero =
  match albero with 
  Empty -> 0
  | Tr (n, alberello1, alberello2) -> n + somma(alberello1) + somma(alberello2);;
  