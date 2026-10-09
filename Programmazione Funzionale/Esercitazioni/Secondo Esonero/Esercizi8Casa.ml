
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

  let rec stampa_lista lst =
  match lst with
  | [] -> print_string "]\n"
  | [x] -> Printf.printf "%d]\n" x
  | x::xs ->
      Printf.printf "%d; " x;
      stampa_lista xs;;

(* Funzione di utilità per stampare con parentesi quadre *)
let print_int_list lst =
  print_string "[";
  stampa_lista lst;;

(*reflect : ’a tree-> ’a tree. Applicata a un albero binario, ne co
struisce l’immagine riflessa. Ad esempio, i due alberi sotto rappresentati
 sono uno l’immagine riflessa dell’altro (• rappresenta l’albero vuoto).*)

let rec reflect albero = 
  match albero with
  Empty -> Empty
  | Tr(x, l, r) -> Tr(x, reflect(r), reflect(l));;

   (* Funzione per stampare l’albero in stile "grafico" *)
let print_tree_graphic tree =
  let rec aux prefix is_left t =
    match t with
    | Empty -> ()
    | Tr (v, l, r) ->
        Printf.printf "%s%s%d\n"
          prefix
          (if is_left then "├── " else "└── ")
          v;
        let new_prefix = prefix ^ (if is_left then "│   " else "    ") in
        aux new_prefix false r;  (* STAMPA PRIMA IL DESTRO *)
        aux new_prefix true l    (* POI IL SINISTRO *)
  in
  match tree with
  | Empty -> print_endline "Albero vuoto."
  | Tr (v, l, r) ->
      Printf.printf "%d\n" v;
      aux "" false r;
      aux "" true l


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

(* fulltree: int->int tree.La funzione,applicata a un intero n,riporta un albero binario completo di altezza n,
  con i nodi etichettati da interi
 come segue: la radice è etichettata da 1, i figli di un nodo etichettato da k
 sono etichettati da 2k e 2k+1.*)

 let rec fulltree n = 
  let rec aux cont n=
  if n = 0 then Empty
  else
  Tr(cont, aux(cont*2) (n-1), aux(cont*2+1) (n-1))
  in aux 1 n;;

  (* Test completo *)
let () =
  Printf.printf "\n=============================\n";
  Printf.printf "Test della funzione fulltree:\n\n";

  let fulltree = fulltree 2 in

  Printf.printf "\nAlbero completo:\n";
  print_tree_graphic fulltree;;

  let rec altezza albero = 
    match albero with
    Empty -> 0
    | Tr(_, l, r) -> 1 + max (altezza(l)) (altezza(r));;

  let () =
  Printf.printf "\n=============================\n";
  Printf.printf "Test della funzione altezza:\n";

  let t =
    Tr (1,
        Tr (2, Tr (4, Empty, Empty), Empty),
        Tr (3, Empty, Tr (5, Empty, Empty))
    )
  in

  let heigth = altezza t in

  Printf.printf "Albero originale:\n";
  print_tree_graphic t;

  Printf.printf "L'altezza e' di: %d" heigth;;

 (* balanced: ’a tree -> bool, determina se un albero è bilanciato (un albero è bilanciato se per ogni nodo n, le altezze dei sottoalberi sinistro e
  destro di n differiscono al massimo di 1). *)

  let rec balanced albero= 
    match albero with
    Empty -> true
    | Tr(x, l, r) -> balanced l && balanced r && abs(altezza(l) - altezza(r))<=1;;

  (*preorder, postorder, inorder, tutte di tipo ’a tree -> ’a list. Dato un albero t, le funzioni riportano la lista dei nodi di t, nell’ordine in
 cui sarebbero visitate secondo gli algoritmi di visita, rispettivamente, in
 preordine, postordine e simmetrica.*)

 let rec preorder albero=
 match albero with
 Empty -> []
 | Tr(x, l, r) -> [x]@preorder(l)@preorder(r);;

  (* Test completo *)
let () =
  Printf.printf "\n=============================\n";
  Printf.printf "Test della funzione preorder con stampa grafica:\n\n";

  let t =
    Tr(1,
     Tr(2,
        Tr(4, Empty, Empty),
        Tr(5, Empty, Empty)),
     Tr(3,
        Empty,
        Tr(6, Empty, Empty)))
  in

  let lista = preorder t in

  Printf.printf "Albero originale:\n";
  print_tree_graphic t;

  
  print_int_list lista;;

let rec postorder albero=
 match albero with
 Empty -> []
 | Tr(x, l, r) -> postorder(l)@postorder(r)@[x];;

  (* Test completo *)
let () =
  Printf.printf "\n=============================\n";
  Printf.printf "Test della funzione postorder con stampa grafica:\n\n";

  let t =
    Tr(1,
     Tr(2,
        Tr(4, Empty, Empty),
        Tr(5, Empty, Empty)),
     Tr(3,
        Empty,
        Tr(6, Empty, Empty)))
  in

  let lista = postorder t in

  Printf.printf "Albero originale:\n";
  print_tree_graphic t;

  
  print_int_list lista;;

let rec inorder albero=
 match albero with
 Empty -> []
 | Tr(x, l, r) -> inorder(l)@[x]@inorder(r);;

  (* Test completo *)
let () =
  Printf.printf "\n=============================\n";
  Printf.printf "Test della funzione inorder con stampa grafica:\n\n";

  let t =
    Tr(1,
     Tr(2,
        Tr(4, Empty, Empty),
        Tr(5, Empty, Empty)),
     Tr(3,
        Empty,
        Tr(6, Empty, Empty)))
  in

  let lista = inorder t in

  Printf.printf "Albero originale:\n";
  print_tree_graphic t;

  
  print_int_list lista;;

(* balpreorder e balinorder, entrambe di tipo ’a list -> ’a tree. Data
 una lista lst, costruiscono un albero bilanciato con nodi etichettati da
 elementi di lst, in modo tale che
 preorder(balpreorder lst)=lst
 inorder (balinorder lst) =lst
 (utilizzare take e drop).*)

 let rec take n list = 
  match list with
  [] -> []
  | x::rest -> if n=0 then [] else x::take (n-1) rest;;

  let rec drop n list = 
    match list with
    [] -> []
    | x::rest -> if n=0 then x::rest else drop (n-1) rest;;

  let rec balpreorder list = 
    match list with
    [] -> Empty
    | x::rest -> let lunghezza = List.length list in
                  Tr(x, balpreorder(take(lunghezza/2) rest), balpreorder(drop(lunghezza/2) rest));;

  let () =
  Printf.printf "\n=============================\n";
  Printf.printf "Test della funzione balpreorder con stampa grafica:\n\n";

  let t = [1;2;3;4;5]
  in

  let albero = balpreorder t in
  Printf.printf "Albero expected :\n";
  let expected= Tr(1,Tr(2,Empty,
 Tr(3,Empty,Empty)),
 Tr(4,Empty, Tr (5, Empty, Empty))) in
  print_tree_graphic expected;
  Printf.printf "Albero risultato :\n";
  print_tree_graphic albero;
  Printf.printf "Lista expected :\n";
  print_int_list t;
  let listaRisultato = preorder (balpreorder t) in
  Printf.printf "Lista risultato :\n";
  print_int_list listaRisultato;;

  let rec balinorder list = 
    match list with
    [] -> Empty
    | lst -> let centro = (List.length lst)/2 in
             let elementoCentrale = List.nth lst centro in
             let listaSinistra = take centro lst in
             let listaDestra = drop (centro+1) lst in
            Tr(elementoCentrale, balinorder(listaSinistra), balinorder(listaDestra));;


   let () =
  Printf.printf "\n=============================\n";
  Printf.printf "Test della funzione balinorder con stampa grafica:\n\n";

  let t = [1;2;3;4;5]
  in

  let albero = balinorder t in
  Printf.printf "Albero expected :\n";
  let expected= Tr(3, Tr (2, Tr (1, Empty, Empty),
 Empty),
 Tr (5, Tr (4, Empty, Empty),
 Empty)) in
  print_tree_graphic expected;
  Printf.printf "Albero risultato :\n";
  print_tree_graphic albero;
  Printf.printf "Lista expected :\n";
  print_int_list t;
  let listaRisultato = inorder (balinorder t) in
  Printf.printf "Lista risultato :\n";
  print_int_list listaRisultato;;

(*  foglie_in_lista: ’a list-> ’a tree-> bool, che, data una list lst e
 un albero binario t, determini se ogni foglia di t appartiene a lst. (Una foglia
 è rappresentata da un valore della forma Tr(x,Empty,Empty)). *) 

 let rec foglie_in_lista list albero = 
    match albero with
    Empty -> true
    | Tr(x, Empty, Empty) ->  if (List.mem x list) then true else false
    | Tr(x, l, r) -> foglie_in_lista list l && foglie_in_lista list r;;

let test_foglie_in_lista () =
  let empty_tree = Empty in
  let leaf_tree1 = Tr(5, Empty, Empty) in
  let leaf_tree2 = Tr("a", Empty, Empty) in
  let tree1 = Tr(1, Tr(2, Empty, Empty), Tr(3, Empty, Empty)) in
  let tree2 = Tr(1, Tr(2, Tr(4, Empty, Empty), Empty), Tr(3, Empty, Empty)) in
  let tree3 = Tr(1, Tr(2, Empty, Empty), Tr(3, Tr(5, Empty, Empty), Empty)) in
  let tree4 = Tr(1, Tr(2, Tr(4, Empty, Empty), Tr(5, Empty, Empty)), Tr(3, Tr(6, Empty, Empty), Tr(7, Empty, Empty))) in

  assert (foglie_in_lista [] empty_tree = true); (* Albero vuoto, lista vuota *)
  assert (foglie_in_lista [5] leaf_tree1 = true); (* Albero con una foglia presente nella lista *)
  assert (foglie_in_lista ["a"] leaf_tree2 = true); (* Albero con una foglia di tipo diverso *)
  assert (foglie_in_lista [2; 3] tree1 = true); (* Tutte le foglie sono nella lista *)
  assert (foglie_in_lista [2; 3; 4] tree2 = true); (* Tutte le foglie sono nella lista (anche in profondità) *)
  assert (foglie_in_lista [2; 5] tree3 = true); (* Tutte le foglie sono nella lista *)
  assert (foglie_in_lista [4; 5; 6; 7] tree4 = true); (* Tutte le foglie sono nella lista (albero completo) *)

  assert (foglie_in_lista [1] leaf_tree1 = false); (* Foglia non presente nella lista *)
  assert (foglie_in_lista [] leaf_tree1 = false); (* Lista vuota, albero con foglia *)
  assert (foglie_in_lista [2] tree1 = false); (* Manca una foglia nella lista *)
  assert (foglie_in_lista [2; 4] tree2 = false); (* Manca una foglia nella lista *)
  assert (foglie_in_lista [2; 3; 6] tree3 = false); (* Foglia non presente nella lista *)
  assert (foglie_in_lista [4; 6] tree4 = false); (* Mancano delle foglie nella lista *)

  print_endline "Test per foglie_in_lista completati con successo.";;

(* Esegui i test *)
test_foglie_in_lista ();;

(* num_foglie: ’a tree-> int che, applicata a un albero binario, riporti il
 numero di foglie dell’albero. *)

 let rec num_foglie albero =
  match albero with
  Empty -> 0
  | Tr(x, Empty, Empty) -> 1
  | Tr(x, l, r) -> num_foglie l + num_foglie r;;  

  let test_num_foglie () =
  let empty_tree = Empty in
  let leaf_tree1 = Tr(5, Empty, Empty) in
  let leaf_tree2 = Tr("a", Empty, Empty) in
  let tree1 = Tr(1, Tr(2, Empty, Empty), Tr(3, Empty, Empty)) in
  let tree2 = Tr(1, Tr(2, Tr(4, Empty, Empty), Empty), Tr(3, Empty, Empty)) in
  let tree3 = Tr(1, Tr(2, Empty, Empty), Tr(3, Tr(5, Empty, Empty), Empty)) in
  let tree4 = Tr(1, Tr(2, Tr(4, Empty, Empty), Tr(5, Empty, Empty)), Tr(3, Tr(6, Empty, Empty), Tr(7, Empty, Empty))) in
  let non_leaf_tree = Tr(1, Tr(2, Tr(4, Empty, Empty), Tr(5, Empty, Empty)), Tr(3, Empty, Empty)) in

  assert (num_foglie empty_tree = 0); (* Albero vuoto non ha foglie *)
  assert (num_foglie leaf_tree1 = 1); (* Albero con una sola foglia *)
  assert (num_foglie leaf_tree2 = 1); (* Albero con una sola foglia di tipo diverso *)
  assert (num_foglie tree1 = 2); (* Albero con due foglie *)
  assert (num_foglie tree2 = 2); (* Albero con due foglie (una in profondità) *)
  assert (num_foglie tree3 = 2); (* Albero con due foglie (una a destra) *)
  assert (num_foglie tree4 = 4); (* Albero completo con quattro foglie *)
  assert (num_foglie non_leaf_tree = 3); (* Albero con nodi interni e foglie *)

  print_endline "Test per num_foglie completati con successo.";;

(* Esegui i test *)
test_num_foglie ();;

(*Una lista di booleani L può determinare un sottoalbero di un albero binario:
 quello che si ottiene, a partire dalla radice, scendendo al figlio sinistro per ogni
 true nella lista, al figlio destro per ogni false. Se la lista è più lunga del ramo
 che si ottiene da essa, allora il sottoalbero determinato da L è l’albero vuoto.
 Si consideri ad esempio l’albero completo rappresentato sopra per l’esercizio 2b.
 La lista [true;false;false] determina il sottoalbero che ha radice 11. La
 lista [false;true] determina il sottoalbero con radice 6. Liste con più di 3
 elementi determinano l’albero vuoto.
 Scrivere una funzione segui_bool: bool list-> ’a tree-> ’a che, data
 una lista L di booleani e un albero binario T, riporti la radice del sottoalbero
 di T determinato da L, se questo non è vuoto, un errore altrimenti.
 Ad esempio, la funzione, applicata alla lista [true;false;false] e all’albero
 rappresentato per l’esercizio 2b riporterà 11. Applicata alla lista [false;true]
 e allo stesso albero, riporterà 6. Riporterà un errore se la lista ha più di 3
 elementi.*)
 exception AlberoVuoto

 let rec segui_bool list albero=
    match albero with
    Empty -> raise AlberoVuoto
    | Tr(x, l, r) -> match list with
                    [] -> x
                    | y::rest -> if y then segui_bool rest l else segui_bool rest r;;

 let albero_esercizio_2b =
  Tr(1,
     Tr(2,
        Tr(4, Empty, Empty),
        Tr(5, Empty, Empty)),
     Tr(3,
        Tr(6, Empty, Empty),
        Tr(7, Empty, Empty)));;

(*  Se T e’ un albero binario etichettato da numeri interi, il costo di una foglia N
 di T è la somma di tutti i nodi che si trovano sul ramo che va dalla radice di
 T a N. Scrivere una funzione foglia_costo: int tree-> (int * int) che,
 dato un albero binario di interi, restuisca l’etichetta e il costo di una delle foglie
 più costosa dell’albero.
 Ad esempio, se l’albero è quello rappresentato a fianco, la funzione potrà
 riportare, indifferentemente, la coppia
 (5,15) oppure (3,15). *)

 (* Esempio di albero fornito *)
let example_tree =
  Tr(0,
     Tr(10,
        Tr(2, Empty, Empty),
        Tr(5, Empty, Empty)),
     Tr(6,
        Tr(6,
           Empty,
           Tr(3, Empty, Empty)),
        Tr(4,
           Tr(3, Empty, Empty),
           Tr(4, Empty, Empty))));;

let albero1 = Tr(5, Empty, Empty)
(* Risultato atteso di foglia_costo: (5, 5) *)


 let foglia_costo albero = 
  let rec aux costo albero =
    match albero with
    | Empty -> (0, 0)  
    | Tr(x, Empty, Empty) -> (x, costo + x) 
    | Tr(x, l, r) ->
        let (foglia1, costo1) = aux (costo + x) l in
        let (foglia2, costo2) = aux (costo + x) r in
        if costo1 >= costo2 then (foglia1, costo1) else (foglia2, costo2)
  in
  aux 0 albero


 let stampa_coppia (x, y) =
  Printf.printf "(%d, %d)\n" x y;;

  let (x, y) = foglia_costo example_tree;;
  Printf.printf "Expected (5, 15)";;
  stampa_coppia (x, y);;
  let (x, y) = foglia_costo albero1;;
  Printf.printf "Expected (5, 5)";;
  stampa_coppia (x, y);;

  let albero2 = Tr(10, Tr(2, Empty, Empty), Tr(8, Empty, Empty));;
(* Risultato atteso di foglia_costo: (8, 18) *)
Printf.printf "Expected (8, 18)";;
let (x, y) = foglia_costo albero2;;
  stampa_coppia (x, y);;

  let albero3 = Tr(1, Tr(2, Tr(3, Tr(4, Empty, Empty), Empty), Empty), Empty);;
  Printf.printf "Expected (4, 10)";;
(* Risultato atteso di foglia_costo: (4, 1 + 2 + 3 + 4) = (4, 10) *)
let (x, y) = foglia_costo albero3;;
  stampa_coppia (x, y);;

  let albero_test_costo_etichetta =
  Tr(2,
     Tr(1, (* Sottoalbero con foglia di etichetta 1 *)
        Tr(3,
           Tr(5,
              Tr(7, Empty, Empty), (* Costo parziale: 2+1+3+5+7 = 18 *)
              Empty),
           Tr(1, Empty, Empty)), (* Foglia 1, costo: 2+1+3+1 = 7 *)
        Empty),
     Tr(15, (* Sottoalbero con foglia di etichetta 15 *)
        Empty,
        Tr(1, Empty, Empty))) (* Foglia 1, costo: 2+15+1 = 18 *);;

  Printf.printf "Expected (1, 18)";;
(* Risultato atteso di foglia_costo: (4, 1 + 2 + 3 + 4) = (4, 10) *)
let (x, y) = foglia_costo albero_test_costo_etichetta;;
  stampa_coppia (x, y);;

(* Definire una funzione foglie_costi: int tree-> (int * int) list che,
 applicata a un albero binario T etichettato da interi, riporti una lista di coppie,
 ciascuna delle quali ha la forma (f,n), dove f è l’etichetta di una foglia in T e
 n il costo di tale foglia (dove il costo di una foglia è definito come nell’esercizio
 precedente). Ad esempio, se l’albero è quello rappresentato nell’esercizio precedente, la fun
zione riporterà la lista [(2,12);(5,15);(3,15);(3,13);(4,14)] (o una sua
 permutazione).*)

 type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

 let foglie_costi albero = 
  let rec aux risultato costo albero =
    match albero with
    | Empty -> []  
    | Tr(x, Empty, Empty) -> risultato@[(x, costo + x)]
    | Tr(x, l, r) -> (aux risultato (costo+x) l)@(aux risultato (costo+x) r)
  in
  aux [] 0 albero;;

  (* Funzione per stampare una lista di coppie (int * int) *)
let print_int_pair_list lst =
  let rec aux l =
    match l with
    | [] -> print_string "]\n"
    | [(a, b)] -> Printf.printf "(%d, %d)]\n" a b
    | (a, b)::xs -> Printf.printf "(%d, %d); " a b; aux xs
  in
  print_string "[";
  aux lst;;

(* Funzione per stampare l'albero in modo grafico *)
let print_tree_graphic tree =
  let rec aux prefix is_left t =
    match t with
    | Empty -> ()
    | Tr (v, l, r) ->
        Printf.printf "%s%s%d\n" prefix (if is_left then "├── " else "└── ") v;
        let new_prefix = prefix ^ (if is_left then "│   " else "    ") in
        aux new_prefix true r;   (* invertiti l e r per coerenza visiva *)
        aux new_prefix false l
  in
  match tree with
  | Empty -> print_endline "."
  | Tr (v, l, r) ->
      Printf.printf "%d\n" v;
      aux "" true r;
      aux "" false l;;

(* Albero di esempio *)
let esempio =
  Tr(0,
    Tr(10,
      Tr(2, Empty, Empty),
      Tr(5, Empty, Empty)),
    Tr(6,
      Tr(3, Empty, Empty),
      Tr(4, Empty, Empty)));;

(* Test *)
let () =
  Printf.printf "\n=============================\n";
  Printf.printf "ALBERO BINARIO DI ESEMPIO:\n";
  print_tree_graphic esempio;

  Printf.printf "\n=============================\n";
  Printf.printf "Risultato atteso (ordine irrilevante):\n";
  Printf.printf "[(2,12); (5,15); (3,15); (4,14)]\n";

  Printf.printf "\nRisultato ottenuto:\n";
  let risultato = foglie_costi esempio in
  print_int_pair_list risultato

  type expr =
 Jolly
 | Int of int
 | Var of string
 | Sum of expr * expr
 | Diff of expr * expr
 | Mult of expr * expr
 | Div of expr * expr;;
  (* Si consideri la seguente estensione del tipo di dati expr per la rappresentazione
 di espressioni aritmetiche:
 Il “jolly” è un carattere speciale @, usato come 'metavariabile' (come la variabile
 muta), che può comparire in una espressione. Chiamiamo “espressione” una
 expr che non contenga alcun jolly, mentre le expr che contengono uno o più
 jolly vengono chiamate “modelli” (o “pattern”). Ad esempio, x × @ è un pattern.
 Una espressione E e un modello M si confrontano positivamente se hanno la
 stessa struttura, cioe’ se E si ottiene da M sostuendo i caratteri @ da opportune
 sottoespressioni (non necessariamente dalla stessa sottoespressione). Il carattere
 @ si comporta cioè come una variabile “muta”. Ad esempio: l’espressione a +
 ((b ∗ c) −d) si confronta positivamente con i seguenti modelli:
 @
 a +@
 @+(@−d)
 a +(@−@)
 Non si confronta positivamente con:
 a +(@−b)
 @+(@×@)
 Scrivere una funzione pattern_matching: expr-> expr-> bool che, da
ta un’espressione E e un modello M, determini se E e M si confrontano
 positivamente oppure no.*)

 let rec pattern_matching e m =
  match (e,m) with
  (_, Jolly) -> true
  | (Int n, Int y) -> n=y
  | (Int n, _) -> false
  | (Var s, Var t) -> s=t
  | (Var _, _) -> false
  | (Sum(e1, e2), Sum(m1,m2)) | (Diff(e1, e2), Diff(m1,m2)) | (Mult(e1, e2), Mult(m1,m2)) | (Div(e1, e2), Div(m1,m2)) -> pattern_matching e1 m1 && pattern_matching e2 m2
  | _ -> false;;

(* Definizioni delle espressioni e dei modelli per i test *)

(* Espressione E di riferimento: a + ((b * c) - d) *)
let e_ref = Sum (Var "a", Diff (Mult (Var "b", Var "c"), Var "d"));;

(* Modelli che dovrebbero confrontarsi positivamente con E *)
let m_jolly = Jolly;;
let m_a_plus_jolly = Sum (Var "a", Jolly);;
let m_jolly_plus_jolly_minus_d = Sum (Jolly, Diff (Jolly, Var "d"));;
let m_a_plus_jolly_minus_jolly = Sum (Var "a", Diff (Jolly, Jolly));;

(* Modelli che NON dovrebbero confrontarsi positivamente con E *)
let m_a_plus_jolly_minus_b = Sum (Var "a", Diff (Jolly, Var "b"));; (* 'b' in E, 'b' in M -> non matcha *)
let m_jolly_plus_jolly_mult_jolly = Sum (Jolly, Mult (Jolly, Jolly));; (* Div in E, Mult in M -> non matcha *)
let m_diff_struct = Diff (Var "a", Sum (Jolly, Jolly));; (* Struttura diversa: Sum in E, Diff in M *)
let m_var_a = Var "a";; (* Pattern troppo specifico *)
let m_int_5 = Int 5;; (* Tipo diverso *)
let m_mult_var_a_jolly = Mult (Var "a", Jolly);; (* Operatore diverso *)


let test_pattern_matching () =
  (* Test case positivi *)
  assert (pattern_matching e_ref m_jolly = true);
  assert (pattern_matching e_ref m_a_plus_jolly = true);
  assert (pattern_matching e_ref m_jolly_plus_jolly_minus_d = true);
  assert (pattern_matching e_ref m_a_plus_jolly_minus_jolly = true);

  (* Test case positivi aggiuntivi *)
  assert (pattern_matching (Int 5) Jolly = true); (* Espressione semplice con Jolly *)
  assert (pattern_matching (Var "x") Jolly = true);
  assert (pattern_matching (Sum (Int 1, Int 2)) (Sum (Jolly, Jolly)) = true);
  assert (pattern_matching (Sum (Int 1, Int 2)) (Sum (Int 1, Jolly)) = true);
  assert (pattern_matching (Mult (Var "a", Var "b")) (Mult (Jolly, Jolly)) = true);
  assert (pattern_matching (Mult (Var "a", Var "b")) (Mult (Var "a", Jolly)) = true);
  assert (pattern_matching (Mult (Var "a", Var "b")) (Mult (Jolly, Var "b")) = true);

  (* Test case negativi *)
  assert (pattern_matching e_ref m_a_plus_jolly_minus_b = false); (* b vs d *)
  assert (pattern_matching e_ref m_jolly_plus_jolly_mult_jolly = false); (* Div vs Mult *)
  assert (pattern_matching e_ref m_diff_struct = false); (* Struttura diversa *)
  assert (pattern_matching e_ref m_var_a = false); (* Pattern troppo specifico *)
  assert (pattern_matching e_ref m_int_5 = false); (* Tipo diverso *)
  assert (pattern_matching e_ref m_mult_var_a_jolly = false); (* Operatore diverso *)

  (* Test case negativi aggiuntivi *)
  assert (pattern_matching (Int 5) (Int 6) = false); (* Valori diversi *)
  assert (pattern_matching (Var "x") (Var "y") = false); (* Nomi variabili diversi *)
  assert (pattern_matching (Sum (Int 1, Int 2)) (Sum (Int 3, Int 4)) = false); (* Valori diversi *)
  assert (pattern_matching (Sum (Int 1, Int 2)) (Sum (Int 1, Int 3)) = false); (* Valore singolo diverso *)
  assert (pattern_matching (Sum (Int 1, Int 2)) (Int 1) = false); (* Espressione vs pattern più semplice *)
  assert (pattern_matching (Int 1) (Sum (Int 1, Int 2)) = false); (* Espressione vs pattern più complesso *)
  assert (pattern_matching (Sum (Jolly, Jolly)) (Sum (Int 1, Int 2)) = false); (* Pattern non può essere espressione *)
  assert (pattern_matching (Sum (Int 1, Jolly)) (Sum (Jolly, Int 2)) = false); (* Jolly non significa "qualsiasi cosa" qui *)
  assert (pattern_matching (Sum (Int 1, Jolly)) (Jolly) = true); (* Jolly matcha sempre *)


  print_endline "Test per pattern_matching completati.";;

(* Esegui i test *)
let _ = test_pattern_matching ();;

(*  Definire una funzione max_common_subtree: string tree-> string tree-> string tree, che, dati due alberi binari A e B, i cui nodi sono etichettati
 da stringhe, costruisca il massimo sottoalbero comune a A e B, partendo dalla
 radice: i nodi di tale sottoalbero avranno la stessa etichetta che hanno i nodi
 corrispondenti in A e in B, se essi sono uguali; altrimenti, se il nodo x di A è
 diverso dal corrispondente nodo di B (o se uno dei due nodi non c’è), il nodo
 corrispondente a x nel massimo sottoalbero comune di A e B sarà una foglia
 etichettata da "@".
 Ad esempio, il massimo sottoalbero comune dei due alberi rappresentati a si
nistra e al centro qui sotto è quello rappresentato a destra (sono qui omesse le
 virgolette per delimitare le stringhe e • rappresenta l’albero vuoto). *)

 let rec max_common_subtree a b =
  match (a,b) with
  (Empty, Empty) -> Empty
  | (_, Empty) | (Empty, _) -> Tr("@", Empty, Empty) 
  | (Tr(x, l, r), Tr(y, s, d)) -> if x=y then Tr(x, max_common_subtree l s, max_common_subtree r d) else Tr("@", Empty, Empty);; 

(* Funzione di stampa generica per 'a tree *)
let rec string_of_tree string_of_val = function
  | Empty -> "•" (* Usiamo • per Empty *)
  | Tr (v, l, r) ->
    "(" ^ (string_of_val v) ^ " " ^ (string_of_tree string_of_val l) ^ " " ^ (string_of_tree string_of_val r) ^ ")"

(* Funzione per stampare l'albero in modo più leggibile *)
let print_tree string_of_val tree =
  print_endline (string_of_tree string_of_val tree)

(* Funzione di uguaglianza per alberi generici *)
let rec tree_equal val_equal t1 t2 =
  match (t1, t2) with
  | (Empty, Empty) -> true
  | (Tr (v1, l1, r1), Tr (v2, l2, r2)) ->
      val_equal v1 v2 && tree_equal val_equal l1 l2 && tree_equal val_equal r1 r2
  | _ -> false

(* --- INIZIO TEST CASES --- *)

let test_max_common_subtree () =

  (* Funzione per convertire una stringa in stringa (per i test) *)
  let string_of_string s = s in

  (* Albero A (Input) *)
  let a_tree =
    Tr ("a",
        Tr ("b",
            Tr ("d", Empty, Empty),
            Tr ("e", Empty, Empty)),
        Tr ("c",
            Tr ("f", Empty, Empty),
            Tr ("g", Empty, Empty)))
  in

  (* Albero B (Input) *)
  let b_tree =
    Tr ("a",
        Tr ("b",
            Tr ("d", Empty, Empty),
            Tr ("e", Empty, Empty)),
        Tr ("x", (* Diverso da "c" in a_tree *)
            Tr ("y", Empty, Empty),
            Tr ("z", Empty, Empty)))
  in

  (* Risultato Atteso per il caso principale *)
  let expected_result_tree =
    Tr ("a",
        Tr ("b",
            Tr ("d", Empty, Empty),
            Tr ("e", Empty, Empty)),
        Tr ("@", Empty, Empty))
  in

  (* Esegui la funzione max_common_subtree (devi averla definita tu) *)
  let actual_result = max_common_subtree a_tree b_tree in

  print_endline "--- Test Caso Principale ---";
  print_endline "Albero A (Input):";
  print_tree string_of_string a_tree;
  print_endline "\nAlbero B (Input):";
  print_tree string_of_string b_tree;
  print_endline "\nRisultato atteso:";
  print_tree string_of_string expected_result_tree;
  print_endline "\nRisultato ottenuto:";
  print_tree string_of_string actual_result;

  assert (tree_equal (=) actual_result expected_result_tree);
  print_endline "\nTest del sottoalbero comune completato con successo (caso principale).\n";

  (* --- Test Caso: un albero è vuoto --- *)
  let a_empty = Empty in
  let b_simple = Tr ("root", Tr ("child", Empty, Empty), Empty) in
  let expected_empty_result = Tr ("@", Empty, Empty) in
  let actual_empty_result = max_common_subtree a_empty b_simple in

  print_endline "--- Test Caso: un albero è Empty ---";
  print_endline "Albero A (Input):"; print_tree string_of_string a_empty;
  print_endline "Albero B (Input):"; print_tree string_of_string b_simple;
  print_endline "\nRisultato atteso:"; print_tree string_of_string expected_empty_result;
  print_endline "\nRisultato ottenuto:"; print_tree string_of_string actual_empty_result;
  assert (tree_equal (=) actual_empty_result expected_empty_result);
  print_endline "\nTest del sottoalbero comune completato con successo (caso albero Empty).\n";

  (* --- Test Caso: radici diverse --- *)
  let a_diff_root = Tr ("x", Tr ("a", Empty, Empty), Empty) in
  let b_diff_root = Tr ("y", Tr ("b", Empty, Empty), Empty) in
  let expected_diff_root = Tr ("@", Empty, Empty) in
  let actual_diff_root = max_common_subtree a_diff_root b_diff_root in

  print_endline "--- Test Caso: radici diverse ---";
  print_endline "Albero A (Input):"; print_tree string_of_string a_diff_root;
  print_endline "Albero B (Input):"; print_tree string_of_string b_diff_root;
  print_endline "\nRisultato atteso:"; print_tree string_of_string expected_diff_root;
  print_endline "\nRisultato ottenuto:"; print_tree string_of_string actual_diff_root;
  assert (tree_equal (=) actual_diff_root expected_diff_root);
  print_endline "\nTest del sottoalbero comune completato con successo (caso radici diverse).\n";

  (* --- Test Caso: un ramo in un albero è Empty, l'altro no --- *)
  let a_partially_empty = Tr ("a", Tr ("b", Empty, Empty), Empty) in
  let b_partially_empty = Tr ("a", Tr ("b", Empty, Empty), Tr("c", Empty, Empty)) in
  let expected_partially_empty = Tr ("a", Tr ("b", Empty, Empty), Tr("@", Empty, Empty)) in
  let actual_partially_empty = max_common_subtree a_partially_empty b_partially_empty in

  print_endline "--- Test Caso: un ramo è Empty, l'altro no ---";
  print_endline "Albero A (Input):"; print_tree string_of_string a_partially_empty;
  print_endline "Albero B (Input):"; print_tree string_of_string b_partially_empty;
  print_endline "\nRisultato atteso:"; print_tree string_of_string expected_partially_empty;
  print_endline "\nRisultato ottenuto:"; print_tree string_of_string actual_partially_empty;
  assert (tree_equal (=) actual_partially_empty expected_partially_empty);
  print_endline "\nTest del sottoalbero comune completato con successo (caso ramo Empty).\n";

  (* --- Test Caso: alberi identici --- *)
  let identical_tree = Tr("p", Tr("q", Empty, Empty), Tr("r", Empty, Empty)) in
  let expected_identical_tree = identical_tree in (* Il risultato atteso è l'albero stesso *)
  let actual_identical_tree = max_common_subtree identical_tree identical_tree in

  print_endline "--- Test Caso: alberi identici ---";
  print_endline "Albero A (Input):"; print_tree string_of_string identical_tree;
  print_endline "Albero B (Input):"; print_tree string_of_string identical_tree;
  print_endline "\nRisultato atteso:"; print_tree string_of_string expected_identical_tree;
  print_endline "\nRisultato ottenuto:"; print_tree string_of_string actual_identical_tree;
  assert (tree_equal (=) actual_identical_tree expected_identical_tree);
  print_endline "\nTest del sottoalbero comune completato con successo (casi identici).\n";
;;

(* Esegui tutti i test *)
let _ = test_max_common_subtree ();;

(* Contare quante volte occorre in un albero ciascuna etichetta *)
(* count t = lista di coppie contenente, per ogni etichetta x di qualche nodo dell'albero, una (unica) coppia (x, n), dove n è il numero di nodi etichettati da x*)

let rec add x assoc_list = 
  match assoc_list with
  [] -> [(x, 1)]
  | (a, n)::rest -> if a=x then (a,n+1)::rest
  else
      (a, n):: (add x rest);;

let rec count t = 
  let rec aux risultato t =
  match t with
  Empty -> risultato
  | Tr(x, l, r) -> aux (aux (add x risultato) l) r
  in aux [] t;;

let print_int_pair_list lst =
  let print_pair (x, y) =
    Printf.printf "(%s, %d)\n" x y
  in
  List.iter print_pair lst

  let a_tree =
    Tr ("a",
        Tr ("a",
            Tr ("a", Empty, Empty),
            Tr ("a", Empty, Empty)),
        Tr ("a",
            Tr ("a", Empty, Empty),
            Tr ("a", Empty, Empty)));;
  
  let x = count a_tree;;

  print_int_pair_list x;;

  (* (a) Scrivere un predicato stessa_struttura: ’a tree -> ’a tree -> bool
che determini se due alberi binari hanno la stessa struttura (cioè se essi sono uguali quando si ignorano le rispettive etichette; ad esempio i tre alberi
rappresentati per l’esercizio successivo hanno tutti la stessa struttura) *)

  let rec stessa_struttura albero1 albero2 =
    match (albero1, albero2) with
    (Empty, Empty) -> true
    | (Empty, Tr(x, l, r)) -> false
    | (Tr(x,l,r), Empty) -> false
    | (Tr(x, l1, r1), Tr(y, l2, r2)) -> (stessa_struttura l1 l2) && (stessa_struttura r1 r2);;

    let a1 = Tr(1,
            Tr(2, Empty, Empty),
            Tr(3, Empty, Empty))

let a2 = Tr("a",
            Tr("b", Empty, Empty),
            Tr("c", Empty, Empty))

let a3 = Tr(true,
            Tr(false, Empty, Empty),
            Tr(true, Empty, Empty))

            let b1 = Tr(1,
            Tr(2, Tr(3, Empty, Empty), Empty),
            Empty)

let b2 = Tr(1,
            Empty,
            Tr(2, Empty, Tr(3, Empty, Empty)))

            let () =
  assert (stessa_struttura a1 a2);            (* true *)
  assert (stessa_struttura a2 a3);            (* true *)
  assert (not (stessa_struttura a1 b1));      (* false *)
  assert (not (stessa_struttura b1 b2));      (* false *)
  Printf.printf "Tutti i test passati con successo.\n";;


