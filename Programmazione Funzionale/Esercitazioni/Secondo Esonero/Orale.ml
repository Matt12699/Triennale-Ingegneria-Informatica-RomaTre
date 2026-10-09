(* (Esercizio 8 pag 45 del libro di testo) Scrivere una funzione data: int *
string -> bool, che, applicata a una coppia (d,m), dove d è un intero e
m una stringa, determini se la coppia rappresenta una data corretta, assumendo che l’anno non sia bisestile. Si assume che i mesi siano rappresentati
da stringhe con caratteri minuscoli ("gennaio", "febbraio",. . . ).
La funzione non deve mai sollevare eccezioni, ma riportare sempre un
bool.*)

let rec data (d,m) =
  match m with
  "gennaio" | "marzo" | "maggio" | "luglio" | "agosto" | "ottobre" | "dicembre" -> d>=1 && d<=31
  | "aprile" | "giugno" | "settembre" | "novembre" -> d>=1 && d<=30
  |"febbraio" -> d>=1 && d<=28
  | _ -> false;;

(* Spaziatore *)
Printf.printf "\n--------------------\n";
(* Test per data *)
Printf.printf "Test per data:\n";
(* Esempio di test *)
Printf.printf "  data(29, \"febbraio\") = %b (atteso: false)\n" (data(29, "febbraio"));
Printf.printf "  data(15, \"marzo\") = %b (atteso: true)\n" (data(15, "marzo"));
Printf.printf "  data(31, \"aprile\") = %b (atteso: false)\n" (data(31, "aprile"));
Printf.printf "  data(0, \"gennaio\") = %b (atteso: false)\n" (data(0, "gennaio"));
Printf.printf "  data(1, \"gennaio\") = %b (atteso: true)\n" (data(1, "gennaio"));
Printf.printf "  data(31, \"dicembre\") = %b (atteso: true)\n" (data(31, "dicembre"));
Printf.printf "  data(32, \"gennaio\") = %b (atteso: false)\n" (data(32, "gennaio"));
Printf.printf "  data(1, \"meseinesistente\") = %b (atteso: false)\n" (data(1, "meseinesistente"));;

(* sumbetween: int -> int -> int, tale che sumbetween n m = somma degli interi compresi tra n e m (estremi inclusi). *)

let rec sumbetween n m = 
  if n = m then m
  else
    n + (sumbetween (n+1) m);;

(* Spaziatore *)
Printf.printf "\n--------------------\n";
(* Test per sumbetween *)
Printf.printf "Test per sumbetween:\n";
(* Esempi di test *)
Printf.printf "  sumbetween 1 5 = %d (atteso: 15)\n" (sumbetween 1 5);
Printf.printf "  sumbetween 0 0 = %d (atteso: 0)\n" (sumbetween 0 0);
Printf.printf "  sumbetween -3 3 = %d (atteso: 0)\n" (sumbetween (-3) 3);
Printf.printf "  sumbetween -5 -1 = %d (atteso: -15)\n" (sumbetween (-5) (-1));
Printf.printf "  sumbetween 10 10 = %d (atteso: 10)\n" (sumbetween 10 10);;

(* Una funzione duplica: ’a list -> ’a list che, applicata a una
lista xs = [x1;x2;...;xn], riporti la lista [x1;x1;x2;x2;...;xn;xn]. *)

let rec duplica lista = 
  let rec aux risultato lista =
    match lista with
    [] -> risultato
    | x::rest -> aux (x::x::risultato) rest
  in List.rev(aux [] lista);;

let rec duplica lista =
  match lista with
  [] -> []
  | x::rest -> x::x:: (duplica rest);;

(* Spaziatore *)
Printf.printf "\n--------------------\n";
(* Test per duplica *)
Printf.printf "Test per duplica:\n";
(* Esempi di test *)
Printf.printf "  duplica [1;2;3] = [";
List.iter (fun x -> Printf.printf "%d;" x) (duplica [1;2;3]);
Printf.printf "] (atteso: [1;1;2;2;3;3;])\n";

Printf.printf "  duplica [\"a\";\"b\"] = [";
List.iter (fun x -> Printf.printf "%s;" x) (duplica ["a";"b"]);
Printf.printf "] (atteso: [\"a\";\"a\";\"b\";\"b\";])\n";

Printf.printf "  duplica [] = [";
List.iter (fun x -> Printf.printf "%d;" x) (duplica []);
Printf.printf "] (atteso: [])\n";

Printf.printf "  duplica [7] = [";
List.iter (fun x -> Printf.printf "%d;" x) (duplica [7]);
Printf.printf "] (atteso: [7;7;])\n";;

(* (a) Scrivere una funzione find: ’a -> ’a list -> ’a list * ’a list, che,
applicata a un elemento x e a una lista L, spezzi L in due parti: la prima
contiene tutti gli elementi che vanno dall’inizio della lista fino alla prima
occorrenza di x esclusa; la seconda contiene tutti gli elementi che seguono
la prima occorrenza di x. La funzione solleverà un’eccezione se L non contiene x. Ad esempio, find 3 [1;2;3;4;5;6;3] = ([1;2],[4;5;6;3]). *)
exception NonTrovato
let find x lista=
  let rec aux prima lista =
  match lista with
  [] -> raise NonTrovato
  | y::rest -> if y=x then (List.rev(prima), rest)
  else
    aux (y::prima) rest
  in aux [] lista;;

(* Funzione di stampa per le liste, se non già definita *)
let print_list_int l =
  Printf.printf "[";
  List.iter (fun x -> Printf.printf "%d;" x) l;
  Printf.printf "]";;

let print_list_string l =
  Printf.printf "[";
  List.iter (fun x -> Printf.printf "%s;" x) l;
  Printf.printf "]";;

(* Spaziatore *)
Printf.printf "\n--------------------\n";
(* Test per find *)
Printf.printf "Test per find:\n";;
(* Esempi di test *)

(* Test 1: Elemento presente al centro *)
let (pre1, post1) = find 3 [1;2;3;4;5;6;3];;
Printf.printf "  find 3 [1;2;3;4;5;6;3] = (";
print_list_int pre1;
Printf.printf ",";
print_list_int post1;
Printf.printf ") (atteso: ([1;2;],[4;5;6;3;]))\n";;

(* Test 2: Elemento presente all'inizio *)
let (pre2, post2) = find 1 [1;2;3;4;5];;
Printf.printf "  find 1 [1;2;3;4;5] = (";
print_list_int pre2;
Printf.printf ",";
print_list_int post2;
Printf.printf ") (atteso: ([],[2;3;4;5;]))\n";;

(* Test 3: Elemento presente alla fine *)
let (pre3, post3) = find 5 [1;2;3;4;5];;
Printf.printf "  find 5 [1;2;3;4;5] = (";
print_list_int pre3;
Printf.printf ",";
print_list_int post3;
Printf.printf ") (atteso: ([1;2;3;4;],[]))\n";;

(* Test 4: Lista con un solo elemento, che è l'elemento cercato *)
let (pre4, post4) = find 7 [7];;
Printf.printf "  find 7 [7] = (";
print_list_int pre4;
Printf.printf ",";
print_list_int post4;
Printf.printf ") (atteso: ([],[]))\n";;

(* Test 5: Lista vuota (dovrebbe sollevare un'eccezione, non possiamo testarla direttamente con Printf) *)
(* Esempio di come si potrebbe testare l'eccezione (richiede un blocco try-with) *)
(*
try
  let _ = find 3 [] in
  Printf.printf "  find 3 [] NON ha sollevato un'eccezione (ERRORE)\n";
with Not_found ->
  Printf.printf "  find 3 [] ha sollevato Not_found (atteso)\n";
*)

(* Test 6: Elemento non presente (dovrebbe sollevare un'eccezione, non possiamo testarla direttamente con Printf) *)
(*
try
  let _ = find 9 [1;2;3] in
  Printf.printf "  find 9 [1;2;3] NON ha sollevato un'eccezione (ERRORE)\n";
with Not_found ->
  Printf.printf "  find 9 [1;2;3] ha sollevato Not_found (atteso)\n";
*)

(* Test 7: Elementi stringa *)
let (pre7, post7) = find "c" ["a";"b";"c";"d"];;
Printf.printf "  find \"c\" [\"a\";\"b\";\"c\";\"d\"] = (";;
print_list_string pre7;;
Printf.printf ",";;
print_list_string post7;;
Printf.printf ") (atteso: ([\"a\";\"b\";],[\"d\";]))\n";;

(* Definire il prodotto sul tipo nat così definito*)
type nat = Zero | Succ of nat
(*usando la funzione somma definita a lezione:
(* somma : nat -> nat -> nat *)*)
let rec somma n m =
match n with
Zero -> m
| Succ k -> Succ(somma k m) ;;

let rec prodotto n m =
  match m with
  Zero -> Zero
  | Succ k -> somma n (prodotto n k);;

(* int_of_nat : nat -> int *)
let rec int_of_nat n =
  match n with
  | Zero -> 0
  | Succ k -> 1 + int_of_nat k;;

(* Spaziatore *)
Printf.printf "\n--------------------\n";
(* Test per prodotto *)
Printf.printf "Test per prodotto:\n";;
(* Esempi di test *)

(* Conversione di interi a nat per i test *)
let rec nat_of_int i =
  if i = 0 then Zero
  else Succ (nat_of_int (i - 1));;

(* Test 1: 3 * 2 = 6 *)
let n1 = nat_of_int 3;;
let m1 = nat_of_int 2;;
let res1 = prodotto n1 m1;;
Printf.printf "  prodotto %d %d = %d (atteso: 6)\n" (int_of_nat n1) (int_of_nat m1) (int_of_nat res1);;

(* Test 2: 5 * 0 = 0 *)
let n2 = nat_of_int 5;;
let m2 = nat_of_int 0;;
let res2 = prodotto n2 m2;;
Printf.printf "  prodotto %d %d = %d (atteso: 0)\n" (int_of_nat n2) (int_of_nat m2) (int_of_nat res2);;

(* Test 3: 0 * 7 = 0 *)
let n3 = nat_of_int 0;;
let m3 = nat_of_int 7;;
let res3 = prodotto n3 m3;;
Printf.printf "  prodotto %d %d = %d (atteso: 0)\n" (int_of_nat n3) (int_of_nat m3) (int_of_nat res3);;

(* Test 4: 1 * 4 = 4 *)
let n4 = nat_of_int 1;;
let m4 = nat_of_int 4;;
let res4 = prodotto n4 m4;;
Printf.printf "  prodotto %d %d = %d (atteso: 4)\n" (int_of_nat n4) (int_of_nat m4) (int_of_nat res4);;

(* Test 5: 4 * 1 = 4 *)
let n5 = nat_of_int 4;;
let m5 = nat_of_int 1;;
let res5 = prodotto n5 m5;;
Printf.printf "  prodotto %d %d = %d (atteso: 4)\n" (int_of_nat n5) (int_of_nat m5) (int_of_nat res5);;

(* Test 6: 1 * 1 = 1 *)
let n6 = nat_of_int 1;;
let m6 = nat_of_int 1;;
let res6 = prodotto n6 m6;;
Printf.printf "  prodotto %d %d = %d (atteso: 1)\n" (int_of_nat n6) (int_of_nat m6) (int_of_nat res6);

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

(* 13. (Dal compito d’esame di febbraio 2010). Si definisca un tipo di dati per la
rappresentazione di alberi binari e scrivere un programma con una funzione
path_coprente: ’a tree -> ’a list -> ’a list che, dato un albero A e
una lista di elementi dello stesso tipo dei nodi di A, restituisca, se esiste, un
ramo dell’albero dalla radice a una foglia che contenga tutti i nodi di L (in
qualsiasi ordine) ed eventualmente anche altri nodi. Se un tale cammino non
esiste, il programma solleverà un’eccezione. Si assuma che la lista L sia senza
ripetizioni.
Ad esempio, se l’albero è quello rappresentato per l’esercizio 6 e la lista è [3;6],
la funzione può restituire il ramo [0;6;6;3] oppure 0;6;4;3]. Se la lista è
[10], la funzione può restituire il ramo [0;10;2] oppure 0;10;5].
 *)
exception NonEsiste
let rec path_coprente albero lista =
  let rec aux visitati albero lista= 
    match albero with
    Empty -> if lista = [] then List.rev visitati else raise NonEsiste
    | Tr(x, l, r) ->  try
                      if List.mem x lista then aux (x::visitati) l (List.filter ((<>) x) lista)
                      else
                        aux (x::visitati) l lista
                      with NonEsiste -> if List.mem x lista then aux (x::visitati) r (List.filter ((<>) x) lista) else aux (x::visitati) r lista
                    in aux [] albero lista;;

let example_tree =
  Tr(0,
       Tr(6,
            Tr(6, Tr(3, Empty, Empty), Empty),
            Tr(4, Tr(3, Empty, Empty), Empty)),
       Tr(10,
            Tr(2, Empty, Empty),
            Tr(5, Empty, Empty)));;

(* Spaziatore *)
Printf.printf "\n--------------------\n";
(* Test per path_coprente *)
Printf.printf "Test per path_coprente: \n";;
let albero =
  Tr(0,
    Tr(6,
      Tr(6, Empty, Empty),
      Tr(4,
        Tr(3, Empty, Empty),
        Empty
      )
    ),
    Tr(10,
      Tr(2, Empty, Empty),
      Tr(5, Empty, Empty)
    )
  );;

  (* Test 1: lista [3;6] – dovrebbe restituire un ramo contenente entrambi (in qualsiasi ordine) *)
let () =
  let res1 = path_coprente albero [3;6] in
  Printf.printf "Test 1 (path_coprente [3;6]): %s\n"
    (String.concat ";" (List.map string_of_int res1))

(* Test 2: lista [10] – anche questo esiste *)
let () =
  let res2 = path_coprente albero [10] in
  Printf.printf "Test 2 (path_coprente [10]): %s\n"
    (String.concat ";" (List.map string_of_int res2))

(* Test 3: lista [6;3;4] – esiste un ramo 0 → 6 → 4 → 3 *)
let () =
  let res3 = path_coprente albero [6;3;4] in
  Printf.printf "Test 3 (path_coprente [6;3;4]): %s\n"
    (String.concat ";" (List.map string_of_int res3))

(* Test 4: lista [99] – non esiste, deve sollevare eccezione *)
let () =
  try
    let _ = path_coprente albero [99] in
    Printf.printf "Test 4 FAILED: doveva sollevare eccezione\n"
  with _ ->
    Printf.printf "Test 4 OK: eccezione sollevata come atteso\n";;

(*  (Dal compito d’esame di settembre 2010).
Scrivere una funzione ramo_di_primi: int ntree -> int che, applicata
a un albero n-ario di interi, riporti, se esiste, una foglia n dell’albero tale
che il ramo dell’albero dalla radice a n sia costituito da tutti numeri primi. *)

type 'a ntree = Tr of 'a * 'a ntree list;;

let rec is_primo x =
  if x < 2 then false else
  let rec aux n =
    if n * n > x then true
    else
      if x mod n = 0 then false else aux (n+1)
    in aux 2;;

let rec ramo_di_primi albero =
  match albero with
  Tr(x, []) -> if is_primo x then x else raise NonEsiste
  | Tr(x, figli) -> if not (is_primo x) then raise NonEsiste else
                      let rec scorriFigli figli = 
                        match figli with
                        [] -> raise NonEsiste
                        | y::rest -> try
                                      ramo_di_primi y
                                      with NonEsiste -> scorriFigli rest
                                    in scorriFigli figli;;

Printf.printf "\n==========================\n";;
Printf.printf "Test per ramo_di_primi: \n";;

let albero =
  Tr (2, [
    Tr (3, [
      Tr (4, []);             (* ramo: 2 → 3 → 5 ✔️ *)
      Tr (6, [])              (* ramo: 2 → 3 → 6 ✖️ *)
    ]);
    Tr (2, [
      Tr (7, []);             (* ramo: 2 → 4 → 7 ✖️ *)
    ])
  ])

  let () =
  let risultato = ramo_di_primi albero in
  print_endline ("Foglia con ramo di soli primi: " ^ string_of_int risultato);;

(* Scrivere una funzione ciclo: ’a graph -> ’a -> ’a list che, dato un
grafo orientato G e un nodo N, riporti, se esiste, un ciclo su N, altrimenti
sollevi un’eccezione (in questo caso la funzione deve riportare una lista di
nodi). *)

type 'a graph = ('a * 'a) list;;
exception NonNelGrafo
let rec successori x grafo = if List.exists (function (y,z) -> x=y || z=x)  grafo then List.map (snd) (List.filter (function (y,_)-> x=y) grafo) else raise NonNelGrafo;;

exception NoCiclo
let ciclo grafo nodo =
  let rec aux visitati pendenti = 
    match pendenti with
    [] -> raise NoCiclo
    | x::rest -> if x=nodo then (x::visitati) else
                  try
                  aux (x::visitati) (successori x grafo)
                  with NoCiclo -> aux visitati rest
    in List.rev (aux [nodo] (successori nodo grafo));;

    let graph = [
  (1, 2); (1, 3); (1,4);
  (2, 6); 
  (3, 5);
  (6, 7);(6, 5);
  (5, 4);
  (4, 6); 
];;

  let x = ciclo graph 6;;

  Printf.printf "\n=======================\n";;
  Printf.printf "Ciclo in 6: ";;
  print_list_int x;;

(* (7 punti) Definire una funzione from stringlist to string : string list -> string tale che
from stringlist to string lst restituisca la stringa che si ottiene concatenando le stringhe nella lista lst
inserendo uno spazio tra due stringhe successive di lst (ma senza alcuno spazio prima della prima stirnga
e dopo l’ultima stringa). In particolare, nessuno spazio deve essere inserito se lst contiene nessuno o un
elemento. Per esempio,
• from stringlist to string [] = ""
• from stringlist to string ["ci"] = "ci"
• from stringlist to string ["ci";"ao"] = "ci ao".
• from stringlist to string ["ci";"a";"o"] = "ci a o".
Suggerimento: Utilizzare l’operatore infisso di concatenazione di stringhe ^. *)
Printf.printf "\n=======================\n";;
Printf.printf "From_stringlist_to_string: \n";;
let rec from_stringlist_to_string lista = 
  let rec aux risultato lista =
  match lista with
  [] -> risultato
  | x::rest -> if rest=[] then aux (risultato^x) rest else aux (risultato^x^" ") rest
  in aux "" lista;;

let from_stringlist_to_string lista =
  String.concat " " lista;;

let x = from_stringlist_to_string [];;
Printf.printf "%s\n" x;;
let x = from_stringlist_to_string ["ci"];;
Printf.printf "%s\n" x;;
let x = from_stringlist_to_string ["ci";"ao"];;
Printf.printf "%s\n" x;;
let x = from_stringlist_to_string ["ci";"a";"o"];;
Printf.printf "%s\n" x;;

(* (8 punti) Si consideri la struttura dati per gli alberi n-ari con nodi di tipo ’a definita a lezione come
type ’a ntree = Tr of ’a * ’a ntree list
Definire una funzione lispy : char ntree -> string tale che lispy t restituisca la stringa che rappresenta
l’albero t di caratteri nella maniera seguente:
(a) se t `e un nodo ’c’ senza figli, allora lispy t `e la stringa "c";
(b) se t un nodo ’c’ con figli t1, . . . , tn, allora lispy t `e la stringa "(c s1 ... sn)" dove s1, . . . , sn
sono le stringhe ottenute applicando ricorsivamente lispy a t1, . . . , tn, rispettivamente.
Per esempio (si vedano anche le rappresentazioni grafiche qui sotto per avere un’idea):
• lispy (Tr(’a’,[])) = "a"
• lispy (Tr(’a’, [Tr(’b’,[])])) = "(a b)"
• lispy (Tr(’a’, [Tr(’b’, [Tr(’c’,[])])])) = "(a (b c))"
• lispy (Tr(’a’, [Tr(’b’,[]); Tr(’c’,[])])) = "(a b c)"
• lispy (Tr(’a’, [Tr(’f’, [Tr(’g’,[])]); Tr(’c’,[]); Tr(’b’, [Tr(’d’,[]); Tr(’e’,[])])]))
= "(a (f g) c (b d e))".
 *)

type 'a ntree = Tr of 'a * 'a ntree list;;

let rec lispy albero =
  let rec aux albero =
  match albero with
  Tr(x, []) -> String.make 1 x
  | Tr(x, figli) -> "("^(String.make 1 x)^" "^from_stringlist_to_string(List.map (lispy) figli)^")"
  in aux albero;;
Printf.printf "\n=======================\n";;
Printf.printf "Lispy: \n";;
let x = lispy (Tr('a', []));;
Printf.printf "%s\n" x;;

let x = lispy (Tr('a', [Tr('b', [])]));;
Printf.printf "%s\n" x;;

let x = lispy (Tr('a', [Tr('b', [Tr('c', [])])]));;
Printf.printf "%s\n" x;;

let x = lispy (Tr('a', [Tr('b', []);Tr('c', [])]));;
Printf.printf "%s\n" x;;

(*  (5 punti) Definire una funzione degree : ’a graph -> ’a -> int tale che degree g n restituisca il grado
del nodo n nel grafo g, cio`e il numero di nodi che sono immedatamente accessibili da n (ignorando l’orientamento).
Per esempio, degree graph 1 = 3 e degree graph 6 = 4. *)
let rec degree grafo n= List.length (List.filter (function (x,y)-> x=n || y=n) grafo);;

let graph = [ (1,2); (1,3); (1,4); (2,6); (3,5); (4,6); (5,4); (6,5); (6,7)];;
Printf.printf "\n=======================\n";;
Printf.printf "Degree: \n";;
let x = degree graph 1;;
Printf.printf "%d\n" x;;

let x = degree graph 6;;
Printf.printf "%d\n" x;;

(* (5 punti) Definire una funzione nodes : ’a graph -> ’a list tale che nodes g restituisca la lista nei nodi
del grafo g, senza ripetizioni. Per esempio, nodes graph = [7; 5; 6; 4; 3; 1; 2]. E preferibile (per `
ottenere il punteggio massimo) utilizzare una funzione ausiliaria che sia ricorsiva di coda/iterativa, cio`e tale
che dopo ogni chiamata ricorsiva le uniche operazioni possibili siano let ... in e if ... then ... else. *)

let nodes grafo =
  let rec aux risultato grafo = 
    match grafo with
    [] -> risultato
    | (x,y)::rest -> if List.mem x risultato then 
                        if List.mem y risultato then 
                          aux risultato (List.filter (function (nodo1, nodo2)-> not(nodo1=x && nodo2=y)) grafo) 
                        else
                          aux (y::risultato) grafo
                        else
                          aux (x::risultato) grafo
                        in aux [] grafo;;
Printf.printf "\n=======================\n";;
Printf.printf "Nodes: ";;
let x = nodes graph;;
print_list_int x;;

(* (5 punti) Definire una funzione nodes with degree : ’a graph -> (’a * int) list tale che nodes with degree
g restituisca la lista dei nodi del grafo g, ciascun nodo con il suo grado. Per esempio, nodes with degree
graph = [(7, 1); (5, 3); (6, 4); (4, 3); (3, 2); (1, 3); (2, 2)]. Si possono utilizzare le funzioni degree e nodes dei punti precedenti anche se non sono state definite.
Suggerimento: Si pu`o utilizzare la funzione List.combine : ’a list -> ’b list -> (’a * ’b) list che
trasforma una coppia di liste della stessa lunghezza in una lista di coppie. Per esempio, combine [a1;...;an]
[b1;...;bn] = [(a1,b1);...;(an,bn)].
*)

let nodes_with_degree grafo = let nodiGrafo = nodes grafo in
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
Printf.printf "\n=======================\n";;
Printf.printf "nodes_with_degree: \n";;
let x = nodes_with_degree graph;;
print_pairs_list x;;

(* (5 punti) Definire una funzione ordered nodes : ’a graph -> ’a list tale che ordered nodes g restituisca la lista dei nodi del grafo g ordinata in modo decrescente secondo il grado. Per esempio, ordered nodes
graph = [6; 5; 4; 1; 3; 2; 7]. Si pu`o utilizzare la funzione nodes with degree del punto precedente
anche se non `e stata definita.
Suggerimento: Si possono utilizzare le funzioni List.sort e compare viste a lezione. *)

let compare (_, grado1) (_, grado2) =
  if grado1 > grado2 then -1
  else if grado1=grado2 then 0
  else 1;;
let rec ordered_nodes grafo = List.map (fst) (List.sort (compare) (nodes_with_degree grafo));;
Printf.printf "\n=======================\n";;
Printf.printf "Ordered_nodes: \n";;
let x = ordered_nodes graph;;
print_list_int x;;
Printf.printf "\n";;
(* (10 punti) Definire una funzione reverse to list : string -> char list tale che reverse to list s
restituisca la lista dei caratteri nella stringa s letta in ordine inverso. Per esempio, reverse to list "ciao"
= [’o’; ’a’; ’i’; ’c’]. Si pu`o utilizzare la funzione String.length : string -> int che restituisce la
lunghezza di una stringa. *)

let rec reverse_to_list stringa = 
  let rec aux i risultato=
    if i > (String.length stringa)-1 then risultato
    else
      aux (i+1) (stringa.[i]::risultato)
    in aux 0 [];;

let x = reverse_to_list "ciao";;

let stampa_char_list lst =
  List.iter (fun c -> print_char c) lst;
  print_newline ();;
Printf.printf "\n=======================\n";;
Printf.printf "reverse_to_list: \n";;
stampa_char_list x;;

(*(3 punti) Definire una funzione reverse : string -> string tale che reverse s restituisca la stringa s
in ordine inverso. Per esempio, reverse "ciao" = "oaic". Anche se non sono state definite, si possono
utilizzare:
• la funzione reverse to list del punto precedente;
• la funzione implode : char list -> string tale che implode lst restituisce la stringa composta dai
caratteri della lista di caratteri lst, nello stesso ordine. Per esempio, implode [’c’; ’i’; ’a’; ’o’]
= "ciao".*)
let rec implode lista = 
  match lista with
  [] -> ""
  | x::rest -> (String.make 1 x)^(implode rest);;

let x = implode ['c';'i';'a';'o'];;
Printf.printf "\n=======================\n";;
Printf.printf "implode: \n";;
Printf.printf "%s" x;;
let reverse stringa = implode ( reverse_to_list stringa);;
let x = reverse "ciao";;
Printf.printf "\n=======================\n";;
Printf.printf "reverse: \n";;
Printf.printf "%s" x;;

(*(2 punti) Definire una funzione palindrome : string -> bool tale che palindrome s restituisca true se la
stinga s `e palindroma, false altrimenti. Una stringa `e palindroma se, letta al contrario, rimane invariata. Per
esempio, palindorme "samas" = true mentre palindrome "saman" = false. Si pu`o utilizzare la funzione
reverse del punto precedente anche se non `e stata definita*)

let palindrome s = s=reverse (s);;
let x = palindrome "saman";;
Printf.printf "\n=======================\n";;
Printf.printf "palindrome: \n";;
Printf.printf "%b" x;;

(* Definire una funzione sumOddSquares : int list -> int tale che sumOddSquares lst calcoli la somma dei
quadrati degli interi dispari nella lista lst. Per esempio, sumOddSquares [1;2;3;4;5;6] = 35. 
Si possono utilizzare le funzioni List.map, List.filter, List.fold left, e List.fold right viste a lezione.
E preferibile (per ottenere il punteggio massimo) `
• usare funzioni anonime o locali invece di definire pi`u funzioni,
• evitare di usare il pattern matching.
*)

let sumOddSquares lista= List.fold_left (+) 0 (List.map (function x-> x*x) (List.filter (function x-> not(x mod 2 = 0)) lista));;
let x = sumOddSquares [1;2;3;4;5;6];;
Printf.printf "\n=======================\n";;
Printf.printf "sumOddSquares: \n";;
Printf.printf "%d" x;;

(* Determinare se un carattere è numerico *)
let numeric x = x>='0' && x<='9';;

(* Posizione del primo carattere non numerico di s a partire dalla posizione i *)
let rec loop s i =
  if not(numeric (s.[i])) then i else loop s (i+1);;

let primo_non_numerico s = loop s 0;;

(* SottoStringa di s che va dalla posizione j alla posizione k *)

let substring s j k = String.sub s j ((k-j)+1);;

let split_string s = (int_of_string (substring s 0 ((primo_non_numerico s)-1)), s.[(primo_non_numerico s)], int_of_string (substring s ((primo_non_numerico s)+1) ((String.length s)-1)));;

exception BadOperation
let evaluate s = 
  let (num1, op, num2) = split_string s in
  match op with
  '+' -> num1 + num2
  | '-' -> num1 - num2
  | '/' -> num1/num2
  | '*' -> num1*num2
  | _ -> raise BadOperation;;
  
let x = evaluate "2+3";;
Printf.printf "\n=======================\n";;
Printf.printf "evaluate: \n";;
Printf.printf "%d" x;;

(* dato un dizionario, rappresentato dalla lista associativa assoc_list, e una chiave k, riportare il valore associato a k in assoc_list, se esiste, un errore altrimenti*)
exception NotFound
let get k assoc_list = try
                      (snd (List.find (function (x,y) -> x=k) assoc_list))
                      with _ -> raise NotFound;;

let rec assoc k = function 
    [] -> raise NotFound
    | (k1, v)::rest -> if k=k1 then v else assoc k rest;;

(* fulltree : int -> int tree. La funzione, applicata a un intero n, riporta un albero binario completo di altezza n, con i nodi etichettati da interi
come segue: la radice è etichettata da 1, i figli di un nodo etichettato da k
sono etichettati da 2k e 2k + 1. *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;
let rec fulltree n =
  let rec aux k n= 
  if n = 0 then Empty
  else
    Tr(k, aux(2*k) (n-1), aux(2*k+1) (n-1))
  in aux 1 n;;

let rec print_tree ?(prefix="") ?(is_left=true) print_elem tree =
  match tree with
  | Empty ->
      Printf.printf "%s%sEmpty\n" prefix (if is_left then "├── " else "└── ")
  | Tr (v, left, right) ->
      Printf.printf "%s%s" prefix (if is_left then "├── " else "└── ");
      print_elem v;
      print_newline ();
      let new_prefix = prefix ^ (if is_left then "│   " else "    ") in
      print_tree ~prefix:new_prefix ~is_left:true print_elem left;
      print_tree ~prefix:new_prefix ~is_left:false print_elem right;;

Printf.printf "\n=======================\n";;
Printf.printf "fulltree: \n";;

let () =
  let tree = fulltree 4
  in
  print_tree (fun x -> Printf.printf "%d" x) tree;;

(* Una lista di interi non negativi L può determinare un sottoalbero di un
albero n-ario T: quello che si ottiene, a partire dalla radice, scendendo,
per ogni elemento n di L, al sottoalbero in posizione n nella lista dei
sottoalberi (se esiste) – si ricordi che la posizione degli elementi in una
lista si conta a partire da 0. Se la lista è più lunga del ramo cui essa
conduce, oppure se a qualche livello non esiste un numero sufficiente di
sottoalberi, allora L non determina alcun sottoalbero di T.
Ad esempio, se T è l’albero sotto rappresentato 
allora la lista [2;0] determina il sottoalbero che ha radice 11, la lista
[2;2;1] quello che ha radice 18, [2;2;1;0] il sottoalbero costituito soltanto
dal nodo 19. Le liste [1;2] e [0;1;1] non determinano alcun sottoalbero
di T.
Scrivere una funzione listaGuida: ’a list -> ’a ntree -> ’a, che,
data una lista L di interi e un albero n-ario T, riporti la radice del sottoalbero di T determinato da L, se L determina un sottoalbero di T, un
errore altrimenti.
*)
type 'a ntree = Tr of 'a * 'a ntree list;;
exception Errore
let rec listaGuida lista (Tr(x, figli)) =
  match lista with
  [] -> x
  | y::rest -> let rec scorrifigli figli indice= 
                match figli with
                [] -> raise Errore
                | z::rest -> if indice=y then listaGuida (List.tl lista) z else scorrifigli rest (indice+1)
in scorrifigli figli 0;;

  let rec listaGuida lst albero = 
    match (lst, albero) with
    ([], Tr(x, _)) -> x
    | (n::rest, Tr(x, lst)) -> listaGuida rest (List.nth lst n);;

 let esempio =
  Tr (1, [
    Tr (2, []);
    Tr (3, [
      Tr (4, []);
      Tr (5, [])
    ])
  ]);;

  let lista = [1;0];;

  let x = listaGuida lista esempio;;

  Printf.printf "Lista guida risultato: %d\n" x;;
  
(* Ricerca di un nodo che soddisfa un predicato *)
exception NonEsiste
let rec nodo_predicato start p grafo = 
  let rec aux visitati pendenti = 
    match pendenti with
    [] -> raise NonEsiste
    | x::rest -> if p x then x else 
                  if List.mem x visitati then aux visitati rest else aux (x::visitati) (rest@(successori x grafo))
    in aux [] [start];;

(* (Dal compito d’esame di settembre 2010). Scrivere una funzione
cammino_di_primi: int graph -> int-> int -> int list
che, applicata a un grafo (orientato) di interi g e interi start e goal riporti,
se esiste, un cammino in g da start a goal costituito soltanto da numeri
primi.*)

let cammino_di_primi grafo start goal = 
  let rec aux visitati pendenti = 
    match pendenti with
    [] -> raise NonEsiste
    | x::rest -> if x=goal && is_primo x then (x::visitati) else
                  if not (is_primo x) then raise NonEsiste else
                    if List.mem x visitati then aux visitati rest else 
                      try
                      aux (x::visitati) ((successori x grafo)@rest)
                      with NonEsiste -> aux (visitati) rest
                    in List.rev (aux [] [start]);;

let grafo = [(2,3); (3,4); (2,11); (11,13)];;

let x = cammino_di_primi grafo 2 13;;
Printf.printf "\n=======================\n";;
Printf.printf "cammino_di_primi: \n";;
print_list_int x;;

(* nth: int -> ’a list -> ’a, tale che nth n lst = elemento di lst
in posizione n, dove il primo elemento della lista è in posizione 0. La
funzione solleverà un’eccezione se n è negativo o se la lista non contiene abbastanza elementi (Notare che il modulo List contiene una
funzione con questo nome, ma qui si chiede di definirla per esercizio). *)

let rec nth n lista = 
  if n<0 then raise BadOperation else
  match lista with
  [] -> raise BadOperation
  | x::rest -> if n=0 then x else nth (n-1) rest;;

let x = nth 2 [0;1;2];;

Printf.printf "\n=======================\n";;
Printf.printf "nth: %d\n" x;;

(* . (Esame di luglio 2011)
(a) Scrivere una funzione find: ’a -> ’a list -> ’a list * ’a list, che,
applicata a un elemento x e a una lista L, spezzi L in due parti: la prima
contiene tutti gli elementi che vanno dall’inizio della lista fino alla prima
occorrenza di x esclusa; la seconda contiene tutti gli elementi che seguono
la prima occorrenza di x. La funzione solleverà un’eccezione se L non contiene x. Ad esempio, find 3 [1;2;3;4;5;6;3] = ([1;2],[4;5;6;3]).
(b) Utilizzando la funzione find definita al punto precedente, definire una funzione spezza: ’a -> ’a list -> ’a list * ’a list, che, applicata a
un elemento x e una lista L riporti una coppia di liste (L1, L2), dove L1
contiene tutti gli elementi di L che vanno dalla prima alla seconda occorrenza di x, estremi esclusi, e L2 contiene tutti gli elementi di L che
seguono la seconda occorrenza di x. La funzione solleverà un’eccezione
se L non contiene almeno due occorrenze di x. Ad esempio, spezza 3
[1;2;3;4;5;6;3;7;8;9;3;10] = ([4;5;6],[7;8;9;3;10]). *)
exception NoElemento
let find x lista =
  let rec aux prima lista = 
    match lista with
    [] -> raise NoElemento
    | y::rest -> if x=y then (List.rev (prima), rest) else aux (y::prima) rest
  in aux [] lista;;

let (prima,dopo) = find 3 [1;2;3;4;5;6;3];;

Printf.printf "\n=======================\n";;
Printf.printf "Find: %d\n" x;;
Printf.printf "Prima: \n";;
print_list_int prima;;
Printf.printf "Dopo: \n";;
print_list_int dopo;;

let spezza x lista= let (_, seconda) = find x lista in
  find x seconda;;

let (prima,dopo) = spezza 3 [1;2;3;4;5;6;3;7;8;9;3;10];;

Printf.printf "\n=======================\n";;
Printf.printf "Spezza: %d\n" x;;
Printf.printf "Prima: \n";;
print_list_int prima;;
Printf.printf "Dopo: \n";;
print_list_int dopo;;

(* balpreorder e balinorder, entrambe di tipo ’a list -> ’a tree. Data
una lista lst, costruiscono un albero bilanciato con nodi etichettati da
elementi di lst, in modo tale che
preorder (balpreorder lst) = lst
inorder (balinorder lst) = lst *)
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;
let rec balpreorder lista =
  match lista with
  [] -> Empty
  | x::rest -> Tr(x, balpreorder (List.take ((List.length rest)/2) rest), balpreorder (List.drop ((List.length rest)/2) rest));;

Printf.printf "\n=======================\n";;
Printf.printf "Balpreorder: \n";;

let rec print_tree ?(prefix="") ?(is_left=true) print_elem tree =
  match tree with
  | Empty ->
      Printf.printf "%s%sEmpty\n" prefix (if is_left then "├── " else "└── ")
  | Tr (v, left, right) ->
      Printf.printf "%s%s" prefix (if is_left then "├── " else "└── ");
      print_elem v;
      print_newline ();
      let new_prefix = prefix ^ (if is_left then "│   " else "    ") in
      print_tree ~prefix:new_prefix ~is_left:true print_elem left;
      print_tree ~prefix:new_prefix ~is_left:false print_elem right;;

let () =
  let tree =  balpreorder [1;2;3;4;5]
  in
  print_tree (fun x -> Printf.printf "%d" x) tree;;

let rec preorder albero = 
  match albero with
  Empty -> []
  | Tr(x, l, r) -> [x]@preorder l@preorder r;;

let x = preorder (balpreorder [1;2;3;4;5]);;
Printf.printf "\n=======================\n";;
Printf.printf "Preorder: \n";;
print_list_int x;;

let rec balinorder list =
  match list with
  [] -> Empty
  | x::rest -> Tr((List.nth list ((List.length list)/2)), balinorder (List.take ((List.length list)/2) list), balinorder(List.drop (((List.length list)/2)+1) list));;

Printf.printf "\n=======================\n";;
Printf.printf "Balinorder: \n";;

let rec print_tree ?(prefix="") ?(is_left=true) print_elem tree =
  match tree with
  | Empty ->
      Printf.printf "%s%sEmpty\n" prefix (if is_left then "├── " else "└── ")
  | Tr (v, left, right) ->
      Printf.printf "%s%s" prefix (if is_left then "├── " else "└── ");
      print_elem v;
      print_newline ();
      let new_prefix = prefix ^ (if is_left then "│   " else "    ") in
      print_tree ~prefix:new_prefix ~is_left:true print_elem left;
      print_tree ~prefix:new_prefix ~is_left:false print_elem right;;

let () =
  let tree =  balinorder [1;2;3;4;5]
  in
  print_tree (fun x -> Printf.printf "%d" x) tree;;

(*  Definire una funzione tutte_foglie_costi: int ntree -> (int * int)
list che, applicata a un albero n-ario T etichettato da interi, riporti una
lista di coppie, ciascuna delle quali ha la forma (f,n), dove f è l’etichetta
di una foglia in T e n il costo di tale foglia (dove il costo di una foglia
è definito come nell’esercizio precedente). Anche in questo caso, l’albero
può anche avere diversi nodi con la stessa etichetta. *)
type 'a ntree = Tr of 'a * 'a ntree list;;
let rec tutte_foglie_costi albero =
  let rec aux costo albero =
    match albero with
    Tr(x, []) -> [(x, costo+x)]
    | Tr(x, figli) -> List.flatten (List.map (aux (costo+x)) figli)
  in aux 0 albero;;

  let albero =
  Tr(5, [
    Tr(3, [
      Tr(4, [])
    ]);
    Tr(2, []);
    Tr(1, [
      Tr(6, [
        Tr(7, [])
      ]);
      Tr(8, [])
    ])
  ]);;

let x = tutte_foglie_costi albero;;
let print_list_pair (lst : (int * int) list) : unit =
  let flatten lst =
    List.fold_right (fun (a, b) acc -> a :: b :: acc) lst []
  in
  print_list_int (flatten lst);;

print_list_pair x;;

(* (Dal compito d’esame di febbraio 2010). Scrivere un programma con
una funzione cammino_con_nodi: ’a graph -> ’a -> ’a list -> ’a
list che, dato un grafo orientato G, un nodo N di G e una lista L senza ripetizioni, restituisca, se esiste, un cammino senza cicli che, partendo
da N, contenga tutti i nodi di L (in qualsiasi ordine) ed eventualmente
anche altri nodi. Se un tale cammino non esiste, il programma solleverà
un’eccezione.
Ad esempio, se G è il grafo rappresentato da [(1, 2); (1, 3); (1, 4);
(2, 6); (3, 5); (4, 6); (6, 5); (6, 7); (5, 4)] e L è la lista [2;
5], la ricerca a partire dal nodo 1 restituirà il cammino [1; 2; 6; 5].
. Se la lista L è [2; 6; 3], la ricerca a partire dal nodo 1 fallirà perché
non esistono cammini in G che a partire da 1 tocchino tutti i nodi di L.*)
let cammino_con_nodi grafo start lista =
  let rec aux visitati pendenti lista =
    match pendenti with
    [] -> raise NonEsiste
    | x::rest -> if List.mem x visitati then aux visitati rest lista
                else
                  if lista=[x] then (x::visitati)
                  else
                  try
                  if List.mem x lista then aux (x::visitati) (successori x grafo) (List.filter ((<>) x) lista) 
                  else aux (x::visitati) (successori x grafo) lista
                  with NonEsiste -> aux visitati rest lista
                in List.rev (aux [] [start] lista);;

let grafo = [(1, 2); (1, 3); (1, 4);
(2, 6); (3, 5); (4, 6); (6, 5); (6, 7); (5, 4)];;

let x = cammino_con_nodi grafo 1 [2;5];;

print_list_int x;;

(* (7 punti) Definire una funzione from stringlist to string : string list -> string tale che
from stringlist to string lst restituisca la stringa che si ottiene concatenando le stringhe nella lista lst
inserendo uno spazio tra due stringhe successive di lst (ma senza alcuno spazio prima della prima stirnga
e dopo l’ultima stringa). In particolare, nessuno spazio deve essere inserito se lst contiene nessuno o un
elemento. Per esempio,
• from stringlist to string [] = ""
• from stringlist to string ["ci"] = "ci"
• from stringlist to string ["ci";"ao"] = "ci ao".
• from stringlist to string ["ci";"a";"o"] = "ci a o".
Suggerimento: Utilizzare l’operatore infisso di concatenazione di stringhe ^ *)

let rec from_stringlist_to_string lista = 
  match lista with
  [] -> ""
  | [x] -> x
  | x::rest -> x^" "^from_stringlist_to_string rest;;

Printf.printf "\n=======================\n";;
Printf.printf "from_stringlist_to_string: \n";;
let x = from_stringlist_to_string [];;
Printf.printf "%s\n" x;;
let x = from_stringlist_to_string ["ci"];;
Printf.printf "%s\n" x;;
let x = from_stringlist_to_string ["ci";"ao"];;
Printf.printf "%s\n" x;;
let x = from_stringlist_to_string ["ci";"a";"o"];;
Printf.printf "%s\n" x;;

(* . (8 punti) Si consideri la struttura dati per gli alberi n-ari con nodi di tipo ’a definita a lezione come
type ’a ntree = Tr of ’a * ’a ntree list
Definire una funzione lispy : char ntree -> string tale che lispy t restituisca la stringa che rappresenta
l’albero t di caratteri nella maniera seguente:
(a) se t `e un nodo ’c’ senza figli, allora lispy t `e la stringa "c";
(b) se t un nodo ’c’ con figli t1, . . . , tn, allora lispy t `e la stringa "(c s1 ... sn)" dove s1, . . . , sn
sono le stringhe ottenute applicando ricorsivamente lispy a t1, . . . , tn, rispettivamente.
Per esempio (si vedano anche le rappresentazioni grafiche qui sotto per avere un’idea):
• lispy (Tr(’a’,[])) = "a"
• lispy (Tr(’a’, [Tr(’b’,[])])) = "(a b)"
• lispy (Tr(’a’, [Tr(’b’, [Tr(’c’,[])])])) = "(a (b c))"
• lispy (Tr(’a’, [Tr(’b’,[]); Tr(’c’,[])])) = "(a b c)"
• lispy (Tr(’a’, [Tr(’f’, [Tr(’g’,[])]); Tr(’c’,[]); Tr(’b’, [Tr(’d’,[]); Tr(’e’,[])])]))
= "(a (f g) c (b d e))".
*)
type 'a ntree = Tr of 'a * 'a ntree list;;
let rec lispy albero =
  match albero with
  Tr(x, []) -> String.make 1 x
  | Tr(x, figli) -> "("^(String.make 1 x)^" "^from_stringlist_to_string(List.map (lispy) figli)^")";;

Printf.printf "\n=======================\n";;
Printf.printf "Lispy: \n";;
let x = lispy (Tr('a', []));;
Printf.printf "%s\n" x;;

let x = lispy (Tr('a', [Tr('b', [])]));;
Printf.printf "%s\n" x;;

let x = lispy (Tr('a', [Tr('b', [Tr('c', [])])]));;
Printf.printf "%s\n" x;;

let x = lispy (Tr('a', [Tr('b', []);Tr('c', [])]));;
Printf.printf "%s\n" x;;

let graph = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6); (5,4); (6,5); (6,7)];;

(* (5 punti) Definire una funzione degree : ’a graph -> ’a -> int tale che degree g n restituisca il grado
del nodo n nel grafo g, cio`e il numero di nodi che sono immedatamente accessibili da n (ignorando l’orientamento).
Per esempio, degree graph 1 = 3 e degree graph 6 = 4 *)

let degree grafo nodo= List.length (List.filter (function (x,y)-> x=nodo || y=nodo) grafo);;

Printf.printf "\n=======================\n";;
Printf.printf "Degree: \n";;
let x = degree graph 1;;
Printf.printf "%d\n" x;;

let x = degree graph 6;;
Printf.printf "%d\n" x;;

(* he sono immedatamente accessibili da n (ignorando l’orientamento).
Per esempio, degree graph 1 = 3 e degree graph 6 = 4.
2. (5 punti) Definire una funzione nodes : ’a graph -> ’a list tale che nodes g restituisca la lista nei nodi
del grafo g, senza ripetizioni. Per esempio, nodes graph = [7; 5; 6; 4; 3; 1; 2]. E preferibile (per `
ottenere il punteggio massimo) utilizzare una funzione ausiliaria che sia ricorsiva di coda/iterativa, cio`e tale
che dopo ogni chiamata ricorsiva le uniche operazioni possibili siano let ... in e if ... then ... else. *)

let nodes grafo = 
  let rec aux nodi grafo =
    match grafo with
    [] -> nodi
    | (x,y)::rest -> if List.mem x nodi then
                        if List.mem y nodi then aux nodi rest
                        else
                          aux (y::nodi) grafo
                        else
                          aux (x::nodi) grafo
                        in aux [] grafo;;

Printf.printf "\n=======================\n";;
Printf.printf "Nodes: ";;
let x = nodes graph;;
print_list_int x;;

(* (5 punti) Definire una funzione nodes with degree : ’a graph -> (’a * int) list tale che nodes with degree
g restituisca la lista dei nodi del grafo g, ciascun nodo con il suo grado. Per esempio, nodes with degree
graph = [(7, 1); (5, 3); (6, 4); (4, 3); (3, 2); (1, 3); (2, 2)]. Si possono utilizzare le funzioni degree e nodes dei punti precedenti anche se non sono state definite.*)

let nodes_with_degree grafo = let nodiGrafo = nodes grafo in
                          List.combine nodiGrafo (List.map (degree grafo) nodiGrafo);;

Printf.printf "\n=======================\n";;
Printf.printf "nodes_with_degree: \n";;
let x = nodes_with_degree graph;;
print_pairs_list x;;

(* (5 punti) Definire una funzione ordered nodes : ’a graph -> ’a list tale che ordered nodes g restituisca la lista dei nodi del grafo g ordinata in modo decrescente secondo il grado. Per esempio, ordered nodes
graph = [6; 5; 4; 1; 3; 2; 7]. Si pu`o utilizzare la funzione nodes with degree del punto precedente
anche se non `e stata definita. *)

let compare (_, grado1) (_, grado2) = 
  if grado1>grado2 then -1
  else if grado1=grado2 then 0
  else 1;;

let ordered_nodes grafo = List.map (fst) (List.sort (compare) (nodes_with_degree grafo));;

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

let rec preorder albero =
  match albero with
  Empty -> []
  | Tr(x, l, r) -> [x]@preorder l@preorder r;;

let rec postorder albero =
  match albero with
  Empty -> []
  | Tr(x, l, r) -> postorder l@postorder r@[x];;

let rec inorder albero = 
  match albero with
  Empty -> []
  | Tr(x, l, r) -> inorder l@[x]@inorder r;;

type 'a ntree = Tr of 'a * 'a ntree list;;

let rec postorder (Tr(x, figli)) = List.flatten (List.map (postorder) figli) @ [x];;

let rec preorder (Tr(x, figli)) = [x]::List.flatten (List.map (preorder) figli) ;;

let rec inorder albero = 
  match albero with
  Tr(x, []) -> [x]
  | Tr(x, y::rest) -> (inorder y)@[x]@(List.flatten (List.map (inorder) rest));;  

(* Visita in profondità *)

let depth_first_search grafo start = 
  let rec aux visitati pendenti =
    match pendenti with
    [] -> visitati
    | x::rest -> if List.mem x visitati then aux visitati rest
    else
      aux (x::visitati) ((successori x grafo)@rest)
    in aux [] [start];;

let breadth_first_search grafo start = 
  let rec aux visitati pendenti =
    match pendenti with
    [] -> visitati
    | x::rest -> if List.mem x visitati then aux visitati rest
    else
      aux (x::visitati) (rest@(successori x grafo))
    in aux [] [start];;

type 'a option = None | Some of 'a;;

let succ_opt n = 
  if n=None then None else Some (n+1)