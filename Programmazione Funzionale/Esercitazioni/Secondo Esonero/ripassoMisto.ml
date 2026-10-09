(* (Dal compito d’esame di febbraio 2010). Si definisca un tipo di dati per la
rappresentazione di alberi binari e scrivere un programma con una funzione
path_coprente: ’a tree -> ’a list -> ’a list che, dato un albero A e
una lista di elementi dello stesso tipo dei nodi di A, restituisca, se esiste, un
ramo dell’albero dalla radice a una foglia che contenga tutti i nodi di L (in
qualsiasi ordine) ed eventualmente anche altri nodi. Se un tale cammino non
esiste, il programma solleverà un’eccezione. Si assuma che la lista L sia senza
ripetizioni.
Ad esempio, se l’albero è quello rappresentato per l’esercizio 6 e la lista è [3;6],
la funzione può restituire il ramo [0;6;6;3] oppure 0;6;4;3]. Se la lista è
[10], la funzione può restituire il ramo [0;10;2] oppure 0;10;5]. *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

exception CamminoNonEsistente
let path_coprente albero lista =
  let rec aux visitati albero lista = 
    match albero with
    Empty -> if lista=[] then visitati else raise CamminoNonEsistente
    | Tr(x, Empty, Empty) -> if lista=[] || lista=[x] then List.rev (x::visitati) else raise CamminoNonEsistente
    | Tr(x, l, r) -> try
                       aux (x::visitati) l (List.filter ((<>) x) lista)
                    with CamminoNonEsistente ->  aux (x::visitati) r (List.filter ((<>) x) lista)
                                              in aux [] albero lista;;

Printf.printf "\nInizio Alberi Binari: \n";;
Printf.printf "\n==========================\n";;
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

  (* Scrivere un predicato stessa_struttura: ’a tree -> ’a tree -> bool
che determini se due alberi binari hanno la stessa struttura (cioè se essi sono uguali quando si ignorano le rispettive etichette; ad esempio i tre alberi
rappresentati per l’esercizio successivo hanno tutti la stessa struttura). *)

Printf.printf "\n==========================\n";;
Printf.printf "Test per stessa_struttura: \n";;

let rec stessa_struttura albero1 albero2 =
  match (albero1,albero2) with
  (Empty, Empty) -> true
  | (_,Empty) | (Empty, _) -> false
  | (Tr(x1, l1, r1), Tr(x2, l2, r2)) -> (stessa_struttura l1 l2) && (stessa_struttura r1 r2);;

  (* Albero 1: struttura Tr(1, Tr(2, Empty, Empty), Tr(3, Empty, Empty)) *)
let a1 = Tr(1, Tr(2, Empty, Empty), Tr(3, Empty, Empty))

(* Albero 2: stesso schema di a1 ma con valori diversi *)
let a2 = Tr('a', Tr('b', Empty, Empty), Tr('c', Empty, Empty))

(* Albero 3: struttura diversa *)
let a3 = Tr(1, Tr(2, Tr(3, Empty, Empty), Empty), Empty)

(* Albero 4: stesso schema di a1 con valori anche uguali *)
let a4 = Tr(10, Tr(20, Empty, Empty), Tr(30, Empty, Empty))

(* Test esecuzioni *)
let () =
  Printf.printf "a1 e a2 stessa struttura: %b\n" (stessa_struttura a1 a2); (* true *)
  Printf.printf "a1 e a3 stessa struttura: %b\n" (stessa_struttura a1 a3); (* false *)
  Printf.printf "a1 e a4 stessa struttura: %b\n" (stessa_struttura a1 a4);; (* true *)

(* Una funzione f è un mapping da un albero binario t1 a un albero t2 se
l’applicazione di f alle etichette di t1 trasforma t1 in t2. In particolare,
perché possa esistere un mapping da t1 a t2, i due alberi devono avere la
stessa struttura, ma questa non è una condizione sufficiente. Esiste un mapping 
dall’albero A all’albero B (la funzione rappresentata dalla lista [(1,10);(2,20)], 
ma non dall’albero A all’albero C (la “trasformazione” [(1,10);(2,20);(2,30)] non è una funzione). Dall’albero C
esiste un mapping all’albero A ([(10,1);(20,2);(30,2)]) e un mapping
all’albero B ([(10,10);(20,20);(30,20)]).
Scrivere un predicato esiste_mapping: ’a tree -> ’a tree -> bool
che, applicato a due alberi binari t1 e t2 determini se esiste un mapping da
t1 a t2. La funzione non deve mai sollevare eccezioni, ma deve riportare
sempre un booleano.
(Suggerimento: costruire prima la lista di coppie che dovrebbe trasformare il primo albero nel secondo, e poi verificare se essa rappresenta una
funzione).
*)

let rec lista_mapping albero1 albero2 =
    match (albero1, albero2) with
    (Empty, Empty) -> []
    | (Tr(x1, l1, r1), Tr(x2, l2, r2))-> (x1,x2)::(lista_mapping l1 l2)@(lista_mapping r1 r2)
    | _ -> [] ;;

let rec is_assoc = function
  [] -> true
| (x,y)::rest -> try y = List.assoc x rest && (is_assoc rest)
              with _ -> true;;


let rec esiste_mapping albero1 albero2 = 
  if not(stessa_struttura albero1 albero2) then false else
    let mapping = lista_mapping albero1 albero2 in
     try is_assoc mapping
  with _ -> false;;
      

(* (Dal compito d’esame di giugno 2011) Definire una funzione
path: (’a -> bool) -> ’a tree -> ’a list,
che, applicata a un predicato p: ’a -> bool e a un albero t: ’a tree, riporti,
se esiste, un cammino dalla radice a una foglia di t che non contenga alcun nodo
che soddisfa p. La funzione solleverà un’eccezione se un tale cammino non esiste. *)

exception IlNodoSoddisfaP
let path p albero =
  let rec aux visitati albero = 
  match albero with
  Empty -> []
  | Tr(x, Empty, Empty) -> if p x then raise IlNodoSoddisfaP else List.rev (x::visitati)
  | Tr(x, l, r) -> if p x then raise IlNodoSoddisfaP else
                    try
                      aux (x::visitati) l
                    with IlNodoSoddisfaP -> aux (x::visitati) r
                  in aux [] albero;;

let albero_di_test =
  Tr (1,
      Tr (2,
          Tr (4, Empty, Empty),
          Tr (5, Empty, Empty)),
      Tr (3,
          Tr (6, Empty, Empty),
          Tr (7, Empty, Empty)));;

let is_even x = x mod 2 = 0;;
let result = path (fun x -> x mod 2 = 0) albero_di_test;;
Printf.printf "\n==========================\n";;
Printf.printf "Test per path: \n";;

try
  let res = path (fun x -> x mod 2 = 0) albero_di_test in
  Printf.printf "Cammino trovato: [%s]\n"
    (String.concat "; " (List.map string_of_int res))
with
  Not_found -> print_endline "Nessun cammino valido trovato.";;


(* (Dal compito d’esame di luglio 2009).
(a) Siano date la seguenti dichiarazioni di tipo*)
type col = Rosso | Giallo | Verde | Blu;;
type 'a col_assoc = (col * 'a list) list;;
(*Una lista di tipo ’a col_assoc rappresenta un’associazione di colori a
liste di elementi di tipo ’a: ogni colore è associato alla lista di elementi di
quel colore.*)
(*Scrivere una funzione colore: ’a -> ’a col_assoc -> col, che, dato
un valore x di tipo ’a e una lista che rappresenta un’associazione di colori,
riporti il colore di x, se tale colore è definito, sollevi un’eccezione altrimenti.
Ad esempio, se lst = [(Rosso,[1;2;4;7;10]); (Giallo,[3;8;11]);
(Verde,[0;5;6;13]); (Blu,[9;12;14;15])], il valore di colore 6 lst
è Verde, mentre colore 100 lst solleva un’eccezione.*)
exception NoColore
let rec colore x lista =
  match lista with
  [] -> raise NoColore
  | (col, numeri)::rest -> if List.mem x numeri then col else colore x rest;;
(*(b) Un ramo di un albero è detto a colori alterni se, nella sequenza di nodi
[x1;...;xn] che lo rappresenta, due valori adiacenti hanno sempre colore
diverso (secondo una data associazione di colori).
Dichiarare un tipo di dati per rappresentare alberi binari e scrivere una
funzione path_to: ’a -> ’a col_assoc -> ’a tree -> ’a list, che,
dato un valore x: ’a, un’associazione di colori e un albero binario, riporti
– se esiste – un ramo a colori alterni, dalla radice dell’albero a una foglia
etichettata da x. Se un tale ramo non esiste, solleverà un’eccezione.
Ad esempio, se lst è l’associazione di colori data al punto 1 e t è l’albero rappresentato qui sotto, il valore di path_to 8 colori t è la lista
[1;5;7;8].*)
exception Colori
let path_to x listaColori albero =
  let rec aux visitati albero colorePrec=
    match albero with
    Empty -> raise Colori
    | Tr(y, Empty, Empty) -> if x=y then List.rev (x::visitati) else raise Colori
    | Tr(y, l, r) -> if not (visitati=[]) then 
                        if colorePrec=(colore y listaColori) then raise Colori else
                          try
                          aux (y::visitati) l (colore y listaColori)
                          with Colori -> aux (y::visitati) r (colore y listaColori)
                        else
                          try
                          aux (y::visitati) l (colore y listaColori)
                          with Colori -> aux (y::visitati) r (colore y listaColori)
                        in aux [] albero Rosso;;

(* Associazione colori di esempio *)
let colori : int col_assoc = [
  (Rosso, [1; 4; 10]);
  (Giallo, [3; 8; 11]);
  (Verde, [0; 5; 6; 13]);
  (Blu, [2; 7; 9; 12; 14])
];;

(* Esempio di albero *)
let albero = 
  Tr (1,
    Tr (4,
      Tr (5,
        Tr (7,
          Tr (8, Empty, Empty),
          Empty),
        Empty),
      Empty),
    Tr (3,
      Tr (6, Empty, Empty),
      Tr (11, Empty, Empty)));;


Printf.printf "\n==========================\n";;

Printf.printf "\nTest path to: \n";;

(* Invocazione della funzione path_to *)
let () =
  try
    let percorso = path_to 6 colori albero in
    Printf.printf "Cammino trovato: [%s]\n"
      (String.concat "; " (List.map string_of_int percorso))
  with
    | Not_found -> print_endline "Nessun cammino trovato."
    | NoColore -> print_endline "Colore non definito.";;

Printf.printf "\nFine Alberi Binari \n";;

Printf.printf "\n==========================\n";;

Printf.printf "\nInizio Alberi N-ari: \n";;

type 'a ntree = Tr of 'a * 'a ntree list;;

(* (Dal compito d’esame di settembre 2010).
Scrivere una funzione ramo_di_primi: int ntree -> int che, applicata
a un albero n-ario di interi, riporti, se esiste, una foglia n dell’albero tale
che il ramo dell’albero dalla radice a n sia costituito da tutti numeri primi. *)

let rec is_primo x =
  if x<2 then false else
  let rec aux n =
    if n*n > x then true
    else
      if x mod n != 0 then aux (n+1) else false
    in aux 2;;

exception NoRamoPrimi
let rec ramo_di_primi (Tr(x, figli)) =
  if not(is_primo x) then raise NoRamoPrimi else
  match figli with
  [] -> x
  | _ ->
                      let rec scorriFigli figli = 
                      match figli with
                      [] -> raise NoRamoPrimi
                      | y::rest -> try
                                    ramo_di_primi y
                                    with NoRamoPrimi -> scorriFigli rest
                                  in scorriFigli figli;;

Printf.printf "\n==========================\n";;
Printf.printf "Test per ramo_di_primi: \n";;

let albero =
  Tr (2, [
    Tr (3, [
      Tr (2, []);             (* ramo: 2 → 3 → 5 ✔️ *)
      Tr (6, [])              (* ramo: 2 → 3 → 6 ✖️ *)
    ]);
    Tr (1, [
      Tr (7, []);             (* ramo: 2 → 4 → 7 ✖️ *)
    ])
  ])

  let () =
  let risultato = ramo_di_primi albero in
  print_endline ("Foglia con ramo di soli primi: " ^ string_of_int risultato);;

(*  Scrivere una funzione num_di_foglie: ’a ntree -> int che, applicata
a un albero n-ario, riporti il numero di foglie dell’albero.
 *)

 let rec num_di_foglie albero =
  match albero with
  Tr(x, []) -> 1
  | Tr(x, figli) -> List.fold_left (+) 0 (List.map (num_di_foglie) figli);;



(* (Dal compito d’esame di febbraio 2009).
Scrivere una funzione ramo_da_lista: ’a ntree -> ’a list -> ’a ->
’a list che, dato un albero T, una lista L senza ripetizioni e un’etichetta
k, riporti, se esiste, un ramo di T dalla radice a una foglia etichettata da k
che passi per tutti gli elementi di L esattamente una volta e contenga solo
nodi etichettati da elementi di L (in pratica, il cammino deve essere una
permutazione di L). Se un tale cammino non esiste, la funzione solleverà
un’eccezione.
 *)
 exception NoCammino

 let rec ramo_da_lista albero lista k =
  let rec aux visitati albero lista =
  match albero with
  Tr(x, []) -> if x=k && lista=[x] then List.rev (x::visitati) else raise NoCammino
  | Tr(x, figli) -> if not(List.mem x lista) then raise NoCammino else 
                    let rec scorriFigli figli =
                      match figli with
                      [] -> raise NoCammino
                      | y::rest -> try
                                    aux (x::visitati) y (List.filter ((<>) x) lista)
                                  with _ -> scorriFigli rest 
                                in scorriFigli figli
                              in aux [] albero lista;;

Printf.printf "\n==========================\n";;
Printf.printf "Test per ramo_da_lista: \n";;

(* Definizione dell'albero *)
let albero_test =
  Tr("A", [
    Tr("B", [
      Tr("D", [])
    ]);
    Tr("C", [
      Tr("E", []);
      Tr("F", [])
    ])
  ])

(* Esecuzione del test *)
let () =
  let risultato = ramo_da_lista albero_test ["A"; "C"; "E"] "E" in
  List.iter print_endline risultato;;
  Printf.printf "\n======================\n";;

  let albero_test =
  Tr("A", [
    Tr("B", [
      Tr("D", [])
    ]);
    Tr("C", [
      Tr("E", [
        Tr("G", [])
      ]);
      Tr("F", [])
    ])
  ]);;
  let risultato = ramo_da_lista albero_test ["A"; "C"; "E"; "G"] "G" in
  List.iter print_endline risultato;;
  Printf.printf "\n======================\n";;
  let risultato = ramo_da_lista albero_test ["A"; "B"; "D"] "D" in
  List.iter print_endline risultato;;
  Printf.printf "\n======================\n";;
  let risultato = ramo_da_lista (Tr("X", [])) ["X"] "X" in
  List.iter print_endline risultato;;


Printf.printf "\nFine Alberi N-ari \n";;

Printf.printf "\n==========================\n";;

Printf.printf "\nInizio grafi: \n";;

type 'a graph = ('a * 'a) list;;

 (* Scrivere una funzione test_connessi: ’a graph -> ’a -> ’a -> bool
che, dato un grafo orientato G e due nodi N e M, determini se esiste un
cammino da N a M. La funzione riporterà un booleano (non un cammino) *)
exception NodoNonEsistente
let rec successori x grafo = if not (List.exists (function (y,z) -> x=y || x=z) grafo) then raise NodoNonEsistente 
else List.map (snd) (List.filter (function (y,_) -> x=y) grafo);;
  let graph = [
  (1, 2); (1, 3); (1,4);
  (2, 6); 
  (3, 5);
  (6, 7);(6, 5);
  (5, 4);
  (4, 6); 
];;

let test_connessi grafo nodo1 nodo2 =
  let rec aux visitati pendenti =
    match pendenti with
    [] -> false
    | x::rest -> if x=nodo2 then true
                  else
                    if List.mem x visitati then aux visitati rest 
                    else aux (x::visitati) ((successori x grafo)@rest)
                  in aux [] [nodo1];;

  let x = test_connessi graph 1 5;;

  Printf.printf "\n=======================\n";;
  Printf.printf "Test test_connessi 1 5: ";;
  Printf.printf "%b\n" x;;

  (* (Dal compito d’esame di febbraio 2009). Sia data la seguente definizione
di tipo per rappresentare grafi orientati:*)
type 'a graph = 'a list * ('a * 'a) list;;

(*In altre parole, un grafo (orientato) è rappresentato da una coppia: il
primo elemento è una lista che rappresenta l’insieme dei nodi del grafo, il
secondo elemento è una lista di coppie, ciascuna delle quali rappresenta
un arco del grafo. Si assume che la lista dei nodi sia senza ripetizioni.*)
(*(a) Scrivere una funzione cammino: ’a graph -> ’a list -> ’a -> ’a
-> ’a list che, dato un grafo G, una lista L senza ripetizioni e due
nodi n e m di G, riporti, se esiste, un cammino da n a m che passi solo
per nodi contenuti in L e per ciascuno di essi esattamente una volta.
Se un tale cammino non esiste, la funzione solleverà un’eccezione.
Suggerimento: adattare l’algoritmo di ricerca di un cammino in modo
tale che dalla lista L vengano via via eliminati i nodi già incontrati. Si
noti che in tal modo non è necessario memorizzare i nodi già visitati,
dato che la lista L stessa serve ad evitare i cicli.*)
exception CamminoInesistente
let rec cammino (listaNodi, listaArchi) lista nodo1 nodo2 =
  if not (List.mem nodo1 listaNodi && List.mem nodo2 listaNodi) then raise CamminoInesistente else
  let rec aux visitati pendenti lista =
    match pendenti with
    [] -> raise CamminoInesistente
    | x::rest -> if x=(List.nth lista ((List.length lista)-1)) && x=nodo2 then List.rev (x::visitati)
                  else
                    if List.mem x visitati then aux visitati rest lista
                    else 
                        if List.mem x lista then aux (x::visitati) (rest@(successori x listaArchi)) lista
                        else aux visitati rest lista
                      in aux [] [nodo1] lista;;

let nodi = ["A"; "B"; "C"; "D"; "E"; "F"];;

let archi = [
  ("A", "B"); ("A", "C");
  ("B", "D"); ("C", "E");
  ("D", "F"); ("E", "F")
];;


  Printf.printf "\n=======================\n";;
  Printf.printf "Test cammino: \n";;

let g : string graph = (nodi, archi);;
let print_string_list lst =
  List.iter (fun s -> print_string s; print_string " ") lst;
  print_newline ();;

(* Caso 1: cammino possibile da A a F passando per B e D *)
print_string_list (cammino g ["A"; "B"; "D"; "F"] "A" "F");;
(* Possibile risultato: ["A"; "B"; "D"; "F"] *)

(* Caso 2: cammino possibile da A a F passando per C ed E *)
print_string_list (cammino g ["A"; "C"; "E"; "F"] "A" "F");;
(* Possibile risultato: ["A"; "C"; "E"; "F"] *)

(* Caso 3: impossibile se manca un nodo intermedio *)
(*print_string_list (cammino g ["A"; "B"; "F"] "A" "F");;*)
(* Solleva eccezione: nessun cammino che passa solo per A, B, F *)

(* Caso 4: se nodo finale non in L *)
(*print_string_list (cammino g ["A"; "B"; "D"] "A" "F");;*)
(* Solleva eccezione: "F" non presente in L, quindi non raggiungibile *)


(*(b) Un ciclo in un grafo è detto hamiltoniano se esso tocca tutti i nodi
del grafo esattamente una volta (eccetto il primo e l’ultimo nodo, che
sono, evidentemente, uguali). Scrivere una funzione hamiltoniano:
’a graph -> ’a list che, dato un grafo orientato G, determini se
in G esiste un ciclo hamiltoniano e riporti un tale ciclo, se esiste, un
errore altrimenti. Si ricordi che un cammino ciclico è una sequenza di nodi n1, n2, ..., nk,
con k > 1 e nk = n1, tale che, per ogni i = 1, ..., k − 1, esiste un arco
da ni a ni+1.
Si noti che, se x e y sono due nodi di un grafo, esiste un ciclo hamiltoniano su x se e solo se esiste un ciclo hamiltoniano su y. Quindi per
controllare se in un grafo c’è un ciclo hamiltoniano basta prendere un
nodo qualsiasi x e vedere se esiste un ciclo su x che passa esattamente
una volta per tutti i nodi del grafo.*)
exception NoHamiltoniano
let rec hamiltoniano (listaNodi, listaArchi) =
  if listaNodi = [] then raise NoHamiltoniano
  else 
  let rec aux visitati pendenti nodoPartenza=
    match pendenti with
    [] -> raise NoHamiltoniano
    | x::rest -> if x=nodoPartenza && (List.length listaNodi)=(List.length visitati) then List.rev (x::visitati)
                  else
                if List.mem x visitati then aux visitati rest nodoPartenza
                else
                  aux (x::visitati) (rest@(successori x listaArchi)) nodoPartenza
                in aux [List.hd listaNodi] (successori (List.hd listaNodi) listaArchi) (List.hd listaNodi);; 

  let g : int graph = 
  ([1; 2; 3; 4], 
   [ (1,2); (2,3); (3,4); (4,1) ]);;  (* Ciclo 1 → 2 → 3 → 4 → 1 *)

Printf.printf "\n=======================\n";;
Printf.printf "Test hamiltoniano: \n";;

let () =
  try
    let ciclo = hamiltoniano ([1;2;3;4], [(1,2); (2,3); (3,4); (4,1)]) in
    print_endline "Ciclo Hamiltoniano trovato:";
    List.iter (fun x -> Printf.printf "%d " x) ciclo;
    print_newline ()
  with
  | NoHamiltoniano -> print_endline "Nessun ciclo Hamiltoniano trovato.";;

(* Ricerca di un nodo raggiungibile da un nodo di ingresso *)

type 'a graph = ('a * 'a) list

exception NodoIrraggiungibile
let rec nodo_raggiungibile start goal grafo = 
  let rec aux visitati pendenti = 
    match pendenti with
    [] -> raise NodoIrraggiungibile
    | x::rest -> if x=goal then goal
                    else
                      if List.mem x visitati then aux visitati rest 
                        else aux (x::visitati) (rest@(successori x grafo))
                      in aux [] [start];;
  
(* Ricerca di un nodo a partire dal nodo di ingresso che soddisfi un determinato predicato*)
exception NonEsiste
let nodo_predicato start grafo p =
  let rec aux visitati pendenti = 
    match pendenti with
    [] -> raise NonEsiste
    | x::rest -> if p x then x
    else
      if List.mem x visitati then aux visitati rest
      else
        aux (x::visitati) (rest@(successori x grafo))
      in aux [] [start];;

let nodo_distanza_4 start grafo d=
  let rec aux visitati pendenti count = 
    match pendenti with
    [] -> raise NonEsiste
    | x::rest -> if count=d then x else
                  if List.mem x visitati then aux visitati rest (count)
                    else aux (x::visitati) (rest@(successori x grafo)) (count+1)
                  in aux [] [start] 1;;

let rec distance_node graph s e d =
  let rec aux visited succ acc =
    match succ with
    [] -> raise NonEsiste
    | x::xs -> if List.mem x visited then aux visited xs acc
    else try
          if x=e then if acc=d then (x::visited) else raise NonEsiste else aux (x::visited) (successori x graph) (acc+1)
    with NonEsiste -> aux visited xs acc
  in List.rev (aux [s] (successori s graph) 1);;

(* (Dal compito d’esame di settembre 2010). Scrivere una funzione
cammino_di_primi: int graph -> int-> int -> int list
che, applicata a un grafo (orientato) di interi g e interi start e goal riporti,
se esiste, un cammino in g da start a goal costituito soltanto da numeri
primi. *)

let is_primo x =
  if abs(x)<2 then false else
  let rec aux n=
    if n*n > abs(x) then true
    else if abs(x) mod n=0 then false
    else aux (n+1)
  in aux 2;;

exception NoCamminoDiPrimi
let rec cammino_di_primi grafo start goal =
  let rec aux visitati pendenti = 
    match pendenti with
    [] -> raise NoCamminoDiPrimi
    | x::rest -> if x=goal && is_primo x then List.rev (x::visitati) else 
                    try
                    if not(is_primo x) then raise NoCamminoDiPrimi
                    else
                      if List.mem x visitati then aux visitati rest
                      else aux (x::visitati) (rest@(successori x grafo))
                    with NoCamminoDiPrimi -> aux visitati rest
                    in aux [] [start];;

  let grafo_di_test = [
  (2, 3); (3, 5); (5, 7); (7, 4); (4, 11); (11, 13); (13, 17)
]

let start = 2
let goal = 7

let expected = [2; 3; 5; 7]
let result = cammino_di_primi grafo_di_test start goal

let () =
  Printf.printf "\n=======================\n";;
  Printf.printf "Test cammino_di_primi: \n";;
  Printf.printf "Grafo: [(2,3); (3,5); (5,7); (7,4); (4,11); (11,13); (13,17)]\n";;
  Printf.printf "Cammino da %d a %d con soli primi\n" start goal;;
  Printf.printf "Mi aspetto: [%s]\n"
    (String.concat "; " (List.map string_of_int expected));;
  Printf.printf "La funzione restituisce: [%s]\n"
    (String.concat "; " (List.map string_of_int result));;

(*  (Dal compito d’esame di giugno 2011). Definire una funzione path_n_p:
’a graph -> (’a -> bool) -> int -> ’a -> ’a list, che, applicata
a un grafo orientato g, un predicato p: ’a -> bool, un intero n e un
nodo start, riporti, se esiste, un cammino non ciclico da start fino a un
nodo x che soddisfa p e che contenga esattamente n nodi che soddisfano
p (incluso x). La funzione solleverà un’eccezione se un tale cammino non
esiste.*)

exception NoCamminoPredicato
let rec path_n_p grafo p n start=
    let rec aux visitati pendenti count =
      match pendenti with
      [] -> raise NoCamminoPredicato
      | x::rest ->
                    if List.mem x visitati then aux visitati rest count else
                        if count = n then 
                        if (successori (List.hd visitati) grafo) = [] then List.rev (visitati) else 
                          let rec verifica_resto_grafo da_esplorare gia_visitati =
                          match da_esplorare with
                          | [] -> List.rev visitati
                          | nodo::altri ->
                              if List.mem nodo gia_visitati then 
                                verifica_resto_grafo altri gia_visitati
                              else if p nodo then 
                                raise NoCamminoPredicato
                              else
                                let nuovi_successori = successori nodo grafo in
                                verifica_resto_grafo (nuovi_successori @ altri) (nodo::gia_visitati)
                        in
                        let primi_successori = successori (List.hd visitati) grafo in
                        verifica_resto_grafo primi_successori visitati
                        else
                        try
                        if p x then aux (x::visitati) ((successori x grafo)@rest) (count+1) else aux (x::visitati) ((successori x grafo)@rest) (count)
                        with NoCamminoPredicato -> aux visitati rest count
                    in aux [] [start] 0;;

  Printf.printf "\n=======================\n";;
  Printf.printf "Test path_n_p: \n";;
let grafo_test = [
  (1, 2); (2, 3); (3, 4); (4, 5); (5, 6);
  (1, 7); (7, 8); (8, 9); (9, 10)
];;

let p x = x mod 2 = 1 ;; (* predicato: dispari *)

let run_test start n expected_result_description =
  try
    let result = path_n_p grafo_test p n start in
    Printf.printf "\n=======================\n";
    Printf.printf "Test da start = %d con n = %d\n" start n;
    Printf.printf "Mi aspetto: %s\n" expected_result_description;
    Printf.printf "La funzione restituisce: [%s]\n"
      (String.concat "; " (List.map string_of_int result))
  with NoCamminoPredicato ->
    Printf.printf "\n=======================\n";
    Printf.printf "Test da start = %d con n = %d\n" start n;
    Printf.printf "Mi aspetto: %s\n" expected_result_description;
    Printf.printf "La funzione solleva: NoCamminoPredicato\n";;

(* Test 1: cammino valido [1;2;3;4;5] ha 3 dispari: 1,3,5 *)
run_test 1 3 "Un cammino valido con 3 nodi dispari (es. [1;2;3;4;5])";;

(* Test 2: cammino con troppi nodi che soddisfano p (dispari) → fallisce *)
run_test 1 2 "Un fallimento: tutti i cammini hanno più di 2 dispari";;

(* Test 3: nessun cammino → eccezione *)
run_test 10 1 "Un fallimento: nessun successore da 10";;

(* Test 4: cammino valido da nodo pari, che porta a 3 dispari *)
run_test 2 3 "Un fallimento: non ci sono 3 dispari";;

(* 9. (Dal compito d’esame di febbraio 2010). Scrivere un programma con
una funzione cammino_con_nodi: ’a graph -> ’a -> ’a list -> ’a
list che, dato un grafo orientato G, un nodo N di G e una lista L senza ripetizioni, restituisca, se esiste, un cammino senza cicli che, partendo
da N, contenga tutti i nodi di L (in qualsiasi ordine) ed eventualmente
anche altri nodi. Se un tale cammino non esiste, il programma solleverà
un’eccezione.
Ad esempio, se G è il grafo rappresentato da [(1, 2); (1, 3); (1, 4);
(2, 6); (3, 5); (4, 6); (6, 5); (6, 7); (5, 4)] e L è la lista [2;
5], la ricerca a partire dal nodo 1 restituirà il cammino [1; 2; 6; 5].
. Se la lista L è [2; 6; 3], la ricerca a partire dal nodo 1 fallirà perché
non esistono cammini in G che a partire da 1 tocchino tutti i nodi di L *)

let rec cammino_con_nodi grafo nodo lista =
  let rec aux visitati pendenti lista=
    match pendenti with
    [] -> raise CamminoInesistente
    | x::rest -> if List.mem x visitati then aux visitati rest lista else
                  if lista=[x] then List.rev (x::visitati) else
                    try
                    if List.mem x lista then 
                      aux (x::visitati) (successori x grafo) (List.filter ((<>) x) lista)
                    else
                      aux (x::visitati) (successori x grafo) lista
                    with CamminoInesistente -> aux visitati rest lista
                in aux [] [nodo] lista;;
                    
let grafo_test = [
  (1, 3); 
  (3, 5);
  (5, 4);
  (4, 6);
  (6, 2); 
];;

let x = cammino_con_nodi grafo_test 1 [2;3];;
Printf.printf "\n=======================\n";;
Printf.printf "Test cammino_con_nodi: \n";;

let print_int_list lst =
  let rec aux = function
    | [] -> print_string "]\n"
    | [x] -> Printf.printf "%d]\n" x
    | x::xs -> Printf.printf "%d; " x; aux xs
  in
  print_string "["; aux lst;;


Printf.printf "Expected: [1;2;6;5]\nRisultato: ";;
print_int_list x;;

(* Scrivere una funzione ciclo: ’a graph -> ’a -> ’a list che, dato un
grafo orientato G e un nodo N, riporti, se esiste, un ciclo su N, altrimenti
sollevi un’eccezione (in questo caso la funzione deve riportare una lista di
nodi). *)
exception NoCiclo
let ciclo grafo nodo =
  let rec aux visitati pendenti=
    match pendenti with
    [] -> raise NoCiclo
    | x::rest -> if x=nodo then List.rev (x::visitati) else
                  if List.mem x visitati then aux visitati rest
                  else
                    try
                      aux (x::visitati) (successori x grafo)
                    with NoCiclo -> aux visitati rest
                  in aux [] [nodo];;