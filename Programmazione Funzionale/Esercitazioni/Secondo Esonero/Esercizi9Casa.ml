(* Nella visita in postordine degli alberi n-ari vengono prima visitati tutti
 i sottoalberi, poi la radice. Nella visita simmetrica viene prima visitato
 il sottoalbero sinistro, poi la radice, poi gli altri sottoalberi (se ve ne
 sono). Implementare due funzioni postorder: ’a ntree-> ’a list e
 inorder: ’a ntree-> ’a list che, dato un albero n-ario, riportino la
 lista dei suoi nodi nell’ordine in cui sarebbero visitati secondo i due algo
ritmi di visita. *)

type 'a ntree = Tr of 'a * 'a ntree list;;

let rec postorder albero =
  match albero with
  Tr(x, []) -> [x]
  | Tr(x, list) -> List.flatten(List.map (postorder) list)@[x];;

let print_int_list lst =
  let rec aux l =
    match l with
    | [] -> print_string "]\n"
    | [x] -> Printf.printf "%d]\n" x
    | x::xs -> Printf.printf "%d; " x; aux xs
  in
  print_string "[";
  aux lst;;

let esempio =
  Tr(1, [Tr(2, []);Tr(3, [Tr(4, []);Tr(5, [])])]);;

  let () =
  let risultato = postorder esempio in
  Printf.printf "Visita postorder dell'albero n-ario:\n";
  print_int_list risultato

  let rec inorder albero =
    match albero with
    Tr(x, []) -> [x]
    | Tr(x, list) -> let meta = (List.length list)/2 in
                      List.flatten(List.map (inorder) (List.take meta list))@[x]@List.flatten(List.map (inorder) (List.drop meta list));;

  let esempio =
  Tr(1, [Tr(2, []);Tr(3, [Tr(4, []);Tr(5, [])])]);;

  let () =
  let risultato = inorder esempio in
  Printf.printf "Visita inorder dell'albero n-ario:\n";
  print_int_list risultato

  (* Scrivere una funzione foglie_in_lista: ’a list-> ’a ntree-> bool
 che, data una lista lst e un albero n-ario t, determini se ogni foglia di t
 appartiene a lst. *)
 let rec foglie_in_lista lst albero =
  match albero with
  Tr(x, []) -> List.mem x lst
  | Tr(x, list) -> List.fold_left (&&) true (List.map (foglie_in_lista lst) list);;

  let esempio =
  Tr (1, [
    Tr (2, []);
    Tr (3, [
      Tr (4, []);
      Tr (5, [])
    ])
  ]);;

let () =
  let lst1 = [2; 4; 5; 99] in
  let lst2 = [2; 4] in
  Printf.printf "Test 1 (true): %b\n" (foglie_in_lista lst1 esempio);
  Printf.printf "Test 2 (false): %b\n" (foglie_in_lista lst2 esempio);;


(* Scrivere una funzione num_di_foglie: ’a ntree-> int che, applicata
 a un albero n-ario, riporti il numero di foglie dell’albero. *)

 let rec num_di_foglie albero =
  match albero with
  Tr(x, []) -> 1
  | Tr(x, list) -> List.fold_left (+) 0 (List.map (num_di_foglie) list);;

  let esempio =
  Tr ("a", [
    Tr ("b", []);
    Tr ("c", [
      Tr ("d", []);
      Tr ("e", [])
    ])
  ])
let () =
  Printf.printf "Numero di foglie: %d\n" (num_di_foglie esempio);;
  (* Output atteso: 3 *)

(* Una lista di interi non negativi L può determinare un sottoalbero di un
 albero n-ario T: quello che si ottiene, a partire dalla radice, scendendo,
 per ogni elemento n di L, al sottoalbero in posizione n nella lista dei
 sottoalberi (se esiste)– si ricordi che la posizione degli elementi in una
 lista si conta a partire da 0. Se la lista è più lunga del ramo cui essa
 conduce, oppure se a qualche livello non esiste un numero sufficiente di
 sottoalberi, allora L non determina alcun sottoalbero di T.
 Ad esempio, se T è l’albero sotto rappresentato 
  allora la lista [2;0] determina il sottoalbero che ha radice 11, la lista
 [2;2;1] quello che ha radice 18, [2;2;1;0] il sottoalbero costituito soltanto
 dal nodo 19. Le liste [1;2] e [0;1;1] non determinano alcun sottoalbero
 di T.
 Scrivere una funzione listaGuida: ’a list-> ’a ntree-> ’a, che,
 data una lista L di interi e un albero n-ario T, riporti la radice del sot
toalbero di T determinato da L, se L determina un sottoalbero di T, un
 errore altrimenti.*)

 (*let sottoalbero listaAlberi lista = let sottoal =  List.hd(List.drop (List.hd lista) listaAlberi) in match sottoal withz;;*)


 exception NoTree
 let rec listaGuida lst albero = 
  match albero with
  | Tr(x, list) ->  match lst with
                    [] -> x
                    | lst -> try
                            listaGuida (List.drop 1 lst) (List.hd(List.drop (List.hd lst) list))
                            with _ -> raise NoTree;;

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

  (*  Se T è un albero n-ario etichettato da numeri interi, il costo di una foglia
 N di T è la somma di tutti i nodi che si trovano sul ramo che va dalla
 radice di T a N. Scrivere una funzione foglia_costo: ’int ntree->
 (int * int) che, dato un albero n-ario di interi, restuisca l’etichetta e il
 costo della foglia più costosa dell’albero. L’albero può anche avere diversi
 nodi con la stessa etichetta. *)
(* Definire una funzione tutte_foglie_costi: int ntree-> (int * int)
 list che, applicata a un albero n-ario T etichettato da interi, riporti una
 lista di coppie, ciascuna delle quali ha la forma (f,n), dove f è l’etichetta
 di una foglia in T e n il costo di tale foglia (dove il costo di una foglia
 è definito come nell’esercizio precedente). Anche in questo caso, l’albero
 può anche avere diversi nodi con la stessa etichetta. *)
 let rec tutte_foglie_costi albero =
  let rec aux costo l albero =
    match albero with
    Tr(x, [])-> (x, costo+x)::l
    | Tr(x, list) -> List.flatten(List.map (aux (costo+x) l) list)
  in aux 0 [] albero;;

 let rec foglia_costo albero = 
  let tutteFoglie = tutte_foglie_costi albero in
  let rec aux (etichetta, max) tutteFoglie =
    match tutteFoglie with
    [] -> (etichetta, max)
    | (e',m')::rest -> if max < m' then aux (e',m') rest else aux (etichetta, max) rest
  in aux (List.hd tutteFoglie) tutteFoglie;;

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

  let () =
  let risultato = foglia_costo albero in
  match risultato with
  | (etichetta, costo) ->
      Printf.printf "Foglia con costo massimo: %d (costo: %d)\n" etichetta costo;;


  (* (Dal compito d’esame di febbraio 2009).
 Scrivere una funzione ramo_da_lista: ’a ntree-> ’a list-> ’a->
 ’a list che, dato un albero T, una lista L senza ripetizioni e un’etichetta
 k, riporti, se esiste, un ramo di T dalla radice a una foglia etichettata da k
 che passi per tutti gli elementi di L esattamente una volta e contenga solo
 nodi etichettati da elementi di L (in pratica, il cammino deve essere una
 permutazione di L). Se un tale cammino non esiste, la funzione solleverà
 un’eccezione. *)

 (*let ramo_da_lista albero lst k =
  let rec aux elementiAttraversati albero lst =
  match albero with
  | Tr(x, list) -> if List.mem x lst then 
                match list with
                 [] -> if x=k then (x::elementiAttraversati) else raise NoTree 
                 | y::rest -> try 
                              aux (x::elementiAttraversati) y (List.filter (function z-> z!=x) lst) 
                              with NoTree -> aux elementiAttraversati y (List.filter (function z-> z!=x) lst) 
                 else raise NoTree 
                in aux [] albero lst;;*)

 (*let ramo_da_lista albero lst k =
  let rec aux elementiAttraversati albero lst =
  match albero with
  Tr(x, []) -> if x=k && lst=[] then elementiAttraversati@[x] else raise NoTree
  | Tr(x, list) ->  match list with
                    [] -> if x=k && lst=[] then elementiAttraversati@[x] else raise NoTree
                    | y::rest -> match y with
                                Tr(x, list) -> 
    
    if List.mem x lst then
                       aux (elementiAttraversati@[x]) y (List.filter ((<>) x) lst)
                      else try
                          aux(elementiAttraversati) y lst
                          with _ -> s
                  in aux [] albero lst;;*)

(*let risultato = ramo_da_lista albero l k;;
(* Output atteso: [3; 5; 7] *)
print_int_list risultato;;*)

(* Se T è un albero n-ario etichettato da numeri interi, il costo di una foglia
N di T è la somma di tutti i nodi che si trovano sul ramo che va dalla
radice di T a N. Scrivere una funzione foglia_costo: ’int ntree ->
(int * int) che, dato un albero n-ario di interi, restuisca l’etichetta e il
costo della foglia più costosa dell’albero. L’albero può anche avere diversi
nodi con la stessa etichetta. *)

let rec tutte_foglie_costi albero = 
  let rec aux risultato costo albero=
    match albero with
    Tr(x, []) -> (x, x+costo)::risultato
    | Tr(x, list) -> List.flatten (List.map (aux risultato (costo+x)) list)
in aux [] 0 albero;;

let foglia_costo albero = 
  let tuttefoglie = tutte_foglie_costi albero in
    let rec aux (etichetta, maxCosto) tuttefoglie =
      match tuttefoglie with
      [] -> (etichetta, maxCosto)
      | (e,c)::rest -> if maxCosto < c then aux (e,c) rest else aux (etichetta, maxCosto) rest
in aux (List.hd tuttefoglie) tuttefoglie;;

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

  let () =
  let risultato = foglia_costo albero in
  match risultato with
  | (etichetta, costo) ->
      Printf.printf "Foglia con costo massimo: %d (costo: %d)\n" etichetta costo;;

  let rec postorder (Tr(x, list)) = List.flatten (List.map (postorder) list)@[x];;

  
let esempio =
  Tr(1, [Tr(2, []);Tr(3, [Tr(4, []);Tr(5, [])])]);;

  let () =
  let risultato = postorder esempio in
  Printf.printf "Visita postorder dell'albero n-ario:\n";
  print_int_list risultato;;

  (*. (Dal compito d’esame di febbraio 2009).
Scrivere una funzione ramo_da_lista: ’a ntree -> ’a list -> ’a ->
’a list che, dato un albero T, una lista L senza ripetizioni e un’etichetta
k, riporti, se esiste, un ramo di T dalla radice a una foglia etichettata da k
che passi per tutti gli elementi di L esattamente una volta e contenga solo
nodi etichettati da elementi di L (in pratica, il cammino deve essere una
permutazione di L). Se un tale cammino non esiste, la funzione solleverà
un’eccezione*)

let rec ramo_da_lista albero lista k =
  let rec aux risultato albero lista = 
    match albero with
    Tr(x, []) -> if x=k && lista=[x] then risultato@[x] else raise NoTree
    | Tr(x, figli) -> let rec scorrifigli figli =
                      match figli with
                      [] -> raise NoTree
                      | z::rest -> 
                        try
                        if (List.mem x lista) then aux (risultato@[x]) z (List.filter ((<>) x) lista) else aux risultato z lista
                        with NoTree -> scorrifigli rest
                      in scorrifigli figli
    in aux [] albero lista;;


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

  (* 9. (Dal compito d’esame di settembre 2010).
Scrivere una funzione ramo_di_primi: int ntree -> int che, applicata
a un albero n-ario di interi, riporti, se esiste, una foglia n dell’albero tale
che il ramo dell’albero dalla radice a n sia costituito da tutti numeri primi. *)

let is_primo x =
  if x = 0 || x = 1 then false else
  let rec check_divisori d =
    if x = d then true
    else if (x mod d)=0 then false else check_divisori(d+1)
  in check_divisori 2;;

exception NoRamoPrimi

let rec ramo_di_primi albero =
    let rec aux (Tr(x, figli)) =
      if not (is_primo x) then raise NoRamoPrimi else (* Se il nodo corrente è primo, controllo i figli *)
      match figli with
      [] -> x 
      | (Tr(y, figli2))::rest -> try (* Provo il figlio*)
                                 aux (Tr(y, figli2))  
                                  with NoRamoPrimi -> aux (Tr(x, rest)) (* Se solleva l'eccezione *)
                                in aux albero;;

let rec ramo_di_primi albero =
    let rec aux (Tr(x, figli)) =
      if not (is_primo x) then raise NoRamoPrimi else (* Se il nodo corrente è primo, controllo i figli *)
      match figli with
      [] -> x 
      | _ -> let rec prova_figli figli=
              match figli with
              [] -> raise NoRamoPrimi
              | y::rest ->
                          try (* Provo il figlio*)
                          aux y  
                          with NoRamoPrimi -> prova_figli rest (* Se solleva l'eccezione *)
                        in prova_figli figli
in aux albero;;



  
let albero_test =
  Tr(2, [                                      (* radice: primo *)
    Tr(4, [                                    (* 4 non primo → ramo scartato *)
      Tr(5, [])                                (* 5 è primo, ma 4 no → scartato *)
    ]);
    Tr(3, [                                    (* 3 primo *)
      Tr(13, [                                  (* 7 primo *)
        Tr(11, [])                             (* 11 primo → foglia valida *)
      ]);
      Tr(2, [                                  (* 6 non primo → ramo scartato *)
        Tr(13, [])
      ])
    ])
  ]);;

  let x = ramo_di_primi albero_test;;

  Printf.printf "Ramo di primi: %d" x;;

(* 10. (Dal compito d’esame di giugno 2011, adattato agli alberi n-ari).
Definire una funzione path_non_pred: (’a -> bool) -> ’a ntree -> ’a list, che, applicata a un predicato p: ’a -> bool e a un albero
t: ’a ntree, riporti, se esiste, un cammino dalla radice a una foglia di
t che non contenga alcun nodo che soddisfa p. La funzione solleverà
un’eccezione se un tale cammino non esiste.*)

exception CamminoNonEsistente
let rec path_non_pred p albero = 
    let rec aux visitati (Tr(x, figli)) =
      if p x then raise CamminoNonEsistente else
        match figli with
        [] -> visitati@[x]
        | _ -> let rec verifica_figli figli =
                match figli with
                [] -> raise CamminoNonEsistente
                | (Tr(y, figli2))::rest -> try
                                            aux (visitati@[x]) (Tr(y, figli2))
                                          with CamminoNonEsistente -> verifica_figli rest
                                        in verifica_figli figli
                                      in aux [] albero;;

  let albero_test =
  Tr(1, [                                      (* radice: primo *)
    Tr(2, [                                    (* 4 non primo → ramo scartato *)
      Tr(5, [])                                (* 5 è primo, ma 4 no → scartato *)
    ]);
    Tr(3, [                                    (* 3 primo *)
      Tr(134, [                                  (* 7 primo *)
        Tr(114, [])                             (* 11 primo → foglia valida *)
      ]);
      Tr(1, [                                  (* 6 non primo → ramo scartato *)
        Tr(13, [])
      ])
    ])
  ]);;

  let x = path_non_pred (function x-> (x mod 2)=0) albero_test;;

  Printf.printf "\nPath non pred: \n";;
  print_int_list x;;

  (*  (Dal compito d’esame di settembre 2011, adattato agli alberi n-ari).
Scrivere un predicato same_structure: ’a ntree -> ’b ntree -> bool
che determini se due alberi n-ari hanno la stessa struttura (cioè se essi sono
uguali quando si ignorano le rispettive etichette). *)

  let rec same_structure (Tr(x, figli1)) (Tr(y, figli2)) =
    let rec scorrifigli figli1 figli2 =
        match (figli1, figli2) with 
        ([], []) -> true
        | ([], piena) -> false
        | (piena, []) -> false
        | (z1::rest1, z2::rest2) -> scorrifigli rest1 rest2
    in scorrifigli figli1 figli2;;


  let a1 = Tr (1, [Tr (2, []); Tr (3, [])])
  let a2 = Tr ('x', [Tr ('y', []); Tr ('z', [])])

  let b1 = Tr (1, [Tr (2, []); Tr (3, [])])
  let b2 = Tr ('a', [Tr ('b', [])]) 

  let c1 = Tr (1, [Tr (2, [Tr (3, [])]); Tr (4, [])])
  let c2 = Tr ('a', [Tr ('b', [Tr ('c', [])]); Tr ('d', [])])

  let x = same_structure c1 c2;;

  Printf.printf "\nHanno la stessa struttura: %b\n" x;;

  (* (Dal compito d’esame di luglio 2009, adattato agli alberi n-ari).
Si considerino le seguenti dichiarazioni di tipo, per la rappresentazione di
colori e associazioni di colori:
type col = Rosso | Giallo | Verde | Blu
type ’a col_assoc = (col * ’a list) list
Scrivere un programma con una funzione
ramo_colorato: ’a -> ’a col_assoc -> ’a ntree -> ’a list, che, dato
un valore x, un’associazione di colori e un albero n-ario, riporti – se esiste
– un ramo a colori alterni, dalla radice dell’albero a una foglia etichettata
da x. Se un tale ramo non esiste, solleverà un’eccezione (si veda l’esercizio
14 del gruppo 8 che propone lo stesso problema per gli alberi binari). *)

    type col = Rosso | Giallo | Verde | Blu
    type 'a col_assoc = (col * 'a list) list

    let lst = [(Rosso,[1;2;4;7;10]); (Giallo,[3;8;11]);
    (Verde,[0;5;6;13]); (Blu,[9;12;14;15])];;

    exception NumeroNonEsistente 
    
    let rec colore x assoc_col =
      match assoc_col with
      [] -> raise NumeroNonEsistente
      | (col, lista)::rest -> if (List.mem x lista) then col else colore x rest;;

    exception NoRamo

    let ramo_colorato x assoc_col (Tr(nodo,figli)) = 
      let rec aux visitati (Tr(nodo2, figli)) =
        match figli with
        [] -> if nodo2=x then (List.rev visitati) else raise NoRamo
        | _ -> let rec scorrifigli figli =
                match figli with
                [] -> raise NoRamo
                | (Tr(y,figli2))::rest -> 
                              try
                              if (colore (List.hd visitati) assoc_col) = (colore y assoc_col) then raise NoRamo 
                              else aux (y::visitati) (Tr(y,figli2))
                              with NoRamo -> scorrifigli rest
                            in scorrifigli figli
                            in aux [nodo] (Tr(nodo, figli));;  

let test_ramo_colorato x col_assoc albero =
  try
    let ramo = ramo_colorato x col_assoc albero in
    Printf.printf "Ramo per %d: [" x;
    List.iter (fun n -> Printf.printf "%d; " n) ramo;
    Printf.printf "]\n"
  with
  | NoRamo -> Printf.printf "Nessun ramo alternato verso %d trovato.\n" x
  | NumeroNonEsistente -> Printf.printf "Il numero %d non ha colore associato.\n" x

  let albero =
  Tr (1, [
    Tr (2, [
      Tr (4, []);
      Tr (5, [])
    ]);
    Tr (3, [
      Tr (6, []);
      Tr (7, [])
    ])
  ])


  let colori = [
  (Rosso, [1; 4; 6]);
  (Giallo, [2; 5]);
  (Verde, [3; 7])
];;


test_ramo_colorato 4 colori albero;;
test_ramo_colorato 6 colori albero;;
test_ramo_colorato 7 colori albero;;  (* Nodo non presente *);;