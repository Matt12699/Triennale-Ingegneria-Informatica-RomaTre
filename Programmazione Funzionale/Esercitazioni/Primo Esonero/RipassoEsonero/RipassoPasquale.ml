(* Definire una funzione ultime_cifre: int-> int * int che riporti il
 valore intero delle due ultime cifre di un int. Ad esempio:
 ultime_cifre 245 = (4,5)
 ultime_cifre 5 = (0,5).
 Se il numero è negativo, il segno va ignorato. Ad esempio
 ultime_cifre (-245) = (4,5)
 ultime_cifre (-5) = (0,5).
 La funzione non deve mai sollevare eccezioni, ma riportare sempre una
 coppia di interi.
 Si cerchi di fornire una soluzione semplice, che operi direttamente sul
 numero, anziché passare per la sua rappresentazione come stringa. Si
 ricordi che n/m e’ il risultato della divisione intera, n mod m il modulo, e
 abs n il valore assoluto. *)

 let ultime_cifre x = (abs((x mod 100)/10) , abs(x mod 10));;

 let (x,y) = ultime_cifre (245);;
Printf.printf "\n------------------------------\n";;
Printf.printf "Le ultime cifre di 245 sono: (%d,%d)" x y;;

(* Una cifra è bella se è 0, 3, 7; un numero è bello se la sua ultima cifra è bella
 e la penultima (se esiste) non lo è. Quindi in particolare le cifre belle sono
 numeri belli. Definire un predicato bello: int-> bool che determini
 se un numero è bello. La funzione non deve mai sollevare eccezioni, ma
 riportare sempre un bool. *)

 let bellino x = x=0 || x=3 || x=7;;

 let bello x = 
  if abs(x) < 10 then
    bellino x
  else
    let (penultima, ultima) = ultime_cifre x in
    if (bellino ultima) && not(bellino penultima) then true
    else 
      false;;

Printf.printf "\n------------------------------\n";;
let x = bello 247;;
Printf.printf "247 e' un numero bello: %b" x;;

(*(f) (stringa_max: unit-> string) Leggere da tastiera una sequenza
 di stringhe non vuote, separate da Enter e terminata dalla stringa
 vuota, e riportare la stringa di lunghezza massima. Se non viene
 immessa nessuna stringa non vuota, la funzione riporterà la stringa
 vuota.
 (Tenere presente la funzione predefinita String.length: string->
 int) *)

 let stringa_max() = 
  let rec aux stringaM lung = 
  let x = read_line() in
  match x with
  "" -> stringaM
  | _ -> if (String.length x)=0 then
          stringaM else
      if lung < (String.length x) then
          aux x (String.length x) else
          aux stringaM lung
  in aux "" 0;; 

(*  Una funzione enumera: ’a list-> (int * ’a) list che, appli
cata a una lista lst=[x0;x1;x2;...;xk], riporti la lista di coppie
 [(0,x0);(1,x1);(2,x2);...;(k,xk)]. *)
 let enumera list = 
  let rec aux cont risultato list =
    match list with
    [] -> risultato
    | x::rest -> aux (cont+1) (risultato@[(cont, x)]) rest
  in aux 0 [] list;;

  
let stampa_enumera lista =
  let enumerata = enumera lista in
  List.iter (fun (i, x) -> Printf.printf "%d: %d\n" i x) enumerata
;;

let list = [0; 0; 2; 3; 4];;
Printf.printf "\n------------------------------\n";;
Printf.printf "La lista con enumera: \n";;
stampa_enumera list;;

(* partition: (’a-> ’bool)-> ’a list-> (’a list * ’a list),tale
 che partition p lst = (yes,no), dove yes contiene tutti gli elementi di
 lst che soddisfano il predicato p, e no quelli che non lo soddisfano. Gli
 elementi delle due liste yes e no possono essere in qualsiasi ordine.
 Adesempio, il valore di partition (function n-> n mod 2 = 0) [0;2;
 4;6;7;8;9;10] può essere ([10; 8; 6; 4; 2; 0], [9; 7]). *)

let partition p list = 
  let rec aux veri falsi list = match list with
  [] -> (veri, falsi)
  | x::rest -> if (p x) then aux (veri@[x]) falsi rest
  else
    aux veri (falsi@[x]) rest
  in aux [] [] list;;
  Printf.printf "\n------------------------------\n";;
  let stampa_partition lista =
    let (yes, no) = partition (fun n -> n mod 2 = 0) lista in
    Printf.printf "Numeri pari: %s\n" (String.concat "; " (List.map string_of_int yes));
    Printf.printf "Numeri dispari: %s\n" (String.concat "; " (List.map string_of_int no));;
  
  (* Eseguiamo la funzione con un esempio *)
  stampa_partition [0; 2; 4; 6; 7; 8; 9; 10];;

(* Scrivere una funzione find: ’a-> ’a list-> ’a list * ’a list,che,
 applicata a un elemento x e a una lista L, spezzi L in due parti: la prima
 contiene tutti gli elementi che vanno dall’inizio della lista fino alla prima
 occorrenza di x esclusa; la seconda contiene tutti gli elementi che seguono
 la prima occorrenza di x. La funzione solleverà un’eccezione se L non con
tiene x. Ad esempio, find 3 [1;2;3;4;5;6;3] = ([1;2],[4;5;6;3]). *)

exception XNotHere
let find y list =
  let rec aux prima list=
    match list with
    [] -> raise XNotHere
    | x::rest ->  
                  if x=y then (prima, rest)
                  else
                    aux (prima@[x]) rest
                  in aux [] list;;

Printf.printf "\n------------------------------\n";;

(* Esempio di utilizzo: *)
let () =
  try
    let (before, after) = find 3 [1; 2; 3; 4; 5; 6; 3] in
    Printf.printf "Before: [%s]\n" (String.concat "; " (List.map string_of_int before));
    Printf.printf "After: [%s]\n" (String.concat "; " (List.map string_of_int after))
  with
  | XNotHere -> Printf.printf "Elemento non trovato nella lista\n"
;;

(* Utilizzando la funzione find definita al punto precedente, definire una fun
zione spezza: ’a-> ’a list-> ’a list * ’a list, che, applicata a
 un elemento x e una lista L riporti una coppia di liste (L1,L2), dove L1
 contiene tutti gli elementi di L che vanno dalla prima alla seconda oc
correnza di x, estremi esclusi, e L2 contiene tutti gli elementi di L che
 seguono la seconda occorrenza di x. La funzione solleverà un’eccezione
 se L non contiene almeno due occorrenze di x. Ad esempio, spezza 3
 [1;2;3;4;5;6;3;7;8;9;3;10] = ([4;5;6],[7;8;9;3;10]). *)

 let spezza y list = 
  try
    let (_, parteBuona) = find y list in
    let (prima, dopo) = find y parteBuona in
    (prima, dopo)
  with  | XNotHere -> raise XNotHere;;

Printf.printf "\n------------------------------\n";;

(* Esempio di utilizzo: *)
let () =
  try
    let (list1, list2) = spezza 3 [1;2;3;4;5;6;3;7;8;9;3;10] in
    Printf.printf "L1: [%s]\n" (String.concat "; " (List.map string_of_int list1));
    Printf.printf "L2: [%s]\n" (String.concat "; " (List.map string_of_int list2))
  with
  | XNotHere -> Printf.printf "Elemento non trovato almeno due volte nella lista\n"
;;

(*(Dall’esame di Febbraio 2010) Scrivere una funzione
 prendi: (’a-> bool)-> ’a list-> ’a * ’a list
 che, applicata a un predicato p e una lista L sollevi un’eccezione se L non
 contiene alcun elemento che soddisfa p, altrimenti restituisca una coppia (x,L′)
 dove x è un elemento di L che soddisfa p e L′ contiene tutti gli altri elementi di
 L, in qualsiasi ordine. Se L contiene più elementi che soddisfano p, dalla lista
 ne verrà rimosso soltanto uno (in altri termini, L′ ha solo un elemento in meno
 di L).
 Ad esempio, il valore di prendi (function x-> x > 10) [3; 20; 7; 11;
 8; 30; 20] può essere (20, [3; 7; 11; 8; 30; 20])*)

 exception NoElement
 let prendi p list = 
  let rec aux risultato list = match list with
    [] -> raise NoElement
    | x::rest -> if p x then (x, risultato@rest)
    else  aux (risultato@[x]) rest
  in aux [] list;;

  Printf.printf "\n------------------------------\n";;

  let () =
  try
    let (preso, resto) = prendi (fun x -> x > 0) [3; 20; 7; 11; 8; 30; 20] in
    Printf.printf "Elemento preso: %d\n" preso;
    Printf.printf "Lista restante: [%s]\n"
      (String.concat "; " (List.map string_of_int resto))
  with
  | NoElement -> Printf.printf "Nessun elemento soddisfa il predicato.\n"
;;

(* Sia data una matrice quadrata le cui caselle possono avere o no un contenuto,
 rappresentata da tre componenti:
 • la dimensione della matrice;
 • una lista associativa, che associa a ogni coppia di coordinate il suo con
tenuto se presente.
 Ad esempio, la variabile labirinto sotto dichiarata rappresenta una matrice
 del tipo considerato:
 let labirinto = (5,
 [((1,0),"oro"); ((3,1),"oro"); ((4,3),"oro");
 ((0,1),"argento"); ((2,4),"argento"); ((0,2),"mostro");
 ((1,1),"mostro"); ((1,3),"mostro"); ((2,3),"mostro");
 ((3,0),"mostro"); ((4,2),"mostro")])
 Le caselle a cui non è associato alcun contenuto sono vuote. Una matrice è
 dunque rappresentata da un valore di tipo int * ((int * int) * ’a) list.
 Definire, usando List.exists, List.for_all e List.find:
 (a) una funzione in_riga: (int * ((int * int) * ’a) list)-> int->
 ’a-> bool, che, data una matrice rappresentata come sopra, un numero
 di riga e un valore, verifichi se la riga data contiene il valore dato;
 (b) una funzione trova_colonna: (int * ((int * int) * ’a) list)->
 int-> ’a, che, data una matrice rappresentata come sopra, un numero di
 riga r e un valore v, riporti il numero di colonna c tale che (r,c) contiene
 v (se esiste), altrimenti sollevi un’eccezione;
 (c) una funzione in_tutte: (int * ((int * int) * ’a) list)-> ’a->
 bool, che, data una matrice rappresentata come sopra e un valore, verifichi
 se tutte le righe della matrice contengono il valore dato.*)

 (*(a) una funzione in_riga: (int * ((int * int) * ’a) list)-> int->
 ’a-> bool, che, data una matrice rappresentata come sopra, un numero
 di riga e un valore, verifichi se la riga data contiene il valore dato; *)
 let in_riga labirinto riga valore = 
  let (_, contenuto) = labirinto in
  List.exists (function ((x,_), valor)-> x=riga && valor=valore) contenuto;;

 let in_riga' labirinto riga valore = 
  let (_, contenuto) = labirinto in 
      let rec aux contenuto=
      match contenuto with
      [] -> false
      | ((x,_),valor)::rest -> if x=riga && valor=valore then true
      else
      aux rest
      in aux contenuto;;

Printf.printf "\n------------------------------\n";;

let mappa = (3, [((0, 0), "a"); ((1, 2), "b"); ((1, 1), "c"); ((2, 0), "d")])

let _ =
  Printf.printf "%b\n" (in_riga' mappa 1 "b");  (* true *)
  Printf.printf "%b\n" (in_riga' mappa 1 "a");  (* false *)
  Printf.printf "%b\n" (in_riga' mappa 2 "d");  (* true *)
;;
                                  
(*(b) una funzione trova_colonna: (int * ((int * int) * ’a) list)->
 int-> ’a, che, data una matrice rappresentata come sopra, un numero di
 riga r e un valore v, riporti il numero di colonna c tale che (r,c) contiene
 v (se esiste), altrimenti sollevi un’eccezione;*)
 exception NonEsiste
 let trova_colonna labirinto riga valore =
  let (_, contenuto) = labirinto in
  try
    let ((_, colonna), _) = List.find (function ((x, c), valor) -> x = riga && valor = valore) contenuto in
    colonna
  with Not_found -> raise NonEsiste
;;

 let trova_colonna' labirinto riga valore =
  let (_, contenuto) = labirinto in 
  let rec aux contenuto=
  match contenuto with
  [] -> raise NonEsiste
  | ((x,c),valor)::rest -> if x=riga && valor=valore then c
  else
  aux rest
  in aux contenuto;;

  Printf.printf "\n------------------------------\n";;

  let labirinto = (3, [((0,0), "a"); ((1,2), "b"); ((2,1), "c")])

  let () =
    Printf.printf "Colonna: %d\n" (trova_colonna' labirinto 1 "b");  (* → 2 *)
    Printf.printf "Colonna: %d\n" (trova_colonna' labirinto 0 "a");  (* → 0 *)
    (*Printf.printf "Colonna: %d\n" (trova_colonna labirinto 2 "x");  (* → eccezione *)*)
  ;;

(*(c) una funzione in_tutte: (int * ((int * int) * ’a) list)-> ’a->
 bool, che, data una matrice rappresentata come sopra e un valore, verifichi
 se tutte le righe della matrice contengono il valore dato.*)
 let in_tutte labirinto valore = 
  let (_, contenuto) = labirinto in
  List.for_all (function ((x,_), valor)-> valor=valore) contenuto;;

let in_tutte' labirinto valore = 
  let (_, contenuto) = labirinto in 
      let rec aux contenuto=
      match contenuto with
      [] -> true
      | ((x,_),valor)::rest -> if valor<>valore then false
      else
      aux rest
      in aux contenuto;;

Printf.printf "\n------------------------------\n";;

let labirinto1 = (3, [((0,0), "x"); ((1,2), "x"); ((2,1), "x")])
let labirinto2 = (3, [((0,0), "x"); ((1,2), "y"); ((2,1), "x")])

let () =
  Printf.printf "%b\n" (in_tutte' labirinto1 "x");  (* true *)
  Printf.printf "%b\n" (in_tutte' labirinto2 "x");  (* false *)
;;

let pi = 3.14159;;
let area x = pi *. x;;
let pi = 0.0;;
let x = "pippo";;

let a = area 3.0;;
Printf.printf "\n------------------------------\n";;
Printf.printf "Il risultato dell'esercizio 1.1 e': %f" a;;

(* (Esercizio 8 pag 45 del libro di testo) Scrivere una funzione data: int *
 string-> bool, che, applicata a una coppia (d,m), dove d è un intero e
 m una stringa, determini se la coppia rappresenta una data corretta, assu
mendoche l’anno non sia bisestile. Si assume che i mesi siano rappresentati
 da stringhe con caratteri minuscoli ("gennaio", "febbraio",...).
 La funzione non deve mai sollevare eccezioni, ma riportare sempre un
 bool *)

 let data (d, m) = match m with
  "gennaio" | "marzo" | "maggio" | "luglio" | "agosto" | "ottobre" | "dicembre" -> d>=0 && d<=31
  | "aprile" | "giugno" | "settembre" | "novembre" -> d>=0 && d<=30
  | "febbraio" -> d>=0 && d<=28
  | _ -> false;;
  
Printf.printf "\n------------------------------\n";;
let x = data (28, "febbraio");;
Printf.printf "Data di (28, febbraio) expected:True %b" x;;
let x = data (28, "mercoledi");;
Printf.printf "\nData di (28, mercoledi) expected:False %b" x;;
let x = data (32, "agosto");;
Printf.printf "\nData di (32, agosto) expected:False %b" x;;

(*  (tutti_minori: int-> bool) Dato un intero n, leggere da tastiera
 una sequenza di interi (anche negativi), separati da Enter e terminata
 dalla stringa vuota (o da una qualsiasi stringa che non rappresenti
 un intero), e determinare (riportando true o false) se i numeri letti
 sono tutti minori di n. La funzione non deve mai sollevare eccezioni
 e la sua esecuzione non deve terminare finché non viene immessa la
 stringa vuota (o non numerica). *)

 let rec tutti_minori n =
  let x = read_int() in
  try
    if x<n then tutti_minori n
    else false
  with _ -> true;;

(*  Una funzione copy: int-> ’a-> ’a list tale che copy n x ri
porti la lista di lunghezza n i cui elementi sono tutti uguali a x.
 Determinare il valore e il tipo di copy 3 (copy 2 8). *)

let copy n x = 
  let rec aux n risultato = 
    if n = 0 then risultato
    else
    aux (n-1) (x::risultato)
    in aux n [];;

Printf.printf "\n------------------------------\n";;

let print_list lst =
  Printf.printf "[";
  List.iter (fun x -> Printf.printf "%d; " x) lst;
  Printf.printf "]\n";;

let x = copy 3 4;;
print_list x;;

(* verifica_matrice: int-> int list list-> bool, che, dato un in
tero n e una matrice di interi, rappresentata mediante liste di liste, ri
porta true se la matrice contiene almeno una riga i cui elementi siano
 tutti minori di n, false altrimenti. Utilizzare le funzioni List.exists e
 List.for_all. *)

 let verifica_matrice n matrice = List.exists (function x-> List.for_all (function y-> y<n) x) matrice;;

(*  tutte_liste_con: int-> ’a-> ’a -> ’a list list, che, dato un in
tero non negativo n e due valori (dello stesso tipo) x e y, riporta una lista
 contenente tutte le possibili liste di lunghezza n contenenti soltanto i due
 valori x e y.
 Ad esempio, per n=3, x=0 e y=1 si avra‘ la lista seguente (o una sua
 permutazione):
 [[0; 0; 0]; [0; 0; 1]; [0; 1; 0]; [0; 1; 1];
 [1; 0; 0]; [1; 0; 1]; [1; 1; 0]; [1; 1; 1]] *)

 let rec tutte_liste_con n x y =
   if n = 0 then
    [[]]
   else
    (List.map (function z-> x::z) (tutte_liste_con (n-1) x y)) @ (List.map (function z-> y::z) (tutte_liste_con (n-1) x y));;

let rec print_list' lst =
  match lst with
  | [] -> ()
  | x::xs -> Printf.printf "[";
             List.iter (fun el -> Printf.printf "%d; " el) x;
             Printf.printf "] ";
             print_list' xs;;

Printf.printf "\n------------------------------\n";;

let test = tutte_liste_con 3 0 1;;
print_list' test;;

(*  interleave: ’a-> ’a list-> ’a list list, tale che interleave
 x lst riporti una lista con tutte le liste che si ottengono inserendo x in
 qualsiasi posizione in lst. Utilizzare la funzione List.map.
 Ad esempio, interleave 10 [0;1;2] = [[10; 0; 1; 2]; [0; 10; 1;
 2]; [0; 1; 10; 2]; [0; 1; 2; 10]]. *)

 let rec interleave x list = 
    match list with
    [] -> [[x]]
    | y::rest -> (x::list) :: (List.map (function z-> y::z) (interleave x rest));;

  Printf.printf "\n------------------------------\n";;
  Printf.printf "Prova interleave: \n";;
  let print_int_list lst =
   let rec print_elements = function
     | [] -> ()
     | [x] -> Printf.printf "%d" x
     | x :: xs -> Printf.printf "%d; " x; print_elements xs
   in
   Printf.printf "[";
   print_elements lst;
   Printf.printf "]";;
 
 let print_list_of_lists lll =
   Printf.printf "[\n";
   List.iter (fun l -> print_string "  "; print_int_list l; print_string ";\n") lll;
   Printf.printf "]\n";;
 
   let result = interleave 10 [0;1;2];;
   print_list_of_lists result;;

(*  split2: ’a list-> ’a list * ’a list, che suddivide una lista
 in due liste di lunghezza più o meno uguale, utilizzando le funzioni
 take: int-> ’a list-> ’a list, definita a lezione, e drop, del
l’esercizio 1d. Come la funzione split definita a lezione, la split2
 si potrebbe utilizzare per implementare il merge sort.
 A differenza di split, che mette gli elementi in posizione pari nella
 prima lista, quelli in posizione dispari nella seconda, split2 mette
rà i primi elementi da una parte e gli ultimi dall’altra. Ad esempio,
 split2 [1;2;3;4;5;6;7] = ([1;2;3], [4;5;6;7]), mentresplit
 [1;2;3;4;5;6;7] = ([1;3;5;7], [2;4;6]) *)

let take n lista = 
    let rec aux n risultato lista' = 
      if n = 0 then risultato
      else
      match lista' with
      [] -> lista
      | x::rest -> aux (n-1) (risultato@[x]) rest
      in aux n [] lista;;

 Printf.printf "\n------------------------------\n";;
  Printf.printf "Prova take: \n";;
  let test = take 10 [1;2;3;4;5];;
  print_list test;;   

(* drop: int-> ’a list-> ’a list, tale che drop n lst = lista
 che si ottiene da lst togliendone i primi n elementi. Se il numero
 di elementi di lst è minore di n (oppure uguale a n), allora drop n
 lst = []. *)

let drop n list = 
  let rec aux n lista = 
    if n = 0 then lista
    else
    match lista with
    [] -> []
    | x::rest -> aux (n-1) rest
    in aux n list;;

Printf.printf "\n------------------------------\n";;
  Printf.printf "Prova drop: \n";;
  let test = drop 3 [1;2;3;4;5];;
  print_list test;; 

(*  split2: ’a list-> ’a list * ’a list, che suddivide una lista
 in due liste di lunghezza più o meno uguale, utilizzando le funzioni
 take: int-> ’a list-> ’a list, definita a lezione, e drop, del
l’esercizio 1d. Come la funzione split definita a lezione, la split2
 si potrebbe utilizzare per implementare il merge sort.
 A differenza di split, che mette gli elementi in posizione pari nella
 prima lista, quelli in posizione dispari nella seconda, split2 mette
rà i primi elementi da una parte e gli ultimi dall’altra. Ad esempio,
 split2 [1;2;3;4;5;6;7] = ([1;2;3], [4;5;6;7]), mentresplit
 [1;2;3;4;5;6;7] = ([1;3;5;7], [2;4;6]) *)

let rec split2 list = 
  let meta = (List.length list) / 2 in
  ((take meta list), (drop meta list));;

  Printf.printf "\n------------------------------\n";;
  Printf.printf "Prova split2: \n";;

  let print_int_list lst =
    Printf.printf "[";
    List.iter (fun x -> Printf.printf "%d; " x) lst;
    Printf.printf "]"
  
  let test_split2 () =
    let lista = [1;2;3;4;5;6;7] in
    let (a, b) = split2 lista in
    Printf.printf "Input: ";
    print_int_list lista;
    Printf.printf "\nOutput:\n - prima metà: ";
    print_int_list a;
    Printf.printf "\n - seconda metà: ";
    print_int_list b;
    Printf.printf "\n"
  ;;
  
  (* Esegui il test *)
  test_split2 ();;

(* Una funzione min_dei_max: int list list-> int che, data una
 lista di liste di interi, riporti il valore minimo tra i massimi di ciascuna
 lista. Ad esempio, per la lista [[3;100;1;9];[2;10;20];[80;65;4]],
 si otterrà il valore 20. *)
exception EmptyList

let massimo list = 
  match list with
  [] -> raise EmptyList
  | x::rest ->
  let rec aux maxProv list =
    match list with
    [] -> maxProv
    | x::rest -> if x > maxProv then aux x rest
                else
                  aux maxProv rest
                in aux x list;;
let minimo list = 
  match list with
  [] -> raise EmptyList
  | x::rest ->
  let rec aux minProv list =
    match list with
    [] -> minProv
    | x::rest -> if x < minProv then aux x rest
                else
                  aux minProv rest
                in aux x list;;

 let min_dei_max list = 
  let rec aux max list = 
    match list with
    [] -> max
    | x::rest -> aux ((massimo x)::max) rest
  in minimo (aux [] list);;

  Printf.printf "\n------------------------------\n";;
  Printf.printf "Prova min dei max: \n";;
  let () =
  let lista = [[3; 100; 1; 9]; [2; 10; 20]; [80; 65; 4]] in
  let risultato = min_dei_max lista in
  Printf.printf "Il minimo dei massimi è: %d\n" risultato
;;

(* Scrivere una funzione find: ’a-> ’a list-> ’a list * ’a list,che,
 applicata a un elemento x e a una lista L, spezzi L in due parti: la prima
 contiene tutti gli elementi che vanno dall’inizio della lista fino alla prima
 occorrenza di x esclusa; la seconda contiene tutti gli elementi che seguono
 la prima occorrenza di x. La funzione solleverà un’eccezione se L non con
tiene x. Ad esempio, find 3 [1;2;3;4;5;6;3] = ([1;2],[4;5;6;3]). *)

exception NoX
let find x list =
  let rec aux prima list = match list with
    [] -> raise NoX
    | y::rest -> if x=y then (prima, rest)
                else
                aux (prima@[y]) rest
                in aux [] list;;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova find: \n";;
(* Esempio di utilizzo: *)
let () =
  try
    let (before, after) = find 3 [1; 2; 3; 4; 5; 6; 3] in
    Printf.printf "Before: [%s]\n" (String.concat "; " (List.map string_of_int before));
    Printf.printf "After: [%s]\n" (String.concat "; " (List.map string_of_int after))
  with
  | NoX -> Printf.printf "Elemento non trovato nella lista\n"
;;

(* prendi: (’a-> bool)-> ’a list-> ’a * ’a list
 che, applicata a un predicato p e una lista L sollevi un’eccezione se L non
 contiene alcun elemento che soddisfa p, altrimenti restituisca una coppia (x,L′)
 dove x è un elemento di L che soddisfa p e L′ contiene tutti gli altri elementi di
 L, in qualsiasi ordine. Se L contiene più elementi che soddisfano p, dalla lista
 ne verrà rimosso soltanto uno (in altri termini, L′ ha solo un elemento in meno
 di L).
 Ad esempio, il valore di prendi (function x-> x > 10) [3; 20; 7; 11;
 8; 30; 20] può essere (20, [3; 7; 11; 8; 30; 20]).*)
exception NoElementP
 let prendi p list = 
  let rec aux prima list = match list with
  [] -> raise NoElementP
  | x::rest -> if p x then (x, prima@rest)
  else
    aux (prima@[x]) rest
  in aux [] list;;

  Printf.printf "\n------------------------------\n";;
  Printf.printf "Prova prendi: \n";;
  let () =
  try
    let (preso, resto) = prendi (fun x -> x > 10) [3; 20; 7; 11; 8; 30; 20] in
    Printf.printf "Elemento preso: %d\n" preso;
    Printf.printf "Lista restante: [%s]\n"
      (String.concat "; " (List.map string_of_int resto))
  with
  | NoElement -> Printf.printf "Nessun elemento soddisfa il predicato.\n"
;;

(* Una funzione enumera: ’a list-> (int * ’a) list che, appli
cata a una lista lst=[x0;x1;x2;...;xk], riporti la lista di coppie
 [(0,x0);(1,x1);(2,x2);...;(k,xk)]. *)

let enumera list = 
  let rec aux risultato cont list = match list with
  [] -> risultato
  | x::rest -> aux (risultato@[(cont, x)]) (cont+1) rest
in aux [] 0 list;;



(* Funzione di stampa per le coppie (indice, valore) *)
let stampa_enumera list =
  let enumerata = enumera list in
  List.iter (fun (indice, valore) -> Printf.printf "(%d, %s)\n" indice valore) enumerata;;

 Printf.printf "\n------------------------------\n";;
Printf.printf "Prova enumera: \n";;
(* Esempio di utilizzo *)
let () =
  let lista = ["a"; "b"; "c"; "d"] in
  stampa_enumera lista;;

(* duplica: int list-> int list, che raddoppia tutti gli elementi di
 una lista di interi, usando la funzione List.map.
 Ad esempio, duplica [0;1;2;3;4] = [0; 2; 4; 6; 8]. *)

 let duplica list = List.map (function x-> x*2) list;;
 Printf.printf "\n------------------------------\n";;
 Printf.printf "Prova duplica: \n";;
 let list = duplica list;;
 print_list list;;
 
(* (a) Scrivere una funzione find: ’a-> ’a list-> ’a list * ’a list,che,
 applicata a un elemento x e a una lista L, spezzi L in due parti: la prima
 contiene tutti gli elementi che vanno dall’inizio della lista fino alla prima
 occorrenza di x esclusa; la seconda contiene tutti gli elementi che seguono
 la prima occorrenza di x. La funzione solleverà un’eccezione se L non con
tiene x. Ad esempio, find 3 [1;2;3;4;5;6;3] = ([1;2],[4;5;6;3]).*)

let find x list = 
  let rec aux prima list = 
    match list with
    [] -> raise NoX
    | y::rest -> if y=x then (prima, rest)
    else
      aux (prima@[y]) rest
    in aux [] list;;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova find: \n";;
(* Esempio di utilizzo: *)
let () =
  try
    let (before, after) = find 3 [1; 2; 3; 4; 5; 6; 3] in
    Printf.printf "Before: [%s]\n" (String.concat "; " (List.map string_of_int before));
    Printf.printf "After: [%s]\n" (String.concat "; " (List.map string_of_int after))
  with
  | NoX -> Printf.printf "Elemento non trovato nella lista\n"
;;

(*(b) Utilizzando la funzione find definita al punto precedente, definire una fun
zione spezza: ’a-> ’a list-> ’a list * ’a list, che, applicata a
 un elemento x e una lista L riporti una coppia di liste (L1,L2), dove L1
 contiene tutti gli elementi di L che vanno dalla prima alla seconda oc
correnza di x, estremi esclusi, e L2 contiene tutti gli elementi di L che
 seguono la seconda occorrenza di x. La funzione solleverà un’eccezione
 se L non contiene almeno due occorrenze di x. Ad esempio, spezza 3
 [1;2;3;4;5;6;3;7;8;9;3;10] = ([4;5;6],[7;8;9;3;10]). *)

 let spezza x list = 
  try
  let (_ , parteBuona) = find x list in
    let (prima, seconda) = find x parteBuona in
      (prima, seconda)
  with NoX -> raise NoX;;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova spezza: \n";;


(* Esempio di utilizzo: *)
let () =
  try
    let (list1, list2) = spezza 3 [1;2;32;4;5;6;3;7;8;9;3;10] in
    Printf.printf "L1: [%s]\n" (String.concat "; " (List.map string_of_int list1));
    Printf.printf "L2: [%s]\n" (String.concat "; " (List.map string_of_int list2))
  with
  | NoX -> Printf.printf "Elemento non trovato almeno due volte nella lista\n"
;;

(*  definire funzioni pi1, pi2, pi3, pi4 che riportino, rispettivamente, il primo,
 secondo, terzo e quarto elemento di una quadrupla. Qual è il loro tipo? Si
 possono applicare a una quintupla?
 Se dichiariamo:
 let quadrupla =
 (5,(’c’,"antonio",(),if 3>4 then 0 else 1),"pippo",true)
 Qual è il valore dell’espressione pi3 (pi2 quadrupla) e di
 pi4 (pi2 quadrupla) ? *)

 let pi1 quadrupla = match quadrupla with
 (primo, _, _, _) -> primo;;

 let pi2 quadrupla = match quadrupla with
 (_, secondo, _, _) -> secondo;;

 let pi3 quadrupla = match quadrupla with
 (_, _, terzo, _) -> terzo;;

 let pi4 quadrupla = match quadrupla with
 (_, _, _, quarto) -> quarto;;

 let quadrupla = (5, ('c',"antonio",(), if 3>4 then 0 else 1), "pippo", true);;

 let x = pi3 (pi2 quadrupla);;
 let x = pi4 (pi2 quadrupla);;

(* Definire una funzione ultime_cifre: int-> int * int che riporti il
 valore intero delle due ultime cifre di un int. Ad esempio:
 ultime_cifre 245 = (4,5)
 ultime_cifre 5 = (0,5).
 Se il numero è negativo, il segno va ignorato. Ad esempio
 ultime_cifre (-245) = (4,5)
 ultime_cifre (-5) = (0,5).
 La funzione non deve mai sollevare eccezioni, ma riportare sempre una
 coppia di interi.
 Si cerchi di fornire una soluzione semplice, che operi direttamente sul
 numero, anziché passare per la sua rappresentazione come stringa. Si
 ricordi che n/m e’ il risultato della divisione intera, n mod m il modulo, e
 abs n il valore assoluto.*)

 let ultime_cifre x = (abs(x mod 100)/10, abs(x mod 100) mod 10)

 let y = 247;;
 let (primo, secondo) = ultime_cifre y;;
 Printf.printf "\n------------------------------\n";;
 Printf.printf "Prova ultime_cifre: \n";;
 Printf.printf "Le ultime cifre di %d sono: (%d, %d)" y primo secondo;;

(* 2. Una cifra è bella se è 0, 3, 7; un numero è bello se la sua ultima cifra è bella
 e la penultima (se esiste) non lo è. Quindi in particolare le cifre belle sono
 numeri belli. Definire un predicato bello: int-> bool che determini
 se un numero è bello. La funzione non deve mai sollevare eccezioni, ma
 riportare sempre un bool. *)

 let bello x = 
  if x<10 then 
    match x with
    0 | 3 | 7 -> true
    | _ -> false
else
  let (penultima, ultima) = ultime_cifre x in
  match ultima with
  0 | 3 | 7 -> if (penultima=0) || (penultima=3) || (penultima=7) then false
                else true
  | _ -> false;;

let x = bello y;;
 Printf.printf "\n------------------------------\n";;
 Printf.printf "Prova bello: \n";;
 Printf.printf "%d e' un numero bello? %b" y x;;


(* (occorre: int-> bool) Dato un intero n, leggere da tastiera una
 sequenza di interi, separati da Enter e terminata dalla stringa vuota
 (o da una qualsiasi stringa non numerica), e determinare (riportando
 true o false) se n occorre nella sequenza. La funzione non deve mai
 sollevare eccezioni e la sua esecuzione non deve terminare finché non
 viene immessa la stringa vuota (o non numerica). *)

 let occorre n =
  let rec aux occorrenze=
  try
  let x = read_int() in
    if x=n then aux (occorrenze+1) else aux occorrenze
  with _ -> occorrenze
 in aux 0;;

 (* Un predicato nondec: int list-> bool che, applicato a una lista
 lst, riporti true se gli elementi di lst sono in ordine non decrescente,
 false altrimenti.
 Adesempio, nondec [1;2;3;4] = true, enondec [1;2;4;3] = false.*)

 let rec nondec list = 
  match list with
  [] -> true
  | x::y::rest -> (x<=y) && nondec(y::rest)
  | x::rest -> true;;

  Printf.printf "\n------------------------------\n";;
  Printf.printf "Prova nondec: \n";;
  let x = nondec list;;
  print_list list;;
  Printf.printf "e' in ordine crescente? %b" x;;

(*  interleave: ’a-> ’a list-> ’a list list, tale che interleave
 x lst riporti una lista con tutte le liste che si ottengono inserendo x in
 qualsiasi posizione in lst. Utilizzare la funzione List.map.
 Ad esempio, interleave 10 [0;1;2] = [[10; 0; 1; 2]; [0; 10; 1;
 2]; [0; 1; 10; 2]; [0; 1; 2; 10]]. *)

 let rec interleave x list =  
  match list with
  [] -> [[x]]
  | y::rest -> (x::list) :: (List.map (function z-> y::z) (interleave x rest));; 

(*  tutte_liste_con: int-> ’a-> ’a ’a list list, che, dato un in
tero non negativo n e due valori (dello stesso tipo) x e y, riporta una lista
 contenente tutte le possibili liste di lunghezza n contenenti soltanto i due
 valori x e y.
 Ad esempio, per n=3, x=0 e y=1 si avra‘ la lista seguente (o una sua
 permutazione):
 [[0; 0; 0]; [0; 0; 1]; [0; 1; 0]; [0; 1; 1];
 [1; 0; 0]; [1; 0; 1]; [1; 1; 0]; [1; 1; 1]] *)


let rec tutte_liste_con n x y =
  match n with
  0 -> [[]]
  | _ ->
  let sottoliste = tutte_liste_con (n-1) x y in
  (List.map (function z-> x::z) sottoliste) @ (List.map (function z-> y::z) sottoliste);;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova tutte_liste_con: \n";;
let test = tutte_liste_con 3 0 1;;
print_list' test;;

(* (Dall’esame di Febbraio 2010) Scrivere una funzione
 prendi: (’a-> bool)-> ’a list-> ’a * ’a list
 che, applicata a un predicato p e una lista L sollevi un’eccezione se L non
 contiene alcun elemento che soddisfa p, altrimenti restituisca una coppia (x,L′)
 dove x è un elemento di L che soddisfa p e L′ contiene tutti gli altri elementi di
 L, in qualsiasi ordine. Se L contiene più elementi che soddisfano p, dalla lista
 ne verrà rimosso soltanto uno (in altri termini, L′ ha solo un elemento in meno
 di L).
 Ad esempio, il valore di prendi (function x-> x > 10) [3; 20; 7; 11;
 8; 30; 20] può essere (20, [3; 7; 11; 8; 30; 20]). *)

 let prendi p list =
  let rec aux risultato list =
    match list with
    [] -> raise NoElementP
    | x::rest -> if p x then (x, risultato@rest) else
                  aux (risultato@[x]) rest
    in aux [] list;;

  Printf.printf "\n------------------------------\n";;
  Printf.printf "Prova prendi: \n";;
  let () =
  try
    let (preso, resto) = prendi (fun x -> x > 10) [3; 20; 7; 11; 8; 30; 20] in
    Printf.printf "Elemento preso: %d\n" preso;
    Printf.printf "Lista restante: [%s]\n"
      (String.concat "; " (List.map string_of_int resto))
  with
  | NoElementP -> Printf.printf "Nessun elemento soddisfa il predicato.\n"
;;

(* Una lista con tutti i segmenti iniziali di list *)

let rec inits list = 
  match list with
  [] -> []
  | [x] -> [[x]]
  | x::rest-> [x] :: (List.map (function y-> x::y) (inits rest));;

  Printf.printf "\n------------------------------\n";;
  Printf.printf "Prova inits: \n";;

  List.iter (fun l -> 
    Printf.printf "[%s]\n" (String.concat ";" (List.map string_of_int l))
  ) (inits [1;2;3]);;

(* dato un dizionario, rappresentato dalla lista associativa
 assoc_list, e una chiave k, riportare il valore associato a k in
 assoc_list, se esiste, un errore altrimenti. *)

 let rec assoc assoc_list k =
  match assoc_list with
  [] -> raise NoElement
  | (k',v)::rest -> if k=k' then v else assoc rest k;;

(* data una chiave k, un valore v e una lista associativa
 assoc_list, riportare la lista che si ottiene inserendo la coppia
 (k,v) in assoc_list– sostituendo (o sovrascrivendo) l’eventuale
 elemento già esistente con chiave k *)

 let inserisci k v assoc_list = (k,v)::assoc_list;;

 (* data una chiave k e una lista associativa assoc_list, riportare
 la lista che si ottiene cancellando da assoc_list tutte le coppie
 con chiave k (potrebbero essercene diverse). Se non c’è
 nessuna coppia con chiave k, riportare assoc_list stessa *)

 let rec cancella k assoc_list = 
  match assoc_list with
  [] -> assoc_list
  | (k1, v)::rest -> if k1=k then cancella k rest else (k1,v)::(cancella k rest);;

  Printf.printf "\n------------------------------\n";;
  Printf.printf "Prova cancella: \n";;
  let rec stampa_assoc_list lst =
    match lst with
    [] -> print_string "[]\n"
    | (k, v)::rest ->
        Printf.printf "(%d, %s) " k v;
        stampa_assoc_list rest;;
  
let assoc_list = [(1, "a"); (2, "b"); (3, "c"); (2, "d"); (4, "e")];;

let nuova_lista = cancella 2 assoc_list;;

stampa_assoc_list nuova_lista;;

 let cancella k assoc_list =
  let rec aux risultato assoc_list = 
    match assoc_list with
    [] -> risultato
    | (k1, v)::rest -> if k1=k then aux risultato rest
    else
      aux (risultato@[(k1,v)]) rest
    in aux [] assoc_list;;
  
(*mem: applicata a un elemento x e la rappresentazione di un insieme
 S, determina se x S (è un predicato).*)
 let mem x s = List.exists (function y-> y=x) s;;

 (*union: applicata a due liste che rappresentano insiemi S1 e S2, riporta
 una rappresentazione di S1 U S2.*)
 let rec union s1 s2 = 
  match s1 with
  [] -> s2
  | x::rest -> if mem x s2 then union rest s2 else union rest s2@[x];;

(* intersect: applicata a due liste che rappresentano insiemi S1 e S2, riporta
 una rappresentazione di S1 intersezione S2.*)
 let intersect s1 s2 = List.filter (function x-> mem x s2) s1;; 

 let intersect s1 s2 = 
  let rec aux risultato s1 =
  match s1 with
  [] -> risultato
  | x::rest-> if mem x s2 then aux (risultato@[x]) rest else aux risultato rest
  in aux [] s1;;

  (*setdiff: applicata a due liste che rappresentano insiemi S1 e S2, riporta
 una rappresentazione di S1 S2.*)

 let setdiff s1 s2 = List.filter (function x-> not(mem x s2)) s1;;

 (* Problema: data una lista lst di tipo ’a list, determinare il numero di elementi di
 lst (la lunghezza della lista). *)
 let rec length list = 
  match list with
  [] -> 0
  | x::rest -> 1+ length rest;;

  Printf.printf "\n------------------------------\n";;
  Printf.printf "Prova length: \n";;
  let x = length list;;
  Printf.printf "(%d)\n" x;;

  (* 1a Costruisco la lista che va da [1;2;...;higher] *)
  let upTo n m = 
    let rec aux risultato n = 
    if n > m then risultato else aux (risultato@[n]) (n+1) 
    in aux [] n;;


Printf.printf "\n------------------------------\n";;
Printf.printf "Prova upTo: \n";;
let lista_upto = upTo 1 90;;
print_list lista_upto;;

(* 1.1a Rigiro la lista *)
let rev list =
  let rec aux risultato list = 
    match list with
    [] -> risultato
    | x::rest -> aux (x::risultato) rest
  in aux [] list;;



Printf.printf "\n------------------------------\n";;
Printf.printf "Prova rev: \n";;
let lista_rev = rev lista_upto;;
print_list lista_rev;;

(* 1b appiattisco la lista estrazioni (Lista contenente lista di interi int list list) trasformandola in una lista di interi*)
(* NOTA: non uso l'operatore concatenazione perchè giro la lista con la funzione reverse *)
let flatten estrazioni = 
  let rec aux risultato estrazioni =
  match estrazioni with
  [] -> risultato
  | x::rest -> aux (risultato@x) rest
  in aux [] estrazioni;;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova flatten: \n";;
let lista_di_liste = [[1;2]; [3;4]; [5]];;
let lista_flatten = flatten lista_di_liste;;
print_list lista_flatten;;

(* 1c contare le occorrenze di ciascun elemento nella lista flatten estrazioni *)
(* 1.1c contare le occorrenze di un elemento in una lista *)

let rec conta n list =
  match list with
  [] -> 0
  | x::rest -> if x=n then 1 + (conta n rest) else conta n rest;;


Printf.printf "\n------------------------------\n";;
Printf.printf "Prova conta: \n";;
let conta_esempio = conta 3 [1;3;3;2;3;4];;
Printf.printf "Il numero 3 compare %d volte.\n" conta_esempio;;

let contatutti elementi listona = 
  let rec aux risultato elementi = 
    match elementi with
    [] -> risultato
    | x::rest -> aux ((x, conta x listona)::risultato) rest
  in aux [] elementi;;

let contatutti elementi listona = 
  match elementi with
  [] -> []
  | x::rest -> ((x, conta x listona):: contatutti rest listona);;

(* Funzione per stampare una lista di coppie *)
let rec stampa_lista = function
  | [] -> ()
  | (x, n) :: rest ->
      Printf.printf "(%d, %d)\n" x n;
      stampa_lista rest;;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova contatutti: \n";;
let elementi_esempio = [1;2;3];;
let listona_esempio = [1;2;2;3;3;3;4;3;1;3;3;3;3;2];;
let lista_contatutti = contatutti elementi_esempio listona_esempio;;
stampa_lista lista_contatutti;;

(* 2 Ordinare la lista di coppie secondo valori non decrescenti del secondo elemento *)
(* 2.1 Scrivere una funzione di ordinamento *)
let comp (v1, n1) (v2, n2) =
  if n1 < n2 then -1
  else if n1 = n2 then 0
  else
    1;;

let sort listaCoppie = List.sort comp listaCoppie;;


Printf.printf "\n------------------------------\n";;
Printf.printf "Prova sort: \n";;
let lista_sortata = sort lista_contatutti;;
stampa_lista lista_sortata;;

(*(* 3 Prendere le prime dim coppie della lista ordinata *)*)
let rec take n list = 
  if n = 0 then []
  else
    match list with
    [] -> []
    | x::rest -> x:: (take (n-1) rest);;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova take: \n";;
let lista_take = take 4 lista_sortata;;
stampa_lista lista_take;;

(* 4 dalla lista di coppie ottenuta, estrarre la lista con solo i primi elementi di ciascuna coppia *)
let rec primi listaCoppie = 
  match listaCoppie with
  [] -> []
  | (v1, n1)::rest -> v1:: (primi rest);;


Printf.printf "\n------------------------------\n";;
Printf.printf "Prova primi: \n";;
let lista_primi = primi lista_take;;
print_list lista_primi;;

let super estrazioni dim higher = primi (take dim (sort (contatutti (upTo 1 higher) (flatten estrazioni))));;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova super: \n";;
let estrazioni = [[1;2;3]; [1;2]; [3;4;4]; [1;5;6;6]];;
let super_vincente = super estrazioni 2 6;;
print_list super_vincente;;

let rec sum f lower upper = 
  if lower > upper then 0
  else f lower + sum f (lower+1)  upper;;

let times = function n-> function m -> n*m;;

let x = sum (times 5) 1 10;;

Printf.printf "\n------------------------------\n";;
Printf.printf "Prova sum: \n";;
Printf.printf "%d" x;;

let exists p lista = if (List.find (p) lista)=[] then false else true;;