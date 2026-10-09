type 'a graph = ('a * 'a) list;;

(* Restituire una lista con i successori di un nodo, dato il nodo e il grafo*)
exception NodoNonAppartenente

let rec successori x grafo =
  if not (List.exists (function (y,z)-> x=y || x=z) grafo) then raise NodoNonAppartenente else
  List.map (snd) (List.filter (function (y, _) -> x=y) grafo);;

(* Stessa definizione ma per grafi non orientati *)
let rec vicini x grafo =
  if not (List.exists (function (y,z)-> x=y || x=z) grafo) then raise NodoNonAppartenente else
  List.map (function (y,z)-> if x=y then z else y) (List.filter (function (y, z) -> x=y || x=z) grafo);;

(* Visita in profondità di un grafo *)

let rec depth_search start grafo =
  let rec aux visitati pendenti =
    match pendenti with
    [] -> List.rev visitati
    | x::rest -> if (List.mem x visitati) then aux visitati rest
                                         else aux (x::visitati) ((successori x grafo)@rest) 
  in aux [] [start];;

  let graph = [
  (1, 2); (1, 3); (1,4);
  (2, 6); 
  (3, 5);
  (6, 7);(6, 5);
  (5, 4);
  (4, 6); 
];;

let print_int_list lst =
  let rec aux = function
    | [] -> print_newline ()
    | [x] -> print_int x; print_newline ()
    | x :: xs -> print_int x; print_string " "; aux xs
  in
  aux lst;;

  let x = depth_search 1 graph;;

  Printf.printf "\n=======================\n";;
  Printf.printf "Visita in profondita del grafo: ";;
  print_int_list x;;

  (* Visita in ampiezza di un grafo *)

  let rec breadth_search start grafo =
  let rec aux visitati pendenti =
    match pendenti with
    [] -> List.rev visitati
    | x::rest -> if (List.mem x visitati) then aux visitati rest
                                         else aux (x::visitati) (rest@(successori x grafo)) 
  in aux [] [start];;

  let x = breadth_search 1 graph;;

  Printf.printf "\n=======================\n";;
  Printf.printf "Visita in ampiezza del grafo: ";;
  print_int_list x;;

  (* Scrivere una funzione test_connessi: ’a graph -> ’a -> ’a -> bool
che, dato un grafo orientato G e due nodi N e M, determini se esiste un
cammino da N a M. La funzione riporterà un booleano (non un cammino) *)

let test_connessi grafo nodo1 nodo2 =
  let rec aux visitati pendenti = 
    match pendenti with
    [] -> false
    | x::rest -> if x=nodo2 then true else 
                  if (List.mem x visitati) then aux visitati rest
                                         else aux (x::visitati) ((successori x grafo)@rest) 
  in aux [] [nodo1];;

  let x = test_connessi graph 1 6;;

  Printf.printf "\n=======================\n";;
  Printf.printf "I nodi 1 e 6 sono connessi: ";;
  Printf.printf "%b\n" x;;

  (* Scrivere una funzione esiste_ciclo: ’a graph -> ’a -> bool che, dato un grafo orientato G e un nodo N, determini se esiste un ciclo su N (cioè
un cammino da N a N che contenga almeno un arco). La funzione riporterà
un booleano (non un cammino). *)

  let esiste_ciclo grafo nodo =
  let rec aux visitati pendenti = 
    match pendenti with
    [] -> false
    | x::rest -> if x=nodo then true else 
                  if (List.mem x visitati) then aux visitati rest
                                         else aux (x::visitati) ((successori x grafo)@rest) 
  in aux [nodo] (successori nodo grafo);;

  let x = esiste_ciclo graph 6;;

  Printf.printf "\n=======================\n";;
  Printf.printf "Esiste un ciclo su 6: ";;
  Printf.printf "%b\n" x;;

  (* Scrivere una funzione ciclo: ’a graph -> ’a -> ’a list che, dato un
grafo orientato G e un nodo N, riporti, se esiste, un ciclo su N, altrimenti
sollevi un’eccezione (in questo caso la funzione deve riportare una lista di
nodi). *)

exception NoCiclo;;

let rec ciclo grafo nodo =
  let rec aux visitati pendenti = 
    match pendenti with
    [] -> raise NoCiclo
    | x::rest -> try
                    if x=nodo then List.rev (x::visitati) else
                    if List.mem x visitati then aux visitati rest else
                    let succ = (successori x grafo) in
                    if succ=[] then
                    raise NoCiclo
                    else aux (x::visitati) (succ)
                  with _ -> aux visitati rest
                  in aux [nodo] (successori nodo grafo);;
  
  let x = ciclo graph 6;;

  Printf.printf "\n=======================\n";;
  Printf.printf "Ciclo in 6: ";;
  print_int_list x;;

  (* Un grafo non orientato è connesso se, per ogni coppia di nodi distinti
N e M, esiste un cammino da N a M. Si definisca un tipo di dati ’a
graph (diverso da quello dato all’inizio di questo gruppo di esercizi) per la
rappresentazione di grafi mediante due componenti: lista di nodi e lista di
archi, e scrivere una funzione grafo_connesso: ’a graph -> bool che,
dato un grafo non orientato G, determini se G è connesso.
Si noti che, per controllare se un grafo non orientato con nodi [n1;n2;....nk]
è connesso, basta controllare se n1 è connesso a n2, poi se n2 è connesso
a n3 (quindi n1 sarà connesso anche a n3), poi se n3 è connesso a n4, ecc.
Oppure, equivalentemente, si può controllare se n1 è connesso a n2, a n3,
a n4,... e basta. *)

type 'a grafo = (('a) list * ('a * 'a) list);;

let grafo_connesso grafo = 
  let rec aux (nodoList, archiList) =
  match nodoList with
  [] -> true
  | x::y::rest -> if (List.exists (function (arcoP,arcoA)-> if (arcoP=x && arcoA=y) || (arcoP=y && arcoA=x) then true else false) archiList) then aux ((y::rest), archiList) else false
  | x::rest -> true
  in aux grafo;;

  (* Esempio di grafo connesso *)
let grafo1 : string grafo = 
  (["A"; "B"; "C"; "D"],
   [("A", "B"); ("B", "C"); ("C", "D")]);;

(* Esempio di grafo non connesso *)
let grafo2 : string grafo = 
  (["A"; "B"; "C"; "D"],
   [("A", "B"); ("C", "D")]);;

(* Output atteso *)
let () =
  Printf.printf "\n=======================\n";;
  Printf.printf "Test grafo connesso: \n";; 
  Printf.printf "grafo1 connesso? %b\n" (grafo_connesso grafo1);
  Printf.printf "grafo2 connesso? %b\n" (grafo_connesso grafo2);;

  (* Si consideri il problema dei missionari e cannibali: tre missionari e tre cannibali
sono sulla riva di un fiume e devono attraversarlo. Per farlo devono utilizzare
una barca che non può trasportare più di due persone alla volta. Durante i
trasferimenti, su nessuna delle due rive i cannibali devono essere in numero
maggiore dei missionari (altrimenti ...).
Per rappresentare le possibili situazioni dichiariamo i tipi seguenti:*)
type obj = Miss | Cann | Barca
type situazione = obj list * obj list
(*Una situazione è rappresentata da due liste di oggetti: quelli che si trovano
sulla riva sinistra e quelli che si trovano sulla riva destra. Se inizialmente i
tre missionari, i tre cannibali e la barca si trovano sulla riva sinistra, possiamo
dichiarare:*)
let initial = ([Miss;Miss;Miss;Cann;Cann;Cann;Barca], [Cann])
(*Le azioni consistono nello spostamento della barca con al massimo due persone
da una delle due rive all’altra. Quindi possiamo dichiarare:*)
type azione =
From_left of obj list
| From_right of obj list
(*Stabiliamo che la lista di oggetti cui si applicano i costruttori non include la
barca (che per forza si deve spostare), ma soltanto gli uomini che si spostano.
(a) Definire una funzione safe: situazione -> bool, che determina se una
situazione e’ sicura (nessun missionario viene mangiato).*)
let count obj riva = (List.length (List.filter (function x-> x=obj) riva));;
let safe (rivaSinistra, rivaDestra) = 
  let ms = count Miss rivaSinistra in
  let md = count Miss rivaDestra in
  let cs = count Cann rivaSinistra in
  let cd = count Cann rivaDestra in
  (ms = 0 || ms >= cs) && (md = 0 || md >= cd) ;;
let x = safe initial;;
Printf.printf "%b" x;;
(*(b) Definire una funzione applica: azione -> situazione -> situazione,
che, applicata a un’azione act e una situazione sit, riporta la situazione
che si ottiene applicando l’azione act a sit. La funzione deve sollevare
un’eccezione se l’azione non è applicabile (ad esempio perché la barca non
si trova sulla riva giusta, o se la riva “sorgente” dello spostamento non
contiene il numero sufficiente di missionari o cannibali che si dovrebbero
spostare), oppure se la situazione risultante non è sicura.*)
exception AzioneNonApplicabile
exception RivaVuota

let rec rimuoviUno oggetto riva = 
  let rec aux risultato riva =
    match riva with
    [] -> risultato
    | x::rest -> if x=oggetto then risultato@rest else aux (x::risultato) rest
  in aux [] riva;;

let rec rimuoviLista list riva =
  match list with
  [] -> riva
  | x::rest -> rimuoviLista rest (rimuoviUno x riva);;

let rec scorriSinistra list (rivaSinistra, rivaDestra) =
  if List.length list > 2 then raise AzioneNonApplicabile else
  match list with
  [] -> (rivaSinistra, rivaDestra)
  | Miss::[] ->  if (safe ((rimuoviLista (Miss::Barca::[]) rivaSinistra),(Miss::Barca::rivaDestra))) && (count Miss rivaSinistra)>=1 && (count Barca rivaSinistra)=1 then ((rimuoviLista (Miss::[]) rivaSinistra), (Miss::Barca::rivaDestra)) else raise AzioneNonApplicabile
  | Cann::[] ->  if (safe ((rimuoviLista (Cann::Barca::[]) rivaSinistra),(Cann::Barca::rivaDestra))) && (count Cann rivaSinistra)>=1 && (count Barca rivaSinistra)=1 then ((rimuoviLista (Cann::[]) rivaSinistra), (Cann::Barca::rivaDestra)) else raise AzioneNonApplicabile
  | Miss::Cann::[] | Cann::Miss::[] -> if (safe ((rimuoviLista (Miss::Cann::Barca::[]) rivaSinistra),(Miss::Cann::Barca::rivaDestra))) && (count Miss rivaSinistra)>=1 && (count Cann rivaSinistra)>=1 && (count Barca rivaSinistra)=1 then ((rimuoviLista (Miss::Cann::[]) rivaSinistra), (Miss::Cann::Barca::rivaDestra)) else raise AzioneNonApplicabile
  | _ -> raise AzioneNonApplicabile;;

let rec scorriDestra list (rivaSinistra, rivaDestra) =
  if List.length list > 2 then raise AzioneNonApplicabile else
  match list with
  [] -> (rivaSinistra, rivaDestra)
  | Miss::[] -> if (safe ((Miss::Barca::rivaSinistra),(rimuoviLista (Miss::Barca::[]) rivaDestra))) && (count Miss rivaDestra)>=1 && (count Barca rivaDestra)=1 then ((Miss::Barca::rivaSinistra),(rimuoviLista (Miss::Barca::[]) rivaDestra)) else raise AzioneNonApplicabile
  | Cann::[] -> if (safe ((Cann::Barca::rivaSinistra),(rimuoviLista (Cann::Barca::[]) rivaDestra))) && (count Cann rivaDestra)>=1 && (count Barca rivaDestra)=1 then ((Cann::Barca::rivaSinistra),(rimuoviLista (Cann::Barca::[]) rivaDestra)) else raise AzioneNonApplicabile
  | Miss::Cann::[] | Cann::Miss::[] -> if (safe ((Miss::Cann::Barca::rivaSinistra),(rimuoviLista (Miss::Cann::Barca::[]) rivaDestra))) && (count Miss rivaDestra)>=1 && (count Cann rivaDestra)>=1 && (count Barca rivaDestra)=1 then ((Miss::Cann::Barca::rivaSinistra),(rimuoviLista (Miss::Cann::Barca::[]) rivaDestra)) else raise AzioneNonApplicabile
  | _ -> raise AzioneNonApplicabile;;

let rec applica azione sit =
  match azione with
  From_left [] -> sit
  | From_right [] -> sit
  | From_left list ->  scorriSinistra list sit
  | From_right list -> scorriDestra list sit;;
  
(*(c) Si definisca come segue la lista di tutte le azioni possibili:*)
let actions =
let elems =
[[Miss];[Cann];[Miss;Cann];[Miss;Miss];[Cann;Cann]]
in (List.map (function x -> From_left x) elems)
@ (List.map (function x -> From_right x) elems);;
(*Definire una funzione from_sit: situazione -> situazione list, che,
applicata a una situazione sit generi tutte le situazioni che si possono
ottenere applicando un’azione possibile a sit.*)

let rec from_sit sit = 
  let rec aux situazioni actions sit =
    match actions with
    [] -> situazioni
    | x::rest -> try
                  aux ((applica x sit)::situazioni) rest sit
                with _ -> aux situazioni rest sit
  in aux [] actions sit;;

(* Si consideri il problema dei missionari e cannibali dell’esercizio 4 del Gruppo 7. La soluzione al problema si può ridurre a un problema di ricerca
di un cammino a partire dal nodo che rappresenta la situazione iniziale
(initial) nel grafo la cui funzione “successori” è la funzione from_sit.
Nella ricerca, il test “nodo già visitato” non dovrebbe utilizzare List.mem,
in quanto l’ordine degli elementi nelle due liste di una situazione deve essere ignorato: ad esempio, ([Miss;Cann;Barca],[Cann;Cann;Miss;Miss])
e ([Cann;Barca;Miss],[Miss;Cann;Miss;Cann]) rappresentano la stessa situazione.
Con tali modifiche, risolvere il problema dei missionari e cannibali, scrivendo una funzione goal: situazione -> bool, che determina se una situazione è l’obiettivo, e una funzione miss_cann: unit -> situazione
list che riporti una lista delle situazioni rappresentante una possibile
soluzione al problema.
Se si vuole invece avere come risultato non una lista di situazioni, ma
una lista di azioni, occorre modificare la funzione from_sit, in modo che,
applicata a una situazione sit, riporti una (azione * situazione) list
(tutte le coppie (a,s) dove a è un’azione applicabile a sit e s la situazione
che ne risulta). Con questa modifica, adattare il codice di miss_cann in
modo che venga riportata una azione list. Suggerimento: trattare a
parte la situazione iniziale, e far sì che la funzione ausiliaria per la ricerca
a partire da una singola situazione abbia come argomento (oltre alla lista
delle situazioni già “visitate”) una coppia (act,sit), di tipo azione *
situazione, dove act è l’azione che ha portato alla situazione sit.
*)

  

