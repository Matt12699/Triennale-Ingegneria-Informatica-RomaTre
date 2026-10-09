(* problema: data la posizione (punto del piano (x, y) e una direzione) dell'oggetto e un'azione di spostamento o di cambiamento di direzione, 
  calcolare la nuova posizione dell'oggetto*)

  type direzione = Su | Giu | Destra | Sinistra;;

  type posizione = int * int * direzione;;

  type azione = Gira | Avanti of int;;

  let int_of_act a =
    match a with
    Avanti n -> n
    | _ -> failwith "int_of_act";;
    
  (* Sottoproblema: data una direzione, riportare la direzione che si ottiene girando di 90 in senso orario *)
  let gira d =
    match d with
    Su-> Destra
    | Giu -> Sinistra
    | Destra -> Giu
    | Sinistra -> Su;;

  (* Sottoproblema: data una posizione (x,y,d) e un intero n, riportare la posizione (x', y', d) che si ottiene andando avanti di n passi nella direzione indicata da d*)
  let avanti (x,y,d) n =
    match d with
    Su -> (x, y+n, d)
    | Giu -> (x, y-n, d)
    | Destra -> (x-n, y, d)
    | Sinistra -> (x+n, y, d);;

  let sposta (x,y,d) act = 
    match act with
    Gira -> (x, y, (gira d))
    | Avanti n -> avanti (x,y,d) n;;

  type expr =
    Int of int
    | Var of string
    | Sum of expr * expr
    | Diff of expr * expr
    | Mult of expr * expr
    | Div of expr * expr;;
  
  type ambiente = (string*int) list;;

  (* eval env e = valore dell'espressione e nell'ambiente env. Errore se qualche variabile in e non ha un valore associato in env*)
  let rec eval env e =
    match e with
    Int n -> n
    | Var n -> List.assoc n env
    | Sum (e1,e2) -> (eval env e1) + (eval env e2)
    | Diff (e1,e2) -> (eval env e1) - (eval env e2)
    | Mult (e1, e2) -> (eval env e1) * (eval env e2)
    | Div (e1, e2) -> (eval env e1) / (eval env e2);;
    
  type 'a tree = Empty 
                | Tr of 'a * 'a tree * 'a tree;;

  let is_empty t = 
    match t with
    Empty -> true
    | _ -> false;;

  exception EmptyTree
  let root t =
    match t with
    Empty -> raise EmptyTree
    | Tr (x, _ , _) -> x;;

  let is_leaf t = 
    match t with
    (_ , Empty, Empty) -> true
    | _ -> false;;
  
  let leaf x = Tr(x, Empty, Empty);;

  (* Inizio Esercizi 7*)

  (*
Utilizzando la funzione sposta, definire una funzione
 esegui: posizione-> azione list-> posizione
 che, applicata a una posizione e una lista di azioni [a1,a2,...,an] riporti la
 posizione in cui si trova l’oggetto che, trovandosi inizialmente nella posizione
 data, esegue in sequenza (e in quest’ordine) le azioni a1,a2,...,an.
 Ad esempio si deve avere:
 # let p = (0,0,Su);;
 val p : int * int * direzione = (0, 0, Su)
 # esegui p [Avanti 3; Gira];;- : int * int * direzione = (0, 3, Destra)
 # esegui p [Avanti 3; Gira; Avanti 2];;- : int * int * direzione = (2, 3, Destra)
 # esegui p [Avanti 2; Gira; Avanti 3];;- : int * int * direzione = (3, 2, Destra)
 # esegui p [Avanti 2; Gira; Gira; Gira; Avanti 3];;- : int * int * direzione = (-3, 2, Sinistra)*)

 let esegui pos act = List.fold_left (sposta) pos act;;

 (* 2. Definire il prodotto sul tipo nat così definito
 type nat = Zero | Succ of nat
 usando la funzione somma definita a lezione:
 (* somma : nat-> nat-> nat *)
 let rec somma n m =
 match n with
 Zero-> m
 | Succ k-> Succ(somma k m) *)
 type nat = Zero | Succ of nat;;

 let rec somma n m =
  match n with
  Zero -> m
  | Succ k -> Succ(somma k m);;

 let rec prod n m =
  match m with
  Zero -> Zero
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

(* 3. 
(Dal compito d’esame di giugno 2010) Le cassaforti della marca VeryHard hanno
 un sistema di apertura alquanto singolare: ciascuna di esse ha un numero N di
 chiavi, disposte in sequenza, ciascuna delle quali può essere in posizione aperta
 o chiusa. Solo quando tutte le chiavi sono aperte, la cassaforte si apre. Le
 chiavi devono essere girate una alla volta (passando così dalla posizione aperta
 a chiusa o viceversa), ma data una certa configurazione delle chiavi, non è
 possibile girare una chiave qualsiasi, ma soltanto la prima chiave (iniziando
 da sinistra), oppure la chiave che segue immediatamente la prima chi
ave chiusa (iniziando sempre da sinistra). Ad esempio, data la configurazione
 di chiavi seguente (dove C indica che la chiave è chiusa, A che è aperta):
 A AACCAC è possibile girare la prima chiave, passando alla configurazione
 C AACCAC, oppure la quinta (infatti la prima chiave chiusa è la quarta),
 passando a A A A C A A C. Se la prima chiave chiusa della configurazione è
 l’ultima (come ad esempio in A A C), si può soltanto girare la prima chiave.
 Date le seguenti dichiarazioni di tipo
 type chiave = Aperta | Chiusa
 type cassaforte = chiave list*)

 type chiave = Aperta | Chiusa;;
 type cassaforte = chiave list;;

 (*(cassaforte rappresenta una configurazione delle chiavi), definire le seguenti fun
zioni:
 (a) giraPrima: cassaforte-> cassaforte, che riporta la configurazione
 che si ottiene girando la prima chiave;*)

 let giraPrima c = 
  match c with
  [] -> c
  | x::rest -> match x with
              Aperta -> Chiusa::rest
              | Chiusa -> Aperta::rest;;

  let stampa_chiave = function
  | Aperta -> Printf.printf "A"
  | Chiusa -> Printf.printf "C"

let stampa_cassaforte cass =
  List.iter stampa_chiave cass;
  Printf.printf "\n"

let () =
  Printf.printf "\n=============================\n";
    Printf.printf "Test della funzione giraPrima:\n";
  let c = [Chiusa; Chiusa; Chiusa; Aperta] in
  Printf.printf "Stato iniziale: ";
  stampa_cassaforte c;
  let c2 = giraPrima c in
  Printf.printf "Dopo giraPrima: ";
  stampa_cassaforte c2



 (*(b) giraDopoChiusa: cassaforte-> cassaforte, che riporta la configurazione
 che si ottiene girando la chiave che segue la prima chiusa (e solleva un’eccezione
 se non è possibile eseguire l’operazione).*)

exception NoMoreKeys

 let rec giraDopoChiusa c = 
  let rec aux prima c = 
  match c with
  [] -> raise NoMoreKeys
  | x::y::rest -> if x=Chiusa then (prima@[x]) @ (giraPrima (y::rest))
                  else
                  aux (prima@[x]) (y::rest)
  | x::rest -> raise NoMoreKeys
                  in aux [] c;;

  let () =
    Printf.printf "\n=============================\n";
    Printf.printf "Test della funzione giraDopoChiusa:\n";

  let c1 = [Aperta; Aperta; Chiusa; Chiusa; Aperta; Chiusa] in
  Printf.printf "Stato iniziale: ";
  stampa_cassaforte c1;

  let c2 = giraDopoChiusa c1 in
  Printf.printf "Dopo giraDopoChiusa: ";
  stampa_cassaforte c2;

  (* Caso limite in cui l’unica chiave chiusa è l’ultima *)
  let c3 = [Aperta; Aperta; Chiusa] in
  Printf.printf "Stato iniziale: ";
  stampa_cassaforte c3;
  try
    let c4 = giraDopoChiusa c3 in
    Printf.printf "Dopo giraDopoChiusa: ";
    stampa_cassaforte c4
  with
    NoMoreKeys ->
      Printf.printf "Eccezione sollevata: Nessuna chiave successiva\n"



(* (c) successori: cassaforte-> cassaforte list, che, applicata a una 
 configurazione clist, riporta la lista con le configurazioni (una o due) che si
 possono ottentere da clist con una delle due operazioni (giraPrima e
 giraDopoChiusa; se giraDopoChiusa non è applicabile, la lista conterrà
 un solo elemento). *)

 let successori c = 
  try
    [(giraPrima c)] @ [(giraDopoChiusa c)]
  with NoMoreKeys -> [(giraPrima c)];; 

  let string_of_chiave = function
  | Aperta -> "A"
  | Chiusa -> "C";;

  Printf.printf "\n=============================\n";;
  Printf.printf "Test della funzione successori:\n";;
let print_cassaforte c =
  let s = List.map string_of_chiave c |> String.concat "" in
  Printf.printf "%s\n" s;;

  let test_successori () =
  let c1 = [Aperta; Aperta; Chiusa; Chiusa; Aperta; Chiusa] in
  Printf.printf "Cassaforte iniziale:\n";
  print_cassaforte c1;

  let succ = successori c1 in
  Printf.printf "Successori:\n";
  List.iter print_cassaforte succ;;

test_successori ();;


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


