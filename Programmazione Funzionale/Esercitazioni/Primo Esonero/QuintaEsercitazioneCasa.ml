(* Funzione per stampare una lista di interi *)
let print_list lst =
  Printf.printf "[";
  List.iter (fun x -> Printf.printf "%d; " x) lst;
  Printf.printf "]\n";;

(* find: (’a-> bool)-> ’a list-> ’a, tale che find p lst riporti
 il primo elemento di lst che soddisfa il predicato p. La funzione soll
eva un’eccezione se nessun elemento della lista soddisfa p. (Notare che il
 modulo List contiene una funzione con questo nome, ma qui si chiede di
 ridefinirla per esercizio).
 Utilizzare find per definire una funzione find_applicata: int list->
 int che, applicata a una lista di interi, riporti il primo elemento della lista
 il cui quadrato sia minore di 30.*)

exception NotFound
 let rec find p lst = 
  match lst with
  [] -> raise NotFound
  | x::rest -> if p x then x
                else
                find p rest;;

let pari x = x mod 2 = 0;;
let list = [0;2;4;6;7;8;9;10];;
print_list list;;
Printf.printf "\n--------------------------------\n";;
let list2 = [6;7;8];;
print_list list2;;
let x = find pari list;;
Printf.printf "\n--------------------------------\n";;
Printf.printf "Il primo elemento pari e': %d" x;;

(*takewhile: (’a-> bool)-> ’a list-> ’a list,tale che takewhile
 p lst riporti la più lunga parte iniziale di lst costituita tutta da elementi
 che soddisfano il predicato p. Gli elementi del risultato devono occorrere
 nello stesso ordine in cui occorrono nell’argomento.
 Adesempio, takewhile (function n-> n mod 2 = 0) [0;2;4;6;7;8;
 9;10] = [0; 2; 4; 6].*)

 let takewhile p list = 
  let rec aux risultato list = 
    match list with
    [] -> risultato
    | x::y::rest -> if p x && not (p y) then risultato@[x]
                    else
                        if p x then aux (risultato@[x]@[y]) rest
      else
        risultato
    | x::[] -> if p x then risultato@[x]
    else
      risultato
    in aux [] list;;

let x = takewhile pari list;;
Printf.printf "\n--------------------------------\n";;
Printf.printf "La sequenza di iniziale di elementi che soddisfano il predicato e': ";;
print_list x;;

(* dropwhile: (’a-> bool)-> ’a list-> ’a list,tale che dropwhile
 p lst riporti la lista che si ottiene eliminando i primi elementi di lst, fino
 a che soddisfano il predicato p. Gli elementi del risultato devono occorrere
 nello stesso ordine in cui occorrono nell’argomento.
 Adesempio, dropwhile (function n-> n mod 2 = 0) [0;2;4;6;7;8;
 9;10] = [7; 8; 9; 10] *)
let dropwhile p list = 
  let rec aux risultato list = 
    match list with
    [] -> risultato
    | x::rest -> if not (p x) then risultato@[x]@rest
    else
      aux risultato rest
    in aux [] list;;

let x = dropwhile pari list;;
Printf.printf "\n--------------------------------\n";;
Printf.printf "La sequenza di iniziale di elementi che non soddisfano il predicato e': ";;
print_list x;;

(* partition: (’a-> ’bool)-> ’a list-> (’a list * ’a list),tale
 che partition p lst = (yes,no), dove yes contiene tutti gli elementi di
 lst che soddisfano il predicato p, e no quelli che non lo soddisfano. Gli
 elementi delle due liste yes e no possono essere in qualsiasi ordine.
 Adesempio, il valore di partition (function n-> n mod 2 = 0) [0;2;
 4;6;7;8;9;10] può essere ([10; 8; 6; 4; 2; 0], [9; 7]). *)

 let partition p list = 
  let rec aux veri falsi list = 
    match list with
    [] -> (veri, falsi)
    | x::rest -> if p x then aux (veri@[x]) falsi rest
    else
      aux veri (falsi@[x]) rest
    in aux [] [] list;;

Printf.printf "\n--------------------------------\n";;
(* Funzione per stampare una int list *)
let rec print_int_list = function
  | [] -> print_string "[]"
  | [x] -> Printf.printf "%d" x
  | x::xs -> Printf.printf "%d; " x; print_int_list xs

(* Funzione per stampare la coppia di liste *)
let print_partition_result (l1, l2) =
print_string "Soddisfano il predicato: ["; print_int_list l1; print_string "]\n";
print_string "NON soddisfano il predicato: ["; print_int_list l2; print_string "]\n"

let risultato = partition pari list
let () = print_partition_result risultato

(* pairwith: ’a-> ’b list-> (’a * ’b) list tale che, pairwith y
 [x1;x2;...;xn] = [(y,x1);(y,x2);....; (y,xn)]. Utilizzare la fun
zione List.map. *)

let pairwith y list = List.map (function x-> (y, x)) list;;

(* Funzione per stampare una lista di coppie int * int *)
let rec print_pair_list = function
  | [] -> print_string "[]\n"
  | [(a, b)] -> Printf.printf "(%d, %d)]\n" a b
  | (a, b)::rest ->
      Printf.printf "(%d, %d); " a b;
      print_pair_list rest;;

Printf.printf "\n--------------------------------\n";;
(* Esempio d'uso *)
let () =
  let result = pairwith 5 list in
  print_string "[";
  print_pair_list result

(*verifica_matrice: int-> int list list-> bool, che, dato un in
tero n e una matrice di interi, rappresentata mediante liste di liste, ri
porta true se la matrice contiene almeno una riga i cui elementi siano
 tutti minori di n, false altrimenti. Utilizzare le funzioni List.exists e
 List.for_all.*)

 let rec verifica_matrice n matrix = 
  match matrix with
  [] -> false
  | x::rest -> if (List.for_all (function x-> x<n) x) then true
  else
    verifica_matrice n rest;;

    let matrice = [
      [7; 8; 9];     (* prima riga *)
      [4; 5; 6];     (* seconda riga *)
      [1; 2; 3]      (* terza riga *)
    ];;

let stampa_matrice matrice =
  let rec stampa_righe = function
    | [] -> ()
    | riga::rest ->
        List.iter (fun x -> Printf.printf "%d " x) riga;
        print_newline ();
        stampa_righe rest
  in
  stampa_righe matrice;;

Printf.printf "\n--------------------------------\n";;
stampa_matrice matrice;;
let x = verifica_matrice 4 matrice;;
Printf.printf "--------------------------------\n";;
Printf.printf "Esiste una riga della matrice che sia tutta <4? %b" x;;

let verifica_matrice n matrix =
  List.exists (fun riga -> List.for_all (fun x -> x < n) riga) matrix;;

(* setdiff: ’a list-> ’a list-> ’a list, la differenza insiemistica,
 utilizzando la funzione List.filter. *)

 let mem l x = List.exists ((=) x) l;;

 let non p x = not (p x);;
 
 let setdiff l1 l2 = List.filter (non (mem l2)) l1;;

 (* subset: ’a list-> ’a list-> bool, tale che subset set1 set2 =
 true se set1 rappresenta un sottoinsieme di set2. Utilizzare la funzione
 List.for_all. *)

 let subset set1 set2 = List.for_all (function x-> List.exists ((=) x) set2) set1;;

 let x = subset list2 list;;
 Printf.printf "\n--------------------------------\n";;
 Printf.printf "List2 è sottoinsieme di list? %b" x;;

 (* duplica: int list-> int list, che raddoppia tutti gli elementi di
 una lista di interi, usando la funzione List.map.
 Ad esempio, duplica [0;1;2;3;4] = [0; 2; 4; 6; 8] *)
 let duplica list = List.map (function x-> x*2) list;;

 let x = duplica list;;
 Printf.printf "\n--------------------------------\n";;
 Printf.printf "List duplicata: ";;
 print_list x;;

 (*mapcons: (’a * ’b list) list-> ’b-> (’a * ’b list) list,che
 che, data una lista di coppie L e un elemento x: ’b, riporta la lista che
 si ottiene inserendo x in testa a ogni secondo elemento delle coppie in L.
 Utilizzare la funzione List.map.
 Adesempio, applicata alla lista [(’A’,[1;2]); (’B’,[3;4;5]); (’C’,[])]
 e al valore 0, la funzione riporta [(’A’,[0;1;2]); (’B’,[0;3;4;5]);
 (’C’,[0])].*)

 let mapcons l x = List.map (function (z,y)-> (z, x::y)) l;;
let esempio = [('A', [1;2]); ('B', [3;4;5]); ('C', [])]
let x = 0

let print_assoc_list l =
  let print_pair (a, lst) =
    Printf.printf "(%c, [" a;
    List.iter (fun n -> Printf.printf "%d;" n) lst;
    Printf.printf "])\n"
  in
  List.iter print_pair l;;
  Printf.printf "\n--------------------------------\n";;
  Printf.printf "Prova mapcons: \n";;
  let risultato = mapcons esempio x;;
  let () = print_assoc_list risultato;;

(* tutte_liste_con: int-> ’a-> ’a ’a list list, che, dato un in
tero non negativo n e due valori (dello stesso tipo) x e y, riporta una lista
 contenente tutte le possibili liste di lunghezza n contenenti soltanto i due
 valori x e y.
 Ad esempio, per n=3, x=0 e y=1 si avra‘ la lista seguente (o una sua
 permutazione):
 [[0; 0; 0]; [0; 0; 1]; [0; 1; 0]; [0; 1; 1];
 [1; 0; 0]; [1; 0; 1]; [1; 1; 0]; [1; 1; 1]] *)
 let rec tutte_liste_con n x y =
  if n = 0 then
    [ [] ]
  else
    (* tutte le liste di lunghezza n-1 *)
    let sottoliste = tutte_liste_con (n-1) x y in
    (* aggiungo x e y in testa a ciascuna di esse *)
    List.map (fun l -> x :: l) sottoliste
    @
    List.map (fun l -> y :: l) sottoliste;;

(* interleave: ’a-> ’a list-> ’a list list, tale che interleave
 x lst riporti una lista con tutte le liste che si ottengono inserendo x in
 qualsiasi posizione in lst. Utilizzare la funzione List.map.
 Ad esempio, interleave 10 [0;1;2] = [[10; 0; 1; 2]; [0; 10; 1;
 2]; [0; 1; 10; 2]; [0; 1; 2; 10]]. *)

 let rec interleave x lst = match lst with
    [] -> [[x]]
    | y::rest -> (x::lst) :: List.map( function z-> y::z) (interleave x rest);;

 Printf.printf "\n--------------------------------\n";;
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

let rec permut list = 
  match list with
  [] -> [[]]
  | x::rest -> List.flatten(List.map (interleave x) (permut rest));;