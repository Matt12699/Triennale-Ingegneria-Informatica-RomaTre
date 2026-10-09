
(* Funzione per stampare una lista di interi *)
let print_list lst =
  Printf.printf "[";
  List.iter (fun x -> Printf.printf "%d; " x) lst;
  Printf.printf "]\n";;

  let rec stampa_lista l =
    match l with
    | [] -> ()
    | x::rest ->
        Printf.printf "%d " x;
        stampa_lista rest
  
  let rec stampa_lista_di_liste ll =
    match ll with
    | [] -> ()
    | x::rest ->
        Printf.printf "[ ";
        stampa_lista x;
        Printf.printf "]\n";
        stampa_lista_di_liste rest

  let rec stampa_coppie lst =
  match lst with
  | [] -> ()
  | (a, b)::rest ->
      Printf.printf "(%d, %d)\n" a b;
      stampa_coppie rest

let rec stampa_int_list l =
  match l with
  | [] -> ()
  | [x] -> Printf.printf "%d" x
  | x::rest ->
      Printf.printf "%d; " x;
      stampa_int_list rest

let rec stampa_enumera lst =
  match lst with
  | [] -> ()
  | (i, l)::rest ->
      Printf.printf "(%d, [" i;
      stampa_int_list l;
      Printf.printf "])\n";
      stampa_enumera rest


(*length: ’a list-> int, che riporta il numero di elementi in una
 lista (Notare che il modulo List contiene una funzione con questo
 nome, ma qui si chiede di ridefinirla per esercizio).*)

 let length l =
  let rec aux cont l = 
    match l with
    [] -> cont
    | x::rest -> aux (cont+1) rest
  in aux 0 l;;

  let list = [1;2;3;4];;
  let l2 = [45; 66;77];;
  print_list list;;
  print_list l2;;
  let x = length list;;
  Printf.printf "\n--------------------------------\n";;
  Printf.printf "La lunghezza della lista e': %d" x;;

  (*sumof: int list-> int, che riporta la somma degli elementi in
 una lista di interi.*)

 let sumof list =
  let rec aux somma list = match list with
    [] -> somma
    | x::rest -> aux (somma+x) rest
 in aux 0 list;;

 let x = sumof list;;
 Printf.printf "\n--------------------------------\n";;
 Printf.printf "La somma della lista e': %d" x;;

 (* maxlist: ’a list-> ’a, che riporta il massimo elemento in una
 lista (la lista vuota non ha elementi, quindi nemmeno un massimo;
 dunque se la lista è vuota deve essere sollevata un’eccezione).*)

 exception NoElements

 let maxlist list =
  match list with
  [] -> raise NoElements
  | x::rest -> let rec aux max list =
                  match list with 
                  [] -> max
                  |x::rest -> if x>max then aux x rest
                  else
                    aux max rest
                  in aux x rest;;
                  
 let x = maxlist list;;
 Printf.printf "\n--------------------------------\n";;
 Printf.printf "Il massimo della lista e': %d" x;;

 (*drop: int-> ’a list-> ’a list, tale che drop n lst = lista
 che si ottiene da lst togliendone i primi n elementi. Se il numero
 di elementi di lst è minore di n (oppure uguale a n), allora drop n
 lst = [].*)

 let rec drop n list = match list with
        [] -> []
        | x::rest -> if n <=0 then x::rest else
                      drop (n-1) rest;;

 let x = drop 2 list;;
 Printf.printf "\n--------------------------------\n";;
 Printf.printf "La lista tagliata di due elementi: \n";;
 print_list x;;

 (* append: ’a list-> ’a list-> ’a list. Se @ non fosse predefinito, come si potrebbe definire (utilizzando solo i costruttori delle
 liste)?*)

 let append l1 l2 = 
    let rec aux risultato l1 l2 =
      match l1 with
      [] -> risultato
      | x::rest -> aux (x::risultato) rest l2
    in aux (List.rev l2) (List.rev l1)  l2;;

    let x = append list l2;;
    Printf.printf "\n--------------------------------\n";;
    Printf.printf "L'append delle due liste: \n";;
    print_list x;;

  (*reverse: ’a list-> ’a list, che rovescia una lista, cioè riporta
 la lista che contiene gli stessi elementi di quella data, ma in ordine
 inverso (Notare che il modulo List contiene una funzione rev che
 rovescia una lista, ma qui si chiede di ridefinirla per esercizio).*)
 let reverse l =
  let rec aux risultato l = match l with
    [] -> risultato
    | x::rest -> aux (x::risultato) rest
 in aux [] l;;

 let x = reverse list;;
  Printf.printf "\n--------------------------------\n";;
  Printf.printf "La lista ribaltata: \n";;
  print_list x;;

  (*nth: int-> ’a list-> ’a, tale che nth n lst = elemento di lst
 in posizione n, dove il primo elemento della lista è in posizione 0. La
 funzione solleverà un’eccezione se n è negativo o se la lista non con
tiene abbastanza elementi (Notare che il modulo List contiene una
 funzione con questo nome, ma qui si chiede di definirla per esercizio).*)

 exception Negative
 exception NeedMoreElements

 let rec nth n lst = if n<0 then raise Negative else
    match lst with
    [] -> raise NeedMoreElements
    | x::rest -> if n = 0 then x
    else 
      nth (n-1) rest;;

  let x = nth 3 list;;
  Printf.printf "\n--------------------------------\n";;
  Printf.printf "Il secondo elemento della lista e': %d" x;;

(*remove: ’a-> ’a list-> ’a list,tale che remove x lst elimina 
tutte le occorrenze di x da lst. Se non ve ne sono, viene riportata
 lst stessa.*)

 let remove x lst = 
  let rec aux risultato x lst = match lst with
    [] -> risultato
    | y::rest -> if y=x then aux risultato x rest else
                  aux (risultato@[y]) x rest
    in aux [] x lst;;

    let x = remove 22 list;;
    Printf.printf "\n--------------------------------\n";;
    Printf.printf "La lista senza 22: \n";;
    print_list x;;

(*Una funzione copy: int-> ’a-> ’a list tale che copy n x ri
porti la lista di lunghezza n i cui elementi sono tutti uguali a x.
 Determinare il valore e il tipo di copy 3 (copy 2 8).*)

 let copy n x =
  let rec aux risultato cont x = if cont = 0 then risultato else
                                    aux (x::risultato) (cont-1) x
  in aux [] n x;;

  let x = copy 3 [12;12];;
  Printf.printf "\n--------------------------------\n";;
  Printf.printf "La lista di soli [12;12]: \n";;
  stampa_lista_di_liste x;;

(* Un predicato nondec: int list-> bool che, applicato a una lista
 lst, riporti true se gli elementi di lst sono in ordine non decrescente,
 false altrimenti.
 Adesempio, nondec [1;2;3;4] = true, enondec [1;2;4;3] = false.*)

 let rec nondec l =  match l with
    [] -> true
    | [x] -> true
    | x::y::rest -> if x>y then false
                    else
                    nondec (y::rest);;

  let x = nondec list;;
  Printf.printf "\n--------------------------------\n";;
  Printf.printf "La lista e' decrescente: %b \n" x;;


(*Una funzione pairwith: ’a-> ’b list-> (’a * ’b) list che,
 applicata a un valore y e una lista xs = [x1;x2;...;xn], riporti la
 lista [(y,x1);(y,x2);....;(y,xn)].*)

 let pairwith n list = 
  let rec aux risultato n list =
    match list with
    [] -> risultato
    | x::rest -> aux (risultato@[(n,x)]) n rest
  in aux [] n list;;

  let x = pairwith 9 list;;
  Printf.printf "\n--------------------------------\n";;
  Printf.printf "La lista con pairwith 9: \n";;
  stampa_coppie x;;

  (*Una funzione duplica: ’a list-> ’a list che, applicata a una
 lista xs = [x1;x2;...;xn], riporti la lista [x1;x1;x2;x2;...;xn;xn].*)
 let duplica list =
  let rec aux risultato list = 
    match list with
    [] -> risultato
    | x::rest -> aux (risultato@[x]@[x]) rest
  in aux [] list;;

let x = duplica list;;
Printf.printf "\n--------------------------------\n";;
Printf.printf "La lista con duplica applicata: \n";;
print_list x;;

(*Una funzione enumera: ’a list-> (int * ’a) list che, appli
cata a una lista lst=[x0;x1;x2;...;xk], riporti la lista di coppie
 [(0,x0);(1,x1);(2,x2);...;(k,xk)].*)
 let enumera list = 
  let rec aux risultato cont list =
    match list with
    [] -> risultato
    | x::rest -> aux (risultato@[(cont,[x])]) (cont+1) rest
  in aux [] 0 list;;

  let x = enumera list;;
  Printf.printf "\n--------------------------------\n";;
  Printf.printf "La lista con enumera: \n";;
  stampa_enumera x;;

(*Una funzione position: ’a-> ’a list-> int tale che position
 x lst riporti la posizione della prima occorrenza di x in lst (con
tando a partire da 0). Se x non occorre in lst, la funzione solleverà
 un’eccezione.*)

 exception NoOccurrency

 let position x list = 
  let rec aux risultato x list = match list with
    [] -> raise NoOccurrency
    | y::rest -> if y=x then risultato else 
                aux (risultato+1) x rest
    in aux 0 x list;;
    
    let x = position 2 list;;
    Printf.printf "\n--------------------------------\n";;
    Printf.printf "Prima posizione di 2 nella lista: %d\n" x;;

(*Una funzione alternate: ’a list-> ’a list che, applicata auna
 lista lst, riporti la lista contentente tutti e soli gli elementi di lst
 che si trovano in posizione dispari. Ricordiamo che, per convenzione,
 il primo elemento di una lista si trova in posizione 0, il secondo in
 posizione 1, ecc. Quindi, ad esempio, alternate [0;1;20;32;4;5]
 = [1;32;5].*)
 let alternate list = 
  let rec aux cont risultato list =
    match list with
    [] -> risultato
    | x::rest -> if cont mod 2!=0 then aux (cont+1) (risultato@[x]) rest
    else
      aux (cont+1) risultato rest
    in aux 0 [] list;;







