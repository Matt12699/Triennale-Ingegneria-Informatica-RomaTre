type 'a graph = ('a * 'a list) list;;

exception Nodo_inesistente
let successori x grafo = 
  try List.assoc x grafo 
with Not_found -> raise Nodo_inesistente;;

(* Li rappresentiamo più semplicemente così *)
(* Questa è solo una lista di archi *)
type 'a graph = ('a * 'a) list;;

(* Si può utilizzare anche per gli archi non orientati. I nodi non possono ripetersi *)
(* vicini sono i "vicini" dei grafi non orientati. i successori sono solo i successori dei grafi orientati *)
let rec successori x grafo = List.map (snd) (List.filter (function (y,z)-> x=y) grafo);;

let rec successori x grafo = if List.exists (function (y,z) -> y=x || z=x) grafo then List.map (snd) (List.filter (function (y,z)-> x=y) grafo) else raise Nodo_inesistente;;

let rec vicini x grafo = List.map (function (y,z) -> if x=y then z else y) (List.filter (function (y,z)-> x=y || x=z) grafo)

(* Quando vuoi esplorare un grafo devi avere un punto da cui partire *)
(* Potrebbero esserci dei cicli e l'algoritmo potrebbe incastrarsi nei cicli *)
(* Bisogna avere un accumulatore che mi tenga conto dei nodi visitati *)
(* Le visite sono due: profondità e ampiezza: profondità comincio da un nodo e visito un successore poi un successore del successore e cosi via*)
(* Quella in ampiezza invece: *)

(*let rec profondita x grafo =
  let grafo = successori x grafo in
  let rec aux visitati grafo = 
  match grafo with
  [] -> visitati@[x]
  | (x,y)::rest -> if List.mem *)

  (* test_connessi: 'a graph -> 'a -> 'a -> bool determini se esiste un cammino dal primo al secondo nodo *)

  let rec test_connessi grafo nodo1 nodo2 =
      let rec aux visitati succ = 
        match succ with
        [] -> false                                                         (* Significa che i successori sono già stati aggiunti *) (*Se x non è stato visitato significa che i successori non erano stati aggiunti*)
        | x::rest -> if x = nodo2 then true else if List.mem x visitati then aux visitati rest else aux (x::visitati) ((successori x grafo)@rest) (* se volevo fare in ampiezza invertivo questa parentesi*)
      in aux [] [nodo1];;

  (* esiste_ciclo: 'a graph -> 'a -> bool Determini se esiste un ciclo su n (ovvero un cammino da n a n che contenga almeno un arco). Altrimenti solleva un eccezione*)
  
  let rec esiste_ciclo grafo nodo =
      let rec aux visitati succ = 
        match succ with
        [] -> false                                                         (* Significa che i successori sono già stati aggiunti *) (*Se x non è stato visitato significa che i successori non erano stati aggiunti*)
        | x::rest -> if x = nodo then true else if List.mem x visitati then aux visitati rest else aux (x::visitati) ((successori x grafo)@rest) (* se volevo fare in ampiezza invertivo questa parentesi*)
      in aux [] (successori nodo grafo);;

  (* ciclo: 'a graph -> 'a -> 'a list deve restituirci il ciclo *)
  exception NoCicle
  let rec ciclo grafo nodo =
      let rec aux visitati succ = 
        match succ with
        [] -> raise NoCicle                                                         
        | x::rest -> 
        try 
        if x = nodo then (List.rev (x::visitati)) else if List.mem x visitati then aux visitati rest else 
        try let s = successori x grafo in
        aux (x::visitati) ([List.hd s]) 
        with _ -> aux (x::visitati) (List.tl (successori x grafo))
      with _ -> aux visitati rest
      in aux [nodo] (successori nodo grafo);;

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

  let x = ciclo graph 6;;

  Printf.printf "\n=======================\n";;
  Printf.printf "Visita in profondita del grafo: ";;
  print_int_list x;;

  (* Ricerca di un nodo raggiungibile da un nodo di ingresso *)

  exception NonRaggiungibile
  let rec nodoRaggiungibile start goal grafo =
    let rec aux visitati pendenti = 
      match pendenti with
      [] -> raise NonRaggiungibile
      | x::rest -> if x=goal then goal
                          else
                      if not(List.mem x visitati) then aux (visitati@[x]) ((successori x grafo)@rest) else
                            aux (visitati) rest 
                          in aux [] [start];;

  exception NonTrovato
  let rec nodoPredicato start p grafo =
    let rec aux visitati pendenti = 
      match pendenti with
      [] -> raise NonTrovato
      | x::rest ->  if p x then x
                          else
                     if not(List.mem x visitati) then aux (visitati@[x]) ((successori x grafo)@rest) else
                            aux (visitati) rest 
                          in aux [] [start];;



  
