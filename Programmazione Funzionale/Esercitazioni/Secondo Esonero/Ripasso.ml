(* Alberi binari *)

type expr =
  Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr

let rec subexpr e1 e2 = match e1 with
    Int _ | Var _ -> e1 = e2
    | Sum(e',e'') | Diff(e',e'') | Mult(e',e'') | Div(e',e'') ->
       e1=e2 || subexpr e' e2 || subexpr e'' e2

let rec subst_in_expr e x e' = match e with
    Int _ -> e
  | Var y -> if x=y then e' else e
  | Sum(e1,e2) -> Sum (subst_in_expr e1 x e', subst_in_expr e2 x e')
  | Diff(e1,e2) -> Diff (subst_in_expr e1 x e', subst_in_expr e2 x e')
  | Mult(e1,e2) -> Mult (subst_in_expr e1 x e', subst_in_expr e2 x e')
  | Div(e1,e2) -> Div (subst_in_expr e1 x e', subst_in_expr e2 x e')
;;

(* Visite alberi binari *)
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

let rec reflect = function
    Empty -> Empty
  | Tr(x,tr1,tr2) -> Tr(x, reflect tr2, reflect tr1);;

let rec take n = function
    [] -> []
  | x::xs -> if n<=0 then []
             else x::(take (n-1) xs)

let rec drop n = function
    [] -> []
  | l -> if n>0 then drop (n-1) (List.tl l)
         else l


  let rec balpreorder list = 
    match list with
    [] -> Empty
    | x::rest -> let lunghezza = List.length list in
                  Tr(x, balpreorder(take(lunghezza/2) rest), balpreorder(drop(lunghezza/2) rest));;

  let rec balinorder list = 
    match list with
    [] -> Empty
    | lst -> let centro = (List.length lst)/2 in
             let elementoCentrale = List.nth lst centro in
             let listaSinistra = take centro lst in
             let listaDestra = drop (centro+1) lst in
            Tr(elementoCentrale, balinorder(listaSinistra), balinorder(listaDestra));;

let balpostorder l = reflect(balpreorder (List.rev l))
;;

let subset l1 l2 = List.for_all (function x -> List.mem x l2) l1;;


 let rec foglie_in_lista list albero = 
    match albero with
    Empty -> true
    | Tr(x, Empty, Empty) ->  if (List.mem x list) then true else false
    | Tr(x, l, r) -> foglie_in_lista list l && foglie_in_lista list r;;

 exception AlberoVuoto

 let rec segui_bool list albero=
    match albero with
    Empty -> raise AlberoVuoto
    | Tr(x, l, r) -> match list with
                    [] -> x
                    | y::rest -> if y then segui_bool rest l else segui_bool rest r;;

  let rec altezza albero = 
    match albero with
    Empty -> 0
    | Tr(_, l, r) -> 1 + max (altezza(l)) (altezza(r));;

 let rec balanced albero= 
    match albero with
    Empty -> true
    | Tr(x, l, r) -> balanced l && balanced r && abs(altezza(l) - altezza(r))<=1;;

(* radice-> sottoalbero sinistro -> sottoalbero destro *)
let rec preorder albero = 
  match albero with
  Empty -> []
  | Tr(x, l , r) -> [x]@(preorder l)@(preorder r);;

(* sottoalbero sinistro-> sottoalbero destro -> radice*)
let rec postorder albero = 
  match albero with
  Empty -> []
  | Tr(x, l, r) -> (postorder l)@(postorder r)@[x];;

(*sottoalbero sinistro -> radice -> sottoalbero destro*)
let rec inorder albero =
  match albero with
  Empty -> []
  | Tr(x, l, r)-> (inorder l)@[x]@(inorder r);;

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
type col = Rosso | Giallo | Verde | Blu;;
type 'a col_assoc = (col * 'a list) list;;

exception NoColore
let rec colore x lista =
  match lista with
  [] -> raise NoColore
  | (col, numeri)::rest -> if List.mem x numeri then col else colore x rest;;

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
  
(* Alberi n-ari *)

type multi_expr = MultiInt of int
                | MultiVar of string
                | MultiDiff of multi_expr * multi_expr
                | MultiDiv of multi_expr * multi_expr
                | MultiSum of multi_expr list
                | MultiMult of multi_expr list

let e = MultiSum [MultiInt 5; MultiVar "x"] 

let rec subexpr e e' = match e with
    MultiInt _ | MultiVar _ | MultiSum [] |  MultiMult [] -> e = e'
    | MultiDiff(e1,e2) | MultiDiv(e1,e2) -> e=e' || subexpr e1 e' || subexpr e2 e'
    | MultiSum(x::xs) | MultiMult(x::xs) -> e=e' || subexpr x e' || List.exists (function x -> subexpr x e') xs

let rec subst e var e' = match e with
    MultiInt _ -> e
  | MultiVar x -> if x=var then e' else e
  | MultiDiff(e1,e2) -> MultiDiff(subst e1 var e', subst e2 var e')
  | MultiDiv(e1,e2) -> MultiDiv(subst e1 var e', subst e2 var e')
  | MultiSum l -> MultiSum (List.map (function x -> subst x var e') l)
  | MultiMult l -> MultiMult (List.map (function x -> subst x var e') l)
;;

(* Visite alberi n-ari *)
type 'a ntree = Tr of 'a * 'a ntree list;;

let rec foglie_in_lista l = function
    Tr(n,[]) -> List.mem n l
  | Tr(_,ts) -> List.for_all (foglie_in_lista l) ts

(* radice -> sottoalberi *)
let rec preorder (Tr(x, figli)) = [x]@(List.flatten (List.map (preorder) figli));;

(* sottoalberi -> radice *)
let rec postorder (Tr(x, figli)) = (List.flatten (List.map (postorder) figli))@[x];;

(* sottoalbero sinistro -> radice -> sottoalbero destro *)
let rec inorder (Tr(x, figli)) = 
  match figli with
  [] -> [x]
  | y::rest -> (inorder y)@[x]@(List.flatten (List.map (inorder) rest));;

 let rec tutte_foglie_costi albero =
  let rec aux costo l albero =
    match albero with
    Tr(x, [])-> (x, costo+x)::l
    | Tr(x, list) -> List.flatten(List.map (aux (costo+x) l) list)
  in aux 0 [] albero;;

exception NoTree

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

    exception NumeroNonEsistente
    let rec colore x assoc_col =
      match assoc_col with
      [] -> raise NumeroNonEsistente
      | (col, lista)::rest -> if (List.mem x lista) then col else colore x rest;;

    exception NoRamo;;
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

(* Visite grafi *)
type 'a graph = ('a * 'a) list;;

(* Funzione successori *)
exception NonNelGrafo
let rec successori x grafo = if List.exists (function (y,z) -> x=y || z=x)  grafo then List.map (snd) (List.filter (function (y,_)-> x=y) grafo) else raise NonNelGrafo;;

(* Funzione successori per grafi non orientati *)
let rec vicini x grafo = if List.exists (function (y,z) -> x=y || z=x)  grafo then List.map (function (y,z)-> if x=y then z else y) 
(List.filter (function (y,z)-> x=y || x=z) grafo) else raise NonNelGrafo;;

(* Visita in profondità *)

let depth_first_search start grafo =
  let rec aux visitati pendenti = 
    match pendenti with
    [] -> List.rev visitati
    | x::rest -> if List.mem x visitati then aux visitati rest
                  else
                    aux (x::visitati) (successori x grafo)@rest
                  in aux [] [start];;

(* Visita in ampiezza *)

let breadth_first_search start grafo =
  let rec aux visitati pendenti = 
    match pendenti with
    [] -> List.rev visitati
    | x::rest -> if List.mem x visitati then aux visitati rest
                  else
                    aux (x::visitati) rest@(successori x grafo)
                  in aux [] [start];;

let test_connessi grafo nodo1 nodo2 =
  let rec aux visitati pendenti =
    match pendenti with
    [] -> false
    | x::rest -> if x=nodo2 then true
                  else
                    if List.mem x visitati then aux visitati rest 
                    else aux (x::visitati) ((successori x grafo)@rest)
                  in aux [] [nodo1];;
                
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