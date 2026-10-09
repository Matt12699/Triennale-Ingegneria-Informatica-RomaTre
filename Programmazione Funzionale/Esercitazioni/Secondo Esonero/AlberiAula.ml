type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

(* Data un etichetta la aggiunge alla lista dei risultati*)

let rec add x assoc_list = 
  match assoc_list with
  [] -> [(x, 1)]
  | (a, n)::rest -> if a=x then (a,n+1)::rest
  else
                            (a, n):: (add x rest);;


let count albero = 
  let rec aux risultato albero =
      match albero with
      Empty -> risultato
      | Tr(x, l, r) -> (aux (aux (add x risultato) l) r)  (* Giallo aggiungi x, applichi aux sull albero l alla lista creata (aggiungi l), applichi aux sull'albero r (aggiungi r)*)
  in aux [] albero;;

(*preorder,postorder,inorder, tutte di tipo ’a tree->’a list. Dato un albero t, le funzioni riportano la lista dei nodi di t, nell’ordine in
 cui sarebbero visitate secondo gli algoritmi di visita, rispettivamente, in
 preordine,postordine e simmetrica.*)
 let rec preorder t = 
  match t with
  Empty -> []
  | Tr(x, l, r) ->  x:: (preorder l) @ (preorder r);;

  let rec postorder t = 
  match t with
  Empty -> []
  | Tr(x, l, r) ->  (postorder r) @ (postorder l) @ [x];;

  let rec inorder t = 
  match t with
  Empty -> []
  | Tr(x, l, r) ->  (inorder l) @ [x] @ (inorder r);;



(* Guardando l'albero delle slide riesco a codificare in morse, figlio sinistro è un ., figlio destro è un -*)
exception NotFound

let alphabet= Tr(1, Empty, Empty);;

let morse s =
  let rec aux risultato alphabet = 
    match alphabet with
    Empty -> raise NotFound
    | Tr(x, l, r) -> if x=s then risultato
    else
                    try aux(risultato ^ "*") l
                    with NotFound -> aux(risultato ^ "-") r
                  in aux "*" alphabet;;

 (* balpreorder e balinorder,entrambe di tipo ’a list->’a tree.Data
 una lista lst, costruiscono un albero bilanciato con nodi etichettati da
 elementi di lst, in modo tale che preorder (balpreorder lst) = lst
 inorder (balinorder lst) = lst*)

 let rec take n list = 
  match list with
  [] -> []
  | x::rest -> if n=0 then [] else x::(take (n-1) rest);;

let rec drop n list =
  match list with
  [] -> []
  | x::rest -> if n=0 then x::rest else (drop (n-1) rest);;

 let rec balpreorder list = 
  match list with
  [] -> Empty
  | x::rest -> let size = List.length rest in
              Tr(x, balpreorder (take (size/2) rest), balpreorder (drop (size/2) rest));;

  let rec balinorder list = 
    match list with
    [] -> Empty
    | l -> let n = List.length l in
          let xs = take(n/2) l in
          let ys = drop(n/2) l in
          match ys with
          [] -> balinorder xs
          | x::rest -> Tr(x, balinorder xs, balinorder rest);;

type player = Min | Max;;
type minimaxtree = Leaf of int 
                  | Node of (player * int) * minimaxtree list;;

type 'a ntree = Tr of 'a * 'a ntree list;;

type multi_expr =
 MultiInt of int
 | MultiVar of string
 | MultiDiff of multi_expr * multi_expr
 | MultiDiv of multi_expr * multi_expr
 | MultiSum of multi_expr list
 | MultiMult of multi_expr list;;
 
 (* Scrivere una funzione subexpr: multi_expr-> multi_expr-> bool
 che, date due espressioni aritmetiche E1 e E2, determini se E2 è una
 sottoespressione di E1. *)

 let rec subexpr e1 e2 =
  let rec aux list =
      match list with
      [] -> false
    | x::rest -> if (subexpr x e2) then true else aux rest
 in
  match e1 with
  MultiInt n -> if e1 = e2 then true else false
 | MultiVar s -> if e1 = e2 then true else false
 | MultiDiff (me1, me2)-> (subexpr me1 e2) || (subexpr me2 e2)
 | MultiDiv (me1,me2)-> (subexpr me1 e1) || (subexpr me2 e2)
 | MultiSum mlist->  aux mlist                          
 | MultiMult mlist-> aux mlist;;

 let rec subexpr e1 e2 =
  match (e1,e2) with
  (MultiInt x1, MultiInt x2) -> x1=x2
  | (MultiInt _, _) -> false
  | (MultiVar x1, MultiVar x2) -> x1=x2
  | (MultiVar _, _) -> false
  | (MultiDiff (me1,me2), m2) -> subexpr me1 m2 || subexpr me2 m2 || MultiDiff (me1,me2) = m2
  | (MultiDiv (me1,me2), m2) -> subexpr me1 m2 || subexpr me2 m2 || MultiDiv (me1,me2) = m2
  | (MultiSum l, m2) -> List.exists (function x -> subexpr x m2) l
  | (MultiMult l, m2) -> List.exists (function x -> subexpr x m2) l;;

  (*Nella visita in postordine degli alberi n-ari vengono prima visitati tutti
 i sottoalberi, poi la radice. Nella visita simmetrica viene prima visitato
 il sottoalbero sinistro, poi la radice, poi gli altri sottoalberi (se ve ne
 sono). Implementare due funzioni postorder: ’a ntree-> ’a list e
 inorder: ’a ntree-> ’a list che, dato un albero n-ario, riportino la
 lista dei suoi nodi nell’ordine in cui sarebbero visitati secondo i due algo
ritmi di visita.*)


let rec postorder albero = 
  match albero with
  Tr(x, []) -> [x]
  | Tr(x, list) -> List.flatten (List.map postorder list)@[x];;